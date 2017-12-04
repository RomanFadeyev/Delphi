unit FC.Trade.Trader.MACross2;

{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderMACross2 = interface
  ['{93A29BEA-F909-4857-B1A5-98AC8A7F6BEC}']
  end;

  INeedCloseAttribute = interface (ISCAttribute)
  ['{20CA817F-0818-4DED-8B96-9253568BDE75}']
    procedure SetExpirationDateTime(const aDateTime:TDateTime);
    function GetExpirationDateTime: TDateTime;
  end;

  TStockTraderMACross2 = class (TStockTraderBase,IStockTraderMACross2)
  private
    FBarHeightD1 : ISCIndicatorBarHeight;
    FFastMA_H1,FSlowMA_H1: ISCIndicatorMA;
    //FLWMA21_H1,FLWMA55_H1: ISCIndicatorMA;
    //FMA84_H1,FMA220_H1: ISCIndicatorMA;
    FLastOpenOrderTime : TDateTime;
    FPropFullBarsOnly  : TPropertyYesNo;
  protected
    function  CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
    function  CreateMA_H1(const aChart: IStockChart; aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;

    //Считает, на какой примерно цене сработает Stop Loss или Trailing Stop
    function  GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;

    //Считает, какой убыток будет, если закроется по StopLoss или Trailing Stop
    function  GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;

    procedure CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
    procedure CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
    function  GetRecommendedLots: TStockOrderLots; override;

    procedure SetTP(const aOrder: IStockOrder; const aTP: TStockRealNumber;  const aComment: string);

    function  GetMainTrend(index: integer): TSCRealNumber;
    function  GetFastTrend(index: integer): TSCRealNumber;
    function  GetCross(index: integer): integer;
    function  PriceToPoint(const aPrice: TSCRealNumber): integer;
    function  GetExpirationDateTime(aTime: TDateTime): TDateTime;

    procedure TrackOrderLevels(const aMALevel: TSCRealNumber);
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    function OpenMASnappedOrder(aKind: TStockOrderKind;const aMALevel:TSCRealNumber; const aComment: string=''): IStockOrder;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses DateUtils,Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;

type
  TNeedCloseAttribute = class (TNameValuePersistentObjectRefCounted,INeedCloseAttribute,ISCAttribute)
  private
    FExpirationDateTime: TDateTime;
  public
    procedure SetExpirationDateTime(const aDateTime:TDateTime);
    function GetExpirationDateTime: TDateTime;
  end;
{ TStockTraderMACross2 }

procedure TStockTraderMACross2.CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
  aAttribute: INeedCloseAttribute;
  aExpTime: TDateTime;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) then
    begin
      if aOrders[i].GetState=osOpened then
      begin
        if aOrders[i].GetAttributes.IndexOf(INeedCloseAttribute)=-1 then
        begin
          aAttribute:=TNeedCloseAttribute.Create;
          aExpTime:=TStockDataUtils.AlignTimeToLeft(GetBroker.GetCurrentTime,sti60);
          aExpTime:=GetExpirationDateTime(aExpTime);
          aAttribute.SetExpirationDateTime(aExpTime);
          aOrders[i].GetAttributes.Add(aAttribute);
        end;

        //CloseOrder(aOrders[i],aComment)
      end
      else if aOrders[i].GetState=osPending then
        aOrders[i].RevokePending;
    end;
  end;
end;

procedure TStockTraderMACross2.CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) and (aOrders[i].GetCurrentProfit>0) then
      CloseOrder(aOrders[i],aComment);
  end;
end;

constructor TStockTraderMACross2.Create;
begin
  inherited Create;

  FPropFullBarsOnly   := TPropertyYesNo.Create('Method','Full Bars Only',self);
  FPropFullBarsOnly.Value:=true;

  RegisterProperties([FPropFullBarsOnly]);
  //UnRegisterProperties([PropLotDefaultRateSize,PropLotDynamicRate]);
end;

function TStockTraderMACross2.CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorBarHeight,'BarHeightD1',true, aCreated) as ISCIndicatorBarHeight;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetPeriod(3);
    Result.SetBarHeight(bhHighLow);
  end;
end;

function TStockTraderMACross2.CreateMA_H1(const aChart: IStockChart;aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'MA'+IntToStr(integer(aMethod))+'_'+IntToStr(aPeriod)+'_H1',true, aCreated) as ISCIndicatorMA;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetMAMethod(aMethod);
    Result.SetPeriod(aPeriod);
  end;
end;


destructor TStockTraderMACross2.Destroy;
begin
  inherited;
end;

procedure TStockTraderMACross2.Dispose;
begin
  inherited;
end;

function TStockTraderMACross2.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if aOrder.GetState=osOpened then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;

function TStockTraderMACross2.GetExpirationDateTime(aTime: TDateTime): TDateTime;
begin
  result:=IncHour(aTime,FFastMA_H1.GetPeriod-1);
end;

function TStockTraderMACross2.GetFastTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FLWMA21_H1.GetValue(index)-FLWMA55_H1.GetValue(index);
end;

function TStockTraderMACross2.GetRecommendedLots: TStockOrderLots;
var
  aDayVolatility,aDayVolatilityM,k: TStockRealNumber;
begin
  if not PropLotDynamicRate.Value then
    exit(inherited GetRecommendedLots);

  aDayVolatility:=FBarHeightD1.GetValue(FBarHeightD1.GetInputData.Count-1);
  //Считаем какая волатильность в деньгах у нас была последние дни
  aDayVolatilityM:=GetBroker.PriceToMoney(GetSymbol,aDayVolatility,1);
  //Считаем, сколько таких волатильностей вынесет наш баланс
  k:=(GetBroker.GetEquity/aDayVolatilityM);
  //Теперь берем допустимый процент
  result:=RoundTo(k*PropLotDynamicRateSize.Value/100,-2);
end;

function TStockTraderMACross2.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

procedure TStockTraderMACross2.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //Создае нужных нам экспертов
    FBarHeightD1:=CreateBarHeightD1(aValue.GetStockChart(sti1440));

    FFastMA_H1:= CreateMA_H1(aValue.GetStockChart(sti60),5,mamSimple);
    FSlowMA_H1:= CreateMA_H1(aValue.GetStockChart(sti60),10,mamSimple);

//    FLWMA21_H1:= CreateMA_H1(aValue.GetStockChart(sti60),21,mamLinearWeighted);
//    FLWMA55_H1:= CreateMA_H1(aValue.GetStockChart(sti60),55,mamLinearWeighted);

//    FMA84_H1:= CreateMA_H1(aValue.GetStockChart(sti60),84,mamSimple);
    //FMA220_H1:= CreateMA_H1(aValue.GetStockChart(sti60),220,mamSimple);
  end;
end;

procedure TStockTraderMACross2.SetTP(const aOrder: IStockOrder;const aTP: TStockRealNumber;  const aComment: string);
var
  aNew : TStockRealNumber;
begin
  aNew:=GetBroker.RoundPrice(aOrder.GetSymbol,aTP);
  if not SameValue(aNew,aOrder.GetTakeProfit) then
  begin
    if aComment<>'' then
      GetBroker.AddMessage(aOrder,aComment);
    aOrder.SetTakeProfit(aNew);
  end;
end;


procedure TStockTraderMACross2.TrackOrderLevels(const aMALevel: TSCRealNumber);
var
  i,j:integer;
  aAttribute : INeedCloseAttribute;
  aOrder: IStockOrder;
  aPrice: TSCRealNumber;
  aPriceKind: TStockBrokerPriceKind;
begin
  //Подправляем значения цены открытия для отложенных ордеров
  for i := 0 to GetOrders.Count-1 do
  begin
    aOrder:=GetOrders[i];
    if aOrder.GetState=osPending then
    begin
      if aOrder.GetKind=okBuy then
        aPriceKind:=bpkAsk
      else
        aPriceKind:=bpkBid;

      if not IsLevelTooCloseToCurrentPrice(aPriceKind,aMALevel) then
         try aOrder.SetPendingOpenPrice(aMALevel); except end;
    end
    else if aOrder.GetState=osOpened then
    begin
      j:=aOrder.GetAttributes.IndexOf(INeedCloseAttribute);
      if j<>-1 then
      begin
        aAttribute:=aOrder.GetAttributes.GetItem(j) as INeedCloseAttribute;
        if aAttribute.GetExpirationDateTime<=GetBroker.GetCurrentTime then
        begin
          aOrder.Close('Close time expired');
          if aOrder.GetKind=okBuy then
            inherited OpenOrder(okSell,'Overturn')
          else
            inherited OpenOrder(okBuy,'Overturn')
        end
        else begin
          if (aOrder.GetKind=okBuy) then
          begin
            aPrice:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
            if (aPrice<aMALevel) then
              aOrder.SetTakeProfit(aMALevel)
            else
              aOrder.SetStopLoss(aMALevel);
          end
          else if (aOrder.GetKind=okSell) then
          begin
            aPrice:=GetBroker.GetCurrentPrice(GetSymbol,bpkAsk);
            if (aPrice>aMALevel) then
              aOrder.SetTakeProfit(aMALevel)
            else
              aOrder.SetStopLoss(aMALevel);
          end
        end;
      end
    end;
  end;
end;

function TStockTraderMACross2.GetMainTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FMA84_H1.GetValue(index)-FMA220_H1.GetValue(index);
end;

function TStockTraderMACross2.GetCross(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FFastMA_H1.GetValue(index)-FSlowMA_H1.GetValue(index));
  x2:=Sign(FFastMA_H1.GetValue(index-1)-FSlowMA_H1.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

procedure TStockTraderMACross2.UpdateStep2(const aTime: TDateTime);
var
  idx60: integer;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
  aOpenedOrder: IStockOrder;
  aOpen : integer;
  aMaCross : integer;
  aTime60 : TDateTime;
  //aFastTrend : TSCRealNumber;
  //aPrice : TSCRealNumber;
  i: Integer;
  aMALevel: TSCRealNumber;
begin
  if FPropFullBarsOnly.Value then
  begin
    if not (MinuteOf(aTime) in [59..59])then
      exit;
  end;

  //Брокер может закрыть ордера и без нас. У нас в списке они останутся,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

  aChart:=GetParentStockChart(FFastMA_H1);
  aInputData:=aChart.GetInputData;
  idx60:=aChart.FindBar(aTime);

  aTime60:=TStockDataUtils.AlignTimeToLeft(aTime,sti60);
  if SameDateTime(FLastOpenOrderTime,aTime60) then
    exit;

  if (idx60<>-1) and (idx60>=FSlowMA_H1.GetPeriod) then
  begin
    aMALevel:=FFastMA_H1.GetValue(idx60);
    TrackOrderLevels(aMALevel);

    aOpen:=0;
    for i := idx60 downto idx60-0 do
    begin
      aMaCross:=GetCross(i);

      //Открываем ордер
      if aMaCross>0 then
        aOpen:=1
      else if aMaCross<0 then
        aOpen:=-1;

      if aOpen<>0 then
        break;
    end;

    if aOpen<>0 then
    begin
      //BUY
      if (aOpen=1) and (LastOrderType<>lotBuy) then
      begin
        CloseAllOrders(okSell,('Trader: Open opposite'));
        TrackOrderLevels(aMALevel);

        aOpenedOrder:=OpenMASnappedOrder(okBuy,aMALevel);
        FLastOpenOrderTime:=aTime60;
      end
      //SELL
      else if (aOpen=-1) and (LastOrderType<>lotSell) then
      begin
        CloseAllOrders(okBuy,('Trader: Open opposite'));
        TrackOrderLevels(aMALevel);
        aOpenedOrder:=OpenMASnappedOrder(okSell,aMALevel);
        FLastOpenOrderTime:=aTime60;
      end;
    end;
  end;
end;

procedure TStockTraderMACross2.OnBeginWorkSession;
begin
  inherited;
  FLastOpenOrderTime:=0;
end;

function TStockTraderMACross2.OpenMASnappedOrder(aKind: TStockOrderKind;const aMALevel:TSCRealNumber; const aComment: string=''): IStockOrder;
var
  aOpenPrice: TSCRealNumber;
  aSpread: TSCRealNumber;
  aExpTime: TDateTime;
begin
  aSpread:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
  result:=nil;
  if aKind=okBuy then
  begin
    aOpenPrice :=aMALevel+aSpread;//GetBroker.GetCurrentPrice(GetSymbol,bpkAsk))/2+aSpread;
    if Abs(GetBroker.GetCurrentPrice(GetSymbol,bpkAsk)-aOpenPrice)<=aSpread then
      result:=OpenOrder(aKind,aComment);
  end
  else begin
    aOpenPrice := aMALevel;//+GetBroker.GetCurrentPrice(GetSymbol,bpkBid))/2-aSpread;
    if Abs(GetBroker.GetCurrentPrice(GetSymbol,bpkBid)-aOpenPrice)<=aSpread then
      result:=OpenOrder(aKind,aComment);
  end;

  if result=nil then
  begin
    result:=inherited OpenOrderAt(aKind,aOpenPrice,aComment);
    aExpTime:=TStockDataUtils.AlignTimeToLeft(GetBroker.GetCurrentTime,sti60);
    aExpTime:=GetExpirationDateTime(aExpTime);
    result.SetPendingExpirationTime(aExpTime);
  end;
end;

function TStockTraderMACross2.PriceToPoint(const aPrice: TSCRealNumber): integer;
begin
  result:=GetBroker.PriceToPoint(GetSymbol,aPrice);
end;

{ TNeedCloseAttribute }

function TNeedCloseAttribute.GetExpirationDateTime: TDateTime;
begin
  result:=FExpirationDateTime;
end;

procedure TNeedCloseAttribute.SetExpirationDateTime(const aDateTime: TDateTime);
begin
  FExpirationDateTime:=aDateTime;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','MA Cross 2',TStockTraderMACross2,IStockTraderMACross2);
end.





