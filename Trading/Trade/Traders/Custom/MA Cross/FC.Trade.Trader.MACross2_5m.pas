unit FC.Trade.Trader.MACross2_5m;

{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderMACross2_5m = interface
  ['{03B06CB7-7B4F-4018-9D18-C76DAFC9D2FE}']
  end;

  TStockTraderMACross2_5m = class (TStockTraderBase,IStockTraderMACross2_5m)
  private
    FBarHeightD1 : ISCIndicatorBarHeight;
    FFastMA_M5,FSlowMA_M5: ISCIndicatorMA;
    //FLWMA21_M5,FLWMA55_M5: ISCIndicatorMA;
    //FMA84_M5,FMA220_M5: ISCIndicatorMA;
    FLastOpenOrderTime : TDateTime;
    FPropFullBarsOnly  : TPropertyYesNo;
  protected
    function  CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
    function  CreateMA_M5(const aChart: IStockChart; aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;

    //Считает, на какой примерно цене сработает Stop Loss или Trailing Stop
    function  GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;

    //Считает, какой убыток будет, если закроется по StopLoss или Trailing Stop
    function  GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;

    procedure CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
    procedure CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
    function GetRecommendedLots: TStockOrderLots; override;

    procedure SetTP(const aOrder: IStockOrder; const aTP: TStockRealNumber;  const aComment: string);

    function  GetMainTrend(index: integer): TSCRealNumber;
    function  GetFastTrend(index: integer): TSCRealNumber;
    function  GetCross(index: integer): integer;
    function  PriceToPoint(const aPrice: TSCRealNumber): integer;
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

{ TStockTraderMACross2_5m }

procedure TStockTraderMACross2_5m.CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) then
      if aOrders[i].GetState=osOpened then
        CloseOrder(aOrders[i],aComment)
      else if aOrders[i].GetState=osPending then
        aOrders[i].RevokePending;
  end;
end;

procedure TStockTraderMACross2_5m.CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
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

constructor TStockTraderMACross2_5m.Create;
begin
  inherited Create;

  FPropFullBarsOnly   := TPropertyYesNo.Create('Method','Full Bars Only',self);
  FPropFullBarsOnly.Value:=true;

  RegisterProperties([FPropFullBarsOnly]);
  //UnRegisterProperties([PropLotDefaultRateSize,PropLotDynamicRate]);
end;

function TStockTraderMACross2_5m.CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
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

function TStockTraderMACross2_5m.CreateMA_M5(const aChart: IStockChart;aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'MA'+IntToStr(integer(aMethod))+'_'+IntToStr(aPeriod)+'_M5',true, aCreated) as ISCIndicatorMA;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetMAMethod(aMethod);
    Result.SetPeriod(aPeriod);
  end;
end;


destructor TStockTraderMACross2_5m.Destroy;
begin
  inherited;
end;

procedure TStockTraderMACross2_5m.Dispose;
begin
  inherited;
end;

function TStockTraderMACross2_5m.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if aOrder.GetState=osOpened then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;

function TStockTraderMACross2_5m.GetFastTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FLWMA21_M5.GetValue(index)-FLWMA55_M5.GetValue(index);
end;

function TStockTraderMACross2_5m.GetRecommendedLots: TStockOrderLots;
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

function TStockTraderMACross2_5m.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

procedure TStockTraderMACross2_5m.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //Создае нужных нам экспертов
    FBarHeightD1:=CreateBarHeightD1(aValue.GetStockChart(sti1440));

    FFastMA_M5:= CreateMA_M5(aValue.GetStockChart(sti5),252,mamSimple);
    FSlowMA_M5:= CreateMA_M5(aValue.GetStockChart(sti5),600,mamSimple);

//    FLWMA21_M5:= CreateMA_M5(aValue.GetStockChart(sti5),21,mamLinearWeighted);
//    FLWMA55_M5:= CreateMA_M5(aValue.GetStockChart(sti5),55,mamLinearWeighted);

//    FMA84_M5:= CreateMA_M5(aValue.GetStockChart(sti5),84,mamSimple);
    //FMA220_M5:= CreateMA_M5(aValue.GetStockChart(sti5),220,mamSimple);
  end;
end;

procedure TStockTraderMACross2_5m.SetTP(const aOrder: IStockOrder;const aTP: TStockRealNumber;  const aComment: string);
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


function TStockTraderMACross2_5m.GetMainTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FMA84_M5.GetValue(index)-FMA220_M5.GetValue(index);
end;

function TStockTraderMACross2_5m.GetCross(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FFastMA_M5.GetValue(index)-FSlowMA_M5.GetValue(index));
  x2:=Sign(FFastMA_M5.GetValue(index-1)-FSlowMA_M5.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

procedure TStockTraderMACross2_5m.UpdateStep2(const aTime: TDateTime);
var
  idx5: integer;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
  aOpenedOrder: IStockOrder;
  aOpen : integer;
  aMaCross : integer;
  aTime5 : TDateTime;
  //aFastTrend : TSCRealNumber;
  //aPrice : TSCRealNumber;
  i: Integer;
begin
  if FPropFullBarsOnly.Value then
  begin
    if ((MinuteOf(aTime) mod 5)<>0) then
      exit;
  end;

  if SecondOf(aTime)<>0 then
    exit;

  aTime5:=TStockDataUtils.AlignTimeToLeft(aTime,sti5);
  if SameDateTime(FLastOpenOrderTime,aTime5) then
    exit;

  //Брокер может закрыть ордера и без нас. У нас в списке они останутся,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

  aChart:=GetParentStockChart(FFastMA_M5);
  aInputData:=aChart.GetInputData;
  idx5:=aChart.FindBar(aTime);

  if (idx5<>-1) and (idx5>=FSlowMA_M5.GetPeriod) then
  begin
    aOpen:=0;
    for i := idx5 downto idx5-0 do
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
        aOpenedOrder:=OpenMASnappedOrder(okBuy,FFastMA_M5.GetValue(idx5));
        FLastOpenOrderTime:=aTime5;
      end
      //SELL
      else if (aOpen=-1) and (LastOrderType<>lotSell) then
      begin
        CloseAllOrders(okBuy,('Trader: Open opposite'));
        aOpenedOrder:=OpenMASnappedOrder(okSell,FFastMA_M5.GetValue(idx5));
        FLastOpenOrderTime:=aTime5;
      end;
    end;
  end;
end;

procedure TStockTraderMACross2_5m.OnBeginWorkSession;
begin
  inherited;
  FLastOpenOrderTime:=0;
end;

function TStockTraderMACross2_5m.OpenMASnappedOrder(aKind: TStockOrderKind;const aMALevel:TSCRealNumber; const aComment: string=''): IStockOrder;
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
    aExpTime:=TStockDataUtils.AlignTimeToLeft(GetBroker.GetCurrentTime,sti5);
    aExpTime:=IncMinute(aExpTime,63*5+1);
    result.SetPendingExpirationTime(aExpTime);
  end;
end;

function TStockTraderMACross2_5m.PriceToPoint(const aPrice: TSCRealNumber): integer;
begin
  result:=GetBroker.PriceToPoint(GetSymbol,aPrice);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','MA Cross 2 (5 min)',TStockTraderMACross2_5m,IStockTraderMACross2_5m);
end.





