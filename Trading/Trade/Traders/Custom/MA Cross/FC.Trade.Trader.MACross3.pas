unit FC.Trade.Trader.MACross3;

{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderMACross3 = interface
  ['{1933A182-D3E9-4A1C-A69E-C87B7E4D4B1C}']
  end;

  TStockTraderMACross3 = class (TStockTraderBase,IStockTraderMACross3)
  private
    FBarHeightD1 : ISCIndicatorBarHeight;
    FSMA21_H1,FSMA55_H1: ISCIndicatorMA;
    //FLWMA21_H1,FLWMA55_H1: ISCIndicatorMA;
    //FMA84_H1,FMA220_H1: ISCIndicatorMA;
    FLastOpenOrderTime : TDateTime;
    FPropFullBarsOnly  : TPropertyYesNo;
    FPropMaxPriceAndMAGap  : TPropertyInt;
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
    function  GetCurrentBid: TSCRealNumber;

    function  OpenPendingOrder(aOrderType:TStockOrderKind; aLevel: TSCRealNumber): IStockOrder;
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    function OpenOrder(aKind: TStockOrderKind;const aComment: string=''): IStockOrder;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses DateUtils,Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;

{ TStockTraderMACross3 }

procedure TStockTraderMACross3.CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) and (aOrders[i].GetState=osOpened) then
      CloseOrder(aOrders[i],aComment);
  end;
end;

procedure TStockTraderMACross3.CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
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

constructor TStockTraderMACross3.Create;
begin
  inherited Create;

  FPropFullBarsOnly   := TPropertyYesNo.Create('Method','Full Bars Only',self);
  FPropFullBarsOnly.Value:=true;

  FPropMaxPriceAndMAGap:=TPropertyInt.Create('Method','Max gap between price and MA level',self);
  FPropMaxPriceAndMAGap.Value:=200;

  RegisterProperties([FPropFullBarsOnly,FPropMaxPriceAndMAGap]);
  //UnRegisterProperties([PropLotDefaultRateSize,PropLotDynamicRate]);
end;

function TStockTraderMACross3.CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
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

function TStockTraderMACross3.CreateMA_H1(const aChart: IStockChart;aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;
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


destructor TStockTraderMACross3.Destroy;
begin
  inherited;
end;

procedure TStockTraderMACross3.Dispose;
begin
  inherited;
end;

function TStockTraderMACross3.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if aOrder.GetState=osOpened then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;

function TStockTraderMACross3.GetFastTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FLWMA21_H1.GetValue(index)-FLWMA55_H1.GetValue(index);
end;

function TStockTraderMACross3.GetRecommendedLots: TStockOrderLots;
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

function TStockTraderMACross3.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

procedure TStockTraderMACross3.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //Создае нужных нам экспертов
    FBarHeightD1:=CreateBarHeightD1(aValue.GetStockChart(sti1440));

    FSMA21_H1:= CreateMA_H1(aValue.GetStockChart(sti60),21,mamSimple);
    FSMA55_H1:= CreateMA_H1(aValue.GetStockChart(sti60),55,mamSimple);

//    FLWMA21_H1:= CreateMA_H1(aValue.GetStockChart(sti60),21,mamLinearWeighted);
//    FLWMA55_H1:= CreateMA_H1(aValue.GetStockChart(sti60),55,mamLinearWeighted);

//    FMA84_H1:= CreateMA_H1(aValue.GetStockChart(sti60),84,mamSimple);
    //FMA220_H1:= CreateMA_H1(aValue.GetStockChart(sti60),220,mamSimple);
  end;
end;

procedure TStockTraderMACross3.SetTP(const aOrder: IStockOrder;const aTP: TStockRealNumber;  const aComment: string);
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


function TStockTraderMACross3.GetMainTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FMA84_H1.GetValue(index)-FMA220_H1.GetValue(index);
end;

function TStockTraderMACross3.GetCross(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FSMA21_H1.GetValue(index)-FSMA55_H1.GetValue(index));
  x2:=Sign(FSMA21_H1.GetValue(index-1)-FSMA55_H1.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

function TStockTraderMACross3.GetCurrentBid: TSCRealNumber;
begin
  result:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
end;

procedure TStockTraderMACross3.UpdateStep2(const aTime: TDateTime);
var
  idx60: integer;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
  aDataOpen,aDataClose : TSCRealNumber;
  aTime60 : TDateTime;
  //aFastTrend : TSCRealNumber;
  //aPrice : TSCRealNumber;
  aMALevel : TSCRealNumber;
  aBid,aAsk: TSCRealNumber;
begin
  aTime60:=TStockDataUtils.AlignTimeToLeft(aTime,sti60);
  if SameTime(FLastOpenOrderTime,aTime60) then
    exit;

  //Брокер может закрыть ордера и без нас. У нас в списке они останутся,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

  //Анализируем экcпертные оценки
  aChart:=GetParentStockChart(FSMA21_H1);
  aInputData:=aChart.GetInputData;
  idx60:=aChart.FindBar(aTime);

  if (idx60<>-1) and (idx60>=FSMA55_H1.GetPeriod) then
  begin
    aMALevel:=FSMA21_H1.GetValue(idx60);
    aBid:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
    aAsk:=GetBroker.GetCurrentPrice(GetSymbol,bpkAsk);

    aDataOpen:=aInputData.DirectGetItem_DataOpen(idx60-1);
    aDataClose:=aInputData.DirectGetItem_DataClose(idx60);

    //Поднимаемся вверх
    if (aBid<aMALevel) and (aDataClose>aDataOpen) then
    begin
      OpenPendingOrder(okBuy,aMALevel)
    end;

    //Опускаемся вниз
    if (aAsk>aMALevel) and (aDataClose<aDataOpen) then
    begin
      OpenPendingOrder(okSell,aMALevel)
    end;

    if (aBid)>aMALevel then
    begin
      //CloseAllOrders(okSell,'Cross up');
    end;

    if (aAsk)<aMALevel then
    begin
      //CloseAllOrders(okBuy,'Cross down');
    end;
  end;
end;

procedure TStockTraderMACross3.OnBeginWorkSession;
begin
  inherited;
  FLastOpenOrderTime:=0;
end;

function TStockTraderMACross3.OpenOrder(aKind: TStockOrderKind;
  const aComment: string): IStockOrder;
begin
  Result:=inherited OpenOrder(aKind,aComment);
end;

function TStockTraderMACross3.OpenPendingOrder(aOrderType: TStockOrderKind;aLevel: TSCRealNumber): IStockOrder;
var
  i: integer;
  aOrder: IStockOrder;
begin
  aLevel:=GetBroker.RoundPrice(GetSymbol,aLevel);

  for i := 0 to GetOrders.Count-1 do
  begin
    aOrder:=GetOrders.Items[i];
    if (aOrder.GetState=osPending) and (aOrder.GetKind=aOrderType) then
    begin
      result:=aOrder;
      break;
    end;
  end;

  try
    if result=nil then
      result:=OpenOrderAt(aOrderType,aLevel)
    else
      result.SetPendingOpenPrice(aLevel);
  except

  end;
end;

function TStockTraderMACross3.PriceToPoint(const aPrice: TSCRealNumber): integer;
begin
  result:=GetBroker.PriceToPoint(GetSymbol,aPrice);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','MA Cross 3',TStockTraderMACross3,IStockTraderMACross3);
end.





