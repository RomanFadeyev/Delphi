unit FC.Trade.Trader.MACross1;

{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderMACross1 = interface
  ['{DD4E918D-BAF8-4CF4-B249-F383337079A7}']
  end;

  TStockTraderMACross1 = class (TStockTraderBase,IStockTraderMACross1)
  private
    FBarHeightD1 : ISCIndicatorBarHeight;
    FSMA21_M15,FSMA55_M15, FSMA84_M15, FSMA220_M15: ISCIndicatorMA;
    FSMA21_M1,FSMA55_M1: ISCIndicatorMA;
    //FLWMA21_M15,FLWMA55_M15: ISCIndicatorMA;
    //FMA84_M15,FMA220_M15: ISCIndicatorMA;
    FLastOpenOrderTime : TDateTime;
    FPropFullBarsOnly  : TPropertyYesNo;
    FPropMaxPriceAndMAGap  : TPropertyInt;
  protected
    function  CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
    function  CreateMA(const aChart: IStockChart; aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;

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
    function  GetCross2155(index: integer): integer;
    function  GetCross84_220(index: integer): integer;
    function  GetCrossM1(index: integer): integer;

    function  PriceToPoint(const aPrice: TSCRealNumber): integer;
    function  GetCurrentBid: TSCRealNumber;
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

{ TStockTraderMACross1 }

procedure TStockTraderMACross1.CloseAllOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) then
      CloseOrder(aOrders[i],aComment);
  end;
end;

procedure TStockTraderMACross1.CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
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

constructor TStockTraderMACross1.Create;
begin
  inherited Create;

  FPropFullBarsOnly   := TPropertyYesNo.Create('Method','Full Bars Only',self);
  FPropFullBarsOnly.Value:=true;

  FPropMaxPriceAndMAGap:=TPropertyInt.Create('Method','Max gap between price and MA level',self);
  FPropMaxPriceAndMAGap.Value:=200;

  RegisterProperties([FPropFullBarsOnly,FPropMaxPriceAndMAGap]);
  //UnRegisterProperties([PropLotDefaultRateSize,PropLotDynamicRate]);
end;

function TStockTraderMACross1.CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
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

function TStockTraderMACross1.CreateMA(const aChart: IStockChart;aPeriod: integer; aMethod: TSCIndicatorMAMethod): ISCIndicatorMA;
var
  aCreated: boolean;
begin

  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,
  'MA'+IntToStr(integer(aMethod))+'_'+IntToStr(aPeriod)+StockTimeIntervalNames[aChart.StockSymbol.TimeInterval],
  true, aCreated) as ISCIndicatorMA;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetMAMethod(aMethod);
    Result.SetPeriod(aPeriod);
  end;
end;

destructor TStockTraderMACross1.Destroy;
begin
  inherited;
end;

procedure TStockTraderMACross1.Dispose;
begin
  inherited;
end;

function TStockTraderMACross1.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if aOrder.GetState=osOpened then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;

function TStockTraderMACross1.GetFastTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FLWMA21_M15.GetValue(index)-FLWMA55_M15.GetValue(index);
end;

function TStockTraderMACross1.GetRecommendedLots: TStockOrderLots;
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

function TStockTraderMACross1.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

procedure TStockTraderMACross1.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //Создае нужных нам экспертов
    FBarHeightD1:=CreateBarHeightD1(aValue.GetStockChart(sti1440));

    FSMA21_M15:= CreateMA(aValue.GetStockChart(sti15),21,mamSimple);
    FSMA21_M15.SetColor(clWebDarkGoldenRod);

    FSMA55_M15:= CreateMA(aValue.GetStockChart(sti15),55,mamSimple);
    FSMA55_M15.SetColor(clRed);

    FSMA84_M15:= CreateMA(aValue.GetStockChart(sti15),84,mamSimple);
    FSMA84_M15.SetColor(clWebSteelBlue);

    FSMA220_M15:= CreateMA(aValue.GetStockChart(sti15),220,mamSimple);
    FSMA220_M15.SetColor(clBlue);

    //M1
    FSMA21_M1:= CreateMA(aValue.GetStockChart(sti1),21,mamSimple);
    FSMA21_M1.SetColor(clWebDarkGoldenRod);

    FSMA55_M1:= CreateMA(aValue.GetStockChart(sti1),55,mamSimple);
    FSMA55_M1.SetColor(clRed);
  end;
end;

procedure TStockTraderMACross1.SetTP(const aOrder: IStockOrder;const aTP: TStockRealNumber;  const aComment: string);
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


function TStockTraderMACross1.GetMainTrend(index: integer): TSCRealNumber;
begin
  result:=0;//FMA84_M15.GetValue(index)-FMA220_M15.GetValue(index);
end;

function TStockTraderMACross1.GetCross2155(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FSMA21_M15.GetValue(index)-FSMA55_M15.GetValue(index));
  x2:=Sign(FSMA21_M15.GetValue(index-1)-FSMA55_M15.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

function TStockTraderMACross1.GetCross84_220(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FSMA84_M15.GetValue(index)-FSMA220_M15.GetValue(index));
  x2:=Sign(FSMA84_M15.GetValue(index-1)-FSMA220_M15.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

function TStockTraderMACross1.GetCrossM1(index: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FSMA21_M1.GetValue(index)-FSMA55_M1.GetValue(index));
  x2:=Sign(FSMA21_M1.GetValue(index-1)-FSMA55_M1.GetValue(index-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

function TStockTraderMACross1.GetCurrentBid: TSCRealNumber;
begin
  result:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
end;

procedure TStockTraderMACross1.UpdateStep2(const aTime: TDateTime);
var
  idx15,idx1: integer;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
  aOpenedOrder: IStockOrder;
  aOpen : integer;
  aMaCross84_220 : integer;
  aMaCross2155 : integer;
  aMaCrossM1 : integer;
  aTime15 : TDateTime;
  aClose : integer;
  //aFastTrend : TSCRealNumber;
  //aPrice : TSCRealNumber;
  j: Integer;
  aMALevel : TSCRealNumber;
  aMaxGap : TSCRealNumber;
begin
  if FPropFullBarsOnly.Value then
  begin
    if not (MinuteOf(aTime) in [55..59])then
      exit;

  end;

  aTime15:=TStockDataUtils.AlignTimeToLeft(aTime,sti15);
  if SameTime(FLastOpenOrderTime,aTime15) then
    exit;

  //Брокер может закрыть ордера и без нас. У нас в списке они останутся,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

  //Анализируем экcпертные оценки
  aChart:=GetParentStockChart(FSMA21_M15);
  aInputData:=aChart.GetInputData;
  idx15:=aChart.FindBar(aTime);

  aChart:=GetParentStockChart(FSMA21_M1);
  aInputData:=aChart.GetInputData;
  idx1:=aChart.FindBar(aTime);


  if (idx1<>-1) and (idx15<>-1) and (idx15>=FSMA84_M15.GetPeriod) then
  begin
    aOpen:=0;
    aClose:=0;

    aMaCrossM1:=GetCrossM1(idx1);
    if aMaCrossM1<>0 then
    begin
      for j:=idx15 downto idx15-12 do
      begin
        aMaCross2155:=GetCross2155(j);
        if aMaCross2155=aMaCrossM1 then
        begin
          //Открываем ордер
          if aMaCross2155>0 then
            aOpen:=1
          else if aMaCross2155<0 then
            aOpen:=-1;

          {for i := idx15 downto idx15-40*4 do
          begin
            aMaCross84_220:=GetCross84_220(i);

            if aMaCross84_220=aMaCross2155 then
            begin
              //Открываем ордер
              if aMaCross84_220>0 then
                aOpen:=1
              else if aMaCross84_220<0 then
                aOpen:=-1;
            end;

            if aMaCross84_220<>0 then
              break;
          end;}
        end;

        if aMaCross2155<>0 then
          break;
      end;
    end;

    aMaCross84_220:=GetCross84_220(idx15);
    //Открываем ордер
    if aMaCross84_220>0 then
      aClose:=-1
    else if aMaCross84_220<0 then
      aClose:=1;


    if aOpen<>0 then
    begin
      aMaxGap:=aInputData.PointToPrice(FPropMaxPriceAndMAGap.Value);
      aMALevel:=FSMA21_M15.GetValue(idx15);

      //aPrice:=aInputData.DirectGetItem_DataClose(idx15);
      //aFastTrend:=GetFastTrend(i);
      //BUY
      if (aOpen=1) {and (aFastTrend>0) }and (LastOrderType<>lotBuy) then
      begin
        CloseAllOrders(okSell,('Trader: Open opposite'));
        //if Abs(PriceToPoint(aPrice-FSMA21_M15.GetValue(idx15)))<100  then
        if Abs(GetCurrentBid-aMALevel)<aMaxGap then
        begin
          aOpenedOrder:=OpenOrder(okBuy);
          FLastOpenOrderTime:=aTime15;
        end;
      end
      //SELL
      else if (aOpen=-1) {and (aFastTrend<0)  }and (LastOrderType<>lotSell) then
      begin
        CloseAllOrders(okBuy,('Trader: Open opposite'));
        if Abs(GetCurrentBid-aMALevel)<aMaxGap then
        begin
          aOpenedOrder:=OpenOrder(okSell);
          FLastOpenOrderTime:=aTime15;
        end;
      end;
    end;

    if aClose<0 then
      CloseAllOrders(okSell,('Trader: Time to close'))
    else if aClose>0 then
      CloseAllOrders(okBuy,('Trader: Time to close'));
  end;
end;

procedure TStockTraderMACross1.OnBeginWorkSession;
begin
  inherited;
  FLastOpenOrderTime:=0;
end;

function TStockTraderMACross1.OpenOrder(aKind: TStockOrderKind;
  const aComment: string): IStockOrder;
begin
  Result:=inherited OpenOrder(aKind,aComment);
end;

function TStockTraderMACross1.PriceToPoint(const aPrice: TSCRealNumber): integer;
begin
  result:=GetBroker.PriceToPoint(GetSymbol,aPrice);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','MA Cross 1',TStockTraderMACross1,IStockTraderMACross1);
end.





