unit FC.Trade.Trader.Pips;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderPips = interface
  ['{E8728AFA-2446-4072-8C03-786D3640A09D}']
  end;

  //Свойства ордера
  TStockOrderProperties=class;
  IStockOrderProperties = interface (ISCAttribute)
  ['{051E6BF5-46FC-4A83-A365-D5B421CF6A89}']
    function GetObject:TStockOrderProperties;
  end;

  TStockOrderProperties = class (TNameValuePersistentObjectRefCounted,IStockOrderProperties,ISCAttribute)
  public
    //Когда перевернулся 1H - тренд не в нашу сторону
    H1TrendTurnTime:TDateTime;

    //from IStockOrderProperties
    function GetObject:TStockOrderProperties;
  end;

  TStockTraderPips = class (TStockTraderBase,IStockTraderPips)
  private
    //Это временные переменные, испольщуемые при трейдинге
    FMA: ISCIndicatorMA;
    FMAIndex: integer;

    FPassedTimes:TMap<TDateTime,Boolean>;
    FPassedTimes2:TMap<TDateTime,Boolean>;
  protected
    //Отрубаем стандартные механизмы установки ST TP
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;
    procedure SetStopLossAccordingProperty(aOrder: IStockOrder); override;

    function  CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
    procedure InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);

    //форма для тестирования
    function TestBenchDialogClass: TClass; override;

    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;

    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,DateUtils, SystemService, Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);

const
  aOrderMargin=0.0005;

{ TStockTraderPips }

constructor TStockTraderPips.Create;
begin
  inherited Create;
  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
  FPassedTimes2:=TMap<TDateTime,Boolean>.Create;
end;

destructor TStockTraderPips.Destroy;
begin
  inherited;
  FreeAndNil(FPassedTimes);
  FreeAndNil(FPassedTimes2);  
end;

procedure TStockTraderPips.Dispose;
begin
  inherited;
end;

function TStockTraderPips.CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'MA-'+aChart.StockSymbol.GetTimeIntervalName,true,aCreated) as ISCIndicatorMA;

  if aCreated then
  begin
    InitMA(result,aChart.StockSymbol.TimeInterval);
  end;
end;

procedure TStockTraderPips.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
  FPassedTimes2.Clear;
end;

procedure TStockTraderPips.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderPips.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
    while ExpertCount>0 do
      DeleteExpert(0);

  if aValue <> nil then
  begin
    FMA:=CreateIndicatorMA(aValue.GetStockChart(sti60));
  end;
end;

procedure TStockTraderPips.SetStopLossAccordingProperty(aOrder: IStockOrder);
begin
  //Ничего!
end;

procedure TStockTraderPips.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
begin
  //Ничего!
end;

function TStockTraderPips.TestBenchDialogClass: TClass;
begin
  result:=nil;
end;

function TStockTraderPips.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderPips.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderPips.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  aOrder: IStockOrder;
  aBid{,aAsk}: TStockRealNumber;
  aOpen : TStockRealNumber;
begin
  for i := 0 to GetOrders.Count - 1 do
  begin
    aOrder:=GetOrders.Items[i];
    if aOrder.GetState=osPending then
      if MinutesBetween(aTime,aOrder.GetCreateTime)>60 then
        aOrder.RevokePending();
  end;
  RemoveClosedOrders;


  if LastOrderType<>lotNone then
   exit;

  FMAIndex:=FMA.GetInputData.FindExactMatched(aTime);
  if FMAIndex=-1 then
    raise EAlgoError.Create;

  //Раньше нельзя начинать
  if FMAIndex<max(FMA.GetPeriod,4) then
    exit;

  aBid:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
  //aAsk:=GetBroker.GetCurrentPrice(GetSymbol,bpkAsk);

  if (FMA.GetValue(FMAIndex-2)<FMA.GetValue(FMAIndex-1)) and
     (FMA.GetValue(FMAIndex-1)<FMA.GetValue(FMAIndex)) then
  begin
    aOpen:=aBid+Spread*2;//+ToPrice(1);
    OpenOrderAt(okSell,
                aOpen,
                PropLotDefaultRateSize.Value,
                aOpen+Spread*2,
                aOpen-Spread*2,
                0
                );
  end;
end;

procedure TStockTraderPips.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderPips.InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);
begin
  aMA.SetMAMethod(mamSimple);
  aMA.SetShift(0);
  aMA.SetPeriod(34);
  aMA.SetApplyTo(atClose);
end;



{ TStockOrderProperties }

function TStockOrderProperties.GetObject: TStockOrderProperties;
begin
  result:=self;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Pips',TStockTraderPips,IStockTraderPips);
end.




