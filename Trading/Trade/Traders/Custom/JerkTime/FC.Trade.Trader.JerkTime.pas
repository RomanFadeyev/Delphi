{-----------------------------------------------------------------------------
 Author:    Roman  Fadeyev
 Purpose:   �������, ���������� �� ���������� �������� ���� �� ����� ������
            ��������
 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Trader.JerkTime;

{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map,Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //���� ����� ��������. ����� ��� ��������, ������� � Definitions
  IStockTraderJerkTime = interface
  ['{2A17579B-F7F0-4531-95CE-884A6A2346DF}']
  end;

  //������ �� ����������� �����
  IStockOppositeOrderLink = interface (ISCAttribute)
  ['{E13E24AD-8E76-4B16-8FE6-D32E601DDA1B}']
    function GetOrder: IStockOrder;
  end;

  //������� ����, ��� ���� ����� �����������
  IStockHedgeSign = interface (ISCAttribute)
  ['{5DC8D74C-2F2E-4F7F-8E37-02EC21B27D27}']
  end;

  TStockTraderJerkTime = class (TStockTraderBase,IStockTraderJerkTime)
  private
    FBarsM1: ISCIndicatorBars;
    FPbSARM5 : ISCIndicatorParabolicSAR;
    FPbSARM15 : ISCIndicatorParabolicSAR;
    FPbSARM60 : ISCIndicatorParabolicSAR;
    FCalendar : ISCIndicatorCalendar;
    FPassedTimes : TMap<TDateTime,Boolean>;

    FJustClosedOrders: IStockOrderCollection;
    FJustOpenedOrders: IStockOrderCollection;
  protected
    function  CreateBarsIndicator(const aChart: IStockChart): ISCIndicatorBars;
    function  CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;
    function  CreatePbSARIndicator(const aChart: IStockChart): ISCIndicatorParabolicSAR;

    //���� �������������� ������������ ������ ��� ���������� ������
    procedure CalcHedgingValues(const aOrder: IStockOrder;  out aOP, aSL,aTP,aTS: TStockRealNumber);
    procedure SetHedgingOrderValues(const aOrder,aHedgingOrder: IStockOrder);

    //��� ���������� ������ ���� ��� ����������� ����� (���� ����)
    function  GetOppositeOrder(const aOrder: IStockOrder): IStockOrder;

    //��������� Trailing Stop �� ��������� ������� ��������
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;

    //����������� ������-����������� ��� �����������, ������������ ������ � ����� ������ �������
    //���������� ��� ������������� �������� �������� (��. SetBroker)
    procedure OnBeginWorkSession; override;

    //������� �� ��������� "������" ������ (������� ��� ������ ���������  OpenOrder.... ��� CreateEmptyOrder)
    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs); override;

    //������� ����-������
    procedure OpenWaitingOrders;

    procedure AnalyzeOpenedOrder(const aOrder: IStockOrder; const aTime: TDateTime);

    //����� ��� ������������
    function TestBenchDialogClass: TClass; override; //TClass = TfmTestBenchDialogClass
  public
    procedure SetProject(const aValue : IStockProject); override;

    //���������
    procedure UpdateStep2(const aTime: TDateTime); override;

    function IsJerkTime(const aTime: TDateTime): boolean; overload;

    property PbSARM5 : ISCIndicatorParabolicSAR read FPbSARM5;
    property PbSARM15 : ISCIndicatorParabolicSAR read FPbSARM15;
    property PbSARM60 : ISCIndicatorParabolicSAR read FPbSARM60;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

  TStockOppositeOrderLink = class (TNameValuePersistentObjectRefCounted,IStockOppositeOrderLink,ISCAttribute)
  private
    FOrder: IStockOrder;
  public
    function GetOrder: IStockOrder;

    constructor Create(aOrder: IStockOrder);
  end;

  TStockHedgeSign = class (TNameValuePersistentObjectRefCounted,IStockHedgeSign,ISCAttribute)
  end;

implementation
  uses Types,DateUtils,Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils,FC.Trade.Trader.JerkTime.TestBenchDialog;

const
  MaxWaitingTime = 30/1440;

{ TStockTraderJerkTime }

procedure TStockTraderJerkTime.AnalyzeOpenedOrder(const aOrder: IStockOrder;const aTime: TDateTime);
var
  aValue: TStockRealNumber;
begin
  //���� ���� ��� �� � ���������, �� ������� ���
  if  (GetExpectedLoss(aOrder)>0) and (aOrder.GetCurrentProfit>=GetBroker.PointToPrice(aOrder.GetSymbol,10)) then
  begin
    aValue:=aOrder.GetOpenPrice+OrderKindSign[aOrder.GetKind]*0.0005;
    if MoveStopLossCloser(aOrder, aValue) then
      GetBroker.AddMessage(aOrder,'��������� ���� � �/�');
  end;
end;

procedure TStockTraderJerkTime.CalcHedgingValues(const aOrder: IStockOrder; out aOP, aSL, aTP, aTS: TStockRealNumber);
begin
  //Open
  aOP:=GetBroker.RoundPrice(aOrder.GetSymbol,GetExpectedStopLossPrice(aOrder));
  //Stop Loss
  aSL:=GetBroker.RoundPrice(aOrder.GetSymbol,aOP +(aOrder.GetOpenPrice-aOrder.GetStopLoss) / 2);
  //Take Profit
  aTP:=GetBroker.RoundPrice(aOrder.GetSymbol,aOP - OrderKindSign[aOrder.GetKind]*(GetExpectedLoss(aOrder) +
                             GetBroker.PointToPrice(aOrder.GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread) ) / 2);
  //Trailing Stop
  aTS:=GetBroker.RoundPrice(aOrder.GetSymbol,GetExpectedLoss(aOrder) / 2);
end;

constructor TStockTraderJerkTime.Create;
begin
  inherited Create;
  //-------
  FJustClosedOrders:=TStockOrderCollection.Create();
  FJustOpenedOrders:=TStockOrderCollection.Create();
  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
end;

destructor TStockTraderJerkTime.Destroy;
begin
  inherited;
  FJustClosedOrders:=nil;
  FJustOpenedOrders:=nil;
  FreeAndNil(FPassedTimes);
end;

procedure TStockTraderJerkTime.Dispose;
begin
  inherited;
end;

function TStockTraderJerkTime.GetOppositeOrder(const aOrder: IStockOrder): IStockOrder;
var
  i: integer;
begin
  result:=nil;
  i:=aOrder.GetAttributes.IndexOf(IStockOppositeOrderLink);
  if i<>-1 then
    result:=(aOrder.GetAttributes[i] as IStockOppositeOrderLink).GetOrder;
end;

function TStockTraderJerkTime.CreateBarsIndicator(const aChart: IStockChart): ISCIndicatorBars;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorBars,'IndicatorBars',true, aCreated) as ISCIndicatorBars;

  //������ �� �����, �������� ������ ��������
  if aCreated then
  begin
    //result.GetJerkTime.SetPeriod(21);
    //result.GetJerkTime.SetDeviations(1.5);

    //result.GetMaxJerkTime.SetPeriod(20);
    //result.GetMaxJerkTime.SetDeviations(3);
  end;
end;

function TStockTraderJerkTime.CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorCalendar,'IndicatorCalendar-'+aChart.StockSymbol.GetTimeIntervalName,true, aCreated) as ISCIndicatorCalendar;
  if aCreated  then
    result.SetCountryFilter('���;��������;��������');
end;

function TStockTraderJerkTime.CreatePbSARIndicator(const aChart: IStockChart): ISCIndicatorParabolicSAR;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorParabolicSAR,'IndicatorPbSAR'+aChart.StockSymbol.GetTimeIntervalName,true, aCreated) as ISCIndicatorParabolicSAR;
end;

procedure TStockTraderJerkTime.SetHedgingOrderValues(const aOrder, aHedgingOrder: IStockOrder);
var
  aSL,aTP,aTS,aOP: TStockRealNumber;
begin
  if aHedgingOrder.GetState<>osPending then
    raise EAlgoError.Create; //���� ������

  //��� ��������� �����������. ����� ������������� ����������� �����, ���� �� � �������
  //����, �������, ��������� ������� ������, �� ����� ��������. � ����� ���� ���������
  if aOrder.GetCurrentProfit<0 then
  begin
    CalcHedgingValues(aOrder,aOP,aSL,aTP,aTS);

    if not SameValue(aOP,aHedgingOrder.GetPendingOpenPrice) then
    begin
      aHedgingOrder.SetPendingOpenPrice(aOP);
      aHedgingOrder.SetTakeProfit(aTP);
      aHedgingOrder.SetStopLoss(aSL);
      aHedgingOrder.SetTrailingStop(aTS);
    end;
  end;
end;

procedure TStockTraderJerkTime.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
  begin
    FBarsM1:=nil;
    FPbSARM5:=nil;
    FPbSARM15:=nil;
    FPbSARM60:=nil;

    while ExpertCount>0 do
      DeleteExpert(0);
  end;

  if aValue <> nil then
  begin
    //������ ������ ��� ���������
    FCalendar:=CreateCalendarIndicator(aValue.GetStockChart(sti60));
    FBarsM1:=CreateBarsIndicator(aValue.GetStockChart(sti1));
    FPbSARM5:=CreatePbSARIndicator(aValue.GetStockChart(sti5));
    FPbSARM15:=CreatePbSARIndicator(aValue.GetStockChart(sti15));
    FPbSARM60:=CreatePbSARIndicator(aValue.GetStockChart(sti60));
  end;
end;

procedure TStockTraderJerkTime.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
begin
  inherited;
end;

function TStockTraderJerkTime.TestBenchDialogClass: TClass;
begin
  result:=TfmJerkTimeTestBenchDialog;
end;

procedure TStockTraderJerkTime.UpdateStep2(const aTime: TDateTime);
var
  aNextMinute: TDateTime;
  i: Integer;
  aOrder: IStockOrder;
begin
  //������ ����� ������� ������ � ��� ���. � ��� � ������ ��� ���������,
  //�� ����� ��� �������. ���� �� �� ������, �� ����������� � ���� �� ������� �� ��
  //������, ���� �� ����� ������� �� ��������. ���� �� �� �������, �������
  //�� �������� � ��� �� ������� ����� ����� ������������
  RemoveClosedOrders;


  for i := 0 to GetOrders.Count-1 do
  begin
    aOrder:=GetOrders[i];
    //��������� ������, � ���� ��� ���� ������� ����� - �������
    if (aOrder.GetState=osPending) and (aTime-aOrder.GetPendingOpenTime>MaxWaitingTime) then
    begin
      GetBroker.AddMessage(aOrder,'��������, ����� �����');
      aOrder.RevokePending;
    end;

    if (aOrder.GetState=osOpened) then
      AnalyzeOpenedOrder(aOrder,aTime);
  end;

  //�������, ��� ����� ����� ��� ����������
  aNextMinute:=IncMinute(aTime);
  if IsJerkTime(aNextMinute) then
  begin
    //GetBroker.AddMessage(FormatDateTime('DD.MM.YYYY ddd hh:mm',aTime));
    //OpenWaitingOrders;
  end;
end;

procedure TStockTraderJerkTime.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
end;

procedure TStockTraderJerkTime.OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  inherited;
  if aModifyEventArgs.ModifyType=omtClose then
    FJustClosedOrders.Add(aOrder)
  else if aModifyEventArgs.ModifyType=omtOpen then
    FJustOpenedOrders.Add(aOrder);
end;

function TStockTraderJerkTime.IsJerkTime(const aTime: TDateTime): boolean;
var
  aTime_: TDateTime;
  aItems: TIntegerDynArray;
begin
  aTime_:=TStockDataUtils.AlignTimeToLeft(aTime,sti1);

  //������ � ������������ ������
  result:=(Frac(aTime)>=EncodeTime(14,30,0,0)) and (Frac(aTime)<=EncodeTime(19,30,0,0));
  if result then
  begin
    result:=false;

    aItems:=FCalendar.GetItems(aTime_);
    if Length(aItems)>0 then
    begin
      GetBroker.AddMessage(Format('%s ��������� %d �������',[DateTimeToStr(aTime_),Length(aItems)]));
      result:=true;
    end;
  end;
end;

procedure TStockTraderJerkTime.OpenWaitingOrders;
const
  aInitialGap = 10;
var
  aOrderBuy,aOrderSell: IStockOrder;
begin
  aOrderBuy:=OpenOrderAt(
     okBuy,
     GetBroker.GetCurrentPrice(GetSymbol,bpkAsk)+GetBroker.PointToPrice(GetSymbol,aInitialGap),
     '���� �������� �����');

  aOrderSell:=OpenOrderAt(
     okSell,
     GetBroker.GetCurrentPrice(GetSymbol,bpkBid)-GetBroker.PointToPrice(GetSymbol,aInitialGap),
     '���� �������� ����');

  aOrderBuy.GetAttributes.Add(TStockOppositeOrderLink.Create(aOrderSell));
  aOrderSell.GetAttributes.Add(TStockOppositeOrderLink.Create(aOrderBuy));
end;

{ TStockOppositeOrderLink }

constructor TStockOppositeOrderLink.Create(aOrder: IStockOrder);
begin
  inherited Create;
  FOrder:=aOrder;
end;

function TStockOppositeOrderLink.GetOrder: IStockOrder;
begin
  result:=FOrder;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Jerk Time EURUSD m1',TStockTraderJerkTime,IStockTraderJerkTime);
end.




