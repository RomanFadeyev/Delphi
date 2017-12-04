{-----------------------------------------------------------------------------
 TrendFollowert Name:
 Author:    Roman
 Purpose:


  ��������:

  ��� �������� ���������� �� �������� ���������� ���. ������ ��� ������ �� ������.
  �������� - �������� ������� ������ ��������.
  ���������� ����� - ���������������� �������, ������ 3 ����� ������ (� �������) - 3

  �������:

  1.  ����������� ����� ����� ������� - �������� ������ ��� ������� �+5�. (���� ����� ����������),
                                                        ��� ������� L-5�  (���� ����� ����������).
  2.  ������� ������ ����� ������� - �������� ����� �� ������� �+5 �.
      ������� ������� ����� ������� - �������� ����� �� ������� L-5�
  3.  ����� �������� ������ ��� ������������� ���������������� �������.
  4.  ��������� ���� ��������� ������ � �� �� �������, �� ����� �������� (���� ��� �������, ���� ��� �������)
  5.  ����� �������� ���� �������� �� ��������������� ����� -(+) 5 �. ��� �� �����.
  6.  ����� ���� ��� ����������� � ��������� (���� �������� ����� ���� (����) ������ �������� ������� ����� ��� 10 �.),
      ��� ������������� ����� �� ������� ���� ��������� ������ ��� ������� ��� �������� ���� ��������� ������ ��� �������.
      ��� �������� �� ������ ����� ����� �� �������� �������.
  7.  ����� ������� �������, �� ������� �� ���������, �������������� ������� �� ���������.
  8.  ���� � ������� ���������� �� �����������.
  9.  ���� ����� �������� ������� �� ����� ������� � ���������, � ��� ����. ����� ������������� ���� ���� �������� �������, ������ ���� +5�.
  10. ���� ������� � ��������� ��� �� �����, ��������� ����� �� �������� �������,
      �� ���� � ��� �� ����������� (���� / ���� ���������� 3 ������), ��������� ������� 1.
  11. ���� ����� ��������� � ����� � ������� ��������, ���������� �� �������, � ����� �� ������ ��������,
      � ��������, ��� ����. ����� �������� �� �������, ��������� ������� 1.

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Trader.AverageBreakthrough;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map,Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderAverageBreakthrough = interface
  ['{797ED06B-5E31-4DD4-8FBE-BEC5CDEF31E1}']
  end;

  //�������� ������
  TStockOrderProperties=class;
  IStockOrderProperties = interface (ISCAttribute)
  ['{FC5C8FCF-7C5E-49AB-A30F-99830E7AC5DF}']
    function GetObject:TStockOrderProperties;
  end;

  //������� "��������" ������
  IStockLargeOrderAttribute = interface (ISCAttribute)
  ['{1197FB87-425F-423A-B546-A6C814832F9C}']
  end;

  //������� "����������" ������
  IStockSmallOrderAttribute = interface (ISCAttribute)
  ['{BDE21E2D-9D99-4E8B-8FF0-566041187DFC}']
  end;

  TStockOrderProperties = class (TNameValuePersistentObjectRefCounted,IStockOrderProperties,ISCAttribute)
  public
    //����� ������������ 1H - ����� �� � ���� �������
    H1TrendTurnTime:TDateTime;

    //from IStockOrderProperties
    function GetObject:TStockOrderProperties;
  end;

  TStockLargeOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockLargeOrderAttribute,ISCAttribute)
  end;

  //������� "����������" ������
  TStockSmallOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockSmallOrderAttribute,ISCAttribute)
  end;


  TStockTraderAverageBreakthrough = class (TStockTraderBase,IStockTraderAverageBreakthrough)
  private
    //��� ��������� ����������, ������������ ��� ���������
    FMA: ISCIndicatorMA;
    FMAIndex: integer;
    FLow: array [0..4] of TSCRealNumber;
    FHigh: array [0..4] of TSCRealNumber;
    FOpen: array [0..4] of TSCRealNumber;
    FClose: array [0..4] of TSCRealNumber;

    FPassedTimes:TMap<TDateTime,Boolean>;
    FPassedTimes2:TMap<TDateTime,Boolean>;
  protected
    function GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;

    procedure GetProperties(aList: TPropertyList); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;

    //�������� ����������� ��������� ��������� ST TP
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;
    procedure SetStopLossAccordingProperty(aOrder: IStockOrder); override;

    function  CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
    procedure InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);

    //����� ��� ������������
    function TestBenchDialogClass: TClass; override;

    function  Spread: TSCRealNumber;

    function  SignalToOpen(const aTime: TDateTime): integer;
    procedure TryOpenOrder(aOrder: IStockOrder; const aTime: TDateTime);
    //����������� ����������� SL � ���������
    procedure MoveSLToProfitablePoint(const aOrder: IStockOrder; const aTime: TDateTime);
    procedure AnalyzeOpenedOrder(const aOrder: IStockOrder; const aTime: TDateTime);

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;

    //���������
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

{ TStockTraderAverageBreakthrough }

constructor TStockTraderAverageBreakthrough.Create;
begin
  inherited Create;
  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
  FPassedTimes2:=TMap<TDateTime,Boolean>.Create;
end;

destructor TStockTraderAverageBreakthrough.Destroy;
begin
  inherited;
  FreeAndNil(FPassedTimes);
  FreeAndNil(FPassedTimes2);  
end;

procedure TStockTraderAverageBreakthrough.Dispose;
begin
  inherited;
end;

function TStockTraderAverageBreakthrough.CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'ParabolicSAR-'+aChart.StockSymbol.GetTimeIntervalName,true,aCreated) as ISCIndicatorMA;

  if aCreated then
  begin
    InitMA(result,aChart.StockSymbol.TimeInterval);
  end;
end;

procedure TStockTraderAverageBreakthrough.GetProperties(aList: TPropertyList);
begin
  inherited;
end;

procedure TStockTraderAverageBreakthrough.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
  FPassedTimes2.Clear;
end;

procedure TStockTraderAverageBreakthrough.OnPropertyChanged(aNotifier: TProperty);
begin
  inherited;
end;

procedure TStockTraderAverageBreakthrough.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderAverageBreakthrough.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
    while ExpertCount>0 do
      DeleteExpert(0);

  if aValue <> nil then
  begin
    FMA:=CreateIndicatorMA(aValue.GetStockChart(sti1440));
  end;
end;

procedure TStockTraderAverageBreakthrough.SetStopLossAccordingProperty(aOrder: IStockOrder);
begin
  //������!
end;

procedure TStockTraderAverageBreakthrough.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
begin
  //������!
end;

function TStockTraderAverageBreakthrough.TestBenchDialogClass: TClass;
begin
  result:=nil;
end;

function TStockTraderAverageBreakthrough.SignalToOpen(const aTime: TDateTime): integer;
var
  aMA: TSCRealNumber;
begin
  result:=0;

  if FMAIndex<FMA.GetPeriod+1 then
    exit;

  aMA:=FMA.GetValue(FMAIndex);

  // ����������� ������� ����� �����, ����� ���������� - ��������, ��. ��.1
  if ((FOpen[0] <=aMA) and (FClose[0] >aMA)) then
  begin
    result:=1;
    exit;
  end;

  // ����������� ������� ����� �����, ����� ���������� - �������, ��. ��.1
  if ((FOpen[0] >=aMA) and (FClose[0] <aMA)) then
  begin
    result:=-1;                       
    exit;
  end;

  // ������� ������� ������ ����� ����� - ����� �� �������,��. ��.2
  // �������� ������� �����������, ����� ����� ����������� �� ����� ��� �� 10pt
  if (FOpen[0] > aMA) and (FClose[0] > aMA) and (FLow[0] <=aMA) {and (aMA-FLow[0]<0.0010) }then
  begin
    result:=2;
    exit;
  end;
   //  else result:=(-3); // (�������,  ��. ��.11)  !!! ��� ���� ���������� ������� �������� ������� �������

  // ������� ������� ������� ����� ����� - ����� �� �������
  // �������� ������� �����������, ����� ����� ����������� �� ����� ��� �� 10pt
  if (FOpen[0] < aMA) and (FClose[0] < aMA) and (FHigh[0] >=aMA) {and (FHigh[0]-aMA<0.0010) }then
  begin
    result:=-2;
    exit;
  end;
   //  else result:=( 3);  // (��������, ��. ��.11)  !!! ��� ���� ���������� ������� �������� ������� �������

(*
  if ((FHigh[1]>FHigh[2]) and (FHigh[1]>FHigh[3]) and (FHigh[1]>FHigh[4])) then
  begin
    result:=( 4); // ��������� ����� ���� ���������� ���� (��������, ��. ��.10)
    exit;
  end;

  if (( FLow[1]< FLow[2]) and ( FLow[1]< FLow[3]) and ( FLow[1]< FLow[4])) then
  begin
    result:=(-4); // ��������� ����� ���� ���������� ���� (��������, ��. ��.10)
    exit;
  end;
*)
end;

function TStockTraderAverageBreakthrough.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderAverageBreakthrough.TryOpenOrder(aOrder: IStockOrder; const aTime: TDateTime);
var
  aOP,aSL,aTP: TSCRealNumber;
  aSignalToOpen: integer;
  aDelete: boolean;
begin
  aSignalToOpen:=SignalToOpen(aTime);

  // ���� ����� ���������� ����� � ���� ������ �� ����� �����, ��:
  if (aOrder<>nil) and (aOrder.GetState=osPending) and (aSignalToOpen<>0) then
  begin
    aDelete:=false;

    if ((aOrder.GetKind=okBuy) and (aSignalToOpen <0)) then // 1) ���� ������ ��������������� ������ (��. ��.3),
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'������ ��������������� ������ Sell (��. ��.3)');
    end
    else if((aOrder.GetKind=okSell) and (aSignalToOpen >0)) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'������ ��������������� ������ Buy (��. ��.3)');
    end
     // 2) ���� ������ ������ � �� �� �������, ��� � ������ (��. ��.4),
    else if (aOrder.GetKind=okBuy) and (aSignalToOpen >0) and  (aOrder.GetPendingOpenPrice>FHigh[0]+aOrderMargin+Spread) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'������ ������ Buy � �� �� �������, ��������������� (��. ��.4)');
    end
    else if (aOrder.GetKind=okSell) and  (aSignalToOpen <0) and (aOrder.GetPendingOpenPrice<FLow[0]-aOrderMargin) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'������ ������ Sell � �� �� �������, ��������������� (��. ��.4)');      
    end;

    if aDelete then
      aOrder.RevokePending //������ ������� �����
    else
      exit;
  end;

  if (aSignalToOpen >0) then
  begin
    aOP := FHigh[0]+aOrderMargin+Spread;
    //���� �������� �� ��������������� ����� -(+) 5 �. ��� �� �����.
    aSL := FLow[0]-aOrderMargin;
    aTP := 0; //TP=OP+1*(OP-SL);
    if aOrder=nil then
      aOrder:=CreateEmptyOrder;
    aOrder.OpenAt(GetSymbol,okBuy,aOP,GetRecommendedRate,aSL,aTP,0); // �������
    AddMessageAndSetMark(aOrder,mkArrowUp,'�������� ������ Buy')
  end
  else if (aSignalToOpen <0) then
  begin
    aOP := FLow[0]-aOrderMargin;
    //���� �������� �� ��������������� ����� -(+) 5 �. ��� �� �����.
    aSL := FHigh[0]+aOrderMargin+Spread;
    aTP := 0; //TP=OP-1*(SL-OP);
    if aOrder=nil then
      aOrder:=CreateEmptyOrder;
    aOrder.OpenAt(GetSymbol,okSell,aOP,GetRecommendedRate,aSL,aTP,0); // �������
    AddMessageAndSetMark(aOrder,mkArrowDown,'�������� ������ Sell')
  end;
end;

procedure TStockTraderAverageBreakthrough.MoveSLToProfitablePoint(const aOrder: IStockOrder; const aTime: TDateTime);
var
  aValue: TStockRealNumber;
begin
  // 1) ���� �������� ����� ��������� ����/����  ���� �������� ������� �� 10 ������� � �����, �� ������ �/�
  if  (GetExpectedLoss(aOrder)>0) and (aOrder.GetCurrentProfit>=GetBroker.PointToPrice(GetSymbol,10)) then
  begin
    aValue:=aOrder.GetOpenPrice+OrderKindSign[aOrder.GetKind]*aOrderMargin;
    if MoveStopLossCloser(aOrder, aValue) then
      GetBroker.AddMessage(aOrder,'��������� ���� � �/�');
  end;
end;

procedure TStockTraderAverageBreakthrough.AnalyzeOpenedOrder(const aOrder: IStockOrder; const aTime: TDateTime);
begin
  // 1) ���� �������� ����� ��������� ����/����  ���� �������� ������� �� 10 ������� � �����, �� ������ �/�
  MoveSLToProfitablePoint(aOrder,aTime);
  

  if (aTime-aOrder.GetOpenTime>=1-1/24) then     // ������ 1 ���� ����� �������� �������
  // 2) ����������� ���� �� �������/�������� ���� ��������� ������, �� ������ � ������� ��� ����������
  begin
    if (aOrder.GetKind=okBuy) then
    begin
      if MoveStopLossCloser(aOrder,Min(FLow[0],FLow[1])) then
        GetBroker.AddMessage(aOrder,'��������� Buy-���� ��  ������� ���� ��������� ������');
    end
    else begin
      if MoveStopLossCloser(aOrder,Max(FHigh[0],FHigh[1])) then
        GetBroker.AddMessage(aOrder,'��������� Sell-���� �� �������� ���� ��������� ������');
    end;
  end;

  if (aTime-aOrder.GetOpenTime>=2-1/24) then    // ������ 2 ��� ����� �������� �������
    if GetExpectedLoss(aOrder)>0 then
    // 3) ���� ����� �������� ������� �� ����� ������� � ���������, � ��� ����.
    //    ����� ������������� ���� ���� �������� �������, ������ ���� 5 �.
    begin
      if (aOrder.GetKind=okBuy) then
      begin
        //aOrder.SetTakeProfit(aOrder.GetOpenPrice+Spread+aOrderMargin);
        //GetBroker.AddMessage(aOrder,'�� ����� ������� � �/�. ��������� Buy-������ +5pt');
      end
      else begin
        //aOrder.SetTakeProfit(aOrder.GetOpenPrice-aOrderMargin);
        //GetBroker.AddMessage(aOrder,'�� ����� ������� � �/�. ��������� Sell-������ +5pt');
      end;
    end;
end;

procedure TStockTraderAverageBreakthrough.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  aOrder: IStockOrder;
begin
  RemoveClosedOrders;

  FMAIndex:=FMA.GetInputData.FindExactMatched(aTime);
  if FMAIndex=-1 then
    raise EAlgoError.Create;

  //������ ������ ��������
  if FMAIndex<max(FMA.GetPeriod,4) then
    exit;

  if aTime<EncodeDate(2006,1,1) then
    exit;

//      if DateTimeToStr(aTime)='20.01.2006 22:59:00' then
//        Pause(DateTimeToStr(aTime));


  // ������ �� ������ ������� ����� ������ ���� ��� ��� � ��������
  if (not FPassedTimes.Lookup(Trunc(aTime))) then
    if ((HourOfTheDay(aTime)=22) and (MinuteOfTheHour(aTime)=59)) or //���������� ������� � 22:59
       ((HourOfTheDay(aTime)=23))  then
    begin
      //��������� ��������� 4 ������
      for i := 0 to 4 do
      begin
        with FMA.GetInputData[FMAIndex-i] do
        begin
          FOpen[i]:=DataOpen;
          FHigh[i]:=DataHigh;
          FLow[i]:=DataLow;
          FClose[i]:=DataClose;
        end;
      end;

      //���������� ��� ���������� � �������� ������
      for i := 0 to GetOrders.Count-1 do
      begin
        aOrder:=GetOrders[i];
        if aOrder.GetState =osPending then
        begin
          TryOpenOrder(aOrder,aTime);
        end
        else if aOrder.GetState = osOpened then
        begin
          AnalyzeOpenedOrder(aOrder,aTime);
        end;
      end;

      //���� ��� ������� �������� �������, �� �������� ������� �����
      RemoveClosedOrders;
      if GetOrders.Count=0 then
      begin
        TryOpenOrder(nil,aTime);
      end;

      //��������� � ������� ������ � ���, ��� ��� ������ �� ��� ����������, �����
      //��������� ��� �� ����� �� ���������
      FPassedTimes.Add(Trunc(aTime),true);
    end;

  //�� �������� ������ ����� ����� ��������� ��� ��� ��������� �� �/�
  //� ����������� ���-�� ��������� ��������, ��-�� ���� ��� ������ � ����� �����, �� �� � ������
  //� ���� ����������� �/� ��� � ��������� ������
  if HourOfTheDay(aTime) in [23,0] then
  begin
    //���������� ��� ���������� � �������� ������
    for i := 0 to GetOrders.Count-1 do
    begin
      aOrder:=GetOrders[i];
      if aOrder.GetState = osOpened then
        //���� �/� ��� � �� ���������, ������� ��� �����, ������ �� ��������� �����
        if (GetExpectedLoss(aOrder)>0) and (aTime-aOrder.GetPendingOpenTime>1)  then
        begin
          MoveSLToProfitablePoint(aOrder,aTime);
          if GetExpectedLoss(aOrder)<0 then
            GetBroker.AddMessage(aOrder,'C��� � �/� ��� �������� ����� �������� ����� (22:59)');
        end;
    end;

    FPassedTimes2.Add(Trunc(aTime),true);
  end;
end;

procedure TStockTraderAverageBreakthrough.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

function TStockTraderAverageBreakthrough.GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;
var
  i: integer;
begin
  i:=aOrder.GetAttributes.IndexOf(IStockOrderProperties);
  if i=-1 then
    raise EAlgoError.Create;

  result:=(aOrder.GetAttributes.Items[i] as IStockOrderProperties).GetObject;
end;

procedure TStockTraderAverageBreakthrough.InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);
begin
  aMA.SetMAMethod(mamExponential);
  aMA.SetShift(3);
  aMA.SetPeriod(3);
  aMA.SetApplyTo(atClose);
end;



{ TStockOrderProperties }

function TStockOrderProperties.GetObject: TStockOrderProperties;
begin
  result:=self;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Average Breakthrough',TStockTraderAverageBreakthrough,IStockTraderAverageBreakthrough);
end.




