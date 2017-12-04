{-----------------------------------------------------------------------------
 TrendFollowert Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Trader.TrendFollower;
{$I Compiler.inc}
//{$DEFINE SMALL_ORDER}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderTrendFollower = interface
  ['{428097F1-A9C0-4180-959B-2EEFB291307F}']
  end;

  //�������� ������
  TStockOrderProperties=class;
  IStockOrderProperties = interface (ISCAttribute)
  ['{48CA6AA9-32DD-4F93-BB38-352A90E0C59D}']
    function GetObject:TStockOrderProperties;
  end;

  //������� "��������" ������
  IStockLargeOrderAttribute = interface (ISCAttribute)
  ['{AAF32E4E-DF5F-4779-B289-29152EC1DE80}']
  end;

  //������� "����������" ������
  IStockSmallOrderAttribute = interface (ISCAttribute)
  ['{D8A15230-1096-4EC9-AB2A-73A62D02CB66}']
  end;

  //������� ����, ��� ���� ����� �����������
  TStockNewsHedgeSign=class;
  IStockNewsHedgeSign = interface (ISCAttribute)
  ['{5DC8D74C-2F2E-4F7F-8E37-02EC21B27D27}']
    function GetObject:TStockNewsHedgeSign;
  end;

  //������ �� ����������� �����
  TStockNewsHedgingOrderLink=class;
  IStockNewsHedgingOrderLink = interface (ISCAttribute)
  ['{F8436717-4426-435C-B54D-DAAB638D426A}']
    function GetObject:TStockNewsHedgingOrderLink;
  end;

  TStockOrderProperties = class (TNameValuePersistentObjectRefCounted,IStockOrderProperties,ISCAttribute)
  public
    //����� ������������ 1H - ����� �� � ���� �������
    H1TrendTurnTime:TDateTime;
    Revoked: boolean;

    //from IStockOrderProperties
    function GetObject:TStockOrderProperties;
  end;

  TStockLargeOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockLargeOrderAttribute,ISCAttribute)
  end;

  //������� "����������" ������
  TStockSmallOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockSmallOrderAttribute,ISCAttribute)
  end;

  TStockNewsHedgeSign = class (TNameValuePersistentObjectRefCounted,IStockNewsHedgeSign,ISCAttribute)
  public
    function GetObject:TStockNewsHedgeSign;
  end;

  TStockNewsHedgingOrderLink = class (TNameValuePersistentObjectRefCounted,IStockNewsHedgingOrderLink,ISCAttribute)
  private
    FOrder: IStockOrder;
  public
    function GetObject:TStockNewsHedgingOrderLink;
    constructor Create(aOrder: IStockOrder);
  end;

  TStockTraderTrendFollower = class (TStockTraderBase,IStockTraderTrendFollower)
  private
    //��� ��������� ����������, ������������ ��� ���������
    FPbSARs: array [TStockTimeInterval] of ISCIndicatorParabolicSAR;
    FPbSAR15mFast: ISCIndicatorParabolicSAR;
    FMA60  : ISCIndicatorMA;
    FMA240 : ISCIndicatorMA;
    FCalendar : ISCIndicatorCalendar;

    FIndexes: array [TStockTimeInterval] of integer;
    FPassedTimes:TMap<TDateTime,Boolean>;

    function GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;
    procedure SetDefaultSL_TP(const aOrder: IStockOrder);
  protected
    procedure GetProperties(aList: TPropertyList); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;

    //�������� ����������� ��������� ��������� ST TP
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;
    procedure SetStopLossAccordingProperty(aOrder: IStockOrder); override;

    function  CreateIndicatorPbSAR(const aChart: IStockChart): ISCIndicatorParabolicSAR;
    procedure InitPbSAR(const aPbSAR:ISCIndicatorParabolicSAR; it:TStockTimeInterval);
    function  CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;

    //����� ��� ������������
    function TestBenchDialogClass: TClass; override;

    function IsNews(const aTime: TDateTime; out aHighPriorityCount,aLowPriorityCount: integer): boolean; overload;

    procedure TryOpenOrder(const aOrder: IStockOrder; const aTime: TDateTime);

    procedure HedgeOrderWithOther(const aOrder,aHedge: IStockOrder; const aTime: TDateTime);
    function  FindHedgeOrder(const aOrder: IStockOrder): IStockOrder;
    function  IsHedge(const aOrder:IStockOrder): boolean;

    //������� � "�������" ��������
    procedure AnalyzeOpenedOrderLarge(const aOrder: IStockOrder; const aTime: TDateTime);
    //������� � "���������" ��������
    procedure AnalyzeOpenedOrderSmall(const aOrder: IStockOrder; const aTime: TDateTime);


    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
  public
    function  GetIndicatorPbSAR(it:TStockTimeInterval):ISCIndicatorParabolicSAR;

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
  FC.DataUtils,FC.Trade.Trader.TrendFollower.TestBenchDialog;

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);


{ TStockTraderTrendFollower }

constructor TStockTraderTrendFollower.Create;
begin
  inherited Create;
  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
end;

function TStockTraderTrendFollower.CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorCalendar,'IndicatorCalendar-'+aChart.StockSymbol.GetTimeIntervalName,true, aCreated) as ISCIndicatorCalendar;
  if aCreated  then
    result.SetCountryFilter('���;��������;��������');
end;

destructor TStockTraderTrendFollower.Destroy;
begin
  inherited;
  FreeAndNil(FPassedTimes);
end;

procedure TStockTraderTrendFollower.Dispose;
begin
  inherited;
end;

function TStockTraderTrendFollower.FindHedgeOrder(const aOrder: IStockOrder): IStockOrder;
var
  i: integer;
begin
  result:=nil;
  i:=aOrder.GetAttributes.IndexOf(IStockNewsHedgingOrderLink);
  if i<>-1 then
    result:=(aOrder.GetAttributes[i] as IStockNewsHedgingOrderLink).GetObject.FOrder;
end;

function TStockTraderTrendFollower.CreateIndicatorPbSAR(const aChart: IStockChart): ISCIndicatorParabolicSAR;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorParabolicSAR,'ParabolicSAR-'+aChart.StockSymbol.GetTimeIntervalName,true,aCreated) as ISCIndicatorParabolicSAR;

  if aCreated then
  begin
    InitPbSAR(result,aChart.StockSymbol.TimeInterval);
  end;
end;

procedure TStockTraderTrendFollower.GetProperties(aList: TPropertyList);
begin
  inherited;
end;

procedure TStockTraderTrendFollower.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
end;

procedure TStockTraderTrendFollower.OnPropertyChanged(aNotifier: TProperty);
begin
  inherited;
end;

procedure TStockTraderTrendFollower.SetDefaultSL_TP(const aOrder: IStockOrder);
var
  aValue: TStockRealNumber;
begin
  if aOrder.GetState=osPending then
    aValue:=aOrder.GetPendingOpenPrice
  else
    aValue:=aOrder.GetOpenPrice;

  //���������� TakeProfit � StopLoss
  case aOrder.GetKind of
    okBuy: begin
      aOrder.SetStopLoss(aValue-aOrder.GetBroker.PointToPrice(aOrder.GetSymbol,PropMaxSubsidence.Value));
      aOrder.SetTakeProfit(aValue+aOrder.GetBroker.PointToPrice(aOrder.GetSymbol,PropEnoughProfit.Value));
    end;
    okSell: begin
       aOrder.SetStopLoss(aValue+aOrder.GetBroker.PointToPrice(aOrder.GetSymbol,PropMaxSubsidence.Value));
       aOrder.SetTakeProfit(aValue-aOrder.GetBroker.PointToPrice(aOrder.GetSymbol,PropEnoughProfit.Value));
    end;
    else
      raise EAlgoError.Create;
  end;
end;

procedure TStockTraderTrendFollower.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderTrendFollower.SetProject(const aValue: IStockProject);
var
  it: TStockTimeInterval;
  aCreated: boolean;
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
    while ExpertCount>0 do
      DeleteExpert(0);

  if aValue <> nil then
  begin
    for it:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      FPbSARs[it]:=CreateIndicatorPbSAR(aValue.GetStockChart(it));

    FMA60:=CreateOrFindIndicator(aValue.GetStockChart(sti60),
                                 ISCIndicatorMA,'MA-60',true,aCreated) as ISCIndicatorMA;
    if aCreated then
    begin
      FMA60.SetMAMethod(mamSimple);
      FMA60.SetPeriod(55);
      FMA60.SetColor(clRed);
      FMA60.SetWidth(2);
    end;

    FMA240:=CreateOrFindIndicator(aValue.GetStockChart(sti60),
                                 ISCIndicatorMA,'MA-240',true,aCreated) as ISCIndicatorMA;
    if aCreated then
    begin
      FMA60.SetMAMethod(mamSimple);
      FMA60.SetPeriod(240);
      FMA60.SetColor(clBlue);
      FMA60.SetWidth(2);
    end;


    FPbSAR15mFast:=CreateOrFindIndicator(
                      aValue.GetStockChart(sti15),
                      ISCIndicatorParabolicSAR,
                      'ParabolicSAR(Fast)-'+aValue.GetStockChart(sti15).StockSymbol.GetTimeIntervalName,
                      true,aCreated) as ISCIndicatorParabolicSAR;
    if aCreated then
      FPbSAR15mFast.SetStep(0.04);

    FCalendar:=CreateCalendarIndicator(aValue.GetStockChart(sti60));
  end;
end;

procedure TStockTraderTrendFollower.SetStopLossAccordingProperty(aOrder: IStockOrder);
begin
  //������!
end;

procedure TStockTraderTrendFollower.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
begin
  //������!
end;

function TStockTraderTrendFollower.TestBenchDialogClass: TClass;
begin
  result:=TfmTrendFollowerTestBenchDialog;
end;

procedure TStockTraderTrendFollower.HedgeOrderWithOther(const aOrder,aHedge: IStockOrder; const aTime: TDateTime);
var
  aOpenPrice: TStockRealNumber;
  aHedgePrice: TStockRealNumber;
  aSL,aTP: TStockRealNumber;
begin
  if aOrder.GetState=osPending then
    aOpenPrice:=aOrder.GetPendingOpenPrice
  else if aOrder.GetState=osOpened then
    aOpenPrice:=aOrder.GetOpenPrice
  else
    raise EAlgoError.Create;

  //�������� ����� � suspend
  if (aOpenPrice<0.0002) or ((aOrder.GetState=osPending) and (aOrder.IsPendingSuspended)) then
  begin
    GetBroker.AddMessage(aHedge,'�������� ����� �� ����������� � ���������. ����...');
    if aHedge.GetState=osPending then
      aHedge.SuspendPending;
    exit;
  end;

  aHedgePrice:=GetBroker.RoundPrice(aOrder.GetSymbol,
   (GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkBid)+
     GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkBid))/2);
  aHedgePrice:=aHedgePrice-OrderKindSign[aOrder.GetKind]*0.0030;

  //��� ��� � ��� ����������
  if (aHedge.GetState=osPending) and (Abs(aHedge.GetPendingOpenPrice-aHedgePrice)<=0.0003) then
    exit;

  aSL:=aHedgePrice+OrderKindSign[aOrder.GetKind]*GetBroker.PointToPrice(aOrder.GetSymbol,PropMaxSubsidence.Value);
  aTP:=aHedgePrice-OrderKindSign[aOrder.GetKind]*GetBroker.RoundPrice(aOrder.GetSymbol,GetBroker.PointToPrice(aOrder.GetSymbol,PropMaxSubsidence.Value)/2);

  aHedge.OpenAt(GetSymbol,OrderKindOpposite[aOrder.GetKind],aHedgePrice,aOrder.GetRate*2,aSL,aTP,0,'��������� news-����');
end;

procedure TStockTraderTrendFollower.TryOpenOrder(const aOrder: IStockOrder; const aTime: TDateTime);

function GetPriceToOpenStopOrder(const aPbSAR: ISCIndicatorParabolicSAR; aIndex: integer; aKind: TSCOrderKind): TStockRealNumber;
var
  aMarketPrice: TStockRealNumber;
  aStopDelta: TStockRealNumber;
begin
  result:=GetBroker.RoundPrice(GetSymbol,
                               (aPbSAR.GetValue(aIndex)+
                                aPbSAR.GetInputData.DirectGetItem_DataClose(aIndex))/2);

  //����� +1, ����� ���������
  aStopDelta:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).StopLevel+1);
  aStopDelta:=GetBroker.RoundPrice(GetSymbol,aStopDelta);

  if aKind= okBuy then
  begin
    aMarketPrice:=GetBroker.GetCurrentPrice(GetSymbol,bpkAsk);
    result:=max(aMarketPrice+aStopDelta,Result);
  end
  else begin
    aMarketPrice:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid);
    result:=min(aMarketPrice-aStopDelta,Result);
  end;

end;

type
  TOperationType = (otNothing, otSetStopOrder,otSuspendOrder,otOpenImmediate);
var
  aOpenDirs: array[TStockTimeInterval] of TStockOrderKind;
  aFast15mDir:TStockOrderKind;
  aMainDir : TStockOrderKind;
  aOperationType: TOperationType;
  a15mClosePriceAt10_00: TSCRealNumber;
  i: integer;

procedure SuspendOrder;
begin
  //��������� � ���������� ���� ��������
  if aOrder.GetState=osNothing then
    aOrder.OpenAt(GetSymbol,aMainDir,0.0001,GetRecommendedRate,'');
  aOrder.SuspendPending;
  aOperationType:=otSuspendOrder;
end;

procedure OpenOrderAt(aPrice: TSCRealNumber);
begin
  aOrder.OpenAt(GetSymbol,aMainDir,aPrice,GetRecommendedRate,'');
  aOperationType:=otSetStopOrder;
end;

begin
  if (FIndexes[sti5]=-1) or  (FIndexes[sti15]=-1) or (FIndexes[sti60]=-1) or (FIndexes[sti240]=-1) then
    exit;

  aOperationType:=otNothing;

  //���� �� ������ �������� ������. ����� ������ 15-������� ����� 10:00
  i:=FPbSARs[sti15].GetInputData.FindExactMatched(Trunc(aTime)+EncodeTime(10,0,0,0));
  if (i=-1) then
  begin
    Pause('Could not find bar at 10:00 for 15 min');
    exit;
  end;
  a15mClosePriceAt10_00:=FPbSARs[sti15].GetInputData.DirectGetItem_DataClose(i);

  //OpenDirs of PbSAR
  aOpenDirs[sti5]:=TrendToOrderKind[FPbSARs[sti5].GetTrendDirection(FIndexes[sti5])];
  aOpenDirs[sti15]:=TrendToOrderKind[FPbSARs[sti15].GetTrendDirection(FIndexes[sti15])];
  aOpenDirs[sti60]:=TrendToOrderKind[FPbSARs[sti60].GetTrendDirection(FIndexes[sti60])];
  aOpenDirs[sti240]:=TrendToOrderKind[FPbSARs[sti240].GetTrendDirection(FIndexes[sti240])];
  aOpenDirs[sti720]:=TrendToOrderKind[FPbSARs[sti720].GetTrendDirection(FIndexes[sti720])];
  aFast15mDir:=TrendToOrderKind[FPbSAR15mFast.GetTrendDirection(FIndexes[sti15])];

  aMainDir:=aOpenDirs[sti240];
  //���� ����� ��� ��������� � ����� �������, � 4H ����������, �� ��� ���� ���������
  if aOrder.GetState = osPending then
    if aOrder.GetKind<>aMainDir then
    begin
      GetBroker.AddMessage(aOrder,'Trend 4h changed. Revoking...');
      aOrder.RevokePending;
      exit;
    end;

  //���� ��� 16:30, �� ���� �����������, ��� ������ ������� �� �����
  if aOrder.GetState = osPending then
    if Frac(aTime)>EncodeTime(16,30,0,0) then
    begin
      GetBroker.AddMessage(aOrder,'Time out. Revoking...');
      aOrder.RevokePending;
      exit;
    end;


  //���� ������� ��������� ���� ������ ���������, �� ����������� ����� ��������
  if aMainDir<>aOpenDirs[sti60] then
  begin
    //���� � 10 ���� h1-����� �� ��������, �� ����� � �� ��������
    if FPbSARs[sti60].GetTrendLength(FIndexes[sti60])>=5 then
    begin
      GetBroker.AddMessage(aOrder,
        Format('Different dirs: 4h (%s) and 1h (%s), and 1h too long. Suspending order...',
               [OrderKindNames[aMainDir],OrderKindNames[aOpenDirs[sti60]]]));


      //!!! �������� �����
      SuspendOrder;
    end

    //h1-����� ������ ����-����, ����� ��������, ����������� ��� ���
    else begin
      i:=0;
      //���� �� ������ �������� ������
      if aMainDir=okBuy then
      begin
        //���� ���� �������� ������ ���� ������ �������, ��� � ������� ��������� ������, ��
        //����������� �������
        if a15mClosePriceAt10_00>FPbSARs[sti15].GetInputData.DirectGetItem_DataClose(FIndexes[sti15]) then
          i:=-2;
      end
      else begin
        //���� ���� �������� ������ ���� ������ �������, ��� � ������� ��������� ������, ��
        //����������� �������
        if a15mClosePriceAt10_00<FPbSARs[sti15].GetInputData.DirectGetItem_DataClose(FIndexes[sti15]) then
          i:=-2;
      end;

      if i=-2 then
      begin
        GetBroker.AddMessage(aOrder,
          Format('Different dirs: 4h (%s) and 1h (%s). Suspending order...',
                 [OrderKindNames[aMainDir],OrderKindNames[aOpenDirs[sti60]]]));


        //!!! �������� �����
        SuspendOrder;
      end;
    end;
  end;

  //15-�������� ����� �� ��������� � �������� �������
  if (aOperationType<>otSuspendOrder) and (aMainDir<>aOpenDirs[sti15]) then
  begin
    //���� ���� �� fast parabolic �����, ����� ����� ����� ��������� �������� �����
    if aFast15mDir=aMainDir then
    begin
      GetBroker.AddMessage(aOrder,
                          Format('Different dirs: 4h (%s) and 15min (%s), but fast 15m is correct. Set %s stop order and waiting...',
                            [OrderKindNames[aMainDir],
                             OrderKindNames[aOpenDirs[sti15]],
                             OrderKindNames[aMainDir]]));

      //!!!������ �������� �����
      OpenOrderAt(GetPriceToOpenStopOrder(FPbSARs[sti15],FIndexes[sti15],aMainDir));
    end
    else begin
      GetBroker.AddMessage(aOrder,
                          Format('Different dirs: 4h (%s) and 15min slow and fast (%s). Suspending order...',
                            [OrderKindNames[aMainDir],
                             OrderKindNames[aOpenDirs[sti15]]]));


      //!!! �������� �����
      SuspendOrder;
    end;
  end;

  //15-�������� ����� ���������, �� �� ������ ��� �������, � �
  //����� ����, ��� ���� ����� �������� �����-�����, �� ���� �������������
  if (aOperationType<>otSuspendOrder) and (FPbSAR15mFast.GetTrendLength(FIndexes[sti15])<=1) then
  begin
    i:=0;
    if aMainDir=okBuy then
    begin
      //���� ���� �������� ������ ���� ������ �������, �� ����������� �������
      if a15mClosePriceAt10_00>FPbSARs[sti15].GetInputData.DirectGetItem_DataClose(FIndexes[sti15]) then
        i:=-2;
    end
    else begin
      //���� ���� �������� ������ ���� ������ �������, �� ����������� �������
      if a15mClosePriceAt10_00<FPbSARs[sti15].GetInputData.DirectGetItem_DataClose(FIndexes[sti15]) then
        i:=-2;
    end;

    if i=-2 then
    begin
      GetBroker.AddMessage(aOrder,
        Format('The 15m trend is too young. Suspending order...',[]));
      //SetMark(aOrder,mkStop,'The 15m trend is too young. Suspending order...');

      //!!! �������� �����
      SuspendOrder;
    end;
  end;


  //5-�������� ����� �� ��������� � 15-��������
  if not (aOperationType in [otSuspendOrder,otSetStopOrder]) and (aOpenDirs[sti15]<>aOpenDirs[sti5]) then
  begin
    GetBroker.AddMessage(aOrder,
                        Format('Different dirs: 15min (%s) and 5min (%s). Set %s stop order and waiting...',
                          [OrderKindNames[aOpenDirs[sti15]],
                           OrderKindNames[aOpenDirs[sti5]],
                           OrderKindNames[aMainDir]]));

    //!!! ������ �������� �����
    OpenOrderAt(GetPriceToOpenStopOrder(FPbSARs[sti5],FIndexes[sti5],aMainDir));
  end;

  if (aOperationType=otNothing) and
     (aOpenDirs[sti15]=aMainDir) and
     (aOpenDirs[sti5]=aMainDir) then
  begin
    //����������� ����������
    aOrder.Open(GetSymbol,aMainDir,GetRecommendedRate,'');
    GetBroker.AddMessage(aOrder,Format('All directions are equal (%s). Open right now...',[OrderKindNames[aMainDir]]));
    aOperationType:=otOpenImmediate;
  end;

  if aOperationType=otNothing then
  begin
    //���-�� �� ��������
    raise EAlgoError.Create;
  end;

  //�������! ��� ������ ����� 12h �� ����������
  if aOperationType in [otSetStopOrder,otOpenImmediate] then
  begin
    if aMainDir<>aOpenDirs[sti720] then
      SetMark(aOrder,mkStop,'Different dirs: 4h and 12h');
  end;

  //������������� StopLoss � TakeProfit
  if (aOrder.GetState in [osOpened,osPending]) and
     ((aOrder.GetState=osOpened) or  (aOrder.GetPendingOpenPrice>0.0002))  then
  begin
    SetDefaultSL_TP(aOrder);
  end;
  //SetStopLossAccordingProperty(aOrder);
  //SetTrailingStopAccordingProperty(aOrder);
end;

procedure TStockTraderTrendFollower.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderTrendFollower.AnalyzeOpenedOrderLarge(const aOrder: IStockOrder; const aTime: TDateTime);
var
  a1hTrend: TSCTrendType;
  a15mTrend : TSCTrendType;
  aProps: TStockOrderProperties;
  aValue: TStockRealNumber;
  j: integer;
  aCurPriceForOrder: TStockRealNumber;
const
  OrderKindToTrendMap: array [TStockOrderKind] of TSCTrendType = (ttBullish,ttBearish);
  OrderKindToSignMap: array [TStockOrderKind] of integer = (1,-1);
  TrendTypeToMark: array [TSCTrendType] of TSCChartMarkKind = (mkUp,mkDown,mkQuestion);


begin
  aProps:=GetOrderProperties(aOrder);

  //���� StopLoss �� ���������� (� ����� ����� ����, �� ��� �������� Pending �������
  //����� StopLoss �� �������������
  if aOrder.GetStopLoss=0 then
  begin
    SetDefaultSL_TP(aOrder);
  end;

  //������� ���� ��� ������
  if aOrder.GetKind=okBuy then
    aCurPriceForOrder:=GetBroker.GetCurrentPrice(GetSymbol,bpkBid)
  else
    aCurPriceForOrder:=GetBroker.GetCurrentPrice(GetSymbol,bpkAsk);

  //���� ����� �����������
  a1hTrend:=FPbSARs[sti60].GetTrendDirection(FIndexes[sti60]);
  a15mTrend:=FPbSARs[sti15].GetTrendDirection(FIndexes[sti15]);

  if ((aOrder.GetKind=okBuy) and (a1hTrend=ttBearish)) or
     ((aOrder.GetKind=okSell) and (a1hTrend=ttBullish))  then
  begin
    //���� ��� ������ ������������, �� �� ������� �������� � �������������
    if aProps.H1TrendTurnTime=0 then
    begin
      aProps.H1TrendTurnTime:=aTime;
      AddMessageAndSetMark(aOrder, TrendTypeToMark[a1hTrend],
        Format('1h trend changed to opposite (%s)',[TrendTypeNames[a1hTrend]]));
    end;
  end
  //�������� �����? �� ����, ����� �� ��������� ����� ����
  else if a1hTrend = ttFlat then
  begin
    AddMessageAndSetMark(aOrder,TrendTypeToMark[a1hTrend],'1h trend is flat');
  end
  else begin
    //���� ����� �������������, �� �� ��������� �������
    if aProps.H1TrendTurnTime<>0 then
    begin
      aProps.H1TrendTurnTime:=0;
      AddMessageAndSetMark(aOrder,TrendTypeToMark[a1hTrend],
        Format('1h trend restored to normal (%s)',[TrendTypeNames[a1hTrend]]));
    end;
  end;


  //�������� ����� �� 15 �, ���� ��������� �������������� ����
  if (OrderKindToTrendMap[aOrder.GetKind]<>a15mTrend) and
     (aOrder.GetCurrentProfit>0.0005)  //������� ������ > 5 �������
     //(GetExpectedLoss(aOrder)>-0.0005)
  then begin
     if aOrder.GetKind=okBuy then
     begin
       aValue:=min(aOrder.GetOpenPrice+0.0002,GetNearestStopLossToMarketPrice(aOrder));
       if aOrder.GetStopLoss<>0 then
         aValue:=max(aValue,aOrder.GetStopLoss); //������� ���� ����, ���� �� ������, �� �������
     end
     else begin
       aValue:=max(aOrder.GetOpenPrice-0.0002,GetNearestStopLossToMarketPrice(aOrder));
       if aOrder.GetStopLoss<>0 then
         aValue:=min(aValue,aOrder.GetStopLoss); //������� ���� ����, ���� �� ������, �� �������
     end;

     if aValue<>aOrder.GetStopLoss then
     begin
       if (TStockDataUtils.IsTimeBetweenHHMM(aTime,14,30,16,10)) then //����� ������ �������� ���������
       begin
           AddMessageAndSetMark(aOrder,mkAttention,
              Format('15m trend changed to oposite (%s). Maybe set stoploss?',[TrendTypeNames[a15mTrend]]));
       end
       else begin
           aOrder.SetStopLoss(aValue);
           AddMessageAndSetMark(aOrder,mkCheck,
              Format('15m trend changed to oposite (%s). Set profitable stoploss',[TrendTypeNames[a15mTrend]]));
       end;
     end;
  end;

  //��� ��������� ����������� ������� ������� ���� �����������

  j:=FPbSARs[sti1].GetInputData.FindBestMatched(max(aTime-15*MinuteAsDateTime,aOrder.GetOpenTime));
  if (FIndexes[sti1]<>-1) then
      j:=OrderKindToSignMap[aOrder.GetKind]*GetBroker.PriceToPoint(GetSymbol,aCurPriceForOrder-FPbSARs[sti1].GetInputData.DirectGetItem_DataClose(j));

  //�� ��������� 5 ����� ��� ������ �� 50 �������, ���� ����� ��������� ����, ����� �� �������� ������ ��������
  if (j>=50) and
     //���� ����� �������� �� �������, ����� ������ �����������, ����� ����� ������ ���������
     (OrderKindToSignMap[aOrder.GetKind]*FPbSARs[sti240].GetTrendVelocity(FIndexes[sti240])<10) then
  begin
    if aOrder.GetKind=okBuy then
      aValue:=min(GetBroker.GetCurrentPrice(GetSymbol,bpkBid)-0.0010,GetNearestStopLossToMarketPrice(aOrder))
    else
      aValue:=max(GetBroker.GetCurrentPrice(GetSymbol,bpkAsk)+0.0010,GetNearestStopLossToMarketPrice(aOrder));

    if (aOrder.GetStopLoss=0) or //�� ����������
       (aOrder.GetKind=okBuy) and (aValue>aOrder.GetStopLoss) or
       (aOrder.GetKind=okSell) and (aValue<aOrder.GetStopLoss) then
    begin
       //���-�� �� ��������� ����. �������� ������ �������� ����� � ����������� �������� ������
       //������� ��������� ������, �� �����������, ��������� ����� �������.
       //aOrder.SetStopLoss(aValue);
       //aOrder.SetTrailingStop(aValue);
      AddMessageAndSetMark(aOrder,mkCheck,
        Format('Large jerk (%d pts) detected, while trend velocity is weak (%d). Maybe set profitable stoploss?',
               [j,Round(FPbSARs[sti240].GetTrendVelocity(FIndexes[sti240]))]));
    end;
  end;


  //������ ���������� �������, ���� ����������� StopLoss
  if aOrder.GetCurrentProfit>0.0050 then
  begin
     //��� ������ ���������� �������� �������
     if aOrder.GetKind=okBuy then
       aValue:=min(aOrder.GetOpenPrice+0.0005,GetNearestStopLossToMarketPrice(aOrder))
     else
       aValue:=max(aOrder.GetOpenPrice-0.0005,GetNearestStopLossToMarketPrice(aOrder));

     if (aOrder.GetStopLoss=0) or //�� ����������
        (aOrder.GetKind=okBuy) and (aValue>aOrder.GetStopLoss) or
        (aOrder.GetKind=okSell) and (aValue<aOrder.GetStopLoss) then
     begin
       aOrder.SetStopLoss(aValue);
       aOrder.SetTrailingStop(0.0050);
       AddMessageAndSetMark(aOrder,mkCheck,
         'Profit > 50 pt. Set profitable stoploss and trailing stop');
     end;
  end;

  //���� ����� 1h �������� � ����, � ������ �� 7 �� 9 ����� ����, ����� ���� ��������� ������������
  //����� �� �������� ������
  if (aOrder.GetCurrentProfit>GetBroker.PointToPrice(GetSymbol,5)) and
     (aProps.H1TrendTurnTime<>0) and
     (HourOfTheDay(aTime) in [7..9]) then
  begin
    if OrderKindToTrendMap[aOrder.GetKind]<>a15mTrend then
    begin
      aOrder.Close('Close because trend turned and time over');
      AddMessageAndSetMark(aOrder,mkSnowFlake,
        'Close because trend turned and time over');
    end
    else begin
      AddMessageAndSetMark(aOrder,mkAttention,
        Format('Time and trend says "close". Waiting 15m proof, it is still %s',[TrendTypeNames[a15mTrend]]));
    end;
  end;
end;

procedure TStockTraderTrendFollower.AnalyzeOpenedOrderSmall(const aOrder: IStockOrder;
  const aTime: TDateTime);
var
  aOrderDir: TStockOrderKind;
//  a1hTrend: TSCTrendType;
  a5mDir : TStockOrderKind;
  a15mFastDir: TStockOrderKind;
begin

  //���� ����� �����������
  aOrderDir:=aOrder.GetKind;

//  a15mTrend:=FPbSARs[sti15].GetTrendDirection(FIndexes[sti15]);
  a15mFastDir:=TrendToOrderKind[FPbSAR15mFast.GetTrendDirection(FIndexes[sti15])];
  a5mDir:=TrendToOrderKind[FPbSARs[sti5].GetTrendDirection(FIndexes[sti5])];

  //���� ����������� 5 ���, � ��� ���� � ��� ���� ��� ���-����� ������, �� ���� �����������
  if (aOrder.GetCurrentProfit>0.0025) and (a5mDir<>aOrderDir) then
  begin
    aOrder.Close('Close because 5m trend turned and profit >25 pts');
  end

  //���� ����������� ������� 15 ���, �� ���� �����������
  else if (aOrderDir<>a15mFastDir) and (aOrder.GetCurrentProfit>0) then
  begin
    aOrder.Close('Close because fast 15m trend turned and profit >0 pts');
  end;
end;

procedure TStockTraderTrendFollower.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  aOrder : IStockOrder;
  aHedge : IStockOrder;
  aNews  : boolean;
  aIsHedge : boolean;
  aNewsHighPriorityCount,aNewsLowPriorityCount: integer;
begin
  RemoveClosedOrders;

  FIndexes[sti1]:=FPbSARs[sti1].GetInputData.FindExactMatched(aTime);
  FIndexes[sti5]:=FPbSARs[sti5].GetInputData.FindExactMatched(aTime);
  FIndexes[sti15]:=FPbSARs[sti15].GetInputData.FindExactMatched(aTime);
  FIndexes[sti60]:=FPbSARs[sti60].GetInputData.FindExactMatched(aTime);
  FIndexes[sti240]:=FPbSARs[sti240].GetInputData.FindExactMatched(aTime);
  FIndexes[sti720]:=FPbSARs[sti720].GetInputData.FindExactMatched(aTime);

  for i := 0 to GetOrders.Count-1 do
  begin
    aOrder:=GetOrders[i];
    aIsHedge :=IsHedge(aOrder);

    if aOrder.GetState =osPending then
    begin
      if not aIsHedge  then //����� �� �����������
      begin
        TryOpenOrder(aOrder,aTime);
      end
      //����������� ����� �� ��������  �� ����� ������ 1 ����
      else if (aTime-aOrder.GetPendingOpenTime)>EncodeTime(0,30,0,0) then
      begin
        GetBroker.AddMessage(aOrder,'������� ��� ������, ��������� news-����');
        GetOrderProperties(aOrder).Revoked:=true; //��������, ��� ���� �������, ���� ������ ��� �� �������
        aOrder.RevokePending;
      end;
    end
    else if aOrder.GetState = osOpened then
    begin
      if not aIsHedge  then //����� �� �����������
      begin
        AnalyzeOpenedOrderLarge(aOrder,aTime);

        //���� ��� ��������� �����, �� ���������, ����� ���� ���������
        if (aOrder.GetState = osOpened) and (aOrder.GetAttributes.IndexOf(IStockSmallOrderAttribute)<>-1) then
        begin
          AnalyzeOpenedOrderSmall(aOrder,aTime);
        end;
      end
      //��� ����� ������ ��������
      else begin
        if aOrder.GetCurrentProfit>0.0025 then  //���� ����� � ���������
        begin
          MoveStopLossToOpenPrice(aOrder);
          aOrder.SetTrailingStop(0.0020);
        end;
      end;
    end;

    aHedge:=FindHedgeOrder(aOrder);

    //���� �� ��� �� ������ ��������������, ��������� Pending Hedge
    if (aHedge<>nil ) and (aOrder.GetState=osOpened) and (GetExpectedLoss(aOrder)<0) then
    begin
      if aHedge.GetState=osPending then
      begin
        GetOrderProperties(aOrder).Revoked:=true; //��������, ��� ���� �������, ���� ������ ��� �� �������
        aHedge.RevokePending;
      end
      else if aHedge.GetState=osOpened then
        //Pause('����������� ����� � ������. ��� ������?');
    end;

    //����� ��������, ���� ��������� ����, ���� ��� �� ������
    if (aHedge<>nil ) and (aOrder.GetState=osNothing) and (aHedge.GetState=osPending) then
    begin
      GetOrderProperties(aOrder).Revoked:=true; //��������, ��� ���� �������, ���� ������ ��� �� �������
      aHedge.RevokePending;
    end;

   //����� ��� ��� �����, � ���� �����, �������������� ����
    if (aHedge<>nil) and (aOrder.GetState=osPending) and (not GetOrderProperties(aHedge).Revoked) then
      HedgeOrderWithOther(aOrder,aHedge,aTime);

    //������ ��� �������� �����
    if (aHedge<>nil) and (aOrder.GetState=osOpened) and (aOrder.GetOpenTime=aTime)  then
      if (aHedge.GetState=osNothing) and (not GetOrderProperties(aHedge).Revoked) then
        HedgeOrderWithOther(aOrder,aHedge,aTime);
  end;


  if (HourOfTheDay(aTime)=14) and (MinuteOfTheHour(aTime) in [29]) and
     (not FPassedTimes.Lookup(TStockDataUtils.AlignTimeToLeft(aTime,sti1)))
  then
  begin
    //"�������" �����
    aOrder:=CreateEmptyOrder;
    aOrder.GetAttributes.Add(TStockOrderProperties.Create);
    aOrder.GetAttributes.Add(TStockLargeOrderAttribute.Create);
    aOrder.SetNotes('Large');
    TryOpenOrder(aOrder,aTime);

    {$IFDEF SMALL_ORDER}
    //"������" �����
    aOrder:=CreateEmptyOrder;
    aOrder.GetAttributes.Add(TStockOrderProperties.Create);
    aOrder.GetAttributes.Add(TStockSmallOrderAttribute.Create);
    aOrder.SetNotes('Small');
    aOrder.SetColor($EFEFEF);
    TryOpenOrder(aOrder,aTime);
    aOrder.SetTrailingStop(0.0025); //25 ������� - �� ��� ����
    {$ENDIF}

    //��������� � ������� ������ � ���, ��� ��� ������ �� ��� ����������, �����
    //��������� ��� �� ����� �� ���������
    FPassedTimes.Add(TStockDataUtils.AlignTimeToLeft(aTime,sti1),true);
  end;

  begin
    //������� ����������� �����
    aNews:=IsNews(aTime+OneMinute,aNewsHighPriorityCount,aNewsLowPriorityCount);

    if aNews (*and
       ((aNewsHighPriorityCount>0) or
        (SameTime(aTime,EncodeTime(14,29,0,0))) or
        (SameTime(aTime,EncodeTime(15,59,0,0))) or
        (SameTime(aTime,EncodeTime(19,59,0,0)))) *) then
    begin
      for i:=GetOrders.Count-1 downto 0 do
      begin
        aOrder:=GetOrders[i];

        if not (aOrder.GetState in [osOpened,osPending]) or IsHedge(aOrder) then
          continue;

        //� ��� ��� ��� ������
        if (GetExpectedLoss(aOrder)<0) then
          continue;

        if (aOrder.GetCurrentProfit>0.0020) then
        begin
          GetBroker.AddMessage(aOrder,'����� ��������� ����������� � ��������� ');
          MoveStopLossToOpenPrice(aOrder);
          continue;
        end;

        //���� ������� ����, ��� �� ���������������
        aHedge:=FindHedgeOrder(aOrder);
        if (aHedge<>nil) and (aHedge.GetState=osClosed) then
        begin
          aOrder.GetAttributes.Remove(IStockNewsHedgingOrderLink);
          aHedge:=nil;
        end;

        //���� � ���� ������??
        if (aHedge<>nil) and (aHedge.GetState=osOpened) then
        begin
          GetBroker.AddMessage(aOrder,'��� ������ ����, � ����� ������� �������!');
          continue;
        end;

        if aHedge=nil then
        begin
          aHedge:=CreateEmptyOrder;
          aHedge.SetNotes('Hedge');
          aHedge.SetColor(clWebBisque);

          aOrder.GetAttributes.Add(TStockNewsHedgingOrderLink.Create(aHedge));
          aHedge.GetAttributes.Add(TStockNewsHedgeSign.Create);
          aHedge.GetAttributes.Add(TStockOrderProperties.Create);
        end;

        GetBroker.AddMessage(aOrder,'����� ��������� ��������� news-����');
        HedgeOrderWithOther(aOrder,aHedge,aTime);
      end; //for
    end; //if aNews
  end;


end;

function TStockTraderTrendFollower.GetIndicatorPbSAR(it: TStockTimeInterval): ISCIndicatorParabolicSAR;
begin
  result:=FPbSARs[it];
end;

function TStockTraderTrendFollower.GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;
var
  i: integer;
begin
  i:=aOrder.GetAttributes.IndexOf(IStockOrderProperties);
  if i=-1 then
    raise EAlgoError.Create;

  result:=(aOrder.GetAttributes.Items[i] as IStockOrderProperties).GetObject;
end;

procedure TStockTraderTrendFollower.InitPbSAR(const aPbSAR:ISCIndicatorParabolicSAR; it:TStockTimeInterval);
begin
  if it=sti60 then
    aPbSAR.SetStep(0.03)
  else if it = sti240 then
    aPbSAR.SetStep(0.04);
end;

function TStockTraderTrendFollower.IsHedge(const aOrder: IStockOrder): boolean;
begin
  result:=aOrder.GetAttributes.IndexOf(IStockNewsHedgeSign)<>-1;
end;

function TStockTraderTrendFollower.IsNews(const aTime: TDateTime; out aHighPriorityCount,aLowPriorityCount: integer): boolean;
var
  aTime_: TDateTime;
  aItems: TIntegerDynArray;
  i: Integer;
begin
  result:=false;
  aHighPriorityCount:=0;
  aLowPriorityCount:=0;
  aTime_:=TStockDataUtils.AlignTimeToLeft(aTime,sti1);

  aItems:=FCalendar.GetItems(aTime_);
  if Length(aItems)>0 then
  begin
    for i := 0 to High(aItems) do
      if AnsiSameText(FCalendar.GetData.GetItem(i).GetPriority,'High') then
        inc(aHighPriorityCount)
      else
        inc(aLowPriorityCount);

    GetBroker.AddMessage(Format('%s ��������� %d �������, �� ��� %d ������',
     [DateTimeToStr(aTime_),Length(aItems),aHighPriorityCount]));

    result:=true;
  end;
end;

{ TStockOrderProperties }

function TStockOrderProperties.GetObject: TStockOrderProperties;
begin
  result:=self;
end;

{ TStockNewsHedgingOrderLink }

constructor TStockNewsHedgingOrderLink.Create(aOrder: IStockOrder);
begin
  inherited Create;
  FOrder:=aOrder;
end;


function TStockNewsHedgingOrderLink.GetObject: TStockNewsHedgingOrderLink;
begin
  result:=self;
end;

{ TStockNewsHedgeSign }

function TStockNewsHedgeSign.GetObject: TStockNewsHedgeSign;
begin
  result:=self;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Trend Follower',TStockTraderTrendFollower,IStockTraderTrendFollower);
end.




