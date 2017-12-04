{-----------------------------------------------------------------------------
 TrendFollowert Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.ParabolicTrend;
{$I Compiler.inc}
//{$DEFINE SMALL_ORDER}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Alerter.Base,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockAlerterTrendFollower = interface
  ['{15D0F49A-6117-4D9C-8E63-273C53CD786C}']
  end;

  TStockAlerterTrendFollower = class (TStockAlerterBase,IStockAlerterTrendFollower)
  private
    //Ёто временные переменные, испольщуемые при трейдинге
    FPbSARs: array [TStockTimeInterval] of ISCIndicatorParabolicSAR;
    FPbSAR15mFast: ISCIndicatorParabolicSAR;

    FIndexes: array [TStockTimeInterval] of integer;
    FPassedPoints:TMap<string,integer>;

    procedure TryOpenOrder(const aTime: TDateTime);
  protected
    procedure GetProperties(aList: TPropertyList); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;

    function  CreateIndicatorPbSAR(const aChart: IStockChart): ISCIndicatorParabolicSAR;
    procedure InitPbSAR(const aPbSAR:ISCIndicatorParabolicSAR; it:TStockTimeInterval);
    function  CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;
  public
    function  GetIndicatorPbSAR(it:TStockTimeInterval):ISCIndicatorParabolicSAR;

    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;

    //ѕосчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,DateUtils, SystemService, Application.Definitions, FC.Trade.OrderCollection,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Alerter.Factory,
  FC.DataUtils;

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);


{ TStockAlerterTrendFollower }

constructor TStockAlerterTrendFollower.Create;
begin
  inherited Create;
  FPassedPoints:=TMap<string,integer>.Create;
end;

function TStockAlerterTrendFollower.CreateCalendarIndicator(const aChart: IStockChart): ISCIndicatorCalendar;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorCalendar,'IndicatorCalendar-'+aChart.StockSymbol.GetTimeIntervalName,true, aCreated) as ISCIndicatorCalendar;
  if aCreated  then
    result.SetCountryFilter('—Ўј;≈врозона;√ермани€');
end;

destructor TStockAlerterTrendFollower.Destroy;
begin
  inherited;
  FreeAndNil(FPassedPoints);
end;

procedure TStockAlerterTrendFollower.Dispose;
begin
  inherited;
end;

function TStockAlerterTrendFollower.CreateIndicatorPbSAR(const aChart: IStockChart): ISCIndicatorParabolicSAR;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorParabolicSAR,'ParabolicSAR-'+aChart.StockSymbol.GetTimeIntervalName,true,aCreated) as ISCIndicatorParabolicSAR;

  if aCreated then
  begin
    InitPbSAR(result,aChart.StockSymbol.TimeInterval);
  end;
end;

procedure TStockAlerterTrendFollower.GetProperties(aList: TPropertyList);
begin
  inherited;
end;

procedure TStockAlerterTrendFollower.OnBeginWorkSession;
begin
  inherited;
  FPassedPoints.Clear;
end;

procedure TStockAlerterTrendFollower.OnPropertyChanged(aNotifier: TProperty);
begin
  inherited;
end;

procedure TStockAlerterTrendFollower.SetProject(const aValue: IStockProject);
var
  it: TStockTimeInterval;
  aCreated: boolean;
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    for it:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      FPbSARs[it]:=CreateIndicatorPbSAR(aValue.GetStockChart(it));

    FPbSAR15mFast:=CreateOrFindIndicator(
                      aValue.GetStockChart(sti15),
                      ISCIndicatorParabolicSAR,
                      'ParabolicSAR(Fast)-'+aValue.GetStockChart(sti15).StockSymbol.GetTimeIntervalName,
                      true,aCreated) as ISCIndicatorParabolicSAR;
    if aCreated then
      FPbSAR15mFast.SetStep(0.04);
  end;
end;

procedure TStockAlerterTrendFollower.UpdateStep2(const aTime: TDateTime);
begin
  FIndexes[sti1]:=FPbSARs[sti1].GetInputData.FindExactMatched(aTime);
  FIndexes[sti5]:=FPbSARs[sti5].GetInputData.FindExactMatched(aTime);
  FIndexes[sti15]:=FPbSARs[sti15].GetInputData.FindExactMatched(aTime);
  FIndexes[sti60]:=FPbSARs[sti60].GetInputData.FindExactMatched(aTime);
  FIndexes[sti240]:=FPbSARs[sti240].GetInputData.FindExactMatched(aTime);
  FIndexes[sti720]:=FPbSARs[sti720].GetInputData.FindExactMatched(aTime);

  TryOpenOrder(aTime);  
end;

function TStockAlerterTrendFollower.GetIndicatorPbSAR(it: TStockTimeInterval): ISCIndicatorParabolicSAR;
begin
  result:=FPbSARs[it];
end;

procedure TStockAlerterTrendFollower.InitPbSAR(const aPbSAR:ISCIndicatorParabolicSAR; it:TStockTimeInterval);
begin
  if it=sti60 then
    aPbSAR.SetStep(0.03)
  else if it = sti240 then
    aPbSAR.SetStep(0.04);
end;

procedure TStockAlerterTrendFollower.TryOpenOrder(const aTime: TDateTime);
type
  TOperationType = (otNothing, otSetStopOrder,otWait,otOpenImmediate);
var
  aOpenDirs: array[TStockTimeInterval] of TStockOrderKind;
  aFast15mDir:TStockOrderKind;
  aMainDir : TStockOrderKind;
  aOperationType: TOperationType;
  aTrendStart,aTrendStop: integer;
  s: string;
begin
  if (FIndexes[sti5]=-1) or  (FIndexes[sti15]=-1) or (FIndexes[sti60]=-1) or (FIndexes[sti240]=-1) then
    exit;

  aOperationType:=otNothing;

  //OpenDirs of PbSAR
  aOpenDirs[sti5]:=TrendToOrderKind[FPbSARs[sti5].GetTrendDirection(FIndexes[sti5])];
  aOpenDirs[sti15]:=TrendToOrderKind[FPbSARs[sti15].GetTrendDirection(FIndexes[sti15])];
  aOpenDirs[sti60]:=TrendToOrderKind[FPbSARs[sti60].GetTrendDirection(FIndexes[sti60])];
  aOpenDirs[sti240]:=TrendToOrderKind[FPbSARs[sti240].GetTrendDirection(FIndexes[sti240])];
  aOpenDirs[sti720]:=TrendToOrderKind[FPbSARs[sti720].GetTrendDirection(FIndexes[sti720])];
  aFast15mDir:=TrendToOrderKind[FPbSAR15mFast.GetTrendDirection(FIndexes[sti15])];

  aMainDir:=aOpenDirs[sti240];

  //≈сли часовой параболик идет против основного, то анализируем более подробно
  if aMainDir<>aOpenDirs[sti60] then
  begin
    aOperationType:=otWait;
  end;

  //15-минутный тренд не совпадает с основным трендом
  if (aOperationType<>otWait) and (aMainDir<>aOpenDirs[sti15]) then
  begin
    //≈сли хот€ бы fast parabolic похож, тогда имеет смысл поставить стоповый ордер
    if aFast15mDir=aMainDir then
    begin
      aOperationType:=otSetStopOrder;
    end
    else begin
      aOperationType:=otWait;
    end;
  end;

  //15-минутный тренд совпадает, но он только что началс€, и в
  //свете того, что весь тренд выгл€дит шатко-валко, мы ждем подтверждени€
  if (aOperationType<>otWait) and (FPbSAR15mFast.GetTrendLength(FIndexes[sti15])<=1) then
  begin
    aOperationType:=otWait;
  end;

  //5-минутный тренд не совпадает с 15-минутным
  if not (aOperationType in [otWait,otSetStopOrder]) and (aOpenDirs[sti15]<>aOpenDirs[sti5]) then
  begin
    aOperationType:=otSetStopOrder;
  end;

  if (aOperationType=otNothing) and
     (aOpenDirs[sti15]=aMainDir) and
     (aOpenDirs[sti5]=aMainDir) then
  begin
    aOperationType:=otOpenImmediate;
  end;

  if aOperationType=otOpenImmediate then
  begin
    FPbSARs[sti5].GetTrendBounds(FIndexes[sti5],aTrendStart,aTrendStop);
    s:='All timeframes equal: '+OrderKindNames[aMainDir]+'. TrendStart='+IntToStr(aTrendStart);

    //≈сли это все тот-же 5-минутный тренд, то мы ничего не говорим
    if not FPassedPoints.Lookup(s) then
    begin
      AddMessage('All timeframes equal: '+OrderKindNames[aMainDir]);
      FPassedPoints.Add(s,0);
    end;
  end;
end;

initialization
  FC.Trade.Alerter.Factory.AlerterFactory.RegisterAlerter('Basic','Parabolic Trend',TStockAlerterTrendFollower,IStockAlerterTrendFollower);
end.




