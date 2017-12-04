unit FC.StockChart.UnitTask.ValueSupport.StatisticsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls, TeEngine, Series, TeeProcs, Chart, FC.StockChart.CustomDialog_B,
  ImgList, JvCaptionButton, JvComponentBase, JvDockControlForm, Mask;

type
  TfmValueSupportStatisticsDialog = class(TfmStockChartCustomDialog_B)
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    pbProgress: TProgressBar;
    taReportTime: TTimeField;
    taReportValue: TFloatField;
    pcPages: TPageControl;
    tsIntradays: TTabSheet;
    tsMonthVolatility: TTabSheet;
    DataSource2: TDataSource;
    taWeekdayStatistics: TMemoryDataSet;
    taWeekdayStatisticsValue: TFloatField;
    taWeekdayStatisticsWeekday: TStringField;
    chIntraday: TChart;
    chIntradayValue: TBarSeries;
    Label3: TLabel;
    cbValueSupport: TExtendComboBox;
    chMonthVolatility: TChart;
    laHelp: TTipPanel;
    chMonthVolatilityValue: TBarSeries;
    tsAllPeaks: TTabSheet;
    Panel1: TPanel;
    edValueTreshold: TExtendEdit;
    ckValueTreshold: TExtendCheckBox;
    chAllPeaks: TChart;
    chAllPeaksValue: TBarSeries;
    ckAllPeaksShowMarks: TExtendCheckBox;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    grWeekdayStatistics: TEditDBGrid;
    grReport: TEditDBGrid;
    laChartPropsHeader: TLabel;
    mmChartProps: TExtendMemo;
    procedure ckAllPeaksShowMarksClick(Sender: TObject);
    procedure edValueTresholdChange(Sender: TObject);
    procedure ckHLClick(Sender: TObject);
    procedure cbValueSupportChange(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure grReportChangeRecord(Sender: TObject);
  private
    FCalculating : integer;
    FIndicator: ISCIndicatorValueSupport;

    procedure Init(const aIndicator: ISCIndicatorValueSupport);

    procedure CalculateIntradays;
    procedure CaclulateMonthVolatility;
    procedure CalculateWeekDayStatistics(const aFrom,aTo: TTime);
    procedure CalculateAllPeaks;
  public
    class procedure Run(const aIndicator: ISCIndicatorValueSupport; const aStockChart: IStockChart);

    constructor Create(const aStockChart: IStockChart); override;
    destructor Destroy; override;
  end;


implementation
  uses Math,DateUtils,Application.Definitions;

type
  TDirection = (dNone,dUp,dDown);

{$R *.dfm}

{ TfmCalculateWidthDialog }

procedure TfmValueSupportStatisticsDialog.CaclulateMonthVolatility;
var
  aInterval: integer;
  i: integer;
begin
  aInterval:=StockChart.StockSymbol.GetTimeIntervalValue;
  cbValueSupport.Clear;
  for i := 0 to (1440 div aInterval)-1 do
    cbValueSupport.AddItem(TimeToStr(i*aInterval/MinsPerDay),TObject(i));
end;

procedure TfmValueSupportStatisticsDialog.CalculateAllPeaks;
var
  i: integer;
  aValue:TStockRealNumber;
  aHLMax: TStockRealNumber;
  aHLR: boolean;
  aInputData : ISCInputDataCollection;
begin
  TWaitCursor.SetUntilIdle;

  aInputData:=(FIndicator as ISCIndicator).GetInputData;
  chAllPeaks.AutoRepaint:=false;
  try
    chAllPeaksValue.Clear;

    aHLMax:=edValueTreshold.ValueFloat;
    if not ckValueTreshold.Checked then
      aHLMax:=Low(integer);

    for i:=(FIndicator as ISCIndicator).GetFirstValidValueIndex to (FIndicator as ISCIndicator).GetInputData.Count-1 do
    begin
      aValue:=FIndicator.GetValue(i);

      aHLR:=aValue>aHLMax;

      if aHLR then
        chAllPeaksValue.AddXY(aInputData.DirectGetItem_DataDateTime(i),aValue);
    end;
  finally
    chAllPeaks.AutoRepaint:=true;
    chAllPeaks.Repaint;
    ckAllPeaksShowMarksClick(nil);
  end;
end;

procedure TfmValueSupportStatisticsDialog.CalculateIntradays;
var
  i,j: Integer;
  aIntradayStatisticsVal: array of TSCRealNumber;
  aIntradayStatisticsCNT: array of integer;
  aInterval: integer;
  aInputData : ISCInputDataCollection;
  aMax,aMin: TSCRealNumber;
  v,aSum,aTotalSum: TSCRealNumber;
  aTotalCount: integer;
  aNullCount : integer;
  aFVI : integer;
  aNullValueSupport : ISCIndicatorNullValueSupport;
begin
  TWaitCursor.SetUntilIdle;
  aInterval:=StockChart.StockSymbol.GetTimeIntervalValue;
  SetLength(aIntradayStatisticsVal,1440 div aInterval);
  SetLength(aIntradayStatisticsCNT,1440 div aInterval);

  aInputData:=(FIndicator as ISCIndicator).GetInputData;
  aFVI:=(FIndicator as ISCIndicator).GetFirstValidValueIndex;
  aTotalSum:=0;
  aTotalCount:=0;
  aNullCount:=0;

  Supports(FIndicator,ISCIndicatorNullValueSupport,aNullValueSupport);

  FIndicator.GetValue(aInputData.Count-1); //Сразу все посчитаем
  for i:=aFVI to aInputData.Count-1 do
  begin
    if (aNullValueSupport<>nil) and (aNullValueSupport.IsNullValue(i)) then
    begin
      inc(aNullCount);
      continue;
    end;

    j:=MinuteOfTheDay(aInputData.DirectGetItem_DataDateTime(i));
    j:=j div aInterval;
    aIntradayStatisticsVal[j]:=aIntradayStatisticsVal[j]+ FIndicator.GetValue(i);
    aIntradayStatisticsCNT[j]:=aIntradayStatisticsCNT[j]+1;
    inc(aTotalCount);
    aTotalSum:=aTotalSum+FIndicator.GetValue(i);
  end;

  aMin:=high(integer);
  aMax:=low(integer);
  aSum:=0;
  inc(FCalculating);
  try
    taReport.DisableControls;
    taReport.EmptyTable;
    taReport.Open;
    chIntradayValue.Clear;

    for I := 0 to High(aIntradayStatisticsVal) do
    begin
      taReport.Append;
      taReportTime.Value:=i*aInterval/MinsPerDay;
      if aIntradayStatisticsCNT[i]<>0 then
      begin
        v:=aIntradayStatisticsVal[i]/aIntradayStatisticsCNT[i];
        taReportValue.Value:=RoundTo(v,-6);
        chIntradayValue.AddXY(taReportTime.Value,taReportValue.Value);        
        aSum:=aSum+v;
        aMin:=min(aMin,v);
        aMax:=max(aMax,v);
     end
      else begin
        chIntradayValue.AddXY(taReportTime.Value,0);
      end;
      taReport.Post;
    end;

    taReport.First;
    chIntraday.LeftAxis.AdjustMaxMin;
    chIntraday.LeftAxis.AutomaticMinimum:=false;
    if chIntraday.LeftAxis.Maximum>aMin then
      chIntraday.LeftAxis.Minimum:=aMin;
  finally
    dec(FCalculating);
    taReport.EnableControls;
  end;

  inc(FCalculating);
  try
    grReport.RefreshSort;
  finally
    dec(FCalculating);
  end;

  mmChartProps.Clear;
  mmChartProps.Lines.Add(Format('Max=%g',[aMax]));
  mmChartProps.Lines.Add(Format('Min=%g',[aMin]));
  mmChartProps.Lines.Add(Format('Sum=%g',[aSum]));
  mmChartProps.Lines.Add(Format('Avg=%g',[aSum/Length(aIntradayStatisticsCNT)]));
  if aSum<>0 then
  begin
    mmChartProps.Lines.Add(Format('Max Percentage=%g',[aMax/aSum*100]));
    mmChartProps.Lines.Add(Format('Min Percentage=%g',[aMin/aSum*100]));
  end;

  mmChartProps.Lines.Add(Format('Total Sum=%g',[aTotalSum]));
  mmChartProps.Lines.Add(Format('Null Value Count=%d',[aNullCount]));
  mmChartProps.Lines.Add(Format('Total Value Count=%d',[aTotalCount]));
  if aTotalCount>0 then
    mmChartProps.Lines.Add(Format('Total Value Avg=%g',[aTotalSum/aTotalCount]));

  i:=DaysBetween(aInputData.DirectGetItem_DataDateTime(aInputData.Count-1),
                 aInputData.DirectGetItem_DataDateTime(aFVI));
  mmChartProps.Lines.Add(Format('Total Days=%d',[i]));
  grReportChangeRecord(nil);
end;

procedure TfmValueSupportStatisticsDialog.CalculateWeekDayStatistics(const aFrom, aTo: TTime);
var
  i,j: Integer;
  aWeekdayStatisticsHL: array [1..7] of TSCRealNumber;
  aWeekdayStatisticsCNT: array [1..7] of integer;
  aDT: TDateTime;
  aInputData : ISCInputDataCollection;
begin
  TWaitCursor.SetUntilIdle;

  aInputData:=(FIndicator as ISCIndicator).GetInputData;
  ZeroMemory(@aWeekdayStatisticsHL[1],7*SizeOf(TSCRealNumber));
  ZeroMemory(@aWeekdayStatisticsCNT[1],7*SizeOf(integer));

  for i:=(FIndicator as ISCIndicator).GetFirstValidValueIndex to (FIndicator as ISCIndicator).GetInputData.Count-1 do
  begin
    aDT:=aInputData.DirectGetItem_DataDateTime(i);
    if (CompareDateTime(Frac(aDT),aFrom)>=0) and (CompareDateTime(Frac(aDT),aTo)<=0) then
    begin
      j:=DayOfTheWeek(aDT);
      aWeekdayStatisticsHL[j]:=aWeekdayStatisticsHL[j]+ FIndicator.GetValue(i);
      aWeekdayStatisticsCNT[j]:=aWeekdayStatisticsCNT[j]+1;
    end;
  end;

  inc(FCalculating);
  try
    taWeekdayStatistics.EmptyTable;
    taWeekdayStatistics.Open;
    for I := 1 to 7 do
    begin
      taWeekdayStatistics.Append;
      taWeekdayStatisticsWeekday.Value:=WeekdaysLong[i];
      if aWeekdayStatisticsCNT[i]<>0 then
        taWeekdayStatisticsValue.Value:=RoundTo(aWeekdayStatisticsHL[i]/aWeekdayStatisticsCNT[i],-6);
      taWeekdayStatistics.Post;
    end;
  finally
    dec(FCalculating);
  end;

  grWeekdayStatistics.RefreshSort;
end;

procedure TfmValueSupportStatisticsDialog.cbValueSupportChange(Sender: TObject);
var
  i: Integer;
  aMinute: integer;
  aCount:integer;
  aValue: TSCRealNumber;
  aCurrentMonth: integer;
  aCurrentYear: integer;
  aInputData : ISCInputDataCollection;
begin
  TWaitCursor.SetUntilIdle;
  chMonthVolatilityValue.Clear;
  aInputData:=(FIndicator as ISCIndicator).GetInputData;

  if cbValueSupport.ItemIndex<>-1 then
  begin
    aMinute:=integer(cbValueSupport.Items.Objects[cbValueSupport.ItemIndex])*StockChart.StockSymbol.GetTimeIntervalValue;

    aValue:=0;
    aCount:=0;
    aCurrentMonth:=-1;
    aCurrentYear:=-1;
    chMonthVolatility.AutoRepaint:=false;
    try
      for i:=(FIndicator as ISCIndicator).GetFirstValidValueIndex to (FIndicator as ISCIndicator).GetInputData.Count-1 do
      begin
        if (aCurrentMonth<>MonthOf(aInputData.DirectGetItem_DataDateTime(i))) or
           (aCurrentYear<>YearOf(aInputData.DirectGetItem_DataDateTime(i))) or
           (i=aInputData.Count-1) then
        begin
          if aCurrentMonth<>-1 then
          begin
            if aCount=0 then
            begin
              chIntradayValue.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),0);
            end
            else begin
              chMonthVolatilityValue.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),aValue/aCount);
            end;
          end;

          aCurrentMonth:=MonthOf(aInputData.DirectGetItem_DataDateTime(i));
          aCurrentYear:=YearOf(aInputData.DirectGetItem_DataDateTime(i));

          aValue:=0;
          aCount:=0;
        end;

        if MinuteOfTheDay(aInputData.DirectGetItem_DataDateTime(i))=aMinute then
        begin
          aValue:=aValue+FIndicator.GetValue(i);
          inc(aCount);
        end;
      end;
    finally
      chMonthVolatility.AutoRepaint:=true;
      chMonthVolatility.Repaint;
    end;
  end;
end;

procedure TfmValueSupportStatisticsDialog.ckAllPeaksShowMarksClick(Sender: TObject);
begin
  inherited;
  chAllPeaksValue.Marks.Visible:=ckAllPeaksShowMarks.Checked;
end;

procedure TfmValueSupportStatisticsDialog.ckHLClick(Sender: TObject);
begin
  edValueTreshold.Enabled:=ckValueTreshold.Checked;
  edValueTresholdChange(nil);
end;

constructor TfmValueSupportStatisticsDialog.Create(const aStockChart: IStockChart);
begin
  inherited;
  //edValueTreshold.ValueFloat:=Workspace.Storage(self).Read(edValueTreshold,'Value',0);

  RegisterPersistValue(ckValueTreshold,true);
  RegisterPersistValue(ckAllPeaksShowMarks,false);

  chIntradayValue.Clear;
  chMonthVolatilityValue.Clear;
  chAllPeaksValue.Clear;

  pcPages.ActivePageIndex:=0;
  pcPagesChange(nil);
end;

destructor TfmValueSupportStatisticsDialog.Destroy;
begin
  //Workspace.Storage(self).WriteInteger(edHL,'Value',edHL.Value);
  inherited;
end;

procedure TfmValueSupportStatisticsDialog.Init(const aIndicator: ISCIndicatorValueSupport);
begin
  FIndicator:=aIndicator;
  Caption:=IndicatorFactory.GetIndicatorInfo((FIndicator as ISCIndicator).GetIID).Name+': '+Caption;
  pcPagesChange(nil);
end;

procedure TfmValueSupportStatisticsDialog.edValueTresholdChange(Sender: TObject);
begin
  inherited;
  if Visible then
    CalculateAllPeaks;
end;

procedure TfmValueSupportStatisticsDialog.grReportChangeRecord(Sender: TObject);
begin
  if FCalculating>0  then
    exit;

  CalculateWeekDayStatistics(
    taReportTime.Value,
    taReportTime.Value+(StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay)-1/SecsPerDay);
end;

procedure TfmValueSupportStatisticsDialog.pcPagesChange(Sender: TObject);
begin
  inherited;
  if not Visible then
    exit;

  if pcPages.ActivePage=tsIntradays then
  begin
    laHelp.Caption:='This page shows bar''s intraday volatility and it''s dependency from the day of the week';
    if (pcPages.ActivePage.Tag=0) then
      CalculateIntradays;
    pcPages.ActivePage.Tag:=1;
  end
  else if pcPages.ActivePage=tsMonthVolatility then
  begin
    laHelp.Caption:='This page shows how the selected bar''s volatility changes from month to month';
    if (pcPages.ActivePage.Tag=0) then
      CaclulateMonthVolatility;
    pcPages.ActivePage.Tag:=1;
  end
  else if pcPages.ActivePage=tsAllPeaks then
  begin
    laHelp.Caption:='This page shows all peaks that meet the conditions';
    if (pcPages.ActivePage.Tag=0) then
      CalculateAllPeaks;
    pcPages.ActivePage.Tag:=1;
  end;
end;

class procedure TfmValueSupportStatisticsDialog.Run(const aIndicator: ISCIndicatorValueSupport; const aStockChart: IStockChart);
begin
  with TfmValueSupportStatisticsDialog.Create(aStockChart) do
  begin
    Init(aIndicator);
    Show;
    pcPagesChange(nil);
  end;
end;

end.


