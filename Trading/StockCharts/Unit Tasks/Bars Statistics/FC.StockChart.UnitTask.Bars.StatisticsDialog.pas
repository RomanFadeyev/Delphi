unit FC.StockChart.UnitTask.Bars.StatisticsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls, TeEngine, Series, TeeProcs, Chart, FC.StockChart.CustomDialog_B,
  ImgList, JvCaptionButton, JvComponentBase, JvDockControlForm;

type
  TfmBarsStatisticsDialog = class(TfmStockChartCustomDialog_B)
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    pbProgress: TProgressBar;
    taReportTime: TTimeField;
    taReportOC: TFloatField;
    taReportHL: TFloatField;
    pcPages: TPageControl;
    tsIntradays: TTabSheet;
    tsMonthVolatility: TTabSheet;
    Label1: TLabel;
    grReport: TEditDBGrid;
    Label2: TLabel;
    grWeekdayStatistics: TEditDBGrid;
    DataSource2: TDataSource;
    taWeekdayStatistics: TMemoryDataSet;
    taWeekdayStatisticsOC: TFloatField;
    taWeekdayStatisticsHL: TFloatField;
    taWeekdayStatisticsWeekday: TStringField;
    chIntraday: TChart;
    chIntradayOC: TBarSeries;
    chIntradayHL: TBarSeries;
    Label3: TLabel;
    cbBars: TExtendComboBox;
    chMonthVolatility: TChart;
    laHelp: TTipPanel;
    chMonthVolatilityOC: TBarSeries;
    chMonthVolatilityHL: TBarSeries;
    tsAllPeaks: TTabSheet;
    Panel1: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    edOC: TExtendSpinEdit;
    ckOC: TExtendCheckBox;
    ckHL: TExtendCheckBox;
    edHL: TExtendSpinEdit;
    buOR: TRadioButton;
    buAND: TRadioButton;
    chAllPeaks: TChart;
    chAllPeaksHL: TBarSeries;
    chAllPeaksOC: TBarSeries;
    ckAllPeaksShowMarks: TExtendCheckBox;
    Bevel1: TBevel;
    procedure ckAllPeaksShowMarksClick(Sender: TObject);
    procedure edOCChange(Sender: TObject);
    procedure ckHLClick(Sender: TObject);
    procedure cbBarsChange(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
    procedure grReportChangeRecord(Sender: TObject);
  private
    FCalculating : integer;
    FIndicator: ISCIndicatorBars;

    procedure CalculateIntradays;
    procedure CaclulateMonthVolatility;
    procedure CalculateWeekDayStatistics(const aFrom,aTo: TTime);
    procedure CalculateAllPeaks;
  public
    class procedure Run(const aIndicator: ISCIndicatorBars; const aStockChart: IStockChart);

    constructor Create(const aStockChart: IStockChart); override;
    destructor Destroy; override;
  end;


implementation
  uses Math,DateUtils,Application.Definitions;

type
  TDirection = (dNone,dUp,dDown);

{$R *.dfm}

{ TfmCalculateWidthDialog }

procedure TfmBarsStatisticsDialog.CaclulateMonthVolatility;
var
  aInterval: integer;
  i: integer;
begin
  aInterval:=StockChart.StockSymbol.GetTimeIntervalValue;
  cbBars.Clear;
  for i := 0 to (1440 div aInterval)-1 do
    cbBars.AddItem(TimeToStr(i*aInterval/MinsPerDay),TObject(i));
end;

procedure TfmBarsStatisticsDialog.CalculateAllPeaks;
var
  i: integer;
  aInputData: ISCInputDataCollection;
  aHL,aOC:integer;
  aHLMax,aOCMax: integer;
  aHLR,aOCR: boolean;
  aIsPeak:boolean;
begin
  TWaitCursor.SetUntilIdle;
  aInputData:=FIndicator.GetInputData;

  chAllPeaks.AutoRepaint:=false;
  try
    chAllPeaksHL.Clear;
    chAllPeaksOC.Clear;

    aHLMax:=edHL.Value;
    aOCMax:=edOC.Value;
    if not ckHL.Checked then
      aHLMax:=MaxInt;
    if not ckOC.Checked then
      aOCMax:=MaxInt;

    for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
    begin
      aHL:=aInputData.PriceToPoint(abs(aInputData.DirectGetItem_DataHigh(i)-aInputData.DirectGetItem_DataLow(i)));
      aOC:=aInputData.PriceToPoint(abs(aInputData.DirectGetItem_DataOpen(i)-aInputData.DirectGetItem_DataClose(i)));

      aIsPeak:=false;
      aHLR:=aHL>aHLMax;
      aOCR:=aOC>aOCMax;

      if ckHL.Checked and ckOC.Checked then
      begin
        if buAND.Checked then
          aIsPeak:=aHLR and aOCR
        else
          aIsPeak:=aHLR or aOCR;
      end
      else if ckHL.Checked then
        aIsPeak:=aHLR
      else if ckOC.Checked then
        aIsPeak:=aOCR;

      if aIsPeak then
      begin
        chAllPeaksHL.AddXY(aInputData.DirectGetItem_DataDateTime(i),aHL);
        chAllPeaksOC.AddXY(aInputData.DirectGetItem_DataDateTime(i),aOC);
      end;
    end;
  finally
    chAllPeaks.AutoRepaint:=true;
    chAllPeaks.Repaint;
    ckAllPeaksShowMarksClick(nil);
  end;
end;

procedure TfmBarsStatisticsDialog.CalculateIntradays;
var
  i,j: Integer;
  aInputData: ISCInputDataCollection;
  aIntradayStatisticsOC: array of TSCRealNumber;
  aIntradayStatisticsHL: array of TSCRealNumber;
  aIntradayStatisticsCNT: array of TSCRealNumber;
  aInterval: integer;
begin
  TWaitCursor.SetUntilIdle;
  aInputData:=FIndicator.GetInputData;

  aInterval:=StockChart.StockSymbol.GetTimeIntervalValue;
  SetLength(aIntradayStatisticsOC,1440 div aInterval);
  SetLength(aIntradayStatisticsHL,1440 div aInterval);
  SetLength(aIntradayStatisticsCNT,1440 div aInterval);

  for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
  begin
    j:=MinuteOfTheDay(aInputData.DirectGetItem_DataDateTime(i));
    j:=j div aInterval;
    aIntradayStatisticsOC[j]:=aIntradayStatisticsOC[j]+
                             Abs(aInputData.DirectGetItem_DataClose(i)-aInputData.DirectGetItem_DataOpen(i));
    aIntradayStatisticsHL[j]:=aIntradayStatisticsHL[j]+
                             Abs(aInputData.DirectGetItem_DataHigh(i)-aInputData.DirectGetItem_DataLow(i));
    aIntradayStatisticsCNT[j]:=aIntradayStatisticsCNT[j]+1;
  end;

  inc(FCalculating);
  try
    taReport.DisableControls;
    taReport.EmptyTable;
    taReport.Open;
    chIntradayOC.Clear;
    chIntradayHL.Clear;

    for I := 0 to High(aIntradayStatisticsOC) do
    begin
      taReport.Append;
      taReportTime.Value:=i*aInterval/MinsPerDay;
      if aIntradayStatisticsCNT[i]<>0 then
      begin
        taReportOC.Value:=RoundTo(aInputData.PriceToPoint(aIntradayStatisticsOC[i])/aIntradayStatisticsCNT[i],-2);
        taReportHL.Value:=RoundTo(aInputData.PriceToPoint(aIntradayStatisticsHL[i])/aIntradayStatisticsCNT[i],-2);
        chIntradayOC.AddXY(taReportTime.Value,taReportOC.Value);
        chIntradayHL.AddXY(taReportTime.Value,taReportHL.Value);
      end
      else begin
        chIntradayOC.AddXY(taReportTime.Value,0);
        chIntradayHL.AddXY(taReportTime.Value,0);
      end;
      taReport.Post;
    end;

    taReport.First;
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


  grReportChangeRecord(nil);
end;

procedure TfmBarsStatisticsDialog.CalculateWeekDayStatistics(const aFrom, aTo: TTime);
var
  i,j: Integer;
  aInputData: ISCInputDataCollection;
  aWeekdayStatisticsOC: array [1..7] of TSCRealNumber;
  aWeekdayStatisticsHL: array [1..7] of TSCRealNumber;
  aWeekdayStatisticsCNT: array [1..7] of integer;
  aDT: TDateTime;
begin
  TWaitCursor.SetUntilIdle;
  aInputData:=FIndicator.GetInputData;

  ZeroMemory(@aWeekdayStatisticsOC[1],7*SizeOf(TSCRealNumber));
  ZeroMemory(@aWeekdayStatisticsHL[1],7*SizeOf(TSCRealNumber));
  ZeroMemory(@aWeekdayStatisticsCNT[1],7*SizeOf(integer));

  for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
  begin
    aDT:=aInputData.DirectGetItem_DataDateTime(i);
    if (CompareDateTime(Frac(aDT),aFrom)>=0) and (CompareDateTime(Frac(aDT),aTo)<=0) then
    begin
      j:=DayOfTheWeek(aDT);
      aWeekdayStatisticsOC[j]:=aWeekdayStatisticsOC[j]+
                               Abs(aInputData.DirectGetItem_DataClose(i)-aInputData.DirectGetItem_DataOpen(i));
      aWeekdayStatisticsHL[j]:=aWeekdayStatisticsHL[j]+
                               Abs(aInputData.DirectGetItem_DataHigh(i)-aInputData.DirectGetItem_DataLow(i));

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
      begin
        taWeekdayStatisticsOC.Value:=RoundTo(aInputData.PriceToPoint(aWeekdayStatisticsOC[i])/aWeekdayStatisticsCNT[i],-2);
        taWeekdayStatisticsHL.Value:=RoundTo(aInputData.PriceToPoint(aWeekdayStatisticsHL[i])/aWeekdayStatisticsCNT[i],-2);
      end;
      taWeekdayStatistics.Post;
    end;
  finally
    dec(FCalculating);
  end;

  grWeekdayStatistics.RefreshSort;
end;

procedure TfmBarsStatisticsDialog.cbBarsChange(Sender: TObject);
var
  i: Integer;
  aInputData: ISCInputDataCollection;
  aMinute: integer;
  aCount:integer;
  aOC,aHL: TSCRealNumber;
  aCurrentMonth: integer;
  aCurrentYear: integer;
begin
  TWaitCursor.SetUntilIdle;
  chMonthVolatilityOC.Clear;
  chMonthVolatilityHL.Clear;

  if cbBars.ItemIndex<>-1 then
  begin
    aMinute:=integer(cbBars.Items.Objects[cbBars.ItemIndex])*StockChart.StockSymbol.GetTimeIntervalValue;
    aInputData:=FIndicator.GetInputData;

    aOC:=0;
    aHL:=0;
    aCount:=0;
    aCurrentMonth:=-1;
    aCurrentYear:=-1;
    chMonthVolatility.AutoRepaint:=false;
    try
      for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
      begin
        if (aCurrentMonth<>MonthOf(aInputData.DirectGetItem_DataDateTime(i))) or
           (aCurrentYear<>YearOf(aInputData.DirectGetItem_DataDateTime(i))) or
           (i=aInputData.Count-1) then
        begin
          if aCurrentMonth<>-1 then
          begin
            if aCount=0 then
            begin
              chIntradayOC.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),0);
              chIntradayHL.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),0);
            end
            else begin
              chMonthVolatilityOC.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),aInputData.PriceToPoint(aOC/aCount));
              chMonthVolatilityHL.AddXY(EncodeDate(aCurrentYear,aCurrentMonth,1),aInputData.PriceToPoint(aHL/aCount));
            end;
          end;

          aCurrentMonth:=MonthOf(aInputData.DirectGetItem_DataDateTime(i));
          aCurrentYear:=YearOf(aInputData.DirectGetItem_DataDateTime(i));

          aOC:=0;
          aHL:=0;
          aCount:=0;
        end;

        if MinuteOfTheDay(aInputData.DirectGetItem_DataDateTime(i))=aMinute then
        begin
          aOC:=aOC+Abs(aInputData.DirectGetItem_DataClose(i)-aInputData.DirectGetItem_DataOpen(i));
          aHL:=aHL+Abs(aInputData.DirectGetItem_DataHigh(i)-aInputData.DirectGetItem_DataLow(i));
          inc(aCount);
        end;
      end;
    finally
      chMonthVolatility.AutoRepaint:=true;
      chMonthVolatility.Repaint;
    end;
  end;
end;

procedure TfmBarsStatisticsDialog.ckAllPeaksShowMarksClick(Sender: TObject);
begin
  inherited;
  chAllPeaksHL.Marks.Visible:=ckAllPeaksShowMarks.Checked;
end;

procedure TfmBarsStatisticsDialog.ckHLClick(Sender: TObject);
begin
  edOC.Enabled:=ckOC.Checked;
  edHL.Enabled:=ckHL.Checked;
  buAND.Enabled:=edOC.Enabled and edHL.Enabled;
  buOR.Enabled:=edOC.Enabled and edHL.Enabled;
  edOCChange(nil);
end;

constructor TfmBarsStatisticsDialog.Create(const aStockChart: IStockChart);
begin
  inherited;
  edHL.Value:=Workspace.Storage(self).ReadInteger(edHL,'Value',20);
  edOC.Value:=Workspace.Storage(self).ReadInteger(edOC,'Value',10);

  RegisterPersistValue(buAND,true);
  buOR.Checked:=not buAND.Checked;
  RegisterPersistValue(ckOC,true);
  RegisterPersistValue(ckHL,true);
  RegisterPersistValue(ckAllPeaksShowMarks,false);

  pcPages.ActivePageIndex:=0;
  pcPagesChange(nil);
end;

destructor TfmBarsStatisticsDialog.Destroy;
begin
  Workspace.Storage(self).WriteInteger(edHL,'Value',edHL.Value);
  Workspace.Storage(self).WriteInteger(edOC,'Value',edOC.Value);
  inherited;
end;

procedure TfmBarsStatisticsDialog.edOCChange(Sender: TObject);
begin
  inherited;
  if Visible then
    CalculateAllPeaks;
end;

procedure TfmBarsStatisticsDialog.grReportChangeRecord(Sender: TObject);
begin
  if FCalculating>0  then
    exit;

  CalculateWeekDayStatistics(
    taReportTime.Value,
    taReportTime.Value+(StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay)-1/SecsPerDay);
end;

procedure TfmBarsStatisticsDialog.pcPagesChange(Sender: TObject);
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

class procedure TfmBarsStatisticsDialog.Run(const aIndicator: ISCIndicatorBars; const aStockChart: IStockChart);
begin
  with TfmBarsStatisticsDialog.Create(aStockChart) do
  begin
    FIndicator:=aIndicator;
    Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;
    Show;
    pcPagesChange(nil);    
  end;
end;

end.


