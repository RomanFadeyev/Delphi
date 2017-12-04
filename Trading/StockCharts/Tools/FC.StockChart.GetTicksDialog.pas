unit FC.StockChart.GetTicksDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, Grids, DBGrids, MultiSelectDBGrid, ColumnSortDBGrid,
  EditDBGrid, DB, FC.Dialogs.DockedDialogCloseAndAppWindow_B, ImgList, JvCaptionButton, JvComponentBase,
  JvDockControlForm,StockChart.Definitions, FC.Definitions,FC.Singletons,DBUtils, ComCtrls, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
  VclTee.Chart, JvExComCtrls, JvComCtrls,Contnrs, FC.StockChart.CustomDialog_B;

type
  TfmGetTicksDialog = class(TfmStockChartCustomDialog_B)
    dsData: TDataSource;
    pcPages: TPageControl;
    tsData: TTabSheet;
    tsTicksInBar: TTabSheet;
    Panel2: TPanel;
    cbIntervals: TExtendComboBox;
    Label5: TLabel;
    tcTabs: TFlatTabControl;
    pcAverageTickTabs: TJvPageControl;
    tsAbsoluteValues: TTabSheet;
    chTickInBarStatistics: TChart;
    chTickInBarStatisticsValue: TBarSeries;
    tsPercentage: TTabSheet;
    chPercentage: TChart;
    BarSeries1: TPieSeries;
    Panel3: TPanel;
    Label6: TLabel;
    ckThu: TExtendCheckBox;
    ckWed: TExtendCheckBox;
    ckMon: TExtendCheckBox;
    ckTue: TExtendCheckBox;
    ckFri: TExtendCheckBox;
    Label7: TLabel;
    ckTime1: TExtendCheckBox;
    ckTime2: TExtendCheckBox;
    ckTime3: TExtendCheckBox;
    ckTime4: TExtendCheckBox;
    Bevel1: TBevel;
    laTicksFound: TLabel;
    chTickInBarStatisticsVolumes: TFastLineSeries;
    chTicks: TChart;
    chTicksEmulated: TFastLineSeries;
    Splitter1: TSplitter;
    Panel4: TPanel;
    chTicksReal: TFastLineSeries;
    paDataReal: TPanel;
    grData: TEditDBGrid;
    Label3: TLabel;
    Panel6: TPanel;
    EditDBGrid1: TEditDBGrid;
    Label8: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    laAverageTicksPerMinute: TLabel;
    laTotalTicks: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    laTotalMinutes: TLabel;
    Bevel2: TBevel;
    ckTickDataShowMarks: TExtendCheckBox;
    procedure ckTickDataShowMarksClick(Sender: TObject);
    procedure tsDataResize(Sender: TObject);
    procedure ckMonClick(Sender: TObject);
    procedure tcTabsChange(Sender: TObject);
    procedure cbIntervalsChange(Sender: TObject);
    procedure pcPagesChange(Sender: TObject);
  private
    FDataSet: IDsPackage;
    FAggregatedTicks  : array [TStockTimeInterval] of TObjectList; //of TTickData
    FStart,FEnd: TDateTime;
  protected

    procedure LoadData;
    procedure LoadTicksInBarStatistics;
    procedure LoadPercentageInBarStatistics;
  public
    class procedure Run(const aStockChart: IStockChart; const aStart,aEnd:TDateTime);

    constructor Create(const aStockChart: IStockChart); override;
    destructor Destroy; override;
  end;

  TTickData = class
  public
    DateTime: TDateTime;
    CountInDateTime: integer;

    constructor Create(const aDateTime: TDateTime; aCountInDateTime: integer);
  end;

implementation
  uses SystemService,Math,DateUtils, Collections.Map, Application.Definitions,
  FC.DataUtils,FC.StockData.DataQueryToInputDataCollectionMediator,
  FC.StockData.StockTickCollection,
  ufmForm_B;
{$R *.dfm}

{ TfmDialogClose_B1 }

procedure TfmGetTicksDialog.cbIntervalsChange(Sender: TObject);
begin
  inherited;
  if pcPages.ActivePage=tsTicksInBar then
  begin
    tsTicksInBar.Tag:=0;
    LoadTicksInBarStatistics;
  end;
end;

procedure TfmGetTicksDialog.ckMonClick(Sender: TObject);
begin
  if Visible then //Срабатывает при загрузке формы
    LoadPercentageInBarStatistics;
end;

procedure TfmGetTicksDialog.ckTickDataShowMarksClick(Sender: TObject);
begin
  inherited;
  chTicksReal.Marks.Visible:=ckTickDataShowMarks.Checked;
  chTicksEmulated.Marks.Visible:=ckTickDataShowMarks.Checked;
end;

constructor TfmGetTicksDialog.Create(const aStockChart: IStockChart);
var
  aInt: TStockTimeInterval;
begin
  inherited;

  pcPages.ActivePageIndex:=0;
  pcAverageTickTabs.ActivePageIndex:=0;

  for aInt := low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    cbIntervals.AddItem(StockTimeIntervalNames[aInt],TObject(aInt));
  end;

  ckMon.Caption:=WeekDaysLong[1];
  ckTue.Caption:=WeekDaysLong[2];
  ckWed.Caption:=WeekDaysLong[3];
  ckThu.Caption:=WeekDaysLong[4];
  ckFri.Caption:=WeekDaysLong[5];

  RegisterPersistValue(ckMon,true);
  RegisterPersistValue(ckTue,true);
  RegisterPersistValue(ckWed,true);
  RegisterPersistValue(ckThu,true);
  RegisterPersistValue(ckFri,true);
  RegisterPersistValue(ckTime1,true);
  RegisterPersistValue(ckTime2,true);
  RegisterPersistValue(ckTime3,true);
  RegisterPersistValue(ckTime4,true);
end;

destructor TfmGetTicksDialog.Destroy;
var
  i:TStockTimeInterval;
begin
  for i := Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FreeAndNil(FAggregatedTicks[i]);
  inherited;
end;

procedure TfmGetTicksDialog.LoadData;
var
  aMinutes: integer;
  aMinutesQuery: IStockDataQuery;
  aMinBars: ISCInputDataCollection;
  i: Integer;
  aTicksInBar : IStockTickCollectionWriteable;
begin
  if tsData.Tag=1 then
    exit;

  TWaitCursor.SetUntilIdle;

  tsData.Tag:=1;
  FDataSet:=StockDataStorage.QueryStockDataTicksAsDataSet(
              StockChart.StockSymbol.Name,
              StockDataStorage.CreateDataTickFilterAdapter.DateBetween(FStart,FEnd),
              true);

  FDataSet.Data.First;
  chTicksReal.AutoRepaint:=false;
  chTicksEmulated.AutoRepaint:=false;
  try
    //реальные
    while not FDataSet.Data.EOF do
    begin
      chTicksReal.AddXY( FDataSet.Data.FieldByName('DATE_TICK').AsDateTime,FDataSet.Data.FieldByName('VALUE_TICK').AsFloat);
      FDataSet.Data.Next;
    end;

    //эмуляция
    aMinBars:=TStockDataQueryToInputDataCollectionMediator.Create(
                 StockDataStorage.QueryStockData(StockChart.StockSymbol.Name,sti1,
                                                  StockDataStorage.CreateDataFilterAdapter.DateBetween(FStart,FEnd)),nil);

    aTicksInBar:=TStockTickCollection.Create;
    for i := 0 to aMinBars.Count - 1 do
      TStockDataUtils.GenerateTicksInBar(aMinBars,i,aTicksInBar);

    for i := 0 to aTicksInBar.Count - 1 do
      chTicksEmulated.AddXY(aTicksInBar.GetDateTime(i),aTicksInBar.GetValue(i));
  finally
    chTicksReal.AutoRepaint:=false;
    chTicksReal.Repaint;
    chTicksEmulated.AutoRepaint:=false;
    chTicksEmulated.Repaint;
  end;
  FDataSet.Data.First;


  aMinutesQuery:=StockDataStorage.QueryStockData(StockChart.StockSymbol.Name,sti1,
    StockDataStorage.CreateDataFilterAdapter.DateBetween(FStart,FEnd));

  aMinutes:=aMinutesQuery.RecordCount;
  laTotalTicks.Caption:=IntToStr(FDataSet.Data.RecordCount);
  laTotalMinutes.Caption:=IntToStr(aMinutes);

  if (aMinutes=0) then
    laAverageTicksPerMinute.Caption:='-'
  else
    laAverageTicksPerMinute.Caption:=FloatToStr(RoundTo(FDataSet.Data.RecordCount/aMinutes,-2));

  dsData.DataSet:=FDataSet.Data;
end;

procedure TfmGetTicksDialog.LoadPercentageInBarStatistics;
var
  it: TMapIterator<integer,integer>;
  b: boolean;
  aWeekdays: array [1..7] of boolean;
  aHours: array [0..23] of boolean;

  aCountRepetitions : TMap<integer,integer>;
  aTickData : TTickData;
  i: integer;
  aInterval: TStockTimeInterval;
  aCountFound: integer;
  aTmp: integer;
begin
  aWeekdays[1]:=ckMon.Checked;
  aWeekdays[2]:=ckTue.Checked;
  aWeekdays[3]:=ckWed.Checked;
  aWeekdays[4]:=ckThu.Checked;
  aWeekdays[5]:=ckFri.Checked;
  aWeekdays[6]:=false;
  aWeekdays[7]:=false;

  for i := 2 to 7 do
    aHours[i]:=ckTime1.Checked;
  for i := 8 to 13 do
    aHours[i]:=ckTime2.Checked;
  for i := 14 to 19 do
    aHours[i]:=ckTime3.Checked;
  for i := 20 to 23 do
    aHours[i]:=ckTime4.Checked;
  aHours[0]:=ckTime4.Checked;
  aHours[1]:=ckTime4.Checked;

  aInterval:=TStockTimeInterval(cbIntervals.ItemIndex);


  aCountRepetitions:=TMap<integer,integer>.Create();
  aCountFound:=0;
  try
    BarSeries1.Clear;
    for i := 0 to FAggregatedTicks[aInterval].Count - 1 do
    begin
      aTickData:=TTickData(FAggregatedTicks[aInterval][i]);

      //Проверяем, чтобы удовлетворяло критериям выборки
      if aWeekdays[DayOfTheWeek(aTickData.DateTime)] and aHours[HourOfTheDay(aTickData.DateTime)] then
      begin
        aTmp:=0;
        aCountRepetitions.Lookup(aTickData.CountInDateTime,aTmp);
        inc(aTmp);
        aCountRepetitions.Add(aTickData.CountInDateTime,aTmp);
        inc(aCountFound);
      end;
    end;

    //считаем процент повторяемости баров
    b:=aCountRepetitions.GetFirst(it);
    while b do
    begin
      BarSeries1.Add(it.Value,'Bars with '+IntToStr(it.Key)+' ticks');
      b:=aCountRepetitions.GetNext(it);
    end;

    laTicksFound.Caption:=Format('%d ticks found',[aCountFound]);
  finally
    aCountRepetitions.Free;
  end;
end;

procedure TfmGetTicksDialog.LoadTicksInBarStatistics;
var
  aTicks   : IStockDataTickAggregatedQuery;
  aBars    : ISCInputDataCollection;
  aInterval: TStockTimeInterval;
  aDate,aDate2    : TDateTime;
  aTickData : TTickData;
  aMax: integer;
  i: integer;
begin
  if tsTicksInBar.Tag=1 then
   exit;

  aInterval:=TStockTimeInterval(cbIntervals.ItemIndex);

  TWaitCursor.SetUntilIdle;
  tsTicksInBar.Tag:=1;

  //Загружаем собранные по count данные
  if FAggregatedTicks[aInterval]=nil then
  begin
    aTicks:=StockDataStorage.QueryStockDataTicksAggregated(
               StockChart.StockSymbol.Name,aInterval,
               StockDataStorage.CreateDataTickFilterAdapter.DateBetween(FStart,FEnd));

    FAggregatedTicks[aInterval]:=TObjectList.Create;

    while not aTicks.EOF do
    begin
      FAggregatedTicks[aInterval].Add(TTickData.Create(aTicks.GetDateTime,aTicks.GetCountInDateTime));
      aTicks.Next;
    end;
  end;

  //загружаем бары, они нам будут нужны, чтобы показать дырки в истории
  aBars:=TStockDataQueryToInputDataCollectionMediator.Create(
              StockDataStorage.QueryStockData(StockChart.StockSymbol.Name,aInterval,
                                              StockDataStorage.CreateDataFilterAdapter.DateBetween(FStart,FEnd)),
              nil);

  chTickInBarStatistics.BottomAxis.Increment:=StockTimeIntervalValues[aInterval]*MinuteAsDateTime;
  chTickInBarStatistics.AutoRepaint:=false;
  chTickInBarStatisticsVolumes.AutoRepaint:=false;
  aMax:=0;
  try
    //Сначала заполняем тики
    chTickInBarStatisticsValue.Clear;
    chTickInBarStatisticsVolumes.Clear;
    for i := 0 to FAggregatedTicks[aInterval].Count - 1 do
    begin
      aTickData:=TTickData(FAggregatedTicks[aInterval][i]);
      chTickInBarStatisticsValue.AddXY(aTickData.DateTime,aTickData.CountInDateTime);
      aMax:=max(aMax,aTickData.CountInDateTime);
    end;

    //Теперь поверх заполняем желтым цветом дырки в истории
    TStockDataUtils.AlignTime(FStart,aInterval,aDate,aDate2);
    while aDate<FEnd do
    begin
      if aBars.FindExactMatched(aDate)=-1 then
        chTickInBarStatisticsValue.AddXY(aDate,aMax,'',clYellow);
      aDate:=TStockDataUtils.AddMinutes(aDate,aInterval);
    end;

    for i := 0 to aBars.Count - 1 do
      chTickInBarStatisticsVolumes.AddXY(aBars.DirectGetItem_DataDateTime(i),aBars.DirectGetItem_DataVolume(i))

  finally
    chTickInBarStatistics.AutoRepaint:=true;
    chTickInBarStatisticsVolumes.AutoRepaint:=true;
    chTickInBarStatistics.Repaint;
    chTickInBarStatisticsVolumes.Repaint;
  end;

  LoadPercentageInBarStatistics;  
end;

procedure TfmGetTicksDialog.pcPagesChange(Sender: TObject);
begin
  inherited;
  if ([csLoading, csReading, csDestroying] * ComponentState)=[] then
  begin
    if pcPages.ActivePage=tsData then
      LoadData
    else
      LoadTicksInBarStatistics;
  end;
end;

class procedure TfmGetTicksDialog.Run(const aStockChart: IStockChart; const aStart, aEnd: TDateTime);
begin
  with TfmGetTicksDialog.Create(aStockChart) do
  begin
    TWaitCursor.SetUntilIdle;

    Caption:='Ticks from '+DateTimeToStr(aStart)+' to '+DateTimeToStr(aEnd);
    FStart:=aStart;
    FEnd:=aEnd-1/SecsPerDay;
    cbIntervals.ItemIndex:=integer(StockChart.StockSymbol.TimeInterval);

    LoadData;
  end;
end;

procedure TfmGetTicksDialog.tcTabsChange(Sender: TObject);
begin
  pcAverageTickTabs.ActivePageIndex:=tcTabs.TabIndex;
end;

procedure TfmGetTicksDialog.tsDataResize(Sender: TObject);
begin
  inherited;
  paDataReal.Width:=paDataReal.Parent.Width div 2-10;
end;

{ TTickData }

constructor TTickData.Create(const aDateTime: TDateTime; aCountInDateTime: integer);
begin
  DateTime:=aDateTime;
  CountInDateTime:=aCountInDateTime;
end;

end.
