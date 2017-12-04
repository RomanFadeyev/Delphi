unit FC.Trade.Trader.TrendFollower.TestBenchDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ComCtrls,
  FC.Trade.Trader.TrendFollower,FC.DataUtils, FC.Dialogs.DockedDialogCloseAndAppWindow_B, ImgList, JvComponentBase,
  JvCaptionButton, CheckLst,
  FC.Definitions, StockChart.Definitions.Units, StockChart.Definitions, JvDockControlForm,
  FC.Trade.Trader.TestBenchDialog_B,
  FC.Trade.Trader.Base, Grids, DBGrids, MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, DB, MemoryDS, JvExExtCtrls,
  JvNetscapeSplitter, Mask;

type
  TfmTrendFollowerTestBenchDialog = class(TfmTestBenchDialog_B)
    paWorkspace: TPanel;
    taTrendFollow: TMemoryDataSet;
    taTrendFollowTime: TDateTimeField;
    taTrendFollowTrendDir: TFloatField;
    dsTrendFollow: TDataSource;
    taTrendFollowClose: TFloatField;
    taTrendFollowNo: TIntegerField;
    PageControlEx1: TPageControlEx;
    TabSheet1: TTabSheet;
    paLeft: TPanel;
    Label1: TLabel;
    buStart: TButton;
    lbTimeIntervals: TCheckListBox;
    ckStopAfterEachRecord: TExtendCheckBox;
    buResume: TButton;
    buStop: TButton;
    JvNetscapeSplitter2: TJvNetscapeSplitter;
    grOrders: TEditDBGrid;
    TabSheet2: TTabSheet;
    taTrendFollow2: TMemoryDataSet;
    dsTrendFollow2: TDataSource;
    Panel1: TPanel;
    buStart2: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    taTrendFollow2Time: TDateTimeField;
    taTrendFollow2No: TIntegerField;
    Label2: TLabel;
    taTrendFollow2Guess: TFloatField;
    taTrendFollow2ZigZag4H: TFloatField;
    laStatisticsMatches: TLabel;
    Panel2: TPanel;
    edFilter: TExtendComboBox;
    EditDBGrid1: TEditDBGrid;
    taTrendFollow2EURUSD_H1_PbSAR: TFloatField;
    taTrendFollow2EURUSD_H1_Acc: TFloatField;
    taTrendFollow2EURUSD_H1_Fr: TFloatField;
    taTrendFollow2GBPUSD_H1_PbSAR: TFloatField;
    taTrendFollow2GBPUSD_H1_Acc: TFloatField;
    taTrendFollow2GBPUSD_H1_Fr: TFloatField;
    taTrendFollow2USDCHF_H1_PbSAR: TFloatField;
    taTrendFollow2USDCHF_H1_Acc: TFloatField;
    taTrendFollow2USDCHF_H1_Fr: TFloatField;
    taTrendFollow2EURUSD_H4_PbSAR: TFloatField;
    taTrendFollow2EURUSD_H4_Acc: TFloatField;
    taTrendFollow2EURUSD_H4_Fr: TFloatField;
    taTrendFollow2GBPUSD_H4_PbSAR: TFloatField;
    taTrendFollow2GBPUSD_H4_Acc: TFloatField;
    taTrendFollow2GBPUSD_H4_Fr: TFloatField;
    taTrendFollow2USDCHF_H4_PbSAR: TFloatField;
    taTrendFollow2USDCHF_H4_Acc: TFloatField;
    taTrendFollow2USDCHF_H4_Fr: TFloatField;
    laRecordCount: TLabel;
    taTrendFollow2Price: TFloatField;
    taTrendFollow2AtPbSARReverse: TFloatField;
    taTrendFollow2ZigZag1H: TFloatField;
    taTrendFollow2ZigZag1HTime: TDateTimeField;
    taTrendFollow2ZigZag4HTime: TDateTimeField;
    lbHours: TCheckListBox;
    procedure edFilterKeyPress(Sender: TObject; var Key: Char);
    procedure EditDBGrid1BeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure buStart2Click(Sender: TObject);
    procedure EditDBGrid1DblClick(Sender: TObject);
    procedure grOrdersDblClick(Sender: TObject);
    procedure grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure buStopClick(Sender: TObject);
    procedure buResumeClick(Sender: TObject);
    procedure buStartClick(Sender: TObject);

    function GetIndicatorFromChart(aChart: IStockChart; const aIID: TGUID): ISCIndicator;
    procedure CalcRecordCount;
  private
    FInputDatas         : TStockTimeIntervalInputDataCollectionArray;
    FCharts             : TStockTimeIntervalChartArray;
    FLastTrendIndexes   : array [TStockTimeInterval] of integer;
    FTrader: TStockTraderTrendFollower;
    FContinue           : boolean;
    FStop               : boolean;

    procedure OnTick(const aDateTime: TDateTime; const aIndexes: TStockTimeIntervalIntArray);
  public
    constructor Create(aTrader: TStockTraderBase); override;
    destructor Destroy; override;
  end;


implementation
  uses Math, BaseUtils, DateUtils, SystemService,StockChart.Obj;

{$R *.dfm}

procedure TfmTrendFollowerTestBenchDialog.CalcRecordCount;
var
  aBk: TBookmark;
  i: integer;
begin
  aBk:=taTrendFollow2.Bookmark;
  taTrendFollow2.DisableControls;
  try
    taTrendFollow2.First; i:=0;
    while not taTrendFollow2.Eof do
    begin
      inc(i);
      taTrendFollow2.Next;
    end;

  finally
    try
      taTrendFollow2.Bookmark:=aBk;
    except
    end;
    taTrendFollow2.EnableControls;
  end;

  laRecordCount.Caption:=Format('Count=%d',[i]);
end;

constructor TfmTrendFollowerTestBenchDialog.Create(aTrader: TStockTraderBase);
var
  i:TStockTimeInterval;
  j: integer;
begin
  inherited;
  FTrader:=aTrader as TStockTraderTrendFollower;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
  begin
    lbTimeIntervals.Items.Add(StockTimeIntervalNames[i]);
    if not (i in [sti1,sti30]) then
      lbTimeIntervals.Checked[integer(i)]:=true;

    FInputDatas[i]:=FTrader.GetProject.GetStockChart(i).GetInputData;
    FCharts[i]:=FTrader.GetProject.GetStockChart(i);
  end;

  for j:=0 to 23 do
    lbHours.Items.AddObject(IntToStrEx(j,2),TObject(j));
  lbHours.Checked[15]:=true;
end;

destructor TfmTrendFollowerTestBenchDialog.Destroy;
begin
  lbTimeIntervals.Items.Clear;
  inherited;
end;

procedure TfmTrendFollowerTestBenchDialog.edFilterKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#13 then
  begin
    taTrendFollow2.Filter:=edFilter.Text;
    taTrendFollow2.Filtered:=true;
    CalcRecordCount;
  end;
end;

procedure TfmTrendFollowerTestBenchDialog.EditDBGrid1BeforeDrawColumnCell(Sender: TObject; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  if (Column.Field.Tag<>0) then
  begin
    if Column.Field.IsNull then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWindow
    else if Column.Field.AsFloat<0 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end
end;

procedure TfmTrendFollowerTestBenchDialog.EditDBGrid1DblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taTrendFollow2Time.Value;
  aCloseTime:=aOpenTime+60/MinsPerDay;

  aShiftState:=KeyboardStateToShiftState;

  FTrader.GetProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

function TfmTrendFollowerTestBenchDialog.GetIndicatorFromChart(aChart: IStockChart; const aIID: TGUID): ISCIndicator;
var
  aCollection: ISCIndicatorCollectionReadOnly;
  s: string;
begin
  aCollection:=aChart.FindIndicators(aIID);
  if aCollection.Count=0 then
  begin
    s:=Format('Indicator "%s" was not found on the chart "%s". Add the indicator before start.',
      [IndicatorFactory.GetIndicatorInfo(aIID).Name,aChart.StockSymbol.GetDisplayText]);
    raise EStockError.Create(s);
  end;

  result:=aCollection[0];
end;

procedure TfmTrendFollowerTestBenchDialog.grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  if Column.Field=taTrendFollowTrendDir then
  begin
    if taTrendFollowTrendDir.IsNull then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWindow
    else if taTrendFollowTrendDir.Value<0 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end
end;

procedure TfmTrendFollowerTestBenchDialog.grOrdersDblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taTrendFollowTime.Value;
  aCloseTime:=aOpenTime+60/MinsPerDay;

  aShiftState:=KeyboardStateToShiftState;

  FTrader.GetProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfmTrendFollowerTestBenchDialog.buResumeClick(Sender: TObject);
begin
  inherited;
  FContinue:=true;
end;

procedure TfmTrendFollowerTestBenchDialog.buStart2Click(Sender: TObject);
const
  ChartCount = 6;
var
  aCharts: array [0..ChartCount-1] of IStockChart;
  aPbSARs: array [0..ChartCount-1] of ISCIndicatorParabolicSAR;
//  aAccs: array [0..ChartCount-1] of ISCIndicatorAccelerator;
  //aFrs: array [0..ChartCount-1] of ISCIndicatorFractals;
  aPbSARFields: array [0..ChartCount-1] of TField;
//  aAccFields: array [0..ChartCount-1] of TField;
  //aFrFields: array [0..ChartCount-1] of TField;
  aStartTime: TDateTime;
  aStopTime: TDateTime;
  i,j: Integer;
  aZigZag1H,aZigZag4H: ISCIndicatorZigZag;
  aGuess: TStockRealNumber;
  aMatched,aMismatched:integer;
begin
  inherited;
  aCharts[0]:=FTrader.GetProject.FindStockChart('EURUSD',sti60);
  aCharts[1]:=FTrader.GetProject.FindStockChart('GBPUSD',sti60);
  aCharts[2]:=FTrader.GetProject.FindStockChart('USDCHF',sti60);
  aCharts[3]:=FTrader.GetProject.FindStockChart('EURUSD',sti240); //EURUSD 4 h
  aCharts[4]:=FTrader.GetProject.FindStockChart('GBPUSD',sti240);
  aCharts[5]:=FTrader.GetProject.FindStockChart('USDCHF',sti240);

  for i := 0 to ChartCount - 1 do
      aPbSARs[i]:=GetIndicatorFromChart(aCharts[i],ISCIndicatorParabolicSAR) as ISCIndicatorParabolicSAR;

//  for i := 0 to ChartCount - 1 do
//      aAccs[i]:=GetIndicatorFromChart(aCharts[i],ISCIndicatorAccelerator) as ISCIndicatorAccelerator;

//  for i := 0 to ChartCount - 1 do
//      aFrs[i]:=GetIndicatorFromChart(aCharts[i],ISCIndicatorFractals) as ISCIndicatorFractals;

  aZigZag1H:=GetIndicatorFromChart(aCharts[0], ISCIndicatorZigZag) as ISCIndicatorZigZag;
  aZigZag4H:=GetIndicatorFromChart(aCharts[3], ISCIndicatorZigZag) as ISCIndicatorZigZag;


  aPbSARFields[0]:= taTrendFollow2EURUSD_H1_PbSAR;
  aPbSARFields[1]:= taTrendFollow2GBPUSD_H1_PbSAR;
  aPbSARFields[2]:= taTrendFollow2USDCHF_H1_PbSAR;
  aPbSARFields[3]:= taTrendFollow2EURUSD_H4_PbSAR;
  aPbSARFields[4]:= taTrendFollow2GBPUSD_H4_PbSAR;
  aPbSARFields[5]:= taTrendFollow2USDCHF_H4_PbSAR;

//  aAccFields[0]:= taTrendFollow2EURUSD_H1_Acc;
//  aAccFields[1]:= taTrendFollow2GBPUSD_H1_Acc;
//  aAccFields[2]:= taTrendFollow2USDCHF_H1_Acc;
//  aAccFields[3]:= taTrendFollow2EURUSD_H4_Acc;
//  aAccFields[4]:= taTrendFollow2GBPUSD_H4_Acc;
//  aAccFields[5]:= taTrendFollow2USDCHF_H4_Acc;

//  aFrFields[0]:= taTrendFollow2EURUSD_H1_Fr;
//  aFrFields[1]:= taTrendFollow2GBPUSD_H1_Fr;
//  aFrFields[2]:= taTrendFollow2USDCHF_H1_Fr;
//  aFrFields[3]:= taTrendFollow2EURUSD_H4_Fr;
//  aFrFields[4]:= taTrendFollow2GBPUSD_H4_Fr;
//  aFrFields[5]:= taTrendFollow2USDCHF_H4_Fr;

  aStartTime:=-1;
  aStopTime:=high(integer);

  for i := 0 to ChartCount - 1 do
    aStartTime:=max(aStartTime,aCharts[i].GetInputData.DirectGetItem_DataDateTime(0));

  for i := 0 to ChartCount - 1 do
    aStopTime:=min(aStopTime,aCharts[i].GetInputData.DirectGetItem_DataDateTime(aCharts[i].GetInputData.Count-1));


  TWaitCursor.SetUntilIdle;
  taTrendFollow2.DisableControls;

  aMatched:=0;
  aMismatched:=0;

  try
    taTrendFollow2.Open;
    taTrendFollow2.EmptyTable;

    while aStartTime<=aStopTime do
    begin
      aStartTime:=TStockDataUtils.RefineTime(aStartTime);

      if not (DayOfTheWeek(aStartTime) in [6,7]) then
        if lbHours.Checked[HourOfTheDay(aStartTime)] then
        begin
          taTrendFollow2.Append;
          taTrendFollow2No.Value:=taTrendFollow2.RecordCount+1;
          taTrendFollow2Time.Value:=aStartTime;

          //Цена закрытия бара, EURUSD 1h
          j:=aCharts[0].GetInputData.FindExactMatched(aStartTime);
          if j<>-1 then
            taTrendFollow2Price.Value:=aCharts[0].GetInputData.DirectGetItem_DataClose(j);

          for i := 0 to ChartCount - 1 do
          begin
            j:=aCharts[i].GetInputData.FindExactMatched(aStartTime);
            if j<>-1 then
            begin
              //Parabolic SAR
              if j>=aPbSARs[i].GetFirstValidValueIndex then
                aPbSARFields[i].AsInteger:=
                  aCharts[i].GetInputData.PriceToPoint(sqrt(Abs(aPbSARs[i].GetDivergence(j)*aPbSARs[i].GetTrendForce(j))))*
                  aPbSARFields[i].Tag*
                Sign(aPbSARs[i].GetDivergence(j));

              //Fractals
{              if (j-2>=2) and (aFrs[i].GetPrecedentExtremumIndex(j-2,k)) then
              begin
                if (aFrs[i].GetValue(k))=1 then
                  aFrFields[i].AsInteger:=1*aFrFields[i].Tag
                else if (aFrs[i].GetValue(k))=2 then
                  aFrFields[i].AsInteger:=-1*aFrFields[i].Tag
              end;
}
              //Accelerator/Deccelerator
{              if j>aAccs[i].GetFirstValidValueIndex then
                aAccFields[i].AsInteger:=PriceToPoint(
                  (aAccs[i].GetValue(j)-aAccs[i].GetValue(j-1))*aAccFields[i].Tag);
}
            end;
          end;

          //Guess
          try
{            j:=0; aGuess:=0;
            for i := 0 to ChartCount - 1 do
            begin
              if aPbSARFields[i].IsNull then
                abort;
              aGuess:=aGuess+aPbSARFields[i].AsInteger;
              inc(j);
            end;
}
            //Пока упрощаем, считаем только по EURUSD
            //j:=2;
            //aGuess:=Sign(aPbSARFields[0].AsInteger)+Sign(aPbSARFields[3].AsInteger);
            aGuess:=aPbSARFields[0].AsInteger+aPbSARFields[3].AsInteger;

{            for i := 0 to ChartCount - 1 do
            begin
              if aAccFields[i].IsNull then
                abort;
              aGuess:=aGuess+aAccFields[i].AsInteger;
              inc(j);
            end;
}
            if aGuess<>0 then
              taTrendFollow2Guess.AsFloat:=aGuess;
          except
            on E:EAbort do ;
          end;


          //ZigZag 4H
          j:=aZigZag4H.GetInputData.FindExactMatched(aStartTime);
          if (j<>-1) and (aZigZag4H.GetSuccedentExtremumIndex(j+1,i)) then
          begin
            taTrendFollow2ZigZag4HTime.Value:=aZigZag4H.GetInputData.DirectGetItem_DataDateTime(i);
                        //Min
            if aZigZag4H.GetValue(i)=-1 then
              taTrendFollow2ZigZag4H.AsInteger:=aZigZag4H.GetInputData.PriceToPoint(aZigZag4H.GetInputData.DirectGetItem_DataLow(i)-taTrendFollow2Price.AsFloat)
            //Max
            else if aZigZag4H.GetValue(i)=1 then
              taTrendFollow2ZigZag4H.AsInteger:=aZigZag4H.GetInputData.PriceToPoint(aZigZag4H.GetInputData.DirectGetItem_DataHigh(i)-taTrendFollow2Price.AsFloat);
          end;

          //ZigZag 1H
          j:=aZigZag1H.GetInputData.FindExactMatched(aStartTime);
          if (j<>-1) and (aZigZag1H.GetSuccedentExtremumIndex(j+1,i)) then
          begin
            //Цена закрытия бара
            taTrendFollow2Price.Value:=aZigZag1H.GetInputData.DirectGetItem_DataClose(j);

            taTrendFollow2ZigZag1HTime.Value:=aZigZag1H.GetInputData.DirectGetItem_DataDateTime(i);

            //Min
            if aZigZag1H.GetValue(i)=-1 then
              taTrendFollow2ZigZag1H.AsInteger:=aZigZag1H.GetInputData.PriceToPoint(aZigZag1H.GetInputData.DirectGetItem_DataLow(i)-taTrendFollow2Price.AsFloat)
            //Max
            else if aZigZag1H.GetValue(i)=1 then
              taTrendFollow2ZigZag1H.AsInteger:=aZigZag1H.GetInputData.PriceToPoint(aZigZag1H.GetInputData.DirectGetItem_DataHigh(i)-taTrendFollow2Price.AsFloat);
          end;

          //At PbSARReverse (EURUSD 4h)
          try
            j:=aCharts[3].GetInputData.FindExactMatched(aStartTime);
            if j<>-1 then
            begin
              i:=j;
              while Sign(aPbSARFields[3].AsInteger)=Sign(aPbSARs[3].GetDivergence(i)*aPbSARFields[3].Tag) do
              begin
                inc(i);
                if i>=aCharts[3].GetInputData.Count then
                  abort;
              end;
              taTrendFollow2AtPbSARReverse.AsInteger:=aCharts[3].GetInputData.PriceToPoint(aCharts[3].GetInputData.DirectGetItem_DataClose(i)-taTrendFollow2Price.AsFloat);
            end;
          except
            on E:EAbort do;
          end;


          if (not taTrendFollow2Guess.IsNull) then
          begin
            if Sign(taTrendFollow2Guess.Value)=Sign(taTrendFollow2ZigZag4H.Value) then
              inc(aMatched)
            else
              inc(aMismatched);
          end;


          taTrendFollow2.Post;
        end;
      aStartTime:=IncHour(aStartTime);
    end;
  finally
    taTrendFollow2.First;
    taTrendFollow2.EnableControls;
  end;

  EditDBGrid1.ColumnSizes.FitColumnsByContents();
  laStatisticsMatches.Caption:=Format('Match/Mism.=%d/%d',[aMatched,aMismatched]);
  CalcRecordCount;
end;

procedure TfmTrendFollowerTestBenchDialog.buStartClick(Sender: TObject);
var
  aNewInputDatas: array [TStockTimeInterval]  of TSCInputDataCollection;
  aInterval: TStockTimeInterval;
  aGenerators: array [TStockTimeInterval] of TStockBarBackwardGenerator;
  i,k: Integer;
  aIData: ISCInputDataCollection;
  aGenerator : TStockBarBackwardGenerator;
  aPriorDate: TDateTime;
  aBarData   : TStockBarData;
  aIndexes: TStockTimeIntervalIntArray;
begin
  inherited;
  TWaitCursor.SetUntilIdle;

  taTrendFollow.OPen;
  taTrendFollow.EmptyTable;

  FStop:=false;
  buStart.Enabled:=false;
  buResume.Enabled:=true;
  buStop.Enabled:=true;
  try
    //Подменяем InputData
    for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      FLastTrendIndexes[aInterval]:=-1;    
      Assert(FCharts[aInterval].GetInputData=FInputDatas[aInterval]);
      aNewInputDatas[aInterval]:=TSCInputDataCollection.Create(nil,StockTimeIntervalValues[aInterval]/MinsPerDay);
      FCharts[aInterval].SetInputData(aNewInputDatas[aInterval]);

      if aInterval<>sti1 then
        aGenerators[aInterval]:=TStockBarBackwardGenerator.Create(FInputDatas[sti1],aInterval)
      else
        aGenerators[aInterval]:=nil;
    end;

    for i := 0 to FInputDatas[sti1].Count-1 do
    begin
      //Минутку добавляем
      aNewInputDatas[sti1].AddItem(
         FInputDatas[sti1].DirectGetItem_DataDateTime(i),
         FInputDatas[sti1].DirectGetItem_DataOpen(i),
         FInputDatas[sti1].DirectGetItem_DataHigh(i),
         FInputDatas[sti1].DirectGetItem_DataLow(i),
         FInputDatas[sti1].DirectGetItem_DataClose(i),
         FInputDatas[sti1].DirectGetItem_DataVolume(i));

      aIndexes[sti1]:=aNewInputDatas[sti1].Count-1;

      //Эмулируем тики на разных таймфреймах
      for aInterval:=Succ(sti1) to high(TStockTimeInterval) do
      begin
        aIData:=FInputDatas[aInterval];
        aGenerator:=aGenerators[aInterval];
        aGenerator.GenerateBar(i);

        aBarData:=aGenerator.BarData;
        aPriorDate:=0;
        k:=aNewInputDatas[aInterval].Count;

        if (k>0) then
          aPriorDate:=aNewInputDatas[aInterval].DirectGetItem_DataDateTime(k-1);

        //Модифицируем старый бар
        if aPriorDate=aGenerator.DestLeftBound then
        begin
          Assert(k>0);
          aNewInputDatas[aInterval].ModifyItem(k-1,
                  aBarData.Open,aBarData.High,aBarData.Low,aBarData.Close,aBarData.Volume)
        end
        //Новый бар - добавляем
        else begin
          Assert(aPriorDate<aGenerator.DestLeftBound);
          aNewInputDatas[aInterval].AddItem(
                  aIData.DirectGetItem_DataDateTime(k),
                  aBarData.Open,aBarData.High,aBarData.Low,aBarData.Close,aBarData.Volume)

        end;

        aIndexes[aInterval]:=aNewInputDatas[aInterval].Count-1;
      end;

      OnTick(FInputDatas[sti1].DirectGetItem_DataDateTime(i),aIndexes);
      if FStop then
        break;
    end;
  finally
    for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      //Возвращаем InputData
      FCharts[aInterval].SetInputData(FInputDatas[aInterval]);
      aGenerators[aInterval].Free;
    end;

    buStart.Enabled:=true;
    buResume.Enabled:=false;
    buStop.Enabled:=false;
  end;
end;

procedure TfmTrendFollowerTestBenchDialog.buStopClick(Sender: TObject);
begin
  inherited;
  FStop:=true;
end;

procedure TfmTrendFollowerTestBenchDialog.OnTick(const aDateTime: TDateTime;const aIndexes: TStockTimeIntervalIntArray);
var
  aHigherTrend: integer;
  aMyTrend: integer;
  bFound : boolean;
  i:TStockTimeInterval;
  index: integer;
  aPbSARs: array [TStockTimeInterval] of ISCExpertParabolicSAR;
begin
  aHigherTrend:=0;
  bFound:=true;

  //Вне этого диапазона волатильность низкая
  if (Frac(aDateTime)<EncodeTime(8,0,0,0)) or (Frac(aDateTime)>EncodeTime(20,30,0,0)) then
    exit;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
  begin
    if not lbTimeIntervals.Checked[integer(i)] then
      aPbSARs[i]:=nil
    else
      raise Exception.Create('Не доделано'); 
      //aPbSARs[i]:=FTrader.GetExpertPbSAR(i);
  end;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
  begin
    if (aPbSARs[i]=nil) then
      continue;

    index:=aIndexes[i];
    if (index<max(1,aPbSARs[i].GetFirstValidValueIndex)) then
    begin
      bFound:=false;
      break;
    end;

    aMyTrend:=Sign(aPbSARs[i].GetTrend(index));
    if aHigherTrend=0 then //Если это первый (самый младший) чарт
    begin
      if FLastTrendIndexes[i]=index then
        aMyTrend:=-2 //Если это все тот-же тренд
      else if (Sign(aMyTrend)=Sign(aPbSARs[i].GetTrend(index-1))) and (index=FLastTrendIndexes[i]+1) then
      begin
        aMyTrend:=-2; //Если это все тот-же тренд
        FLastTrendIndexes[i]:=index;
      end
      else
        aHigherTrend:=aMyTrend;
    end;

    if aHigherTrend<>aMyTrend then
    begin
      bFound:=false;
      break;
    end;
  end;

  if bFound then
  begin
    for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    begin
      FLastTrendIndexes[i]:=aIndexes[i];
    end;


    taTrendFollow.Append;
    taTrendFollowNo.Value:=taTrendFollow.RecordCount+1;
    taTrendFollowTime.Value:=aDateTime;
    taTrendFollowTrendDir.Value:=aHigherTrend;
    taTrendFollowClose.Value:=FCharts[sti1].GetInputData.DirectGetItem_DataClose(aIndexes[sti1]);
    taTrendFollow.Post;
    Application.ProcessMessages;

    if ckStopAfterEachRecord.Checked then
    begin
      FContinue:=false;
      FStop:=false;
      while not FContinue and not FStop do
        Application.ProcessMessages;
    end
  end;
end;

end.
