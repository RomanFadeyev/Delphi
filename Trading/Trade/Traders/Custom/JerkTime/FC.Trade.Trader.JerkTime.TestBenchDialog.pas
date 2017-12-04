unit FC.Trade.Trader.JerkTime.TestBenchDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ComCtrls,
  FC.Trade.Trader.JerkTime,FC.DataUtils, FC.Dialogs.DockedDialogCloseAndAppWindow_B, ImgList, JvComponentBase,
  JvCaptionButton, CheckLst,
  FC.Definitions, StockChart.Definitions.Units, StockChart.Definitions, DB, MemoryDS, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, JvDockControlForm, FC.Trade.Trader.TestBenchDialog_B,
  FC.Trade.Trader.Base;

type
  TfmJerkTimeTestBenchDialog = class(TfmTestBenchDialog_B)
    paWorkspace: TPanel;
    PageControlEx1: TPageControlEx;
    tsTrendFollow: TTabSheet;
    grOrders: TEditDBGrid;
    taTrendFollow: TMemoryDataSet;
    dsTrendFollow: TDataSource;
    taTrendFollowTime: TDateTimeField;
    taTrendFollowPbSAR5m: TFloatField;
    taTrendFollowPbSAR15m: TFloatField;
    taTrendFollowPbSAR60m: TFloatField;
    taTrendFollowTrendDir: TFloatField;
    taTrendFollowFractals5m: TIntegerField;
    taTrendFollowFractals15m: TIntegerField;
    taTrendFollowFractals60m: TIntegerField;
    Panel1: TPanel;
    buStart: TButton;
    ckStopAfterEachRecord: TExtendCheckBox;
    buResume: TButton;
    buStop: TButton;
    procedure buStopClick(Sender: TObject);
    procedure buResumeClick(Sender: TObject);
    procedure grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure grOrdersDblClick(Sender: TObject);
    procedure buStartClick(Sender: TObject);
  private
    FTrader: TStockTraderJerkTime;
    FInputDatas         : TStockTimeIntervalInputDataCollectionArray;
    FCharts             : TStockTimeIntervalChartArray;
    FContinue           : boolean;
    FStop               : boolean;


    procedure OnTick(const aDateTime: TDateTime; const aIndexes: TStockTimeIntervalIntArray);
  public
    constructor Create(aTrader: TStockTraderBase);   override;
  end;


implementation
  uses Math, SystemService,StockChart.Obj;

{$R *.dfm}


constructor TfmJerkTimeTestBenchDialog.Create(aTrader: TStockTraderBase);
var
  aInterval: TStockTimeInterval;
begin
  inherited Create(aTrader);
  FTrader:=aTrader as TStockTraderJerkTime;

  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    FInputDatas[aInterval]:=FTrader.GetProject.GetStockChart(aInterval).GetInputData;
    FCharts[aInterval]:=FTrader.GetProject.GetStockChart(aInterval);
  end;
end;

procedure TfmJerkTimeTestBenchDialog.grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
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
  else if (Column.Field = taTrendFollowFractals5m)  or
          (Column.Field = taTrendFollowFractals15m) or
          (Column.Field = taTrendFollowFractals60m) then
  begin
    if Column.Field.AsInteger=2 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else if Column.Field.AsInteger=1 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end
  else if (Column.Field = taTrendFollowPbSAR5m)  or
          (Column.Field = taTrendFollowPbSAR15m) or
          (Column.Field = taTrendFollowPbSAR60m) then
  begin
    if Column.Field.AsFloat<0 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else 
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end;
end;

procedure TfmJerkTimeTestBenchDialog.grOrdersDblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taTrendFollowTime.Value;
  aCloseTime:=aOpenTime+60/MinsPerDay;

  aShiftState:=KeyboardStateToShiftState;
  Trader.GetProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfmJerkTimeTestBenchDialog.OnTick(const aDateTime: TDateTime; const aIndexes: TStockTimeIntervalIntArray);
var
  aFractalM5,aFractalM15,aFractalM60: integer;
  aMin,aMax: TSCRealNumber;
begin
  if FTrader.IsJerkTime(aDateTime) then
  begin
    if (aIndexes[sti1]<2) or (aIndexes[sti5]<2) or (aIndexes[sti15]<2) or (aIndexes[sti60]<2)   then
    begin
      taTrendFollow.Append;
      taTrendFollowTime.Value:=aDateTime;
      taTrendFollow.Post;
    end
    else begin
      aFractalM5:=0;
      aFractalM15:=0;
      aFractalM60:=0;

      taTrendFollow.Append;
      taTrendFollowTime.Value:=aDateTime;
      taTrendFollowPbSAR5m.Value:=FTrader.GetBroker.PriceToPoint(FTrader.GetSymbol, FTrader.PbSARM5.GetDivergence(aIndexes[sti5]));
      taTrendFollowPbSAR15m.Value:=FTrader.GetBroker.PriceToPoint(FTrader.GetSymbol, FTrader.PbSARM15.GetDivergence(aIndexes[sti15]));
      taTrendFollowPbSAR60m.Value:=FTrader.GetBroker.PriceToPoint(FTrader.GetSymbol, FTrader.PbSARM60.GetDivergence(aIndexes[sti60]));
      taTrendFollowFractals5m.Value:=aFractalM5;
      taTrendFollowFractals15m.Value:=aFractalM15;
      taTrendFollowFractals60m.Value:=aFractalM60;

      if (aIndexes[sti1]>=0) and (aIndexes[sti1]<FInputDatas[sti1].Count-60) then
      begin
        FInputDatas[sti1].FindMinMaxValues(aIndexes[sti1],aIndexes[sti1]+60,aMin,aMax);

        if Abs(aMin-FInputDatas[sti1].DirectGetItem_DataClose(aIndexes[sti1]))>
           Abs(aMax-FInputDatas[sti1].DirectGetItem_DataClose(aIndexes[sti1])) then
          taTrendFollowTrendDir.Value:=FTrader.GetBroker.PriceToPoint(FTrader.GetSymbol, aMin-FInputDatas[sti1].DirectGetItem_DataClose(aIndexes[sti1]))
        else
          taTrendFollowTrendDir.Value:=FTrader.GetBroker.PriceToPoint(FTrader.GetSymbol, aMax-FInputDatas[sti1].DirectGetItem_DataClose(aIndexes[sti1]));
      end;

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

    FContinue:=false;
  end;
end;

procedure TfmJerkTimeTestBenchDialog.buResumeClick(Sender: TObject);
begin
  inherited;
  FContinue:=true;
end;

procedure TfmJerkTimeTestBenchDialog.buStartClick(Sender: TObject);
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

procedure TfmJerkTimeTestBenchDialog.buStopClick(Sender: TObject);
begin
  inherited;
  FStop:=true;
end;

end.
