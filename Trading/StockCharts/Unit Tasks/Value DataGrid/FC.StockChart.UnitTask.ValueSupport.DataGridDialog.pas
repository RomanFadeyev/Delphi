unit FC.StockChart.UnitTask.ValueSupport.DataGridDialog;
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls, FC.StockChart.CustomDialog_B, ImgList, JvCaptionButton,
  JvComponentBase, JvDockControlForm, FC.Common.PeriodFrame, ToolWin,FC.fmUIDataStorage,
  Menus;

type
  TfmValueStatisticsDialog = class(TfmStockChartCustomDialog_B)
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    grReport: TEditDBGrid;
    Label2: TLabel;
    pbProgress: TProgressBar;
    taReportBarNo: TIntegerField;
    taReportDateTime: TDateTimeField;
    taReportValue: TFloatField;
    paTop: TPanel;
    frmPeriod: TfrmPeriod;
    ckFilter: TExtendCheckBox;
    buApplyFilter: TExtendButton;
    laStat: TLabel;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    acReload: TAction;
    acExportAsInserts: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    pmExport: TPopupMenu;
    ExportasSQLInserts1: TMenuItem;
    acCopyAsText: TAction;
    Copytoclipboardascommaseparatedtext1: TMenuItem;
    acExportToDB: TAction;
    N1: TMenuItem;
    ExporttoDB1: TMenuItem;
    procedure buOKClick(Sender: TObject);
    procedure grReportDblClick(Sender: TObject);
    procedure buCalculateClick(Sender: TObject);
    procedure ckFilterClick(Sender: TObject);
    procedure buApplyFilterClick(Sender: TObject);
    procedure acExportAsInsertsExecute(Sender: TObject);
    procedure acCopyAsTextExecute(Sender: TObject);
    procedure acExportToDBExecute(Sender: TObject);
  private
    FIndicator: ISCIndicatorValueSupport;

    procedure Calculate;
    procedure ApplyFilter;
  public
    class procedure Run(const aIndicator: ISCIndicatorValueSupport; const aStockChart: IStockChart);

    constructor Create(const aStockChart: IStockChart); override;
    destructor Destroy; override;
  end;


implementation
  uses Math,Clipbrd,FC.StockData.StockTempValueCollection,FC.Singletons;

type
  TDirection = (dNone,dUp,dDown);

{$R *.dfm}

{ TfmCalculateWidthDialog }

type
 TOrderState = (osNone,osBuy,osSell);

procedure TfmValueStatisticsDialog.buOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmValueStatisticsDialog.Calculate;
var
  i,j,k: Integer;
  aInputData: ISCInputDataCollection;
  aCurProgress: integer;
  aValue : TStockRealNumber;
begin
  TWaitCursor.SetUntilIdle;
  aInputData:=(FIndicator as ISCIndicator).GetInputData;

  taReport.DisableControls;
  try
    taReport.EmptyTable;
    taReport.Open;

    pbProgress.Max:=100;
    pbProgress.Position:=0;
    pbProgress.Visible:=true;
    aCurProgress:=0;


    for i:=(FIndicator as ISCIndicator).GetFirstValidValueIndex to aInputData.Count-1 do
    begin
      aValue:=FIndicator.GetValue(i);
      k:=taReport.DirectInsertRecord;
      taReport.DirectSetFieldData(k,taReportBarNo,i);
      taReport.DirectSetFieldData(k,taReportDateTime,aInputData.DirectGetItem_DataDateTime(i));
      taReport.DirectSetFieldData(k,taReportValue,aValue);


      j:=(i div aInputData.Count)*100;
      if j<>aCurProgress then
      begin
        pbProgress.Position:=j;
        aCurProgress:=j;
        Application.ProcessMessages;
      end;
    end;

    i:=(FIndicator as ISCIndicator).GetFirstValidValueIndex;
    if i<=aInputData.Count-1 then
    begin
      frmPeriod.StartFrom:=aInputData.DirectGetItem_DataDateTime(i);
      frmPeriod.StopAt:=aInputData.DirectGetItem_DataDateTime(aInputData.Count-1);
    end;

    ApplyFilter;
  finally
    taReport.Refresh;

    grReport.RefreshSort;
    pbProgress.Visible:=false;
    taReport.EnableControls;
  end;
end;

procedure TfmValueStatisticsDialog.ckFilterClick(Sender: TObject);
begin
  inherited;
  frmPeriod.Enabled:=ckFilter.Checked;
end;

procedure TfmValueStatisticsDialog.acCopyAsTextExecute(Sender: TObject);
var
  aStrings:TStringList;
begin
  inherited;
  TWaitCursor.SetUntilIdle;
  aStrings:=TStringList.Create;
  try

    taReport.DisableControls;
    try
      taReport.First;
      while not taReport.EOF do
      begin
        aStrings.Add(Format('%d, %s, %g',[taReportBarNo.Value,DateTimeToStr(taReportDateTime.Value),taReportValue.Value]));
        taReport.Next;
      end;
    finally
      taReport.EnableControls;
    end;
    Clipboard.Open;
    Clipboard.AsText:=aStrings.Text;
    Clipboard.Close;
  finally
    aStrings.Free;
  end;
end;

procedure TfmValueStatisticsDialog.acExportAsInsertsExecute(Sender: TObject);
var
  aStrings:TStringList;
begin
  inherited;
  TWaitCursor.SetUntilIdle;
  aStrings:=TStringList.Create;
  try
    taReport.SaveToSqlInserts(aStrings,0,'TMP$VALUE');
    Clipboard.Open;
    Clipboard.AsText:=aStrings.Text;
    Clipboard.Close;
  finally
    aStrings.Free;
  end;
end;

procedure TfmValueStatisticsDialog.acExportToDBExecute(Sender: TObject);
var
  aCollection:TStockTempValueCollection;
  aClearPrevData: boolean;
begin
  inherited;

  aClearPrevData:=MsgBox.Confirm(Handle,'Empty database table before exporting?',[]);

  TWaitCursor.SetUntilIdle;
  aCollection:=TStockTempValueCollection.Create;
  IInterface(aCollection)._AddRef;
  try
    taReport.DisableControls;
    try
      taReport.First;
      while not taReport.EOF do
      begin
        aCollection.Add(taReportDateTime.Value,taReportValue.Value,taReportBarNo.Value,StockChart.StockSymbol.Name);
        taReport.Next;
      end;
    finally
      taReport.EnableControls;
    end;
    StockDataStorage.ImportTempValues(aCollection,aClearPrevData);
  finally
    IInterface(aCollection)._Release;
  end;
end;

procedure TfmValueStatisticsDialog.ApplyFilter;
var
  aAVG:TStockRealNumber;
  aCount : integer;
begin
  taReport.DisableControls;

  try
    taReport.Filter:=Format('(DATETIME >= %g) and (DATETIME<=%g)',[frmPeriod.StartFrom,frmPeriod.StopAt]);
    if taReport.Filtered<>ckFilter.Checked then
      taReport.Filtered:=ckFilter.Checked;

    taReport.First;
    aAVG:=0;
    aCount:=0;
    while not taReport.Eof do
    begin
      aAVG:=aAVG+taReportValue.Value;
      inc(aCount);
      taReport.Next;
    end;
    taReport.First;
  finally
    taReport.EnableControls;
  end;

  aAVG:=aAVG / aCount;
  laStat.Caption:=Format('COUNT = %d, AVG = %g',[aCount,aAVG]);
end;

procedure TfmValueStatisticsDialog.buApplyFilterClick(Sender: TObject);
begin
  inherited;
  frmPeriod.AddValuesToMRU;
  TWaitCursor.SetUntilIdle;
  ApplyFilter;
end;

procedure TfmValueStatisticsDialog.buCalculateClick(Sender: TObject);
begin
  inherited;
  Calculate;
end;


constructor TfmValueStatisticsDialog.Create(const aStockChart: IStockChart);
begin
  inherited;
  frmPeriod.LoadSettings;
  ckFilterClick(nil);
end;

destructor TfmValueStatisticsDialog.Destroy;
begin
  inherited;
end;

procedure TfmValueStatisticsDialog.grReportDblClick(Sender: TObject);
begin
  inherited;
  TWaitCursor.SetUntilIdle;
  StockChart.LocateTo(taReportDateTime.Value,lmCenter);
  StockChart.Mark(taReportDateTime.Value);
end;

class procedure TfmValueStatisticsDialog.Run(const aIndicator: ISCIndicatorValueSupport; const aStockChart: IStockChart);
begin
  with TfmValueStatisticsDialog.Create(aStockChart) do
  begin
    FIndicator:=aIndicator;
    Caption:=IndicatorFactory.GetIndicatorInfo((FIndicator as ISCIndicator).GetIID).Name+': '+Caption;
    Forms.Application.ProcessMessages;
    Calculate;
    Show;
  end;
end;

end.
