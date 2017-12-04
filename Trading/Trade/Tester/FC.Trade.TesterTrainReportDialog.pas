unit FC.Trade.TesterTrainReportDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, ComCtrls, StdCtrls, ExtCtrls, ExtendControls,
  Properties.Definitions,
  StockChart.Definitions,
  FC.Trade.TesterDialog;

type
  TfmTesterTrainReportDialog = class(TfmDialogClose_B)
    paWorkSpace: TPanel;
    Label1: TLabel;
    lvReport: TExtendListView;
    buStopTrain: TButton;
    procedure lvReportColumnClick(Sender: TObject; Column: TListColumn);
    procedure buStopTrainClick(Sender: TObject);
  private
    FTesterDialog: TfmTradeTesterDialog;
    FCount: integer;
    FStopped: boolean;
    FRunning: boolean;
    FTrainingProperties: TInterfaceList;
    FLastSortOrderColumn: integer;

    procedure RunTrainingInternal(index: integer);
  public
    class procedure Run(aDialog: TfmTradeTesterDialog; aTrainingProperties : TInterfaceList);
  end;


implementation

uses ufmDialog_B;

{$R *.dfm}

const
  ProfitColumnName = 'Profit';

{ TfmTesterTrainReportDialog }

class procedure TfmTesterTrainReportDialog.Run(aDialog: TfmTradeTesterDialog; aTrainingProperties: TInterfaceList);
var
  i: integer;
begin
  with TfmTesterTrainReportDialog.Create(nil) do
  try
    FTesterDialog:=aDialog;
    FTrainingProperties:=aTrainingProperties;
    FLastSortOrderColumn:=-1;

    for i:=0 to aTrainingProperties.Count-1 do
      with lvReport.Columns.Add do
      begin
        Caption:=(aTrainingProperties[i] as IProperty).GetName;
      end;

    with lvReport.Columns.Add do
      Caption:=ProfitColumnName;

    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmTesterTrainReportDialog.RunTrainingInternal(index: integer);
var
  aProperty: ISCIndicatorPropertyTrainee;
  aItem: TListItem;
  i: integer;
begin
  aProperty:=FTrainingProperties[index] as ISCIndicatorPropertyTrainee;

  buOK.Enabled:=false;
  try
    aProperty.StartTrain;
    repeat
      if index<FTrainingProperties.Count-1 then
        RunTrainingInternal(index+1)
      else begin
        //Report
        Inc(FCount);
        aItem:=lvReport.Items.Add;
        aItem.Caption:=IntToStr(FCount);

        for i:=0 to FTrainingProperties.Count-1 do
          aItem.SubItems.Add((FTrainingProperties[i] as ISCIndicatorProperty).ValueAsText);
        Forms.Application.ProcessMessages;

        //Run
        FTesterDialog.RunTesting;
        Forms.Application.ProcessMessages;

        aItem.SubItems.Add(CurrToStr(FTesterDialog.frmStockTradeResult.Statictics.Balance));
      end;

      if FStopped then
        break;

    until not aProperty.DoTrainStep;
  finally
    buOK.Enabled:=true;
  end;
end;

procedure TfmTesterTrainReportDialog.buStopTrainClick(Sender: TObject);
begin
  inherited;
  if FRunning then
  begin
    FStopped:=true
  end
  else begin
    buStopTrain.Caption:='Stop Train';
    FRunning:=true;
    Forms.Application.ProcessMessages;
    RunTrainingInternal(0);
    buStopTrain.Caption:='Start Train';
    FRunning:=false;
    FStopped:=false;
  end;
end;

procedure TfmTesterTrainReportDialog.lvReportColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Caption<>ProfitColumnName then
    exit;
    
  if FLastSortOrderColumn = Column.Index then
  begin
    FLastSortOrderColumn := -1;
    lvReport.SortColumn(Column,smCurrency,true);
  end
  else
  begin
    FLastSortOrderColumn := Column.Index;
    lvReport.SortColumn(Column,smCurrency,false);
  end;
end;

end.
