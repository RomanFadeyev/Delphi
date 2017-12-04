unit FC.Trade.StatisticsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ComCtrls, ExtendControls, ExtCtrls,FC.fmUIDataStorage,
  ActnList, ToolWin,FC.Definitions;

type
  TfmTradingStatisticDialog = class(TfmDialogClose_B)
    lvProperties: TExtendListView;
    Properties: TLabel;
    ToolBar1: TToolBar;
    tbExport: TToolButton;
    alActions: TActionList;
    acExport: TAction;
    procedure acExportExecute(Sender: TObject);
  private
    FTrader: IStockTrader;
  public
    property Trader: IStockTrader read FTrader write FTrader;
    procedure AddProperty(const aName: string; const aValue: string);
  end;


implementation
  uses Export.Definitions;

{$R *.dfm}

{ TfmTradingStatisticDialog }

procedure TfmTradingStatisticDialog.AddProperty(const aName, aValue: string);
begin
  with lvProperties.Items.Add do
  begin
    Caption:=aName;
    SubItems.Add(aValue);
  end;
end;

procedure TfmTradingStatisticDialog.acExportExecute(Sender: TObject);
var
  aExportInfo: TExportInfo;
  aPropCollection: IStockTraderPropertyCollection;
  aExportString  : TExportString;
  i: integer;
begin
  aExportInfo:=TExportInfo.Create;

  aPropCollection:=Trader.GetProperties;
  try
    //Default format
    aExportInfo.DefaultFormat.FontName:='Tahoma';
    aExportInfo.DefaultFormat.FontSize:=7;
    aExportInfo.DefaultFormat.IgnoreFormat:=false;

    //Title
    aExportString:=TExportString.Create(Trader.GetCategory+'\'+Trader.GetName+#13#10);
    aExportString.Format.IgnoreFormat:=false;
    aExportString.Format.FontSize:=12;
    aExportString.Format.FontStyle:=[efsBold,efsItalic];
    aExportInfo.HeadStrings.Add(aExportString);

    //Properties
    for i:=0 to aPropCollection.Count-1 do
    begin
      aExportString:=TExportString.Create(aPropCollection.Items[i].GetCategory+'\'+
                                         aPropCollection.Items[i].GetName+'='+
                                         aPropCollection.Items[i].ValueAsText);
      aExportString.Format.IgnoreFormat:=false;
      aExportString.Format.FontSize:=10;
      aExportInfo.HeadStrings.Add(aExportString);
    end;

    ExporterManager.ExportOperator.DoExport(lvProperties,aExportInfo);
  finally
    aExportInfo.Free;
  end;
end;

end.
