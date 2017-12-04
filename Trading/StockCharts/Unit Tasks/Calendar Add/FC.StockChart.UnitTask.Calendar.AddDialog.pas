unit FC.StockChart.UnitTask.Calendar.AddDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, OleServer, WordXP, ComCtrls, ToolWin;

type
  TfmDialogOkCancel_B1 = class(TfmDialogOkCancel_B)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    WordDocument: TWordDocument;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
