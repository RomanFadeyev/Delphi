unit FC.Trade.Trader.TestBenchDialog_B;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ComCtrls,
  FC.DataUtils, FC.Dialogs.DockedDialogCloseAndAppWindow_B, ImgList, JvComponentBase,
  JvCaptionButton, CheckLst,
  FC.Definitions, StockChart.Definitions.Units, StockChart.Definitions, JvDockControlForm,
  FC.Trade.Trader.Base;

type
  TfmTestBenchDialog_B = class(TfmDockedDialogCloseAndAppWindow_B)
  private
    FTrader: TStockTraderBase;
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(aTrader: TStockTraderBase); reintroduce; virtual;
    destructor Destroy; override;

    property Trader: TStockTraderBase read FTrader;
  end;

  TfmTestBenchDialogClass  = class of TfmTestBenchDialog_B;

implementation
  uses Math, SystemService;

{$R *.dfm}


{ TfmTestBenchDialog_B }

constructor TfmTestBenchDialog_B.Create(aTrader: TStockTraderBase);
begin
  inherited Create(nil);
  FTrader:=aTrader;
end;

destructor TfmTestBenchDialog_B.Destroy;
begin
  inherited;
end;

procedure TfmTestBenchDialog_B.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
  if FTrader<>nil then
    FTrader.OnCloseTestBenchDialog(self);
  FTrader:=nil;
end;

end.
