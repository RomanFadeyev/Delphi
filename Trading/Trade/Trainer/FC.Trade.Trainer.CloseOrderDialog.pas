unit FC.Trade.Trainer.CloseOrderDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  FC.Trade.TrainerDialog, Mask,
  StockChart.Definitions,  FC.Definitions;

type
  TfmTrainerCloseOrderDialog = class(TfmDialogOkCancel_B)
    edComment: TExtendEdit;
    laCommentTitle: TLabel;
    Label8: TLabel;
    laSymbol: TLabel;
    laPrice: TLabel;
    laPriceValue: TLabel;
    laProfit: TLabel;
    Label2: TLabel;
    procedure acOKExecute(Sender: TObject);
  private
    FTrader: TStockTraderTrainer;
    FOpenedOrder: IStockOrder;
  public
    property OpenedOrder: IStockOrder read FOpenedOrder write FOpenedOrder;
    property Trader : TStockTraderTrainer read FTrader;

    constructor Create(const aTrader: TStockTraderTrainer; aOrder: IStockorder); reintroduce;
    destructor Destroy; override;

  end;


implementation
  uses Math, Baseutils, SystemService, FC.Datautils, FC.Trade.Trader.Base;

{$R *.dfm}

{ TfmTrainerCloseOrderDialog }

procedure TfmTrainerCloseOrderDialog.acOKExecute(Sender: TObject);
begin
  try
    OpenedOrder.Close(edComment.Text);
  except
    on E:Exception do
    begin
      ModalResult:=mrNone;
      MsgBox.MessageFailure(0,E.Message);
    end;
  end;
end;

constructor TfmTrainerCloseOrderDialog.Create(const aTrader: TStockTraderTrainer; aOrder: IStockorder);
var
  aMarketPrice: TStockRealNumber;
begin
  if aOrder=nil then
    raise EAlgoError.Create;

  if not (aOrder.GetState in [osOpened]) then
    raise EAlgoError.Create;

  inherited Create(nil);

  FTrader:=aTrader;
  OpenedOrder:=aOrder;
  Caption:='Close '+OrderKindNames[aOrder.GetKind]+' Order';

  laSymbol.Caption:=FTrader.GetSymbol;

  if OpenedOrder.GetKind=okSell then
  begin
    aMarketPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkAsk);
    laPrice.Caption:='Close Price (Ask)';
  end
  else begin
    aMarketPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkBid);
    laPrice.Caption:='Close Price (Bid)';
  end;
  laPriceValue.Caption:=PriceToStr(FTrader,aMarketPrice);
  laProfit.Caption:=IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetCurrentProfit));
end;

destructor TfmTrainerCloseOrderDialog.Destroy;
begin

  inherited;
end;

end.
