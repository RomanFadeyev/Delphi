unit FC.Trade.Trainer.ModifyOrderDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Menus, ActnPopup,
  Buttons, Spin, Mask, StockChart.Definitions,  FC.Definitions, ufmDialogClose_B,FC.Trade.TrainerDialog,
  FC.Trade.Trainer.NewOrderDialog, PlatformDefaultStyleActnCtrls;

type
  TfmTrainerModifyOrderDialog = class(TfmTrainerNewOrderDialog)
    procedure acOKExecute(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
  public
    constructor Create(const aTrader: TStockTraderTrainer; aOrder: IStockorder);
    destructor Destroy; override;
  end;

implementation
  uses SystemService, BaseUtils, FC.Datautils, FC.Trade.Trader.Base;

{$R *.dfm}

{ TfmTrainerModifyOrderDialog }

procedure TfmTrainerModifyOrderDialog.acOKExecute(Sender: TObject);
begin
  try
    if (OpenedOrder.GetState=osPending) and (GetOpenPrice<>OpenedOrder.GetPendingOpenPrice)  then
      OpenedOrder.OpenAt(OpenedOrder.GetSymbol,OpenedOrder.GetKind,GetOpenPrice,GetRate,GetStopLoss,GetTakeProfit,GetTrailingStop,GetComment)
    else begin
      OpenedOrder.SetStopLoss(GetStopLoss);
      OpenedOrder.SetTakeProfit(GetTakeProfit);
      OpenedOrder.SetTrailingStop(GetTrailingStop);
    end;
  except
    on E:Exception do
    begin
      ModalResult:=mrNone;
      MsgBox.MessageFailure(0,E.Message);
    end;
  end;
end;

procedure TfmTrainerModifyOrderDialog.acOKUpdate(Sender: TObject);
begin
  inherited;
  edOpenPrice.Enabled:=OpenedOrder.GetState=osPending;
  laOpenPriceTitle.Enabled:=edOpenPrice.Enabled;

  edLots.Enabled:=OpenedOrder.GetState=osPending;
  laLotsTitle.Enabled:=edLots.Enabled;

  edComment.Enabled:=(OpenedOrder.GetState=osPending) and (GetOpenPrice<>OpenedOrder.GetPendingOpenPrice) ;
  laCommentTitle.Enabled:=edComment.Enabled;

  if (OpenedOrder.GetState=osPending) and (GetOpenPrice<>OpenedOrder.GetPendingOpenPrice)  then
    laOrderType.Caption:='Reopen Pending Order'
  else if OpenedOrder.GetState=osPending then
    laOrderType.Caption:='Modify Pending Order'
  else
    laOrderType.Caption:='Modify Opened Order';
end;

constructor TfmTrainerModifyOrderDialog.Create(const aTrader: TStockTraderTrainer; aOrder: IStockorder);
begin
  if aOrder=nil then
    raise EAlgoError.Create;

  if not (aOrder.GetState in [osOpened,osPending]) then
    raise EAlgoError.Create;

  inherited Create(aTrader,aOrder.GetKind);

  OpenedOrder:=aOrder;
  Caption:='Modify '+OrderKindNames[aOrder.GetKind]+' Order';

  if OpenedOrder.GetState=osPending then
    edOpenPrice.ValueFloat:=OpenedOrder.GetPendingOpenPrice
  else begin
    edOpenPrice.ValueFloat:=OpenedOrder.GetOpenPrice;
    if OpenedOrder.GetKind=okBuy then
    begin
      laAsk.Font.Style:=[];
      laBid.Font.Style:=[fsBold];
    end
    else begin
      laAsk.Font.Style:=[fsBold];
      laBid.Font.Style:=[];
    end;
  end;


  edLots.ValueFloat:=OpenedOrder.GetLots;
  edStopLoss.ValueFloat:=OpenedOrder.GetStopLoss;
  edTakeProfit.ValueFloat:=OpenedOrder.GetTakeProfit;
  edTrailingStop.ValueFloat:=OpenedOrder.GetTrailingStop;

  if edStopLoss.ValueFloat=0 then
    edStopLoss.Text:='';
  if edTakeProfit.ValueFloat=0 then
    edTakeProfit.Text:='';
  if edTrailingStop.ValueFloat=0 then
    edTrailingStop.Text:='';
end;

destructor TfmTrainerModifyOrderDialog.Destroy;
begin
  inherited;
end;

end.
