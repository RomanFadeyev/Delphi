unit FC.Trade.Trainer.NewOrderDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Menus, ActnPopup,
  Buttons, Spin, Mask, StockChart.Definitions,  FC.Definitions, ufmDialogClose_B,FC.Trade.TrainerDialog,
  PlatformDefaultStyleActnCtrls;

type
  TfmTrainerNewOrderDialog = class(TfmDialogOkCancel_B)
    laLotsTitle: TLabel;
    edStopLoss: TExtendEdit;
    Label2: TLabel;
    Label3: TLabel;
    edTakeProfit: TExtendEdit;
    Label4: TLabel;
    buStopLoss: TSpeedButton;
    buTakeProfit: TSpeedButton;
    pmStopLoss: TPopupActionBar;
    N10pt1: TMenuItem;
    N201: TMenuItem;
    N30pt1: TMenuItem;
    N40pt1: TMenuItem;
    N50pt1: TMenuItem;
    N50pt2: TMenuItem;
    N50pt3: TMenuItem;
    N50pt4: TMenuItem;
    N80pt1: TMenuItem;
    N100pt1: TMenuItem;
    pmTakeProfit: TPopupActionBar;
    edTrailingStop: TExtendEdit;
    buTrailingStop: TSpeedButton;
    pmTrailingStop: TPopupActionBar;
    edLots: TExtendEdit;
    SpeedButton2: TSpeedButton;
    pmLots: TPopupActionBar;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    N1: TMenuItem;
    laCommentTitle: TLabel;
    edComment: TExtendEdit;
    laSymbol: TLabel;
    Label8: TLabel;
    laAsk: TLabel;
    laAskTitle: TLabel;
    N10pt2: TMenuItem;
    N20pt1: TMenuItem;
    N30pt2: TMenuItem;
    N40pt2: TMenuItem;
    N50pt5: TMenuItem;
    N60pt1: TMenuItem;
    N70pt1: TMenuItem;
    N80pt2: TMenuItem;
    N90pt1: TMenuItem;
    N100pt2: TMenuItem;
    N10pt3: TMenuItem;
    N20pt2: TMenuItem;
    N30pt3: TMenuItem;
    N40pt3: TMenuItem;
    N50pt6: TMenuItem;
    N60pt2: TMenuItem;
    N70pt2: TMenuItem;
    N80pt3: TMenuItem;
    N90pt2: TMenuItem;
    N100pt3: TMenuItem;
    edOpenPrice: TExtendEdit;
    laOpenPriceTitle: TLabel;
    laOrderType: TLabel;
    laBid: TLabel;
    Label5: TLabel;
    procedure acOKUpdate(Sender: TObject);
    procedure miTrailingStopClick(Sender: TObject);
    procedure miTakeProfit(Sender: TObject);
    procedure miStopLossClick(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure buTrailingStopClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure buTakeProfitClick(Sender: TObject);
    procedure buStopLossClick(Sender: TObject);
  private
    FOpenedOrder: IStockOrder;
    FTrader : TStockTraderTrainer;
    FOrderKind : TSCOrderKind;
    FZeroPrice: TStockRealNumber;
    FMarketPrice:TStockRealNumber;

  protected
    function PointToPrice(const aPoint: integer): TSCRealNumber;
    procedure ShowPriceLines;

    function GetOpenPrice: TStockRealNumber;
    function GetRate: integer;
    function GetStopLoss: TStockRealNumber;
    function GetTakeProfit: TStockRealNumber;
    function GetTrailingStop: TStockRealNumber;
    function GetComment: string;
    function GetIsPendingOrder: boolean;
  public
    property OpenedOrder: IStockOrder read FOpenedOrder write FOpenedOrder;
    property Trader : TStockTraderTrainer read FTrader;

    constructor Create(const aTrader: TStockTraderTrainer; aOrderKind: TSCOrderKind); reintroduce;
    destructor Destroy; override;
  end;

implementation
  uses Math, SystemService, FC.Datautils, FC.Trade.Trader.Base,StockChart.Definitions.Units;

type
  ICurentPriceLineID = interface
  ['{F01E139A-3370-4254-95C0-F74195D21621}']
  end;

  IStopLossLineID = interface
  ['{C136D790-C51E-4CD3-85A8-047EC21DEE66}']
  end;

  ITakeProfitLineID = interface
  ['{FE24B064-8D9F-42F5-A2B3-C2E0453F0E14}']
  end;

{$R *.dfm}

procedure TfmTrainerNewOrderDialog.acOKUpdate(Sender: TObject);
begin
  inherited;
  ShowPriceLines;

  if OpenedOrder=nil then
    if GetIsPendingOrder then
    begin
      laOrderType.Caption:='Pending Order';
      //TODO  Надо бы проверять близость цены и проч
    end
    else begin
      laOrderType.Caption:='Instant Execution';
    end;
end;

procedure TfmTrainerNewOrderDialog.buStopLossClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmStopLoss.Popup(P.x, P.y);
end;

constructor TfmTrainerNewOrderDialog.Create(const aTrader: TStockTraderTrainer; aOrderKind: TSCOrderKind);
var
  aMenuItem: TMenuItem;
  i: Integer;
  s,s2: string;
begin
  inherited Create(nil);
  FTrader:=aTrader;
  FOrderKind:=aOrderKind;
  Assert(aTrader<>nil);

  Caption:='New Order - Instant/Pending ' + OrderKindNames[aOrderKind];

  laSymbol.Caption:=FTrader.GetSymbol;
  laAsk.Caption:=PriceToStr(aTrader,aTrader.GetBroker.GetCurrentPrice(aTrader.GetSymbol,bpkAsk));
  laBid.Caption:=PriceToStr(aTrader,aTrader.GetBroker.GetCurrentPrice(aTrader.GetSymbol,bpkBid));

  if aOrderKind=okSell then
  begin
    FZeroPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkAsk);
    FMarketPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkBid);
    laBid.Font.Style:=[fsBold];
    laAsk.Font.Style:=[];
  end
  else begin
    FZeroPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkBid);
    FMarketPrice:=FTrader.GetBroker.GetCurrentPrice(FTrader.GetSymbol,bpkAsk);
    laAsk.Font.Style:=[fsBold];
    laBid.Font.Style:=[];
  end;
  edOpenPrice.ValueFloat :=FMarketPrice;

  //Stop Loss
  if aOrderKind=okSell then
    s:='+'
  else
    s:='-';

  for i := 0 to pmStopLoss.Items.Count - 1 do
  begin
    aMenuItem:=pmStopLoss.Items[i];

    if aMenuItem.Tag<100 then
      s2:='&'+IntToStr(aMenuItem.Tag)
    else
      s2:=IntToStr(aMenuItem.Tag);

    aMenuItem.Caption:=PriceToStr(FTrader,FZeroPrice)+ ' '+s+' '+s2+' pt = '+
                       PriceToStr(FTrader,FZeroPrice-OrderKindSign[FOrderKind]*PointToPrice(aMenuItem.Tag));
  end;

  ShowPriceLines;
end;

destructor TfmTrainerNewOrderDialog.Destroy;
begin
  edOpenPrice.Value:=0; //обязательно, чтобы стереть линию
  edStopLoss.Value:=0;
  edTakeProfit.Value:=0;
  FMarketPrice:=0;
  ShowPriceLines;

  FOpenedOrder:=nil;
  FTrader:=nil;

  inherited;
end;

procedure TfmTrainerNewOrderDialog.acOKExecute(Sender: TObject);
begin
  inherited;
  try
    if GetIsPendingOrder then
      FOpenedOrder:=FTrader.OpenOrderAt(FOrderKind,GetOpenPrice,GetRate,GetStopLoss,GetTakeProfit,GetTrailingStop,GetComment)
    else
      FOpenedOrder:=FTrader.OpenOrder(FOrderKind,GetRate,GetStopLoss,GetTakeProfit,GetTrailingStop,GetComment);
  except
    on E:Exception do
    begin
      ModalResult:=mrNone;
      MsgBox.MessageFailure(0,E.Message);
    end;
  end;
end;

function TfmTrainerNewOrderDialog.GetComment: string;
begin
  result:=edComment.Text;
end;

function TfmTrainerNewOrderDialog.GetIsPendingOrder: boolean;
begin
  result:=not SameValue(GetOpenPrice,FMarketPrice);
end;

function TfmTrainerNewOrderDialog.GetOpenPrice: TStockRealNumber;
begin
  result:=edOpenPrice.ValueFloat;
end;

function TfmTrainerNewOrderDialog.GetRate: integer;
begin
  result:=Round(edLots.ValueFloat*10);
end;

function TfmTrainerNewOrderDialog.GetStopLoss: TStockRealNumber;
begin
  result:=edStopLoss.ValueFloat;
end;

function TfmTrainerNewOrderDialog.GetTakeProfit: TStockRealNumber;
begin
  result:=edTakeProfit.ValueFloat;
end;

function TfmTrainerNewOrderDialog.GetTrailingStop: TStockRealNumber;
begin
  result:=PointToPrice(edTrailingStop.Value);
end;

procedure TfmTrainerNewOrderDialog.miStopLossClick(Sender: TObject);
begin
  inherited;
  edStopLoss.ValueFloat:=FZeroPrice-OrderKindSign[FOrderKind]*PointToPrice(TMenuItem(Sender).Tag);
end;

procedure TfmTrainerNewOrderDialog.miTakeProfit(Sender: TObject);
begin
  edTakeProfit.ValueFloat:={FZeroPrice}+FMarketPrice+OrderKindSign[FOrderKind]*PointToPrice(TMenuItem(Sender).Tag);
end;

procedure TfmTrainerNewOrderDialog.miTrailingStopClick(Sender: TObject);
begin
  edTrailingStop.Value:=TMenuItem(Sender).Tag;
end;

function TfmTrainerNewOrderDialog.PointToPrice(const aPoint: integer): TSCRealNumber;
begin
  result:=FTrader.GetBroker.PointToPrice(FTrader.GetSymbol,aPoint);
end;

procedure TfmTrainerNewOrderDialog.buTakeProfitClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmTakeProfit.Popup(P.x, P.y);
end;

procedure TfmTrainerNewOrderDialog.buTrailingStopClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmTrailingStop.Popup(P.x, P.y);
end;

procedure TfmTrainerNewOrderDialog.ShowPriceLines;
var
  aProject: IStockProject;
  aInterval: TStockTimeInterval;
  aLine:ISCIndicatorPriceLine;
begin
  aProject:=FTrader.GetProject;
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    if FMarketPrice<>0 then
    begin
      aLine:=aProject.GetStockChart(aInterval).GetPriceLine(ICurentPriceLineID,true);
      aLine.SetPrice(FMarketPrice);
      aLine.SetColor(clTeal);
      aLine.SetPrice(FMarketPrice);
      aLine.SetAlwaysVisible(true);
   end
   else
     aProject.GetStockChart(aInterval).RemovePriceLine(ICurentPriceLineID);

   if GetStopLoss<>0 then
   begin
      aLine:=aProject.GetStockChart(aInterval).GetPriceLine(IStopLossLineID,true);
      aLine.SetPrice(GetStopLoss);
      aLine.SetColor(clRed);
      aLine.SetStyle(lsDashDot);
      aLine.SetTitle('SL');
      aLine.SetAlwaysVisible(true);
   end
   else
     aProject.GetStockChart(aInterval).RemovePriceLine(IStopLossLineID);

   if GetTakeProfit<>0 then
   begin
      aLine:=aProject.GetStockChart(aInterval).GetPriceLine(ITakeProfitLineID,true);
      aLine.SetPrice(GetTakeProfit);
      aLine.SetColor(clGreen);
      aLine.SetStyle(lsDashDot);
      aLine.SetTitle('TP');      
      aLine.SetAlwaysVisible(true);
    end
    else
     aProject.GetStockChart(aInterval).RemovePriceLine(ITakeProfitLineID);
  end;
end;

procedure TfmTrainerNewOrderDialog.SpeedButton2Click(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmLots.Popup(P.x, P.y);
end;

end.
