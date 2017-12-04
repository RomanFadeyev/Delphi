unit FC.Trade.TerminalFrame;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtendControls, FC.Trade.ResultPage, ExtCtrls, Menus, ActnPopup, DB,
  MemoryDS, ActnList, ComCtrls, ToolWin, StdCtrls, TeEngine, Series, TeeProcs,
  Chart, Grids, DBGrids, MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid,
  JvExComCtrls, JvComCtrls, PlatformDefaultStyleActnCtrls, FC.Definitions,Collections.Map;

type
  TfrmTerminalFrame = class(TfrmStockTradeResult)
    acModifyOrder: TAction;
    N2: TMenuItem;
    miModifyOrder: TMenuItem;
    acCloseOrder: TAction;
    CloseOrder1: TMenuItem;
    procedure acCloseOrderUpdate(Sender: TObject);
    procedure acModifyOrderExecute(Sender: TObject);
    procedure acModifyOrderUpdate(Sender: TObject);
  private type
    TOrderMap = TMap<TGUID,IStockOrder>; //Order Id -> IStockOrder
  private
    FOrders : TOrderMap;
    FWorking : boolean;
    function GetSelectedOrder: IStockOrder;
  public
    procedure OnNewOrder(const aOrder: IStockOrder); override;
    procedure OnStart(const aBroker: IStockBroker; const aTrader:IStockTrader); override;
    procedure OnEnd; override;

    property SelectedOrder: IStockOrder read GetSelectedOrder;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
  uses BaseUtils;

{$R *.dfm}

procedure TfrmTerminalFrame.acCloseOrderUpdate(Sender: TObject);
var
  aSelectedOrder:IStockOrder;
begin
  aSelectedOrder:=SelectedOrder;
  TAction(Sender).Enabled:=FWorking and (aSelectedOrder<>nil) and (aSelectedOrder.GetState in [osPending,osOpened]);
end;

procedure TfrmTerminalFrame.acModifyOrderExecute(Sender: TObject);
begin
  inherited;
  //TODO доделать
  raise ENotSupported.Create;
end;

procedure TfrmTerminalFrame.acModifyOrderUpdate(Sender: TObject);
var
  aSelectedOrder:IStockOrder;
begin
  aSelectedOrder:=SelectedOrder;
  TAction(Sender).Enabled:=FWorking and (aSelectedOrder<>nil) and (aSelectedOrder.GetState in [osPending,osOpened]);
end;

constructor TfrmTerminalFrame.Create(aOwner: TComponent);
begin
  inherited;
  Mode:=tmReal;
  FOrders:=TOrderMap.Create;
end;

destructor TfrmTerminalFrame.Destroy;
begin
  FreeAndNil(FOrders);
  inherited;
end;

function TfrmTerminalFrame.GetSelectedOrder: IStockOrder;
begin
  result:=nil;
  if (taOrders.Active) and (not taOrders.Eof) and (not taOrders.Bof) then
     FOrders.Lookup(StringToGUID(taOrdersOrderID.Value),result);
end;

procedure TfrmTerminalFrame.OnEnd;
begin
  inherited;
  FWorking:=false;
end;

procedure TfrmTerminalFrame.OnNewOrder(const aOrder: IStockOrder);
begin
  inherited;
  FOrders.Add(aOrder.GetID,aOrder);
end;

procedure TfrmTerminalFrame.OnStart(const aBroker: IStockBroker; const aTrader: IStockTrader);
begin
  inherited;
  FOrders.Clear;
  FWorking:=true;
end;

end.
