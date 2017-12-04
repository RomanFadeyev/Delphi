unit FC.Trade.ViewTradersDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ComCtrls,
  FC.Definitions, ActnList;

type
  TfmViewTradersDialog = class(TfmDialogClose_B)
    Label1: TLabel;
    lvTraders: TExtendListView;
    buProperties: TButton;
    ActionList1: TActionList;
    acTraderProperties: TAction;
    acDeleteTrader: TAction;
    Button1: TButton;
    Button2: TButton;
    accAddTrader: TAction;
    procedure accAddTraderExecute(Sender: TObject);
    procedure acDeleteTraderUpdate(Sender: TObject);
    procedure acDeleteTraderExecute(Sender: TObject);
    procedure lvTradersDblClick(Sender: TObject);
    procedure buOKDialogKey(Sender: TObject; var Key: Word);
    procedure lvTradersEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure acTraderPropertiesExecute(Sender: TObject);
    procedure acTraderPropertiesUpdate(Sender: TObject);
  private
    FProject: IStockProject;

    procedure Fill;

    function CurrentTrader: IStockTrader;
  public

    constructor Create(aProject: IStockProject); reintroduce;
  end;


implementation
  uses
  FC.Singletons,
  FC.Trade.CreateTraderDialog;

{$R *.dfm}

type
  TTraderListItem = class (TListItem)
  public
    Trader: IStockTrader;
  end;

{ TfmViewTradersDialog }

procedure TfmViewTradersDialog.Fill;
var
  aListItem: TTraderListItem;
  aTrader: IStockTrader;
  i: Integer;
begin

  lvTraders.Items.BeginUpdate;
  try
    lvTraders.Items.Clear;

    for i := 0 to FProject.TraderCount-1 do
    begin
      aTrader:=FProject.GetTrader(i);
      aListItem:=TTraderListItem.Create(lvTraders.Items);
      lvTraders.Items.AddItem(aListItem);
      aListItem.Caption:=aTrader.GetName;
      aListItem.Trader:=aTrader;
    end;
  finally
    lvTraders.Items.EndUpdate;
  end;

  if lvTraders.Items.Count=1 then
    lvTraders.Items[0].Selected:=true;
end;

procedure TfmViewTradersDialog.acTraderPropertiesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvTraders.Selected<>nil;
end;

constructor TfmViewTradersDialog.Create(aProject: IStockProject);
begin
  inherited Create(nil);
  Assert(aProject<>nil);
  FProject:=aProject;
  Fill;
end;

function TfmViewTradersDialog.CurrentTrader: IStockTrader;
begin
  result:=TTraderListItem(lvTraders.Selected).Trader;
end;

procedure TfmViewTradersDialog.acTraderPropertiesExecute(Sender: TObject);
begin
  CurrentTrader.ShowPropertyWindow;
end;

procedure TfmViewTradersDialog.lvTradersEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  TTraderListItem(Item).Trader.SetName(S);
end;

procedure TfmViewTradersDialog.buOKDialogKey(Sender: TObject; var Key: Word);
begin
  if (Key=VK_RETURN) then
    if lvTraders.IsEditing then
    begin
      key:=0;
      //Как сделать EndEdit?
      buOK.SetFocus;
      lvTraders.SetFocus;
    end;
end;

procedure TfmViewTradersDialog.lvTradersDblClick(Sender: TObject);
begin
  acTraderProperties.Execute;
end;

procedure TfmViewTradersDialog.accAddTraderExecute(Sender: TObject);
var
  aDialog : TfmCreateTraderDialog;
  aTrader : IStockTrader;
begin
  aDialog:=TfmCreateTraderDialog.Create(nil);
  try
    //Запуск диалога
    if aDialog.ShowModal=mrOK then
    begin
      aTrader:=FC.Singletons.TraderFactory.CreateTrader(aDialog.SelectedTrader.IID);
      aTrader.SetProject(FProject);
      if aTrader.ShowPropertyWindow then
      begin
        FProject.AddTrader(aTrader);
        Fill;
      end
      else begin
        aTrader.SetProject(nil);
        aTrader.Dispose;
        aTrader:=nil;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TfmViewTradersDialog.acDeleteTraderExecute(Sender: TObject);
begin
  CurrentTrader.GetProject.RemoveTrader(CurrentTrader);
  lvTraders.Selected.Free;
end;

procedure TfmViewTradersDialog.acDeleteTraderUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvTraders.Selected<>nil;
end;

end.
