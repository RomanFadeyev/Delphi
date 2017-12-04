unit FC.Trade.ViewAlertersDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ComCtrls,
  FC.Definitions, ActnList, Menus, ActnPopup;

type
  TfmViewAlertersDialog = class(TfmDialogClose_B)
    Label1: TLabel;
    lvAlerters: TExtendListView;
    buProperties: TButton;
    ActionList1: TActionList;
    acAlerterProperties: TAction;
    acDeleteAlerter: TAction;
    buAdd: TButton;
    accAddAlerter: TAction;
    Button1: TButton;
    pmAlerter: TPopupActionBar;
    Properties1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Add1: TMenuItem;
    procedure lvAlertersAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvAlertersChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure accAddAlerterExecute(Sender: TObject);
    procedure acDeleteAlerterUpdate(Sender: TObject);
    procedure acDeleteAlerterExecute(Sender: TObject);
    procedure lvAlertersDblClick(Sender: TObject);
    procedure buOKDialogKey(Sender: TObject; var Key: Word);
    procedure lvAlertersEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure acAlerterPropertiesExecute(Sender: TObject);
    procedure acAlerterPropertiesUpdate(Sender: TObject);
  private
    FProject: IStockProject;

    procedure Fill;

    function CurrentAlerter: IStockAlerter;
  public
    constructor Create(aProject: IStockProject); reintroduce;
  end;


implementation
  uses
  FC.Singletons,
  FC.Trade.CreateAlerterDialog;

{$R *.dfm}

type
  TAlerterListItem = class (TListItem)
  public
    Alerter: IStockAlerter;
  end;

{ TfmViewAlertersDialog }

procedure TfmViewAlertersDialog.Fill;
var
  aListItem: TAlerterListItem;
  aAlerter: IStockAlerter;
  i: Integer;
begin
  lvAlerters.HandleNeeded;

  lvAlerters.Items.BeginUpdate;
  try
    lvAlerters.Items.Clear;

    for i := 0 to FProject.AlerterCount-1 do
    begin
      aAlerter:=FProject.GetAlerter(i);
      aListItem:=TAlerterListItem.Create(lvAlerters.Items);
      lvAlerters.Items.AddItem(aListItem);
      aListItem.Caption:=aAlerter.GetName;
      aListItem.Alerter:=aAlerter;
      aListItem.Checked:=aAlerter.GetEnabled;
    end;
  finally
    lvAlerters.Items.EndUpdate;
  end;

  if lvAlerters.Items.Count>0 then
    lvAlerters.Items[0].Selected:=true;
end;

procedure TfmViewAlertersDialog.acAlerterPropertiesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvAlerters.Selected<>nil;
end;

constructor TfmViewAlertersDialog.Create(aProject: IStockProject);
begin
  inherited Create(nil);
  Assert(aProject<>nil);
  FProject:=aProject;
  Fill;
end;

function TfmViewAlertersDialog.CurrentAlerter: IStockAlerter;
begin
  result:=TAlerterListItem(lvAlerters.Selected).Alerter;
end;

procedure TfmViewAlertersDialog.acAlerterPropertiesExecute(Sender: TObject);
begin
  CurrentAlerter.ShowPropertyWindow;
  lvAlerters.Selected.Caption:=CurrentAlerter.GetName;
end;

procedure TfmViewAlertersDialog.lvAlertersEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  TAlerterListItem(Item).Alerter.SetName(S);
end;

procedure TfmViewAlertersDialog.buOKDialogKey(Sender: TObject; var Key: Word);
begin
  if (Key=VK_RETURN) then
    if lvAlerters.IsEditing then
    begin
      key:=0;
      //Как сделать EndEdit?
      buOK.SetFocus;
      lvAlerters.SetFocus;
    end;
end;

procedure TfmViewAlertersDialog.lvAlertersAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  inherited;
  if not Item.Checked then
    Sender.Canvas.Font.Color:=clGrayText
  else
    Sender.Canvas.Font.Color:=clWindowText  
end;

procedure TfmViewAlertersDialog.lvAlertersChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Item<>nil then
    if TAlerterListItem(Item).Alerter<>nil then
      if TAlerterListItem(Item).Alerter.GetEnabled<>Item.Checked then
        TAlerterListItem(Item).Alerter.SetEnabled(Item.Checked);
end;

procedure TfmViewAlertersDialog.lvAlertersDblClick(Sender: TObject);
begin
  acAlerterProperties.Execute;
end;

procedure TfmViewAlertersDialog.accAddAlerterExecute(Sender: TObject);
var
  aDialog : TfmCreateAlerterDialog;
  aAlerter : IStockAlerter;
begin
  aDialog:=TfmCreateAlerterDialog.Create(nil);
  try
    //Запуск диалога
    if aDialog.ShowModal=mrOK then
    begin
      aAlerter:=FC.Singletons.AlerterFactory.CreateAlerter(aDialog.SelectedAlerter.IID);
      aAlerter.SetProject(FProject);
      if aAlerter.ShowPropertyWindow then
      begin
        FProject.AddAlerter(aAlerter);
        aAlerter.SetEnabled(true);
        Fill;
      end
      else begin
        aAlerter.SetProject(nil);
        aAlerter.Dispose;
        aAlerter:=nil;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TfmViewAlertersDialog.acDeleteAlerterExecute(Sender: TObject);
begin
  CurrentAlerter.GetProject.RemoveAlerter(CurrentAlerter);
  lvAlerters.Selected.Free;
end;

procedure TfmViewAlertersDialog.acDeleteAlerterUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvAlerters.Selected<>nil;
end;

end.
