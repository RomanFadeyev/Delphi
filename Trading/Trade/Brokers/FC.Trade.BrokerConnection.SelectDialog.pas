{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   ƒиалог дл€ выборка активного соединени€ с брокером
            
 History:
-----------------------------------------------------------------------------}

unit FC.Trade.BrokerConnection.SelectDialog;

interface
{$I Compiler.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,ComCtrls,
  FC.fmUIDataStorage,FC.Definitions, ufmForm_B;

type
  TfmSelectBrokerConnectionDialog = class(TfmDialogOkCancel_B)
    paWorkSpace: TPanel;
    Label1: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    lvBrokerConnections: TExtendListView;
    ckConnectAtStartup: TExtendCheckBox;
    procedure lvBrokerConnectionsDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acOKUpdate(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation

uses
  SystemService,Application.Definitions,Application.Obj,FC.Factory,FC.Singletons,ufmDialogOK_B;

{$R *.dfm}

type
  TDialogStarter = class(Application.Obj.TActionTarget,IWorkspaceEventHandler)
  private
    FRunAtStartup: boolean;
    FCurrentBrokerConnection: string;

    procedure OnBrokerConnectionCenterExecute(Action: TCustomAction);
  public
    //from IWorkspaceEventHandler
    procedure OnEvent(const aEventName: string; aParams: TObject);

    constructor Create;
    destructor  Destroy; override;
  end;

{ TDialogStarter }

constructor TDialogStarter.Create;
begin
  inherited;
  Workspace.AddEventHandler(self);

  RegisterActionExecute(fmUIDataStorage.acToolsStockBrokerCenter,OnBrokerConnectionCenterExecute);
  FCurrentBrokerConnection:=Workspace.Storage(self).ReadString('Connections','CurrentConnection','');
  FRunAtStartup:=Workspace.Storage(self).ReadBoolean('Connections','RunAtStartup',false);
end;

destructor TDialogStarter.Destroy;
begin
  Workspace.Storage(self).WriteString('Connections','CurrentConnection',FCurrentBrokerConnection);
  Workspace.Storage(self).WriteBoolean('Connections','RunAtStartup',FRunAtStartup);
  inherited;
end;

procedure TDialogStarter.OnBrokerConnectionCenterExecute(Action: TCustomAction);
begin
  with TfmSelectBrokerConnectionDialog.Create(nil) do
  begin
    ckConnectAtStartup.Checked:=FRunAtStartup;

    if ShowModal=mrOk then
      if StockBrokerConnectionRegistry.CurrentConnection=nil then
      begin
        FCurrentBrokerConnection:=''
      end
      else begin
        FCurrentBrokerConnection:=StockBrokerConnectionRegistry.CurrentConnection.GetName;
        FRunAtStartup:=ckConnectAtStartup.Checked;
      end;
  end;
end;

procedure TDialogStarter.OnEvent(const aEventName: string; aParams: TObject);
var
  i: integer;
begin
  if AnsiSameText(aEventName,'Start') then
  begin
    if (FCurrentBrokerConnection<>'') and FRunAtStartup then
    begin
      for i := 0 to StockBrokerConnectionRegistry.ConnectionCount - 1 do
        if StockBrokerConnectionRegistry.Connections[i].GetName=FCurrentBrokerConnection then
        begin
          try
            StockBrokerConnectionRegistry.CurrentConnection:=StockBrokerConnectionRegistry.Connections[i];
          except
            on E:Exception do
              Workspace.MainFrame.HandleException(E);
          end;

          break;
        end;
    end;
  end;
end;

constructor TfmSelectBrokerConnectionDialog.Create(aOwner: TComponent);
var
  i: integer;
  aConnection: IStockBrokerConnection;
  aItem: TListItem;
begin
  inherited;

  aItem:=lvBrokerConnections.Items.Add;
  aItem.Caption:='[None]';
  aItem.Data:=nil;
  aItem.Selected:=true;
  aItem.Focused:=true;

  for i:=0 to StockBrokerConnectionRegistry.ConnectionCount-1 do
  begin
    aConnection:=StockBrokerConnectionRegistry.Connections[i];
    aItem:=lvBrokerConnections.Items.Add;
    aItem.Caption:=aConnection.GetName;
    aItem.SubItems.Add(aConnection.GetDescription);
    aItem.Data:=pointer(aConnection);

    if aConnection=StockBrokerConnectionRegistry.CurrentConnection then
    begin
      aItem.Selected:=true;
      aItem.Focused:=true;
    end;
  end;
end;

destructor TfmSelectBrokerConnectionDialog.Destroy;
begin
  inherited;
end;

procedure TfmSelectBrokerConnectionDialog.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvBrokerConnections.Selected<>nil;
  ckConnectAtStartup.Enabled:=(lvBrokerConnections.Selected<>nil) and (lvBrokerConnections.Selected.Data<>nil);
end;

procedure TfmSelectBrokerConnectionDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    try
      TWaitCursor.SetUntilIdle;
      if lvBrokerConnections.Selected.Data=nil then
        StockBrokerConnectionRegistry.CurrentConnection:=nil
      else
        StockBrokerConnectionRegistry.CurrentConnection:=IStockBrokerConnection(lvBrokerConnections.Selected.Data);
    except
      on E:Exception do
      begin
        Workspace.MainFrame.HandleException(E);
        CanClose:=false;
      end;
    end;
  end;
end;

procedure TfmSelectBrokerConnectionDialog.lvBrokerConnectionsDblClick(Sender: TObject);
begin
  inherited;
  acOK.Execute;
end;

initialization
  TActionTargetRegistry.AddActionTarget(TDialogStarter.Create);


end.


