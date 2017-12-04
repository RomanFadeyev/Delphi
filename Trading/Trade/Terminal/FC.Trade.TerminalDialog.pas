unit FC.Trade.TerminalDialog;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FC.Dialogs.DockedDialogAndAppWindow_B, ImgList, JvCaptionButton, JvComponentBase,
  JvDockControlForm, ExtendControls,Application.Definitions,FC.Definitions, StdCtrls,ActnList,
  FC.Trade.TerminalFrame, FC.Trade.ResultPage;

type
  TfmTerminalDialog = class(TfmDockedDialogAndAppWindow_B,IStockBrokerEventHandler)
    frmTerminal: TfrmTerminalFrame;
  private
    procedure OnViewTerminalExecute(aAction:TCustomAction);
    procedure OnViewTerminalUpdate(aAction:TCustomAction);

    //from IStockBrokerEventHandler
    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewOrder   (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewData    (const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;
  protected
    procedure OnFirstShow; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function  IsAppWindowByDefault: boolean; override;
  end;


implementation
uses
  SystemService,Application.Obj,FC.Factory,FC.Singletons,
  FC.fmUIDataStorage, ufmForm_B;

{$R *.dfm}


type
  TDialogStarter = class(Application.Obj.TActionTarget,IWorkspaceEventHandler)
  private
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
  //RegisterActionExecute(fmUIDataStorage.acToolsStockBrokerCenter,OnBrokerConnectionCenterExecute);
end;

destructor TDialogStarter.Destroy;
begin
  inherited;
end;

procedure TDialogStarter.OnEvent(const aEventName: string; aParams: TObject);
begin
  if AnsiSameText(aEventName,'MainFrameCreated') then
  begin
    TfmTerminalDialog.Create(nil);
  end;
end;

{ TfmTerminalDialog }

constructor TfmTerminalDialog.Create(aOwner: TComponent);
begin
  inherited;
  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);  
  ActionTarget.RegisterAction(fmUIDataStorage.acViewTradeTerminal,OnViewTerminalUpdate,OnViewTerminalExecute);
  Workspace.MainFrame.AddActionTarget(self);
end;

destructor TfmTerminalDialog.Destroy;
begin
  StockBrokerConnectionRegistry.RemoveBrokerEventHandler(self);
  inherited;
end;

function TfmTerminalDialog.IsAppWindowByDefault: boolean;
begin
  result:=false;
end;

procedure TfmTerminalDialog.OnFirstShow;
begin
  inherited;
end;

procedure TfmTerminalDialog.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder;  const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  frmTerminal.OnModifyOrder(aOrder,aModifyEventArgs);
end;

procedure TfmTerminalDialog.OnNewData(const aSender: IStockBroker;
  const aSymbol: string);
begin

end;

procedure TfmTerminalDialog.OnNewMessage(const aSender: IStockBroker;
  const aMessage: IStockBrokerMessage);
begin
  frmTerminal.OnNewMessage(aSender,aMessage);
end;

procedure TfmTerminalDialog.OnNewMessage(const aSender: IStockBroker;
  const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
  frmTerminal.OnNewMessage(aSender,aOrder,aMessage);
end;

procedure TfmTerminalDialog.OnNewOrder(const aSender: IStockBroker;
  const aOrder: IStockOrder);
begin
  frmTerminal.OnNewOrder(aOrder);
end;

procedure TfmTerminalDialog.OnStart(const aSender: IStockBroker);
begin
  frmTerminal.OnStart(aSender,nil);
end;

procedure TfmTerminalDialog.OnViewTerminalExecute(aAction: TCustomAction);
begin
  Show;
end;

procedure TfmTerminalDialog.OnViewTerminalUpdate(aAction: TCustomAction);
begin
  //aAction.Checked:=Self.Visible;
end;

initialization
  TActionTargetRegistry.AddActionTarget(TDialogStarter.Create);

end.


