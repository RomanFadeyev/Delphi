{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Базовый класс реализации соединения с брокером. См. IStockBrokerConnection
            Каждый класс соединения с брокером должен зарегистировать свой экземпляр в 
            TStockBrokerConnectionRegistry (см. FC.Singletons)
            
 History:
-----------------------------------------------------------------------------}

unit FC.BrokerConnection.Base;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,BaseUtils,Properties.Obj,Properties.Definitions,
  Application.Definitions, FC.Definitions,StockChart.Definitions;

type
  TStockBrokerPropertyEventHandler = class;

  TStockBrokerConnectionBase = class (TStockInterfacedObjectVirtual,IStockBrokerConnection,IWorkspaceEventHandler)
  private
    FPropertyMediator: TStockBrokerPropertyEventHandler;
  protected
    procedure GetProperties(aList: TPropertyList); overload; virtual;
    function  GetProperties: IPropertyCollection; overload;
    procedure OnPropertyChanged(aNotifier: TProperty); virtual;
    property  PropertyMediator: TStockBrokerPropertyEventHandler read FPropertyMediator;
  public
    //from IStockBrokerConnection

    function  GetName: string; virtual; abstract;
    function  GetDescription: string; virtual; abstract;
    function  GetBroker: IStockBroker;virtual; abstract;

    procedure Enable(aValue:boolean); virtual; abstract;
    function  HasShowSettings: boolean;
    function  ShowSettingsDialog:boolean; virtual;
    //end of IStockBrokerConnection

    //from IWorkspaceEventHandler
    procedure OnEvent(const aEventName: string; aParams: TObject);
    //end  IWorkspaceEventHandler

    constructor Create; override;
    destructor Destroy; override;
  end;

  TStockBrokerPropertyEventHandler = class (TInterfacedObject,IPropertyChangeHandler)
  private
    FOwner: TStockBrokerConnectionBase;
  public
    //from IPropertyChangeHandler
    procedure OnPropertyChanged(aNotifier:TProperty);

    constructor Create(aOwner: TStockBrokerConnectionBase);
  end;

implementation

{ TStockBrokerPropertyEventHandler }

constructor TStockBrokerPropertyEventHandler.Create(aOwner: TStockBrokerConnectionBase);
begin
  FOwner:=aOwner;
end;

procedure TStockBrokerPropertyEventHandler.OnPropertyChanged(aNotifier: TProperty);
begin
  FOwner.OnPropertyChanged(aNotifier);
end;

{ TStockBrokerConnectionBase }

constructor TStockBrokerConnectionBase.Create;
begin
  FPropertyMediator:=TStockBrokerPropertyEventHandler.Create(self);
  IInterface(FPropertyMediator)._AddRef;
  Workspace.AddEventHandler(self);
end;

destructor TStockBrokerConnectionBase.Destroy;
begin
  IInterface(FPropertyMediator)._Release;
  FPropertyMediator:=nil;
  inherited;
end;

function TStockBrokerConnectionBase.ShowSettingsDialog: boolean;
begin
  result:=false;
end;

function TStockBrokerConnectionBase.HasShowSettings: boolean;
begin
  result:=false;
end;

function TStockBrokerConnectionBase.GetProperties: IPropertyCollection;
var
  aCollection: TPropertyCollection;
begin
  aCollection:=TPropertyCollection.Create;
  GetProperties(aCollection.List);
  result:=aCollection;
end;

procedure TStockBrokerConnectionBase.GetProperties(aList: TPropertyList);
begin

end;


procedure TStockBrokerConnectionBase.OnPropertyChanged(aNotifier: TProperty);
begin

end;

procedure TStockBrokerConnectionBase.OnEvent(const aEventName: string; aParams: TObject);
begin
  if AnsiSameText(aEventName,'Exit') then
  begin
    Workspace.RemoveEventHandler(self);
    Enable(false);
  end;
end;

end.
