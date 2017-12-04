{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Конечная реализация соединения с брокером. В данном случае используется
            тестовый брокер, эмулирующий котировки по истории. См проект
            "X:\Trade\Forecaster\Stock Server"

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.TestServer.BrokerConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,BaseUtils, StockServer_TLB,
  FC.Definitions,
  FC.BrokerConnection.Base,
  FC.Trade.Brokers.TestServer.Broker,
  FC.Factory;

type
  TStockBrokerConnectionTestServer = class (TStockBrokerConnectionBase)
  private
    FDataDistributor: IStockDataDistributor;
    FEventSink      : IStockDataDistributorEventSink;
    FBroker         : TStockBroker;

    procedure Disconnect;
  public
    function  GetName: string; override;
    function  GetDescription: string; override;
    function  GetBroker: IStockBroker; override;

    procedure Enable(aValue:boolean); override;
    destructor Destroy; override;
  end;

  TStockDataDistributorEventSink = class (TInterfacedObject,IStockDataDistributorEventSink)
  private
    FOwner: TStockBrokerConnectionTestServer;
  public
    procedure OnNewData(const a_Symbol: WideString; a_Interval: Integer; a_DateTime: TDateTime;
                        a_Open: Double; a_High: Double; a_Low: Double; a_Close: Double;
                        a_Volume: Integer); safecall;

    procedure OnOpenOrder(const aOrder: IStockServerOrder); safecall;
    procedure OnCloseOrder(const aOrder: IStockServerOrder); safecall;

    constructor Create(aOwner: TStockBrokerConnectionTestServer);
  end;

implementation
  uses ActiveX,ComObj,
  StockChart.Obj,StockChart.Definitions,
  FC.Singletons,FC.DataUtils;

{ TStockBrokerConnectionTestServer }

destructor TStockBrokerConnectionTestServer.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TStockBrokerConnectionTestServer.Disconnect;
begin
  try
    if (FDataDistributor<>nil) and (FEventSink<>nil) then
      FDataDistributor.RemoveEventSink(FEventSink);
  except
    on E:EOleException do
      ;
  end;

  if FBroker<>nil then
  begin
    IInterface(FBroker)._Release;
    FBroker:=nil;
  end;
  
  FEventSink:=nil;
  FDataDistributor:=nil;
end;

procedure TStockBrokerConnectionTestServer.Enable(aValue: boolean);
var
  unk: IInterface;
begin
  if (FDataDistributor<>nil) and aValue then
    exit;

  Disconnect;

  if aValue then
  begin
    if not Succeeded(GetActiveObject(CLASS_StockDataDistributor, nil, unk)) then
       unk := CreateComObject(CLASS_StockDataDistributor);

    FDataDistributor:=unk as IStockDataDistributor;
    FEventSink:=TStockDataDistributorEventSink.Create(self);
    FDataDistributor.AddEventSink(FEventSink);

    FBroker:=TStockBroker.Create(FDataDistributor.GetBroker);
    IInterface(FBroker)._AddRef;
  end;
end;

function TStockBrokerConnectionTestServer.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockBrokerConnectionTestServer.GetDescription: string;
begin
  result:='Provides connection to test server. It is used for debug.'
end;

function TStockBrokerConnectionTestServer.GetName: string;
begin
  result:='Test Server';
end;

{ TStockDataDistributorEventSink }

constructor TStockDataDistributorEventSink.Create(aOwner: TStockBrokerConnectionTestServer);
begin
  FOwner:=aOwner;
end;

procedure TStockDataDistributorEventSink.OnNewData(const a_Symbol: WideString; a_Interval: Integer;
  a_DateTime: TDateTime; a_Open, a_High, a_Low, a_Close: Double; a_Volume: Integer);
var
  aData: TSCInputData;
  aD: ISCInputData;
  aTI: TStockTimeInterval;
begin
  if TStockDataUtils.GetTimeIntervalByValue(a_Interval,aTI) then
  begin
    TStockDataUtils.CheckTimeAligned(a_DateTime,aTI);

    aData:=TSCInputData.Create(nil);
    aData.DataDateTime:=a_DateTime;
    aData.DataOpen:=a_Open;
    aData.DataHigh:=a_High;
    aData.DataLow:=a_Low;
    aData.DataClose:=a_Close;
    aData.DataVolume:=a_Volume;

    aD:=aData; //Засада: Нужно переприсвоить так как параммер const
    StockDataStorage.UpdateBar(a_Symbol,aTI,aD);
    FOwner.FBroker.NewData(a_Symbol,aTI,aD);
  end;
end;

procedure TStockDataDistributorEventSink.OnCloseOrder(const aOrder: IStockServerOrder);
begin
  FOwner.FBroker.OnOpenOrder(FOwner.FBroker.GetOrderFromComOrder(aOrder));
end;

procedure TStockDataDistributorEventSink.OnOpenOrder(const aOrder: IStockServerOrder);
begin
  FOwner.FBroker.OnCloseOrder(FOwner.FBroker.GetOrderFromComOrder(aOrder));
end;

initialization
  StockBrokerConnectionRegistry.AddConnection(TStockBrokerConnectionTestServer.Create);

end.

