{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   �������� ���������� ���������� � ��������. � ������ ������ ������������
            COM-������, ������������� � ���������� MetaTrader
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.MT.BrokerConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,Windows,BaseUtils, MTT_TLB,
  FC.Definitions, Documents.Definitions,
  FC.Trade.BrokerConnection.Base,
  FC.Trade.Brokers.MT.Broker,
  FC.Factory;

type
  //�����, ����������� ������� ���������� � ������� ��������. ��������� ��������� IStockBrokerConnection
  TStockBrokerConnectionMT = class (TStockBrokerConnectionBase)
  private
    FMTTServer    : IMTTServer;
    FBroker       : TStockBroker;

    procedure Disconnect;
  public
    function  GetName: string; override;
    function  GetDescription: string; override;
    function  GetBroker: IStockBroker; override;

    procedure Enable(aValue:boolean); override;
    destructor Destroy; override;
  end;

  //COM-������, ����� ������� ���������� ����� � COM-��������. ��. IMTTClient
  TBrokerClient = class (TInterfacedObject,IMTTClient)
  private
    FSymbols: TStockSymbolInfoArray;
    FOwner: TStockBrokerConnectionMT;
  public
    //from IMTTClient
    procedure Ping; safecall;
    procedure OnSetQuotation(const a_Symbol: WideString; a_Time: TDateTime; a_Bid: Double; a_Ask: Double; a_SavedToHistoryDB: WordBool); safecall;
    procedure OnNewData(const a_Symbol: WideString; a_IntervalType: Integer; a_Data: OleVariant; a_SavedToHistoryDB: WordBool); safecall;

    procedure OnOpenOrder(const aOrder: IMTTOrder); safecall;
    procedure OnCloseOrder(const aOrder: IMTTOrder); safecall;

    function  GetLastM1Bar(const a_Symbol: WideString): TDateTime; safecall;
    function  IsListenedSymbol(const a_Symbol:WideString):WordBool; safecall;
    //end of IMTTClient

    constructor Create(aOwner: TStockBrokerConnectionMT);
  end;

implementation
  uses ActiveX,ComObj,DB,Math,DateUtils, SystemService,
  StockChart.Obj,StockChart.Definitions,
  FC.Singletons,FC.DataUtils;

{ TStockBrokerConnectionMT }

destructor TStockBrokerConnectionMT.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TStockBrokerConnectionMT.Disconnect;
begin
  try
    if (FMTTServer<>nil) and (FBroker<>nil) and (FBroker.GetNative<>nil) then
    begin
      FBroker.GetNative.SetClient(nil); //����������� ���� ����������
      //������������ ����������
      FMTTServer.DestroyConnection(FBroker.GetNative);
      //�������� �������, �������, ��� ���������� ������ ���
      FBroker.SetNative(nil);
    end;
  except
    on E:EOleException do
      ;
  end;

  if FBroker<>nil then
  begin
    IInterface(FBroker)._Release;
    FBroker:=nil;
  end;

  FMTTServer:=nil;
end;

procedure TStockBrokerConnectionMT.Enable(aValue: boolean);
var
  unk: IInterface;
  aMTTBroker: IMTTBroker;
begin
  if (FMTTServer<>nil) and aValue then
    exit;

  TWaitCursor.SetUntilIdle;
  Disconnect;

  if aValue then
  begin
    if not Succeeded(GetActiveObject(CLASS_MTTServer, nil, unk)) then
       unk := CreateComObject(CLASS_MTTServer);

    FMTTServer:=unk as IMTTServer;

    //������� ����� ����� �������� � ����
    aMTTBroker:=FMTTServer.CreateConnection;
    //��������� ������� � �������� �����������
    aMTTBroker.SetClient(TBrokerClient.Create(self));

    //������� �������� �������, ��� ������� � ����������� ������������
    FBroker:=TStockBroker.Create(aMTTBroker);
    IInterface(FBroker)._AddRef;
  end;
end;

function TStockBrokerConnectionMT.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockBrokerConnectionMT.GetDescription: string;
begin
  result:='Provides connection to the server via MetaTrader client terminal.'
end;

function TStockBrokerConnectionMT.GetName: string;
begin
  result:='MetaTrader Terminal';
end;

{ TBrokerClient }

constructor TBrokerClient.Create(aOwner: TStockBrokerConnectionMT);
begin
  FOwner:=aOwner;
  FSymbols:=StockDataStorage.GetSymbols;
end;

procedure TBrokerClient.OnNewData(const a_Symbol: WideString; a_IntervalType: Integer; a_Data: OleVariant; a_SavedToHistoryDB: WordBool);
var
  aInputDataCollection : ISCInputDataCollection;
begin
  aInputDataCollection:=TStockDataUtils.BinaryDataToInputData(a_Data);

  if aInputDataCollection.Count=0 then //�� ������ ������
    exit;

  if not a_SavedToHistoryDB then
    StockDataStorage.UpdateBars(a_Symbol,TStockTimeInterval(a_IntervalType),aInputDataCollection)
  else
    StockDataStorage.OnBarsAddedOutside(a_Symbol,TStockTimeInterval(a_IntervalType),aInputDataCollection);

  FOwner.FBroker.OnNewData(a_Symbol);
end;

function TBrokerClient.GetLastM1Bar(const a_Symbol: WideString): TDateTime;
begin
  result:=StockDataStorage.GetLastDateTime(a_Symbol,sti1);
end;

function  TBrokerClient.IsListenedSymbol(const a_Symbol:WideString):WordBool;
var
  i: integer;
begin
  result:=false;

  for i := 0 to High(FSymbols) do
  begin
    if AnsiSameText(a_Symbol,FSymbols[i].Name) then
    begin
      result:=true;
      exit;
    end;
  end;
end;

procedure TBrokerClient.OnCloseOrder(const aOrder: IMTTOrder);
begin
  //FOwner.FBroker.OnOpenOrder(FOwner.FBroker.GetOrderFromComOrder(aOrder));
end;

procedure TBrokerClient.OnOpenOrder(const aOrder: IMTTOrder);
begin
  //FOwner.FBroker.OnCloseOrder(FOwner.FBroker.GetOrderFromComOrder(aOrder));
end;

procedure TBrokerClient.OnSetQuotation(const a_Symbol: WideString; a_Time: TDateTime; a_Bid, a_Ask: Double;a_SavedToHistoryDB: WordBool);
begin
  //��������� ������ � ��� ������, ���� �������� �� ��������
  if not a_SavedToHistoryDB then
    if IsListenedSymbol(a_Symbol) then
      StockDataStorage.AddTick(a_Symbol,a_Time,a_Bid);   //�������� Bid � �������� ��������
end;

procedure TBrokerClient.Ping;
begin

end;

initialization
  StockBrokerConnectionRegistry.AddConnection(TStockBrokerConnectionMT.Create);

end.

