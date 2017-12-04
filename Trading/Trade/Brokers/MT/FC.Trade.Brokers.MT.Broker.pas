{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация брокера, работающего через терминал MT
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.MT.Broker;
{$I Compiler.inc}

interface
  uses Classes, BaseUtils, SysUtils, StockChart.Obj,
  FC.Definitions,StockChart.Definitions,
  FC.Trade.Brokers.BrokerBase,
  FC.Trade.Brokers.MT.Order,
  MTT_TLB,FC.Singletons;

type
  TStockBroker = class (TStockBrokerBase,IStockBroker,IStockBrokerMTSupport)
  private
    FBrokerNative: IMTTBroker;
    FOrders      : TList;

    procedure AddOrder(aOrder: TStockOrder);
    procedure DeleteOrder(index: integer);
    procedure ClearOrders;
    function  GetOrder(index:integer): TStockOrder;
    //from IMTTBrokerClient
  public
    //Доступ к COM-объекту сервера
    function GetNative: IMTTBroker;
    procedure SetNative(aBroker: IMTTBroker);

    //from IStockBroker

    //Идентификация Account
    function GetAccount: string;
    function GetEquity: TStockRealNumber;
    function GetBalance: TStockRealNumber;
    function GetCurrentPrice(const aSymbol: string; aKind: TStockBrokerPriceKind): TStockRealNumber;
    function GetCurrentTime: TDateTime;

    function IsRealTime: boolean;

    function GetMargin: integer;

    //Получение информации о параметрах торговли
    function GetMarketInfo(const aSymbol: string):TStockMarketInfo;

    //Создать пустой ордер
    function CreateOrder(aTrader: IStockTrader): IStockOrder;

    //Дать все ордера у брокера (закрытые, открытые...)
    function GetAllOrders: IStockOrderCollection;

    //Дать все текущие открытые у брокера ордера
    function GetOpenedOrders: IStockOrderCollection;

    //Дать все отложенные ордера (стоповые и лимитные)
    function GetPendingOrders: IStockOrderCollection;

     //Кол-во точек после запятой
    function GetPricePrecision(const aSymbol: string) : integer; override;
    //Коэффициент трансформации из точки в цену, для EURUSD=10000, для USDJPY=100
    function GetPricesInPoint(const aSymbol: string): integer; override;
    //end of IStockBroker

    //from IStockBrokerMTSupport
    procedure OnModifyOrder(aOrder: TStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    //end of IStockBrokerMTSupport

    procedure OnNewData(const aSymbol: string);

    function  GetOrderFromComOrder(const aOrder:IMTTOrder): IStockOrder;

    constructor Create(const aBrokerNative: IMTTBroker);
    destructor Destroy; override;
  end;



implementation
  uses Math,FC.DataUtils,FC.Trade.OrderCollection,Application.Definitions;

{ TStockBroker }

constructor TStockBroker.Create(const aBrokerNative: IMTTBroker);
begin
  inherited Create;
  FOrders:=TList.Create;
  FBrokerNative:=aBrokerNative;
end;

destructor TStockBroker.Destroy;
begin
  inherited;
  ClearOrders;
  FreeAndNil(FOrders);

  FBrokerNative:=nil;
end;

function TStockBroker.GetCurrentPrice(const aSymbol: string; aKind: TStockBrokerPriceKind): TStockRealNumber;
begin
  if aKind=bpkBid then
    result:=GetNative.GetCurrentBidPrice(aSymbol)
  else
    result:=GetNative.GetCurrentAskPrice(aSymbol);

  Assert(result>0);
end;

function TStockBroker.CreateOrder(aTrader: IStockTrader): IStockOrder;
var
  aOrder: TStockOrder;
begin
  aOrder:=TStockOrder.Create(self,aTrader);
  AddOrder(aOrder);
  result:=aOrder;
end;

function TStockBroker.GetOpenedOrders: IStockOrderCollection;
var
  aCollection: TStockOrderCollection;
  i: integer;
begin
  aCollection:=TStockOrderCollection.Create();
  for i := 0 to FOrders.Count - 1 do
    if GetOrder(i).GetState=osOpened then
      aCollection.Add(GetOrder(i));
  result:=aCollection;
end;

function TStockBroker.GetPendingOrders: IStockOrderCollection;
var
  aCollection: TStockOrderCollection;
  i: integer;
begin
  aCollection:=TStockOrderCollection.Create();
  for i := 0 to FOrders.Count - 1 do
    if (GetOrder(i).GetState=osPending) then
      aCollection.Add(GetOrder(i));
  result:=aCollection;
end;

function TStockBroker.IsRealTime: boolean;
begin
  result:=true;
end;

function TStockBroker.GetNative: IMTTBroker;
begin
  if FBrokerNative=nil then
    raise EAssertionFailed.Create('There is no active server');

  result:=FBrokerNative;
end;

procedure TStockBroker.OnModifyOrder(aOrder: TStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  Workspace.MainFrame.ShowTrayBalloonHint('Modify on '+DateTimeToStr(GetCurrentTime),StockOrderModifyTypeNames[aModifyEventArgs.ModifyType],btInfo,1000*60*60,true);
end;

procedure TStockBroker.OnNewData(const aSymbol: string);
begin
  RaiseOnNewDataEvent(aSymbol) ;
end;


function TStockBroker.GetPricePrecision(const aSymbol: string): integer;
begin
  //TODO переделать
  result:=TKnownSymbolRegistry.GetPricePrecision(aSymbol);
end;

function TStockBroker.GetPricesInPoint(const aSymbol: string): integer;
begin
  //TODO переделать
  result:=TKnownSymbolRegistry.GetPricesInPoint(aSymbol);
end;

procedure TStockBroker.SetNative(aBroker: IMTTBroker);
begin
  FBrokerNative:=aBroker;
end;

function TStockBroker.GetAccount: string;
begin
  result:=GetNative.GetAccount;
end;

function TStockBroker.GetAllOrders: IStockOrderCollection;
var
  aCollection: TStockOrderCollection;
  i: integer;
begin
  aCollection:=TStockOrderCollection.Create();
  for i := 0 to FOrders.Count - 1 do
    aCollection.Add(GetOrder(i));

  result:=aCollection;
end;

function TStockBroker.GetCurrentTime: TDateTime;
begin
  result:=GetNative.GetCurrentTime;
end;

function TStockBroker.GetBalance: TStockRealNumber;
begin
  result:=GetNative.GetDeposit;
end;

function TStockBroker.GetEquity: TStockRealNumber;
begin
  raise ENotSupported.Create;
end;

function TStockBroker.GetMargin: integer;
begin
  result:=Round(GetNative.GetMargin);
end;

function TStockBroker.GetMarketInfo(const aSymbol: string): TStockMarketInfo;
begin
  FillChar(result,sizeof(result),0);
  result.Spread:=3; //TODO
  result.StopLevel:=3; //TODO
end;

procedure TStockBroker.AddOrder(aOrder: TStockOrder);
begin
  IInterface(aOrder)._AddRef;
  FOrders.Add(aOrder);
end;

procedure TStockBroker.DeleteOrder(index: integer);
begin
  TStockOrder(FOrders[index]).Dispose;
  IInterface(TStockOrder(FOrders[index]))._Release;
  FOrders.Delete(index);
end;

procedure TStockBroker.ClearOrders;
begin
  while FOrders.Count>0 do
    DeleteOrder(0);
end;

function TStockBroker.GetOrder(index: integer): TStockOrder;
begin
  result:=FOrders[index];
end;

function TStockBroker.GetOrderFromComOrder(const aOrder: IMTTOrder): IStockOrder;
var
  i: integer;
begin
  for I :=FOrders.Count - 1 downto 0 do
  begin
    if GetOrder(i).Native=aOrder then
    begin
      result:=GetOrder(i);
      exit;
    end;
  end;

  raise EAlgoError.Create;
end;

end.

