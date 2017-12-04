{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация брокера, работящего с TestServer См проект
            "X:\Trade\Forecaster\Stock Server"
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.TestServer.Broker;
{$I Compiler.inc}

interface
  uses Classes, BaseUtils, SysUtils,
  FC.Definitions,StockChart.Definitions,
  FC.Trade.Brokers.BrokerBase,
  FC.Trade.Brokers.TestServer.Order,
  StockServer_TLB;

type
  TStockBroker = class (TStockBrokerBase,IStockBroker,IStockBrokerTestServerSupport)
  private
    FBrokerNative: IStockServerBroker;
    FOrders      : TList;

    procedure AddOrder(aOrder: TStockOrder);
    procedure DeleteOrder(index: integer);
    procedure ClearOrders;
    function  GetOrder(index:integer): TStockOrder;


    //Доступ к COM-объекту сервера
    function GetNative: IStockServerBroker;
  public
    //IStockObjct
    function GetHashCode: integer;

    //from IStockBroker
    function GetDeposit: TStockRealNumber;
    function GetCurrentPrice(aKind: TStockBrokerPriceKind): TStockRealNumber;
    function GetCurrentTime: TDateTime;

    function GetMargin: integer;

    //Получение информации о параметрах торговли
    function GetMarketInfo:TStockMarketInfo;

    //Создать пустой ордер
    function CreateOrder(aTrader: IStockTrader): IStockOrder;

    //Дать все ордера у брокера (закрытые, открытые...)
    function GetAllOrders: IStockOrderCollection;

    //Дать все текущие открытые у брокера ордера
    function GetOpenedOrders: IStockOrderCollection;

    //Дать все отложенные ордера (стоповые и лимитные)
    function GetPendingOrders: IStockOrderCollection;
    //end of IStockBroker

    procedure NewData(const aSymbol: string; aInterval: TStockTimeInterval; const aData: ISCInputData);

    procedure OnCloseOrder(aOrder: IStockOrder);
    procedure OnOpenOrder(aOrder: IStockOrder);

    function  GetOrderFromComOrder(const aOrder:IStockServerOrder): IStockOrder;

    constructor Create(const aBrokerNative: IStockServerBroker);
    destructor Destroy; override;
  end;



implementation
  uses Math,FC.DataUtils,FC.Trade.OrderCollection;

{ TStockBroker }

constructor TStockBroker.Create(const aBrokerNative: IStockServerBroker);
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

function TStockBroker.GetHashCode: integer;
begin
  result:=integer(self);
end;

function TStockBroker.GetCurrentPrice(aKind: TStockBrokerPriceKind): TStockRealNumber;
begin
  if aKind=bpkBid then
    result:=GetNative.GetCurrentBidPrice
  else
    result:=GetNative.GetCurrentAskPrice;

  Assert(result>0);
end;

procedure TStockBroker.OnCloseOrder(aOrder: IStockOrder);
begin
  RaiseOnCloseOrderEvent(aOrder);
end;

procedure TStockBroker.OnOpenOrder(aOrder: IStockOrder);
begin
  RaiseOnOpenOrderEvent(aOrder);
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

function TStockBroker.GetNative: IStockServerBroker;
begin
  if FBrokerNative=nil then
    raise EAssertionFailed.Create('There is no active server');

  result:=FBrokerNative;
end;

procedure TStockBroker.NewData(const aSymbol: string; aInterval: TStockTimeInterval; const aData: ISCInputData);
begin
  RaiseOnNewDataEvent(aSymbol,aInterval,aData);
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

function TStockBroker.GetDeposit: TStockRealNumber;
begin
  result:=GetNative.GetDeposit;
end;

function TStockBroker.GetMargin: integer;
begin
  result:=Round(GetNative.GetMargin);
end;

function TStockBroker.GetMarketInfo: TStockMarketInfo;
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

function TStockBroker.GetOrderFromComOrder(const aOrder: IStockServerOrder): IStockOrder;
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

