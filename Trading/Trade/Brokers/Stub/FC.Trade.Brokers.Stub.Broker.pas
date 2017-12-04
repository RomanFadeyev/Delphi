{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный класc, реализующий заглушку для брокера, используется
            тестером для эмуляции торгов по истории
 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Brokers.Stub.Broker;
{$I Compiler.inc}

interface
  uses Windows, Classes, BaseUtils, SysUtils,
  FC.Definitions,StockChart.Definitions,
  FC.Trade.Brokers.BrokerBase,
  FC.Trade.Brokers.Stub.Order,Contnrs;

type
  TBrokerGettingPriceType = (gptMedian,gptPessimistic,gptUltraPessimistic, gptOptimistic, gptUltraOptimistic,gptClose,gptOpen);
const
  TraderGetPriceTypeNames : array [TBrokerGettingPriceType] of string =
  ('Median','Pessimistic','Ultra Pessimistic', 'Optimistic', 'Ultra Optimistic','Close','Open');

type
  TStockBroker = class (TStockBrokerBase,IStockBroker,IStockBrokerStubSupport)
  private
    FDeposit: TStockRealNumber;
    FSpread : TStockRealNumber;
    FSymbol : TStockSymbolInfo;
    FStopLevel   : integer; //Минимальный отступ от маркет-цены при установке SL, TP или PendingOrders
    FRealTime : boolean;


    FMinutes : ISCInputDataCollection;
    FTickCacheStart,FTickCacheStop: TDateTime;
    FEmulateTickInMinute : boolean;
    FGettingPriceType : TBrokerGettingPriceType;

    FCurrentMinuteTicks: IStockTickCollectionWriteable;
    FCurrentTickIndex: integer;
    FCurrentTimeIndex: integer;
    FCurrentBid,FCurrentAsk: TStockRealNumber;
    FCurrentTime           : TDateTime;

    FOrders      : TList;

    procedure SetGettingPriceType(const Value: TBrokerGettingPriceType);

    procedure AddOrder(aOrder: TStockOrder);
    procedure DeleteOrder(index: integer);
    procedure ClearOrders;
    function  GetOrder(index:integer): TStockOrder; inline;
  public
    //IStockObjct

    //from IStockBroker

    //Идентификация Account
    function GetAccount: string;

    function GetBalance: TStockRealNumber;
    function GetEquity: TStockRealNumber;
    function GetCurrentPrice(const aSymbol: string; aKind: TStockBrokerPriceKind): TStockRealNumber;
    function GetCurrentTime: TDateTime;

    function GetMargin: integer;

    function IsRealTime: boolean;

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

    //from IBrokerStubExtension
    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    //end of IBrokerStubExtension

    procedure SetDeposit(const aValue: TStockRealNumber);
    procedure SetSpread(const aValue: integer);
    procedure SetStopLevel(const aValue: integer);
    procedure SetParams(const aSymbol:TStockSymbolInfo; const a1MinBarCollection: ISCInputDataCollection;  const aStart,aStop: TDateTime; aEmulateTickInMinute: boolean);

    procedure Reset;
    procedure StartTicking;
    function  Tick: boolean;
    procedure AfterTick;

    property  CurrentTime: TDateTime read GetCurrentTime;
    property  CurrentBidPrice: TStockRealNumber read FCurrentBid;
    property  CurrentAskPrice: TStockRealNumber read FCurrentAsk;
    property  GettingPriceType : TBrokerGettingPriceType read FGettingPriceType write SetGettingPriceType;
    property  RealTime: boolean read FRealTime write FRealTime;

    constructor Create;
    destructor Destroy; override;
  end;



implementation
  uses Math,FC.DataUtils,FC.Trade.OrderCollection, FC.StockData.StockTickCollection,FC.Singletons;

{ TStockBroker }

procedure TStockBroker.AfterTick;
begin
  RaiseOnNewDataEvent(FSymbol.Name);
end;

procedure TStockBroker.ClearOrders;
begin
  while FOrders.Count>0 do
    DeleteOrder(0);
end;

constructor TStockBroker.Create;
begin
  inherited Create;
  FCurrentMinuteTicks:=TStockTickCollection.Create;
  FGettingPriceType:=gptPessimistic;
  FOrders:=TList.Create;
end;

destructor TStockBroker.Destroy;
begin
  inherited;
  FMinutes:=nil;;
  FCurrentMinuteTicks:=nil;
  ClearOrders;
end;

function TStockBroker.CreateOrder(aTrader: IStockTrader): IStockOrder;
var
  aOrder: TStockOrder;
begin
  aOrder:=TStockOrder.Create(self,aTrader);
  AddOrder(aOrder);
  RaiseOnNewOrderEvent(aOrder);
  result:=aOrder;
end;

procedure TStockBroker.DeleteOrder(index: integer);
begin
  TStockOrder(FOrders[index]).Dispose;
  IInterface(TStockOrder(FOrders[index]))._Release;
  FOrders.Delete(index);
end;

function TStockBroker.GetBalance: TStockRealNumber;
begin
  result:=FDeposit;
end;

function TStockBroker.GetEquity: TStockRealNumber;
var
  i: integer;
  aDelta : TStockRealNumber;
  aOrder: TStockOrder;
begin
  aDelta:=0;
  for i := 0 to  FOrders.Count - 1 do
  begin
    aOrder:=GetOrder(i);
    if aOrder.GetState=osOpened then
      aDelta:=aDelta+PriceToMoney(aOrder.GetSymbol,aOrder.GetCurrentProfit,aOrder.GetLots);
  end;
  result:=aDelta+FDeposit;
end;

procedure TStockBroker.SetDeposit(const aValue: TStockRealNumber);
begin
  FDeposit:=aValue;
end;

procedure TStockBroker.SetSpread(const aValue: integer);
begin
  FSpread:=PointToPrice(FSymbol.Name,aValue);
end;

procedure TStockBroker.SetStopLevel(const aValue: integer);
begin
  FStopLevel:=aValue;
end;

function TStockBroker.GetCurrentPrice(const aSymbol: string; aKind: TStockBrokerPriceKind): TStockRealNumber;
begin
  if not AnsiSameText(aSymbol,FSymbol.Name) then
    raise EStockError.Create('Unsupported symbol');

  if aKind=bpkAsk then
    result:=FCurrentAsk
  else
    result:=FCurrentBid;

  Assert(result>0);
end;

procedure TStockBroker.OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
var
  aProfitMoney : TStockRealNumber;
begin
  if aModifyEventArgs.ModifyType=omtClose then
  begin
    aProfitMoney:=PriceToMoney(aOrder.GetSymbol,aOrder.GetCurrentProfit,aOrder.GetLots);
    FDeposit:=FDeposit+aProfitMoney;
  end;


  RaiseOnModifyOrderEvent(aOrder,aModifyEventArgs);
end;

procedure TStockBroker.Reset;
begin
  ClearOrders;
end;

procedure TStockBroker.AddOrder(aOrder: TStockOrder);
begin
  IInterface(aOrder)._AddRef;
  FOrders.Add(aOrder);
end;

procedure TStockBroker.SetParams(const aSymbol:TStockSymbolInfo; const a1MinBarCollection: ISCInputDataCollection; const aStart,aStop: TDateTime; aEmulateTickInMinute: boolean);
begin
  FSymbol:=aSymbol;
  FMinutes:=a1MinBarCollection;
  FTickCacheStart:=aStart;
  FTickCacheStop:=aStop;
  FEmulateTickInMinute:=aEmulateTickInMinute;
end;

procedure TStockBroker.SetGettingPriceType(const Value: TBrokerGettingPriceType);
begin
  if FGettingPriceType<>Value then
  begin
    FGettingPriceType := Value;
    FTickCacheStart:=-1;
    FTickCacheStop:=-1;
  end;
end;

function TStockBroker.Tick: boolean;
var
  i: integer;
  aResultAsk: TStockRealNumber;
  aResultBid: TStockRealNumber;
  aDataLow,aDataHigh: TStockRealNumber;
begin
  result:=false;

  //Если режим тиков внутри минутки, и при этом в этой минутке еще есть
  //тики, берем их
  inc(FCurrentTickIndex);
  if FEmulateTickInMinute and (FCurrentTickIndex<FCurrentMinuteTicks.Count) then
  begin
    FCurrentBid:=FCurrentMinuteTicks.GetValue(FCurrentTickIndex);
    FCurrentAsk:=FCurrentBid+FSpread;
    FCurrentTime:=FCurrentMinuteTicks.GetDateTime(FCurrentTickIndex);

    for i:=0 to FOrders.Count-1 do
      GetOrder(i).Tick;

    result:=true;
    exit;
  end;

  inc(FCurrentTimeIndex);
  if FCurrentTimeIndex>=FMinutes.Count then
    exit;

  FCurrentTime:=FMinutes.DirectGetItem_DataDateTime(FCurrentTimeIndex);

  if FCurrentTime>FTickCacheStop then
    exit;

  if not FEmulateTickInMinute then
  begin
    aDataLow:=FMinutes.DirectGetItem_DataLow(FCurrentTimeIndex);
    aDataHigh:=FMinutes.DirectGetItem_DataHigh(FCurrentTimeIndex);

    case FGettingPriceType of
      gptMedian: begin
        aResultAsk:=(aDataHigh+aDataLow)/2;
        aResultBid:=aResultAsk;
      end;
      gptPessimistic: begin
        aResultAsk:=Max(aDataHigh,aDataLow)-(aDataHigh-aDataLow)/4;
        aResultBid:=Min(aDataHigh,aDataLow)+(aDataHigh-aDataLow)/4;
      end;
      gptUltraPessimistic: begin
        aResultAsk:=Max(aDataHigh,aDataLow);
        aResultBid:=Min(aDataHigh,aDataLow);
      end;
      gptOptimistic: begin
        aResultBid:=Max(aDataHigh,aDataLow)-(aDataHigh-aDataLow)/4;
        aResultAsk:=Min(aDataHigh,aDataLow)+(aDataHigh-aDataLow)/4;
      end;
      gptUltraOptimistic: begin
        aResultBid:=Max(aDataHigh,aDataLow);
        aResultAsk:=Min(aDataHigh,aDataLow);
      end;
      gptClose: begin
        aResultBid:=FMinutes.DirectGetItem_DataClose(FCurrentTimeIndex);
        aResultAsk:=aResultBid;
      end;
      gptOpen: begin
        aResultBid:=FMinutes.DirectGetItem_DataOpen(FCurrentTimeIndex);
        aResultAsk:=aResultBid;
      end;
      else
        raise EAlgoError.Create;
    end;

    //07.08.06 Так как общепринятым является хранение котировок по Bid, то мы добавляем Spread к Ask
    FCurrentAsk:=RoundPrice(FSymbol.Name, aResultAsk+FSpread);
    FCurrentBid:=RoundPrice(FSymbol.Name, aResultBid);
  end

  //Эмулируем тики
  else begin
    FCurrentMinuteTicks.Clear;
    TStockDataUtils.GenerateTicksInBar(FMinutes,FCurrentTimeIndex,FCurrentMinuteTicks);
    Assert(FCurrentMinuteTicks.Count>0);
    FCurrentTickIndex:=0;
    FCurrentTime:=FCurrentMinuteTicks.GetDateTime(FCurrentTickIndex);
    FCurrentBid:=RoundPrice(FSymbol.Name,FCurrentMinuteTicks.GetValue(FCurrentTickIndex));
    FCurrentAsk:=RoundPrice(FSymbol.Name,FCurrentBid+FSpread);
  end;

  for i:=0 to FOrders.Count-1 do
    GetOrder(i).Tick;

  result:=true;
end;

procedure TStockBroker.StartTicking;
begin
  if FMinutes.Count=0 then
   FCurrentTimeIndex:=0
  else begin
    //Находим начальную дату
    if FTickCacheStart<=FMinutes.DirectGetItem_DataDateTime(0) then
      FCurrentTimeIndex:=0
    else
      FCurrentTimeIndex:=FMinutes.FindBestMatched(FTickCacheStart);

    if FCurrentTimeIndex=-1 then
      FCurrentTimeIndex:=FMinutes.Count;
  end;

  dec(FCurrentTimeIndex);
  FCurrentMinuteTicks.Clear;
  FCurrentTickIndex:=0;
  RaiseOnStartEvent;
end;

function TStockBroker.GetMargin: integer;
begin
  result:=100;
end;

function TStockBroker.GetMarketInfo(const aSymbol: string): TStockMarketInfo;
begin
  FillChar(result,sizeof(result),0);
  result.Spread:=PriceToPoint(FSymbol.Name,FSpread);
  result.StopLevel:=FStopLevel;
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

function TStockBroker.GetPricePrecision(const aSymbol: string): integer;
begin
  result:=FSymbol.Digits;
end;

function TStockBroker.GetPricesInPoint(const aSymbol: string): integer;
begin
  result:=Round(1/FSymbol.Point);
end;

function TStockBroker.IsRealTime: boolean;
begin
  result:=FRealTime;
end;

function TStockBroker.GetAccount: string;
begin
  result:='';
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

function TStockBroker.GetOrder(index: integer): TStockOrder;
begin
  result:=FOrders[index];
end;

function TStockBroker.GetCurrentTime: TDateTime;
begin
  result:=FCurrentTime;
end;



end.

