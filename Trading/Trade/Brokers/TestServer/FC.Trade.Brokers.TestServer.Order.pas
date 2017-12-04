{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация ордера для TestServer См проект
            "X:\Trade\Forecaster\Stock Server"

            Внимание
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.TestServer.Order;
{$I Compiler.inc}

interface
uses
  SysUtils,BaseUtils, StockChart,StockServer_TLB,
  Serialization, FC.Definitions,StockChart.Definitions;

type

  //Специальное расширение для поддержки нас
  IStockBrokerTestServerSupport = interface
  ['{CAFBB2D1-1E2C-499E-A585-99EB8FCEF041}']
    function GetNative: IStockServerBroker;
  end;

 { TStockOrder }

  TStockOrder = class (TInterfacedObject,IStockOrder)
  private
    FTraderID : TGUID;
    FBroker   : IStockBroker; //TStockBroker
    FID       : TGUID;
    FNative   : IStockServerOrder;
  public
    //Уникальный идентификатор ордера
    function  GetID: TGUID;

    //Под каким брокером выдан ордер
    function GetBroker: IStockBroker;

    //Какой трейдер создал ордер. Cсылку сохранить нельзя, сохраняем ID
    function GetTraderID: TGUID;

    //Тип ордера - на покупку или продажу
    function GetKind : TStockOrderKind;

    //Текущее состояние - пустой, открыт, закрыт
    function GetState: TStockOrderState;

    //Кол-во лотов. Rate = 1 означает 0.1 лотов
    function GetRate: integer;

    //Атрибуты открытия
    function GetOpenTime : TDateTime;
    function GetOpenPrice: TStockRealNumber;
    function GetOpenComment: string;

    //Атрибуты закрытия
    function GetCloseTime : TDateTime;
    function GetClosePrice: TStockRealNumber;
    function GetCloseComment: string;

    //Худшее состояние за все время ордера
    function GetWorstProfit: TStockRealNumber;

    //максимальный профит, который был за все время ордера
    function GetBestProfit: TStockRealNumber;

    //Дать текущий профит
    function GetCurrentProfit: TStockRealNumber;

    //Установить уровень StopLoss (в реальных ценовых единицах)
    procedure SetTrailingStop(aPriceDelta: TStockRealNumber);
    function  GetTrailingStop:TStockRealNumber;

    //Установить уровень StopLoss (в пунктах)
    procedure SetStopLoss(aPrice: TStockRealNumber);
    function  GetStopLoss:TStockRealNumber;

    //Установить уровень TakeProfit (в пунктах)
    procedure SetTakeProfit(aPrice: TStockRealNumber);
    function  GetTakeProfit:TStockRealNumber;

    //Закрыть по текущей цене
    procedure Close(const aComment: string);

    //Открыть ордер по текущей цене
    procedure Open(aKind: TStockOrderKind; aRate: integer);

    //Открыть ордер по указанной цене
    //aRate указывает отношение pt:$, отношение 1:1 означает 0.1 лота
    procedure OpenAt(aKind: TStockOrderKind; aPrice: TStockRealNumber; aRate: integer);

    //Получить затребованную цену открытия (см. OpenAt)
    //Эту функцию имеет смысл только, если ордер открывался через OpenAt. В противном случае будет выброшено исключение
    function GetPendingOpenPrice: TStockRealNumber;

    //Получить тип отложенного ордера (лимитный или стоповый)
    //Эту функцию имеет смысл только, если ордер открывался через OpenAt. В противном случае будет выброшено исключение
    function GetPendingType: TStockOrderPendingType;

    //Implementation
    procedure Dispose;

    property  Native:IStockServerOrder read FNative;

    constructor Create(const aStockBroker: IStockBrokerTestServerSupport; const aStockTrader: IStockTrader);
  end;

implementation
  uses Math,FC.Trade.Brokers.TestServer.Broker;


{ TStockOrder }

constructor TStockOrder.Create(const aStockBroker: IStockBrokerTestServerSupport; const aStockTrader: IStockTrader);
begin
  FTraderID:=aStockTrader.GetID;
  FBroker:=aStockBroker as IStockBroker;
  FNative:=aStockBroker.GetNative.CreateOrder;

  CreateGUID(FID);
end;

procedure TStockOrder.Dispose;
begin
  FBroker:=nil;
end;

function TStockOrder.GetOpenTime: TDateTime;
begin
  result:=FNative.GetOpenTime;
end;

function TStockOrder.GetPendingOpenPrice: TStockRealNumber;
begin
  //Если открывались не через OpenAt
  raise ENotSupported.Create;
  //if FNative.GetQueriedOpenPrice=0 then
  //  raise EStockError.Create('Order is not pending');

  //result:=FNative.GetQueriedOpenPrice;
end;

function TStockOrder.GetPendingType: TStockOrderPendingType;
begin
  raise ENotSupported.Create;
end;

function TStockOrder.GetKind: TStockOrderKind;
begin
  result:=TStockOrderKind(FNative.GetKind);
end;

function TStockOrder.GetTakeProfit: TStockRealNumber;
begin
  result:=FNative.GetTakeProfit;
end;

function TStockOrder.GetTraderID: TGUID;
begin
  result:=FTraderID;
end;

function TStockOrder.GetTrailingStop: TStockRealNumber;
begin
  result:=FNative.GetTrailingStop;
end;

procedure TStockOrder.SetStopLoss(aPrice: TStockRealNumber);
begin
  FNative.SetStopLoss(aPrice);
end;

procedure TStockOrder.SetTakeProfit(aPrice: TStockRealNumber);
begin
  FNative.SetTakeProfit(aPrice);
end;

procedure TStockOrder.SetTrailingStop(aPriceDelta: TStockRealNumber);
begin
  FNative.SetTrailingStop(aPriceDelta);
end;

function TStockOrder.GetOpenComment: string;
begin
  result:=FNative.GetOpenComment;
end;

function TStockOrder.GetOpenPrice: TStockRealNumber;
begin
  result:=FNative.GetOpenPrice;
end;

function TStockOrder.GetCloseTime: TDateTime;
begin
  result:=FNative.GetCloseTime;
end;

function TStockOrder.GetClosePrice: TStockRealNumber;
begin
  result:=FNative.GetClosePrice;
end;

procedure TStockOrder.Close(const aComment: string);
begin
  FNative.Close(aComment);
end;

procedure TStockOrder.Open(aKind: TStockOrderKind; aRate: integer);
begin
  FNative.Open(integer(aKind),aRate);
end;

procedure TStockOrder.OpenAt(aKind: TStockOrderKind; aPrice: TStockRealNumber; aRate: integer);
begin
  FNative.OpenAt(integer(aKind),aPrice,aRate);
end;

function TStockOrder.GetID: TGUID;
begin
  result:=FID;
end;

function TStockOrder.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockOrder.GetState: TStockOrderState;
begin
  result:=TStockOrderState(FNative.GetState);
end;

function TStockOrder.GetStopLoss: TStockRealNumber;
begin
  result:=FNative.GetStopLoss;
end;

function TStockOrder.GetCurrentProfit: TStockRealNumber;
begin
  result:=FNative.GetCurrentProfit;
end;

function TStockOrder.GetCloseComment: string;
begin
  result:=FNative.GetCloseComment;
end;

function TStockOrder.GetRate: integer;
begin
  result:=FNative.GetRate;
end;

function TStockOrder.GetWorstProfit: TStockRealNumber;
begin
  result:=-1;
end;

function TStockOrder.GetBestProfit: TStockRealNumber;
begin
  result:=-1;
end;


end.

