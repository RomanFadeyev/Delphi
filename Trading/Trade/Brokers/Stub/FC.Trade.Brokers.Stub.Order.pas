unit FC.Trade.Brokers.Stub.Order;
{$I Compiler.inc}

interface
uses
  SysUtils,BaseUtils,Graphics,
  Serialization, FC.Definitions,FC.Trade.Brokers.BrokerBase,
  StockChart.Definitions,FC.Trade.Brokers.OrderBase;

type

  //Специальное расширение для поддержки брокеров-"заглушек"
  IStockBrokerStubSupport = interface
  ['{CAFBB2D1-1E2C-499E-A585-99EB8FCEF041}']
    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);

    //Добавить произволный комментарий. Время установи комментария берется текущее
    function AddMessage(const aOrder:IStockOrder; const aMessage: string; aColor: TColor=clDefault): IStockBrokerMessage;  overload;

    function  GetCurrentPrice(const aSymbol: string; aKind: TStockBrokerPriceKind): TStockRealNumber;
    function  GetCurrentTime: TDateTime;
  end;

 { TStockOrder }

  TStockOrder = class (TStockOrderBase)
  private
    FBrokerCallBack   : IStockBrokerStubSupport;
  protected
    procedure OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs); override;  
  public
    //Implementation
    procedure Tick;
    procedure Dispose; override;

    constructor Create(const aStockBroker: IStockBrokerStubSupport; const aStockTrader: IStockTrader); overload;
  end;

implementation
  uses Math;

{ TStockOrder }

constructor TStockOrder.Create(const aStockBroker: IStockBrokerStubSupport; const aStockTrader: IStockTrader);
begin
  inherited Create(aStockBroker as IStockBroker,aStockTrader);
  FBrokerCallBack:=aStockBroker;
end;

procedure TStockOrder.Dispose;
begin
  inherited;
  FBrokerCallBack:=nil;
end;

procedure TStockOrder.OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  inherited;
  FBrokerCallBack.OnModifyOrder(Self,aModifyEventArgs);
end;

procedure TStockOrder.Tick;
var
  aPrice: TStockRealNumber;
begin
  //Ордер уже открыт
  if GetState = osOpened then
  begin
    case GetKind of
      okBuy: begin
        aPrice:=FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkAsk);
        FWorstPrice:=min(FWorstPrice,aPrice);
        FBestPrice:=max(FBestPrice,aPrice);

        aPrice:=FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkBid);
        FCurrentProfit:=aPrice-GetOpenPrice;
        FWorstProfit:=min(FWorstProfit, FCurrentProfit);
        FBestProfit:=max(FBestProfit,FCurrentProfit);

        //Trailing Stop
        if (GetTrailingStop<>0) then
        begin
          //Если лучшая цена изменилась, то двигаем StopLoss ближе, так, чтобы
          //от лучшей цены до SL был наш TS
          if ((GetStopLoss=0) or (GetStopLoss<GetBestPrice-GetTrailingStop)) and
             (GetBestPrice-GetTrailingStop>=GetOpenPrice) then
            SetStopLossInternal(GetBestPrice-GetTrailingStop,true);
        end;

        //Stop Loss
        if (GetStopLoss<>0) and (aPrice<=GetStopLoss) then
          Close('Broker: Stop loss triggered')
        //Take profit
        else if (GetTakeProfit<>0) and (aPrice>=GetTakeProfit) then
          Close('Broker: Take profit triggered');
      end;
      okSell: begin
        aPrice:=FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkBid);
        FWorstPrice:=max(FWorstPrice,aPrice);
        FBestPrice:=min(FBestPrice,aPrice);

        aPrice:=FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkAsk);
        FCurrentProfit:=GetOpenPrice-aPrice;
        FWorstProfit:=min(FWorstProfit, FCurrentProfit);
        FBestProfit:=max(FBestProfit,FCurrentProfit);

        //Trailing Stop
        if (GetTrailingStop<>0) then
        begin
          //Если лучшая цена изменилась, то двигаем StopLoss ближе, так, чтобы
          //от лучшей цены до SL был наш TS
          if ((GetStopLoss=0) or (GetStopLoss>GetBestPrice+GetTrailingStop)) and
             (GetBestPrice+GetTrailingStop<=GetOpenPrice) then
            SetStopLossInternal(GetBestPrice+GetTrailingStop,true);
        end;

        //Stop Loss
        if (GetStopLoss<>0) and (aPrice>=GetStopLoss) then
          Close('Broker: Stop loss triggered')
        //Take profit
        else if (GetTakeProfit<>0) and (aPrice<=GetTakeProfit) then
          Close('Broker: Take profit triggered');
      end
      else
        raise EAlgoError.Create;
    end;
  end

  //Ордер не открыт, он отложенный
  else if (GetState=osPending) and (not IsPendingSuspended) then
  begin
    if (GetPendingExpirationTime<>0) and (FBrokerCallBack.GetCurrentTime>=GetPendingExpirationTime) then
      RevokePending
    else begin
      Assert(GetPendingOpenPrice<>0);

      //Стоповый ордер на покупку
      if (GetKind=okBuy) and (GetPendingType=ptStop) and (FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkAsk)>=GetPendingOpenPrice) then
        OpenInternal(GetSymbol, GetKind,GetLots,'Broker: Buy Stop order triggered')
      //Лимитный ордер на покупку
      else if (GetKind=okBuy) and (GetPendingType=ptLimit) and (FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkAsk)<=GetPendingOpenPrice) then
        OpenInternal(GetSymbol,GetKind,GetLots,'Broker: Buy Limit order triggered')
      //Стоповый ордер на продажу
      else if (GetKind=okSell) and (GetPendingType=ptStop) and (FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkBid)<=GetPendingOpenPrice) then
        OpenInternal(GetSymbol,GetKind,GetLots,'Broker: Sell Stop order triggered')
      //Лимитный ордер на продажу
      else if (GetKind=okSell) and (GetPendingType=ptLimit) and (FBrokerCallBack.GetCurrentPrice(GetSymbol,bpkBid)>=GetPendingOpenPrice) then
        OpenInternal(GetSymbol,GetKind,GetLots,'Broker: Sell Limit order triggered')
    end;
  end;
end;

end.

