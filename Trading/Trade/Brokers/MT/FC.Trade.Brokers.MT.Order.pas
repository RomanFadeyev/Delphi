{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация ордера для MT См проект
            "X:\Trade\Forecaster\Stock Server"

            Внимание
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.MT.Order;
{$I Compiler.inc}

interface
uses
  SysUtils,BaseUtils, MTT_TLB,
  Serialization, FC.Definitions,StockChart.Definitions,FC.Trade.Brokers.OrderBase;

type
  TStockOrder = class;

  //Специальное расширение для поддержки нас
  IStockBrokerMTSupport = interface
  ['{CAFBB2D1-1E2C-499E-A585-99EB8FCEF041}']
    procedure OnModifyOrder(aOrder: TStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);

    function GetNative: IMTTBroker;
  end;

 { TStockOrder }

  TStockOrder = class (TStockOrderBase)
  private
    FNative   : IMTTOrder;
    FBrokerCallBack   : IStockBrokerMTSupport;
  protected
    procedure OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs); override;
  public
    //Implementation
    procedure Dispose; override;


    property  Native:IMTTOrder read FNative;

    constructor Create(const aStockBroker: IStockBrokerMTSupport; const aStockTrader: IStockTrader);
  end;

implementation
  uses Math,FC.Trade.Brokers.MT.Broker;




{ TStockOrder }

constructor TStockOrder.Create(const aStockBroker: IStockBrokerMTSupport; const aStockTrader: IStockTrader);
begin
  inherited Create(aStockBroker as IStockBroker, aStockTrader);
  FBrokerCallBack:=aStockBroker;
  if (aStockBroker<>nil) then
    FNative:=aStockBroker.GetNative.CreateOrder;
end;

procedure TStockOrder.Dispose;
begin
  inherited;
end;

procedure TStockOrder.OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  inherited;
  if (FBrokerCallBack<>nil) then
    FBrokerCallBack.OnModifyOrder(self,aModifyEventArgs);
end;

end.

