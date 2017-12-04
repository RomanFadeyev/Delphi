unit FC.Trade.Trader.StopAndReverse;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderStopAndReverse = interface
  ['{3B2DB2CC-FC9B-4226-BFB0-0D208E465E86}']
  end;

  TStockTraderStopAndReverse = class (TStockTraderBase,IStockTraderStopAndReverse)
  private
  protected
    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);

    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs); override;    
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;

    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,DateUtils, SystemService, Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;

{ TStockTraderStopAndReverse }

constructor TStockTraderStopAndReverse.Create;
begin
  inherited Create;
//  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
end;

destructor TStockTraderStopAndReverse.Destroy;
begin
  inherited;
end;

procedure TStockTraderStopAndReverse.Dispose;
begin
  inherited;
end;


procedure TStockTraderStopAndReverse.OnBeginWorkSession;
begin
  inherited;
end;

procedure TStockTraderStopAndReverse.OnModifyOrder(const aOrder: IStockOrder;const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  inherited;
  if aModifyEventArgs.ModifyType=omtClose then
  begin
    if aOrder.GetKind=okBuy then
      OpenOrder(okSell)
    else
     OpenOrder(okBuy);
  end;
end;

procedure TStockTraderStopAndReverse.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderStopAndReverse.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;
end;

function TStockTraderStopAndReverse.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderStopAndReverse.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderStopAndReverse.UpdateStep2(const aTime: TDateTime);
begin
  RemoveClosedOrders;

  if LastOrderType=lotNone then
    OpenOrder(okBuy);
end;

procedure TStockTraderStopAndReverse.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','StopAndReverse',TStockTraderStopAndReverse,IStockTraderStopAndReverse);
end.




