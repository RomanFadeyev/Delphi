{-----------------------------------------------------------------------------
 Author:    Roman
 Purpose:   

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Trader.Manual;
{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Интерфейс нашего трейдера
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderManual = interface (IStockTrader)

  end;

  //Собственно трейдер
  TStockTraderManual = class (TStockTraderBase,IStockTraderManual)
  private
    procedure IStockTraderManual.Update = UpdateStep1;
  public
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;



{ TStockTraderManual }

constructor TStockTraderManual.Create;
begin
  inherited Create;
end;

destructor TStockTraderManual.Destroy;
begin
  inherited;
end;

procedure TStockTraderManual.Dispose;
begin
  inherited;
end;

procedure TStockTraderManual.UpdateStep2(const aTime: TDateTime);
begin
end;

end.




