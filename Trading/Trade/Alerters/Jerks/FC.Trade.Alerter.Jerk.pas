{-----------------------------------------------------------------------------
 Jerkt Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.Jerk;
{$I Compiler.inc}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Alerter.Base,FC.Trade.Properties, FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockAlerterJerk = interface
  ['{172B5F8D-6866-4F98-BDCF-505E7F0FE6F7}']
  end;

  TStockAlerterJerk = class (TStockAlerterBase,IStockAlerterJerk)
  private
    FPropTimeInterval: TPropertyStockTimeInterval;
    FPropJerk: TPropertySmallUint;
  public
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,DateUtils, SystemService, Application.Definitions, FC.Trade.OrderCollection,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Alerter.Factory,
  FC.DataUtils;

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);


{ TStockAlerterJerk }

constructor TStockAlerterJerk.Create;
begin
  inherited Create;

  FPropTimeInterval:=TPropertyStockTimeInterval.Create(self);
  FPropTimeInterval.Value:=sti15;

  FPropJerk:=TPropertySmallUint.Create('Method','Jerk Size',self);
  FPropJerk.Value:=10;

  RegisterProperties([FPropTimeInterval,FPropJerk]);
end;

destructor TStockAlerterJerk.Destroy;
begin
  inherited;
end;

procedure TStockAlerterJerk.Dispose;
begin
  inherited;
  FreeAndNil(FPropTimeInterval);
  FreeAndNil(FPropJerk);
end;

procedure TStockAlerterJerk.UpdateStep2(const aTime: TDateTime);
var
  aData: ISCInputDataCollection;
  i: integer;
  aCurrClose, aPrevClose: TStockRealNumber;
  s: string;
  aDiffPoints: integer;
begin
  aData:=GetProject.GetStockChart(FPropTimeInterval.Value).GetInputData;
  i:=aData.FindExactMatched(aTime);
  if (i<>-1) and (i>0) then
  begin
    aCurrClose:=aData.DirectGetItem_DataClose(i);
    aPrevClose:=aData.DirectGetItem_DataClose(i-1);
    aDiffPoints:=GetBroker.PriceToPoint(GetSymbol,aCurrClose-aPrevClose);
    if Abs(aDiffPoints)>=FPropJerk.Value then
    begin
      s:='Jerk '+IntToStr(aDiffPoints)+' pt since '+DefaultFormatter.TimeToStr(aData.DirectGetItem_DataDateTime(i-1));
      AddMessage(s);
    end;
  end;
end;

initialization
  FC.Trade.Alerter.Factory.AlerterFactory.RegisterAlerter('Basic','Jerk',TStockAlerterJerk,IStockAlerterJerk);
end.




