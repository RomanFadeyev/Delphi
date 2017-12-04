{-----------------------------------------------------------------------------
 AlarmClock Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.AlarmClock;
{$I Compiler.inc}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Alerter.Base,FC.Trade.Properties, FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockAlerterAlarmClock = interface
  ['{B2185D0F-F4D3-4D83-A1D3-0E27031B1CB6}']
  end;

  TStockAlerterAlarmClock = class (TStockAlerterBase,IStockAlerterAlarmClock)
  private
    FPropTime: TPropertyTime;
    FPassedTimes : TMap<TDateTime,Boolean>;
  protected
    procedure OnBeginWorkSession; override;  
    procedure FormatMessage(const aMessage: string; out aCaption,aText: string); override;
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
  FC.DataUtils,FC.Singletons;

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);


{ TStockAlerterAlarmClock }

constructor TStockAlerterAlarmClock.Create;
begin
  inherited Create;

  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
  FPropTime:=TPropertyTime.Create('Method','Time',self);
  RegisterProperties([FPropTime]);
end;

destructor TStockAlerterAlarmClock.Destroy;
begin
  inherited;
  FreeAndNil(FPassedTimes);
end;

procedure TStockAlerterAlarmClock.Dispose;
begin
  inherited;
  FreeAndNil(FPropTime);
end;

procedure TStockAlerterAlarmClock.FormatMessage(const aMessage: string; out aCaption, aText: string);
begin
  inherited;
  aText:=aMessage;
end;

procedure TStockAlerterAlarmClock.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
end;

procedure TStockAlerterAlarmClock.UpdateStep2(const aTime: TDateTime);
var
  aAlarmTime: TDateTime;
  aAlarmDateTime: TDateTime;
begin
  aAlarmTime:=FPropTime.Value;
  if CompareTime(aTime,aAlarmTime)>=0 then
  begin
    aAlarmDateTime:=Trunc(aTime)+Frac(aAlarmTime);
    if not FPassedTimes.Lookup(aAlarmDateTime) then
    begin
      FPassedTimes.Add(aAlarmDateTime,true);
      AddMessage('It''s '+TimeToHHMMStr(aAlarmTime)+' now');
    end;
  end;
end;

initialization
  FC.Trade.Alerter.Factory.AlerterFactory.RegisterAlerter('Basic','Alarm Clock',TStockAlerterAlarmClock,IStockAlerterAlarmClock);
end.




