{-----------------------------------------------------------------------------
 Calendar Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.Calendar;
{$I Compiler.inc}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, 
  Serialization, FC.Definitions, FC.Trade.Alerter.Base,FC.Trade.Properties, FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockAlerterCalendar = interface
  ['{BA54574E-1FC5-4112-9DE1-D77E55680492}']
  end;

  TStockAlerterCalendar = class (TStockAlerterBase,IStockAlerterCalendar)
  private
    FPropBeforeMinutes: TPropertySmallUint;
    FPropCountryFilter: TPropertyString;
    FAllowedCountries: TStringList; //из FCountryFilter
    FPassedItems : TStringList;

    function FilterCalendarItem(const aItem: ISCCalendarItem): boolean;
  protected
    procedure OnBeginWorkSession; override;

    procedure OnPropertyChanged(aProperty:TProperty); override;
    procedure FormatMessage(const aMessage: string; out aCaption,aText: string); override;    
  public
    //ѕосчитать
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


{ TStockAlerterCalendar }

constructor TStockAlerterCalendar.Create;
begin
  inherited Create;

  FPassedItems:=TStringList.Create;
  FPassedItems.Duplicates:=dupIgnore;
  FPassedItems.Sorted:=true;

  FAllowedCountries:= TStringList.Create;


  FPropBeforeMinutes:=TPropertySmallUint.Create('Method','Minutes Before',self);
  FPropBeforeMinutes.Value:=10;

  FPropCountryFilter:=TPropertyString.Create('Method','Country Filter',self);
  FPropCountryFilter.HelpString:='Use comma to separate several countries';

  RegisterProperties([FPropBeforeMinutes,FPropCountryFilter]);
end;

destructor TStockAlerterCalendar.Destroy;
begin
  inherited;
  FreeAndNil(FAllowedCountries);
  FreeAndNil(FPassedItems);
end;

procedure TStockAlerterCalendar.Dispose;
begin
  inherited;
  FreeAndNil(FPropBeforeMinutes);
  FreeAndNil(FPropCountryFilter);
end;

function TStockAlerterCalendar.FilterCalendarItem(const aItem: ISCCalendarItem): boolean;
begin
  result:=true;

  if FAllowedCountries.Count<>0 then
  begin
    if FAllowedCountries.IndexOf(aItem.GetCountry)=-1 then
    begin
      result:=false;
      exit;
    end;
  end;
end;

procedure TStockAlerterCalendar.FormatMessage(const aMessage: string; out aCaption, aText: string);
begin
  inherited;
  aText:=aMessage;
end;

procedure TStockAlerterCalendar.OnBeginWorkSession;
begin
  inherited;
  FPassedItems.Clear;
end;

procedure TStockAlerterCalendar.OnPropertyChanged(aProperty: TProperty);
begin
  inherited;
  if aProperty=FPropCountryFilter then
    SplitString(FPropCountryFilter.Value,FAllowedCountries,[';',',']);
end;

procedure TStockAlerterCalendar.UpdateStep2(const aTime: TDateTime);
var
  aDate: TDateTime;
  i: integer;
  aItem : ISCCalendarItem;
  s: string;
  aKey: string;
begin
  i:=CalendarManager.Calendar.FindFirstItemGE(aTime);
  if i=-1 then
    exit;

  aDate:=IncMinute(aTime,FPropBeforeMinutes.Value*10);
  s:='';

  for i := i to CalendarManager.Calendar.Count - 1 do
  begin
    aItem:=CalendarManager.Calendar.GetItem(i);
    if aItem.GetDateTime>aDate then
      break;

    //»так, здесь у нас событи€, попадающие в промежуток от текущего времени + n Mninutes
    aKey:=aItem.GetIndicator+'|'+aItem.GetCountry+'|'+DateTimeToStr(aItem.GetDateTime)+'|'+aItem.GetPeriod;
    if (FPassedItems.IndexOf(aKey)=-1) and (FilterCalendarItem(aItem)) then
    begin
      if s<>'' then
        s:=s+#13#10;

      s:=s+aItem.GetIndicator+' ('+
           aItem.GetCountry;
      if aItem.GetPriority<>'' then
        s:=s+', '+ aItem.GetPriority;
      s:=s+') at '+ DefaultFormatter.TimeToStr(aItem.GetDateTime);
      FPassedItems.Add(aKey);
    end;
  end;

  if s<>'' then
    AddMessage(s);
end;

initialization
  FC.Trade.Alerter.Factory.AlerterFactory.RegisterAlerter('Basic','Calendar',TStockAlerterCalendar,IStockAlerterCalendar);
end.




