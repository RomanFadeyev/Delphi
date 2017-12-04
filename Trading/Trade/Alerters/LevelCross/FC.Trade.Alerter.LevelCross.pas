{-----------------------------------------------------------------------------
 LevelCrosst Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.LevelCross;
{$I Compiler.inc}
//{$DEFINE SMALL_ORDER}

interface

uses
  Types, Windows, Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Alerter.Base,FC.Trade.Properties, FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics,
  StockChart.Indicators.Properties.Groups;

type
  IStockAlerterLevelCross = interface
  ['{1A0C8D49-E73D-4AF5-AC9D-58E7ADCC589E}']
  end;

  TStockAlerterLevelCross = class (TStockAlerterBase,IStockAlerterLevelCross)
  private
    FCounter: integer;
    FPropPriceRelation: TPropertyPriceRelation;
    FPropPriceLevel: TPropertyReal;
    FPropPriceKind: TPropertyPriceKind;
    FLineStyle    : TPropertyGroupPenStyle;
  protected
    procedure OnPropertyChanged(aNotifier:TProperty); override;
    procedure ResetCounter;
    procedure ShowLine;
    procedure HideLine;
    procedure RefreshLine;
  public
    procedure SetProject(const aValue : IStockProject); override;
    procedure SetEnabled(const aValue : boolean); override;
    procedure GetProperties(aList: TPropertyList); override;

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

const
  AlerterName = 'Level Cross';


{ TStockAlerterLevelCross }

constructor TStockAlerterLevelCross.Create;
begin
  inherited Create;
  FPropPriceKind:=TPropertyPriceKind.Create('Method','Type',self);
  FPropPriceKind.NameSortOrder:=10;

  FPropPriceRelation:=TPropertyPriceRelation.Create('Method','Condition',self);
  FPropPriceRelation.Value:=rlHigher;
  FPropPriceKind.NameSortOrder:=11;

  FPropPriceLevel:=TPropertyReal.Create('Method','Price',self);
  FPropPriceLevel.Value:=0;
  FPropPriceKind.NameSortOrder:=12;  

  FLineStyle:=TPropertyGroupPenStyle.Create(self);
  FLineStyle.Color.Value:=clRed;
  FLineStyle.LineStyle.Value:=lsDot;
  FLineStyle.SetCategorySortOrder(9991);

  RegisterProperties([FPropPriceKind,FPropPriceRelation,FPropPriceLevel]);

  PropBreak.Value:=0;
  UnRegisterProperties([PropBreak]);
end;

destructor TStockAlerterLevelCross.Destroy;
begin
  inherited;
end;

procedure TStockAlerterLevelCross.Dispose;
begin
  inherited;
  FreeAndNil(FPropPriceRelation);
  FreeAndNil(FPropPriceLevel);
  FreeAndNil(FPropPriceKind);;  
  FreeAndNil(FLineStyle);
end;

procedure TStockAlerterLevelCross.GetProperties(aList: TPropertyList);
begin
  inherited;
  FLineStyle.GetProperties(aList);
end;

procedure TStockAlerterLevelCross.HideLine;
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    GetProject.GetStockChart(aInterval).RemovePriceLine(GetID);
end;

procedure TStockAlerterLevelCross.OnPropertyChanged(aNotifier: TProperty);
var
  s: string;
begin
  inherited;
  ResetCounter;

  if (not (isReading in State)) and (aNotifier<>nil) then
  begin
    if FPropPriceLevel.Value<>0 then
    begin
      if GetBroker<>nil then
        s:=PriceToStr(GetBroker,GetSymbol,FPropPriceLevel.Value)
      else
        s:=PriceToStr(GetSymbol,FPropPriceLevel.Value);
      s:=AlerterName+' ('+FPropPriceKind.ValueAsText+FPropPriceRelation.ValueAsText+s+')';
    end
    else
      s:=AlerterName;

    SetName(s);
    RefreshLine;
  end;
end;

procedure TStockAlerterLevelCross.RefreshLine;
begin
  if GetProject=nil then
    exit;
  if GetEnabled and (FPropPriceLevel.Value<>0) then
    ShowLine
  else
    HideLine;
end;

procedure TStockAlerterLevelCross.ResetCounter;
begin
  FCounter:=1;
end;

procedure TStockAlerterLevelCross.SetEnabled(const aValue: boolean);
var
  aOld: boolean;
begin
  aOld:=GetEnabled;
  inherited;
  if GetEnabled and not aOld then
    ResetCounter;
  RefreshLine;    
end;

procedure TStockAlerterLevelCross.SetProject(const aValue: IStockProject);
begin
  if (GetProject<>nil) and (aValue<>GetProject) then
    HideLine;

  inherited;

  ResetCounter;
  RefreshLine;
end;

procedure TStockAlerterLevelCross.ShowLine;
var
  aInterval: TStockTimeInterval;
  aLine: ISCIndicatorPriceLine;
begin
  if FPropPriceLevel.Value=0 then
    HideLine
  else if GetProject<>nil then
    for aInterval:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    begin
      aLine:=GetProject.GetStockChart(aInterval).GetPriceLine(GetID,true);
      aLine.SetPrice(FPropPriceLevel.Value);
      aLine.SetStyle(FLineStyle.LineStyle.Value);
      aLine.SetColor(FLineStyle.Color.Value);
      aLine.SetWidth(FLineStyle.Width.Value);
      if FPropPriceRelation.Value=rlHigher then
        aLine.SetTitle(FPropPriceKind.ValueAsText+' Res')
      else
        aLine.SetTitle(FPropPriceKind.ValueAsText+' Sup')
    end;
end;

procedure TStockAlerterLevelCross.UpdateStep2(const aTime: TDateTime);
var
  aPrice: TStockRealNumber;
  aLevel: TStockRealNumber;
begin
  if (FCounter>0) and (FPropPriceLevel.Value<>0) then
  begin
    aLevel:=FPropPriceLevel.Value;
    if FPropPriceRelation.Value=rlHigher then
    begin
      aPrice:=GetBroker.GetCurrentPrice(GetSymbol,FPropPriceKind.Value);
      if aPrice>aLevel then
      begin
        AddMessage(FPropPriceKind.ValueAsText+'>'+PriceToStr(GetBroker,GetSymbol,aLevel));
        dec(FCounter);
      end;
    end
    else begin
      aPrice:=GetBroker.GetCurrentPrice(GetSymbol,FPropPriceKind.Value);
      if aPrice<aLevel then
      begin
        AddMessage(FPropPriceKind.ValueAsText+'<'+PriceToStr(GetBroker,GetSymbol,aLevel));
        dec(FCounter);
      end;
    end;
  end;

  if FCounter=0 then
    SetEnabled(false);
end;

initialization
  FC.Trade.Alerter.Factory.AlerterFactory.RegisterAlerter('Basic',AlerterName,TStockAlerterLevelCross,IStockAlerterLevelCross);
end.




