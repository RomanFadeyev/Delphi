unit FC.Trade.Trader.PipsRSI;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderPipsRSI = interface
  ['{5C4D7EFC-ECFF-48DB-933A-34E64810BAD4}']
  end;

  TStockTraderPipsRSI = class (TStockTraderBase,IStockTraderPipsRSI)
  private
    FRSI_M1: ISCIndicatorTRSIMA;
    FRSI_M5: ISCIndicatorTRSIMA;
    FRSI_M15: ISCIndicatorTRSIMA;
    FMA84_M1 : ISCIndicatorMA;
  protected
    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
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

{ TStockTraderPipsRSI }

constructor TStockTraderPipsRSI.Create;
begin
  inherited Create;
//  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
end;

destructor TStockTraderPipsRSI.Destroy;
begin
  inherited;
end;

procedure TStockTraderPipsRSI.Dispose;
begin
  inherited;
end;


procedure TStockTraderPipsRSI.OnBeginWorkSession;
begin
  inherited;
end;

procedure TStockTraderPipsRSI.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderPipsRSI.SetProject(const aValue: IStockProject);
var
  aCreated: boolean;
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    FRSI_M1:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorTRSIMA,'RSI'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorTRSIMA;
    FRSI_M5:=CreateOrFindIndicator(aValue.GetStockChart(sti5),ISCIndicatorTRSIMA,'RSI'+'_'+StockTimeIntervalNames[sti5],true, aCreated) as ISCIndicatorTRSIMA;
    FRSI_M15:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorTRSIMA,'RSI'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorTRSIMA;

    FMA84_M1:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorMA,'MA84'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorMA;

    //Ничего не нашли, создадим нового эксперта
    if aCreated then
    begin
      FMA84_M1.SetPeriod(84);
    end;
  end;
end;

function TStockTraderPipsRSI.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderPipsRSI.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderPipsRSI.UpdateStep2(const aTime: TDateTime);
var
  aIdxM1: integer;
  aIdxM5: integer;
  aIdxM15: integer;
  aOpen,aClose : integer;
  //aRSIValueM1_1,aRSIValueM1_0, aRSIValueM5_1,aRSIValueM5_0 : TSCRealNumber;
  aMAValue,aRSIValue: TSCRealNumber;
  x: integer;
begin
  RemoveClosedOrders;

  //aInputData:=aChart.GetInputData;
  aIdxM1:=TStockDataUtils.FindBar(FRSI_M1.GetInputData,aTime,sti1);
  aIdxM5:=TStockDataUtils.FindBar(FRSI_M5.GetInputData,aTime,sti5);
  aIdxM15:=TStockDataUtils.FindBar(FRSI_M15.GetInputData,aTime,sti15);

  if (aIdxM1<>-1) and (aIdxM1>=FRSI_M1.GetPeriod) and
     (aIdxM5<>-1) and (aIdxM5>=FRSI_M5.GetPeriod) and
     (aIdxM15<>-1) and (aIdxM15>=FRSI_M15.GetPeriod) then
  begin
    aOpen:=0;
    aClose:=0;
    (*aRSIValueM1_1:=FRSI_M1.GetValue(aIdxM1-2);
    aRSIValueM1_0:=FRSI_M1.GetValue(aIdxM1-1);
    aRSIValueM5_1:=FRSI_M5.GetValue(aIdxM5-2);
    aRSIValueM5_0:=FRSI_M5.GetValue(aIdxM5-1);


    //Открываем ордер
    if //(aRSIValueM1_1<30) and (aRSIValueM1_0>aRSIValueM1_1) and
       (aRSIValueM5_1<30) and (aRSIValueM5_0>aRSIValueM5_1) then
      aOpen:=1
    else if //(aRSIValueM1_1>70) and (aRSIValueM1_0<aRSIValueM1_1) and
            (aRSIValueM5_1>70) and (aRSIValueM5_0<aRSIValueM5_1) then
      aOpen:=-1;
    *)

    aRSIValue:=FRSI_M1.GetValue(aIdxM1);
    aMAValue:=FMA84_M1.GetValue(aIdxM1);
    x:=Sign(FMA84_M1.GetInputData.Items[aIdxM1].DataClose-aMAValue);
    if x<>Sign(FMA84_M1.GetInputData.Items[aIdxM1-1].DataClose-aMAValue) then
    begin
      if (x>0) and (aRSIValue>50) then
      begin
        aOpen:=1;
        aClose:=-1;
      end
      else if (x<0)  and (aRSIValue<50) then
      begin
        aOpen:=-1;
        aClose:=1;
      end;
    end;


    (*
    if (FRSI_M1.GetValue(aIdxM1-1)<50) and (FRSI_M1.GetValue(aIdxM1)>=50) then
    begin
      aOpen:=1;
      aClose:=-1;
    end
    else if (FRSI_M1.GetValue(aIdxM1-1)>50) and (FRSI_M1.GetValue(aIdxM1)<=50) then
    begin
      aOpen:=-1;
      aClose:=1;
    end;*)


    if aClose=-1 then
      CloseAllSellOrders('')
    else if aClose=1 then
      CloseAllBuyOrders('');

    if aOpen<>0 then
    begin
      //BUY
      if (aOpen=1) and (LastOrderType<>lotBuy) then
      begin
        OpenOrder(okBuy);
      end
      //SELL
      else if (aOpen=-1) and (LastOrderType<>lotSell) then
      begin
        OpenOrder(okSell);
      end;
    end;
  end;
end;

procedure TStockTraderPipsRSI.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','PipsRSI',TStockTraderPipsRSI,IStockTraderPipsRSI);
end.




