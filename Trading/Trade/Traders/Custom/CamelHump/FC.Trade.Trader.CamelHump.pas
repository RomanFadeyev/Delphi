unit FC.Trade.Trader.CamelHump;

interface
  uses Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage, Dialogs;

{$HINTS OFF}

type
  //Идентификационный интерфейс трейдера.
  IStockTraderCamelHump = interface
  ['{48F7CD30-6A29-4DC9-8E1D-DBEBDA6BC97F}']
  end;

  TStockTraderCamelHump = class (TStockTraderBase, IStockTraderCamelHump)
  private
    FStopLoss, FTakeProfit: double;

    FMA_min : ISCIndicatorMA;
    FMA_max : ISCIndicatorMA;
    FMA_fast : ISCIndicatorMA;
  protected
  public
    //Создание-удаление своих объектов
    procedure OnCreateObjects; override;
    procedure OnReleaseObjects; override;

    constructor Create; override;
    destructor Destroy; override;

    //как я понял функция аналогичная Start() в МТ
    procedure UpdateStep2(const aTime: TDateTime); override;
  end;

implementation
uses FC.Trade.Trader.Factory;


constructor TStockTraderCamelHump.Create;
begin
  FStopLoss:=50;
  FTakeProfit:=100;

  inherited Create;
end;

destructor TStockTraderCamelHump.Destroy;
begin
  inherited;
end;

procedure TStockTraderCamelHump.OnCreateObjects;
var
  aCreated: boolean;
begin
  inherited;

  //Создаем SMA_40 по минимумам на ТФ_60
  FMA_min:=CreateOrFindIndicator(GetProject.GetStockChart(sti60),ISCIndicatorMA,'60, SMA(40) at Low',true, aCreated) as ISCIndicatorMA;
  //Если индикатор был только что создан, мы выставляем ему значения по умолчанию,
  //иначе оставляем как есть - ведь пользователь мог их изменить
  if aCreated then
  begin
    FMA_min.SetPeriod(40);
    FMA_min.SetMAMethod(mamSimple);
    FMA_min.SetApplyTo(atLow);
  end;

  //Создаем SMA_40 по максимумам на ТФ_60
  FMA_max:=CreateOrFindIndicator(GetProject.GetStockChart(sti60),ISCIndicatorMA,'60, SMA(40) at High',true, aCreated) as ISCIndicatorMA;
  if aCreated then
  begin
    FMA_max.SetPeriod(40);
    FMA_max.SetMAMethod(mamSimple);
    FMA_max.SetApplyTo(atHigh);
  end;

{
  //Создаем EMA_15 по клосам на ТФ_60
  FMA_fast:=CreateOrFindIndicator(aValue.GetStockChart(sti60),ISCIndicatorMA) as ISCIndicatorMA;
  FMA_fast.SetPeriod(15);
  FMA_fast.SetMAMethod(mamExponential);
  FMA_fast.SetApplyTo(atClose);
}
end;

procedure TStockTraderCamelHump.OnReleaseObjects;
begin
  inherited;

  if FMA_min<>nil then
    OnRemoveObject(FMA_min);
  FMA_min:=nil;

  if FMA_max<>nil then
    OnRemoveObject(FMA_max);
  FMA_max:=nil;

{
  if FMA_fast<>nil then
    OnRemoveObject(FMA_fast);
  FMA_fast:=nil;
}
end;

procedure TStockTraderCamelHump.UpdateStep2(const aTime: TDateTime);
var
  j: integer;
  aChart: IStockChart;
  aInputData : ISCInputDataCollection;
  aData: ISCInputData;

  aOrders: IStockOrderCollection;
  aAsk,aBid: double;
  aOrder: IStockOrder;
  aBroker: IStockBroker;
begin

  aChart:=GetProject.GetStockChart(sti60);
  aInputData:=aChart.GetInputData;

  //определение номера бара, с запрашиваемым временем
  j:=aChart.FindBar(aTime);
  if j=50 then
    aData:=aInputData.Items[49];
  

  //Определяем текущую цену Ask и Bid
  aBroker:=GetBroker;
  aAsk:=aBroker.GetCurrentPrice(GetSymbol,bpkAsk);
  aBid:=aBroker.GetCurrentPrice(GetSymbol,bpkBid);


  {

  if aOrders.Count<1 then
  begin
      aOrder:=CreateEmptyOrder;
      aOpenPrice :=aBroker.GetCurrentPrice(bpkAsk)+0.0005;
      aOrder.OpenAt(okBuy,aOpenPrice,1);
      aOrder.SetStopLoss(aOpenPrice-FStopLoss/10000);
      aOrder.SetTakeProfit(aOpenPrice+FTakeProfit/10000);
      aOrder.SetTrailingStop(0);
    end
    else if aGuessOpen=-10 then
    begin
      ShowMessage(FloatToStr(aGuessOpen)+'          '+IntToStr(j));
      aOrder:=CreateEmptyOrder;
      aOpenPrice :=aBroker.GetCurrentPrice(bpkBid)-0.0005;
      aOrder.OpenAt(okSell,aOpenPrice,1);
      aOrder.SetStopLoss(aOpenPrice+FStopLoss/10000);
      aOrder.SetTakeProfit(aOpenPrice-FTakeProfit/10000);
      aOrder.SetTrailingStop(0);

    end;
}
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Test','CamelHump',TStockTraderCamelHump,IStockTraderCamelHump);
end.
