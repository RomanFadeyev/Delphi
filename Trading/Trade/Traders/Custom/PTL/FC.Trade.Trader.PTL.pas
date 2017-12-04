unit FC.Trade.Trader.PTL;

interface
  uses Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage, Dialogs;

type
  //тип игры - никак, прорывы вверх и вниз, отбои вверх и вниз 
  TTradeType = (ttNothing, ttUp_Break, ttDown_Break, ttUp_Recoil, ttDown_Recoil);

  //Идентификационный интерфейс трейдера.
  IStockTraderPTL = interface
    ['{04FA6D2C-252B-4EF2-AA67-5CD97B07CF88}']
  end;

  TStockTraderPTL = class (TStockTraderBase, IStockTraderPTL)
  private
    FStopLoss, FTakeProfit: double;
    FTrailingStop: double;
    FSafeGap: double;
    FPropSafeGap: TPropertySmallUint;
    FTradeType: TTradeType;

    FPC_10 : ISCIndicatorPriceChannel;
    FPC_22 : ISCIndicatorPriceChannel;
    FPC_3 : ISCIndicatorPriceChannel;
    FTF: TStockTimeInterval;
    FCurrentBar: integer;
  protected
  public
    //Создание-удаление своих объектов
    procedure OnCreateObjects; override;
    procedure OnReleaseObjects; override;

    constructor Create; override;
    destructor Destroy; override;

    procedure UpdateStep2(const aTime: TDateTime); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;
  end;

implementation
uses FC.Trade.Trader.Factory;

constructor TStockTraderPTL.Create;
begin
  inherited Create;

  FTF:=sti1440;

  FPropSafeGap:=TPropertySmallUint.Create('Method','Safe Gap',self);
  FPropSafeGap.Value:=10;
  RegisterProperties([FPropSafeGap]);
  FCurrentBar:=-1;
  FTradeType:=ttNothing;
end;

destructor TStockTraderPTL.Destroy;
begin
  inherited;
end;

procedure TStockTraderPTL.OnCreateObjects;
var
  aCreated: boolean;
begin
  inherited;

  //Создаем PC-10 на ТФ_D1
  FPC_10:=CreateOrFindIndicator(GetProject.GetStockChart(FTF),ISCIndicatorPriceChannel,'1440, PC(10)',true, aCreated) as ISCIndicatorPriceChannel;
  //Если индикатор был только что создан, мы выставляем ему значения по умолчанию,
  //иначе оставляем как есть - ведь пользователь мог их изменить
  if aCreated then
  begin
    FPC_10.SetPeriod(10);
    FPC_10.SetTopLineColor(clGreen);
    FPC_10.SetBottomLineColor(clRed);
    FPC_10.SetTopLineStyle(lsDash);
    FPC_10.SetBottomLineStyle(lsDot);
  end;


  //Создаем PC-22 на ТФ_D1
  FPC_22:=CreateOrFindIndicator(GetProject.GetStockChart(FTF),ISCIndicatorPriceChannel,'1440, PC(22)',true, aCreated) as ISCIndicatorPriceChannel;
  if aCreated then
  begin
    FPC_22.SetPeriod(22);
    FPC_22.SetTopLineColor(clBlack);
    FPC_22.SetBottomLineColor(clBlack);
    FPC_22.SetTopLineStyle(lsSolid);
    FPC_22.SetBottomLineStyle(lsSolid);
  end;
{
  //Создаем PC-3 на ТФ_D1
  FPC_3:=CreateOrFindIndicator(GetProject.GetStockChart(FTF),ISCIndicatorPriceChannel,'1440, PC(3)',true, aCreated) as ISCIndicatorPriceChannel;
  if aCreated then
  begin
    FPC_3.SetPeriod(3);
  end;
}
end;

procedure TStockTraderPTL.OnReleaseObjects;
begin
  inherited;

  if FPC_10<>nil then
    OnRemoveObject(FPC_10);
  FPC_10:=nil;

  if FPC_22<>nil then
    OnRemoveObject(FPC_22);
  FPC_22:=nil;

  if FPC_3<>nil then
    OnRemoveObject(FPC_3);
  FPC_3:=nil;

end;

procedure TStockTraderPTL.UpdateStep2(const aTime: TDateTime);
var
  j: integer;
  aChart: IStockChart;
  aInputData : ISCInputDataCollection;
  aOrder: IStockOrder;
  aOrders: IStockOrderCollection;
  aBroker: IStockBroker;
  aPC_10_Top, aPC_10_Bottom: double;
//  aPC_22, aPC_3: double;
  aOpenPrice: double;
  aSpread: integer;
//  aBid,aAsk: double;
begin
  //Спрэд для установки ордеров с экстремумов
  //Спрэд, который дает тестовый брокер тут не подходит
  aSpread:=3;

  aChart:=GetProject.GetStockChart(FTF);
  aInputData:=aChart.GetInputData;
  aOrders:=GetOrders;
  aBroker:=GetBroker;
  //aBid:=aBroker.GetCurrentPrice(GetSymbol,bpkBid);
  //aAsk:=aBroker.GetCurrentPrice(GetSymbol,bpkAsk);

  if aOrders.Count>1 then
  begin
    ShowMessage('Слишком много ордеров');
    exit;
  end;

  RemoveClosedOrders;

  //Если ордеров нет, значит мы никуда не играем
  if aOrders.Count<1 then FTradeType:=ttNothing;

  //определение номера бара, с запрашиваемым временем
  j:=aChart.FindBar(aTime);

  if j<2 then exit;

  //ловим момент завершение предыдущего бара
  if FCurrentBar=j then exit
  else FCurrentBar:=j;

  //все, что ниже должно происходить один раз за бар

  aPC_10_Top:=FPC_10.GetTopValue(j-2);
  if aPC_10_Top<=0 then exit;

  aPC_10_Bottom:=FPC_10.GetBottomValue(j-2);
  if aPC_10_Bottom<=0 then exit;

  if FTradeType=ttNothing then
  begin
    //ждем касания вчерашней цены позавчерашнего PC-10
    //(вчерашняя свеча только что закончилась)

    //касание верхней линии
    if aInputData[j-1].DataHigh>=aPC_10_Top then
    begin
      //пробой верхней линии закрытием свечи
      if  aInputData[j-1].DataClose>aPC_10_Top then
      begin
        aOpenPrice:=aInputData[j-1].DataHigh+GetBroker.PointToPrice(GetSymbol,aSpread)+FSafeGap;
        aOrder:=OpenOrderAt(okBuy,aOpenPrice,'');
//        aOrder.OpenAt(okBuy,aOpenPrice,1);
{
        aOrder.SetStopLoss(aOpenPrice-FStopLoss);
        aOrder.SetTakeProfit(aOpenPrice+FTakeProfit);
        aOrder.SetTrailingStop(FTrailingStop);
}
        FTradeType:=ttUp_Break;
      end

      //пробоя нет - играем на отбой от верхней линии
      else
      begin
        aOpenPrice:=aInputData[j-1].DataLow-FSafeGap;
        aOrder:=OpenOrderAt(okSell,aOpenPrice,'');
//        aOrder:=CreateEmptyOrder;
//        aOrder.OpenAt(okSell,aOpenPrice,1);
{
        aOrder.SetStopLoss(aOpenPrice+FStopLoss);
        aOrder.SetTakeProfit(aOpenPrice-FTakeProfit);
        aOrder.SetTrailingStop(FTrailingStop);
}
        FTradeType:=ttDown_Recoil;
      end;
    end;

    //касание нижней линии
    if aInputData[j-1].DataLow<=aPC_10_Bottom then
    begin
      //пробой нижней линии закрытием
      if  aInputData[j-1].DataClose<aPC_10_Bottom then
      begin
        aOpenPrice:=aInputData[j-1].DataLow-FSafeGap;
        aOrder:=OpenOrderAt(okSell,aOpenPrice,'');
//        aOrder:=CreateEmptyOrder;
//        aOrder.OpenAt(okSell,aOpenPrice,1);
{
        aOrder.SetStopLoss(aOpenPrice+FStopLoss);
        aOrder.SetTakeProfit(aOpenPrice-FTakeProfit);
        aOrder.SetTrailingStop(FTrailingStop);
}
        FTradeType:=ttDown_Break;
      end

      //пробоя нет - играем на отбой от нижней линии
      else
      begin
        aOpenPrice:=aInputData[j-1].DataHigh+GetBroker.PointToPrice(GetSymbol,aSpread)+FSafeGap;
        aOrder:=OpenOrderAt(okBuy,aOpenPrice,'');        
//        aOrder:=CreateEmptyOrder;
//        aOrder.OpenAt(okBuy,aOpenPrice,1);
{
        aOrder.SetStopLoss(aOpenPrice-FStopLoss);
        aOrder.SetTakeProfit(aOpenPrice+FTakeProfit);
        aOrder.SetTrailingStop(FTrailingStop);
}
        FTradeType:=ttUp_Recoil;
      end;
    end;
  end

  //корректируем отложенные ордеры на отбой от нижней линии
  else if (FTradeType=ttUp_Recoil) then
    if  (aOrders.Items[0].GetState=osPending) and (aInputData[j-1].DataLow<=aPC_10_Bottom) and (aInputData[j-1].DataClose>=aPC_10_Bottom) then
    begin


    end

  //корректируем отложенные ордеры на отбой от верхней линии
  else if (FTradeType=ttDown_Recoil) then
    if(aOrders.Items[0].GetState=osPending) and (aInputData[j-1].DataHigh>=aPC_10_Top) and (aInputData[j-1].DataClose<=aPC_10_Top) then
    begin


    end;


  //надо не забыть про ситуацию, когда цена касается обеих линий
  //if (aInputData[j].DataHigh>=aPC_10_Top) and (aInputData[j].DataLow<=aPC_10_Bottom) then
end;


procedure TStockTraderPTL.OnPropertyChanged(aNotifier:TProperty);
var
  aPropName: string;
  i: integer;
begin
  //берем значения из свойств трэйдера
  for i := 0 to GetProperties.Count-1 do
  begin
    aPropName:=GetProperties.Items[i].GetName;

    if aPropName='Enough Profit' then FTakeProfit:=GetBroker.PointToPrice(GetSymbol,GetProperties.Items[i].Value)
    else if aPropName='Max Subsidence' then FStopLoss:=GetBroker.PointToPrice(GetSymbol,GetProperties.Items[i].Value)
    else if aPropName='Trailing stop' then FTrailingStop:=GetBroker.PointToPrice(GetSymbol,GetProperties.Items[i].Value)
    else if aPropName='Safe Gap' then FSafeGap:=GetBroker.PointToPrice(GetSymbol,GetProperties.Items[i].Value);
  end;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Test','Primitive Technical Look',TStockTraderPTL,IStockTraderPTL);

end.
