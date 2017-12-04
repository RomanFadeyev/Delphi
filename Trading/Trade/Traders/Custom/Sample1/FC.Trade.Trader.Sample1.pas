{-----------------------------------------------------------------------------
 Author:    Roman
 Purpose:   Пример реализации трейдера. Данный трейдер работает с
            двумя индикаторами MACD и RSI на фреймах 60 и 15

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Trader.Sample1;
{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Интерфейс нашего трейдера
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderSample1 = interface
  ['{D80D6DF8-2FF2-42A0-A5F6-9C306D046994}']
  end;

  //Собственно трейдер
  TStockTraderSample1 = class (TStockTraderBase,IStockTraderSample1)
  private
    FMACD: ISCIndicatorMACD;
    FRSI : ISCIndicatorRSI;
  public
    //Создание-удаление своих объектов
    procedure OnCreateObjects; override;
    procedure OnReleaseObjects; override;

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



{ TStockTraderSample1 }

constructor TStockTraderSample1.Create;
begin
  inherited Create;
end;

destructor TStockTraderSample1.Destroy;
begin
  inherited;
end;

procedure TStockTraderSample1.Dispose;
begin
  inherited;
end;

procedure TStockTraderSample1.OnCreateObjects;
begin
  inherited;
  //Создаем MACD на 60
  FMACD:=CreateOrFindIndicator(GetProject.GetStockChart(sti60),ISCIndicatorMACD,'60, MACD') as ISCIndicatorMACD;

  //Создаем RSI На 15
  FRSI:=CreateOrFindIndicator(GetProject.GetStockChart(sti15),ISCIndicatorRSI,'15, RSI') as ISCIndicatorRSI;
end;

procedure TStockTraderSample1.OnReleaseObjects;
begin
  inherited;
  if FMACD<>nil then
    OnRemoveObject(FMACD);
  FMACD:=nil;

  if FRSI<>nil then
    OnRemoveObject(FRSI);
  FRSI:=nil;
end;

procedure TStockTraderSample1.UpdateStep2(const aTime: TDateTime);
var
  j: integer;
  aGuessOpen,aGuessClose : TStockExpertGuess;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
begin
  aGuessOpen:=0;
  aGuessClose:=0;

  //Брокер может закрыть ордера и без нас. У нас в списке они остануться,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

  //----- Анализируем экcпертные оценки ---- 
  aChart:=GetProject.GetStockChart(sti60);
  aInputData:=aChart.GetInputData;
  j:=aChart.FindBar(aTime); //Получаем индекс бара по времени

  if j<>-1 then
  begin
    //Здесь принимается решение об открытии/закрытии ордеров
    //Анализ приведен только в качестве примера!!!
    if (FMACD.GetValue(j)>0) and (FRSI.GetValue(j)<0) then
    begin
      aGuessClose:=egSellSurely;
      aGuessOpen:=egBuySurely;
    end
    else if (FMACD.GetValue(j)<0) and (FRSI.GetValue(j)>0) then
    begin
      aGuessClose:=egBuySurely;
      aGuessOpen:=egSellSurely;
    end;

    //Если есть команда к закрытию открытых ордеров
    if aGuessClose<egSell70 then
    begin
      if LastOrderType = lotSell then
        CloseLastOrder('Trader: time to close');
    end
    else if aGuessClose>egBuy70 then
    begin
      if LastOrderType=lotBuy then
        CloseLastOrder('Trader: time to close');
    end;

    //Открываемся на продажу
    if aGuessOpen<egSell70 then
    begin
      case LastOrderType of
        lotBuy: begin
          //Автоматически закрываем предыдущий противоположный ордер
          CloseLastOrder('Trader: open opposite');
          OpenOrder(okSell);
        end;
        lotSell:; //Мы уже и так открыты в эту же сторону. Второй раз открываться не будем
        lotNone: OpenOrder(okSell);
      end;
    end
    //Открываемся на покупку
    else if aGuessOpen>egBuy70 then
    begin
      case LastOrderType of
        lotSell: begin
          //Автоматически закрываем предыдущий противоположный ордер
          CloseLastOrder('Trader: open opposite');
          OpenOrder(okBuy);
        end;
        lotBuy:; //Мы уже и так открыты в эту же сторону. Второй раз открываться не будем
        lotNone: OpenOrder(okBuy);
      end;
    end
    
  end;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Samples','Sample1',TStockTraderSample1,IStockTraderSample1);
end.




