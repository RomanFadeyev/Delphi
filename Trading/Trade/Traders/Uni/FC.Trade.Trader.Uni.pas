{-----------------------------------------------------------------------------
 Unit Name:
 Author:    Roman
 Purpose:

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Trader.Uni;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Controls, SysUtils, BaseUtils, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderUni = interface
  ['{F5CEB066-F287-452C-B5D3-67F33B32283B}']
  end;

  TStockTraderUni = class (TStockTraderBase,IStockTraderUni)
  private
    //Свойства (настраиваемые)
    FPropExpertWeighingType: TSCTraderPropertyExpertWeighingType;
    FPropExperts: TPropertyExperts;
  protected
    procedure GetProperties(aList: TPropertyList); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;
  public
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    procedure Dispose; override;

    constructor Create; override;
    destructor Destroy; override;
  end;


implementation
  uses Variants,FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  StockChart.Obj,
  FC.DataUtils;

{ TStockTraderUni }

constructor TStockTraderUni.Create;
begin
  inherited Create;

  FPropExpertWeighingType:=TSCTraderPropertyExpertWeighingType.Create(self);
  FPropExpertWeighingType.Value:=ewtAllEqual;

  FPropExperts:=TPropertyExperts.Create('Method','Experts',self);

  RegisterProperties([FPropExpertWeighingType,FPropExperts]);
end;

destructor TStockTraderUni.Destroy;
begin
  inherited;
end;

procedure TStockTraderUni.Dispose;
begin
  inherited;
  FreeAndNil(FPropExpertWeighingType);
  FreeAndNil(FPropExperts);
end;

procedure TStockTraderUni.GetProperties(aList: TPropertyList);
var
  aExperts: ISCIndicatorCollection;
  i: integer;
begin
  inherited;
  FPropExperts.EventHandler:=nil;

  try
    aExperts:=TSCIndicatorCollection.Create();

    for i:=0 to ExpertCount-1 do
      aExperts.Add(GetExpert(i));

    FPropExperts.Value:=aExperts;
  finally
    FPropExperts.EventHandler:=self;
  end;
end;

procedure TStockTraderUni.OnPropertyChanged(aNotifier: TProperty);
var
  aExperts: ISCIndicatorCollection;
  i: integer;
begin
  inherited;
  if (aNotifier=FPropExperts) and not (isReading in State) then
  begin
    while ExpertCount>0 do
      DeleteExpert(0);

    aExperts:=FPropExperts.Value;
    for i:=0 to aExperts.Count-1 do
      AddExpert(aExperts.Items[i] as ISCExpert);
  end;
end;

procedure TStockTraderUni.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  j: integer;
  aGuessOpenBuy,aGuessCloseBuy,aGuessOpenSell,aGuessCloseSell : TStockExpertGuess;
  aX: TStockExpertGuess;
  kOpenBuy,kCloseBuy, kOpenSell,kCloseSell  : integer;
  aWeight: integer;
  aExpert: ISCExpert;
  aChart : IStockChart;
  aInputData : ISCInputDataCollection;
  aPropExpertWeighingType : TTraderExpertWeighingType;
begin
  aGuessOpenBuy:=0;
  aGuessCloseBuy:=0;
  aGuessOpenSell:=0;
  aGuessCloseSell:=0;

  kOpenBuy:=0;
  kCloseBuy:=0;
  kOpenSell:=0;
  kCloseSell:=0;

  aPropExpertWeighingType:=FPropExpertWeighingType.Value;

  for i:=0 to ExpertCount-1 do
  begin
    aExpert:=GetExpert(i);
    aChart:=GetParentStockChart(aExpert);
    aInputData:=aChart.GetInputData;
    j:=aChart.FindBar(aTime);

    if j<>-1 then
    begin
      case aPropExpertWeighingType of
        ewtAllEqual: aWeight:=1;
        ewtTimeFrame:aWeight:=StockTimeIntervalValues[aChart.StockSymbol.TimeInterval];
        ewtSeniority:aWeight:=integer(aChart.StockSymbol.TimeInterval)-integer(Low(TStockTimeInterval))+1;
        else
          raise EAlgoError.Create;
      end;

      aX:=aExpert.GetGuessOpenBuy(j);
      aGuessOpenBuy:=aGuessOpenBuy+aX*aWeight;
      inc(kOpenBuy,aWeight);

      aX:=aExpert.GetGuessCloseBuy(j);
      aGuessCloseBuy:=aGuessCloseBuy+aX*aWeight;
      inc(kCloseBuy,aWeight);

      aX:=aExpert.GetGuessOpenSell(j);
      aGuessOpenSell:=aGuessOpenSell+aX*aWeight;
      inc(kOpenSell,aWeight);

      aX:=aExpert.GetGuessCloseSell(j);
      aGuessCloseSell:=aGuessCloseSell+aX*aWeight;
      inc(kCloseSell,aWeight);
    end;
  end;

  //--- Анализируем экcпертные оценки ---

  //Есть ли сигнал на закрытие
  if kCloseBuy>0 then
  begin
    aGuessCloseBuy:=Round(aGuessCloseBuy/kCloseBuy);
    if aGuessCloseBuy>eg70 then
    begin
      if LastOrderType = lotBuy then
        CloseLastOrder('Expert: time to close');
    end
  end;

  if kCloseSell>0 then
  begin
    aGuessCloseSell:=Round(aGuessCloseSell/kCloseSell);
    if aGuessCloseSell>eg70 then
    begin
      if LastOrderType = lotSell then
        CloseLastOrder('Expert: time to close');
    end
  end;

  //Есть ли сигнал на открытие
  if kOpenBuy>0 then
  begin
    aGuessOpenBuy:=Round(aGuessOpenBuy/kOpenBuy);
    if aGuessOpenBuy>eg70 then
    begin
      case LastOrderType of
        //Автоматически закрываем предыдущий противоположный ордер
        lotSell: begin CloseLastOrder('Expert: open opposite'); OpenOrder(okBuy); end;
        lotBuy:{OpenOrder(okBuy)}; //Мы уже и так открыты в эту же сторону. Второй раз открываться не будем
        lotNone: OpenOrder(okBuy);
      end;
    end
  end;


  //Есть ли сигнал на открытие
  if kOpenSell>0 then
  begin
    aGuessOpenSell:=Round(aGuessOpenSell/kOpenSell);
    if aGuessOpenSell>eg70 then
    begin
      case LastOrderType of
        //Автоматически закрываем предыдущий противоположный ордер
        lotBuy: begin CloseLastOrder('Expert: open opposite'); OpenOrder(okSell); end;
        lotSell:{OpenOrder(okSell)}; //Мы уже и так открыты в эту же сторону. Второй раз открываться не будем
        lotNone: OpenOrder(okSell);
      end;
    end
  end;

end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Universal',TStockTraderUni,IStockTraderUni);
end.



