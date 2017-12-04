{-----------------------------------------------------------------------------
 TrendFollowert Name:
 Author:    Roman
 Purpose:


  АЛГОРИТМ:

  Все действия производим по закрытию указанного дня. Внутри дня ничего не делаем.
  Открытие - закрытие позиции только ордерами.
  Сигнальная линия - экспоненциальная средняя, период 3 сдвиг вперед (в будущее) - 3

  Правила:

  1.  Пересечение телом свечи средней - ставятся ордера или покупку Н+5п. (если свеча восходящая),
                                                        или продажу L-5п  (если свеча нисходящая).
  2.  Касание нижней тенью средней - ставится ордер на покупку Н+5 п.
      Касание верхней тенью средней - ставится ордер на продажу L-5п
  3.  Ордер отменяем только при возникновении противоположного сигнала.
  4.  Переносим если возникает сигнал в ту же сторону, но более выгодный (выше при продаже, ниже при покупке)
  5.  После открытия стоп ставится на противоположном конце -(+) 5 п. той же свечи.
  6.  Потом стоп или переносится в безубыток (если закрытие свечи выше (ниже) уровня открытия позиции более чем 10 п.),
      или подтягивается ближе на минимум двух последних свечей при покупке или максимум двух последних свечей при продаже.
      Это делается на каждой новой свече до закрытия позиции.
  7.  Когда позиция открыта, на сигналы не реагируем, дополнительных позиций не открываем.
  8.  Стоп в сторону увеличения не переносится.
  9.  Если после открытия позиции не можем поджать в безубыток, и две след. свечи заканчиваются хуже цены открытия позиции, ставим тейк +5п.
  10. Если закрыло в безубытке или по тейку, следующая свеча не касается средней,
      но идет в том же направлении (выше / ниже предыдущих 3 свечей), исполняем условие 1.
  11. Если свеча упирается в линию с сильным наклоном, пересекает ее хвостом, а телом не смогла пересечь,
      и очевидно, что след. свеча начнется за средней, исполняем условие 1.

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.Trader.AverageBreakthrough;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList,
  Collections.Map,Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderAverageBreakthrough = interface
  ['{797ED06B-5E31-4DD4-8FBE-BEC5CDEF31E1}']
  end;

  //Свойства ордера
  TStockOrderProperties=class;
  IStockOrderProperties = interface (ISCAttribute)
  ['{FC5C8FCF-7C5E-49AB-A30F-99830E7AC5DF}']
    function GetObject:TStockOrderProperties;
  end;

  //Атрибут "большого" ордера
  IStockLargeOrderAttribute = interface (ISCAttribute)
  ['{1197FB87-425F-423A-B546-A6C814832F9C}']
  end;

  //Атрибут "маленького" ордера
  IStockSmallOrderAttribute = interface (ISCAttribute)
  ['{BDE21E2D-9D99-4E8B-8FF0-566041187DFC}']
  end;

  TStockOrderProperties = class (TNameValuePersistentObjectRefCounted,IStockOrderProperties,ISCAttribute)
  public
    //Когда перевернулся 1H - тренд не в нашу сторону
    H1TrendTurnTime:TDateTime;

    //from IStockOrderProperties
    function GetObject:TStockOrderProperties;
  end;

  TStockLargeOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockLargeOrderAttribute,ISCAttribute)
  end;

  //Атрибут "маленького" ордера
  TStockSmallOrderAttribute = class (TNameValuePersistentObjectRefCounted,IStockSmallOrderAttribute,ISCAttribute)
  end;


  TStockTraderAverageBreakthrough = class (TStockTraderBase,IStockTraderAverageBreakthrough)
  private
    //Это временные переменные, испольщуемые при трейдинге
    FMA: ISCIndicatorMA;
    FMAIndex: integer;
    FLow: array [0..4] of TSCRealNumber;
    FHigh: array [0..4] of TSCRealNumber;
    FOpen: array [0..4] of TSCRealNumber;
    FClose: array [0..4] of TSCRealNumber;

    FPassedTimes:TMap<TDateTime,Boolean>;
    FPassedTimes2:TMap<TDateTime,Boolean>;
  protected
    function GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;

    procedure GetProperties(aList: TPropertyList); override;
    procedure OnPropertyChanged(aNotifier:TProperty); override;

    //Отрубаем стандартные механизмы установки ST TP
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;
    procedure SetStopLossAccordingProperty(aOrder: IStockOrder); override;

    function  CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
    procedure InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);

    //форма для тестирования
    function TestBenchDialogClass: TClass; override;

    function  Spread: TSCRealNumber;

    function  SignalToOpen(const aTime: TDateTime): integer;
    procedure TryOpenOrder(aOrder: IStockOrder; const aTime: TDateTime);
    //Постараться передвинуть SL в безубыток
    procedure MoveSLToProfitablePoint(const aOrder: IStockOrder; const aTime: TDateTime);
    procedure AnalyzeOpenedOrder(const aOrder: IStockOrder; const aTime: TDateTime);

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

const
  TrendToOrderKind : array [TSCTrendType] of TStockOrderKind = (okBuy,okSell,okBuy);

const
  aOrderMargin=0.0005;

{ TStockTraderAverageBreakthrough }

constructor TStockTraderAverageBreakthrough.Create;
begin
  inherited Create;
  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
  FPassedTimes:=TMap<TDateTime,Boolean>.Create;
  FPassedTimes2:=TMap<TDateTime,Boolean>.Create;
end;

destructor TStockTraderAverageBreakthrough.Destroy;
begin
  inherited;
  FreeAndNil(FPassedTimes);
  FreeAndNil(FPassedTimes2);  
end;

procedure TStockTraderAverageBreakthrough.Dispose;
begin
  inherited;
end;

function TStockTraderAverageBreakthrough.CreateIndicatorMA(const aChart: IStockChart): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'ParabolicSAR-'+aChart.StockSymbol.GetTimeIntervalName,true,aCreated) as ISCIndicatorMA;

  if aCreated then
  begin
    InitMA(result,aChart.StockSymbol.TimeInterval);
  end;
end;

procedure TStockTraderAverageBreakthrough.GetProperties(aList: TPropertyList);
begin
  inherited;
end;

procedure TStockTraderAverageBreakthrough.OnBeginWorkSession;
begin
  inherited;
  FPassedTimes.Clear;
  FPassedTimes2.Clear;
end;

procedure TStockTraderAverageBreakthrough.OnPropertyChanged(aNotifier: TProperty);
begin
  inherited;
end;

procedure TStockTraderAverageBreakthrough.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderAverageBreakthrough.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
    while ExpertCount>0 do
      DeleteExpert(0);

  if aValue <> nil then
  begin
    FMA:=CreateIndicatorMA(aValue.GetStockChart(sti1440));
  end;
end;

procedure TStockTraderAverageBreakthrough.SetStopLossAccordingProperty(aOrder: IStockOrder);
begin
  //Ничего!
end;

procedure TStockTraderAverageBreakthrough.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
begin
  //Ничего!
end;

function TStockTraderAverageBreakthrough.TestBenchDialogClass: TClass;
begin
  result:=nil;
end;

function TStockTraderAverageBreakthrough.SignalToOpen(const aTime: TDateTime): integer;
var
  aMA: TSCRealNumber;
begin
  result:=0;

  if FMAIndex<FMA.GetPeriod+1 then
    exit;

  aMA:=FMA.GetValue(FMAIndex);

  // Пересечение Средней телом свечи, свеча восходящая - покупаем, см. Пр.1
  if ((FOpen[0] <=aMA) and (FClose[0] >aMA)) then
  begin
    result:=1;
    exit;
  end;

  // Пересечение Средней телом свечи, свеча нисходящая - продаем, см. Пр.1
  if ((FOpen[0] >=aMA) and (FClose[0] <aMA)) then
  begin
    result:=-1;                       
    exit;
  end;

  // Касание Средней нижней тенью свечи - ордер на покупку,см. Пр.2
  // Касанием считаем пересечение, когда хвост выглядывает не более чем на 10pt
  if (FOpen[0] > aMA) and (FClose[0] > aMA) and (FLow[0] <=aMA) {and (aMA-FLow[0]<0.0010) }then
  begin
    result:=2;
    exit;
  end;
   //  else result:=(-3); // (продаем,  см. Пр.11)  !!! ТУТ НАДО ДОРАБОТАТЬ УСЛОВИЕ СИЛЬНОГО НАКЛОНА СРЕДНЕЙ

  // Касание Средней верхней тенью свечи - ордер на продажу
  // Касанием считаем пересечение, когда хвост выглядывает не более чем на 10pt
  if (FOpen[0] < aMA) and (FClose[0] < aMA) and (FHigh[0] >=aMA) {and (FHigh[0]-aMA<0.0010) }then
  begin
    result:=-2;
    exit;
  end;
   //  else result:=( 3);  // (покупаем, см. Пр.11)  !!! ТУТ НАДО ДОРАБОТАТЬ УСЛОВИЕ СИЛЬНОГО НАКЛОНА СРЕДНЕЙ

(*
  if ((FHigh[1]>FHigh[2]) and (FHigh[1]>FHigh[3]) and (FHigh[1]>FHigh[4])) then
  begin
    result:=( 4); // Последняя свеча выше предыдущих трех (покупаем, см. Пр.10)
    exit;
  end;

  if (( FLow[1]< FLow[2]) and ( FLow[1]< FLow[3]) and ( FLow[1]< FLow[4])) then
  begin
    result:=(-4); // Последняя свеча выше предыдущих трех (покупаем, см. Пр.10)
    exit;
  end;
*)
end;

function TStockTraderAverageBreakthrough.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderAverageBreakthrough.TryOpenOrder(aOrder: IStockOrder; const aTime: TDateTime);
var
  aOP,aSL,aTP: TSCRealNumber;
  aSignalToOpen: integer;
  aDelete: boolean;
begin
  aSignalToOpen:=SignalToOpen(aTime);

  // Если имеем ОТЛОЖЕННЫЙ ордер и есть сигнал на новой свече, то:
  if (aOrder<>nil) and (aOrder.GetState=osPending) and (aSignalToOpen<>0) then
  begin
    aDelete:=false;

    if ((aOrder.GetKind=okBuy) and (aSignalToOpen <0)) then // 1) Если возник противоположный сигнал (см. Пр.3),
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'Возник противоположный сигнал Sell (см. Пр.3)');
    end
    else if((aOrder.GetKind=okSell) and (aSignalToOpen >0)) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'Возник противоположный сигнал Buy (см. Пр.3)');
    end
     // 2) Если возник сигнал в ту же сторону, что и прежде (см. Пр.4),
    else if (aOrder.GetKind=okBuy) and (aSignalToOpen >0) and  (aOrder.GetPendingOpenPrice>FHigh[0]+aOrderMargin+Spread) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'Возник сигнал Buy в ту же сторону, переоткрываемся (см. Пр.4)');
    end
    else if (aOrder.GetKind=okSell) and  (aSignalToOpen <0) and (aOrder.GetPendingOpenPrice<FLow[0]-aOrderMargin) then
    begin
      aDelete:=true;
      GetBroker.AddMessage(aOrder,'Возник сигнал Sell в ту же сторону, переоткрываемся (см. Пр.4)');      
    end;

    if aDelete then
      aOrder.RevokePending //удалем прежний ордер
    else
      exit;
  end;

  if (aSignalToOpen >0) then
  begin
    aOP := FHigh[0]+aOrderMargin+Spread;
    //стоп ставится на противоположном конце -(+) 5 п. той же свечи.
    aSL := FLow[0]-aOrderMargin;
    aTP := 0; //TP=OP+1*(OP-SL);
    if aOrder=nil then
      aOrder:=CreateEmptyOrder;
    aOrder.OpenAt(GetSymbol,okBuy,aOP,GetRecommendedRate,aSL,aTP,0); // ПОКУПКА
    AddMessageAndSetMark(aOrder,mkArrowUp,'Сработал сигнал Buy')
  end
  else if (aSignalToOpen <0) then
  begin
    aOP := FLow[0]-aOrderMargin;
    //стоп ставится на противоположном конце -(+) 5 п. той же свечи.
    aSL := FHigh[0]+aOrderMargin+Spread;
    aTP := 0; //TP=OP-1*(SL-OP);
    if aOrder=nil then
      aOrder:=CreateEmptyOrder;
    aOrder.OpenAt(GetSymbol,okSell,aOP,GetRecommendedRate,aSL,aTP,0); // ПРОДАЖА
    AddMessageAndSetMark(aOrder,mkArrowDown,'Сработал сигнал Sell')
  end;
end;

procedure TStockTraderAverageBreakthrough.MoveSLToProfitablePoint(const aOrder: IStockOrder; const aTime: TDateTime);
var
  aValue: TStockRealNumber;
begin
  // 1) Если закрытие свечи произошло выше/ниже  цены открытия позиции на 10 пунктов и более, то ставим б/у
  if  (GetExpectedLoss(aOrder)>0) and (aOrder.GetCurrentProfit>=GetBroker.PointToPrice(GetSymbol,10)) then
  begin
    aValue:=aOrder.GetOpenPrice+OrderKindSign[aOrder.GetKind]*aOrderMargin;
    if MoveStopLossCloser(aOrder, aValue) then
      GetBroker.AddMessage(aOrder,'Поставили стоп в б/у');
  end;
end;

procedure TStockTraderAverageBreakthrough.AnalyzeOpenedOrder(const aOrder: IStockOrder; const aTime: TDateTime);
begin
  // 1) Если закрытие свечи произошло выше/ниже  цены открытия позиции на 10 пунктов и более, то ставим б/у
  MoveSLToProfitablePoint(aOrder,aTime);
  

  if (aTime-aOrder.GetOpenTime>=1-1/24) then     // Прошел 1 день после открытия позиции
  // 2) Передвигаем стоп на минимум/максимум двух последних свечей, но только в сторону его уменьшения
  begin
    if (aOrder.GetKind=okBuy) then
    begin
      if MoveStopLossCloser(aOrder,Min(FLow[0],FLow[1])) then
        GetBroker.AddMessage(aOrder,'Подтянули Buy-стоп на  минимум двух последних свечей');
    end
    else begin
      if MoveStopLossCloser(aOrder,Max(FHigh[0],FHigh[1])) then
        GetBroker.AddMessage(aOrder,'Подтянули Sell-стоп на максимум двух последних свечей');
    end;
  end;

  if (aTime-aOrder.GetOpenTime>=2-1/24) then    // Прошло 2 дня после открытия позиции
    if GetExpectedLoss(aOrder)>0 then
    // 3) Если после открытия позиции не можем поджать в безубыток, и две след.
    //    Свечи заканчиваются хуже цены открытия позиции, ставим тейк 5 п.
    begin
      if (aOrder.GetKind=okBuy) then
      begin
        //aOrder.SetTakeProfit(aOrder.GetOpenPrice+Spread+aOrderMargin);
        //GetBroker.AddMessage(aOrder,'Не можем поджать в б/у. Поставили Buy-профит +5pt');
      end
      else begin
        //aOrder.SetTakeProfit(aOrder.GetOpenPrice-aOrderMargin);
        //GetBroker.AddMessage(aOrder,'Не можем поджать в б/у. Поставили Sell-профит +5pt');
      end;
    end;
end;

procedure TStockTraderAverageBreakthrough.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  aOrder: IStockOrder;
begin
  RemoveClosedOrders;

  FMAIndex:=FMA.GetInputData.FindExactMatched(aTime);
  if FMAIndex=-1 then
    raise EAlgoError.Create;

  //Раньше нельзя начинать
  if FMAIndex<max(FMA.GetPeriod,4) then
    exit;

  if aTime<EncodeDate(2006,1,1) then
    exit;

//      if DateTimeToStr(aTime)='20.01.2006 22:59:00' then
//        Pause(DateTimeToStr(aTime));


  // Входим на каждой дневной свече только один раз при её открытии
  if (not FPassedTimes.Lookup(Trunc(aTime))) then
    if ((HourOfTheDay(aTime)=22) and (MinuteOfTheHour(aTime)=59)) or //желательно попасть в 22:59
       ((HourOfTheDay(aTime)=23))  then
    begin
      //Заполняем последние 4 свечки
      for i := 0 to 4 do
      begin
        with FMA.GetInputData[FMAIndex-i] do
        begin
          FOpen[i]:=DataOpen;
          FHigh[i]:=DataHigh;
          FLow[i]:=DataLow;
          FClose[i]:=DataClose;
        end;
      end;

      //Перебираем все отложенные и открытые ордера
      for i := 0 to GetOrders.Count-1 do
      begin
        aOrder:=GetOrders[i];
        if aOrder.GetState =osPending then
        begin
          TryOpenOrder(aOrder,aTime);
        end
        else if aOrder.GetState = osOpened then
        begin
          AnalyzeOpenedOrder(aOrder,aTime);
        end;
      end;

      //Если нет текущих открытых ордеров, то пытаемся открыть новый
      RemoveClosedOrders;
      if GetOrders.Count=0 then
      begin
        TryOpenOrder(nil,aTime);
      end;

      //Добавляем в словарь запись о том, что эту минуту мы уже обработали, чтобы
      //следующий раз мы опять не открылись
      FPassedTimes.Add(Trunc(aTime),true);
    end;

  //На открытие каждой новой свечи повторяем еще раз процедуру со б/у
  //У Бапишпольца как-то непонятно написано, то-ли надо это делать в конце суток, то ли в начале
  //У него срабатывает б/у уже в следующих сутках
  if HourOfTheDay(aTime) in [23,0] then
  begin
    //Перебираем все отложенные и открытые ордера
    for i := 0 to GetOrders.Count-1 do
    begin
      aOrder:=GetOrders[i];
      if aOrder.GetState = osOpened then
        //Если б/у так и не выставили, пробуем еще разок, только на следующие сутки
        if (GetExpectedLoss(aOrder)>0) and (aTime-aOrder.GetPendingOpenTime>1)  then
        begin
          MoveSLToProfitablePoint(aOrder,aTime);
          if GetExpectedLoss(aOrder)<0 then
            GetBroker.AddMessage(aOrder,'Cтоп в б/у был подвинут после закрытия рынка (22:59)');
        end;
    end;

    FPassedTimes2.Add(Trunc(aTime),true);
  end;
end;

procedure TStockTraderAverageBreakthrough.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

function TStockTraderAverageBreakthrough.GetOrderProperties(const aOrder: IStockOrder): TStockOrderProperties;
var
  i: integer;
begin
  i:=aOrder.GetAttributes.IndexOf(IStockOrderProperties);
  if i=-1 then
    raise EAlgoError.Create;

  result:=(aOrder.GetAttributes.Items[i] as IStockOrderProperties).GetObject;
end;

procedure TStockTraderAverageBreakthrough.InitMA(const aMA:ISCIndicatorMA; it:TStockTimeInterval);
begin
  aMA.SetMAMethod(mamExponential);
  aMA.SetShift(3);
  aMA.SetPeriod(3);
  aMA.SetApplyTo(atClose);
end;



{ TStockOrderProperties }

function TStockOrderProperties.GetObject: TStockOrderProperties;
begin
  result:=self;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Average Breakthrough',TStockTraderAverageBreakthrough,IStockTraderAverageBreakthrough);
end.




