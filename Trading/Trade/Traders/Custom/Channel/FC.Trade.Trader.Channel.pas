{-----------------------------------------------------------------------------
 Author:    Roman  Fadeyev
 Purpose:   Трейдер, работающий в границах канала. Канал определяется границами
            Болинджера

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Trader.Channel;
{$I Compiler.inc}

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderChannel = interface
  ['{3E5F7C15-DD48-451C-A218-3BFF5B7DFFA8}']
  end;

  //Ссылка на хеджирующий ордер
  IStockHedgingLink = interface (ISCAttribute)
  ['{7EBFAB30-9127-427D-98FA-E3E5C3BFC2F8}']
    function GetHedgingOrder: IStockOrder;
  end;

  //ПРизнак того, что этот ордер хеджирующий
  IStockHedgeSign = interface (ISCAttribute)
  ['{393255E4-DDC9-4134-A1DE-91885D85F7BE}']
  end;

  TStockTraderChannel = class (TStockTraderBase,IStockTraderChannel)
  private
    FExpertRSI   : ISCExpertRSI;
    FBarHeightD1 : ISCIndicatorBarHeight;
    FMA21_M1,FMA55_M1: ISCIndicatorMA;
    FLastOpenOrderTime : TDateTime;
  protected
    function  CreateExpertRSI(const aChart: IStockChart): ISCExpertRSI;
    function  CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
    function  CreateMA_M1(const aChart: IStockChart; aPeriod: integer): ISCIndicatorMA;

    //Считает, на какой примерно цене сработает Stop Loss или Trailing Stop
    function  GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;

    //Считает, какой убыток будет, если закроется по StopLoss или Trailing Stop
    function  GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;

    //Дать характеристики хеджирующего ордера для указанного ордера
    procedure CalcHedgingValues(const aOrder: IStockOrder;  out aOP, aSL,aTP,aTS: TStockRealNumber);
    procedure SetHedgingOrderValues(const aOrder,aHedgingOrder: IStockOrder);

    //Для указанного ордера дать его хеджирующий ордер (если есть)
    function  GetHedgingOrder(const aOrder: IStockOrder): IStockOrder;

    //Установка Trailing Stop на основании свойств трейдера
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); override;

    procedure CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
    function GetRecommendedLots: TStockOrderLots; override;

    procedure SetTP(const aOrder: IStockOrder; const aTP: TStockRealNumber;  const aComment: string);
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;
    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    function OpenOrder(aKind: TStockOrderKind;const aComment: string=''): IStockOrder;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

  TStockHedgeLink = class (TNameValuePersistentObjectRefCounted,IStockHedgingLink,ISCAttribute)
  private
    FHedgingOrder: IStockOrder;
  public
    function GetHedgingOrder: IStockOrder;

    constructor Create(aHedgingOrder: IStockOrder);
  end;

  TStockHedgeSign = class (TNameValuePersistentObjectRefCounted,IStockHedgeSign,ISCAttribute)
  end;

implementation
  uses DateUtils,Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils;

{ TStockTraderChannel }

procedure TStockTraderChannel.CalcHedgingValues(const aOrder: IStockOrder; out aOP, aSL, aTP, aTS: TStockRealNumber);
begin
  //Open
  aOP:=GetBroker.RoundPrice(aOrder.GetSymbol,GetExpectedStopLossPrice(aOrder));
  //Stop Loss
  aSL:=GetBroker.RoundPrice(aOrder.GetSymbol, aOP +(aOrder.GetOpenPrice-aOrder.GetStopLoss) / 2);
  //Take Profit
  aTP:=GetBroker.RoundPrice(aOrder.GetSymbol, aOP -
                  OrderKindSign[aOrder.GetKind]*
                    (GetExpectedLoss(aOrder)+
                      GetBroker.PointToPrice(aOrder.GetSymbol, GetBroker.GetMarketInfo(GetSymbol).Spread)) / 2);
  //Trailing Stop
  aTS:=GetBroker.RoundPrice(aOrder.GetSymbol,GetExpectedLoss(aOrder) / 2);
end;

procedure TStockTraderChannel.CloseProfitableOrders(aKind: TSCOrderKind;const aComment: string);
var
  i: Integer;
  aOrders : IStockOrderCollection;
begin
  aOrders:=GetOrders;
  for i := aOrders.Count- 1 downto 0 do
  begin
    if (aOrders[i].GetKind=aKind) and (aOrders[i].GetCurrentProfit>0) then
      CloseOrder(aOrders[i],aComment);
  end;
end;

constructor TStockTraderChannel.Create;
begin
  inherited Create;

  //UnRegisterProperties([PropLotDefaultRateSize,PropLotDynamicRate]);
end;

function TStockTraderChannel.CreateBarHeightD1(const aChart: IStockChart): ISCIndicatorBarHeight;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorBarHeight,'BarHeightD1',true, aCreated) as ISCIndicatorBarHeight;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetPeriod(3);
    Result.SetBarHeight(bhHighLow);
  end;
end;

function TStockTraderChannel.CreateMA_M1(const aChart: IStockChart;aPeriod: integer): ISCIndicatorMA;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCIndicatorMA,'MA'+IntToStr(aPeriod)+'_M1',true, aCreated) as ISCIndicatorMA;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
    Result.SetPeriod(aPeriod);
  end;
end;


destructor TStockTraderChannel.Destroy;
begin
  inherited;
end;

procedure TStockTraderChannel.Dispose;
begin
  inherited;
end;

function TStockTraderChannel.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if aOrder.GetState=osOpened then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;

function TStockTraderChannel.GetHedgingOrder(const aOrder: IStockOrder): IStockOrder;
var
  i: integer;
begin
  result:=nil;
  i:=aOrder.GetAttributes.IndexOf(IStockHedgingLink);
  if i<>-1 then
    result:=(aOrder.GetAttributes[i] as IStockHedgingLink).GetHedgingOrder;
end;

function TStockTraderChannel.GetRecommendedLots: TStockOrderLots;
var
  aDayVolatility,aDayVolatilityM,k: TStockRealNumber;
begin
  if not PropLotDynamicRate.Value then
    exit(inherited GetRecommendedLots);

  aDayVolatility:=FBarHeightD1.GetValue(FBarHeightD1.GetInputData.Count-1);
  //Считаем какая волатильность в деньгах у нас была последние дни
  aDayVolatilityM:=GetBroker.PriceToMoney(GetSymbol,aDayVolatility,1);
  //Считаем, сколько таких волатильностей вынесет наш баланс
  k:=(GetBroker.GetEquity/aDayVolatilityM);
  //Теперь берем допустимый процент
  result:=RoundTo(k*PropLotDynamicRateSize.Value/100,-2);
end;

function TStockTraderChannel.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

function TStockTraderChannel.CreateExpertRSI(const aChart: IStockChart): ISCExpertRSI;
var
  aCreated: boolean;
begin
  result:=CreateOrFindIndicator(aChart,ISCExpertRSI,'ExpertRSI',true, aCreated) as ISCExpertRSI;

  //Ничего не нашли, создадим нового эксперта
  if aCreated then
  begin
  end;

  if IndexOfExpert(result)=-1 then
    AddExpert(result);
end;

procedure TStockTraderChannel.SetHedgingOrderValues(const aOrder, aHedgingOrder: IStockOrder);
var
  aSL,aTP,aTS,aOP: TStockRealNumber;
begin
  if aHedgingOrder.GetState<>osPending then
    raise EAlgoError.Create; //надо думать

  //Это небольшая оптимизация. Зачем редактировать хеджирующий ордер, если мы в профите
  //Есть, конечно, опасность резкого скачка, но иначе тормозит. В реале надо отключать
  if aOrder.GetCurrentProfit<0 then
  begin
    CalcHedgingValues(aOrder,aOP,aSL,aTP,aTS);

    if not SameValue(aOP,aHedgingOrder.GetPendingOpenPrice) then
    begin
      aHedgingOrder.SetPendingOpenPrice(aOP);
      aHedgingOrder.SetTakeProfit(aTP);
      aHedgingOrder.SetStopLoss(aSL);
      aHedgingOrder.SetTrailingStop(aTS);
    end;
  end;
end;

procedure TStockTraderChannel.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue=nil then
  begin
    FExpertRSI:=nil;
    while ExpertCount>0 do
      DeleteExpert(0);
  end;

  if aValue <> nil then
  begin
    //Создае нужных нам экспертов
    FExpertRSI:=CreateExpertRSI(aValue.GetStockChart(sti60));
    FBarHeightD1:=CreateBarHeightD1(aValue.GetStockChart(sti1440));

    FMA21_M1:= CreateMA_M1(aValue.GetStockChart(sti1),21);
    FMA55_M1:= CreateMA_M1(aValue.GetStockChart(sti1),55);
  end;
end;

procedure TStockTraderChannel.SetTP(const aOrder: IStockOrder;const aTP: TStockRealNumber;  const aComment: string);
var
  aNew : TStockRealNumber;
begin
  aNew:=GetBroker.RoundPrice(aOrder.GetSymbol,aTP);
  if not SameValue(aNew,aOrder.GetTakeProfit) then
  begin
    if aComment<>'' then
      GetBroker.AddMessage(aOrder,aComment);
    aOrder.SetTakeProfit(aNew);
  end;
end;

procedure TStockTraderChannel.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
var
  i: integer;
begin
  for I := 0 to aOrder.GetAttributes.Count- 1 do
    if Supports(aOrder.GetAttributes.Items[i],IStockHedgeSign) then
      exit;

  inherited;

end;

procedure TStockTraderChannel.UpdateStep2(const aTime: TDateTime);

function MACross(idx1: integer): integer;
var
  x1,x2: integer;
begin
  x1:=Sign(FMA21_M1.GetValue(idx1)-FMA55_M1.GetValue(idx1));
  x2:=Sign(FMA21_M1.GetValue(idx1-1)-FMA55_M1.GetValue(idx1-1));

  if x1=x2 then exit(0);
  result:=x1;
end;

var
  idx60,idx1: integer;
  aInputData : ISCInputDataCollection;
  aChart : IStockChart;
  aBarHeight : TSCRealNumber;
  aOpenedOrder,aOrder{, aHedgingOrder}: IStockOrder;
  i: Integer;
  aV: TStockRealNumber;
  aOpen : integer;
begin

  //Брокер может закрыть ордера и без нас. У нас в списке они останутся,
  //но будут уже закрыты. Если их не убрать, то открываться в этоу же сторону мы не
  //сможем, пока не будет сигнала от эксперта. Если же их удалить, сигналы
  //от эксперта в эту же сторону опять можно отрабатывать
  RemoveClosedOrders;

(*
  //Корректируем значения хеджирующего ордера
  for k := 0 to GetOrders.Count - 1 do
  begin
    aOrder:=GetOrders[k];
    if aOrder.GetState=osOpened then
    begin
      aHedgingOrder:=GetHedgingOrder(aOrder);
      if aHedgingOrder<>nil then
      begin
        Assert(aHedgingOrder<>aOrder);
        Assert(aHedgingOrder.GetKind<>aOrder.GetKind);
        if aHedgingOrder.GetState=osOpened then //Бывает редко, когда хеджирующий ордер открыли, а основной не закрыли
        begin
          aOrder.Close('Close because hedging order was opened (Close price must be )'+
                       PriceToStr(aOrder,GetExpectedStopLossPrice(aOrder)));
        end
        else
          SetHedgingOrderValues(aOrder,aHedgingOrder);
      end;
    end;
  end;
*)
  //Анализируем экcпертные оценки
  aChart:=GetParentStockChart(FExpertRSI);
  aInputData:=aChart.GetInputData;

  //idx1:=-1;
  //idx60:=-1;
    idx60:=aChart.FindBar(aTime);
    idx1:=GetParentStockChart(FMA21_M1).FindBar(aTime);


  if (idx60<>-1) and (idx1<>-1) then
  begin
    //Открываем ордер
    if MinutesBetween(GetBroker.GetCurrentTime,FLastOpenOrderTime)>60*3 then
    begin
      aOpen:=0;
      if MinuteOf(aTime) in [55..59] then
      begin
        if (FExpertRSI.GetGuessOpenBuy(idx60)=egSurely) then
        begin
          if MACross(idx1)=1 then
            aOpen:=1;
        end
        else if (FExpertRSI.GetGuessOpenSell(idx60)=egSurely) then
        begin
          if MACross(idx1)=-1 then
            aOpen:=-1;
        end
      end;

      if aOpen=0 then
      begin
        if (FExpertRSI.GetGuessOpenBuy(idx60-1)=egSurely) then
        begin
          if MACross(idx1)=1 then
            aOpen:=1;
        end
        else if (FExpertRSI.GetGuessOpenSell(idx60-1)=egSurely) then
        begin
          if MACross(idx1)=-1 then
            aOpen:=-1;
        end;
      end;



      //BUY
      if aOpen=1 then
      begin
        CloseProfitableOrders(okSell,('Trader: Open opposite'));
        aOpenedOrder:=OpenOrder(okBuy);
        FLastOpenOrderTime:=GetBroker.GetCurrentTime;
      end
      //SELL
      else if aOpen=-1 then
      begin
        CloseProfitableOrders(okBuy,('Trader: Open opposite'));
        aOpenedOrder:=OpenOrder(okSell);
        FLastOpenOrderTime:=GetBroker.GetCurrentTime;
      end;
    end;
  end;

  if not (MinuteOf(aTime) in [55..59])then
    idx60:=-1;

  if (idx60<>-1) then
  begin
    //Смотрим, не сказал ли нам Эксперт закрывать позиции
    if (FExpertRSI.GetGuessCloseBuy(idx60)=egSurely) then
    begin
      CloseProfitableOrders(okBuy,('Trader: Signal to close'));
    end
    else if (FExpertRSI.GetGuessCloseSell(idx60)=egSurely) then
    begin
      CloseProfitableOrders(okSell,('Trader: Signal to close'));
    end;

    //Смотрим, не сказал ли нам эксперт ставить безубыток
    if FExpertRSI.IsFlag(idx60,efSetNonLoss) then
    begin
      for i := GetOrders.Count-1 downto 0 do
      begin
        aOrder:=GetOrders[i];
        if aOrder.GetState=osOpened then
        begin
          if aOrder.GetCurrentProfit>0 then
          begin
            if (aOrder.GetCurrentProfit>FExpertRSI.GetAverageBarHeight(idx60)) or
               (GetBroker.GetCurrentTime-aOrder.GetOpenTime>2/24) then
            begin
              MoveStopLossCloser(aOrder,aOrder.GetOpenPrice);
            end;
          end
          else begin
            if (GetBroker.GetCurrentTime-aOrder.GetOpenTime<2/24) then
              //aOrder.SetTakeProfit(aOrder.GetOpenPrice)
            else //(aOrder.GetCurrentProfit>-FExpertRSI.GetAverageBarHeight(idx60)) or
              CloseOrder(aOrder,'Trader: Close unprofitable order');
          end;
        end;
      end;
    end;

    //Проверяем на наличие плачевных ордеров
    aBarHeight:=FExpertRSI.GetAverageBarHeight(idx60);
    for i := GetOrders.Count-1 downto 0 do
    begin
      aOrder:=GetOrders[i];
      if aOrder.GetState=osOpened then
      begin
        if aOrder.GetCurrentProfit<-aBarHeight*2 then
        begin
          //Ставим TP, который принесет нам убыток, но деватьcя некуда - надо закрыться хоть как-то
          if aOrder.GetKind=okBuy then
          begin
            if FExpertRSI.GetMainTrend(idx60)<0 then //Тренд вниз
            begin
              if aOrder.GetCurrentProfit<-aBarHeight*2 then
                aV:=aOrder.GetOpenPrice-aBarHeight/4
              else
                aV:=aOrder.GetOpenPrice-aBarHeight/2;
              SetTP(aOrder,aV,'Trader: Heavy situation... Let''s move TP...');
            end;
          end
          else begin
            if FExpertRSI.GetMainTrend(idx60)>0 then //Тренд вверх
            begin
              if aOrder.GetCurrentProfit<-aBarHeight*2 then
                aV:=aOrder.GetOpenPrice+aBarHeight/4
              else
                aV:=aOrder.GetOpenPrice+aBarHeight/2;

              SetTP(aOrder,aV,'Trader: Heavy situation... Let''s move TP...');
            end;
          end;
        end;
      end;
    end;
  end;

  if aOpenedOrder<>nil then
  begin
      //Assert(GetOrders.Items[GetOrders.Count-1]=aHedgingOrder);
      //GetOrders.Delete(GetOrders.Count-1);
  end;

(*  with OpenOrderAt(okBuy,GetBroker.GetCurrentPrice(GetSymbol,bpkBid),'1231231323') do
  try
    Close('');
  except
    on E:Exception do
     GetBroker.AddMessage(E.Message);
  end;*)
end;

procedure TStockTraderChannel.OnBeginWorkSession;
begin
  inherited;
  FLastOpenOrderTime:=0;
end;

function TStockTraderChannel.OpenOrder(aKind: TStockOrderKind;
  const aComment: string): IStockOrder;
begin
  try
    Result:=inherited OpenOrder(aKind,aComment);
  except
    on E:Exception do
      GetBroker.AddMessage(E.Message)
  end;
end;

{ TStockHedgeLink }

constructor TStockHedgeLink.Create(aHedgingOrder: IStockOrder);
begin
  inherited Create;
  FHedgingOrder:=aHedgingOrder;
end;

function TStockHedgeLink.GetHedgingOrder: IStockOrder;
begin
  result:=FHedgingOrder;
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','Channel',TStockTraderChannel,IStockTraderChannel);
end.





