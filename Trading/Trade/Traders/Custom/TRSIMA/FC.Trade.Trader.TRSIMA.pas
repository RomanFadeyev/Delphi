unit FC.Trade.Trader.TRSIMA;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics,Collections.Map;

type
  IStockTraderTRSIMA = interface
  ['{AF7E6D65-2FCF-4038-A1FD-F3D43AEEA909}']
  end;

  TStockTraderTRSIMA = class (TStockTraderBase,IStockTraderTRSIMA)
  private
    FRSI_M15_Black: ISCIndicatorTRSIMA;
    FRSI_M15_Blue: ISCIndicatorTRSIMA;
    FRSI_M15_Pink: ISCIndicatorTRSIMA;

    FRSI_H1_Black: ISCIndicatorTRSIMA;
    FRSI_H1_Blue: ISCIndicatorTRSIMA;
    FRSI_H1_Pink: ISCIndicatorTRSIMA;

    FRSI_H4_Black: ISCIndicatorTRSIMA;
    FRSI_H4_Blue: ISCIndicatorTRSIMA;
    FRSI_H4_Pink: ISCIndicatorTRSIMA;

    FRSI_D1_Black: ISCIndicatorTRSIMA;
    FRSI_D1_Blue: ISCIndicatorTRSIMA;
    FRSI_D1_Pink: ISCIndicatorTRSIMA;

    //FRSI_M15: ISCIndicatorTRSIMA;
    //FMA84_M1 : ISCIndicatorMA;
    FLastOrderH1Index: integer;
    FSkipH1Index : integer;

    FOrderBuyH1Signals: TMap<double,boolean>;
    FOrderSellH1Signals: TMap<double,boolean>;
    FLastUpdateTime  : TDateTime;

    FPropIncreateOrdersOnLoss     : TPropertyYesNo;
    FPropUseM15Signals     : TPropertyYesNo;
  protected
    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string); overload;
    procedure AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string; aMessageColor: TColor=clDefault);overload;

    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    function GetRecommendedLots: TStockOrderLots; override;

    procedure Calculate_H1(const aTime: TDateTime);
    procedure Calculate_M15(const aTime: TDateTime);
  public
    procedure SetProject(const aValue : IStockProject); override;

    procedure OnBeginWorkSession; override;

    function GetSignals(const aRSI_Black: ISCIndicatorTRSIMA; aRSI_Blue: ISCIndicatorTRSIMA; aRSI_Pink: ISCIndicatorTRSIMA; aIndex: integer; aShowAlerts: boolean; out aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean): integer;

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

{ TStockTraderTRSIMA }

procedure TStockTraderTRSIMA.AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string;
  aMessageColor: TColor);
begin
  GetBroker.AddMessage(aMessage,aMessageColor);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

constructor TStockTraderTRSIMA.Create;
begin
  inherited Create;

  FPropIncreateOrdersOnLoss   := TPropertyYesNo.Create('Lots\Dynamic','Increase on Loss',self);
  FPropIncreateOrdersOnLoss.Value:=False;

  FPropUseM15Signals:=TPropertyYesNo.Create('','Use M15 Signals',self);

  FOrderBuyH1Signals:=TMap<double,boolean>.Create;
  FOrderSellH1Signals:=TMap<double,boolean>.Create;

  RegisterProperties([FPropIncreateOrdersOnLoss,FPropUseM15Signals]);
//  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
end;

destructor TStockTraderTRSIMA.Destroy;
begin
  inherited;
  FreeAndNil(FOrderSellH1Signals);
  FreeAndNil(FOrderBuyH1Signals);
end;

procedure TStockTraderTRSIMA.Dispose;
begin
  inherited;
  FreeAndNil(FPropIncreateOrdersOnLoss);
  FreeAndNil(FPropUseM15Signals);
end;


procedure TStockTraderTRSIMA.OnBeginWorkSession;
begin
  inherited;
  FLastOrderH1Index:=0;
  FSkipH1Index:=0;
  FLastUpdateTime:=0;
  FOrderBuyH1Signals.Clear;
  FOrderSellH1Signals.Clear;
end;

procedure TStockTraderTRSIMA.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderTRSIMA.SetProject(const aValue: IStockProject);
var
  aCreated: boolean;
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //M15
    FRSI_M15_Black:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorTRSIMA,'RSI_BLACK'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M15_Black.SetPeriod(12);
      FRSI_M15_Black.GetTrendCorrectionMA.SetPeriod(55);
      FRSI_M15_Black.SetTrendCorrectionEnabled(true);
      FRSI_M15_Black.GetMA.SetPeriod(21);
      FRSI_M15_Black.SetColor(clBlack);
    end;

    FRSI_M15_Blue:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorTRSIMA,'RSI_BLUE'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M15_Blue.SetPeriod(12);
      FRSI_M15_Blue.GetTrendCorrectionMA.SetPeriod(84);
      FRSI_M15_Blue.SetTrendCorrectionEnabled(true);
      FRSI_M15_Blue.GetMA.SetPeriod(21);
      FRSI_M15_Blue.SetColor(clBlue);
    end;

    FRSI_M15_Pink:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorTRSIMA,'RSI_PINK'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M15_Pink.SetPeriod(12);
      FRSI_M15_Pink.GetTrendCorrectionMA.SetPeriod(1);
      FRSI_M15_Pink.SetTrendCorrectionEnabled(false);
      FRSI_M15_Pink.GetMA.SetPeriod(21);
      FRSI_M15_Pink.SetColor(clWebPlum);
    end;


    //H1
    FRSI_H1_Black:=CreateOrFindIndicator(aValue.GetStockChart(sti60),ISCIndicatorTRSIMA,'RSI_BLACK'+'_'+StockTimeIntervalNames[sti60],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H1_Black.SetPeriod(12);
      FRSI_H1_Black.GetTrendCorrectionMA.SetPeriod(55);
      FRSI_H1_Black.SetTrendCorrectionEnabled(true);
      FRSI_H1_Black.GetMA.SetPeriod(21);
      FRSI_H1_Black.SetColor(clBlack);
    end;

    FRSI_H1_Blue:=CreateOrFindIndicator(aValue.GetStockChart(sti60),ISCIndicatorTRSIMA,'RSI_BLUE'+'_'+StockTimeIntervalNames[sti60],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H1_Blue.SetPeriod(12);
      FRSI_H1_Blue.GetTrendCorrectionMA.SetPeriod(84);
      FRSI_H1_Blue.SetTrendCorrectionEnabled(true);
      FRSI_H1_Blue.GetMA.SetPeriod(21);
      FRSI_H1_Blue.SetColor(clBlue);
    end;

    FRSI_H1_Pink:=CreateOrFindIndicator(aValue.GetStockChart(sti60),ISCIndicatorTRSIMA,'RSI_PINK'+'_'+StockTimeIntervalNames[sti60],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H1_Pink.SetPeriod(12);
      FRSI_H1_Pink.GetTrendCorrectionMA.SetPeriod(1);
      FRSI_H1_Pink.SetTrendCorrectionEnabled(false);
      FRSI_H1_Pink.GetMA.SetPeriod(21);
      FRSI_H1_Pink.SetColor(clWebPlum);
    end;

    //H4
    FRSI_H4_Black:=CreateOrFindIndicator(aValue.GetStockChart(sti240),ISCIndicatorTRSIMA,'RSI_BLACK'+'_'+StockTimeIntervalNames[sti240],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H4_Black.SetPeriod(12);
      FRSI_H4_Black.GetTrendCorrectionMA.SetPeriod(55);
      FRSI_H4_Black.SetTrendCorrectionEnabled(true);
      FRSI_H4_Black.GetMA.SetPeriod(21);
      FRSI_H4_Black.SetColor(clBlack);
    end;

    FRSI_H4_Blue:=CreateOrFindIndicator(aValue.GetStockChart(sti240),ISCIndicatorTRSIMA,'RSI_BLUE'+'_'+StockTimeIntervalNames[sti240],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H4_Blue.SetPeriod(12);
      FRSI_H4_Blue.GetTrendCorrectionMA.SetPeriod(84);
      FRSI_H4_Blue.SetTrendCorrectionEnabled(true);
      FRSI_H4_Blue.GetMA.SetPeriod(21);
      FRSI_H4_Blue.SetColor(clBlue);
    end;

    FRSI_H4_Pink:=CreateOrFindIndicator(aValue.GetStockChart(sti240),ISCIndicatorTRSIMA,'RSI_PINK'+'_'+StockTimeIntervalNames[sti240],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_H4_Pink.SetPeriod(12);
      FRSI_H4_Pink.GetTrendCorrectionMA.SetPeriod(1);
      FRSI_H4_Pink.SetTrendCorrectionEnabled(false);
      FRSI_H4_Pink.GetMA.SetPeriod(21);
      FRSI_H4_Pink.SetColor(clWebPlum);
    end;

    //D1
    FRSI_D1_Black:=CreateOrFindIndicator(aValue.GetStockChart(sti1440),ISCIndicatorTRSIMA,'RSI_BLACK'+'_'+StockTimeIntervalNames[sti1440],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_D1_Black.SetPeriod(12);
      FRSI_D1_Black.GetTrendCorrectionMA.SetPeriod(55);
      FRSI_D1_Black.SetTrendCorrectionEnabled(true);
      FRSI_D1_Black.GetMA.SetPeriod(21);
      FRSI_D1_Black.SetColor(clBlack);
    end;

    FRSI_D1_Blue:=CreateOrFindIndicator(aValue.GetStockChart(sti1440),ISCIndicatorTRSIMA,'RSI_BLUE'+'_'+StockTimeIntervalNames[sti1440],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_D1_Blue.SetPeriod(12);
      FRSI_D1_Blue.GetTrendCorrectionMA.SetPeriod(84);
      FRSI_D1_Blue.SetTrendCorrectionEnabled(true);
      FRSI_D1_Blue.GetMA.SetPeriod(21);
      FRSI_D1_Blue.SetColor(clBlue);
    end;

    FRSI_D1_Pink:=CreateOrFindIndicator(aValue.GetStockChart(sti1440),ISCIndicatorTRSIMA,'RSI_PINK'+'_'+StockTimeIntervalNames[sti1440],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_D1_Pink.SetPeriod(12);
      FRSI_D1_Pink.GetTrendCorrectionMA.SetPeriod(1);
      FRSI_D1_Pink.SetTrendCorrectionEnabled(false);
      FRSI_D1_Pink.GetMA.SetPeriod(21);
      FRSI_D1_Pink.SetColor(clWebPlum);
    end;

    //FRSI_M15:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorTRSIMA,'RSI'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorTRSIMA;

    //FMA84_M1:=CreateOrFindIndicator(aValue.GetStockChart(sti15),ISCIndicatorMA,'MA84'+'_'+StockTimeIntervalNames[sti15],true, aCreated) as ISCIndicatorMA;

    //Ничего не нашли, создадим нового эксперта
    if aCreated then
    begin
      //FMA84_M1.SetPeriod(84);
    end;
  end;
end;

function TStockTraderTRSIMA.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderTRSIMA.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

function TStockTraderTRSIMA.GetRecommendedLots: TStockOrderLots;
var
  aOrders: IStockOrderCollection;
  i: Integer;
  x: integer;
begin
  result:=inherited GetRecommendedLots;

  if FPropIncreateOrdersOnLoss.Value then
  begin
    x:=1;
    aOrders:=GetBroker.GetAllOrders;
    for i :=aOrders.Count - 1 downto 0 do
    begin
      if aOrders[i].GetState=osClosed then
      begin
        if aOrders[i].GetCurrentProfit<0 then
        begin
          result:=result+aOrders[i].GetLots/x; //(inherited GetRecommendedLots/x);
          x:=x*2;
          if result>inherited GetRecommendedLots*5 then
            break;
        end
        else begin
          break;
        end;

      end;


    end;
  end;

  result:=Math.RoundTo(result,-3);
end;

function TStockTraderTRSIMA.GetSignals(const aRSI_Black: ISCIndicatorTRSIMA; aRSI_Blue, aRSI_Pink: ISCIndicatorTRSIMA; aIndex: integer; aShowAlerts: boolean; out aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean): integer;
var
  aRSIValueBlack,aRSIValueBlue,aRSIValuePink: TSCRealNumber;
  aRSIValueBlack_1,aRSIValueBlue_1,aRSIValuePink_1: TSCRealNumber;
begin
  result:=0;
  aRSIValueBlack:=aRSI_Black.GetValue(aIndex);
  aRSIValueBlue:=aRSI_Blue.GetValue(aIndex);
  aRSIValuePink:=aRSI_Pink.GetValue(aIndex);

  aRSIValueBlack_1:=aRSI_Black.GetValue(aIndex-1);
  aRSIValueBlue_1:=aRSI_Blue.GetValue(aIndex-1);
  aRSIValuePink_1:=aRSI_Pink.GetValue(aIndex-1);

  //Анализ BUY
  aRSIChangedBlack:=(aRSIValueBlack>=50) and (aRSIValueBlack_1<50);
  aRSIChangedBlue:= (aRSIValueBlue>=50) and (aRSIValueBlue_1<50);
  aRSIChangedPink:= (aRSIValuePink>=50) and (aRSIValuePink_1<50);

  if aRSIChangedBlack or aRSIChangedBlue or aRSIChangedPink then
  begin
    result:= integer(aRSIValueBlack>=50)+integer(aRSIValueBlue>=50)+integer(aRSIValuePink>=50);

    //Слишком далеко Pink, не похоже на правду
    if (result=2) and (aRSIValuePink<20) then
      result:=0;

    if result>=2  then
    begin
      //Если поменялся Blue или Black, и при этом Pink уже отчскочил от верхней границы, то игнорируем сигнал
      if (aRSIChangedBlack or aRSIChangedBlue) and (aRSI_Pink.GetLastSide(aIndex,1)=1) then
      begin
        if aShowAlerts then
          AddMessageAndSetMark(mkStop,'Reject opening BUY: RSI H1 on top',clRed);
        result:=0;
      end
    end;
  end;

  if result<>0 then
    exit;


  //Анализ SELL
  aRSIChangedBlack:=(aRSIValueBlack<=50) and (aRSIValueBlack_1>50);
  aRSIChangedBlue:= (aRSIValueBlue<=50) and (aRSIValueBlue_1>50);
  aRSIChangedPink:= (aRSIValuePink<=50) and (aRSIValuePink_1>50);

  if aRSIChangedBlack or aRSIChangedBlue or aRSIChangedPink then
  begin
    result:=integer(aRSIValueBlack<=50) + integer(aRSIValueBlue<=50) + integer(aRSIValuePink<=50);
    if (result=2) and (aRSIValuePink>80) then
      result:=0;

    if result>=2 then
    begin
      //Если поменялся Blue или Black, и при этом Pink уже отчскочил нижней от границы, то игнорируем сигнал
      if (aRSIChangedBlack or aRSIChangedBlue) and (aRSI_Pink.GetLastSide(aIndex,1)=-1) then
      begin
        if aShowAlerts then
          AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI H1 on bottom',clRed);
        result:=0;
      end;
    end;

    result:=-result;
  end;
end;

procedure TStockTraderTRSIMA.UpdateStep2(const aTime: TDateTime);
begin
  Calculate_H1(aTime);
  Calculate_M15(aTime);
end;

procedure TStockTraderTRSIMA.Calculate_H1(const aTime: TDateTime);
var
//  aIdxM5: integer;
//  aIdxM15: integer;
  aIdxH1 : integer;
  //aIdxH4 : integer;
  //aIdxD1 : integer;
  aOpen,aClose : integer;
  aSignals,aSignalsPrev: integer;
  i: integer;
  aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean;
  aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2: boolean;
begin
  RemoveClosedOrders;

  //Не чаще одного раза в минуту
  if (aTime-FLastUpdateTime)*MinsPerDay<1 then
    exit;

  FLastUpdateTime:=aTime;


  aIdxH1:=TStockDataUtils.FindBar(FRSI_H1_Black.GetInputData,aTime,sti60);
  if aIdxH1=FSkipH1Index then
    exit;
  if aIdxH1=FLastOrderH1Index then
    exit;

  //aIdxH4:=TStockDataUtils.FindBar(FRSI_H4_Black.GetInputData,aTime,sti240);

  //if aIdxH4<=2 then
  //  exit;

  //aIdxD1:=TStockDataUtils.FindBar(FRSI_D1_Black.GetInputData,aTime,sti1440);
  //if aIdxD1<=2 then
  //  exit;

  if (aIdxH1<>-1) and (aIdxH1>=100) then
  begin
    aOpen:=0;
    aClose:=0;

    aSignals:=GetSignals(FRSI_H1_Black,FRSI_H1_Blue,FRSI_H1_Pink,aIdxH1,true,aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink);

    //open buy, close sell -------------------------------------------
    if aSignals>0 then
    begin
      if (aIdxH1-FLastOrderH1Index)<=2 then
      begin
        FSkipH1Index:=aIdxH1;
        AddMessageAndSetMark(mkStop,'Reject opening BUY: too close to previous order',clRed);
        aSignals:=0;
      end;

      //нужно проверить, когда был последний переворот, если ближе 12 часов - пропускаем
      if (aSignals=2) then
      begin
        for i:=aIdxH1-1 downto aIdxH1-12 do
        begin
          aSignalsPrev := GetSignals(FRSI_H1_Black,FRSI_H1_Blue,FRSI_H1_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          if (aSignalsPrev<=-2) then
          begin
            AddMessageAndSetMark(mkStop,'Reject opening BUY: Too close to previous opposite signal',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;

      //Посмотрим, а не остаточное ли это явление, Pink может быть уже давно вверху, и тогда покупать опасно
      if (aSignals=2) and (aRSIChangedBlue) and not (aRSIChangedPink) and  (FRSI_H1_Pink.GetValue(aIdxH1)>50) then
      begin
        for i:=aIdxH1-1 downto 0 do
        begin
          //Если где-то Pink был снизу - нормально
          if FRSI_H1_Pink.GetValue(i)<50 then
            break;

          aSignalsPrev := GetSignals(FRSI_H1_Black,FRSI_H1_Blue,FRSI_H1_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          //Нашли предыдущее пересечение вверх, а за это время Pink так и не опустился
          if (aSignalsPrev>=2) then
          begin
            AddMessageAndSetMark(mkAttention,'Reject opening BUY: Pink is too high and too long',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;



      if aSignals>=2 then
      begin
        aOpen:=1;
        aClose:=-1;
      end;
    end;



    if aSignals<0 then
    begin
      if (aIdxH1-FLastOrderH1Index)<=2 then
      begin
        FSkipH1Index:=aIdxH1;
        AddMessageAndSetMark(mkStop,'Reject opening SELL: too close to previous order',clRed);
        aSignals:=0;
      end;

      if (aSignals=-2) then
      begin
        for i:=aIdxH1-1 downto aIdxH1-12 do
        begin
          aSignalsPrev := GetSignals(FRSI_H1_Black,FRSI_H1_Blue,FRSI_H1_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          if (aSignalsPrev>=2) then
          begin
            AddMessageAndSetMark(mkStop,'Reject opening SELL: Too close to previous opposite signal',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;

      //Посмотрим, а не остаточное ли это явление, Pink может быть уже давно внизу, и тогда продавать опасно
      if (aSignals=-2) and (aRSIChangedBlue) and not (aRSIChangedPink) and  (FRSI_H1_Pink.GetValue(aIdxH1)<50) then
      begin
        for i:=aIdxH1-1 downto 0 do
        begin
          //Если где-то Pink был сверху - нормально
          if FRSI_H1_Pink.GetValue(i)>50 then
            break;

          aSignalsPrev := GetSignals(FRSI_H1_Black,FRSI_H1_Blue,FRSI_H1_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          //Нашли предыдущее пересечение вниз, а за это время Pink так и не поднялся
          if (aSignalsPrev<=-2) then
          begin
            AddMessageAndSetMark(mkAttention,'Reject opening SELL: Pink is too low and too long',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;


      if aSignals<=-2 then
      begin
          aOpen:=-1;
          aClose:=1;
      end;
    end;

    if aClose=-1 then
      CloseAllSellOrders('')
    else if aClose=1 then
      CloseAllBuyOrders('');

    if aOpen<>0 then
    begin
      //BUY
      if (aOpen=1) {and (LastOrderType<>lotBuy)} then
      begin
        FOrderSellH1Signals.Clear;
        FOrderBuyH1Signals.Add(OpenOrder(okBuy).GetLots,true);
        FLastOrderH1Index:=aIdxH1;
      end
      //SELL
      else if (aOpen=-1) {and (LastOrderType<>lotSell)} then
      begin
        FOrderBuyH1Signals.Clear;
        FOrderSellH1Signals.Add(OpenOrder(okSell).GetLots,true);
        FLastOrderH1Index:=aIdxH1;
      end;
    end;
  end;
end;


procedure TStockTraderTRSIMA.Calculate_M15(const aTime: TDateTime);
var
  aIdxM15: integer;
  aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean;
  aSignals: integer;
  it: TMapIterator<double,boolean>;
begin

  if FPropUseM15Signals.Value and ((FOrderBuyH1Signals.Count>0) or (FOrderSellH1Signals.Count>0)) then
  begin
    aIdxM15:=TStockDataUtils.FindBar(FRSI_M15_Black.GetInputData,aTime,sti15);

    if (aIdxM15<>-1) then
    begin
      aSignals:=GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,aIdxM15,false,aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink);

      //Buy
      if (aSignals=3) then
      begin
        FOrderBuyH1Signals.GetFirst(it);
        while it.Valid do
        begin
          if not it.Value then
            OpenOrder(okBuy,it.Key,0,0,0);
          FOrderBuyH1Signals.GetNext(it);
        end;
        FOrderBuyH1Signals.SetValueToAllKeys(true);

        CloseAllSellOrders('M15 signal');
        FOrderSellH1Signals.SetValueToAllKeys(false);
      end;

      //Sell
      if (aSignals=-3) then
      begin
        FOrderSellH1Signals.GetFirst(it);
        while it.Valid do
        begin
          if not it.Value then
            OpenOrder(okSell,it.Key,0,0,0);
          FOrderSellH1Signals.GetNext(it);
        end;
        FOrderSellH1Signals.SetValueToAllKeys(true);

        CloseAllBuyOrders('M15 signal');
        FOrderBuyH1Signals.SetValueToAllKeys(false);
      end;
    end;
  end;
end;

procedure TStockTraderTRSIMA.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','TRSIMA',TStockTraderTRSIMA,IStockTraderTRSIMA);
end.




