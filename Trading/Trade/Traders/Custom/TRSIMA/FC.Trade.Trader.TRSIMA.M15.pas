unit FC.Trade.Trader.TRSIMA.M15;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderTRSIMA_M15 = interface
  ['{1D9D4481-8715-428F-A8D3-731B610EF67A}']
  end;

  TStockTraderTRSIMA_M15 = class (TStockTraderBase,IStockTraderTRSIMA_M15)
  private
    FRSI_M1_Black: ISCIndicatorTRSIMA;
    FRSI_M1_Blue: ISCIndicatorTRSIMA;
    FRSI_M1_Pink: ISCIndicatorTRSIMA;

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

    FLastOrderM15Index: integer;
    FSkipM15Index : integer;
    FOrderBuyM15Signals: integer;
    FOrderSellM15Signals: integer;
    FLastUpdateTime  : TDateTime;

    FPropIncreateOrdersOnLoss     : TPropertyYesNo;
  protected
    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string); overload;
    procedure AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string; aMessageColor: TColor=clDefault);overload;

    procedure SetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
    function GetRecommendedLots: TStockOrderLots; override;

    procedure Calculate(const aTime: TDateTime);
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

{ TStockTraderTRSIMA_M15 }

procedure TStockTraderTRSIMA_M15.AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string;
  aMessageColor: TColor);
begin
  GetBroker.AddMessage(aMessage,aMessageColor);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

constructor TStockTraderTRSIMA_M15.Create;
begin
  inherited Create;

  FPropIncreateOrdersOnLoss   := TPropertyYesNo.Create('Lots\Dynamic','Increase on Loss',self);
  FPropIncreateOrdersOnLoss.Value:=False;

  RegisterProperties([FPropIncreateOrdersOnLoss]);
//  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
end;

destructor TStockTraderTRSIMA_M15.Destroy;
begin
  inherited;
end;

procedure TStockTraderTRSIMA_M15.Dispose;
begin
  inherited;
  FreeAndNil(FPropIncreateOrdersOnLoss);
end;


procedure TStockTraderTRSIMA_M15.OnBeginWorkSession;
begin
  inherited;
  FLastOrderM15Index:=0;
  FSkipM15Index:=0;
  FLastUpdateTime:=0;
  FOrderBuyM15Signals:=0;
  FOrderSellM15Signals:=0;
end;

procedure TStockTraderTRSIMA_M15.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderTRSIMA_M15.SetProject(const aValue: IStockProject);
var
  aCreated: boolean;
begin
  if GetProject=aValue then
    exit;

  inherited;

  if aValue <> nil then
  begin
    //M15
    FRSI_M1_Black:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorTRSIMA,'RSI_BLACK'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M1_Black.SetPeriod(12);
      FRSI_M1_Black.GetTrendCorrectionMA.SetPeriod(55);
      FRSI_M1_Black.SetTrendCorrectionEnabled(true);
      FRSI_M1_Black.GetMA.SetPeriod(21);
      FRSI_M1_Black.SetColor(clBlack);
    end;

    FRSI_M1_Blue:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorTRSIMA,'RSI_BLUE'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M1_Blue.SetPeriod(12);
      FRSI_M1_Blue.GetTrendCorrectionMA.SetPeriod(84);
      FRSI_M1_Blue.SetTrendCorrectionEnabled(true);
      FRSI_M1_Blue.GetMA.SetPeriod(21);
      FRSI_M1_Blue.SetColor(clBlue);
    end;

    FRSI_M1_Pink:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorTRSIMA,'RSI_PINK'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorTRSIMA;
    if aCreated then
    begin
      FRSI_M1_Pink.SetPeriod(12);
      FRSI_M1_Pink.GetTrendCorrectionMA.SetPeriod(1);
      FRSI_M1_Pink.SetTrendCorrectionEnabled(false);
      FRSI_M1_Pink.GetMA.SetPeriod(21);
      FRSI_M1_Pink.SetColor(clWebPlum);
    end;

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

    //FMA84_M1:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorMA,'MA84'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorMA;

    //Ничего не нашли, создадим нового эксперта
    if aCreated then
    begin
      //FMA84_M1.SetPeriod(84);
    end;
  end;
end;

function TStockTraderTRSIMA_M15.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderTRSIMA_M15.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

function TStockTraderTRSIMA_M15.GetRecommendedLots: TStockOrderLots;
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

  result:=RoundTo(result,-2);
end;

function TStockTraderTRSIMA_M15.GetSignals(const aRSI_Black: ISCIndicatorTRSIMA; aRSI_Blue, aRSI_Pink: ISCIndicatorTRSIMA; aIndex: integer; aShowAlerts: boolean; out aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean): integer;
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

procedure TStockTraderTRSIMA_M15.UpdateStep2(const aTime: TDateTime);
begin
  Calculate(aTime);
end;

procedure TStockTraderTRSIMA_M15.Calculate(const aTime: TDateTime);
var
  aIdxM15 : integer;
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


  aIdxM15:=TStockDataUtils.FindBar(FRSI_M15_Black.GetInputData,aTime,sti15);
  if aIdxM15=FSkipM15Index then
    exit;
  if aIdxM15=FLastOrderM15Index then
    exit;

  if (aIdxM15<>-1) and (aIdxM15>=100) then
  begin
    aOpen:=0;
    aClose:=0;

    aSignals:=GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,aIdxM15,true,aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink);

    //open buy, close sell -------------------------------------------
    if aSignals>0 then
    begin
      if (aIdxM15-FLastOrderM15Index)<=2 then
      begin
        FSkipM15Index:=aIdxM15;
        AddMessageAndSetMark(mkStop,'Reject opening BUY: too close to previous order',clRed);
        aSignals:=0;
      end;

      //нужно проверить, когда был последний переворот, если ближе 12 часов - пропускаем
      if (aSignals=2) then
      begin
        for i:=aIdxM15-1 downto aIdxM15-12 do
        begin
          aSignalsPrev := GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          if (aSignalsPrev<=-2) then
          begin
            AddMessageAndSetMark(mkStop,'Reject opening BUY: Too close to previous opposite signal',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;

      //Посмотрим, а не остаточное ли это явление, Pink может быть уже давно вверху, и тогда покупать опасно
      if (aSignals=2) and (aRSIChangedBlue) and not (aRSIChangedPink) and  (FRSI_M15_Pink.GetValue(aIdxM15)>50) then
      begin
        for i:=aIdxM15-1 downto 0 do
        begin
          //Если где-то Pink был снизу - нормально
          if FRSI_M15_Pink.GetValue(i)<50 then
            break;

          aSignalsPrev := GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
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
      if (aIdxM15-FLastOrderM15Index)<=2 then
      begin
        FSkipM15Index:=aIdxM15;
        AddMessageAndSetMark(mkStop,'Reject opening SELL: too close to previous order',clRed);
        aSignals:=0;
      end;

      if (aSignals=-2) then
      begin
        for i:=aIdxM15-1 downto aIdxM15-12 do
        begin
          aSignalsPrev := GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
          if (aSignalsPrev>=2) then
          begin
            AddMessageAndSetMark(mkStop,'Reject opening SELL: Too close to previous opposite signal',clRed);
            aSignals:=0;
            break;
          end;
        end
      end;

      //Посмотрим, а не остаточное ли это явление, Pink может быть уже давно внизу, и тогда продавать опасно
      if (aSignals=-2) and (aRSIChangedBlue) and not (aRSIChangedPink) and  (FRSI_M15_Pink.GetValue(aIdxM15)<50) then
      begin
        for i:=aIdxM15-1 downto 0 do
        begin
          //Если где-то Pink был сверху - нормально
          if FRSI_M15_Pink.GetValue(i)>50 then
            break;

          aSignalsPrev := GetSignals(FRSI_M15_Black,FRSI_M15_Blue,FRSI_M15_Pink,i,false,aRSIChangedBlack2,aRSIChangedBlue2,aRSIChangedPink2);
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
        inc(FOrderBuyM15Signals);
        FOrderSellM15Signals:=0;
        OpenOrder(okBuy);
        FLastOrderM15Index:=aIdxM15;
      end
      //SELL
      else if (aOpen=-1) {and (LastOrderType<>lotSell)} then
      begin
        inc(FOrderSellM15Signals);
        FOrderBuyM15Signals:=0;
        OpenOrder(okSell);
        FLastOrderM15Index:=aIdxM15;
      end;
    end;
  end;
end;


procedure TStockTraderTRSIMA_M15.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','TRSIMA M15',TStockTraderTRSIMA_M15,IStockTraderTRSIMA_M15);

end.



