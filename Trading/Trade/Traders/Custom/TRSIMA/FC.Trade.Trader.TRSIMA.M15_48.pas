unit FC.Trade.Trader.TRSIMA.M15_48;
{$I Compiler.inc}

interface

uses
  Classes, Math,Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage,
  StockChart.Definitions.Drawing,Graphics;

type
  IStockTraderTRSIMA_M15_48 = interface
  ['{7C232A67-47BA-4745-B13E-BCF94A2D1AB2}']
  end;

  TStockTraderTRSIMA_M15_48 = class (TStockTraderBase,IStockTraderTRSIMA_M15_48)
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
    FLastOrderM15Index: integer;
    FSkipM15Index : integer;
    FLastUpdateTime  : TDateTime;
  protected
    function  Spread: TSCRealNumber;
    function  ToPrice(aPoints: integer): TSCRealNumber;

    procedure AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string); overload;
    procedure AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string; aMessageColor: TColor=clDefault);overload;

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

{ TStockTraderTRSIMA_M15_48 }

procedure TStockTraderTRSIMA_M15_48.AddMessageAndSetMark(const aMarkType: TSCChartMarkKind; const aMessage: string;
  aMessageColor: TColor);
begin
  GetBroker.AddMessage(aMessage,aMessageColor);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

constructor TStockTraderTRSIMA_M15_48.Create;
begin
  inherited Create;
//  UnRegisterProperties([PropTrailingStop,PropTrailingStopDescend,PropMinimizationRiskType]);
end;

destructor TStockTraderTRSIMA_M15_48.Destroy;
begin
  inherited;
end;

procedure TStockTraderTRSIMA_M15_48.Dispose;
begin
  inherited;
end;


procedure TStockTraderTRSIMA_M15_48.OnBeginWorkSession;
begin
  inherited;
  FLastOrderM15Index:=0;
  FSkipM15Index:=0;
  FLastUpdateTime:=0;
end;

procedure TStockTraderTRSIMA_M15_48.SetMark(const aOrder: IStockOrder;const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

procedure TStockTraderTRSIMA_M15_48.SetProject(const aValue: IStockProject);
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

    //FMA84_M1:=CreateOrFindIndicator(aValue.GetStockChart(sti1),ISCIndicatorMA,'MA84'+'_'+StockTimeIntervalNames[sti1],true, aCreated) as ISCIndicatorMA;

    //Ничего не нашли, создадим нового эксперта
    if aCreated then
    begin
      //FMA84_M1.SetPeriod(84);
    end;
  end;
end;

function TStockTraderTRSIMA_M15_48.ToPrice(aPoints: integer): TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,1);
end;

function TStockTraderTRSIMA_M15_48.Spread: TSCRealNumber;
begin
  result:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).Spread);
end;

procedure TStockTraderTRSIMA_M15_48.UpdateStep2(const aTime: TDateTime);
var
  aIdxM15 : integer;
  //aIdxH1 : integer;
  //aIdxH4 : integer;
  aOpen,aClose : integer;
  aRSIValueBlack,aRSIValueBlue,aRSIValuePink: TSCRealNumber;
  aRSIValueBlack_1,aRSIValueBlue_1,aRSIValuePink_1: TSCRealNumber;

  aRSIChangedBlack,aRSIChangedBlue,aRSIChangedPink: boolean;
  x: integer;
begin
  RemoveClosedOrders;

  //Не чаще одного раза в минуту
  //if (aTime-FLastUpdateTime)*MinsPerDay<1 then
  //  exit;

  //if (not DateUtils.MinuteOf(aTime) in [1,16,31,46]) then
  //  exit;

  FLastUpdateTime:=aTime;


  aIdxM15:=TStockDataUtils.FindBar(FRSI_M15_Black.GetInputData,aTime,sti15);
  if aIdxM15=FSkipM15Index then
    exit;
  if aIdxM15=FLastOrderM15Index then
    exit;

  //aIdxH1:=TStockDataUtils.FindBar(FRSI_H1_Black.GetInputData,aTime,sti60);

  //if aIdxH1<=2 then
  //  exit;

  //aIdxH4:=TStockDataUtils.FindBar(FRSI_H4_Black.GetInputData,aTime,sti240);
  //if aIdxH4<=2 then
  //  exit;

  if (aIdxM15<>-1) and (aIdxM15>=100) then
  begin
    aOpen:=0;
    aClose:=0;

    aRSIValueBlack:=FRSI_M15_Black.GetValue(aIdxM15);
    aRSIValueBlue:=FRSI_M15_Blue.GetValue(aIdxM15);
    aRSIValuePink:=FRSI_M15_Pink.GetValue(aIdxM15);

    aRSIValueBlack_1:=FRSI_M15_Black.GetValue(aIdxM15-1);
    aRSIValueBlue_1:=FRSI_M15_Blue.GetValue(aIdxM15-1);
    aRSIValuePink_1:=FRSI_M15_Pink.GetValue(aIdxM15-1);

    //Анализ BUY
    aRSIChangedBlack:=(aRSIValueBlack>=50) and (aRSIValueBlack_1<50);
    aRSIChangedBlue:= (aRSIValueBlue>=50) and (aRSIValueBlue_1<50);
    aRSIChangedPink:= (aRSIValuePink>=50) and (aRSIValuePink_1<50);

    if aRSIChangedBlack or aRSIChangedBlue or aRSIChangedPink then
    begin
      x:= integer(aRSIValueBlack>=50)+integer(aRSIValueBlue>=50)+integer(aRSIValuePink>=50);
      if (x=2) and (aRSIValuePink<20) then
        x:=0;

      //x:= integer((aRSIValueBlack>=50) or (aRSIValueBlue>=50))+integer(aRSIValuePink>=50);
      if x>=2  then
      begin
        if (aIdxM15-FLastOrderM15Index)<=2 then
        begin
          FSkipM15Index:=aIdxM15;
          //AddMessageAndSetMark(mkStop,'Reject opening BUY: too close to previous order',clRed);
        end

        //Если поменялся Blue или Black, и при этом Pink уже отчскочил от верхней границы, то игнорируем сигнал
        else if (aRSIChangedBlack or aRSIChangedBlue) and (FRSI_M15_Pink.GetLastSide(aIdxM15,1)=1) then
        begin
          //AddMessageAndSetMark(mkStop,'Reject opening BUY: RSI M15 on top',clRed);
        end

        (*else if FRSI_H1_Pink.GetTrendDirection(aIdxH1)=-1 then
        begin
          AddMessageAndSetMark(mkStop,'Reject opening BUY: RSI Pink H1 trend down',clRed);
        end*)


        else begin
          aOpen:=1;
          aClose:=-1;
        end;
      end;
    end;

    //Анализ SELL
    aRSIChangedBlack:=(aRSIValueBlack<=50) and (aRSIValueBlack_1>50);
    aRSIChangedBlue:= (aRSIValueBlue<=50) and (aRSIValueBlue_1>50);
    aRSIChangedPink:= (aRSIValuePink<=50) and (aRSIValuePink_1>50);

    if aRSIChangedBlack or aRSIChangedBlue or aRSIChangedPink then
    begin
      x:=integer(aRSIValueBlack<=50) + integer(aRSIValueBlue<=50) + integer(aRSIValuePink<=50);
      if (x=2) and (aRSIValuePink>80) then
        x:=0;

      //x:=integer((aRSIValueBlack<=50) or (aRSIValueBlue<=50)) + integer(aRSIValuePink<=50);
      if x>=2 then
      begin
        if (aIdxM15-FLastOrderM15Index)<=2 then
        begin
          FSkipM15Index:=aIdxM15;
          //AddMessageAndSetMark(mkStop,'Reject opening SELL: too close to previous order',clRed);
        end
        //Если поменялся Blue или Black, и при этом Pink уже отчскочил нижней от границы, то игнорируем сигнал
        else if (aRSIChangedBlack or aRSIChangedBlue) and (FRSI_M15_Pink.GetLastSide(aIdxM15,1)=-1) then
        begin
          //AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI M15 on bottom',clRed);
        end

        (*else if FRSI_H1_Pink.GetTrendDirection(aIdxH1)=1 then
        begin
          AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI Pink H1 trend up',clRed);
        end*)

        (*else if FRSI_H4_Pink.GetTrendDirection(aIdxH4)=1 then
        begin
          AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI Pink H4 trend up',clRed);
        end*)

        (*else if FRSI_H1_Pink.GetDelta(aIdxH1)=1 then
        begin
          AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI Pink H1 delta up',clRed);
        end*)

        (*else if FRSI_H4_Pink.GetDelta(aIdxH4)=1 then
        begin
          AddMessageAndSetMark(mkStop,'Reject opening SELL: RSI Pink H4 delta up',clRed);
        end*)

        else begin
          aOpen:=-1;
          aClose:=1;
        end;
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
        OpenOrder(okBuy);
        FLastOrderM15Index:=aIdxM15;
      end
      //SELL
      else if (aOpen=-1) {and (LastOrderType<>lotSell)} then
      begin
        FLastOrderM15Index:=aIdxM15;
        OpenOrder(okSell);
      end;
    end;
  end;
end;

procedure TStockTraderTRSIMA_M15_48.AddMessageAndSetMark(const aOrder: IStockOrder; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  GetBroker.AddMessage(aOrder,aMessage);
  AddMarkToCharts(GetBroker.GetCurrentTime,
                  GetBroker.GetCurrentPrice({aOrder}self.GetSymbol,bpkBid),
                  aMarkType,aMessage);
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Basic','TRSIMA M15 48',TStockTraderTRSIMA_M15_48,IStockTraderTRSIMA_M15_48);
end.




