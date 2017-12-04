unit FC.Trade.Statistics;
{$I Compiler.inc}

interface
uses  Classes, SysUtils, FC.Definitions;

type
  TStockTradingStatistics = class
  private
    FBalance: TStockRealNumber;

    FLargestProfit: TStockRealNumber;
    FLargestLoss  : TStockRealNumber;
    FLargestProfitPts: integer;
    FLargestLossPts  : integer;

    FOrderCount   : integer;
    FProfitOrderCount: integer;
    FLossOrderCount  : integer;

    FTotalProfit     : TStockRealNumber;
    FTotalLoss       : TStockRealNumber;
    FTotalProfitPts  : integer;
    FTotalLossPts    : integer;

    FTotalPossibleProfit : TStockRealNumber;
    FTotalPossibleProfitPts : integer;
  public
    StartDate : TDateTime;
    StopDate  : TDateTime;

    property Balance: TStockRealNumber read FBalance write FBalance;
    property LargestProfit: TStockRealNumber read FLargestProfit;
    property LargestLoss  : TStockRealNumber read FLargestLoss;
    property LargestProfitPts: integer read FLargestProfitPts;
    property LargestLossPts  : integer read FLargestLossPts;

    property OrderCount      : integer read FOrderCount;
    property ProfitOrderCount: integer read FProfitOrderCount;
    property LossOrderCount  : integer read FLossOrderCount;
    property TotalProfit     : TStockRealNumber read FTotalProfit;
    property TotalLoss       : TStockRealNumber read FTotalLoss;
    property TotalProfitPts  : integer read FTotalProfitPts;
    property TotalLossPts    : integer read FTotalLossPts;

    property TotalPossibleProfit  : TStockRealNumber read FTotalPossibleProfit;
    property TotalPossibleProfitPts  : integer read FTotalPossibleProfitPts;

    procedure AddValue(aValue,aBestPossible,aWorstPossible: TStockRealNumber;
                       aValuePts,aBestPossiblePts,aWorstPossiblePts: integer);

    procedure ToStrings(aList: TStrings);
  end;

implementation
  uses Math,DateUtils;

{ TStockTradingStatistics }

procedure TStockTradingStatistics.AddValue(aValue,aBestPossible,aWorstPossible: TStockRealNumber;
                              aValuePts,aBestPossiblePts,aWorstPossiblePts: integer);
begin
  FBalance:=FBalance+aValue;
  FLargestProfit:=max(aValue,FLargestProfit);
  FLargestProfitPts:=max(aValuePts,FLargestProfitPts);

  FLargestLoss:=min(aValue,FLargestLoss);
  FLargestLossPts:=min(aValuePts,FLargestLossPts);

  inc(FOrderCount);

  if aValue>0 then
  begin
    inc(FProfitOrderCount);
    FTotalProfit:=FTotalProfit+aValue;
    FTotalProfitPts:=FTotalProfitPts+aValuePts;

    FTotalPossibleProfit:=FTotalPossibleProfit+aBestPossible;
    FTotalPossibleProfitPts:=FTotalPossibleProfitPts+aBestPossiblePts;
  end
  else if aValue<0 then
  begin
    inc(FLossOrderCount);
    FTotalLoss:=FTotalLoss+aValue;
    FTotalLossPts:=FTotalLossPts+aValuePts;
  end;
end;

procedure TStockTradingStatistics.ToStrings(aList: TStrings);
var
  aDays: TStockRealNumber;
  s: string;
begin
  aDays:=DaySpan(StopDate,StartDate);

  aList.Clear;
  aList.Values['Start Scanning at']:=DateTimeToStr(StartDate);
  aList.Values['Stop Scanning at']:=DateTimeToStr(StopDate);
  aList.Values['Days of trading']:=FloatToStr(RoundTo(aDays,-2));
  aList.Values['Orders per day']:= FloatToStr(RoundTo(OrderCount/max(aDays,1),-2));
  aList.Add('');
  aList.Values['Balance']:=FormatCurr('0,.00',Balance);
  aList.Values['Total Profit($)']:=FormatCurr('0,.00',TotalProfit);
  aList.Values['Total Profit(pt)']:=FormatCurr('0,.00',TotalProfitPts);
  aList.Values['Possible Profit($)']:=FormatCurr('0,.00',TotalPossibleProfit);
  aList.Values['Possible Profit(pt)']:=FormatCurr('0,.00',TotalPossibleProfitPts);

  //-----
  if TotalPossibleProfit=0 then
    s:='-'
  else
    s:=FormatCurr('0,.00',TotalProfit/TotalPossibleProfit*100);
  aList.Values['% from Possible Profit($)']:=s;

  //-----
  if TotalPossibleProfitPts=0 then
    s:='-'
  else
    s:=FormatCurr('0,.00',TotalProfitPts/TotalPossibleProfitPts*100);
  aList.Values['% from Possible Profit(pt)']:=s;

  //-----
  aList.Values['Total Loss($)']:=FormatCurr('0,.00',TotalLoss);
  aList.Values['Total Loss(pt)']:=FormatCurr('0,.00',TotalLossPts);

  //-----
  if FTotalLossPts=0 then
    s:='-'
  else
    s:=FormatCurr('0,.00',abs(FTotalProfitPts/FTotalLossPts))+':1';
  aList.Values['Profit(pt):Loss(pt)']:=s;

  //-----
  aList.Values['Count of wins']:=IntToStr(ProfitOrderCount);
  aList.Values['Count of losses']:=IntToStr(LossOrderCount);

  //-----
  if ProfitOrderCount>0 then
  begin
    aList.Values['Average Profit($)']:=FormatCurr('0,.00', TotalProfit/ProfitOrderCount);
    aList.Values['Average Profit(pt)']:=FormatCurr('0,.00', TotalProfitPts/ProfitOrderCount)
  end
  else begin
    aList.Values['Average Profit($)']:='-';
    aList.Values['Average Profit(pt)']:='-';
  end;

  //-----
  if LossOrderCount>0 then
  begin
    aList.Values['Average Loss($)']:=FormatCurr('0,.00', TotalLoss/LossOrderCount);
    aList.Values['Average Loss(pt)']:=FormatCurr('0,.00', TotalLossPts/LossOrderCount);
  end
  else begin
    aList.Values['Average Loss($)']:='-';
    aList.Values['Average Loss(pt)']:='-';
  end;

  //-----
  aList.Values['Largest Profit($)']:=FormatCurr('0,.00', LargestProfit);
  aList.Values['Largest Profit(pt)']:=FormatCurr('0,.00', LargestProfitPts);
  aList.Values['Largest Loss($)']:=FormatCurr('0,.00',LargestLoss);
  aList.Values['Largest Loss(pt)']:=FormatCurr('0,.00',LargestLossPts);

  aList.Values['Profit per day($)']:=FormatCurr('0,.00',Balance/max(aDays,1));
end;

end.
