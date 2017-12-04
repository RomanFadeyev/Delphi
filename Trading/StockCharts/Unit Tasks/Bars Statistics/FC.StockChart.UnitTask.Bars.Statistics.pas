unit FC.StockChart.UnitTask.Bars.Statistics;

{$I Compiler.inc}
interface

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.Bars.StatisticsDialog;

type
  TStockUnitTaskBarsStatistics = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskBarsStatistics }

function TStockUnitTaskBarsStatistics.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorBars);
  if result then
    aOperationName:='Bars: Get Statistics';
end;

procedure TStockUnitTaskBarsStatistics.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmBarsStatisticsDialog.Run(aIndicator as ISCIndicatorBars,aStockChart);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskBarsStatistics.Create);


end.
