unit FC.StockChart.UnitTask.ValueSupport.Statistics;

{$I Compiler.inc}
interface

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.ValueSupport.StatisticsDialog;

type
  TStockUnitTaskValueSupportStatistics = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskValueSupportStatistics }

function TStockUnitTaskValueSupportStatistics.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorValueSupport);
  if result then
    aOperationName:='Get Statistics';
end;

procedure TStockUnitTaskValueSupportStatistics.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmValueSupportStatisticsDialog.Run(aIndicator as ISCIndicatorValueSupport,aStockChart);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskValueSupportStatistics.Create);


end.
