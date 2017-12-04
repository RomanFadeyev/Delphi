unit FC.StockChart.UnitTask.ValueSupport.ValueDataGrid;

{$I Compiler.inc}
interface

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.ValueSupport.DataGridDialog;

type
  TStockUnitTaskDataGrid = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskDataGrid }

function TStockUnitTaskDataGrid.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorValueSupport);
  if result then
    aOperationName:='Data Grid';
end;

procedure TStockUnitTaskDataGrid.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmValueStatisticsDialog.Run(aIndicator as ISCIndicatorValueSupport, aStockChart);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskDataGrid.Create);


end.
