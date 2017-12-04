unit FC.StockChart.UnitTask.MBB.CalculateProfit;

{$I Compiler.inc}
interface

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.MBB.CalculateProfitDialog;

type
  TStockUnitTaskMBBCalculateProfit = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskMBBCalculateProfit }

function TStockUnitTaskMBBCalculateProfit.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorMBB);
  if result then
    aOperationName:='MBB: Calculate Profit';
end;

procedure TStockUnitTaskMBBCalculateProfit.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmMBBCalculateProfitDialog.Run(aIndicator as ISCIndicatorMBB);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskMBBCalculateProfit.Create);


end.
