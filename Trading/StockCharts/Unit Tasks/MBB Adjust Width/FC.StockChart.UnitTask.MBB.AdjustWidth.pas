unit FC.StockChart.UnitTask.MBB.AdjustWidth;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.MBB.AdjustWidthDialog;

type
  TStockUnitTaskMBBAdjustWidth = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskMBBAdjustWidth }

function TStockUnitTaskMBBAdjustWidth.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorMBB);
  if result then
    aOperationName:='MBB: Adjust Channel Width';
end;

procedure TStockUnitTaskMBBAdjustWidth.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmMBBAdjustWidthDialog.Run(aIndicator as ISCIndicatorMBB);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskMBBAdjustWidth.Create);


end.
