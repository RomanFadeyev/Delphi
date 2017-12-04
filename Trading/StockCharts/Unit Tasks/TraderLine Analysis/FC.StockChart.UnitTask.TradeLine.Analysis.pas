unit FC.StockChart.UnitTask.TradeLine.Analysis;

{$I Compiler.inc}
interface

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.TradeLine.AnalysisDialog;

type
  TStockUnitTradeLineAnalysis = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTradeLineAnalysis }

function TStockUnitTradeLineAnalysis.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorTradeLine);
  if result then
    aOperationName:='Open/Close Order Analysis';
end;

procedure TStockUnitTradeLineAnalysis.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmTradeLineAnalysisDialog.Run(aIndicator as ISCIndicatorTradeLine, aStockChart);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTradeLineAnalysis.Create);


end.
