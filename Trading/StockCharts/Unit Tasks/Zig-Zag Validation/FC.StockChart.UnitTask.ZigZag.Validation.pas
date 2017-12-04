unit FC.StockChart.UnitTask.ZigZag.Validation;


interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
  uses Clipbrd,StockChart.Definitions.Drawing;

type
  //Специальный Unit Task для автоматического подбора размеров грани и длины периода.
  //Подюор оусщестьвляется прямым перебором.
  TStockUnitTaskZigZagValidation = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskZigZagValidation }

function TStockUnitTaskZigZagValidation.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorZigZag);
  if result then
    aOperationName:='Zig-Zag: Validate Calculations';
end;

procedure TStockUnitTaskZigZagValidation.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  (aIndicator as ISCIndicatorZigZag).ValidateCalculation;
end;

initialization
  //Регистрируем Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskZigZagValidation.Create);


end.
