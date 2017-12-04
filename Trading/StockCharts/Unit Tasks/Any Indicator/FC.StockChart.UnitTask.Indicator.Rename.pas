unit FC.StockChart.UnitTask.Indicator.Rename;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, Dialogs, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation

type
  TStockUnitTaskValueSupportSnapshot = class(TStockUnitTaskBase)
  public
    function CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskValueSupportSnapshot }

function TStockUnitTaskValueSupportSnapshot.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCWritableName);
  if result then
    aOperationName:='Rename';
end;

procedure TStockUnitTaskValueSupportSnapshot.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  s: string;
  aValueSupport : ISCWritableName;
begin
  aValueSupport:=aIndicator as ISCWritableName;

  s:=aIndicator.Caption;
  if InputQuery('Rename','Enter new name of the unit', s) then
    aValueSupport.SetName(s);
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskValueSupportSnapshot.Create);

end.

