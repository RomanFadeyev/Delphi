unit FC.StockChart.UnitTask.Indicator.Duplicate;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, Dialogs, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation

type
  TStockUnitTaskIndicatorDuplicate = class(TStockUnitTaskBase)
  public
    function CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskIndicatorDuplicate }

function TStockUnitTaskIndicatorDuplicate.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=true;
  if result then
    aOperationName:='Duplicate';                 
end;

procedure TStockUnitTaskIndicatorDuplicate.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  aIndicatorInfo : ISCIndicatorInfo;
  aCopy: ISCIndicator;
  i: integer;
  aSrcProp,aDstProp: ISCIndicatorProperty;
begin
  aIndicatorInfo:=IndicatorFactory.GetIndicatorInfo(aIndicator.GetIID);
  aCopy:=aStockChart.CreateIndicator(aIndicatorInfo,false);
  for i := 0 to aIndicator.GetProperties.Count - 1 do
  begin
    aSrcProp:=aIndicator.GetProperties.Items[i];
    aDstProp:=aCopy.GetProperties.Items[i];
    Assert(aSrcProp.GetName=aDstProp.GetName);
    aDstProp.Value:=aSrcProp.Value;
  end;
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskIndicatorDuplicate.Create);

end.

