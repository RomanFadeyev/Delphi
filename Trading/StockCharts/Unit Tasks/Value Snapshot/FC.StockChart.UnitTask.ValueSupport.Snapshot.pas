unit FC.StockChart.UnitTask.ValueSupport.Snapshot;

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
  result:=Supports(aIndicator,ISCIndicatorValueSupport);
  if result then
    aOperationName:='Make Snapshot';
end;

procedure TStockUnitTaskValueSupportSnapshot.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  s: string;
  aValueSupport : ISCIndicatorValueSupport;
  aShapshot: ISCIndicatorCustomValues;
  aIndicatorInfo : ISCIndicatorInfo;
begin
  aValueSupport:=aIndicator as ISCIndicatorValueSupport;
  aIndicatorInfo:=IndicatorFactory.GetIndicatorInfo(ISCIndicatorCustomValues);

  s:=aIndicator.Caption+' - Snapshot['+DateTimeToStr(Now)+']';
  if InputQuery('Snapshot for expert line '+aIndicator.Caption,
                'Input snapshot name',s) then
  begin
    aShapshot:= aStockChart.CreateIndicator(aIndicatorInfo,false) as ISCIndicatorCustomValues;
    aShapshot.MakeSnapshot(aValueSupport);
    (aShapshot as ISCWritableName).SetName(s);
  end;
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskValueSupportSnapshot.Create);

end.

