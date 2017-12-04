unit FC.StockChart.UnitTask.TradeLine.Snapshot;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, Dialogs, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation

type
  TStockUnitTaskExpertLineSnapshot = class(TStockUnitTaskBase)
  public
    function CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskExpertLineSnapshot }

function TStockUnitTaskExpertLineSnapshot.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorTradeLine);
  if result then
    aOperationName:='Make Snapshot';
end;

procedure TStockUnitTaskExpertLineSnapshot.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  s: string;
  aExpertLine : ISCIndicatorTradeLine;
  aShapshot: ISCIndicatorTradeLineSnapshot;
  aIndicatorInfo : ISCIndicatorInfo;
begin
  aExpertLine:=aIndicator as ISCIndicatorTradeLine;
  aIndicatorInfo:=IndicatorFactory.GetIndicatorInfo(ISCIndicatorTradeLineSnapshot);

  s:=aExpertLine.Caption+' - Snapshot['+DateTimeToStr(Now)+']';
  if InputQuery('Snapshot for expert line '+aIndicator.Caption,
                'Input snapshot name',s) then
  begin
    aShapshot:= aStockChart.CreateIndicator(aIndicatorInfo,false) as ISCIndicatorTradeLineSnapshot;
    aShapshot.Snap(aExpertLine);
    (aShapshot as ISCWritableName).SetName(s);
    aExpertLine.Clear;
  end;
end;

initialization
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskExpertLineSnapshot.Create);

end.

