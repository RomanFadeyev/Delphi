unit FC.StockChart.UnitTask.Calendar.Editor;

interface
{$I Compiler.inc}

uses
  Types, SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  Graphics,
  StockChart.Definitions.Drawing, StockChart.Drawing,
  FC.StockChart.UnitTask.Calendar.EditorDialog;

type
  //Специальный Unit Task для автоматического подбора размеров грани и длины периода.
  //Подюор оусщестьвляется прямым перебором.
  TStockUnitTaskCalendarEditor = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskCalendarEditor }

function TStockUnitTaskCalendarEditor.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorCalendar);
  if result then
    aOperationName:='Calendar: Edit Item';
end;

procedure TStockUnitTaskCalendarEditor.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  aCalendar: ISCIndicatorCalendar;
  aHitTest: TSCHitTestData;
begin
  if aCurrentPosition.X>0 then
  begin
    aCalendar:=aIndicator as ISCIndicatorCalendar;
    aHitTest.Bounds:=TSCGeometry.MakeBoundsRect(
        aCurrentPosition.X,
        aCurrentPosition.Y-aIndicator.GetInputData.PointToPrice(1),
        aCurrentPosition.X,
        aCurrentPosition.Y+aIndicator.GetInputData.PointToPrice(1));

    aHitTest.Tag:=-1;
    if (aIndicator as ISCIndicatorDrawSupport).HitTest(aHitTest) and (aHitTest.Tag<>-1) then
    begin
      TfmCalendarEditorDialog.RunEdit(aCalendar.GetData.GetItem(aHitTest.Tag));
    end;
  end;
end;

initialization
  //Регистрируем Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskCalendarEditor.Create);


end.
