unit FC.StockChart.UnitTask.Calendar.BarList;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.Calendar.BarListDialog;

type
  //Специальный Unit Task для автоматического подбора размеров грани и длины периода.
  //Подюор оусщестьвляется прямым перебором.
  TStockUnitTaskCalendarBarList = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskCalendarBarList }

function TStockUnitTaskCalendarBarList.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorCalendar);
  if result then
    aOperationName:='Calendar: List Items For This Bar';
end;

procedure TStockUnitTaskCalendarBarList.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  if (aCurrentPosition.X>0) and (aCurrentPosition.X<aStockChart.GetInputData.Count) then
    TfmCalendarBarListDialog.Run(
      aIndicator as ISCIndicatorCalendar,
      aStockChart,
      aStockChart.GetInputData.DirectGetItem_DataDateTime(Trunc(aCurrentPosition.X)));
end;

initialization
  //Регистрируем Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskCalendarBarList.Create);


end.
