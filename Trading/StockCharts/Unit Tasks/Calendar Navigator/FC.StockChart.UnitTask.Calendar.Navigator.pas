unit FC.StockChart.UnitTask.Calendar.Navigator;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.Calendar.NavigatorDialog;

type
  //����������� Unit Task ��� ��������������� ������� �������� ����� � ����� �������.
  //������ ��������������� ������ ���������.
  TStockUnitTaskCalendarNavigator = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskCalendarNavigator }

function TStockUnitTaskCalendarNavigator.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorCalendar);
  if result then
    aOperationName:='Calendar: Navigate';
end;

procedure TStockUnitTaskCalendarNavigator.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmCalendarNavigatorDialog.Run(aIndicator as ISCIndicatorCalendar,aStockChart);
end;

initialization
  //������������ Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskCalendarNavigator.Create);


end.
