unit FC.StockChart.UnitTask.Bars.Navigator;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.Bars.NavigatorDialog;

type
  //Специальный Unit Task для автоматического подбора размеров грани и длины периода.
  //Подюор оусщестьвляется прямым перебором.
  TStockUnitTaskBarsNavigator = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskBarsNavigator }

function TStockUnitTaskBarsNavigator.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorBars);
  if result then
    aOperationName:='Bars: Navigate';
end;

procedure TStockUnitTaskBarsNavigator.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmBarsNavigatorDialog.Run(aIndicator as ISCIndicatorBars,aStockChart);
end;

initialization
  //Регистрируем Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskBarsNavigator.Create);


end.
