{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Модуль реализации Unit Task для Bollinger Bands для подбора размера
            канала и периода 

 History:
-----------------------------------------------------------------------------}

unit FC.StockChart.UnitTask.BB.AdjustWidth;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
uses
  FC.StockChart.UnitTask.BB.AdjustWidthDialog;

type
  //Специальный Unit Task для автоматического подбора размеров грани и длины периода.
  //Подюор оусщестьвляется прямым перебором.
  TStockUnitTaskBBAdjustWidth = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskBBAdjustWidth }

function TStockUnitTaskBBAdjustWidth.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorBB);
  if result then
    aOperationName:='BB: Adjust Channel Width';
end;

procedure TStockUnitTaskBBAdjustWidth.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
begin
  TfmAdjustWidthDialog.Run(aIndicator as ISCIndicatorBB);
end;

initialization
  //Регистрируем Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskBBAdjustWidth.Create);


end.
