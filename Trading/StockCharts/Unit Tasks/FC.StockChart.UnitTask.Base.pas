unit FC.StockChart.UnitTask.Base;

interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, FC.Definitions, StockChart.Definitions;

type
  TStockUnitTaskBase = class (TInterfacedObjectEx,IStockUnitTask)
  public
    //from IStockUnitTask
    function CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; virtual; abstract;

    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); virtual; abstract;    
    //end of IStockUnitTask
  end;

implementation

initialization


end.

