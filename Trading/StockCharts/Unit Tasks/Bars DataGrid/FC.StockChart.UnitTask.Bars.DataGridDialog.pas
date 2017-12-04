unit FC.StockChart.UnitTask.Bars.DataGridDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FC.Dialogs.DockedDialogCloseAndAppWindow_B, JvDockControlForm, ImgList, JvComponentBase, JvCaptionButton, StdCtrls,
  ExtendControls, ExtCtrls,StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, DB,FC.StockData.InputDataCollectionToDataSetMediator,
  FC.StockChart.CustomDialog_B;

type
  TfmBarsDataGridDialog = class(TfmStockChartCustomDialog_B)
    dsData: TDataSource;
    grData: TEditDBGrid;
  private
    FDataSet   : TStockInputDataCollectionToDataSetMediator;
    FIndicator : ISCIndicatorBars;
  protected
  public
    constructor Create(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart); reintroduce;

    class procedure Run(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
  end;


implementation

uses ufmDialog_B,DateUtils, Application.Definitions;

{$R *.dfm}

{ TfmBarsDataGridDialog }

constructor TfmBarsDataGridDialog.Create(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
begin
  inherited Create(aStockChart);
  FIndicator:=aExpert;
  FDataSet:=TStockInputDataCollectionToDataSetMediator.Create(self);
  FDataSet.InputDataCollection:=StockChart.GetInputData;
  dsData.DataSet:=FDataSet;

  Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;
end;

class procedure TfmBarsDataGridDialog.Run(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
begin
  with TfmBarsDataGridDialog.Create(aExpert,aStockChart) do
    Show;
end;

end.
