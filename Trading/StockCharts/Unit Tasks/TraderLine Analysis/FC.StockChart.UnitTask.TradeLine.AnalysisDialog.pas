unit FC.StockChart.UnitTask.TradeLine.AnalysisDialog;
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls, FC.StockChart.CustomDialog_B, ImgList, JvCaptionButton,
  JvComponentBase, JvDockControlForm, JvExExtCtrls, JvNetscapeSplitter;

type
  TfmTradeLineAnalysisDialog = class(TfmStockChartCustomDialog_B)
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    taReportNo: TIntegerField;
    taReportDateTime: TDateTimeField;
    taReportPrice: TFloatField;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    buStart2: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    lvIndicators: TExtendListView;
    taReportProfit: TIntegerField;
    taReportIndicator: TFloatField;
    alActions: TActionList;
    acStart: TAction;
    taReportOrderKind: TStringField;
    Panel3: TPanel;
    edFilter: TExtendComboBox;
    grReport: TEditDBGrid;
    buOpenOrder: TRadioButton;
    buCloseOrder: TRadioButton;
    ckSyncAllCharts: TExtendCheckBox;
    procedure lvIndicatorsDblClick(Sender: TObject);
    procedure edFilterKeyPress(Sender: TObject; var Key: Char);
    procedure acStartUpdate(Sender: TObject);
    procedure lvIndicatorsCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure grReportBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure buStart2Click(Sender: TObject);
    procedure buOKClick(Sender: TObject);
    procedure grReportDblClick(Sender: TObject);
  private
    FIndicator: ISCIndicatorTradeLine;
    procedure CollectIndicators;
  public
    class procedure Run(const aIndicator: ISCIndicatorTradeLine; const aStockChart: IStockChart);

    constructor Create(const aStockChart: IStockChart); override;
    destructor Destroy; override;
  end;


implementation
  uses Math,StockChart.Definitions.Drawing, ufmForm_B;

{$R *.dfm}

type

  TIndicatorListItem = class (TListItem)
    Indicator: ISCIndicatorValueSupport;
  end;

{ TfmCalculateWidthDialog }

procedure TfmTradeLineAnalysisDialog.acStartUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=lvIndicators.Selected<>nil;
end;

procedure TfmTradeLineAnalysisDialog.buOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmTradeLineAnalysisDialog.buStart2Click(Sender: TObject);
var
  i,j: integer;
  aOrder: ISCIndicatorTradeLineItem;
  aIndicator: ISCIndicatorValueSupport;
  aTime: TDateTime;
begin
  aIndicator:=TIndicatorListItem(lvIndicators.Selected).Indicator;

  taReport.DisableControls;
  taReport.Open;
  taReport.EmptyTable;
  try
    for i := 0 to FIndicator.ItemCount - 1 do
    begin
      aOrder:=FIndicator.GetItem(i);
      if aOrder.GetKind<>mkNone then
        continue;


      taReport.Append;
      taReportNo.Value:=taReport.RecordCount+1;
      taReportOrderKind.Value:=OrderKindNames[aOrder.GetOrderKind];
      if buOpenOrder.Checked then
      begin
        taReportDateTime.Value:=aOrder.GetOpenTime;
        taReportPrice.Value:=aOrder.GetOpenPrice;
        aTime:=aOrder.GetOpenTime;
      end
      else begin
        taReportDateTime.Value:=aOrder.GetCloseTime;
        taReportPrice.Value:=aOrder.GetClosePrice;
        aTime:=aOrder.GetClosePrice;
      end;

      if aOrder.GetClosePrice<>0 then //незакрытый ордер
      begin
        if aOrder.GetOrderKind=okBuy then
          taReportProfit.Value:=FIndicator.GetInputData.PriceToPoint(aOrder.GetClosePrice-aOrder.GetOpenPrice)
        else
          taReportProfit.Value:=FIndicator.GetInputData.PriceToPoint(-aOrder.GetClosePrice+aOrder.GetOpenPrice);
      end;

      j:=(aIndicator as ISCIndicator).GetInputData.FindExactMatched(aTime);
      if j<>-1 then
        taReportIndicator.Value:=FIndicator.GetInputData.RoundPrice(aIndicator.GetValue(j));

      taReport.Post;
    end;
  finally
    taReport.First;
    taReport.EnableControls;
  end;
end;

procedure TfmTradeLineAnalysisDialog.CollectIndicators;
var
  aIndicators: ISCIndicatorCollectionReadOnly;
  i: integer;
begin
  lvIndicators.HandleNeeded;


  aIndicators:=StockChart.FindIndicators(ISCIndicatorValueSupport);
  for i := 0 to aIndicators.Count-1 do
  begin
    with lvIndicators.Items.Add as TIndicatorListItem do
    begin
      Caption:=aIndicators.Items[i].Caption;
      Indicator:=aIndicators.Items[i] as ISCIndicatorValueSupport;
    end;
  end;

  lvIndicators.ViewStyle:=vsIcon;
  lvIndicators.ViewStyle:=vsList;
end;

constructor TfmTradeLineAnalysisDialog.Create(const aStockChart: IStockChart);
var
  s: string;
  i: integer;
begin
  inherited;

  s:='';
  for i:= 0 to taReport.FieldCount- 1 do
  begin
    s:=s+taReport.Fields[i].FieldName+', ';
  end;

  s:='Filter. Available fields: '+StrDeleteRightEdge(s,', ');
  edFilter.Hint:=s;

  RegisterPersistValue(buOpenOrder,true);
  RegisterPersistValue(buCloseOrder,false);
  RegisterPersistValue(ckSyncAllCharts,true);
end;

destructor TfmTradeLineAnalysisDialog.Destroy;
begin
  inherited;
end;

procedure TfmTradeLineAnalysisDialog.edFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
  begin
    taReport.Filter:=edFilter.Text;
    taReport.Filtered:=true;
  end;
end;

procedure TfmTradeLineAnalysisDialog.grReportBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  if (Column.Field=taReportProfit) or (Column.Field=taReportIndicator) then
  begin
    if Column.Field.IsNull then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWindow
    else if Column.Field.AsFloat<0 then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end
  else if Column.Field=taReportOrderKind then
  begin
    if Column.Field.IsNull then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWindow
    else if Column.Field.AsString=OrderKindNames[okSell] then
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightBlue
    else
      TEditDBGrid(Sender).Canvas.Brush.Color:=clWebBisque;
  end;
end;

procedure TfmTradeLineAnalysisDialog.grReportDblClick(Sender: TObject);
var
  aX1,aX2: TDateTime;
begin
  inherited;
  TWaitCursor.SetUntilIdle;

  if ckSyncAllCharts.Checked then
  begin
    aX1:=taReportDateTime.Value;
    aX2:=aX1+StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay;
    StockChart.GetProject.HilightOnCharts(aX1,aX2,true);
  end
  else begin
    StockChart.LocateTo(taReportDateTime.Value,lmCenter);
    StockChart.Mark(taReportDateTime.Value);
  end;
end;

procedure TfmTradeLineAnalysisDialog.lvIndicatorsCreateItemClass(Sender: TCustomListView;
  var ItemClass: TListItemClass);
begin
  inherited;
  ItemClass:=TIndicatorListItem;
end;

procedure TfmTradeLineAnalysisDialog.lvIndicatorsDblClick(Sender: TObject);
begin
  inherited;
  if lvIndicators.Selected<>nil then
    acStart.Execute;
end;

class procedure TfmTradeLineAnalysisDialog.Run(const aIndicator: ISCIndicatorTradeLine; const aStockChart: IStockChart);
begin
  with TfmTradeLineAnalysisDialog.Create(aStockChart) do
  begin
    FIndicator:=aIndicator;
    Caption:=IndicatorFactory.GetIndicatorInfo((FIndicator as ISCIndicator).GetIID).Name+': '+Caption;
    CollectIndicators;
    Forms.Application.ProcessMessages;
    Show;
  end;
end;

end.
