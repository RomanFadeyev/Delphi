unit FC.StockChart.UnitTask.MBB.CalculateProfitDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls;

type
  TfmMBBCalculateProfitDialog = class(TfmDialogClose_B)
    buCalculate: TButton;
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    grReport: TEditDBGrid;
    taReportNumber: TIntegerField;
    Label1: TLabel;
    laBestValue: TLabel;
    Label2: TLabel;
    pbProgress: TProgressBar;
    taReportOpenTime: TDateTimeField;
    taReportOpenPrice: TCurrencyField;
    taReportCloseTime: TDateTimeField;
    taReportClosePrice: TCurrencyField;
    taReportProfitPt: TIntegerField;
    procedure buCalculateClick(Sender: TObject);
  private
    FIndicator: ISCIndicatorMBB;

    procedure Calculate;
  public
    class procedure Run(const aIndicator: ISCIndicatorMBB);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation
  uses Math;

type
  TDirection = (dNone,dUp,dDown);

{$R *.dfm}

{ TfmCalculateWidthDialog }

type
 TOrderState = (osNone,osBuy,osSell);

procedure TfmMBBCalculateProfitDialog.Calculate;
var
  i,j: Integer;
  aTotalProfit: integer;
  aInputData: ISCInputDataCollection;
  aCurProgress: integer;
  aOrderState : TOrderState;
  aValue : integer;
  aOpenPrice: TStockRealNumber;
  aOpenDate: TDateTime;
  aClosePrice: TStockRealNumber;
  aProfit: integer;
  aNumber: integer;
begin
  TWaitCursor.SetUntilIdle;
  aInputData:=FIndicator.GetInputData;
  aTotalProfit:=0;

//  taReport.DisableControls;
  try
    taReport.EmptyTable;
    taReport.Open;

    pbProgress.Max:=100;
    pbProgress.Position:=0;
    pbProgress.Visible:=true;
    aCurProgress:=0;


    aOrderState:=osNone;
    aOpenPrice:=0;
    aOpenDate:=0;
    aNumber:=1;

    for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
    begin
      aValue:=FIndicator.GetCrosses(i);
      if aValue<>0 then
      begin
        aClosePrice:=0;
        aProfit:=0;

        case aOrderState of
          osBuy: begin
            if aValue=1 then
            begin
              aClosePrice:=FIndicator.GetTopLine(i);
              aProfit:=aInputData.PriceToPoint(aClosePrice-aOpenPrice);
            end;
          end;
          osSell: begin
            if aValue=-1 then
            begin
              aClosePrice:=FIndicator.GetBottomLine(i);
              aProfit:=aInputData.PriceToPoint(aOpenPrice-aClosePrice);
            end;
          end;
        end;

        if aClosePrice<>0 then
        begin
          taReport.Append;
          taReportNumber.Value:=aNumber;
          inc(aNumber);
          taReportOpenTime.Value:=aOpenDate;
          taReportOpenPrice.Value:=aOpenPrice;
          taReportCloseTime.Value:=aInputData.DirectGetItem_DataDateTime(i);
          taReportClosePrice.Value:=aClosePrice;
          taReportProfitPt.Value:=aProfit;
          taReport.Post;

          aTotalProfit:=aTotalProfit+aProfit;
        end;

        if aValue=1 then
        begin
          aOrderState:=osSell;
          aOpenPrice:=FIndicator.GetTopLine(i);
          aOpenDate:=aInputData.DirectGetItem_DataDateTime(i);
        end
        else if aValue=-1 then
        begin
          aOrderState:=osBuy;
          aOpenPrice:=FIndicator.GetBottomLine(i);
          aOpenDate:=aInputData.DirectGetItem_DataDateTime(i);
        end;
      end;

      j:=(i div aInputData.Count)*100;
      if j<>aCurProgress then
      begin
        pbProgress.Position:=j;
        aCurProgress:=j;
        Application.ProcessMessages;
      end;

    end;
  finally
    grReport.RefreshSort;
    pbProgress.Visible:=false;
    //taReport.EnableControls;
  end;

  laBestValue.Caption:=IntToStr(aTotalProfit)+' pt';
end;

procedure TfmMBBCalculateProfitDialog.buCalculateClick(Sender: TObject);
begin
  inherited;
  Calculate;
end;


constructor TfmMBBCalculateProfitDialog.Create(aOwner: TComponent);
begin
  inherited;
end;

destructor TfmMBBCalculateProfitDialog.Destroy;
begin
  inherited;
end;

class procedure TfmMBBCalculateProfitDialog.Run(const aIndicator: ISCIndicatorMBB);
begin
  with TfmMBBCalculateProfitDialog.Create(nil) do
  try
    FIndicator:=aIndicator;
    Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;
    ShowModal;
  finally
    Free;
  end;
end;

end.
