unit FC.StockChart.UnitTask.MBB.AdjustWidthDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls;

type
  TfmMBBAdjustWidthDialog = class(TfmDialogClose_B)
    buAdjust: TButton;
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    grReport: TEditDBGrid;
    taReportPeriod: TIntegerField;
    taReportProfit: TFloatField;
    taReportNumber: TIntegerField;
    Label1: TLabel;
    laBestValue: TLabel;
    Label2: TLabel;
    edJitter: TExtendSpinEdit;
    Label3: TLabel;
    taReportTrendPeriod: TIntegerField;
    pbProgress: TProgressBar;
    DateTimePicker1: TExtendDateTimePicker;
    Label4: TLabel;
    DateTimePicker2: TExtendDateTimePicker;
    Label5: TLabel;
    procedure buAdjustClick(Sender: TObject);
  private
    FIndicator: ISCIndicatorMBB;

    function  Iterate:double;
    procedure Adjust;
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

{ TfmAdjustWidthDialog }

function TfmMBBAdjustWidthDialog.Iterate: double;
var
  i: integer;
  aSumm     : double;
  aInputData: ISCInputDataCollection;
  aTop,aBottom: TSCRealNumber;
  aCurrentDirection: TDirection;
  aPrevValue : TSCRealNumber;
  aJitter: TSCRealNumber;
  aDelta   : TSCRealNumber;
begin
  aInputData:=FIndicator.GetInputData;
  aCurrentDirection:=dNone;
  aPrevValue:=0;
  aSumm:=0;
  aJitter:=aInputData.PointToPrice(edJitter.Value);

  //Проверка левая, нужна только для того, чтобы обратиться к последнему значению
  //и тем самым сразу расчитать весь диапазон. Небольшая оптимизация
  if IsNan(FIndicator.GetValue(aInputData.Count-1)) then
    raise EAlgoError.Create;


  for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
  begin
    aBottom:=aInputData.RoundPrice(FIndicator.GetBottomLine(i));
    //Пересечение нижней границы
    if aInputData.DirectGetItem_DataLow(i)<=aBottom then
    begin
      case aCurrentDirection of
        dNone: ;
        //Уже отталкивлись от границы и опять от нее отталкиваемся
        dUp: begin
          aDelta:= (aBottom-aPrevValue);
          if abs(aDelta)>aJitter then
            aSumm:=aSumm+aDelta;
        end;
        dDown: begin
          aSumm:=aSumm+(aPrevValue-aBottom);
        end;
      end;

      aCurrentDirection:=dUp;
      aPrevValue:=aBottom;
      continue;
    end;

    aTop:=aInputData.RoundPrice(FIndicator.GetTopLine(i));
    //Пересечение верхней границы
    if aInputData.DirectGetItem_DataHigh(i)>=aTop then
    begin
      case aCurrentDirection of
        dNone: ;
        dUp: begin
          aSumm:=aSumm+(aTop-aPrevValue);
        end;
        //Уже отталкивлись от границы и опять от нее отталкиваемся
        dDown: begin
          aDelta:=(aPrevValue-aTop);
          if abs(aDelta)>aJitter then
            aSumm:=aSumm+aDelta;
        end;
      end;

      aCurrentDirection:=dDown;
      aPrevValue:=aTop;
      continue;
    end;
  end;

  result:=aSumm
end;

procedure TfmMBBAdjustWidthDialog.Adjust;
var
  i,j: Integer;
  aBestPeriod,aBestTrendPeriod: integer;
  aBestValue: double;
begin
  TWaitCursor.SetUntilIdle;

  aBestValue:=0;
  aBestPeriod:=21;
  aBestTrendPeriod:=40;

  taReport.DisableControls;
  try
    taReport.EmptyTable;
    taReport.Open;

    pbProgress.Max:=100-21;
    pbProgress.Position:=0;
    pbProgress.Visible:=true;


    j:=0;
    for i := 21 to 100 do
    begin
      Application.ProcessMessages;
      FIndicator.SetPeriod(i);

      FIndicator.GetGeneralTrendMA.SetPeriod(40);
      while FIndicator.GetGeneralTrendMA.GetPeriod<200 do
      begin
        taReport.Append;
        taReport['Number']:=j;
        taReport['TrendPeriod']:=FIndicator.GetGeneralTrendMA.GetPeriod;
        taReport['Period']:=i;
        taReport['Profit']:=FIndicator.GetInputData.PriceToPoint(Iterate);
        if aBestValue<taReport['Profit'] then
        begin
          aBestValue:=taReport['Profit'];
          aBestPeriod:=i;
          aBestTrendPeriod:=FIndicator.GetGeneralTrendMA.GetPeriod;
        end;

        taReport.Post;

        FIndicator.GetGeneralTrendMA.SetPeriod(FIndicator.GetGeneralTrendMA.GetPeriod+10);
      end;

      pbProgress.StepIt;
      Application.ProcessMessages;
    end;
  finally
    grReport.RefreshSort;

    pbProgress.Visible:=false;
    taReport.EnableControls;
  end;

  FIndicator.SetPeriod(aBestPeriod);
  laBestValue.Caption:=IntToStr(aBestTrendPeriod)+', '+IntToStr(aBestPeriod);
end;

procedure TfmMBBAdjustWidthDialog.buAdjustClick(Sender: TObject);
begin
  inherited;
  Adjust;
end;


constructor TfmMBBAdjustWidthDialog.Create(aOwner: TComponent);
begin
  inherited;
end;

destructor TfmMBBAdjustWidthDialog.Destroy;
begin
  inherited;
end;

class procedure TfmMBBAdjustWidthDialog.Run(const aIndicator: ISCIndicatorMBB);
begin
  with TfmMBBAdjustWidthDialog.Create(nil) do
  try
    FIndicator:=aIndicator;
    Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;    
    ShowModal;
  finally
    Free;
  end;
end;

end.
