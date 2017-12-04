unit FC.StockChart.UnitTask.BB.AdjustWidthDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, BaseUtils,SystemService, ufmDialogClose_B, ufmDialog_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, Spin,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, DB, Grids, DBGrids, MultiSelectDBGrid,
  ColumnSortDBGrid, EditDBGrid, MemoryDS, ComCtrls;

type
  TfmAdjustWidthDialog = class(TfmDialogClose_B)
    buAdjust: TButton;
    taReport: TMemoryDataSet;
    DataSource1: TDataSource;
    grReport: TEditDBGrid;
    taReportPeriod: TIntegerField;
    taReportDeviations: TFloatField;
    taReportProfit: TFloatField;
    taReportNumber: TIntegerField;
    Label1: TLabel;
    laBestValue: TLabel;
    Label2: TLabel;
    edJitter: TExtendSpinEdit;
    Label3: TLabel;
    pbProgress: TProgressBar;
    procedure buAdjustClick(Sender: TObject);
  private
    FIndicator: ISCIndicatorBB;

    function  Iterate:double;
    procedure Adjust;
  public
    class procedure Run(const aIndicator: ISCIndicatorBB);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


implementation
  uses Math;

type
  TDirection = (dNone,dUp,dDown);

{$R *.dfm}

{ TfmAdjustWidthDialog }

function TfmAdjustWidthDialog.Iterate: double;
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
  aJitter:=FIndicator.GetInputData.PointToPrice(edJitter.Value);

  //Проверка левая, нужна только для того, чтобы обратиться к последнему значению
  //и тем самым сразу расчитать весь диапазон. Небольшая оптимизация
  if IsNan(FIndicator.GetValue(aInputData.Count-1)) then
    raise EAlgoError.Create;


  for i:=FIndicator.GetFirstValidValueIndex to aInputData.Count-1 do
  begin
    aBottom:=FIndicator.GetInputData.RoundPrice(FIndicator.GetBottomLine(i));
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

    aTop:=FIndicator.GetInputData.RoundPrice(FIndicator.GetTopLine(i));
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

procedure TfmAdjustWidthDialog.Adjust;
var
  i,j: Integer;
  aBestPeriod: integer;
  aBestDeviation,aBestValue: double;
begin
  TWaitCursor.SetUntilIdle;

  aBestValue:=0;
  aBestPeriod:=21;
  aBestDeviation:=2;

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
      FIndicator.SetPeriod(i);
      FIndicator.SetDeviation(1.5);
      while FIndicator.GetDeviation<4 do
      begin
        inc(j);
        taReport.Append;
        taReport['Number']:=j;
        taReport['Period']:=i;
        taReport['Deviations']:=FIndicator.GetDeviation;
        taReport['Profit']:=FIndicator.GetInputData.PriceToPoint(Iterate);
        if aBestValue<taReport['Profit'] then
        begin
          aBestValue:=taReport['Profit'];
          aBestPeriod:=i;
          aBestDeviation:=FIndicator.GetDeviation;
        end;

        taReport.Post;

        FIndicator.SetDeviation(FIndicator.GetDeviation+0.1);
      end;

      pbProgress.StepIt;
    end;
  finally
    grReport.RefreshSort;

    pbProgress.Visible:=false;
    taReport.EnableControls;
  end;

  FIndicator.SetPeriod(aBestPeriod);
  FIndicator.SetDeviation(aBestDeviation);
  laBestValue.Caption:=IntToStr(aBestPeriod)+', '+FormatCurr('0,.00', aBestDeviation);
end;

procedure TfmAdjustWidthDialog.buAdjustClick(Sender: TObject);
begin
  inherited;
  Adjust;
end;


constructor TfmAdjustWidthDialog.Create(aOwner: TComponent);
begin
  inherited;
end;

destructor TfmAdjustWidthDialog.Destroy;
begin
  inherited;
end;

class procedure TfmAdjustWidthDialog.Run(const aIndicator: ISCIndicatorBB);
begin
  with TfmAdjustWidthDialog.Create(nil) do
  try
    FIndicator:=aIndicator;
    ShowModal;
  finally
    Free;
  end;
end;

end.
