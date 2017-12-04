unit FC.StockChart.UnitTask.Calendar.NavigatorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, CheckLst, ComCtrls, ExtCtrls, ExtendControls,
  StockChart.Definitions.Units,
  FC.Definitions, Mask, Spin, ActnList, FC.StockChart.CustomDialog_B, ImgList, JvCaptionButton, JvComponentBase,
  JvDockControlForm;

type
  TfmCalendarNavigatorDialog = class(TfmStockChartCustomDialog_B)
    Panel2: TPanel;
    Panel1: TPanel;
    pbProgress: TProgressBar;
    buSearchNext: TButton;
    buReset: TButton;
    buPrev: TButton;
    Button1: TButton;
    Button2: TButton;
    ActionList1: TActionList;
    acPrev: TAction;
    acNext: TAction;
    ckCopyToClipboard: TExtendCheckBox;
    ckSyncAllCharts: TExtendCheckBox;
    Label2: TLabel;
    cbWeekday: TExtendComboBox;
    edData: TExtendMemo;
    procedure acNextUpdate(Sender: TObject);
    procedure acPrevUpdate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure buPrevClick(Sender: TObject);
    procedure buSearchNextClick(Sender: TObject);
    procedure buResetClick(Sender: TObject);
    procedure buOKClick(Sender: TObject);
  private
    FIndicator: ISCIndicatorCalendar;
    FIndex: integer;
    FCurrentCalendarItem: integer;
    FCalendarMin,FCalendarMax: integer;

    function IsAppropriate(aCurrentCalendarItem: integer): boolean;
    procedure SetPosition;
  public
    constructor Create(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart); reintroduce;

    class procedure Run(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart);
  end;


implementation

uses ufmDialog_B,BaseUtils,DateUtils, Application.Definitions,StockChart.Definitions,StockChart.Definitions.Drawing, ClipBrd,
  ufmForm_B;

{$R *.dfm}

{ TfmCalendarNavigatorDialog }

constructor TfmCalendarNavigatorDialog.Create(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart);
var
  aList: TStringList;
  i: integer;
  aFirstDate,aLastDate: TDateTime;
begin
  inherited Create(aStockChart);
  FIndicator:=aExpert;

  Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;
  if StockChart.GetInputData.Count=0 then
    exit;

  aFirstDate:=StockChart.GetInputData.DirectGetItem_DataDateTime(0);
  aLastDate:=StockChart.GetInputData.DirectGetItem_DataDateTime(StockChart.GetInputData.Count-1);

  FCalendarMin:=0;
  for i := 0 to  FIndicator.GetData.Count - 1 do
    if FIndicator.GetData.GetItem(i).GetDateTime<aFirstDate then
      FCalendarMin:=i
    else
      break;

  FCalendarMax:=0;
  for i := FIndicator.GetData.Count - 1  downto 0 do
    if FIndicator.GetData.GetItem(i).GetDateTime>aLastDate then
      FCalendarMax:=i
    else
      break;

  aList:=TStringList.Create;
  try
    aList.Sorted:=true;
    aList.Duplicates:=dupIgnore;

    i:=FCalendarMin-1;
    repeat
      i:=FIndicator.GetNextItem(i);
      if (i<>-1) and (i<=FCalendarMax) then
        aList.Add(FIndicator.GetData.GetItem(i).GetIndicator);
    until (i=-1) or(i>FCalendarMax);

    cbWeekday.Items.Assign(aList);
  finally
    aList.Free;
  end;

  buResetClick(nil);
end;

function TfmCalendarNavigatorDialog.IsAppropriate(
  aCurrentCalendarItem: integer): boolean;
begin
  result:=StrIPos(cbWeekday.CurrentItem,FIndicator.GetData.GetItem(aCurrentCalendarItem).GetIndicator)<>0;
end;

procedure TfmCalendarNavigatorDialog.SetPosition;

function GetTextFromCalendarItem: string;
var
  aItem: ISCCalendarItem;
begin
  aItem:=FIndicator.GetData.GetItem(FCurrentCalendarItem);

  result:='Date:' +DefaultFormatter.DateTimeToStr(aItem.GetDateTime,false,true)+ #13#10+
          'Country:'+aItem.GetCountry+#13#10+
          'Name: '+aItem.GetIndicator+#13#10+
          'Priority: '+aItem.GetPriority+#13#10+
          'Period: '+aItem.GetPeriod+#13#10+
          'Previous Value: '+aItem.GetPreviousValue+#13#10+
          'Forecast Value: '+aItem.GetForecastValue+#13#10+
          'Fact Value: '+aItem.GetFactValue+#13#10+#13#10;
end;

var
  aInputData:ISCInputDataCollection;
  aData: ISCInputData;
  aX1,aX2: TDateTime;
begin
  if ((FIndex<0) or (FIndex>StockChart.GetInputData.Count-1)) and
     ((FCurrentCalendarItem<FCalendarMin) or (FCurrentCalendarItem>FCalendarMax)) then
  begin
    pbProgress.Visible:=false;
    exit;
  end;

  if ((FIndex<0) or (FIndex>StockChart.GetInputData.Count-1)) then
  begin
    edData.Text:=GetTextFromCalendarItem+'Cannot find bar';
    exit;
  end;

  pbProgress.Visible:=true;
  aInputData:=StockChart.GetInputData;

  pbProgress.Max:=aInputData.Count;
  pbProgress.Position:=FIndex;

  aData:=aInputData.Items[FIndex];

  StockChart.LocateTo(aData.DataDateTime,lmCenter);
  StockChart.Hilight(aData.DataDateTime,aData.DataDateTime+1/MinsPerDay);

  edData.Text:=GetTextFromCalendarItem+
                   'OC = '+ IntToStr(aInputData.PriceToPoint(Abs(aData.DataClose-aData.DataOpen)))+
                   '; HL = '+ IntToStr(aInputData.PriceToPoint(Abs(aData.DataHigh-aData.DataLow)));

  if ckCopyToClipboard.Checked then
  begin
    Clipboard.Open;
    Clipboard.AsText:=edData.Text;
    Clipboard.Close;
  end;

  if ckSyncAllCharts.Checked then
  begin
    aX1:=aData.DataDateTime;
    aX2:=aX1+StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay;
    StockChart.GetProject.HilightOnCharts(aX1,aX2,true,StockChart);
  end;
end;

procedure TfmCalendarNavigatorDialog.acNextUpdate(Sender: TObject);
begin
  acNext.Enabled:=FIndex<StockChart.GetInputData.Count-1;
end;

procedure TfmCalendarNavigatorDialog.acPrevUpdate(Sender: TObject);
begin
  acPrev.Enabled:=FIndex>0;
end;

procedure TfmCalendarNavigatorDialog.buOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmCalendarNavigatorDialog.buPrevClick(Sender: TObject);
begin
  edData.Text:='';
  if FCurrentCalendarItem>FIndicator.GetData.Count then
    FCurrentCalendarItem:=FIndicator.GetData.Count;

  while true do
  begin
    FCurrentCalendarItem:=FIndicator.GetPrevItem(FCurrentCalendarItem);
    if (FCurrentCalendarItem=-1) then
    begin
      FIndex:=-1;
      break;
    end
    else begin
      if IsAppropriate(FCurrentCalendarItem) then
      begin
        FIndex:=StockChart.GetInputData.FindExactMatched(FIndicator.GetData.GetItem(FCurrentCalendarItem).GetDateTime);
        break;
      end;
    end;
  end;

  SetPosition;
end;

procedure TfmCalendarNavigatorDialog.buResetClick(Sender: TObject);
begin
  inherited;
  FCurrentCalendarItem:=FCalendarMin-1;
  FIndex:=-1;
end;

procedure TfmCalendarNavigatorDialog.buSearchNextClick(Sender: TObject);
begin
  edData.Text:='';
  if FCurrentCalendarItem<-1 then
    FCurrentCalendarItem:=-1;

  while true do
  begin
    FCurrentCalendarItem:=FIndicator.GetNextItem(FCurrentCalendarItem);
    if (FCurrentCalendarItem=-1) then
    begin
      FCurrentCalendarItem:=FIndicator.GetData.Count;
      FIndex:=StockChart.GetInputData.Count;
      break;
    end
    else begin
      if IsAppropriate(FCurrentCalendarItem) then
      begin
        FIndex:=StockChart.GetInputData.FindExactMatched(FIndicator.GetData.GetItem(FCurrentCalendarItem).GetDateTime);
        break;
      end;
    end;
  end;

  SetPosition;
end;

procedure TfmCalendarNavigatorDialog.Button1Click(Sender: TObject);
begin
  inherited;
  FCurrentCalendarItem:=FCalendarMax+1;
  FIndex:=StockChart.GetInputData.Count;
  SetPosition;
end;

procedure TfmCalendarNavigatorDialog.Button2Click(Sender: TObject);
begin
  inherited;
  FCurrentCalendarItem:=FCalendarMin-1;
  FIndex:=-1;
  SetPosition;
end;

class procedure TfmCalendarNavigatorDialog.Run(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart);
begin
  with TfmCalendarNavigatorDialog.Create(aExpert,aStockChart) do
    Show;
end;


end.
