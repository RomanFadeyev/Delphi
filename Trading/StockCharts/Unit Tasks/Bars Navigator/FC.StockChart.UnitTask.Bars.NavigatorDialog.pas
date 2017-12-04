unit FC.StockChart.UnitTask.Bars.NavigatorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, CheckLst, ComCtrls, ExtCtrls, ExtendControls,
  StockChart.Definitions.Units,
  FC.Definitions, Mask, Spin, ActnList, FC.StockChart.CustomDialog_B, ImgList, JvCaptionButton, JvComponentBase,
  JvDockControlForm;

type
  TfmBarsNavigatorDialog = class(TfmStockChartCustomDialog_B)
    Panel2: TPanel;
    Panel1: TPanel;
    pbProgress: TProgressBar;
    buSearchNext: TButton;
    buReset: TButton;
    buPrev: TButton;
    pcPages: TPageControl;
    tsTime: TTabSheet;
    Label2: TLabel;
    Label1: TLabel;
    dtTime: TExtendMinutePicker;
    cbWeekday: TExtendComboBox;
    tsVolatility: TTabSheet;
    edOC: TExtendSpinEdit;
    Label4: TLabel;
    laFound: TLabel;
    Button1: TButton;
    Button2: TButton;
    ckOC: TExtendCheckBox;
    ckHL: TExtendCheckBox;
    edHL: TExtendSpinEdit;
    Label3: TLabel;
    buOR: TRadioButton;
    buAND: TRadioButton;
    ActionList1: TActionList;
    acPrev: TAction;
    acNext: TAction;
    ckCopyToClipboard: TExtendCheckBox;
    ckSyncAllCharts: TExtendCheckBox;
    procedure acNextUpdate(Sender: TObject);
    procedure acPrevUpdate(Sender: TObject);
    procedure ckOCClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure buPrevClick(Sender: TObject);
    procedure buSearchNextClick(Sender: TObject);
    procedure buResetClick(Sender: TObject);
    procedure buOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FIndicator: ISCIndicatorBars;
    FIndex: integer;

    function IsAppropriate(index: integer): boolean;
    procedure SetPosition(index: integer);
  public
    constructor Create(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart); reintroduce;

    class procedure Run(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
  end;


implementation

uses ufmDialog_B,DateUtils, Application.Definitions,StockChart.Definitions,StockChart.Definitions.Drawing, ClipBrd,
  ufmForm_B;

{$R *.dfm}

{ TfmBarsNavigatorDialog }

constructor TfmBarsNavigatorDialog.Create(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
begin
  inherited Create(aStockChart);
  FIndicator:=aExpert;

  Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+Caption;

  cbWeekday.Items.Add('All');
  cbWeekday.Items.Add(WeekDaysLong[1]);
  cbWeekday.Items.Add(WeekDaysLong[2]);
  cbWeekday.Items.Add(WeekDaysLong[3]);
  cbWeekday.Items.Add(WeekDaysLong[4]);
  cbWeekday.Items.Add(WeekDaysLong[5]);

  try
    cbWeekday.ItemIndex:=Workspace.Storage(self).ReadInteger(cbWeekday,'ItemIndex',0);
  except
  end;

  try
    pcPages.ActivePageIndex:=Workspace.Storage(self).ReadInteger(pcPages,'ItemIndex',0);
  except
  end;

  edHL.Value:=Workspace.Storage(self).ReadInteger(edHL,'Value',20);
  edOC.Value:=Workspace.Storage(self).ReadInteger(edOC,'Value',10);
  dtTime.Time:=Workspace.Storage(self).ReadDateTime(dtTime,'Time',0);

  RegisterPersistValue(buAND,true);
  buOR.Checked:=not buAND.Checked;
  RegisterPersistValue(ckOC,true);
  RegisterPersistValue(ckHL,true);
  RegisterPersistValue(ckCopyToClipboard,true);
  RegisterPersistValue(ckSyncAllCharts,true);

  buResetClick(nil);
  ckOCClick(nil);
end;

procedure TfmBarsNavigatorDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Workspace.Storage(self).WriteInteger(cbWeekday,'ItemIndex',cbWeekday.ItemIndex);
  Workspace.Storage(self).WriteInteger(pcPages,'ItemIndex',pcPages.ActivePageIndex);
  Workspace.Storage(self).WriteDateTime(dtTime,'Time',dtTime.Time);
  Workspace.Storage(self).WriteInteger(edHL,'Value',edHL.Value);
  Workspace.Storage(self).WriteInteger(edOC,'Value',edOC.Value);
end;

function TfmBarsNavigatorDialog.IsAppropriate(index: integer): boolean;
var
  aDT: TDateTime;
  aFrom,aTo: TDateTime;
  aData: ISCInputData;
  aHLR,aOCR: boolean;
begin
  result:=false;

  if pcPages.ActivePage=tsTime then
  begin
    aFrom:= Frac(dtTime.Time);
    aTo:=aFrom+(StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay)-1/SecsPerDay;

    aDT:=FIndicator.GetInputData.DirectGetItem_DataDateTime(index);
    if (CompareDateTime(Frac(aDT),aFrom)>=0) and (CompareDateTime(Frac(aDT),aTo)<=0) then
    begin
      if (cbWeekday.ItemIndex=0) or (cbWeekday.ItemIndex=DayOfTheWeek(aDT))  then
      begin
        result:=true;
      end;
    end;
  end
  else if pcPages.ActivePage=tsVolatility then
  begin
    aData:=StockChart.GetInputData.Items[index];
    aHLR:=ckHL.Checked;
    aOCR:=ckOC.Checked;

    if aHLR then
      aHLR:=Abs(StockChart.GetInputData.PriceToPoint(Abs(aData.DataHigh-aData.DataLow)))>edHL.Value;
    if aOCR then
      aOCR:=Abs(StockChart.GetInputData.PriceToPoint(Abs(aData.DataOpen-aData.DataClose)))>edOC.Value;

    if ckHL.Checked and ckOC.Checked then
    begin
      if buAND.Checked then
        result:=aHLR and aOCR
      else
        result:=aHLR or aOCR;
    end
    else if ckHL.Checked then
      result:=aHLR
    else if ckOC.Checked then
      result:=aOCR;
  end;
end;

procedure TfmBarsNavigatorDialog.SetPosition(index: integer);
var
  aInputData:ISCInputDataCollection;
  aData: ISCInputData;
  aX1,aX2: TDateTime;
begin
  FIndex:=index;

  aInputData:=StockChart.GetInputData;

  pbProgress.Max:=aInputData.Count;
  pbProgress.Position:=FIndex;

  aData:=aInputData.Items[FIndex];

  StockChart.LocateTo(aData.DataDateTime,lmCenter);
  StockChart.Hilight(aData.DataDateTime,aData.DataDateTime+1/MinsPerDay);

  laFound.Caption:=DefaultFormatter.DateTimeToStr(aData.DataDateTime,false,true)+
                   '; OC = '+ IntToStr(aInputData.PriceToPoint(Abs(aData.DataClose-aData.DataOpen)))+
                   '; HL = '+ IntToStr(aInputData.PriceToPoint(Abs(aData.DataHigh-aData.DataLow)));

  if ckCopyToClipboard.Checked then
  begin
    Clipboard.Open;
    Clipboard.AsText:=laFound.Caption;
    Clipboard.Close;
  end;

  if ckSyncAllCharts.Checked then
  begin
    aX1:=aData.DataDateTime;
    aX2:=aX1+StockChart.StockSymbol.GetTimeIntervalValue/MinsPerDay;
    StockChart.GetProject.HilightOnCharts(aX1,aX2,true,StockChart);
  end;
end;

procedure TfmBarsNavigatorDialog.acNextUpdate(Sender: TObject);
begin
  acNext.Enabled:=FIndex<StockChart.GetInputData.Count-1;
end;

procedure TfmBarsNavigatorDialog.acPrevUpdate(Sender: TObject);
begin
  acPrev.Enabled:=FIndex>0;
end;

procedure TfmBarsNavigatorDialog.buOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmBarsNavigatorDialog.buPrevClick(Sender: TObject);
begin
  laFound.Caption:='';
  if FIndex>StockChart.GetInputData.Count then
    FIndex:=StockChart.GetInputData.Count;

  while true do
  begin
    Dec(FIndex);
    if FIndex<0 then
      break;

    if IsAppropriate(FIndex) then
    begin
      SetPosition(FIndex);
      break;
    end;
  end;
end;

procedure TfmBarsNavigatorDialog.buResetClick(Sender: TObject);
begin
  inherited;
  FIndex:=-1;
end;

procedure TfmBarsNavigatorDialog.buSearchNextClick(Sender: TObject);
begin
  laFound.Caption:='';
  if FIndex<-1 then
    FIndex:=-1;

  while true do
  begin
    Inc(FIndex);
    if FIndex>=StockChart.GetInputData.Count then
      break;

    if IsAppropriate(FIndex) then
    begin
      SetPosition(FIndex);
      break;
    end;
  end;
end;

procedure TfmBarsNavigatorDialog.Button1Click(Sender: TObject);
begin
  inherited;
  SetPosition(StockChart.GetInputData.Count-1);
end;

procedure TfmBarsNavigatorDialog.Button2Click(Sender: TObject);
begin
  inherited;
  SetPosition(0);
end;

procedure TfmBarsNavigatorDialog.ckOCClick(Sender: TObject);
begin
  edOC.Enabled:=ckOC.Checked;
  edHL.Enabled:=ckHL.Checked;
  buAND.Enabled:=edOC.Enabled and edHL.Enabled;
  buOR.Enabled:=edOC.Enabled and edHL.Enabled;
end;

class procedure TfmBarsNavigatorDialog.Run(const aExpert: ISCIndicatorBars; const aStockChart: IStockChart);
begin
  with TfmBarsNavigatorDialog.Create(aExpert,aStockChart) do
    Show;
end;


end.
