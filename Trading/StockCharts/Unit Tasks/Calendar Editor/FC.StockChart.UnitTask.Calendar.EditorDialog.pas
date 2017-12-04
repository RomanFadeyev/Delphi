unit FC.StockChart.UnitTask.Calendar.EditorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FC.Dialogs.DockedDialogCloseAndAppWindow_B, JvDockControlForm, ImgList, JvComponentBase, JvCaptionButton, StdCtrls,
  ExtendControls, ExtCtrls,StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, DB,FC.StockData.InputDataCollectionToDataSetMediator,
  MemoryDS, ufmDialogOKCancel_B, ActnList, Mask, ComCtrls,
  ufmDialogOKCancelApply_B, Menus, ActnPopup, Buttons, OleServer, WordXP;

type
  TWorkMode = (wmAdding, wmEditing);

  TfmCalendarEditorDialog = class(TfmDialogOKCancelApply_B)
    Label1: TLabel;
    edOpenDate: TExtendDateTimePicker;
    edCountry: TExtendComboBox;
    Label6: TLabel;
    Label2: TLabel;
    edIndicator: TExtendEdit;
    Label3: TLabel;
    edPeriod: TExtendEdit;
    Label4: TLabel;
    edPriority: TExtendEdit;
    Label5: TLabel;
    edForecast: TExtendEdit;
    Label7: TLabel;
    edFact: TExtendEdit;
    Label8: TLabel;
    edNotes: TExtendEdit;
    Label9: TLabel;
    cbPFTrend: TExtendComboBox;
    Label10: TLabel;
    cbFFTrend: TExtendComboBox;
    Label11: TLabel;
    edPrevious: TExtendEdit;
    buPaste: TSpeedButton;
    pmPaste: TPopupActionBar;
    miFromIfcMarkets: TMenuItem;
    edOpenTime: TExtendMinutePicker;
    WordDocument: TWordDocument;
    miFromSaxoBankcom: TMenuItem;
    cbTimeZone: TExtendComboBox;
    Label12: TLabel;
    edSource: TExtendEdit;
    procedure miFromSaxoBankcomClick(Sender: TObject);
    procedure buPasteClick(Sender: TObject);
    procedure miFromIfcMarketsClick(Sender: TObject);
  private
    FItem: ISCCalendarItem;
    FSaved: boolean;
  protected
    function    LoadData : boolean;                                     override;
    function    SaveData : boolean;                                     override;
    function    IsDataChanged: boolean;                                 override;

    function    WorkMode: TWorkMode;
  public
    constructor Create(const aItem: ISCCalendarItem); reintroduce;

    class function RunEdit(const aItem: ISCCalendarItem):boolean;
    class function RunNew:boolean;
  end;


implementation

uses Types, Clipbrd, StrUtils, DateUtils, BaseUtils, SystemService, Application.Definitions,
     FC.Singletons,
     FC.StockData.StockCalendar,
     FC.DataUtils;

{$R *.dfm}

{ TfmCalendarEditorDialog }

procedure TfmCalendarEditorDialog.buPasteClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmPaste.Popup(P.x, P.y);
end;

constructor TfmCalendarEditorDialog.Create(const aItem: ISCCalendarItem);
var
  ii: TSCCalendarChangeType;
begin
  inherited Create(nil);
  for ii := Low(TSCCalendarChangeType) to High(TSCCalendarChangeType) do
  begin
    cbPFTrend.Items.AddData(CalendarChangeTypeNames[ii],integer(ii));
    cbFFTrend.Items.AddData(CalendarChangeTypeNames[ii],integer(ii));
  end;

  FItem:=aItem;
{
  if (FItem=nil) then
  begin
    edOpenDate.Enabled:=true;
    edOpenTime.Enabled:=true;
    edCountry.ReadOnly:=false;
    edIndicator.ReadOnly:=false;
    edPeriod.ReadOnly:=false;
  end;
}
  cbTimeZone.Items.AddData('GMT',0);

  if GetLocalTimeZoneHourOffset>0 then
    cbTimeZone.Items.AddData('Local (+'+IntToStr(GetLocalTimeZoneHourOffset)+')',1)
  else
    cbTimeZone.Items.AddData('Local ('+IntToStr(GetLocalTimeZoneHourOffset)+')',1);


  if GetHistoryDataTimeZoneHourOffset>0 then
    cbTimeZone.Items.AddData('History Center (+'+IntToStr(GetHistoryDataTimeZoneHourOffset)+')',2)
  else
    cbTimeZone.Items.AddData('History Center ('+IntToStr(GetHistoryDataTimeZoneHourOffset)+')',2);

  cbTimeZone.SelectByData(2);
end;

procedure TfmCalendarEditorDialog.miFromIfcMarketsClick(Sender: TObject);
var
  s: string;
  aStrings: TStringList;
  aSaveChanges: OleVariant;
begin
  TWaitCursor.SetUntilIdle;

  WordDocument.Connect;
  try
  WordDocument.Range.Paste;
  WordDocument.Range.Select;
  WordDocument.Range.Copy;
  aSaveChanges:=false;
  WordDocument.Close(aSaveChanges);
  finally
    WordDocument.Disconnect;
  end;

  Clipboard.Open;
  try
    s:=StrTrimRight(Clipboard.AsText,[#13,#10]);
  finally
    Clipboard.Close;
  end;

  aStrings:=TStringList.Create;
  try
    SplitString(s,aStrings,[#9],false);

    //Country
    try
      edCountry.Text:=aStrings[0];
    except
      MsgBox.MessageFailure(Handle,'Bad format: country not found');
      exit;
    end;

    //Indicator
    try
      edIndicator.Text:=aStrings[1];
    except
      MsgBox.MessageFailure(Handle,'Bad format: indicator not found');
      exit;
    end;

    //Time
    try
      edOpenTime.Time:=StrToTime(aStrings[2]);
    except
      MsgBox.MessageFailure(Handle,'Bad format: time is not valid');
      exit;
    end;

    //Forecast
    try
      edForecast.Text:=aStrings[3];
    except
      MsgBox.MessageFailure(Handle,'Bad format: forecast not found');
      exit;
    end;

    //Previous
    try
      edPrevious.Text:=aStrings[4];
    except
      MsgBox.MessageFailure(Handle,'Bad format: previous data not found');
      exit;
    end;
  finally
    aStrings.Free;
  end;
end;

procedure TfmCalendarEditorDialog.miFromSaxoBankcomClick(Sender: TObject);
var
  x: integer;
  I: Integer;
  Data: THandle;
  pch: pchar;
  aStrings: TStringList;
  aCountry: string;
  aSaveChanges: OleVariant;
  s: string;
begin
  inherited;
  aStrings:=TStringList.Create;
  Clipboard.Open;
  Data := GetClipboardData(49358);
  try
    if Data <> 0 then
      pch := PChar(GlobalLock(Data))
    else
      pch := '';
    aStrings.Text:=pch;
  finally
    if Data <> 0 then GlobalUnlock(Data);
    Clipboard.Close;
  end;
  for i := 0 to aStrings.Count - 1 do
  begin
    x:=StrIPos('<IMG',aStrings[i]);
    if x<>0 then
    begin
      x:=StrIPosEnd('alt=',aStrings[i],x);
      if (x<>0) then
      begin
        aCountry:=Trim(MidStr(aStrings[i],x,High(word)));
        aCountry:=StrDeleteEdges(aCountry);
        aCountry:=Trim(aCountry);
        break;
      end;
    end;
  end;

  WordDocument.Connect;
  try
  WordDocument.Range.Paste;
  WordDocument.Range.Select;
  WordDocument.Range.Copy;
  aSaveChanges:=false;
  WordDocument.Close(aSaveChanges);
  finally
    WordDocument.Disconnect;
  end;

  Clipboard.Open;
  try
    s:=StrTrimRight(Clipboard.AsText,[#13,#10]);
  finally
    Clipboard.Close;
  end;


  aStrings.Clear;
  try
    SplitString(s,aStrings,[#9],false);

    //Country
    try
      edCountry.Text:=aCountry;
    except
      MsgBox.MessageFailure(Handle,'Bad format: country not found');
      exit;
    end;

    //Indicator
    try
      edIndicator.Text:=aStrings[2];
    except
      MsgBox.MessageFailure(Handle,'Bad format: indicator not found');
      exit;
    end;

    //Time
    try
      edOpenTime.Time:=IncHour(StrToTime(aStrings[0]),2);
    except
      MsgBox.MessageFailure(Handle,'Bad format: time is not valid');
      exit;
    end;

    //Period
    try
      edPeriod.Text:=aStrings[3];
    except
      MsgBox.MessageFailure(Handle,'Bad format: time is not valid');
      exit;
    end;

    //Forecast
    try
      edForecast.Text:=aStrings[4];
    except
      MsgBox.MessageFailure(Handle,'Bad format: forecast not found');
      exit;
    end;

    //Previous
    try
      edPrevious.Text:=aStrings[5];
    except
      MsgBox.MessageFailure(Handle,'Bad format: previous data not found');
      exit;
    end;
  finally
    aStrings.Free;
  end;
end;

function TfmCalendarEditorDialog.IsDataChanged: boolean;
begin
  if WorkMode=wmEditing then
  begin
    result:=
    (not SameDateTime(Trunc(edOpenDate.DateTime)+Frac(edOpenTime.Time),FItem.GetDateTime)) or
    (edCountry.Text<>FItem.GetCountry) or
    (edIndicator.Text<>FItem.GetIndicator) or
    (edPriority.Text<>FItem.GetPriority) or
    (edPeriod.Text<>FItem.GetPeriod) or
    (edPrevious.Text<>FItem.GetPreviousValue) or
    (edForecast.Text<>FItem.GetForecastValue) or
    (edFact.Text<>FItem.GetFactValue) or
    (cbPFTrend.CurrentItemData<>integer(FItem.GetPFChangeType)) or
    (cbFFTrend.CurrentItemData<>integer(FItem.GetFFChangeType)) or
    (edSource.Text<>FItem.GetSource) or 
    (edNotes.Text<>FItem.GetNotes);
  end
  else begin
    result:=true;
  end;
end;

function TfmCalendarEditorDialog.LoadData: boolean;
var
  aCountries: TStringDynArray;
  i: integer;
begin
  aCountries:=StockDataStorage.QueryStockDataCalendarCountries;
  for i := 0 to High(aCountries) do
    edCountry.Items.Add(aCountries[i]);

  
  if WorkMode=wmEditing then
  begin
    edOpenDate.DateTime:=FItem.GetDateTime;
    edOpenTime.Time:=Frac(FItem.GetDateTime);

    edCountry.Text:=FItem.GetCountry;
    edIndicator.Text:=FItem.GetIndicator;
    edPriority.Text:=FItem.GetPriority;

    edPeriod.HandleNeeded; //??? какая-то фигня. теряется Text
    edPeriod.Text:=FItem.GetPeriod;

    edPrevious.Text:=FItem.GetPreviousValue;
    edForecast.Text:=FItem.GetForecastValue;
    edFact.Text:=FItem.GetFactValue;
    edNotes.Text:=FItem.GetNotes;
    edSource.Text:=FItem.GetSource;


      //в какую сторону сменился показатель, если сравнивать Previous и Fact
    cbPFTrend.SelectByData(integer(FItem.GetPFChangeType));
      //в какую сторону сменился показатель, если сравнивать Forecast и Fact
    cbFFTrend.SelectByData(integer(FItem.GetFFChangeType));
  end
  else begin
    edOpenDate.DateTime:=GetNowInHistoryCenterZone;
    edOpenTime.Time:=Frac(GetNowInHistoryCenterZone);

     //в какую сторону сменился показатель, если сравнивать Previous и Fact
    cbPFTrend.SelectByData(integer(cttUnknown));
    //в какую сторону сменился показатель, если сравнивать Forecast и Fact
    cbFFTrend.SelectByData(integer(cttUnknown));
  end;

  result:=true;
end;

function TfmCalendarEditorDialog.SaveData: boolean;
var
  aCalendar: IStockCalendarWriteable;
  aDate: TDateTime;
begin
  aDate:=Trunc(edOpenDate.DateTime)+Frac(edOpenTime.Time);

  if (cbTimeZone.CurrentItemData=0) then //GMT
    aDate:=IncHour(aDate,GetHistoryDataTimeZoneHourOffset)
  else if cbTimeZone.CurrentItemData=1 then //local
    aDate:=LocalTimeToHistoryTime(aDate);

  aCalendar:=TStockCalendar.Create;

  if WorkMode=wmEditing then
  begin
    aCalendar.Add(
      FItem.GetID,
      aDate,
      edCountry.Text,
      FItem.GetClass,
      edIndicator.Text,
      edPriority.Text,
      edPeriod.Text,
      edPrevious.Text,
      edForecast.Text,
      edFact.Text,
      TSCCalendarChangeType(cbPFTrend.CurrentItemData),
      TSCCalendarChangeType(cbFFTrend.CurrentItemData),
      edSource.Text,
      edNotes.Text);

    StockDataStorage.UpdateCalendarItem(aCalendar.GetItem(0));
    FItem:=aCalendar.GetItem(0);
  end
  else begin
    aCalendar.Add(
      -1,
      aDate,
      edCountry.Text,
      0,
      edIndicator.Text,
      edPriority.Text,
      edPeriod.Text,
      edPrevious.Text,
      edForecast.Text,
      edFact.Text,
      TSCCalendarChangeType(cbPFTrend.CurrentItemData),
      TSCCalendarChangeType(cbFFTrend.CurrentItemData),
      edSource.Text,
      edNotes.Text);

    StockDataStorage.ImportCalendar(aCalendar,imMergeReplaceOld,nil);
  end;

  FSaved:=true;
  result:=true;
end;

function TfmCalendarEditorDialog.WorkMode: TWorkMode;
begin
  if FItem=nil then
    Result:=wmAdding
  else
    Result:=wmEditing;
end;

class function TfmCalendarEditorDialog.RunEdit(const aItem: ISCCalendarItem):boolean;
begin
  with TfmCalendarEditorDialog.Create(aItem) do
  try
    ShowModal;
    result:=FSaved;
  finally
    Free;
  end;
end;


class function TfmCalendarEditorDialog.RunNew: boolean;
begin
  with TfmCalendarEditorDialog.Create(nil) do
  try
    ShowModal;
    result:=FSaved;
  finally
    Free;
  end;
end;

end.
