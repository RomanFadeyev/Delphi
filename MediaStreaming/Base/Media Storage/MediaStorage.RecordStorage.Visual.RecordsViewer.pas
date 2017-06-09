unit MediaStorage.RecordStorage.Visual.RecordsViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtendControls, ComCtrls, ExtCtrls, ActnList,
  ToolWin, ImgList,Menus,
  MediaStorage.RecordStorage, Data.DB, MemDS, Vcl.Grids, Vcl.DBGrids,
  MultiSelectDBGrid, SortDBGrid;

const
  WM_SET_FILE_EXISTS_STATUS = WM_USER+200;

type
  TRecordSourceListItemData = record
    Name: string;
    //Url  : string;
    Id  : TrsRecordSourceId;
    //ReserveConnectionString: string;
    //Available : boolean;
  end;


  TfrmMediaStorageRecordsView = class(TFrame)
    ilImages: TImageList;
    alActions: TActionList;
    acCheckFileExists: TAction;
    mmLog: TExtendMemo;
    Splitter2: TSplitter;
    acProperties: TAction;
    acPlay: TAction;
    acCopy: TAction;
    acCopyFileNameToClipboard: TAction;
    pmRecords: TPopupMenu;
    N1: TMenuItem;
    dlgSave: TSaveDialogEx;
    paRecordSources: TPanel;
    Label1: TLabel;
    lvRecordSources: TSortDBGrid;
    Splitter1: TSplitter;
    pcData: TPageControlEx;
    tsData: TTabSheet;
    Panel2: TPanel;
    Label2: TLabel;
    lvRecords: TSortDBGrid;
    ToolBar1: TToolBar;
    ToolButton7: TToolButton;
    ToolButton3: TToolButton;
    ToolButton8: TToolButton;
    ToolButton6: TToolButton;
    ToolButton5: TToolButton;
    tsWelcome: TTabSheet;
    acRefresh: TAction;
    tsEmpty: TTabSheet;
    paWelcome: TPanel;
    paLoad: TPanel;
    laRefresh: TExtendLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLoadFrom: TExtendDateTimePicker;
    edLoadTo: TExtendDateTimePicker;
    ToolButton1: TToolButton;
    laLoadDataAjust: TExtendLabel;
    acDeleteAbsent: TAction;
    ToolButton2: TToolButton;
    acDelete: TAction;
    ToolButton4: TToolButton;
    ToolButton9: TToolButton;
    ToolBar2: TToolBar;
    ToolButton18: TToolButton;
    acRecordSourceRefresh: TAction;
    acRecordSourceGetStatistics: TAction;
    ToolButton10: TToolButton;
    taRecordSources: TMemDataSet;
    dsRecordSources: TDataSource;
    taRecordSourcesID: TStringField;
    taRecordSourcesNAME: TStringField;
    taRecordSourcesMINDATE: TDateTimeField;
    taRecordSourcesMAXDATE: TDateTimeField;
    taRecordSourcesRECORD_COUNT: TIntegerField;
    taRecordSourcesTOTAL_DURATION: TDateTimeField;
    acRecordSourceDelete: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    dsRecords: TDataSource;
    taRecords: TMemDataSet;
    taRecordsID: TStringField;
    taRecordsNAME: TStringField;
    taRecordsBEGIN: TDateTimeField;
    taRecordsEND: TDateTimeField;
    taRecordsTRANSPORT: TStringField;
    taRecordsPATH: TStringField;
    taRecordsDATA_TRANSPORT: TVariantField;
    taRecordsACCESSIBLE: TSmallintField;
    taRecordsOWNER: TStringField;
    procedure acDeleteUpdate(Sender: TObject);
    procedure lvRecordsDeletion(Sender: TObject; Item: TListItem);
    procedure acDumpUpdate(Sender: TObject);
    procedure acCheckFileExistsExecute(Sender: TObject);
    procedure acPropertiesExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCopyFileNameToClipboardUpdate(Sender: TObject);
    procedure acCopyFileNameToClipboardExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure laRefreshClick(Sender: TObject);
    procedure paWelcomeResize(Sender: TObject);
    procedure laLoadDataAjustClick(Sender: TObject);
    procedure acDeleteAbsentExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acRecordSourceRefreshExecute(Sender: TObject);
    procedure acRecordSourceGetStatisticsExecute(Sender: TObject);
    procedure lvRecordSourcesChangeRecord(Sender: TObject);
    procedure taRecordSourcesTOTAL_DURATIONGetText(Sender: TField;
      var Text: string; DisplayText: Boolean);
    procedure acRecordSourceDeleteUpdate(Sender: TObject);
    procedure acRecordSourceDeleteExecute(Sender: TObject);
    procedure taRecordsACCESSIBLEGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
  private
    FRecordStorage: TRecordStorage;
    FRetrieveInfoThread: TThread;

    procedure UpdateRecordsForCurrentRecordSource;
    procedure UpdateRecordSources;

    procedure WmSetFileExistsStatus(var Message: TMessage); message WM_SET_FILE_EXISTS_STATUS;
    function  GetRecordSource(index: integer): TRecordSourceListItemData;
  protected
    procedure Log(const aMessage: string);
  public
    procedure Init(aRecordStorage: TRecordStorage);

    function RecordSourceCount: integer;
    property RecordSources[index: integer]:TRecordSourceListItemData read GetRecordSource;

    function  IsSelectedRecordSource: boolean;
    function  SetSelectedRecordSource(const aSourceName: string):boolean; overload;
    function  SetSelectedRecordSource(const aCurrentRecordSourceId: TrsRecordSourceId):boolean; overload;

    function  SelectedRecordSource:TRecordSourceListItemData;

    destructor Destroy; override;
    constructor Create(aOwner: TComponent); override;
  end;

implementation
  uses Math, Variants, SyncObjs, VCLUtils,DateUtils, ClipBrd,uBaseClasses, uBaseUtils, ufmProgressWithBar, MediaStream.Framer,MediaStream.FramerFactory,
       MediaStorage.Transport,MediaStorage.Transport.Samba,MediaStorage.Transport.FileServer,
       MediaStorage.RecordStorage.Visual.RecordPreview,ufmChooseItems;

{$R *.DFM}

type
  TRetrieveFileInfoThread = class (TThread)
  private
    FRecords: TrsRecordObjectInfoArray;
    FOwner: TfrmMediaStorageRecordsView;
  protected
    procedure Execute; override;

    constructor Create(aOwner: TfrmMediaStorageRecordsView; const aRecords: TrsRecordObjectInfoArray);
  end;

  TCopyFileThread = class (TThread)
  private
    FSourceTransport: IRecordObjectTransport;
    FDestStream: TStream;
    FProcessedBytes: TThreadVar<int64>;
  protected
    procedure Execute; override;
  public
    constructor Create(const aSourceTransport: IRecordObjectTransport; aDestStream: TStream);
    destructor Destroy; override;

    function ProcessedBytes: int64;
  end;


type
  TRecordStorageReadDataProgress = class
  private
    FForm: TfmProgressWithBar;

    procedure OnRecordStorageReadProgress(aSender: TRecordStorage; aPosition, aSize: int64; var aContinue: boolean);
  public
    constructor Create(aOwner: TfrmMediaStorageRecordsView; const aText: string);
    destructor Destroy; override;
  end;

function GetTransport(const aV: variant): IRecordObjectTransport;
begin
  Assert(VarIsType(aV, varUnknown));
  if not Supports(aV, IRecordObjectTransport, Result) then
    result:=nil;
end;

{ TRecordStorageReadDataProgress }

constructor TRecordStorageReadDataProgress.Create(aOwner: TfrmMediaStorageRecordsView; const aText: string);
begin
  FForm:=TfmProgressWithBar.Create(Application);
  FForm.Caption:='Чтение данных';
  FForm.Text:=aText;

  //FForm.SetMarqueeMode;
  FForm.Show;
  FForm.Repaint;
end;

destructor TRecordStorageReadDataProgress.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

procedure TRecordStorageReadDataProgress.OnRecordStorageReadProgress(aSender: TRecordStorage; aPosition, aSize: int64; var aContinue: boolean);
begin
  if FForm.ModalResult=mrCancel then
  begin
    aContinue:=false;
    exit;
  end;

  if aSize>1024 then
  begin
    FForm.SetProgressMode(100,aPosition*100 div aSize);
    FForm.Repaint;
    if aSize>1024*1024 then
      FForm.laStatus.Caption:=Format('Прочитано %d МБ из %d МБ',[aPosition div 1024 div 1024, aSize div 1024 div 1024])
    else
      FForm.laStatus.Caption:=Format('Прочитано %d КБ из %d КБ',[aPosition div 1024, aSize div 1024]);
    Application.ProcessMessages;
  end;
end;

{ TRetrieveFileInfoThread }

constructor TRetrieveFileInfoThread.Create(aOwner: TfrmMediaStorageRecordsView;
  const aRecords: TrsRecordObjectInfoArray);
begin
  inherited Create(false);
  FOwner:=aOwner;
  FRecords:=aRecords;
end;

procedure TRetrieveFileInfoThread.Execute;
var
  i: Integer;
  aStatus: integer;
begin
  for i := 0 to High(FRecords) do
  begin
    if Terminated then
      break;

    try
      if FRecords[i].Transport.FileExists then
        aStatus:=1
      else
        aStatus:=0
    except
      aStatus:=0; //правильно ли?
    end;

    SendMessage(FOwner.WindowHandle,WM_SET_FILE_EXISTS_STATUS, WParam(PChar(FRecords[i].Id)),aStatus);
  end;
end;

{ TCopyFileThread }

constructor TCopyFileThread.Create(
  const aSourceTransport: IRecordObjectTransport; aDestStream: TStream);
begin
  FSourceTransport:=aSourceTransport;
  FDestStream:=aDestStream;
  FProcessedBytes:=TThreadVar<int64>.Create;
  inherited Create(false);
end;

destructor TCopyFileThread.Destroy;
begin
  FreeAndNil(FProcessedBytes);
  inherited;
end;

procedure TCopyFileThread.Execute;
const
  BufSize = 1024;
var
  aSourceStream: TStream;
  N: Integer;
  Buffer: PByte;
  aReader: IRecordObjectReader;
begin
  aReader:=FSourceTransport.GetReader;
  aSourceStream:=aReader.GetStream;

  GetMem(Buffer, BufSize);
  try
    while true do
    begin
      N:=aSourceStream.Read(Buffer^, BufSize);
      FDestStream.WriteBuffer(Buffer^, N);
      FProcessedBytes.Value:=aSourceStream.Position;

      if N<BufSize then
        break;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;

  aReader:=nil;
end;

function TCopyFileThread.ProcessedBytes: int64;
begin
  result:=FProcessedBytes.Value;
end;

{ TfmSystemConsole }

constructor TfrmMediaStorageRecordsView.Create(aOwner: TComponent);
begin
  inherited;
  pcData.ShowTabs:=false;
  pcData.ActivePage:=tsEmpty;
  edLoadTo.DateTime:=Now;
  edLoadFrom.DateTime:=Now-7;
end;

destructor TfrmMediaStorageRecordsView.Destroy;
begin
  FreeAndNil(FRetrieveInfoThread);
  taRecords.EmptyTable;
  taRecordSources.EmptyTable;
  inherited;
end;

procedure TfrmMediaStorageRecordsView.Init(aRecordStorage: TRecordStorage);
begin
  TWaitCursor.SetUntilIdle;

  FRecordStorage:=aRecordStorage;
  Caption:=Caption+': '+FRecordStorage.GetConnectionString;

  acDelete.Visible:=aRecordStorage.RecordWriteableAccess<>nil;
  acDeleteAbsent.Visible:=aRecordStorage.RecordWriteableAccess<>nil;
  acRecordSourceDelete.Visible:=aRecordStorage.RecordSourceWriteableAccess<>nil;

  UpdateRecordSources;
end;

function TfrmMediaStorageRecordsView.IsSelectedRecordSource: boolean;
begin
  result:=lvRecordSources.IsCurrentRecord;
end;

procedure TfrmMediaStorageRecordsView.laLoadDataAjustClick(Sender: TObject);
var
  aStart,aEnd:TDateTime;
begin
  FRecordStorage.GetRecordSourceDateRanges(SelectedRecordSource.Id,aStart,aEnd);
  if aStart=0 then
    MsgBox.MessageInfo(Handle,'В хранилище отсутствуют записи для данного источника');
  edLoadFrom.DateTime:=aStart;
  edLoadTo.DateTime:=aEnd;
end;

procedure TfrmMediaStorageRecordsView.laRefreshClick(Sender: TObject);
begin
  UpdateRecordsForCurrentRecordSource;
end;

procedure TfrmMediaStorageRecordsView.lvRecordSourcesChangeRecord(
  Sender: TObject);
begin
  if not IsSelectedRecordSource then
    pcData.ActivePage:=tsEmpty
  else
    pcData.ActivePage:=tsWelcome;
end;

procedure TfrmMediaStorageRecordsView.paWelcomeResize(Sender: TObject);
begin
  paLoad.Left:=(paWelcome.Width-paLoad.Width) div 2;
  paLoad.Top:=(paWelcome.Height-paLoad.Height) div 2;
end;

function TfrmMediaStorageRecordsView.RecordSourceCount: integer;
begin
  result:=lvRecordSources.DataSource.DataSet.RecordCount;
end;

function TfrmMediaStorageRecordsView.SelectedRecordSource: TRecordSourceListItemData;
begin
  if not IsSelectedRecordSource then
    raise Exception.Create('Не выбран источник записей');

  result:=GetRecordSource(taRecordSources.RecNo-1);
end;

function TfrmMediaStorageRecordsView.SetSelectedRecordSource(const aCurrentRecordSourceId: TrsRecordSourceId): boolean;
var
  i: Integer;
begin
  result:=false;
  for i := 0 to RecordSourceCount-1 do
    if RecordSources[i].Id.Equals(aCurrentRecordSourceId) then
    begin
      taRecordSources.RecNo:=i+1;
      result:=true;
      break;
    end;
end;

function TfrmMediaStorageRecordsView.SetSelectedRecordSource(const aSourceName: string): boolean;
var
  i: Integer;
begin
  result:=false;
  for i := 0 to RecordSourceCount-1 do
    if RecordSources[i].Name=aSourceName then
    begin
      taRecordSources.RecNo:=i+1;
      result:=true;
      break;
    end;
end;

procedure TfrmMediaStorageRecordsView.taRecordsACCESSIBLEGetText(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  if Sender.IsNull then
  else if Sender.Value=1 then
    Text:='Да'
  else if Sender.Value=0 then
    Text:='Нет'
  else if Sender.Value=2 then
    Text:='?'
  else
    Text:='Ошибка';
end;

procedure TfrmMediaStorageRecordsView.taRecordSourcesTOTAL_DURATIONGetText(
  Sender: TField; var Text: string; DisplayText: Boolean);
var
  aDT: TDateTime;
begin
  if (Sender.IsNull) or (Sender.Value=0) then
    Text:=''
  else begin
    aDT:=Sender.Value;
    Text:=TimeToStr(Sender.Value);

    if aDT>1 then
    begin
      Text:=Format('%d дн. %s',[Trunc(aDT),Text]);
    end;
  end;
end;

procedure TfrmMediaStorageRecordsView.acDeleteAbsentExecute(Sender: TObject);
var
  aBk: TBookmark;
  aTransport: IRecordObjectTransport;
begin
  inherited;

  TWaitCursor.SetUntilIdle;
  taRecords.DisableControls;
  try
    aBk:=taRecords.Bookmark;

    taRecords.First;
    while not taRecords.EOF do
    begin
      aTransport:=GetTransport(taRecordsDATA_TRANSPORT.Value);
      if not aTransport.FileExists then
      begin
        TArgumentValidation.NotNil(FRecordStorage.RecordWriteableAccess).DeleteRecord(taRecordsID.Value);
        Log(Format('Файл "%s" не существует. Удален',[aTransport.ConnectionString]));
      end;
      taRecords.Next;
    end;

    if taRecords.BookmarkValid(aBk) then
      taRecords.Bookmark:=aBk;
  finally
    taRecords.EnableControls;
  end;

  UpdateRecordsForCurrentRecordSource;
end;

procedure TfrmMediaStorageRecordsView.acDeleteExecute(Sender: TObject);
begin
  TArgumentValidation.NotNil(FRecordStorage.RecordWriteableAccess).DeleteRecord(taRecordsID.Value);
  taRecords.Delete;
end;

procedure TfrmMediaStorageRecordsView.acDeleteUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=lvRecords.IsCurrentRecord;
end;

procedure TfrmMediaStorageRecordsView.UpdateRecordsForCurrentRecordSource;
var
  aRecords: TrsRecordObjectInfoArray;
  i: integer;
  aRecordSourceId: TrsRecordSourceId;

  aProgress: TRecordStorageReadDataProgress;
begin
  FreeAndNil(FRetrieveInfoThread);

  aRecords:=nil;
  if  IsSelectedRecordSource then
  begin
    TWaitCursor.SetUntilIdle;
    taRecords.DisableControls;
    try
      aProgress:=TRecordStorageReadDataProgress.Create(self,Format('Чтение записей с %s по %s',[DateToStr(edLoadFrom.DateTime),DateToStr(edLoadTo.DateTime)]));
      try
        taRecords.Open;
        taRecords.EmptyTable;
        aRecordSourceId:=SelectedRecordSource.Id;
        aRecords:=FRecordStorage.GetRecords(
           aRecordSourceId,
           Trunc(edLoadFrom.DateTime),
           Trunc(edLoadTo.DateTime)+1-1/SecsPerDay,
           aProgress.OnRecordStorageReadProgress);

        aProgress.FForm.Text:=Format('Построение списка (%d записей)',[Length(aRecords)]);
        aProgress.FForm.SetProgressMode(Length(aRecords),0);
        aProgress.FForm.Repaint;
        for i:=0 to High(aRecords) do
        begin
          taRecords.Append;
          taRecordsID.Value:=aRecords[i].Id;
          taRecordsNAME.Value:=ExtractFileName(aRecords[i].Transport.ConnectionString);
          taRecordsPATH.Value:=ExtractFileDir(aRecords[i].Transport.ConnectionString);
          taRecordsOWNER.Value:=aRecords[i].Owner;

          if aRecords[i].StartDateTime<>0 then
            taRecordsBEGIN.Value:=aRecords[i].StartDateTime;

          if aRecords[i].EndDateTime<>0 then
            taRecordsEND.Value:=aRecords[i].EndDateTime;


          taRecordsTRANSPORT.Value:=aRecords[i].Transport.Name;
          taRecordsDATA_TRANSPORT.Value:=aRecords[i].Transport;
          taRecordsACCESSIBLE.Value:=2;
          taRecords.Post;

          aProgress.FForm.SetProgressMode(Length(aRecords),i+1);
          //aProgress.FForm.Repaint;
        end;

        aProgress.FForm.Text:=Format('Сортировка списка (%d записей)',[Length(aRecords)]);
        aProgress.FForm.Repaint;
        taRecords.SortOnFields(taRecordsBEGIN.FieldName);
        taRecords.First;
      finally
        aProgress.Free;
      end;
    finally
      taRecords.EnableControls;
    end;
    pcData.ActivePage:=tsData;
  end;

  FRetrieveInfoThread:=TRetrieveFileInfoThread.Create(self,aRecords);
end;

procedure TfrmMediaStorageRecordsView.UpdateRecordSources;
var
  aRSs: TrsRecordSourceInfoArray;
  i: integer;
  aBk: TBookmark;
begin
  aRSs:=nil;
  aBk:=taRecordSources.Bookmark;
  taRecordSources.DisableControls;
  try
    taRecordSources.Open;
    taRecordSources.EmptyTable;

    aRSs:=FRecordStorage.GetAllRecordSources;
    for i:=0 to High(aRSs) do
    begin
      taRecordSources.Append;
      taRecordSourcesID.Value:=aRSs[i].Id.Name;
      taRecordSourcesNAME.Value:=aRSs[i].Name;
      taRecordSources.Post;
    end;

    taRecordSources.First;
    if taRecordSources.BookmarkValid(aBk) then
      taRecordSources.Bookmark:=aBk;
  finally
    taRecordSources.EnableControls;
  end;

  lvRecordSourcesChangeRecord(nil); //Автоматом не срабатывает, потому что DisableControls
end;

procedure TfrmMediaStorageRecordsView.WmSetFileExistsStatus(var Message: TMessage);
var
  aBk: TBookmark;
begin
  taRecords.DisableControls;
  try
    aBk:=taRecords.Bookmark;
    if taRecords.Locate(taRecordsID.FieldName,string(PChar(Message.WParam)),[loCaseInsensitive]) then
    begin
      taRecords.Edit;
      try
        taRecordsACCESSIBLE.Value:=Message.LParam;
      finally
        taRecords.Post;
      end;

      if taRecords.BookmarkValid(aBk) then
        taRecords.Bookmark:=aBk;
    end;
  finally
    taRecords.EnableControls;
  end;
end;

procedure TfrmMediaStorageRecordsView.lvRecordsDeletion(Sender: TObject;
  Item: TListItem);
begin
  inherited;
  TObject(Item.Data).Free;
  Item.Data:=nil;
end;

procedure TfrmMediaStorageRecordsView.acDumpUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=lvRecords.IsCurrentRecord;
end;

procedure TfrmMediaStorageRecordsView.acRecordSourceDeleteExecute(Sender: TObject);
var
  aStart,aEnd:TDateTime;
  aDialog: TfmChooseItems;
  aSourceNames: TArray<variant>;
  aSourceIds: TArray<variant>;
  aSourceId: TrsRecordSourceId;
  i: integer;
begin
  if not MsgBox.Confirm(Handle,'Вы уверены, что хотите удалить выбранный источник?') then
    exit;

  TWaitCursor.SetUntilIdle;
  FRecordStorage.GetRecordSourceDateRanges(SelectedRecordSource.Id,aStart,aEnd);
  if (aStart<>0) then
  begin
    if RecordSourceCount>1 then
    begin
      case MsgBox.ConfirmYesNoCancelFmt(Handle,'Удаляемый источник имеет записи. Вы хотите перенести эти записи на другой источник?',[]) of
      ID_YES:
        begin
          aSourceNames:=taRecordSources.GetAllValues(taRecordSourcesNAME);
          aSourceIds:=taRecordSources.GetAllValues(taRecordSourcesID);

          aDialog:=TfmChooseItems.Create(Application);
          try
            aDialog.RadioMode:=true;
            aDialog.Init('Выберите новый источник для записей','Удаляемый источник содержит записи. Укажите другой источник, куда они будут перенесены');
            for i := 0 to High(aSourceNames) do
            begin
              if aSourceIds[i]<>SelectedRecordSource.Id.Name then
                aDialog.AddItem(aSourceNames[i],false,pointer(i));
            end;

            if aDialog.ShowModal=mrOk then
            begin
              i:=aDialog.FindFirstCheckedItem;
              Assert(i<>-1);

              i:=integer(aDialog.ItemData[i]);
              aSourceId.Init(aSourceIds[i]);
              TArgumentValidation.NotNil(FRecordStorage.RecordWriteableAccess).MoveRecords(SelectedRecordSource.Id,aSourceId);
            end
            else begin
              exit; //!!!
            end;
          finally
            aDialog.Free;
          end;
        end;
      ID_NO: ;
      ID_CANCEL:
        exit;
      end;
    end
    else begin
      if not MsgBox.Confirm(Handle,'Удаляемый источник имеет записи, которые будут также удалены. Вы уверены, что хотите удалить источник?') then
        exit;
    end;
  end;

  TArgumentValidation.NotNil(FRecordStorage.RecordSourceWriteableAccess).DeleteRecordSource(SelectedRecordSource.Id);
  acRecordSourceRefresh.Execute;
end;

procedure TfrmMediaStorageRecordsView.acRecordSourceDeleteUpdate(
  Sender: TObject);
begin
  TAction(Sender).Enabled:=IsSelectedRecordSource;
end;

procedure TfrmMediaStorageRecordsView.acRecordSourceGetStatisticsExecute(Sender: TObject);
var
  aMin,aMax: TDateTime;
  i,j: integer;
  aIds: TArray<variant>;
  aId: TrsRecordSourceId;
  aRecords: TrsRecordObjectInfoArray;
  aDuration: TDateTime;
  aBk: TBookmark;
begin
  TWaitCursor.SetUntilIdle;

  aBk:=taRecordSources.Bookmark;
  taRecordSources.DisableControls;
  try
    taRecordSourcesMINDATE.Visible:=true;
    taRecordSourcesMAXDATE.Visible:=true;
    taRecordSourcesRECORD_COUNT.Visible:=true;
    taRecordSourcesTOTAL_DURATION.Visible:=true;

    aIds:=taRecordSources.GetAllValues(taRecordSourcesID);

    for i := 0 to High(aIds) do
    begin
      aId.Init(aIds[i]);
      FRecordStorage.GetRecordSourceDateRanges(aId, aMin,aMax);
      aRecords:=FRecordStorage.GetRecords(aId);
      taRecordSources.Locate(taRecordSourcesID.FieldName,aIds[i],[loCaseInsensitive]);
      taRecordSources.Edit;
      if aMin=0 then
        taRecordSourcesMINDATE.Clear
      else
        taRecordSourcesMINDATE.Value:=aMin;

      if aMax=0 then
        taRecordSourcesMAXDATE.Clear
      else
        taRecordSourcesMAXDATE.Value:=aMax;

      taRecordSourcesRECORD_COUNT.Value:=Length(aRecords);

      aDuration:=0;
      for j := 0 to High(aRecords) do
        if (aRecords[j].StartDateTime<>0) and (aRecords[j].EndDateTime<>0) then
          aDuration:=aDuration+(aRecords[j].EndDateTime-aRecords[j].StartDateTime);

      taRecordSourcesTOTAL_DURATION.Value:=aDuration;
      taRecordSources.Post;
    end;

    if taRecordSources.BookmarkValid(aBk) then
      taRecordSources.Bookmark:=aBk;
  finally
    taRecordSources.EnableControls;
  end;

  if acRecordSourceGetStatistics.Tag=0 then
  begin
    if paRecordSources.Width<500 then
      paRecordSources.Width:=500;
    lvRecordSources.ColumnSizes.FitColumns();
  end;

  acRecordSourceGetStatistics.Tag:=1;
end;

procedure TfrmMediaStorageRecordsView.Log(const aMessage: string);
begin
  mmLog.Lines.Add(aMessage);
  SendMessage(mmLog.Handle,EM_LINESCROLL,0,mmLog.Lines.Count-1);
end;

function TfrmMediaStorageRecordsView.GetRecordSource(index: integer): TRecordSourceListItemData;
begin
  result.Id.Init(taRecordSources.DirectGetFieldData(index,taRecordSourcesID.FieldName));
  result.Name:=taRecordSources.DirectGetFieldData(index,taRecordSourcesNAME.FieldName);
end;

procedure TfrmMediaStorageRecordsView.acCheckFileExistsExecute(Sender: TObject);
var
  s: string;
  aTransport : IRecordObjectTransport;
  aStart,aStop: cardinal;
  res : boolean;
  i: integer;
begin
  inherited;
  s:='\\127.0.0.1\C$\FileStorageFiles\Files\IP Video\Files\';
  if InputQuery('Имя файла','Введите имя файла, включая имя компьютера',s) then
  begin
    Application.ProcessMessages;
    Log(Format('Выполняется проверка наличия файла "%s"...',[s]));

    TWaitCursor.SetUntilIdle;
    for i:=0 to 1 do
    begin
      aStart:=GetTickCount;
      try
        if i=0 then
          aTransport:=TRecordObjectTransport_NetworkPath.Create(s)
        else
          aTransport:=TRecordObjectTransport_FileServer.Create(s);

        res:=aTransport.FileExists;
        aStop:=GetTickCount;

        Log(Format('  Транспорт %s: Результат=%s, Время=%d мс',[aTransport.Name,fsiBaseUtils.BoolToStr(res),aStop-aStart]));
      except
        on E:Exception do
          Log(Format('  Транспорт %s: Ошибка= "%s", Время=%d мс',[aTransport.Name,E.Message,GetTickCount-aStart]));
      end;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TfrmMediaStorageRecordsView.acPropertiesExecute(Sender: TObject);
var
  aTransport: IRecordObjectTransport;
  aTransportReader : IRecordObjectReader;
  aReader:TStreamFramer;
  aStream: TStream;
  aFPS: string;
begin
  aTransport:=GetTransport(taRecordsDATA_TRANSPORT.Value);
  aTransportReader:=aTransport.GetReader;

  TWaitCursor.SetUntilIdle;
  aReader:= GetFramerClassFromFileName(aTransport.ConnectionString).Create;
  try
    aStream:=aTransportReader.GetStream;
    aReader.OpenStream(aStream);

    Log(Format('Размер:%d',[aStream.Size]));
    if aReader.VideoInfo.State=isOK then
    begin
      Log(Format('Разрешение:%dx%d',[aReader.VideoInfo.Width,aReader.VideoInfo.Height]));
      if aReader.RandomAccess<>nil then
      begin
        Log(Format('Длина видео, мсек:%d',[aReader.RandomAccess.StreamInfo.Length]));

        aFPS:='?';
        if aReader.RandomAccess.StreamInfo.Length>0 then
          aFPS:=FormatFloat('0.0',aReader.RandomAccess.StreamInfo.VideoFrameCount/(aReader.RandomAccess.StreamInfo.Length/1000));
        //Log(Format('Длина аудио, мсек:%d',[aReader.Statistics.AudioTotalLength]));
        Log(Format('FPS:%s',[aFPS]));
        Log(Format('Кадров: VI:%d',[aReader.RandomAccess.StreamInfo.VideoFrameCount]));
      end;
   end;
  finally
    aReader.Free;
  end;

  aTransportReader:=nil;
end;

procedure TfrmMediaStorageRecordsView.acRefreshExecute(Sender: TObject);
begin
  pcData.ActivePage:=tsWelcome;
end;

procedure TfrmMediaStorageRecordsView.acRecordSourceRefreshExecute(Sender: TObject);
begin
  UpdateRecordSources;
end;

procedure TfrmMediaStorageRecordsView.acCopyExecute(Sender: TObject);
var
  aDestStream : TFileStream;
  aCopyThread: TCopyFileThread;
  s: AnsiString;
  aFileSize,aProcessedBytes: int64;
  aProgress: TfmProgressWithBar;
  aStartTicks,aDeltaTicks: cardinal;
  aTransport: IRecordObjectTransport;
begin
  inherited;
  aTransport:=GetTransport(taRecordsDATA_TRANSPORT.Value);

  dlgSave.DefaultExt:=ExtractFileExt(aTransport.ConnectionString);
  dlgSave.FileName:=ExtractFileName(aTransport.ConnectionString);
  if dlgSave.Execute then
  begin
    TWaitCursor.SetUntilIdle;
    aDestStream:=TFileStream.Create(dlgSave.FileName,fmCreate);
    try
      aFileSize:=aTransport.FileSize;
      aProcessedBytes:=0;

      aProgress:=TfmProgressWithBar.Create(Application);
      try
        aProgress.CancelButtonEnabled:=false; //TODO
        aProgress.SetProgressMode(100,0);
        aProgress.Text:='Инициализация чтения...';
        aProgress.Show;
        aStartTicks:=GetTickCount;
        aCopyThread:=TCopyFileThread.Create(aTransport,aDestStream);
        while not aCopyThread.Finished do
        begin
          aProcessedBytes:=aCopyThread.ProcessedBytes;
          if aProcessedBytes>0 then //Иначе еще не кончилась инициализация
          begin
            aProgress.SetProgressMode(aProcessedBytes*100 div aFileSize);
            aDeltaTicks:=(GetTickCount-aStartTicks);
            aProgress.Text:=Format('Скачано: %.2f МБ из %.2f МБ. Время: %d сек. Скорость: %d КБ/сек',
            [
             aProcessedBytes/1024/1024,
             aFileSize/1024/1024,
             aDeltaTicks div 1000,
             aProcessedBytes div Max(aDeltaTicks,1)
             ]);
          end;

          Application.ProcessMessages;
        end;

        if aCopyThread.FatalException<>nil then
        begin
          if aCopyThread.FatalException is Exception then
            MsgBox.Error(Handle,aCopyThread.FatalException as Exception)
          else
            MsgBox.MessageFailure(Handle,'Во время копирования произошла ошибка');
        end
        else begin
          aProgress.Hide;
          aDeltaTicks:=(GetTickCount-aStartTicks);
          MsgBox.MessageInfo(Handle,
            Format('Копирование выполнено. Время: %d сек. Скорость: %d КБ/сек. Размер: %.2f МБ',
            [
              aDeltaTicks div 1000,
              aProcessedBytes div Max(aDeltaTicks,1),
              aFileSize/1024/1024]));
        end;
      finally
        aProgress.Free;
      end;
    finally
      aDestStream.Free;
    end;

    aDestStream:=TFileStream.Create(ChangeFileExt(dlgSave.FileName,'.info'),fmCreate);
    try

      s:=Format('Id=%s'#13#10+
         'ConnectionString=%s'#13#10+
         'StartDateTime=%s'#13#10+
         'EndDateTime=%s'#13#10,
         [taRecordsID.Value,
          aTransport.ConnectionString,
          DateTimeToStr(taRecordsBEGIN.Value),
          DateTimeToStr(taRecordsEND.Value)]);
      aDestStream.WriteBuffer(PAnsiChar(s)^,Length(s));
    finally
      aDestStream.Free;
    end;
  end;
end;

procedure TfrmMediaStorageRecordsView.acCopyFileNameToClipboardExecute(Sender: TObject);
var
  aTransport: IRecordObjectTransport;
begin
  aTransport:=GetTransport(taRecordsDATA_TRANSPORT.Value);

  Clipboard.Open;
  Clipboard.AsText:=aTransport.ConnectionString;
  Clipboard.Close;
end;

procedure TfrmMediaStorageRecordsView.acCopyFileNameToClipboardUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=lvRecords.IsCurrentRecord;
end;

procedure TfrmMediaStorageRecordsView.acPlayExecute(Sender: TObject);
var
  aRecords: TrsRecordObjectInfoArray;
begin
  SetLength(aRecords,1);
  aRecords[0].Id:=taRecordsID.Value;
  aRecords[0].StartDateTime:=taRecordsBEGIN.Value;
  aRecords[0].EndDateTime:=taRecordsEND.Value;
  aRecords[0].Transport:=GetTransport(taRecordsDATA_TRANSPORT.Value);
  TfmMediaStorageRecordPreview.Run(aRecords);
end;

end.

