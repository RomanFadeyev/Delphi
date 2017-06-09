unit MediaProcessing.ProcessorListFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,MediaProcessing.Definitions, ComCtrls, ExtendControls, Buttons,
  StdCtrls, ActnList, ToolWin, ImgList, ExtCtrls, Vcl.Menus;

type
  TMediaServerProcessorListItemData = class;
  TfrmMediaProcessorList = class(TFrame)
    alActions: TActionList;
    acAddMediaProcessor: TAction;
    acMediaProcessorProps: TAction;
    acDeleteMediaProcessor: TAction;
    ilActions: TImageList;
    Panel1: TPanel;
    lvMediaServerProcessors: TExtendListView;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    paOriginalStreamType: TPanel;
    buGetStreamType: TExtendSpeedBtn;
    laMediaServerStreamType: TLabel;
    cbMediaServerStreamType: TExtendComboBox;
    ExtendButton1: TExtendButton;
    pmPopup: TPopupMenu;
    acCopy: TAction;
    acPaste: TAction;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure acAddMediaProcessorExecute(Sender: TObject);
    procedure acAddMediaProcessorUpdate(Sender: TObject);
    procedure acMediaProcessorPropsExecute(Sender: TObject);
    procedure acMediaProcessorPropsUpdate(Sender: TObject);
    procedure acDeleteMediaProcessorExecute(Sender: TObject);
    procedure acDeleteMediaProcessorUpdate(Sender: TObject);
    procedure lvMediaServerProcessorsDblClick(Sender: TObject);
    procedure lvMediaServerProcessorsDeletion(Sender: TObject; Item: TListItem);
    procedure cbMediaServerStreamTypeChange(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCopyUpdate(Sender: TObject);
    procedure acPasteUpdate(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
  private
    FOnDataChanged: TNotifyEvent;
    FOutputStreamTypeFilter: TStreamType;
    FAcceptableMediaTypes: TMediaTypeSet;

    class function GetMediaProcessorListItemData(aListItem: TListItem): TMediaServerProcessorListItemData;
    function AddMediaProcessor(const aIID: TGUID): TMediaServerProcessorListItemData;
    function GetOriginalStreamType: TStreamType;
    procedure SetOriginalStreamType(const Value: TStreamType);
    function GetOutputStreamType: TStreamType;
    function GetChangingOriginalStreamTypeAvailable: boolean;
    procedure SetChangingOriginalStreamTypeAvailable(const Value: boolean);
    function GetMediaProcessor(index: integer): TMediaServerProcessorListItemData;
    function GetOriginalStreamTypeList(index: integer): TStreamType;
  protected
    function GetEnabled: boolean; override;
    procedure SetEnabled(Value: boolean); override;
    procedure DoDataChanged;
  public
    constructor Create(aOwner: TComponent);                      override;

    procedure LoadData(aData: TBytes);
    function  SaveData: TBytes;

    property MediaProcessors[index: integer]: TMediaServerProcessorListItemData read GetMediaProcessor;
    function MediaProcessorCount:integer;
    property OriginalStreamType: TStreamType read GetOriginalStreamType write SetOriginalStreamType;
    property OutputStreamType: TStreamType read GetOutputStreamType;

    //Ограничение на выходные типы фильтров
    property OutputStreamTypeFilter: TStreamType read FOutputStreamTypeFilter write FOutputStreamTypeFilter;

    property ChangingOriginalStreamTypeAvailable: boolean read GetChangingOriginalStreamTypeAvailable write SetChangingOriginalStreamTypeAvailable;

    function  OriginalStreamTypeListCount: integer;
    property  OriginalStreamTypeList[index: integer]:TStreamType read GetOriginalStreamTypeList;

    procedure FillOriginalStreamTypeList(aMediaType: TMediaType);

    procedure RemoveFromOriginalStreamTypeList(aStreamType: TStreamType);

    property  OnDataChanged: TNotifyEvent read  FOnDataChanged write FOnDataChanged;
    property  AcceptableMediaTypes: TMediaTypeSet read FAcceptableMediaTypes write FAcceptableMediaTypes;
  end;

  TMediaServerProcessorListItemData = class
    MediaProcessor: IMediaProcessor;
  end;

implementation
  uses VCLUtils, MediaProcessing.Global, MediaProcessing.ChooseProcessorDialog, ClipBrd,IdGlobal;
{$R *.dfm}

var
  CF_MEDIA_PROCESSORS: array [TMediaType] of word;

type
  TFriendlyClipboard = class (TClipboard);

procedure TfrmMediaProcessorList.acAddMediaProcessorExecute(Sender: TObject);
var
  aStreamType: TStreamType;
  aIID : TGUID;
begin
  inherited;
  aStreamType:=Low(TStreamType);
  if cbMediaServerStreamType.ItemIndex<>-1 then
    aStreamType:=TStreamType(cbMediaServerStreamType.CurrentItemData);

  if MediaProcessorCount>0 then
    aStreamType:= self.MediaProcessors[MediaProcessorCount-1].MediaProcessor.Info.OutputStreamType;

  if TfmChooseMediaProcessorDialog.Run(aStreamType,FOutputStreamTypeFilter,aIID) then
  begin
    AddMediaProcessor(aIID);
    with lvMediaServerProcessors.Items[MediaProcessorCount-1] do
    begin
      Selected:=true;
      Focused:=true;
    end;
    DoDataChanged;
  end;
end;

procedure TfrmMediaProcessorList.acAddMediaProcessorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=self.Enabled and (MediaProcessorCount>0) or (cbMediaServerStreamType.ItemIndex<>-1);
  buGetStreamType.Visible:=Assigned(buGetStreamType.OnClick);
end;

procedure TfrmMediaProcessorList.acCopyExecute(Sender: TObject);
var
  aData: TBytes;
  aMediaType: TMediaType;
begin
  aData:=SaveData;

  for aMediaType := Low(TMediaType) to High(TMediaType) do
    if aMediaType in FAcceptableMediaTypes then
      TFriendlyClipboard(Clipboard).SetBuffer(CF_MEDIA_PROCESSORS[aMediaType],aData[0],Length(aData));
end;

procedure TfrmMediaProcessorList.acCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=MediaProcessorCount>0;
end;

procedure TfrmMediaProcessorList.acMediaProcessorPropsExecute(Sender: TObject);
begin
  GetMediaProcessorListItemData(lvMediaServerProcessors.Selected).MediaProcessor.ShowCustomProperiesDialog;
  DoDataChanged;
end;

procedure TfrmMediaProcessorList.acMediaProcessorPropsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=self.Enabled and (lvMediaServerProcessors.Selected<>nil) and (GetMediaProcessorListItemData(lvMediaServerProcessors.Selected).MediaProcessor.HasCustomProperties);
end;

procedure TfrmMediaProcessorList.acPasteExecute(Sender: TObject);
var
  Data: THandle;
  aBytes: TBytes;
  DataPtr: Pointer;
  aMediaType: TMediaType;
begin
  Clipboard.Open;
  try
    for aMediaType := Low(TMediaType) to High(TMediaType) do
      if aMediaType in FAcceptableMediaTypes then
      begin
        Data := GetClipboardData(CF_MEDIA_PROCESSORS[aMediaType]);
        if Data = 0 then
          Exit;
        DataPtr := GlobalLock(Data);
        if DataPtr = nil then
          Exit;

        try
          SetLength(aBytes,GlobalSize(Data));
          Move(DataPtr^, aBytes[0], Length(aBytes));
        finally
          GlobalUnlock(Data);
        end;

        break;
      end;
  finally
    Clipboard.Close;
  end;

  LoadData(aBytes);
  DoDataChanged;
end;

procedure TfrmMediaProcessorList.acPasteUpdate(Sender: TObject);
var
  aMediaType: TMediaType;
  b: boolean;
begin
  b:=false;
  for aMediaType := Low(TMediaType) to High(TMediaType) do
    if aMediaType in FAcceptableMediaTypes then
      b:=b or Clipboard.HasFormat(CF_MEDIA_PROCESSORS[aMediaType]);

  TAction(Sender).Enabled:=b;
end;

procedure TfrmMediaProcessorList.acDeleteMediaProcessorExecute(Sender: TObject);
var
  i: integer;
begin
  inherited;
  i:=lvMediaServerProcessors.Selected.Index;
  lvMediaServerProcessors.Selected.Free;

  if MediaProcessorCount>i then
  begin
    lvMediaServerProcessors.Items[i].Selected:=true;
    lvMediaServerProcessors.Items[i].Focused:=true;
  end
  else begin
    if MediaProcessorCount>0 then
    begin
      lvMediaServerProcessors.Items[MediaProcessorCount-1].Selected:=true;
      lvMediaServerProcessors.Items[MediaProcessorCount-1].Focused:=true;
    end
  end;
  DoDataChanged;
end;

procedure TfrmMediaProcessorList.acDeleteMediaProcessorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=self.Enabled and (lvMediaServerProcessors.Selected<>nil) and (lvMediaServerProcessors.Selected.Index=MediaProcessorCount-1);
end;

function TfrmMediaProcessorList.GetChangingOriginalStreamTypeAvailable: boolean;
begin
  result:=cbMediaServerStreamType.Enabled;
end;

function TfrmMediaProcessorList.GetEnabled: boolean;
begin
  result:=inherited GetEnabled;
end;

function TfrmMediaProcessorList.GetMediaProcessor(index: integer): TMediaServerProcessorListItemData;
begin
  result:=GetMediaProcessorListItemData(lvMediaServerProcessors.Items[index]);
end;

class function TfrmMediaProcessorList.GetMediaProcessorListItemData(aListItem: TListItem): TMediaServerProcessorListItemData;
begin
  Assert(aListItem<>nil);
  Assert(aListItem.Data<>nil);
  result:=aListItem.Data;
end;

function TfrmMediaProcessorList.GetOriginalStreamType: TStreamType;
begin
  if cbMediaServerStreamType.ItemIndex=-1 then
    result:=0
  else
    result:=TStreamType(cbMediaServerStreamType.CurrentItemData)

//raise Exception.Create('Не указан исходный тип потока');
end;

function TfrmMediaProcessorList.GetOriginalStreamTypeList(
  index: integer): TStreamType;
begin
  result:=TStreamType(cbMediaServerStreamType.Items.Data[index]);
end;

function TfrmMediaProcessorList.GetOutputStreamType: TStreamType;
begin
  result:=OriginalStreamType;
  if MediaProcessorCount>0 then
    result:=MediaProcessors[MediaProcessorCount-1].MediaProcessor.Info.OutputStreamType;
end;

procedure TfrmMediaProcessorList.LoadData(aData: TBytes);
var
  i: Integer;
  aByteStream: TBytesStream;
  aIID : TGUID;
begin
  lvMediaServerProcessors.Items.Clear;
  if Length(aData)>0 then
  begin
    aByteStream:=TBytesStream.Create(aData);
    try
      //Processors
      aByteStream.ReadBuffer(i,sizeof(i));
      Assert(i<1000);
      for i := 0 to i-1 do
      begin
        aByteStream.Read(aIID,sizeof(aIID));
        AddMediaProcessor(aIID).MediaProcessor.LoadCustomProperties(aByteStream);
      end;
    finally
      aByteStream.Free;
    end;

    if MediaProcessorCount>0 then
    begin
      lvMediaServerProcessors.Items[0].Selected:=true;
      lvMediaServerProcessors.Items[0].Focused:=true;
    end;
  end;
end;

procedure TfrmMediaProcessorList.lvMediaServerProcessorsDblClick(
  Sender: TObject);
begin
  acMediaProcessorProps.Execute;
end;

procedure TfrmMediaProcessorList.lvMediaServerProcessorsDeletion(
  Sender: TObject; Item: TListItem);
begin
  TObject(Item.Data).Free;
  Item.Data:=nil;
end;

function TfrmMediaProcessorList.MediaProcessorCount: integer;
begin
  result:=lvMediaServerProcessors.Items.Count;
end;

function TfrmMediaProcessorList.OriginalStreamTypeListCount: integer;
begin
  result:=cbMediaServerStreamType.Items.Count;
end;

procedure TfrmMediaProcessorList.RemoveFromOriginalStreamTypeList(aStreamType: TStreamType);
var
  i: integer;
begin
  inherited;
  for i := 0 to OriginalStreamTypeListCount-1 do
    if OriginalStreamTypeList[i]=aStreamType then
    begin
      cbMediaServerStreamType.Items.Delete(i);

      if cbMediaServerStreamType.ItemIndex=-1 then
        cbMediaServerStreamType.ItemIndex:=0;

      break;
    end;
end;

function TfrmMediaProcessorList.SaveData: TBytes;
var
  i: Integer;
  aListItemData: TMediaServerProcessorListItemData;
  aByteStream: TBytesStream;
  aIID : TGUID;
begin
  aByteStream:=TBytesStream.Create(nil);
  try
    i:=MediaProcessorCount;
    aByteStream.Write(i,sizeof(i));
    for i := 0 to MediaProcessorCount-1 do
    begin
      aListItemData:=Self.MediaProcessors[i];
      aIID:=aListItemData.MediaProcessor.Info.TypeID;
      aByteStream.Write(aIID,sizeof(aIID));
      aListItemData.MediaProcessor.SaveCustomProperties(aByteStream);
    end;
    result:=Copy(aByteStream.Bytes,0,aByteStream.Size);
  finally
    aByteStream.Free;
  end;
end;

procedure TfrmMediaProcessorList.SetChangingOriginalStreamTypeAvailable(
  const Value: boolean);
begin
  cbMediaServerStreamType.Enabled:=Value;
  laMediaServerStreamType.Enabled:=Value;
  buGetStreamType.Enabled:=Value;
end;

procedure TfrmMediaProcessorList.SetEnabled(Value: boolean);
var
  i: integer;
begin
  for i := 0 to ComponentCount-1 do
    if Components[i] is TControl then
      TControl(Components[i]).Enabled:=Value;

  inherited SetEnabled(Value);
end;

procedure TfrmMediaProcessorList.SetOriginalStreamType(const Value: TStreamType);
begin
  cbMediaServerStreamType.ItemIndex:=cbMediaServerStreamType.Items.IndexOfData(integer(Value));
end;

function TfrmMediaProcessorList.AddMediaProcessor(
  const aIID: TGUID): TMediaServerProcessorListItemData;
var
  aData : TMediaServerProcessorListItemData;
  aInfo : TMediaProcessorInfo;

  aItem : TListItem;
  s: string;
begin
  aInfo:=MediaProceccorFactory.GetMediaProcessorInfoByTypeID(aIID);
  aData:=TMediaServerProcessorListItemData.Create;
  aData.MediaProcessor:=MediaProceccorFactory.CreateMediaProcessor(aIID,false);

  aItem:=lvMediaServerProcessors.Items.Add;
  aItem.Caption:=aInfo.Name;

  if Length(aInfo.InputStreamTypes)=1 then
    s:=GetStreamTypeName(aInfo.InputStreamTypes[0])
  else
    s:=StreamTypesToFourccString(aInfo.InputStreamTypes);

  aItem.SubItems.Add(s);
  aItem.SubItems.Add(GetStreamTypeName(aInfo.OutputStreamType));
  aItem.SubItems.Add(aInfo.Description);
  aItem.Data:=aData;

  result:=aData;
end;

procedure TfrmMediaProcessorList.cbMediaServerStreamTypeChange(Sender: TObject);
var
  aFirstMP: TMediaServerProcessorListItemData;
begin
  if MediaProcessorCount>0 then
  begin
    aFirstMP:=self.MediaProcessors[0];
    if not aFirstMP.MediaProcessor.Info.CanAcceptInputStreamType(GetOriginalStreamType) and (Length(aFirstMP.MediaProcessor.Info.InputStreamTypes)>0) then
    begin
      if MsgBox.ConfirmWarningFmt(Handle,
        'Выбранный тип потока несовместим с первым в очереди медиа-процессором. '+
        'Чтобы использовать новый тип потока, вы должны очистить список медиа-процессоров. Выполнить очистку? '+
        'Нажмите "Нет", чтобы оставить список медиа-процессоров и вернуть тип потока в исходный вариант',
        []) then
       lvMediaServerProcessors.Items.Clear
      else
        SetOriginalStreamType(aFirstMP.MediaProcessor.Info.InputStreamTypes[0]);

    end;
  end;

end;

constructor TfrmMediaProcessorList.Create(aOwner: TComponent);
begin
  inherited;
  AcceptableMediaTypes:=AllMediaTypes;
end;

procedure TfrmMediaProcessorList.DoDataChanged;
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(self);
end;

procedure TfrmMediaProcessorList.FillOriginalStreamTypeList(aMediaType: TMediaType);
var
  st: integer;
begin
  inherited;
  cbMediaServerStreamType.Items.Clear;

  for st := 0 to High(stKnownTypes[aMediaType]) do
    cbMediaServerStreamType.Items.AddData(GetStreamTypeName(stKnownTypes[aMediaType][st]),stKnownTypes[aMediaType][st]);

  if cbMediaServerStreamType.Items.Count>0 then
    cbMediaServerStreamType.ItemIndex:=0;

  cbMediaServerStreamType.Sorted:=true;
end;

procedure InitClipboardFormats;
var
  aMediaType: TMediaType;
  s: string;
begin
  for aMediaType := Low(TMediaType) to High(TMediaType) do
  begin
    s:='Media Processors. '+MediaTypeNames[aMediaType]; { Do not localize }
    CF_MEDIA_PROCESSORS[aMediaType]:=RegisterClipboardFormat(PChar(s));
  end;
end;

initialization
  InitClipboardFormats;

end.

