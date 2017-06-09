unit MediaProcessing.VideoAnalytics.Net.EventDefinitions;

interface
  uses SysUtils, Classes, MediaProcessing.VideoAnalytics,MediaProcessing.VideoAnalytics.Definitions;

const
   VaepProtocolVerion = 110220; //'2011.02.20'
   VaepFrameBeginMarker = cardinal($46424D4B); //FBMK
   VaepFrameEndMarker = cardinal($46454D4B); //FEMK

   VaepMaxQueue = 200;
   VaepProtocolDefaultPort = 19227;

   VaepSuperUserName = 'su';
   VaepSuperUserPassword = '{BB968164-B35C-4321-96CB-A720924FAFCC}';

type
  TVaepDataObject = class
  public
    procedure LoadFromStream(aStream: TStream); virtual;
    procedure SaveToStream(aStream: TStream); virtual;

    procedure LoadFromBytes(aBytes: TBytes);
    procedure SaveToBytes(var aBytes: TBytes);
  end;

  //------ Login
  TVaepLoginParams = class (TVaepDataObject)
  private
    FClientName: string;
    FUserPasswordDigest: string;
    FUserName: string;
    FProtocolVersion: cardinal;
  public
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;

    constructor Create(aProtocolVersion: cardinal; const aUserName, aUserPasswordDigest: string; const aClientName: string); overload;
    constructor Create(aBytes : TBytes);overload;

    property ProtocolVersion: cardinal read FProtocolVersion;
    property UserName: string read FUserName;
    property UserPasswordDigest: string read FUserPasswordDigest;

    property ClientName: string read FClientName;
  end;

type
  TVaepLoginResultCode = (
    iceOK,
    iceWrongProtocolVersion,
    iceWrongUserNameOrPassword
  );

  function VaepLoginResultCodeToString(aCode: TVaepLoginResultCode): string;

type
  TVaepLoginResult = class (TVaepDataObject)
  private
    FServerID: TGUID;
    FCode: TVaepLoginResultCode;
  public
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;

    property Code: TVaepLoginResultCode read FCode;
    property ServerID: TGUID read FServerID;

    constructor Create(aResult: TVaepLoginResultCode; const aServerID: TGUID); overload;
    constructor Create(aBytes : TBytes);overload;
  end;

type
  TVaepNotifyType = (
    VaepEvent);

  //Базовый класс уведомления
  TVaepNotify = class (TVaepDataObject)
  public
    class function NotifyType: TVaepNotifyType; virtual; abstract;
    constructor CreateFromBytes(aBytes: TBytes); virtual;
  end;

  TVaepNotifyClass = class of TVaepNotify;

  //Уведомление "Было установлено клиентское подключение для получения медиа-потока"
  TVaepEventNotify = class (TVaepNotify)
  private
    FConnectionUrl: string;
    FProcessingResult: TVaProcessingResult;
  public
    class function NotifyType: TVaepNotifyType; override;
    function ToString():string; override;

    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;

    constructor Create(const aConnectionUrl: string; const aProcessingResult: TVaProcessingResult); overload;

    property ConnectionUrl: string read FConnectionUrl;
    property ProcessingResult: TVaProcessingResult read FProcessingResult;
  end;

  function GetNotifyClass(aNotifyType: TVaepNotifyType): TVaepNotifyClass;

implementation
  uses uBaseClasses;

const
  VaepLoginResultCodeNames : array [TVaepLoginResultCode] of string =
  (
   'OK',
   'Неподдерживаемая версия протокола',
   'Неверное имя пользователя или пароль'
  );

function VaepLoginResultCodeToString(aCode: TVaepLoginResultCode): string;
begin
  if integer(aCode)>=Length(VaepLoginResultCodeNames) then
    result:='неизвестная ошибка';

  result:=VaepLoginResultCodeNames[aCode];
end;

function GetNotifyClass(aNotifyType: TVaepNotifyType): TVaepNotifyClass;
begin
  case aNotifyType of
    VaepEvent: result:=TVaepEventNotify;
    else
      raise Exception.Create('Неизвестный тип нотификации');
  end;
end;

{ TVaepDataObject }

procedure TVaepDataObject.LoadFromBytes(aBytes: TBytes);
var
  aStream: TBytesStream;
begin
  aStream:=TBytesStream.Create(aBytes);
  try
    aStream.Position:=0;
    LoadFromStream(aStream);
  finally
    aStream.Free;
  end;
end;

procedure TVaepDataObject.SaveToBytes(var aBytes: TBytes);
var
  aStream: TBytesStream;
begin
  aStream:=TBytesStream.Create;
  try
    SaveToStream(aStream);
    aBytes:=Copy(aStream.Bytes,0,aStream.Size);
  finally
    aStream.Free;
  end;
end;

procedure TVaepDataObject.LoadFromStream(aStream: TStream);
begin
end;

procedure TVaepDataObject.SaveToStream(aStream: TStream);
begin

end;

{ TVaepLoginParams }

constructor TVaepLoginParams.Create(aProtocolVersion: cardinal; const aUserName, aUserPasswordDigest:string; const aClientName: string);
begin
  FProtocolVersion:=aProtocolVersion;
  FUserName:=aUserName;
  FUserPasswordDigest:=aUserPasswordDigest;
  FClientName:=aClientName;
end;

constructor TVaepLoginParams.Create(aBytes : TBytes);
begin
  inherited Create;
  LoadFromBytes(aBytes);
end;

procedure TVaepLoginParams.LoadFromStream(aStream: TStream);
begin
  inherited;
  aStream.ReadCardinal(FProtocolVersion);
  aStream.ReadString(FUserName);
  aStream.ReadString(FUserPasswordDigest);
  aStream.ReadString(FClientName);
end;

procedure TVaepLoginParams.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.WriteCardinal(FProtocolVersion);
  aStream.WriteString(FUserName);
  aStream.WriteString(FUserPasswordDigest);
  aStream.WriteString(FClientName);
end;

{ TVaepLoginResult }

constructor TVaepLoginResult.Create(aBytes: TBytes);
begin
  LoadFromBytes(aBytes);
end;

procedure TVaepLoginResult.LoadFromStream(aStream: TStream);
var
  i: integer;
begin
  inherited;
  aStream.ReadInteger(i);
  FCode:=TVaepLoginResultCode(i);
  aStream.ReadBuffer(FServerID,sizeof(FServerID));
end;

procedure TVaepLoginResult.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.WriteInteger(integer(FCode));
  aStream.WriteBuffer(FServerID,sizeof(FServerID));
end;

constructor TVaepLoginResult.Create(aResult: TVaepLoginResultCode; const aServerID: TGUID);
begin
  inherited Create;
  FCode:=aResult;
  FServerID:=aServerID;
end;

{ TVaepNotify }

constructor TVaepNotify.CreateFromBytes(aBytes: TBytes);
begin
  LoadFromBytes(aBytes);
end;

{ TVaepEventNotify }

constructor TVaepEventNotify.Create(const aConnectionUrl: string; const aProcessingResult: TVaProcessingResult);
begin
  FConnectionUrl:=aConnectionUrl;
  FProcessingResult:=aProcessingResult;
end;

procedure TVaepEventNotify.LoadFromStream(aStream: TStream);
var
  i: Integer;
  aObject: ^TVaObject;
  aEvent: ^TVaEvent;
  j: Integer;
begin
  inherited;
  aStream.ReadString(FConnectionUrl);

  aStream.ReadInteger(i);
  SetLength(FProcessingResult.Objects,i);

  for i := 0 to High(FProcessingResult.Objects) do
  begin
    aObject:=@FProcessingResult.Objects[i];
    aStream.ReadInteger(aObject.id);
    aStream.ReadInteger(aObject.type_);
    aStream.ReadBuffer(aObject.position,sizeof(aObject.position));
    aStream.ReadBuffer(aObject.rect,sizeof(aObject.rect));
    aStream.ReadBuffer(aObject.start_position,sizeof(aObject.start_position));

    aStream.ReadInteger(j);
    SetLength(aObject.trajectory,j);

    for j := 0 to High(aObject.trajectory) do
      aStream.ReadBuffer(aObject.trajectory[j],sizeof(aObject.trajectory[j]));

    aStream.ReadInteger(aObject.mask_index);
    aStream.ReadBuffer(aObject.mask_rect,sizeof(aObject.mask_rect));

    aStream.ReadInteger(j);
    SetLength(aObject.all_events_deprecated,j);
    for j := 0 to High(aObject.all_events_deprecated) do
      aStream.ReadInteger(aObject.all_events_deprecated[j]);
  end;

  aStream.ReadInteger(i);
  SetLength(FProcessingResult.Events,i);
  for i := 0 to High(FProcessingResult.Events) do
  begin
    aEvent:=@FProcessingResult.Events[i];
    aStream.ReadInteger(j);
    aEvent.type_:=TVaEventType(j);
    aStream.ReadInteger(aEvent.level);
    aStream.ReadInteger(aEvent.object_id);
    aStream.ReadInteger(aEvent.rule_id);
    aStream.ReadString(aEvent.description);
  end;

end;

class function TVaepEventNotify.NotifyType: TVaepNotifyType;
begin
  result:=VaepEvent;
end;

procedure TVaepEventNotify.SaveToStream(aStream: TStream);
var
  i: Integer;
  aObject: ^TVaObject;
  aEvent: ^TVaEvent;
  j: Integer;
begin
  inherited;
  aStream.WriteString(FConnectionUrl);

  aStream.WriteInteger(Length(FProcessingResult.Objects));
  for i := 0 to High(FProcessingResult.Objects) do
  begin
    aObject:=@FProcessingResult.Objects[i];
    aStream.WriteInteger(aObject.id);
    aStream.WriteInteger(aObject.type_);
    aStream.WriteBuffer(aObject.position,sizeof(aObject.position));
    aStream.WriteBuffer(aObject.rect,sizeof(aObject.rect));
    aStream.WriteBuffer(aObject.start_position,sizeof(aObject.start_position));

    aStream.WriteInteger(Length(aObject.trajectory));
    for j := 0 to High(aObject.trajectory) do
      aStream.WriteBuffer(aObject.trajectory[j],sizeof(aObject.trajectory[j]));

    aStream.WriteInteger(aObject.mask_index);
    aStream.WriteBuffer(aObject.mask_rect,sizeof(aObject.mask_rect));

    aStream.WriteInteger(Length(aObject.all_events_deprecated));
    for j := 0 to High(aObject.all_events_deprecated) do
      aStream.WriteInteger(aObject.all_events_deprecated[j]);
  end;

  aStream.WriteInteger(Length(FProcessingResult.Events));
  for i := 0 to High(FProcessingResult.Events) do
  begin
    aEvent:=@FProcessingResult.Events[i];
    aStream.WriteInteger(integer(aEvent.type_));
    aStream.WriteInteger(aEvent.level);
    aStream.WriteInteger(aEvent.object_id);
    aStream.WriteInteger(aEvent.rule_id);
    aStream.WriteString(aEvent.description);
  end;
end;

function TVaepEventNotify.ToString: string;
begin
  result:=Format('ConnectionUrl: %s',[FConnectionUrl]);
end;

initialization


end.
