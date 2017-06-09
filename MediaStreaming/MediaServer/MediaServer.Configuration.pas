{***********************************<_INFO>************************************}
{  <Проект>      Видеосервер                                                   }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Конфигурация видеосервера                                     }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        10.12.2008                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaServer.Configuration;

interface

uses Windows,SysUtils, Classes, IniFiles, ActiveX, ComObj, Contnrs,ExtCtrls, Generics.Collections,
     HHCommon, MediaProcessing.Definitions;

type
  TBufferOverflowAction  = (boaDoNotWriting,boaWriteIFramesOnly);
  TPhysicalDeviceType = (dtInPointsMultiplexer, dtIpCameraBeward,dtFile,dtRecordStorageSource,dtCompressionCardHikvision,dtDesktop,dtWebCamera,dtMediaServer,dtRTSP, dtInPointsRetransmitter);

const
  PhysicalDeviceTypeNamesLong: array [TPhysicalDeviceType] of string =
  ('Мультиплексор','Ip камера "Beward"','Файл','Файловое хранилище','Компрессионная карта "Hikvision"','Рабочий стол','Web-камера','Медиа-сервер','RTSP','Ретранслятор');

  PhysicalDeviceTypeNamesShort: array [TPhysicalDeviceType] of string =
  ('Мультиплексор','Ip камера "Beward"','Файл','ФХ','Компр. карта "Hikvision"','Раб. стол','Web-камера','Медиа-сервер','RTSP','Ретранслятор');

  PhysicalDeviceTypeNamesSignature: array [TPhysicalDeviceType] of string =
  ('Multiplexer','IpCamBeward','File','FileStorage','CompCardHikvision','Desktop','WebCam','MediaServer','RTSP','Retransmitter');

type
  TPtzPoint = record
    DeviceId: integer;
    Name: string;
    Description: string;
  end;
  TPtzPointArray = array of TPtzPoint;

  TPtzProtocol = record
    Enabled: boolean;
    ProtocolClassName: string;
    Port: Word;
    UserName: string;
    UserPassword: string;

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
  end;

  TPtzStopTranslationWhileMoving = record
    Enabled: boolean;
    Period: integer; //мс

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
  end;

  TPhysicalDeviceParameters = record
    DeviceType:   TPhysicalDeviceType;
    ServerIP:     string;
    ServerPort:   Integer;

    Protocol:     Integer; //HHNet Protocol, TRecordStorageTransportType

    ChannelNo:    Integer; //Как правило, что-то одно из двух: channel или Name
    ChannelProfile: Integer;
    SourceName:   string;

    UserName:     string;
    UserPassword: string;

    TransmitAudio : boolean;

    PeriodStart,PeriodEnd: TDateTime; //Для архивных записей

    KeepPersistentConnection: boolean;

    PtzCustomProtocol: TPtzProtocol;

    PtzStopTranslationWhileMoving: TPtzStopTranslationWhileMoving;

    PtzPoints:TPtzPointArray;

    function GetDisplayDescription(aLongFormat: boolean): string;

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
  end;

  TClientAuthorizationProvider = (apBuiltIn,apDatabase);

  TClientConnectionParameters = record
    //Идентификация устройства, как к нему буду обращаться внешние клиенты
    Name:   string;
    Description: string;

    AuthorizationProvider: TClientAuthorizationProvider;
    UserName: string;
    Password: string; //Если пусто, значит не нужен
  end;

  TRecordStorageTransportType = (ttNetworkPath,ttFileServer);

  TArchiveSource = record
    Text: string; //Наименование (для пользователя)
    ConnectionString: string;
    SourceName: string;
    RecordStorageTransport: TRecordStorageTransportType;
    //Медиа-процессоры
    MediaProcessors: TAllMediaStreamBytes;

    function GetUrl: string;
  end;

  TArchiveParameters = record
    Sources: TArray<TArchiveSource>;

    function IsEnabled: boolean;
    function ToString: string;
  end;
  //TArchiveParametersArray = array of TArchiveParameters;

  //.TOverqueueDetectMode = (oqdmConstantBitrate, oqdmPercent)

  TProcessingQueueParams = record
    //макс. кол-во кадров, хранящихся в очереди обработки
    MaxSize: integer;

    //максимально допустимая длина в мсек первого и последнего кадра, хранящегося в очереди
    //для оценки используется системный поток
    MaxDuration: integer;

    //процент отдаваемого потока относительно входящего потока
    OverqueueBitrateTresholdPercent: integer;
    //период для измерения скорости потока
    OverqueueBitrateMeasureIntervalMs: integer;

    procedure Initialize;

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection: string);
  end;


  TInPointConfig = class
    Id:           string;
    Name:         string;
    Folder: string;

    //Только для информации
    Info_OriginalStreamType: TAllMediaStreamTypes;
    Info_ResultStreamType: TAllMediaStreamTypes;

    //Настройки физического поключения к камере
    DeviceParams:TPhysicalDeviceParameters;

    //Медиа-процессоры
    MediaProcessors: TAllMediaStreamBytes;

    //Extensions
    Extensions: TBytes;

    //Настройки очереди обработки
    ProcessingQueueParams: TProcessingQueueParams;
    //Использовать очсередь обработки только при наличии клиентских соединений к этой точке
    UseProcessingQueueOnlyIfNeeded: boolean;
    //надо ли при таком режиме очищать предбуфер (иначе туда пойдут необработанные кадры)
    UseProcessingQueueOnlyIfNeededAndClearPrebuffer: boolean;

    //максимальный размер пред. буфера
    PrebufferMaxSize: integer;

    constructor Create;

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection: string);
  end;

  TBackingParams = record
    //Подложка
    ImagePath: string;

    //Положение камеры на карте
    DevicePosition: TPoint;

    //Угол камеры, соответствующий "северу" на подложке
    DeviceZeroAngle: integer;
  end;

  TOutPointConfig = class
    Id:           string;
    //Включен
    Enabled:      Boolean;
    //Разрешать управление объектом (масштаб, поворот и проч), при наличии такой возможности у устройства
    UserInteractiveControlAllowed: boolean;

    //Группа
    Folder        : string;

    SourceId:string;
    SourceIdForUnauthorizedUser:string;
    SourceIdForBlockedUser:string;
    SourceIdForInsufficientPrivileges:string;
    SourceIdForDisconnectedState: string;
    SourceIdForOverQueue: string;

    //Настройки подложки
    BackingParams: TBackingParams;

    //Настройки очереди обработки
    ProcessingQueueParams: TProcessingQueueParams;

    //Настройки для внешнего подключения
    ClientParams:TClientConnectionParameters;

    //Медиа-процессоры
    MediaProcessors: TAllMediaStreamBytes;

    //Параметы архива для источника
    ArchiveParams: TArchiveParameters;

    constructor Create;

    procedure LoadFromIni(aIniFile: TCustomIniFile; const aSection: string);
    procedure SaveToIni(aIniFile: TCustomIniFile; const aSection: string);
  end;

  //Замещение одного сетевого адреса другим
  TNetAddressSubstitution = class
  private
    FSourceAddress: string;
    FDestAddress: string;
  public
    property SourceAddress: string read FSourceAddress write FSourceAddress;
    property DestAddress: string read FDestAddress write FDestAddress;
  end;

  TMediaServerConfig = class;
  TOnModifiedExternallyEvent = procedure (aSender: TMediaServerConfig) of object;

  TMediaServerConfig = class
  private
    FFileName   : string;
    FFileData   : AnsiString;
    FFileModificationDate : integer;
    FFileCheckTimer: TTimer;
    FUpdateLock: integer;

    FInPoints  : TObjectList<TInPointConfig>;
    FOutPoints  : TObjectList<TOutPointConfig>;
    FNetAddressSubstitutions: TObjectList<TNetAddressSubstitution>;

    FRestartAppEnabled: boolean;
    FRestartAppTime: TDateTime;
    FShowBaloonMessagesWhenMinimized: boolean;

    FOnModifiedExternally: TOnModifiedExternallyEvent;
    FMaxBufferSize: Integer;
    FBufferOverflowAction: TBufferOverflowAction;
    FProxyUsingPolicy: THHNetProxyUsingPolicy;
    FRtspPort: Word;
    FRtpPort: Word;
    F3sCommandPort: Word;
    F3sStreamPort: Word;
    FUserDataBaseUDL: string;
    FRtpTcpEnabled: boolean;
    FRtpUdpEnabled: boolean;
    FMscpPort: Word;
    FMsepPort: Word;
    F3sStreamSyncPeriod: cardinal;

    FNetRegistrationServerIp: string;
    FNetRegistrationName: string;
    FNetRegistrationEnabled: boolean;
    FRelayStartPort: Word;

    FRestartIfPageFileTooLargeEnabled: boolean;
    FRestartIfPageFileTooLargeMB: integer;

    class var FWatchingLock: integer;

    function  GetInPointCount: Integer;
    function  GetInPoint(i: Integer): TInPointConfig;
    function  GetOutPointCount: Integer;
    function  GetOutPoint(i: Integer): TOutPointConfig;

    procedure OnCheckFileTimer(Sender: TObject);
    procedure DoOnChangeFileExternally;
    procedure StartChangeWatching;

    procedure Load;

    function  AddInPointInternal: TInPointConfig;
    function  AddOutPointInternal: TOutPointConfig;
    function  GetNetAddressSubsitution(index: integer): TNetAddressSubstitution;
  public
    constructor Create(const aFilePath: string='');overload;
    destructor  Destroy; override;

    function FileName_: string;
    class function DefaultFileName: string;

    procedure   Reload;
    procedure   Save;

    function    AddInPoint(aInPointType: TPhysicalDeviceType; aCreateAutoName: boolean): TInPointConfig;
    function    IsInPointNameUnique(aInPoint: TInPointConfig):boolean;

    function    AddOutPoint(aCreateAutoName: boolean): TOutPointConfig;
    function    IsOutPointNameUnique(aOutPoint: TOutPointConfig):boolean;

    procedure   DeleteInPoint(aInPoint: TInPointConfig);
    procedure   ClearInPoints;
    property    InPoints[i: Integer]: TInPointConfig read GetInPoint;
    property    InPointCount: Integer read GetInPointCount;

    function    FindInPoint(const aIP: string; aPort: integer; aChannelNo: integer) : TInPointConfig; overload;
    function    FindInPoint(const aID: string) : TInPointConfig; overload;

    procedure   DeleteOutPoint(aOutPoint: TOutPointConfig);
    procedure   ClearOutPoints;
    property    OutPoints[i: Integer]: TOutPointConfig read GetOutPoint;
    property    OutPointCount: Integer read GetOutPointCount;

    // Максимальный размер буфера записи, МБ (для типа буфера btCommonDynamic)
    property    MaxBufferSizeMB: Integer read FMaxBufferSize write FMaxBufferSize;
    property    BufferOverflowAction : TBufferOverflowAction read FBufferOverflowAction write FBufferOverflowAction;

    property    RtspPort: Word read FRtspPort write FRtspPort;
    property    RtpPort: Word read FRtpPort write FRtpPort;
    property    RtpUdpEnabled: boolean read FRtpUdpEnabled write FRtpUdpEnabled;
    property    RtpTcpEnabled: boolean read FRtpTcpEnabled write FRtpTcpEnabled;


    property    Ms3sCommandPort: Word read F3sCommandPort write F3sCommandPort;
    property    Ms3sStreamPort: Word read F3sStreamPort write F3sStreamPort;
    property    Ms3sStreamSyncPeriod: cardinal read F3sStreamSyncPeriod write F3sStreamSyncPeriod;

    property    MscpPort: Word read FMscpPort write FMscpPort;
    property    MsepPort: Word read FMsepPort write FMsepPort;

    property    UserDataBaseUDL: string read FUserDataBaseUDL write FUserDataBaseUDL;

    //протокол для получения удаленных файлов
    property    ShowBaloonMessagesWhenMinimized: boolean read FShowBaloonMessagesWhenMinimized write FShowBaloonMessagesWhenMinimized;

    property    RestartAppEnabled: boolean read FRestartAppEnabled write FRestartAppEnabled;
    property    RestartAppTime: TDateTime read FRestartAppTime write FRestartAppTime;

    property    RestartIfPageFileTooLargeEnabled: boolean read FRestartIfPageFileTooLargeEnabled write FRestartIfPageFileTooLargeEnabled;
    property    RestartIfPageFileTooLargeMB: integer read FRestartIfPageFileTooLargeMB write FRestartIfPageFileTooLargeMB;

    property    RelayStartPort: Word read FRelayStartPort write FRelayStartPort;

    function    NetAddressSubsitutionCount: integer;
    property    NetAddressSubsitutions[index:integer]: TNetAddressSubstitution read GetNetAddressSubsitution;
    function    AddNetAddressSubsitution(const aSourceAddress,aDestAddress: string):TNetAddressSubstitution;
    procedure   ClearNetAddressSubsitutions;

    //регистрация в сети
    property    NetRegistrationEnabled: boolean read FNetRegistrationEnabled write FNetRegistrationEnabled;
    property    NetRegistrationServerIp: string read FNetRegistrationServerIp write FNetRegistrationServerIp;
    property    NetRegistrationName: string read FNetRegistrationName write FNetRegistrationName;

    function    SuperUserPassword: string;

    property    ProxyUsingPolicy: THHNetProxyUsingPolicy read FProxyUsingPolicy write FProxyUsingPolicy;
    property    OnModifiedExternally: TOnModifiedExternallyEvent read FOnModifiedExternally write FOnModifiedExternally;

    class procedure LockWatching;
    class procedure UnlockWatching;
  end;

  EConfigError = class (Exception);

  function RecordStorageTransportTypeToString(const aType: TRecordStorageTransportType): string;


implementation
  uses Generics.Defaults, uBaseUtils, MediaServer.Net.Definitions,IdHashMessageDigest;

const
  DefaultProcessingQueueMaxSize = 120;
  DefaultPrebufferMaxSize = 120;
  DefaultMediaProcessingQueueMaxDuration = 3000;
  DefaultOverqueueBitrateMeasureIntervalMs = 25000;
  DefaultOverqueueBitrateTresholdPercent=50;

function RecordStorageTransportTypeToString(const aType: TRecordStorageTransportType): string;
const
  Names : array [TRecordStorageTransportType] of string = ('NetworkPath','FileServer');
begin
  if not (integer(aType) in [0..Length(Names)-1]) then
    result:=''
  else
    result:=Names[aType];
end;

function MaskString(const aString: string): string;
var
  i: integer;
begin
  result:=aString;
  for i := 1 to Length(aString) do
    result[i]:=char(integer(aString[i]) XOR (Length(aString)-i+1));
end;

{ TInPointConfig }

{ TMediaServerConfig }

constructor TMediaServerConfig.Create(const aFilePath: string='');
var
  s: string;
begin
  s:=aFilePath;
  if s='' then
    s:=DefaultFileName;
  FFileName:=s;

  FInPoints := TObjectList<TInPointConfig>.Create(TComparer<TInPointConfig>.Default);
  FOutPoints := TObjectList<TOutPointConfig>.Create(TComparer<TOutPointConfig>.Default);
  FNetAddressSubstitutions:=TObjectList<TNetAddressSubstitution>.Create(TComparer<TNetAddressSubstitution>.Default);

  FFileCheckTimer:=TTimer.Create(nil); //на объекте была какая-то странная ситуация, когда стандартные функции Windows по обнаружению изменений работали неправильно. Пришлось переделать на таймер со сравнением даты файлов
  FFileCheckTimer.OnTimer:=OnCheckFileTimer;
  FFileCheckTimer.Interval:=1000;
  FFileCheckTimer.Enabled:=false;

  Load;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.Load;
var
  aSection: string;
  aIndex: Integer;
  aInPointConfig: TInPointConfig;
  aOutPointConfig: TOutPointConfig;
  aIniFile: TMemIniFile;
  FStream: TStream;
  x: integer;
begin
  Assert(FFileName<>'');

  FFileCheckTimer.Enabled:=false;

  if FileExists(FFileName) then
  begin
    //Читаем полностью содержимое исходного файла
    FStream:=TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
    try
      SetLength(FFileData,FStream.Size);
      FStream.Read(FFileData[1],Length(FFileData));
    finally
      FreeAndNil(FStream);
    end;
  end;

  aIniFile:=TMemIniFile.Create(FFileName);
  try
    FInPoints.Clear;
    FOutPoints.Clear;
    FNetAddressSubstitutions.Clear;

    FBufferOverflowAction := TBufferOverflowAction(aIniFile.ReadInteger('General', 'BufferOverflowAction', integer(boaWriteIFramesOnly)));
    FMaxBufferSize := aIniFile.ReadInteger('General', 'MaxBufferSize', 200);
    FRestartAppEnabled:=aIniFile.ReadBool('General', 'RestartAppEnabled', true);
    FRestartAppTime:=aIniFile.ReadTime('General', 'RestartAppTime', EncodeTime(6,0,0,0));
    FRestartIfPageFileTooLargeEnabled:=aIniFile.ReadBool('General', 'RestartIfPageFileTooLargeEnabled', true);
    FRestartIfPageFileTooLargeMB:=aIniFile.ReadInteger('General', 'RestartIfPageFileTooLargeMB', 2500);

    FShowBaloonMessagesWhenMinimized:=aIniFile.ReadBool('General', 'ShowBaloonMessagesWhenMinimized', false);

    FRtspPort:=aIniFile.ReadInteger('General','RtspPort',icRtspServerPort);
    FRtpPort:=aIniFile.ReadInteger('General','RtpPort',6700);
    FRtpTcpEnabled:=aIniFile.ReadBool('General','RtpTcpEnabled',true);
    FRtpUdpEnabled:=aIniFile.ReadBool('General','RtpUdpEnabled',true);

    F3sCommandPort:=aIniFile.ReadInteger('General','3SCommandPort',icCommandServerPort);
    F3sStreamPort:=aIniFile.ReadInteger('General','3SStreamPort',icStreamServerPort);
    F3sStreamSyncPeriod:=aIniFile.ReadInteger('General','3SStreamSyncPeriod',25000);
    if F3sStreamSyncPeriod<5000 then
      F3sStreamSyncPeriod:=5000; //04.07.2012 Слишком маленький период дает большую нагрузку на сеть


    FMscpPort:=aIniFile.ReadInteger('General','MscpPort',icMSCPServerPort);
    FMsepPort:=aIniFile.ReadInteger('General','MsepPort',icMsepServerPort);

    FRelayStartPort:=aIniFile.ReadInteger('General','RelayStartPort',18700);

    FUserDataBaseUDL:=aIniFile.ReadString('General','UserDataBaseUDL','');

    x:=aIniFile.ReadInteger('General','ProxyUsingPolicy',0);
    if (x>integer(High(FProxyUsingPolicy))) or (x<integer(Low(FProxyUsingPolicy))) then
      x:=integer(Low(FProxyUsingPolicy));
    FProxyUsingPolicy:=THHNetProxyUsingPolicy(x);

    FNetRegistrationEnabled:=aIniFile.ReadBool('NetRegistration','Enabled',false);
    FNetRegistrationServerIp:=aIniFile.ReadString('NetRegistration','ServerIp','');
    FNetRegistrationName:=aIniFile.ReadString('NetRegistration','Name','');

    aIndex := 0;
    while true do
    begin
      aSection := Format('InPoint %d', [aIndex]);
      if not aIniFile.SectionExists(aSection) then
        break;

      aInPointConfig := AddInPointInternal;
      aInPointConfig.LoadFromIni(aIniFile,aSection);;

      Inc(aIndex);
    end;

    aIndex := 0;
    while true do
    begin
      aSection := Format('OutPoint %d', [aIndex]);
      if not aIniFile.SectionExists(aSection) then
        break;

      aOutPointConfig := AddOutPointInternal;
      aOutPointConfig.LoadFromIni(aIniFile,aSection);
      Inc(aIndex);
    end;

    aIndex := 0;
    while true do
    begin
      aSection := Format('NetAddressSubstitution %d', [aIndex]);
      if not aIniFile.SectionExists(aSection) then
        break;

      AddNetAddressSubsitution(
        aIniFile.ReadString(aSection, 'Source', ''),
        aIniFile.ReadString(aSection, 'Destination','')
      );
      Inc(aIndex);
    end;
  finally
    aIniFile.Free;
  end;

  StartChangeWatching;
end;
//------------------------------------------------------------------------------
class procedure TMediaServerConfig.LockWatching;
begin
  InterlockedIncrement(FWatchingLock);
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.NetAddressSubsitutionCount: integer;
begin
  result:=FNetAddressSubstitutions.Count;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.Save;
var
  aSection: string;
  i,j: Integer;
  aInPointConfig: TInPointConfig;
  aOutPointConfig: TOutPointConfig;
  aNetAddressSubstitution: TNetAddressSubstitution;
  aIniFile : TMemIniFile;
begin
  Assert(FFileName<>'');

  if FRtpPort mod 2 <>0 then
    raise EConfigError.Create('RTP порт должен быть четным');

  for i := 0 to FInPoints.Count - 1 do
  begin
    if (FInPoints[i].Name='') then
      raise EConfigError.Create('Не задано имя для устройства');

    for j:=i+1 to FInPoints.Count - 1 do
    begin
      if (AnsiSameText(FInPoints[i].Name, FInPoints[j].Name))  then
        raise EConfigError.CreateFmt('Имя "%s" повторяется несколько раз. Не допускается одинаковое описание для двух или более камер.',[FInPoints[i].Name]);
    end;
  end;

  for i := 0 to FOutPoints.Count - 1 do
  begin
    if (FOutPoints[i].ClientParams.Name='') then
      raise EConfigError.Create('Не задано имя для исходящей точки подключения');

    if (FOutPoints[i].SourceId='') then
      raise EConfigError.CreateFmt('Для исходящей точки подключения "%s" не указан источник',[FOutPoints[i].ClientParams.Name]);

    if (fsiBaseUtils.StrPosOneOf(['/','@','\'],FOutPoints[i].ClientParams.Name)>0) then
      raise EConfigError.CreateFmt('В имени исходящей точки подключения "%s" присутствуют недопустимые символы',[FOutPoints[i].ClientParams.Name]);


    if FindInPoint(FOutPoints[i].SourceId)=nil then
      raise EConfigError.CreateFmt('Для исходящей точки подключения "%s" не найден указанный источник',[FOutPoints[i].ClientParams.Name]);


    for j:=i+1 to FOutPoints.Count - 1 do
    begin
      if (AnsiSameText(FOutPoints[i].ClientParams.Name, FOutPoints[j].ClientParams.Name))  then
        raise EConfigError.CreateFmt('Имя "%s" повторяется несколько раз. Не допускается одинаковое описание для двух или более камер.',[FOutPoints[i].ClientParams.Name]);
    end;

    if FOutPoints[i].ClientParams.AuthorizationProvider=apDatabase then
    begin
      if FUserDataBaseUDL='' then
        raise EConfigError.CreateFmt('Исходящая точка подключения "%s" использует авторизацию из внешней БД, но параметры подключения к БД не указаны.',[FOutPoints[i].ClientParams.Name]);
      if not FileExists(FUserDataBaseUDL)  then
        raise EConfigError.CreateFmt('Исходящая точка подключения "%s" использует авторизацию из внешней БД, но параметры подключения к БД не корректны.',[FOutPoints[i].ClientParams.Name]);
    end;
  end;

  FFileCheckTimer.Enabled:=false;
  DeleteFile(FFileName);
  if FileExists(FFileName)  then
    raise Exception.Create('Не удается получить доступ на запись к файлу конфигурации');

  aIniFile:=TMemIniFile.Create(FFileName);
  try
    // общие параметры
    aIniFile.WriteInteger('General', 'BufferOverflowAction', Integer(FBufferOverflowAction));
    aIniFile.WriteInteger('General', 'MaxBufferSize', FMaxBufferSize);

    aIniFile.WriteBool   ('General', 'RestartAppEnabled', FRestartAppEnabled);
    aIniFile.WriteTime   ('General', 'RestartAppTime', FRestartAppTime);
    aIniFile.WriteBool   ('General', 'RestartIfPageFileTooLargeEnabled', FRestartIfPageFileTooLargeEnabled);
    aIniFile.WriteInteger('General', 'RestartIfPageFileTooLargeMB', FRestartIfPageFileTooLargeMB);

    aIniFile.WriteBool   ('General', 'ShowBaloonMessagesWhenMinimized', FShowBaloonMessagesWhenMinimized);

    aIniFile.WriteInteger('General','ProxyUsingPolicy',integer(FProxyUsingPolicy));

    aIniFile.WriteInteger('General','RtspPort',FRtspPort);
    aIniFile.WriteInteger('General','RtpPort',FRtpPort);
    aIniFile.WriteBool('General','RtpTcpEnabled',FRtpTcpEnabled);
    aIniFile.WriteBool('General','RtpUdpEnabled',FRtpUdpEnabled);

    aIniFile.WriteInteger('General','3SCommandPort',F3sCommandPort);
    aIniFile.WriteInteger('General','3SStreamPort',F3sStreamPort);
    aIniFile.WriteInteger('General','3SStreamSyncPeriod',F3sStreamSyncPeriod);

    aIniFile.WriteInteger('General','MscpPort',FMscpPort);
    aIniFile.WriteInteger('General','MsepPort',FMsepPort);

    aIniFile.WriteString('General','UserDataBaseUDL',FUserDataBaseUDL);

    aIniFile.WriteBool('NetRegistration','Enabled',FNetRegistrationEnabled);
    aIniFile.WriteString('NetRegistration','ServerIp',FNetRegistrationServerIp);
    aIniFile.WriteString('NetRegistration','Name',FNetRegistrationName);
    aIniFile.WriteInteger('General','RelayStartPort',FRelayStartPort);

    // список устройств
    for i := 0 to FInPoints.Count - 1 do
    begin
      aSection := Format('InPoint %d', [i]);
      aInPointConfig := FInPoints[i];
      aInPointConfig.SaveToIni(aIniFile,aSection);
    end;

    // список устройств
    for i := 0 to FOutPoints.Count - 1 do
    begin
      aSection := Format('OutPoint %d', [i]);
      aOutPointConfig := FOutPoints[i];
      aOutPointConfig.SaveToIni(aIniFile,aSection);
    end;

    // список устройств
    for i := 0 to FNetAddressSubstitutions.Count - 1 do
    begin
      aSection := Format('NetAddressSubstitution %d', [i]);
      aNetAddressSubstitution := FNetAddressSubstitutions[i];

      aIniFile.WriteString(aSection, 'Source', aNetAddressSubstitution.SourceAddress);
      aIniFile.WriteString(aSection, 'Destination', aNetAddressSubstitution.DestAddress);
    end;

    aIniFile.UpdateFile;
  finally
    aIniFile.Free;
  end;

  StartChangeWatching;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.AddInPoint(aInPointType: TPhysicalDeviceType; aCreateAutoName: boolean): TInPointConfig;
var
  i,j: integer;
  s: string;
  aPrefix: string;
begin
  result:=AddInPointInternal;
  Result.DeviceParams.DeviceType:=aInPointType;

  aPrefix:=PhysicalDeviceTypeNamesShort[aInPointType];
  s:='';
  if aCreateAutoName then
  begin
    //Подбираем имя по умолчанию
    for j:=1 to 512 do
    begin
      s := Format('%s_%d', [aPrefix,j]);
      for i:=0 to InPointCount-1 do
        if AnsiSameText(FInPoints[i].Name,s) then
        begin
          s:='';
          break;
        end;

      if s<>'' then
        break;
    end;
  end;


  s:=StringReplace(s,'"','',[rfReplaceAll]);
  Result.Name:=s;

  case aInPointType of
    dtIpCameraBeward: begin
      Result.DeviceParams.UserName:='admin';
      Result.DeviceParams.UserPassword:='admin';
      Result.DeviceParams.ServerIP := '192.168.1.1';
      Result.DeviceParams.ServerPort := 5000;
    end;

    dtFile: begin
      Result.DeviceParams.ChannelNo:=-1;
    end;

    dtRecordStorageSource: begin
      Result.DeviceParams.ChannelNo:=-1;
    end;

    dtCompressionCardHikvision: begin
    end;

    dtMediaServer: begin
      Result.DeviceParams.ServerIP := '192.168.1.1';
      Result.DeviceParams.ServerPort := icCommandServerPort;
      Result.DeviceParams.UserName:='root';
      Result.DeviceParams.UserPassword:='root';
    end;
  end;

  result.ProcessingQueueParams.Initialize;
  result.PrebufferMaxSize:=DefaultPrebufferMaxSize;
end;
//------------------------------------------------------------------------------
function  TMediaServerConfig.AddOutPoint(aCreateAutoName: boolean): TOutPointConfig;
var
  i,j: integer;
  s: string;
begin
  result:=AddOutPointInternal;

  s:='';
  if aCreateAutoName then
  begin
    //Подбираем имя по умолчанию
    for j:=1 to 512 do
    begin
      s := Format('Out_%d', [j]);
      for i:=0 to OutPointCount-1 do
        if AnsiSameText(FOutPoints[i].ClientParams.Name,s) then
        begin
          s:='';
          break;
        end;

      if s<>'' then
        break;
    end;
  end;


  s:=StringReplace(s,'"','',[rfReplaceAll]);
  Result.ClientParams.UserName:='root';
  Result.ClientParams.Name:=s;
  Result.ArchiveParams.Sources:=nil;


  result.ProcessingQueueParams.Initialize;
  // по умолчанию устройство включено
  Result.Enabled := True;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.AddOutPointInternal: TOutPointConfig;
begin
  Result := TOutPointConfig.Create;
  Result.ClientParams.Description:='';
  Result.ProcessingQueueParams.Initialize;
  FOutPoints.Add(Result);
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.DeleteInPoint(aInPoint: TInPointConfig);
var
  aFoundIndex: Integer;
begin
  aFoundIndex := FInPoints.IndexOf(aInPoint);
  if aFoundIndex <> -1 then
    FInPoints.Delete(aFoundIndex);
end;
//------------------------------------------------------------------------------
destructor TMediaServerConfig.Destroy;
begin
  FreeAndNil(FInPoints);
  FreeAndNil(FOutPoints);
  FreeAndNil(FNetAddressSubstitutions);
  FreeAndNil(FFileCheckTimer);
  inherited;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.GetInPointCount: Integer;
begin
  Result := FInPoints.Count;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.GetNetAddressSubsitution(index: integer): TNetAddressSubstitution;
begin
  result:=FNetAddressSubstitutions[index];
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.GetInPoint(i: Integer): TInPointConfig;
begin
  Result := TInPointConfig(FInPoints[i]);
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.AddInPointInternal: TInPointConfig;
begin
  Result := TInPointConfig.Create;
  Result.DeviceParams.Protocol := 0;
  Result.DeviceParams.ChannelNo := 0;
  Result.DeviceParams.KeepPersistentConnection:=true;
  Result.DeviceParams.PtzCustomProtocol.Port:=80;
  Result.DeviceParams.PtzStopTranslationWhileMoving.Period:=200;
  Result.ProcessingQueueParams.Initialize;
  FInPoints.Add(Result);
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.AddNetAddressSubsitution(const aSourceAddress,
  aDestAddress: string): TNetAddressSubstitution;
begin
  result:=TNetAddressSubstitution.Create;
  result.SourceAddress:=aSourceAddress;
  result.DestAddress:=aDestAddress;
  FNetAddressSubstitutions.Add(result);
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.ClearInPoints;
begin
  FInPoints.Clear;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.ClearNetAddressSubsitutions;
begin
  FNetAddressSubstitutions.Clear;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.FindInPoint(const aIP: string; aPort: integer; aChannelNo: integer): TInPointConfig;
var
  i: integer;
begin
  result:=nil;
  for i:=0 to InPointCount-1 do
  begin
    if AnsiSameText(FInPoints[i].DeviceParams.ServerIP, aIP) and
       (FInPoints[i].DeviceParams.ServerPort=aPort)  and
       (FInPoints[i].DeviceParams.ChannelNo=aChannelNo) then
    begin
      result:=FInPoints[i];
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.FindInPoint(const aID: string): TInPointConfig;
var
  i: integer;
begin
  result:=nil;
  for i:=0 to InPointCount-1 do
  begin
    if FInPoints[i].Id=aID then
    begin
      result:=FInPoints[i];
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.ClearOutPoints;
begin
  FOutPoints.Clear;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.GetOutPointCount: Integer;
begin
  Result := FOutPoints.Count;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.GetOutPoint(i: Integer): TOutPointConfig;
begin
  Result := TOutPointConfig(FOutPoints[i]);
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.DeleteOutPoint(aOutPoint: TOutPointConfig);
var
  aFoundIndex: Integer;
begin
  aFoundIndex := FOutPoints.IndexOf(aOutPoint);
  if aFoundIndex <> -1 then
    FOutPoints.Delete(aFoundIndex);
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.FileName_: string;
begin
  result:=FFileName;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.OnCheckFileTimer(Sender: TObject);
const
  aMethodName = 'TMediaServerConfig.OnCheckFileTimer';
var
  aTmp: integer;
begin
  if (FUpdateLock>0) or (FWatchingLock>0)  then
    exit;

  aTmp:=FileAge(FFileName);
  if (aTmp<>-1) and (aTmp<>FFileModificationDate) then
  begin
    FFileCheckTimer.Enabled:=false;
    try
      if FWatchingLock=0 then
        DoOnChangeFileExternally;
    except
      on E:Exception do
        ;//??
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.Reload;
begin
  Load;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.DoOnChangeFileExternally;
var
  FStream: TStream;
  aData: AnsiString;
begin
  if FUpdateLock>0 then
    exit;

  //Читаем полностью содержимое исходного файла
  FStream:=TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
  try
    SetLength(aData,FStream.Size);
    FStream.Read(aData[1],Length(aData));
  finally
    FreeAndNil(FStream);
  end;

  //ничего не изменилось
  if aData=FFileData then
  begin
    StartChangeWatching;
    exit;
  end;

  Load;
  if Assigned(FOnModifiedExternally) then
    FOnModifiedExternally(self);
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.IsInPointNameUnique(aInPoint: TInPointConfig):boolean;
var
  i: integer;
begin
  for i:=0 to InPointCount-1 do
    if FInPoints[i]<>aInPoint then
      if AnsiSameText(FInPoints[i].Name,aInPoint.Name) then
        exit(false);

  result:=true;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.IsOutPointNameUnique(aOutPoint: TOutPointConfig): boolean;
var
  i: integer;
begin
  for i:=0 to OutPointCount-1 do
    if FOutPoints[i]<>aOutPoint then
      if AnsiSameText(FOutPoints[i].ClientParams.Name, aOutPoint.ClientParams.Name) then
        exit(false);

  result:=true;
end;
//------------------------------------------------------------------------------
procedure TMediaServerConfig.StartChangeWatching;
var
  FStream: TStream;
begin
  Assert(FFileName<>'');
  FFileModificationDate:=FileAge(FFileName);

  if FileExists(FFileName) then
  begin
    //Читаем полностью содержимое исходного файла
    FStream:=TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
    try
      SetLength(FFileData,FStream.Size);
      FStream.Read(FFileData[1],Length(FFileData));
    finally
      FreeAndNil(FStream);
    end;
  end;


  FFileCheckTimer.Enabled:=true;
end;
//------------------------------------------------------------------------------
function TMediaServerConfig.SuperUserPassword: string;
begin
  result:=DefaultSuperUserPassword;
end;
//------------------------------------------------------------------------------
class procedure TMediaServerConfig.UnlockWatching;
begin
  InterlockedDecrement(FWatchingLock);
end;
//------------------------------------------------------------------------------
class function TMediaServerConfig.DefaultFileName: string;
begin
  result:=ExtractFileDir(ParamStr(0)) + '\MediaServer.cfg';
end;

{ TPhysicalDeviceParameters }

function TPhysicalDeviceParameters.GetDisplayDescription(aLongFormat: boolean): string;
var
  aAddress: string;
  aChannel : string;
  i: integer;
begin
  if aLongFormat then
    aAddress:=PhysicalDeviceTypeNamesLong[DeviceType]
  else
    aAddress:=PhysicalDeviceTypeNamesShort[DeviceType];

  if DeviceType=dtInPointsMultiplexer then
  begin
    if ServerIP='' then
      i:=0
    else
      i:=fsiBaseUtils.CharCount(ServerIP,';')+1;

    result:=aAddress+ ', '+IntToStr(i);
    exit;
  end;


  if ServerPort<>0 then
    aAddress:=aAddress+Format(' %s:%d',[ServerIP,ServerPort])
  else
    aAddress:=aAddress+' '+ServerIP;

  if ChannelNo>=0 then
  begin
    aChannel:=Format(', канал %d',[ChannelNo+1]);
    if ChannelProfile>0 then
      aChannel:=Format('%s/%d',[aChannel,ChannelProfile+1]);
  end;

  result:=Trim(aAddress)+Trim(aChannel);
end;


procedure TPhysicalDeviceParameters.LoadFromIni(aIniFile: TCustomIniFile;const aSection,aPrefix: string);
var
  i: integer;
begin
  self.DeviceType:= TPhysicalDeviceType(aIniFile.ReadInteger(aSection, aPrefix+'Type', 0));
  self.ServerIP := aIniFile.ReadString(aSection, aPrefix+'ServerIP', '');
  self.ServerPort := aIniFile.ReadInteger(aSection, aPrefix+'ServerPort', 0);
  self.Protocol := aIniFile.ReadInteger(aSection, aPrefix+'Protocol', 0);
  self.ChannelNo := aIniFile.ReadInteger(aSection, aPrefix+'ChannelNum', 0);
  self.ChannelProfile := aIniFile.ReadInteger(aSection, aPrefix+'ChannelProfile', 0);
  self.SourceName:=aIniFile.ReadString(aSection, aPrefix+'SourceName', '');
  self.UserName := aIniFile.ReadString(aSection, aPrefix+'UserName', '');
  self.UserPassword := MaskString(fsiBaseUtils.CodesToString(aIniFile.ReadString(aSection, aPrefix+'UserPassword', '')));
  self.KeepPersistentConnection := aIniFile.ReadBool(aSection, aPrefix+'KeepPersistentConnection', false);
  self.TransmitAudio := aIniFile.ReadBool(aSection, aPrefix+'TransmitAudio', false);

  PtzCustomProtocol.LoadFromIni(aIniFile,aSection,aPrefix+'PtzCustomProtocol.');
  PtzStopTranslationWhileMoving.LoadFromIni(aIniFile,aSection,aPrefix+'PtzStopTranslationWhileMoving.');

  SetLength(self.PtzPoints,aIniFile.ReadInteger(aSection, aPrefix+'PtzPoints.Count', 0));
  for i := 0 to High(self.PtzPoints) do
  begin
    self.PtzPoints[i].DeviceId:=aIniFile.ReadInteger(aSection, Format(aPrefix+'PtzPoints.%d.DeviceId',[i]), 0);
    self.PtzPoints[i].Name:=aIniFile.ReadString(aSection, Format(aPrefix+'PtzPoints.%d.Name',[i]), '');
    self.PtzPoints[i].Description:=aIniFile.ReadString(aSection, Format(aPrefix+'PtzPoints.%d.Description',[i]), '');
  end;


  self.PeriodStart:=aIniFile.ReadDateTime(aSection, aPrefix+'PeriodStart', 0);
  self.PeriodEnd:=aIniFile.ReadDateTime(aSection, aPrefix+'PeriodEnd', 0);
end;

procedure TPhysicalDeviceParameters.SaveToIni(aIniFile: TCustomIniFile;const aSection,aPrefix: string);
var
  j: integer;
begin
  aIniFile.WriteInteger(aSection, aPrefix+'Type', integer(self.DeviceType));
  aIniFile.WriteString(aSection, aPrefix+'ServerIP', self.ServerIP);
  aIniFile.WriteInteger(aSection, aPrefix+'ServerPort', self.ServerPort);
  aIniFile.WriteInteger(aSection, aPrefix+'Protocol', self.Protocol);
  aIniFile.WriteInteger(aSection, aPrefix+'ChannelNum', self.ChannelNo);
  aIniFile.WriteInteger(aSection, aPrefix+'ChannelProfile', self.ChannelProfile);
  aIniFile.WriteString(aSection, aPrefix+'SourceName', self.SourceName);
  aIniFile.WriteString(aSection, aPrefix+'UserName', self.UserName);
  aIniFile.WriteString(aSection, aPrefix+'UserPassword', fsiBaseUtils.StringToCodes(MaskString(self.UserPassword)));
  aIniFile.WriteBool(aSection, aPrefix+'KeepPersistentConnection', self.KeepPersistentConnection);
  aIniFile.WriteDateTime(aSection, aPrefix+'PeriodStart', self.PeriodStart);
  aIniFile.WriteDateTime(aSection, aPrefix+'PeriodEnd', self.PeriodEnd);
  aIniFile.WriteBool(aSection, aPrefix+'TransmitAudio', self.TransmitAudio);

  PtzCustomProtocol.SaveToIni(aIniFile,aSection,aPrefix+'PtzCustomProtocol.');
  PtzStopTranslationWhileMoving.SaveToIni(aIniFile,aSection,aPrefix+'PtzStopTranslationWhileMoving.');

  aIniFile.WriteInteger(aSection, aPrefix+'PtzPoints.Count', Length(self.PtzPoints));
  for j := 0 to High(self.PtzPoints) do
  begin
    aIniFile.WriteInteger(aSection, Format(aPrefix+'PtzPoints.%d.DeviceId',[j]), self.PtzPoints[j].DeviceId);
    aIniFile.WriteString(aSection, Format(aPrefix+'PtzPoints.%d.Name',[j]), self.PtzPoints[j].Name);
    aIniFile.WriteString(aSection, Format(aPrefix+'PtzPoints.%d.Description',[j]), self.PtzPoints[j].Description);
  end;
end;

{ TInPointConfig }

constructor TInPointConfig.Create;
var
  aGUID: TGUID;
begin
  CoCreateGuid(aGUID);
  UseProcessingQueueOnlyIfNeeded:=false;
  UseProcessingQueueOnlyIfNeededAndClearPrebuffer:=true;
  Id:=GUIDToString(aGUID);
end;

procedure TInPointConfig.LoadFromIni(aIniFile: TCustomIniFile;const aSection: string);
var
  aMediaType: TMediaType;
  s: AnsiString;
begin
  self.Id := aIniFile.ReadString(aSection, 'ID', '');
  self.ProcessingQueueParams.LoadFromIni(aIniFile, aSection);
  self.PrebufferMaxSize:=aIniFile.ReadInteger(aSection, 'PrebufferMaxSize', DefaultPrebufferMaxSize);
  self.UseProcessingQueueOnlyIfNeeded:=aIniFile.ReadBool(aSection, 'UsesProcessingQueueOnlyIfNeeded',false);
  self.UseProcessingQueueOnlyIfNeededAndClearPrebuffer:=aIniFile.ReadBool(aSection, 'UseProcessingQueueOnlyIfNeededAndClearPrebuffer',true);
  //self.Enabled := aIniFile.ReadBool(aSection, 'Enabled', True);

  //Physical Device
  self.Name := aIniFile.ReadString(aSection, 'Name', '');
  self.Folder := aIniFile.ReadString(aSection, 'Folder', '');

  Self.DeviceParams.LoadFromIni(aIniFile,aSection,'DeviceParams.');
  //Media Processors
  for aMediaType := Low(TMediaType) to High(TMediaType) do
  begin
    s:=aIniFile.ReadString(aSection,'MediaProcessors.'+MediaTypeNames[aMediaType],'');
    self.MediaProcessors[aMediaType]:=fsiBaseUtils.AnsiStringToBytes(fsiBaseUtils.CodesToString(s));

    self.Info_OriginalStreamType[aMediaType]:=aIniFile.ReadInteger(aSection,'Info.OriginalStreamType.'+MediaTypeNames[aMediaType],0);
    self.Info_ResultStreamType[aMediaType]:=aIniFile.ReadInteger(aSection,'Info.ResultStreamType.'+MediaTypeNames[aMediaType],0);
  end;

  //Extensions
  s:=aIniFile.ReadString(aSection,'Extensions','');
  self.Extensions:=fsiBaseUtils.AnsiStringToBytes(fsiBaseUtils.CodesToString(s));
end;

procedure TInPointConfig.SaveToIni(aIniFile: TCustomIniFile;
  const aSection: string);
var
  aMediaType: TMediaType;
  s: AnsiString;
begin
  aIniFile.WriteString(aSection, 'ID', self.Id);
  aIniFile.WriteString(aSection, 'Name', self.Name);
  aIniFile.WriteString(aSection, 'Folder', self.Folder);
  aIniFile.WriteInteger(aSection, 'PrebufferMaxSize', self.PrebufferMaxSize);
  aIniFile.WriteBool(aSection, 'UsesProcessingQueueOnlyIfNeeded', self.UseProcessingQueueOnlyIfNeeded);
  aIniFile.WriteBool(aSection, 'UseProcessingQueueOnlyIfNeededAndClearPrebuffer', self.UseProcessingQueueOnlyIfNeededAndClearPrebuffer);
  self.ProcessingQueueParams.SaveToIni(aIniFile,aSection);

  //Physical Device
  self.DeviceParams.SaveToIni(aIniFile,aSection,'DeviceParams.');

  for aMediaType:=Low(aMediaType) to High(aMediaType) do
  begin
    //Медиа-процессоры
    s:=fsiBaseUtils.BytesToAnsiString(self.MediaProcessors[aMediaType]);
    s:=fsiBaseUtils.StringToCodes(s);
    Assert(Length(s)=2*Length(self.MediaProcessors[aMediaType]));
    aIniFile.WriteString(aSection,'MediaProcessors.'+MediaTypeNames[aMediaType],s);

    aIniFile.WriteInteger(aSection,'Info.OriginalStreamType.'+MediaTypeNames[aMediaType],self.Info_OriginalStreamType[aMediaType]);
    aIniFile.WriteInteger(aSection,'Info.ResultStreamType.'+MediaTypeNames[aMediaType],self.Info_ResultStreamType[aMediaType]);
  end;

  //Extensions
  s:=fsiBaseUtils.BytesToAnsiString(self.Extensions);
  s:=fsiBaseUtils.StringToCodes(s);
  aIniFile.WriteString(aSection,'Extensions',s);
end;

{ TOutPointConfig }

constructor TOutPointConfig.Create;
var
  aGUID: TGUID;
begin
  CoCreateGuid(aGUID);
  Id:=GUIDToString(aGUID);
end;

procedure TOutPointConfig.LoadFromIni(aIniFile: TCustomIniFile; const aSection: string);
var
  aName: string;
  i: Integer;
  aMediaType: TMediaType;
  s: AnsiString;
  b: boolean;
begin
  self.Id := aIniFile.ReadString(aSection, 'ID', '');
  self.Folder := aIniFile.ReadString(aSection, 'Folder', '');
  self.ProcessingQueueParams.LoadFromIni(aIniFile,aSection);

  self.SourceId := aIniFile.ReadString(aSection, 'SourceID', '');
  self.SourceIdForBlockedUser := aIniFile.ReadString(aSection, 'SourceIdForBlockedUser', '');
  self.SourceIdForUnauthorizedUser := aIniFile.ReadString(aSection, 'SourceIdForUnauthorizedUser', '');
  self.SourceIdForInsufficientPrivileges := aIniFile.ReadString(aSection, 'SourceIdForInsufficientPrivileges', '');
  self.SourceIdForDisconnectedState := aIniFile.ReadString(aSection, 'SourceIdForDisconnectedState', '');
  self.SourceIdForOverQueue := aIniFile.ReadString(aSection, 'SourceIdForOverQueue', '');

  self.BackingParams.ImagePath := aIniFile.ReadString(aSection, 'BackingParams.ImagePath', '');
  self.BackingParams.DeviceZeroAngle := aIniFile.ReadInteger(aSection, 'BackingParams.DeviceZeroAngle', 0);
  self.BackingParams.DevicePosition.X:=aIniFile.ReadInteger(aSection, 'BackingParams.DevicePosition.X', 0);
  self.BackingParams.DevicePosition.Y:=aIniFile.ReadInteger(aSection, 'BackingParams.DevicePosition.Y', 0);

  self.Enabled := aIniFile.ReadBool(aSection, 'Enabled', True);
  self.UserInteractiveControlAllowed:= aIniFile.ReadBool(aSection, 'UserInteractiveControlAllowed', True);

  //Client Connections
  self.ClientParams.Name:=aIniFile.ReadString(aSection, 'ClientParams.Name', '');
  self.ClientParams.Description:=aIniFile.ReadString(aSection, 'ClientParams.Description', '');
  //self.ClientParams.PasswordNeeded:=aIniFile.ReadBool(aSection,'ClientParams.PasswordNeeded',false);

  s:=fsiBaseUtils.StringToCodes(MaskString('root'));
  self.ClientParams.UserName:=MaskString(fsiBaseUtils.CodesToString(aIniFile.ReadString(aSection,'ClientParams.UserName',s)));
  self.ClientParams.Password:=MaskString(fsiBaseUtils.CodesToString(aIniFile.ReadString(aSection,'ClientParams.Password','')));
  self.ClientParams.AuthorizationProvider:=TClientAuthorizationProvider(aIniFile.ReadInteger(aSection,'ClientParams.AuthorizationProvider',0));

  //Archive Settings
  b:=aIniFile.ReadBool(aSection, 'ArchiveParams.Name', false);
  if b then //old format
  begin
    SetLength(self.ArchiveParams.Sources,1);
    self.ArchiveParams.Sources[0].ConnectionString:=aIniFile.ReadString(aSection, 'ArchiveParams.ConnectionString','');
    self.ArchiveParams.Sources[0].SourceName:=aIniFile.ReadString(aSection, 'ArchiveParams.SourceName', '');
    self.ArchiveParams.Sources[0].RecordStorageTransport:=TRecordStorageTransportType(aIniFile.ReadInteger(aSection, 'ArchiveParams.RecordStorageTransport', 0));
    //Media Processors for archive
    for aMediaType := Low(TMediaType) to High(TMediaType) do
    begin
      s:=aIniFile.ReadString(aSection,'ArchiveParams.MediaProcessors.'+MediaTypeNames[aMediaType],'');
      self.ArchiveParams.Sources[0].MediaProcessors[aMediaType]:=fsiBaseUtils.AnsiStringToBytes(fsiBaseUtils.CodesToString(s));
    end;
  end

  //Actual format
  else begin
    i:=aIniFile.ReadInteger(aSection,'ArchiveParams.Sources.Count',0);
    SetLength(self.ArchiveParams.Sources,i);
    for i := 0 to High(self.ArchiveParams.Sources) do
    begin
      aName:=Format('ArchiveParams.Sources.%d',[i]);

      self.ArchiveParams.Sources[i].ConnectionString:=aIniFile.ReadString(aSection, aName+'ConnectionString',self.ArchiveParams.Sources[i].ConnectionString);
      self.ArchiveParams.Sources[i].SourceName:=aIniFile.ReadString(aSection, aName+'SourceName', self.ArchiveParams.Sources[i].SourceName);
      self.ArchiveParams.Sources[i].RecordStorageTransport:=TRecordStorageTransportType(aIniFile.ReadInteger(aSection, aName+'RecordStorageTransport', integer(self.ArchiveParams.Sources[i].RecordStorageTransport)));
      self.ArchiveParams.Sources[i].Text:=aIniFile.ReadString(aSection, aName+'Text',self.ArchiveParams.Sources[i].Text);

      //Media Processors for archive
      for aMediaType := Low(TMediaType) to High(TMediaType) do
      begin
        s:=aIniFile.ReadString(aSection,aName+'MediaProcessors.'+MediaTypeNames[aMediaType],'');
        self.ArchiveParams.Sources[i].MediaProcessors[aMediaType]:=fsiBaseUtils.AnsiStringToBytes(fsiBaseUtils.CodesToString(s));
      end;
    end;
  end;


  //Media Processors
  for aMediaType := Low(TMediaType) to High(TMediaType) do
  begin
    s:=aIniFile.ReadString(aSection,'MediaProcessors.'+MediaTypeNames[aMediaType],'');
    self.MediaProcessors[aMediaType]:=fsiBaseUtils.AnsiStringToBytes(fsiBaseUtils.CodesToString(s));
  end;
end;

procedure TOutPointConfig.SaveToIni(aIniFile: TCustomIniFile; const aSection: string);
var
  aName: string;
  j: Integer;
  aMediaType: TMediaType;
  s: AnsiString;
begin
  aIniFile.WriteString(aSection, 'ID', self.Id);
  aIniFile.WriteString(aSection, 'Folder', self.Folder);
  aIniFile.WriteBool(aSection, 'Enabled', self.Enabled);
  aIniFile.WriteString(aSection, 'SourceID', self.SourceID);
  aIniFile.WriteString(aSection, 'SourceIdForBlockedUser', self.SourceIdForBlockedUser);
  aIniFile.WriteBool(aSection, 'UserInteractiveControlAllowed', self.UserInteractiveControlAllowed);
  aIniFile.WriteString(aSection, 'SourceIdForUnauthorizedUser', self.SourceIdForUnauthorizedUser);
  aIniFile.WriteString(aSection, 'SourceIdForInsufficientPrivileges', self.SourceIdForInsufficientPrivileges);
  aIniFile.WriteString(aSection, 'SourceIdForDisconnectedState', self.SourceIdForDisconnectedState);
  aIniFile.WriteString(aSection, 'SourceIdForOverQueue', self.SourceIdForOverQueue);
  self.ProcessingQueueParams.SaveToIni(aIniFile,aSection);

  aIniFile.WriteString(aSection, 'BackingParams.ImagePath', self.BackingParams.ImagePath);
  aIniFile.WriteInteger(aSection,'BackingParams.DeviceZeroAngle', self.BackingParams.DeviceZeroAngle);
  aIniFile.WriteInteger(aSection,'BackingParams.DevicePosition.X', self.BackingParams.DevicePosition.X);
  aIniFile.WriteInteger(aSection,'BackingParams.DevicePosition.Y', self.BackingParams.DevicePosition.Y);

  //Client Connections
  aIniFile.WriteString(aSection, 'ClientParams.Name', self.ClientParams.Name);
  aIniFile.WriteString(aSection, 'ClientParams.Description',self.ClientParams.Description);
  //aIniFile.WriteBool(aSection, 'ClientParams.PasswordNeeded', self.ClientParams.PasswordNeeded);
  aIniFile.WriteString(aSection, 'ClientParams.UserName', fsiBaseUtils.StringToCodes(MaskString(self.ClientParams.UserName)));
  aIniFile.WriteString(aSection, 'ClientParams.Password', fsiBaseUtils.StringToCodes(MaskString(self.ClientParams.Password)));
  aIniFile.WriteInteger(aSection,'ClientParams.AuthorizationProvider',integer(self.ClientParams.AuthorizationProvider));

  //Archive Settings
  aIniFile.WriteInteger(aSection,'ArchiveParams.Sources.Count',Length(self.ArchiveParams.Sources));
  for j := 0 to High(self.ArchiveParams.Sources) do
  begin
    aName:=Format('ArchiveParams.Sources.%d',[j]);

    aIniFile.WriteString(aSection, aName+'ConnectionString',self.ArchiveParams.Sources[j].ConnectionString);
    aIniFile.WriteString(aSection, aName+'SourceName', self.ArchiveParams.Sources[j].SourceName);
    aIniFile.WriteInteger(aSection, aName+'RecordStorageTransport', integer(self.ArchiveParams.Sources[j].RecordStorageTransport));
    aIniFile.WriteString(aSection, aName+'Text',self.ArchiveParams.Sources[j].Text);

    //Media Processors for archive
    for aMediaType := Low(TMediaType) to High(TMediaType) do
    begin
      s:=fsiBaseUtils.BytesToAnsiString(self.ArchiveParams.Sources[j].MediaProcessors[aMediaType]);
      s:=fsiBaseUtils.StringToCodes(s);
      Assert(Length(s)=2*Length(self.ArchiveParams.Sources[j].MediaProcessors[aMediaType]));

      aIniFile.WriteString(aSection,aName+'MediaProcessors.'+MediaTypeNames[aMediaType],s);
    end;
  end;

  //Медиа-процессоры
  for aMediaType:=Low(aMediaType) to High(aMediaType) do
  begin
    //Медиа-процессоры
    s:=fsiBaseUtils.BytesToAnsiString(self.MediaProcessors[aMediaType]);
    s:=fsiBaseUtils.StringToCodes(s);
    Assert(Length(s)=2*Length(self.MediaProcessors[aMediaType]));
    aIniFile.WriteString(aSection,'MediaProcessors.'+MediaTypeNames[aMediaType],s);
  end;
end;

{ TArchiveParameters }

function TArchiveParameters.IsEnabled: boolean;
var
  k: integer;
begin
  result:=false;
  for k := 0 to High(Sources) do
   if (Sources[k].SourceName<>'') and (Sources[k].ConnectionString<>'') then
   begin
     result:=true;
     break;
   end;
end;

function TArchiveParameters.ToString: string;
var
  k: integer;
begin
  result:='';
  for k := 0 to High(Sources) do
   if (Sources[k].SourceName<>'') and (Sources[k].ConnectionString<>'') then
   begin
     if result<>'' then
       result:=result+'; ';

     result:=result+Format('%s/%s, %s',[
            Sources[k].ConnectionString,
            Sources[k].SourceName,
            RecordStorageTransportTypeToString(Sources[k].RecordStorageTransport)]);
   end;
end;

{ TArchiveSource }

function MD5Encode(const aData: string): String;
var
  md5indy: TIdHashMessageDigest;
begin
  md5indy:=TIdHashMessageDigest5.Create;//создаем экземпляр объекта
  try
    result:=LowerCase(md5indy.HashStringAsHex(aData));//тот же хэш, но в HEX-форме
  finally
    md5indy.Free;
  end;
end;

function TArchiveSource.GetUrl: string;
begin
  if (ConnectionString<>'') and (SourceName<>'') then
    result:=MD5Encode(ConnectionString)+'/'+SourceName;
end;

{ TProcessingQueueParams }

procedure TProcessingQueueParams.Initialize;
begin
  MaxSize:=DefaultProcessingQueueMaxSize;
  MaxDuration:=DefaultMediaProcessingQueueMaxDuration;
  OverqueueBitrateTresholdPercent:=DefaultOverqueueBitrateTresholdPercent;
  OverqueueBitrateMeasureIntervalMs:=DefaultOverqueueBitrateMeasureIntervalMs;
end;

procedure TProcessingQueueParams.LoadFromIni(aIniFile: TCustomIniFile;const aSection: string);
begin
  self.MaxSize:=aIniFile.ReadInteger(aSection, 'ProcessingQueue.MaxSize', MaxSize);
  self.MaxDuration:=aIniFile.ReadInteger(aSection, 'ProcessingQueue.MaxDuration', MaxDuration);
  self.OverqueueBitrateTresholdPercent:=aIniFile.ReadInteger(aSection, 'ProcessingQueue.OverqueueBitrateTresholdPercent', OverqueueBitrateTresholdPercent);
  self.OverqueueBitrateMeasureIntervalMs:=aIniFile.ReadInteger(aSection, 'ProcessingQueue.OverqueueBitrateMeasureIntervalMs', OverqueueBitrateMeasureIntervalMs);
end;

procedure TProcessingQueueParams.SaveToIni(aIniFile: TCustomIniFile;
  const aSection: string);
begin
  aIniFile.WriteInteger(aSection, 'ProcessingQueue.MaxSize', MaxSize);
  aIniFile.WriteInteger(aSection, 'ProcessingQueue.MaxDuration', MaxDuration);
  aIniFile.WriteInteger(aSection, 'ProcessingQueue.OverqueueBitrateTresholdPercent', OverqueueBitrateTresholdPercent);
  aIniFile.WriteInteger(aSection, 'ProcessingQueue.OverqueueBitrateMeasureIntervalMs', OverqueueBitrateMeasureIntervalMs);
end;

{ TPtzProtocol }

procedure TPtzProtocol.LoadFromIni(aIniFile: TCustomIniFile; const aSection,
  aPrefix: string);
begin
  self.Enabled:=aIniFile.ReadBool(aSection, aPrefix+'Enabled', false);
  self.ProtocolClassName:=aIniFile.ReadString(aSection, aPrefix+'Implementor', '');
  self.UserName:=aIniFile.ReadString(aSection, aPrefix+'UserName', '');
  self.UserPassword:=aIniFile.ReadString(aSection, aPrefix+'UserPassword', '');
  self.Port:=aIniFile.ReadInteger(aSection, aPrefix+'Port', 80);
end;

procedure TPtzProtocol.SaveToIni(aIniFile: TCustomIniFile; const aSection,aPrefix: string);
begin
  aIniFile.WriteBool(aSection, aPrefix+'Enabled', self.Enabled);
  aIniFile.WriteString(aSection, aPrefix+'Implementor', self.ProtocolClassName);

  aIniFile.WriteString(aSection, aPrefix+'UserName', self.UserName);
  aIniFile.WriteString(aSection, aPrefix+'UserPassword', self.UserPassword);
  aIniFile.WriteInteger(aSection, aPrefix+'Port', self.Port);
end;

{ TPtzStopTranslationWhileMoving }

procedure TPtzStopTranslationWhileMoving.LoadFromIni(aIniFile: TCustomIniFile;
  const aSection, aPrefix: string);
begin
  self.Enabled:=aIniFile.ReadBool(aSection, aPrefix+'Enabled', false);
  self.Period:=aIniFile.ReadInteger(aSection, aPrefix+'Period', 200);
end;

procedure TPtzStopTranslationWhileMoving.SaveToIni(aIniFile: TCustomIniFile;const aSection, aPrefix: string);
begin
  aIniFile.WriteBool(aSection, aPrefix+'Enabled', self.Enabled);
  aIniFile.WriteInteger(aSection, aPrefix+'Period', self.Period);
end;

end.



