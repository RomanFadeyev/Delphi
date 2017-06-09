unit MediaServer.Definitions;

interface
  uses SysUtils, Classes, Generics.Collections, SyncObjs,
  MediaProcessing.Definitions,MediaServer.Net.Definitions,RTSP.Definitions;

type
  TMediaStreamWorkingConditions = (mscNormal,mscUnauthorizedUser,mscBlockedUser,mscDisconnected,mscInsufficientPrivileges, mscOverqueue);

  TForwarderDataInfo = record
    SkippedFrameCount: cardinal;
    SkippedFrameSize: cardinal
  end;


  IMediaStreamForwarder = interface
  ['{A32EC10F-BE08-4C1B-80C7-30A75F2CBFC5}']
    procedure OnData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal; const aDataInfo: TForwarderDataInfo);

    procedure Shutdown;

    function TypeName: string;
    function LastResponse: TDateTime;
    function DestinationDescription: string;
    function Remarks: string;

    //Сколько данных отправлено
    function DataSent: int64;
  end;

  IMediaStreamForwarderSyncExtension = interface
  ['{5AFC3317-75EC-4A56-BFEA-AB8BA0492DAE}']
    function OnSync(aCurrentQueueSize,aCurrentQueueLength, aMaxQueueSize, aMaxQueueLength,aMaxQueueDuration: cardinal; out aDelayInFrames,aDelayInMs, aDelayInBytes: int64):boolean;
  end;

  IMediaStreamTransport = interface
    procedure Send(ABuffer: pointer; aBufferSize: integer; out aAck: boolean);
    procedure SendBytes(ABuffer: TBytes; out aAck: boolean);
    function  DestinationDescription: string;
    function  Name: string;

    procedure Shutdown;
  end;

  IMediaStreamTransportReadExtension = interface
  ['{0B85CB65-7FED-41E8-B064-345EACB08829}']
    procedure ReadBytes(var ABuffer: TBytes);
  end;



  IRtspMediaStreamTransport = IMediaStreamTransport;

  IRtspMediaStreamForwarder = interface (IMediaStreamForwarder)
  ['{C16FBF1C-82F9-486C-9503-A47CC0D315C1}']
  end;

  I3SMediaStreamForwarder = interface (IMediaStreamForwarder)
  ['{1FE4AD3B-A081-4EE5-9FA6-6695500C8890}']
    procedure SetDataTransport(const aTransport: IMediaStreamTransport);
    procedure SetSyncTransport(const aSocket: THandle);
  end;

  IMscpMediaStreamForwarder = interface (IMediaStreamForwarder)
  ['{241532F3-DA23-46BE-804C-AC252ACDC878}']
    //Получить снимок в формате BMP для универсальности данные передаются в виде потока
    //Чтобы загрузить из в Bitmap, можно сделать TBitmap.LoadFromStream
    procedure GetSnapshot(aStream: TStream);

    function  DataReadyEventHandle: THandle;
  end;

  IRelayMediaStreamForwarder = interface (IMediaStreamForwarder)
  ['{2DEB1957-6EDE-4C9C-91C5-9E9F8354342E}']
  end;

  IMediaStreamDataSink = interface
    procedure OnData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
  end;
  IMediaStreamDataSinkArray = array of IMediaStreamDataSink;

  TUserInfo = record
    UserName : string;
    Password: string;
    IsBlocked: boolean;
    IsUnauthorized: boolean;
    InsufficientPrivileges: boolean; //Для данного пользователя запрошенный источник не доступен
  end;


  EMediaServerCommandError = class (Exception);

  EMediaServerCommandError_WrongLogin = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_WrongSourceName = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_WrongSourceIndex = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_WrongStreamHandle = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_NeedAuthentication = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_WrongUserNameOrPassword = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_SourceUnavailable = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_UnsupportedMediaType = class (EMediaServerCommandError)
    constructor Create;
  end;

  EMediaServerCommandError_ArchiveDisabled = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_ArchiveSourceNotFound = class (EMediaServerCommandError)
  public
    constructor Create;
  end;


  EMediaServerCommandError_ArchiveUnavailable = class (EMediaServerCommandError)
  public
    constructor Create(const aInnerExceptionMessage: string);
  end;

  EMediaServerCommandError_ArchiveQueryIsEmpty = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_UserBlocked = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_SourceInaccessibleForUser = class (EMediaServerCommandError)
  public
    constructor Create;
  end;

  EMediaServerCommandError_TimedOut = class (EMediaServerCommandError)
  public
    constructor Create; overload;
    constructor Create(const aMessage: string); overload;
  end;

  EMediaServerCommandError_NotAppropriateState = class (EMediaServerCommandError)
  public
    constructor Create(const aCurrentState: string);
  end;



implementation

{ EMediaServerCommandError_WrongSourceName }

constructor EMediaServerCommandError_WrongSourceName.Create;
begin
  inherited Create('Неверное имя устройства');
end;

{ EMediaServerCommandError_WrongLogin }

constructor EMediaServerCommandError_WrongLogin.Create;
begin
  inherited Create('Неверный логин');
end;

{ EMediaServerCommandError_Index }

constructor EMediaServerCommandError_WrongSourceIndex.Create;
begin
  inherited Create('Неверный индекс устройства');
end;

{ EMediaServerCommandError_WrongUserNameOrPassword }

constructor EMediaServerCommandError_WrongUserNameOrPassword.Create;
begin
  inherited Create('Неверное имя пользователя или пароль');
end;

{ EMediaServerCommandError_NeedAuthentication }

constructor EMediaServerCommandError_NeedAuthentication.Create;
begin
  inherited Create('Необходима аутентификация');
end;

{ EMediaServerCommandError_WrongStreamHandle }

constructor EMediaServerCommandError_WrongStreamHandle.Create;
begin
  inherited Create('Неверный дескриптор потока');
end;

{ EMediaServerCommandError_UnsupportedMediaType }

constructor EMediaServerCommandError_UnsupportedMediaType.Create;
begin
  inherited Create('Неподдерживаемый тип потока');
end;

{ EMediaServerCommandError_ArchiveDisabled }

constructor EMediaServerCommandError_ArchiveDisabled.Create;
begin
  inherited Create('Функция архива не подключена');
end;

{ EMediaServerCommandError_ArchiveUnavailable }

constructor EMediaServerCommandError_ArchiveUnavailable.Create(
  const aInnerExceptionMessage: string);
begin
  inherited CreateFmt('Не удалось подключиться к архиву: %s',[aInnerExceptionMessage]);
end;

{ EMediaServerCommandError_ArchiveQueryIsEmpty }

constructor EMediaServerCommandError_ArchiveQueryIsEmpty.Create;
begin
  inherited Create('За указанный период в архиве нет сведений');
end;


{ EMediaServerCommandError_UserBlocked }

constructor EMediaServerCommandError_UserBlocked.Create;
begin
  inherited Create('Пользователь заблокирован');
end;

{ EMediaServerCommandError_TimedOut }

constructor EMediaServerCommandError_TimedOut.Create;
begin
  inherited Create('Таймаут выполнения операции');
end;

constructor EMediaServerCommandError_TimedOut.Create(const aMessage: string);
begin
  inherited Create(aMessage);
end;

{ EMediaServerCommandError_SourceInaccessibleForUser }

constructor EMediaServerCommandError_SourceInaccessibleForUser.Create;
begin
  inherited Create('Источник для данного пользователя не доступен');
end;

{ EMediaServerCommandError_NotWorking }

constructor EMediaServerCommandError_NotAppropriateState.Create(const aCurrentState: string);
begin
  inherited CreateFmt('Сервер находится в состоянии %s',[aCurrentState]);
end;

{ EMediaServerCommandError_ArchiveSourceNotFound }

constructor EMediaServerCommandError_ArchiveSourceNotFound.Create;
begin
  inherited Create('Указанный источник архива не найден');
end;

{ EMediaServerCommandError_SourceUnavailable }

constructor EMediaServerCommandError_SourceUnavailable.Create;
begin
  inherited Create('Устройство не доступно для подключения');
end;

end.



