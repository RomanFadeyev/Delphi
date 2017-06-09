{ ***********************************<_INFO>************************************ }
{ <Проект>      Видеосервер }
{ }
{ <Область>     16:Медиа-контроль }
{ }
{ <Задача>      Класс, выполнящий связь с камерой и получение от нее потока и }
{ различных событий }
{ }
{ <Автор>       Фадеев Р.В. }
{ }
{ <Дата>        10.12.2008 }
{ }
{ <Примечание>  Нет примечаний. }
{ }
{ <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт" }
{ }
{ ***********************************</_INFO>*********************************** }
unit MediaServer.Stream.Source;

interface

uses Windows, SysUtils, SyncObjs, Classes, ExtCtrls,
  MediaProcessing.Definitions, uBaseClasses,
  MediaServer.Definitions, MediaServer.Obj, Generics.Collections,
  MediaStream.DataSource.Base, MediaStream.Statistics,
  MediaStream.PtzProtocol.Base;

const
  // интервал восстановления аварийно закрытых каналов
  DEF_RECONNECT_INTERVAL: integer = 20 * 1000;
  // Слеующую камеру запускаем не раньше чем через 1 секунду
  DEF_CONNECT_NEXT_INTERVAL = 250;

type
  TMediaServerSource = class;

  // Событие "Пришел очередной кадр". Рассылается в отдельном потоке
  TDataReceivedEvent = procedure(aSender: TMediaServerSource;
    const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
    aInfo: pointer; aInfoSize: cardinal) of object;

  // Событие "произошел таймаут получения данных"
  TDataReceiveTimeoutEvent = procedure(aSender: TMediaServerSource;
    aTimeout: integer) of object;

  // Событие "Смена состояния: Opened/Closed"
  TConnectionOkEvent = procedure(aSender: TMediaServerSource) of object;

  TConnectionFailedEvent = procedure(aSender: TMediaServerSource;
    aError: Exception) of object;

  TClosedEvent = procedure(aSender: TMediaServerSource) of object;

  TReconnectEvent = procedure(aSender: TMediaServerSource; var aAccept: boolean)
    of object;

  TNotificationEvent = procedure(aSender: TMediaServerSource; aArgs: TMediaStreamDataSourceNotificationParams) of object;

  TMediaServerSourceMode = (ssmRealTime, ssmFile);

  TAllMediaStreamStatistics = array [TMediaType] of TMediaStreamStatistics;

  TOpenConnectionOkParams = record
    Stream: TMediaStreamDataSource;
  end;

  POpenConnectionOkParams = ^TOpenConnectionOkParams;

  TOpenConnectionFailedParams = record
    E: Exception;
  end;

  POpenConnectionFailedParams = ^TOpenConnectionFailedParams;

  (*
    TMediaServerSourcePtzCapabilities = (
    ptzcMoving, //Интерактивное движение по сторонам
    ptzcZoom, //Зум
    ptzcPosByCoord, //Возможность указать пози
    ptszGet);
  *)
  // Класс, выполняющий непосредственно получение данных (видеопотока) из камеры
  TMediaServerSource = class
  private
    FOnDataReceived: TDataReceivedEvent;

    FOnDataReceivedTimeout: TDataReceiveTimeoutEvent;
    FOnConnectionOk: TConnectionOkEvent;
    FOnConnectionFailed: TConnectionFailedEvent;
    FOnClosed: TClosedEvent;
    FOnReconnect: TReconnectEvent;
    FOnNotification: TNotificationEvent;

    FForceTimer: TTimer;

    FDestroying: boolean;
    FClosing: boolean;

    FLastCloseDateTime: TDateTime;
    FLastStreamTypes: TAllMediaStreamTypes;

    FStreamLock: TCriticalSection;

    FDataReceiveTimeout: integer; // таймаут получения данных от канала
    // FConnect_ThreadHandle: THandle;
    FTag: integer;

    FDataSinks: TMediaStreamDataMulticast;

    FLastStreamDateTime: TThreadVar<TDateTime>;
    FLastStreamDateTimeTotal: TThreadVar<TDateTime>;

    FLastConnectedDateTime: TDateTime;
    FLastConnectingDateTime: TDateTime;

    FStatistics: TAllMediaStreamStatistics;

    FPingTimer: TTimer;
    FCreationDateTime: TDateTime;

    function GetLastStreamDataTime: TDateTime;

    procedure SetOnDataReceived(const Value: TDataReceivedEvent);
    procedure OnPingTimer(aSender: TObject);
    function GetLastStreamDataTimeTotal: TDateTime;
  protected
    // Статистика. Должна записываться внутри LockStream
    FAFramesReceived: int64;
    FVIFramesReceived: int64;
    FVPFramesReceived: int64;
    FPtzLastCommandDateTime: TThreadVar<TDateTime>;
    FPtzCustomProtocol: TPtzProtocol;

    procedure OnDataReceiveTimeout(aTimeoutMSecs: integer);
    procedure OnCheckTimer;

    procedure CloseInternal;
    procedure DoClose; virtual; abstract;

    procedure StartReconnect;

    procedure DoConnectionOK;
    procedure DoConnectionFailed(E:Exception);

    procedure DoDataReceived(const aFormat: TMediaStreamDataHeader;
      aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal; aRealDataFromSource: boolean=true);

    procedure DoOpen(aSync: boolean); virtual; abstract;

    procedure LockStream;
    procedure UnlockStream;

    function GetStreamType(aMediaType: TMediaType): TStreamType;
      virtual; abstract;

    property Destroying: boolean read FDestroying;
    property Closing: boolean read FClosing;
  public
    constructor Create(aDataReceiveTimeout: integer);
    destructor Destroy; override;

    procedure Open(aSync: boolean);
    procedure Close;

    function Opened: boolean; virtual; abstract;
    function Connecting: boolean; virtual;

    procedure WaitWhileConnecting(aTimeout: integer); virtual;
    procedure CheckConnected;

    // Статистика
    // TODO обернуть все в LockStream
    property AFramesReceived: int64 read FAFramesReceived;
    property VIFramesReceived: int64 read FVIFramesReceived;
    property VPFramesReceived: int64 read FVPFramesReceived;
    property Statistics: TAllMediaStreamStatistics read FStatistics;

    property StreamType[aMediaType: TMediaType]: TStreamType read GetStreamType;

    //Время последнего получения данных
    //Имеет смысл только при наличии соединения. Когда соединения нет, =0
    property LastStreamDataTime: TDateTime read GetLastStreamDataTime;

    //Время последнего получения данных вообще за все время жизни
    //Не сбрасывается при отсоединении
    property LastStreamDataTimeTotal: TDateTime read GetLastStreamDataTimeTotal;

    // Время последнего останова канала
    property LastCloseDataTime: TDateTime read FLastCloseDateTime;

    // Время последнего перехода канала в состояние "Connecting..."
    property LastConnectingDateTime: TDateTime read FLastConnectingDateTime;

    // Время последнего перехода канала в состояние "Connected"
    property LastConnectedDateTime: TDateTime read FLastConnectedDateTime;

    property CreationDateTime: TDateTime read FCreationDateTime;

    // Не потокобезопасное!
    property OnDataReceived: TDataReceivedEvent read FOnDataReceived
      write SetOnDataReceived;

    property OnDataReceivedTimeout: TDataReceiveTimeoutEvent read FOnDataReceivedTimeout write FOnDataReceivedTimeout;
    property OnConnectionOk: TConnectionOkEvent read FOnConnectionOk write FOnConnectionOk;
    property OnConnectionFailed: TConnectionFailedEvent read FOnConnectionFailed write FOnConnectionFailed;
    property OnReconnect: TReconnectEvent read FOnReconnect write FOnReconnect;
    property OnClosed: TClosedEvent read FOnClosed write FOnClosed;
    property OnNotification: TNotificationEvent read FOnNotification write FOnNotification;

    property Tag: integer read FTag write FTag;
    property DataSinks: TMediaStreamDataMulticast read FDataSinks;

    // property  Connect_ThreadHandle: THandle read FConnect_ThreadHandle;

    function Name: string; virtual; abstract;

    function DeviceType: string; virtual; abstract;
    function ConnectionString: string; virtual; abstract;
    function StreamInfo: TBytes; virtual;
    function StateInfo: string; virtual;

    function PtzSupported: boolean; virtual;
    procedure PtzApertureDecrease; virtual;
    procedure PtzApertureDecreaseStop; virtual;
    procedure PtzApertureIncrease; virtual;
    procedure PtzApertureIncreaseStop; virtual;
    procedure PtzFocusIn; virtual;
    procedure PtzFocusInStop; virtual;
    procedure PtzFocusOut; virtual;
    procedure PtzFocusOutStop; virtual;
    procedure PtzMoveDown(aSpeed: byte); virtual;
    procedure PtzMoveDownStop; virtual;
    procedure PtzMoveLeft(aSpeed: byte); virtual;
    procedure PtzMoveLeftStop; virtual;
    procedure PtzMoveRight(aSpeed: byte); virtual;
    procedure PtzMoveRightStop; virtual;
    procedure PtzMoveUp(aSpeed: byte); virtual;
    procedure PtzMoveUpStop; virtual;
    procedure PtzZoomIn; virtual;
    procedure PtzZoomInStop; virtual;
    procedure PtzZoomOut; virtual;
    procedure PtzZoomOutStop; virtual;

    // ===== Перемещение на заданную позицию
    // Движение на указанную точку-пресет
    procedure PtzMoveToPoint(aId: cardinal); virtual;

    // Движение в указанную позицию. Позиция указывается по оси X и Y в градусах
    procedure PtzMoveToPosition(const aPositionPan, aPositionTilt
      : double); virtual;

    // Статистика PTZ
    function PtzLastCommandDateTime: TDateTime; virtual;
    procedure SetCustomPtz(aPtz: TPtzProtocol);

    class procedure Start;
    class procedure Stop;
  end;

  TMediaServerSourceBasedOnMediaStream = class(TMediaServerSource)
  private
    FLock: TCriticalSection;
    FStream: TMediaStreamDataSource;

    FConnectParams: TMediaStreamDataSourceConnectParams;
    FStreamClass: TMediaStreamDataSourceClass;

    FTransmitAudio: boolean; // Записывать ли аудио
    FConnect_ThreadHandle: THandle;
    FConnect_Lock: TCriticalSection;

    procedure PtzInit;
    procedure FreeStream;
  protected
    procedure OnConnectionOKThreaded(aParams: POpenConnectionOkParams); virtual;
    procedure OnConnectionOKSync(aParams: POpenConnectionOkParams); virtual;
    procedure OnConnectionFailedSync(aParams: pointer); virtual;

    procedure OnDataSourceData(Sender: TMediaStreamDataSource;
      const aFormat: TMediaStreamDataHeader; aData: pointer;
      aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); virtual;


    procedure OnDataSourceNotification(Sender: TMediaStreamDataSource; const aData: TMediaStreamDataSourceNotificationParams);

    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
    function CanFreeStreamAsync: boolean; virtual;
  public
    constructor Create(const aConnectionParams
      : TMediaStreamDataSourceConnectParams;
      aStreamClass: TMediaStreamDataSourceClass; aTransmitAudio: boolean;
      // Записывать ли аудио
      aDataReceiveTimeout: integer // таймаут получения данных от канала
      ); overload;

    destructor Destroy; override;

    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function Opened: boolean; override;
    function Connecting: boolean; override;

    property Stream: TMediaStreamDataSource read FStream;
    property ConnectParams: TMediaStreamDataSourceConnectParams read FConnectParams;

    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    procedure PtzApertureDecrease; override;
    procedure PtzApertureDecreaseStop; override;
    procedure PtzApertureIncrease; override;
    procedure PtzApertureIncreaseStop; override;
    procedure PtzFocusIn; override;
    procedure PtzFocusInStop; override;
    procedure PtzFocusOut; override;
    procedure PtzFocusOutStop; override;
    procedure PtzMoveDown(aSpeed: byte); override;
    procedure PtzMoveDownStop; override;
    procedure PtzMoveLeft(aSpeed: byte); override;
    procedure PtzMoveLeftStop; override;
    procedure PtzMoveRight(aSpeed: byte); override;
    procedure PtzMoveRightStop; override;
    procedure PtzMoveUp(aSpeed: byte); override;
    procedure PtzMoveUpStop; override;
    procedure PtzZoomIn; override;
    procedure PtzZoomInStop; override;
    procedure PtzZoomOut; override;
    procedure PtzZoomOutStop; override;

    // ===== Перемещение на заданную позицию
    // Движение на указанную точку-пресет
    procedure PtzMoveToPoint(aId: cardinal); override;
    // Движение в указанную позицию. Позиция указывается по оси X и Y в градусах
    procedure PtzMoveToPosition(const aPositionPan, aPositionTilt
      : double); override;

  end;

  // TODO вынести либо на интерфейс, либо воткнуть как методы в основной класс. Не клеится из-за необходимости множественного наследования
  TMediaServerSourceFinite = class(TMediaServerSource)
  public
    // msec
    function Duration: integer; virtual; abstract;
    // msec
    function Position: integer; virtual; abstract;

    function Finished: boolean; virtual; abstract;
  end;

  TMediaServerSourceLastCommand = (rslcNone, rslcStart, rslcStop);

var
  gLastCommand: TMediaServerSourceLastCommand;

implementation

uses Math, Forms, MediaServer.Workspace, uTrace, uSync, Patterns.AppGuard, ThreadNames;

type
  TMediaServerSourceRegistry = class
  private
    FCheckTimer: TTimer;
    FReconnectTimer: TTimer;

    FRecordSources: TList;
    FRecordSourcesToReconnect: TList;

    procedure OnCheckTimer(Sender: TObject);
    procedure OnReconnectTimer(Sender: TObject);

    function GeTMediaServerSource(index: integer): TMediaServerSource;
  public
    procedure Add(aRecordSource: TMediaServerSource);
    procedure Remove(aRecordSource: TMediaServerSource);

    procedure AddToReconnectQueue(aRecordSource: TMediaServerSource);
    procedure RemoveFromReconnectQueue(aRecordSource: TMediaServerSource);

    function RecordSourceCount: integer;
    property RecordSources[index: integer]: TMediaServerSource
      read GeTMediaServerSource;

    constructor Create;
    destructor Destroy; override;
  end;

var
  gRegistry: TMediaServerSourceRegistry;

function RecordSourceRegistry: TMediaServerSourceRegistry;
begin
  if gRegistry = nil then
    gRegistry := TMediaServerSourceRegistry.Create;

  result := gRegistry;
end;

{ TMediaServerSourceRegistry }

constructor TMediaServerSourceRegistry.Create;
begin
  FRecordSources := TList.Create;
  FRecordSourcesToReconnect := TList.Create;

  FCheckTimer := TTimer.Create(nil);
  FCheckTimer.Interval := 500;
  FCheckTimer.OnTimer := OnCheckTimer;
  FCheckTimer.Enabled := true;

  FReconnectTimer := TTimer.Create(nil);
  FReconnectTimer.Interval := DEF_RECONNECT_INTERVAL;
  FReconnectTimer.OnTimer := OnReconnectTimer;
  FReconnectTimer.Enabled := true;
end;

destructor TMediaServerSourceRegistry.Destroy;
begin
  FreeAndNil(FCheckTimer);
  FreeAndNil(FReconnectTimer);
  FreeAndNil(FRecordSources);
  FreeAndNil(FRecordSourcesToReconnect);
  inherited;
end;

procedure TMediaServerSourceRegistry.Add(aRecordSource: TMediaServerSource);
begin
  if (FRecordSources.IndexOf(aRecordSource) <> -1) then
    Remove(aRecordSource);
  FRecordSources.Add(aRecordSource);
end;

procedure TMediaServerSourceRegistry.Remove(aRecordSource: TMediaServerSource);
begin
  FRecordSources.Remove(aRecordSource);
end;

function TMediaServerSourceRegistry.GeTMediaServerSource(index: integer)
  : TMediaServerSource;
begin
  result := TMediaServerSource(FRecordSources[index]);
end;

function TMediaServerSourceRegistry.RecordSourceCount: integer;
begin
  result := FRecordSources.Count;
end;

procedure TMediaServerSourceRegistry.OnCheckTimer(Sender: TObject);
const
  aMethodName = 'TMediaServerSourceRegistry.OnCheckTimer';
var
  i: integer;
begin
  for i := RecordSourceCount - 1 downto 0 do
  begin
    try
      RecordSources[i].OnCheckTimer;
    except
      on E: Exception do
        Workspace.HandleException(self, E, aMethodName);
    end
  end;
end;

procedure TMediaServerSourceRegistry.OnReconnectTimer(Sender: TObject);
const
  aMethodName = 'TMediaServerSourceRegistry.OnReconnectTimer';
var
  aRS: TMediaServerSource;
  aRSs: array of TMediaServerSource;
  i: integer;
begin
  if FRecordSourcesToReconnect.Count = 0 then
    exit;

  FReconnectTimer.Enabled := false;
  try
    SetLength(aRSs, FRecordSourcesToReconnect.Count);
    for i := 0 to FRecordSourcesToReconnect.Count - 1 do
      aRSs[i] := FRecordSourcesToReconnect[i];

    FRecordSourcesToReconnect.Clear;
    for i := 0 to High(aRSs) do
    begin
      try
        aRS := aRSs[i];
        if FRecordSources.IndexOf(aRS) <> -1 then
          if not aRS.Opened then
            aRS.Open(false);
      except
        on E: Exception do
          Workspace.HandleException(self, E, aMethodName);
      end
    end;
  finally
    FReconnectTimer.Enabled := true;
  end;
end;

procedure TMediaServerSourceRegistry.AddToReconnectQueue(aRecordSource
  : TMediaServerSource);
begin
  RemoveFromReconnectQueue(aRecordSource);
  FRecordSourcesToReconnect.Add(aRecordSource);
end;

procedure TMediaServerSourceRegistry.RemoveFromReconnectQueue
  (aRecordSource: TMediaServerSource);
begin
  FRecordSourcesToReconnect.Remove(aRecordSource);
end;

{ TMediaServerSource }

constructor TMediaServerSource.Create(aDataReceiveTimeout: integer);
var
  aMediaType: TMediaType;
begin
  inherited Create;

  FCreationDateTime:=Now;
  FLastStreamDateTime:= TThreadVar<TDateTime>.Create;
  FLastStreamDateTimeTotal:= TThreadVar<TDateTime>.Create;
  FPtzLastCommandDateTime := TThreadVar<TDateTime>.Create;
  FStreamLock := TCriticalSection.Create;
  for aMediaType := low(TMediaType) to High(TMediaType) do
    FStatistics[aMediaType] := TMediaStreamStatistics.Create;

  FDataReceiveTimeout := aDataReceiveTimeout;
  FDataSinks := TMediaStreamDataMulticast.Create;

  FPingTimer:=TTimer.Create(nil);
  FPingTimer.OnTimer:=OnPingTimer;
  FPingTimer.Enabled:=false;

  // Регистриуемся в общем списке
  RecordSourceRegistry.Add(self);
end;

destructor TMediaServerSource.Destroy;
var
  aMediaType: TMediaType;
begin
  RecordSourceRegistry.Remove(self);
  FDestroying := true;
  try
    Close;
  except
    // TODO
  end;

  FreeAndNil(FPingTimer);
  FreeAndNil(FForceTimer);
  FreeAndNil(FStreamLock);
  for aMediaType := low(TMediaType) to High(TMediaType) do
    FreeAndNil(FStatistics[aMediaType]);
  FreeAndNil(FDataSinks);
  FreeAndNil(FPtzLastCommandDateTime);
  FreeAndNil(FPtzCustomProtocol);
  FreeAndNil(FLastStreamDateTime);
  FreeAndNil(FLastStreamDateTimeTotal);
  inherited;
end;

procedure TMediaServerSource.DoConnectionFailed(E: Exception);
begin
  if Assigned(OnConnectionFailed) then
    OnConnectionFailed(self, E);
end;

procedure TMediaServerSource.DoConnectionOK;
begin
  FLastStreamDateTime.Value := Now;
  FLastConnectedDateTime := Now;
  FPingTimer.Enabled:=true;
  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSource.DoDataReceived(const aFormat
  : TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal; aRealDataFromSource: boolean);
var
  aNow: TDateTime;
begin
  if aRealDataFromSource then
  begin
    LockStream;
    try
      // Если не нужно записывать аудио данные, то выходим
      // TODO AUDIO
      // if (aFormat.=dtAudio) then
      // exit; //TODO доработать когда появиться поддержка аудио

      // if not FTransmitAudio and (aFormat.DataType=dtAudio) then
      // exit;

      FStatistics[aFormat.biMediaType].AddData(aDataSize, aInfoSize);

      if aFormat.biMediaType = mtVideo then
      begin
        if ffKeyFrame in aFormat.biFrameFlags then
          inc(FVIFramesReceived)
        else
          inc(FVPFramesReceived);
      end;


      aNow:=Now;
      FLastStreamDateTime.Value := aNow;
      FLastStreamDateTimeTotal.Value:=aNow;
      FLastStreamTypes[aFormat.biMediaType] := aFormat.biStreamType;
    finally
      UnlockStream;
    end;
  end;

  // TODO сделать LOck на FOnDataReceived
  if Assigned(FOnDataReceived) then
    FOnDataReceived(self, aFormat, aData, aDataSize, aInfo, aInfoSize);

  Assert(FDataSinks<>nil);
  FDataSinks.OnData(aFormat, aData, aDataSize, aInfo, aInfoSize);
end;

function TMediaServerSource.GetLastStreamDataTime: TDateTime;
begin
  CheckConnected;
  result := FLastStreamDateTime.Value;
end;

function TMediaServerSource.GetLastStreamDataTimeTotal: TDateTime;
begin
  result := FLastStreamDateTimeTotal.Value;
end;

procedure TMediaServerSource.CloseInternal;
const
  aMethodName = 'TMediaServerSource.CloseInternal';
var
  aTraceID: cardinal;
begin
  aTraceID := TraceProcBegin(aMethodName, ConnectionString);
  try
    FClosing := true;
    try
      FPingTimer.Enabled:=false;
      // Удалим себя из списка очередников на Reconnect
      RecordSourceRegistry.RemoveFromReconnectQueue(self);

      DoClose;

      FLastStreamDateTime.Value := 0;
      FLastCloseDateTime := Now;
    finally
      FClosing := false;
    end;
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

function TMediaServerSource.Connecting: boolean;
begin
  result:=false;
end;

procedure TMediaServerSource.Close;
const
  aMethodName = 'TMediaServerSource.Close';
var
  aTraceID: cardinal;
  aConnected: boolean;
begin
  aTraceID := TraceProcBegin(aMethodName, ConnectionString);
  try
    aConnected := Opened;
    CloseInternal;
    if aConnected then
      if not FDestroying then
        if Assigned(FOnClosed) then
          FOnClosed(self);
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

procedure TMediaServerSource.StartReconnect;
var
  aReconnect: boolean;
begin
  aReconnect := true;
  if Assigned(FOnReconnect) then
    FOnReconnect(self, aReconnect);

  if aReconnect then
    RecordSourceRegistry.AddToReconnectQueue(self);
end;

function TMediaServerSource.StateInfo: string;
begin
  result := '';
end;

procedure TMediaServerSource.CheckConnected;
begin
  if not Opened then
    raise Exception.Create('Устройство не доступно');
end;

procedure TMediaServerSource.OnCheckTimer;
var
  aDeltaMsecs: integer;
begin
  // Проверим, а нет ли таймаута от канала
  if (Opened) and (FLastStreamDateTime.Value <> 0) and (FDataReceiveTimeout > 0) then
  begin
    aDeltaMsecs := Trunc((Now - FLastStreamDateTime.Value) * MSecsPerDay);
    if aDeltaMsecs > FDataReceiveTimeout then
    begin
      OnDataReceiveTimeout(aDeltaMsecs);
    end;
  end;
end;

procedure TMediaServerSource.LockStream;
begin
  FStreamLock.TryEnterOrRaise(10 * 1000);
end;

procedure TMediaServerSource.UnlockStream;
begin
  FStreamLock.Leave;
end;

procedure TMediaServerSource.WaitWhileConnecting(aTimeout: integer);
begin

end;

procedure TMediaServerSource.SetCustomPtz(aPtz: TPtzProtocol);
begin
  FreeAndNil(FPtzCustomProtocol);
  FPtzCustomProtocol:=aPtz;
end;

procedure TMediaServerSource.SetOnDataReceived(const Value: TDataReceivedEvent);
begin
  LockStream;
  try
    FOnDataReceived := Value;
  finally
    UnlockStream;
  end;
end;

procedure TMediaServerSource.OnDataReceiveTimeout(aTimeoutMSecs: integer);
const
  aMethodName = 'TMediaServerSource.OnDataReceiveTimeout';
var
  aTraceID: cardinal;
begin
  aTraceID := TraceProcBegin(aMethodName, ConnectionString);
  try
    if (Assigned(FOnDataReceivedTimeout)) then
      FOnDataReceivedTimeout(self, aTimeoutMSecs);
    try
      Close;
    finally
      StartReconnect;
    end;
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

procedure TMediaServerSource.PtzApertureDecrease;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzApertureDecreaseStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzApertureIncrease;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzApertureIncreaseStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzFocusIn;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzFocusInStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzFocusOut;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzFocusOutStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

function TMediaServerSource.PtzLastCommandDateTime: TDateTime;
begin
  result := FPtzLastCommandDateTime.Value;
end;

procedure TMediaServerSource.PtzMoveDown(aSpeed: byte);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveDownStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveLeft(aSpeed: byte);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveLeftStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveRight(aSpeed: byte);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveRightStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveToPoint(aId: cardinal);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveToPosition(const aPositionPan,
  aPositionTilt: double);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveUp(aSpeed: byte);
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzMoveUpStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

function TMediaServerSource.PtzSupported: boolean;
begin
  result := false;
end;

procedure TMediaServerSource.PtzZoomIn;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzZoomInStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzZoomOut;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

procedure TMediaServerSource.PtzZoomOutStop;
begin
  FPtzLastCommandDateTime.Value := Now;
end;

class procedure TMediaServerSource.Start;
begin
  gLastCommand := rslcStart;
end;

class procedure TMediaServerSource.Stop;
begin
  gLastCommand := rslcStop;
end;

function TMediaServerSource.StreamInfo: TBytes;
begin
  result := nil;
end;

procedure TMediaServerSource.OnPingTimer(aSender: TObject);
var
  aFormat: TMediaStreamDataHeader;
  aData: TPingArgs;
begin
  aFormat.Clear;
  aFormat.biMediaType:=mtSysData;
  aFormat.biStreamType:=stPING;
  aFormat.TimeStamp:=GetTickCount;

  aData.Reserved:=0;
  if Opened then
    DoDataReceived(aFormat,@aData,sizeof(aData),nil,0,false);
end;

procedure TMediaServerSource.Open(aSync: boolean);
begin
  if Opened or Connecting then
    exit;

  Close;
  FLastConnectingDateTime:=Now;
  DoOpen(aSync);
end;

{ TMediaServerSourceBasedOnMediaStream }

function TMediaServerSourceBasedOnMediaStream.CanFreeStreamAsync: boolean;
begin
  result:=true;
end;

function TMediaServerSourceBasedOnMediaStream.Connecting: boolean;
begin
  result:=FConnect_ThreadHandle<>0;
end;

constructor TMediaServerSourceBasedOnMediaStream.Create(const aConnectionParams
  : TMediaStreamDataSourceConnectParams;
  aStreamClass: TMediaStreamDataSourceClass; aTransmitAudio: boolean;
  // Записывать ли аудио
  aDataReceiveTimeout: integer // таймаут получения данных от канала
  );
var
  aStream: TMediaStreamDataSource;
  i: TMediaType;
begin
  FLock := TCriticalSection.Create;
  // В первую очередь создадим объект синхронизации
  FConnect_Lock := TCriticalSection.Create;

  inherited Create(aDataReceiveTimeout);

  FTransmitAudio := aTransmitAudio;
  FConnectParams := aConnectionParams;
  FStreamClass := aStreamClass;

  aStream := FStreamClass.Create;
  try
    for i := Low(TMediaType) to High(TMediaType) do
      FLastStreamTypes[i] := aStream.StreamType[i];
  finally
    aStream.Free;
  end;
end;

destructor TMediaServerSourceBasedOnMediaStream.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
  FreeAndNil(FConnect_Lock);
  FreeAndNil(FConnectParams);
end;

procedure TMediaServerSourceBasedOnMediaStream.OnConnectionFailedSync
  (aParams: pointer);
begin
  // Добавим себя в список очередников на повторное соединение
  StartReconnect;
  DoConnectionFailed(POpenConnectionFailedParams(aParams).E);
end;

procedure TMediaServerSourceBasedOnMediaStream.OnConnectionOKSync
  (aParams: POpenConnectionOkParams);
var
  i: TMediaType;
begin
  FreeStream;
  // На всякий случай надо убедиться что нет текущего подключения
  FStream := POpenConnectionOkParams(aParams).Stream;

  for i := Low(TMediaType) to High(TMediaType) do
    FLastStreamTypes[i] := FStream.StreamType[i];

  DoConnectionOK;
  Assert(FStream <> nil);

  if FPtzCustomProtocol<>nil then
    FStream.SetCustomPtz(FPtzCustomProtocol);

  FStream.OnNotification:=OnDataSourceNotification;
  // Если все успешно открылось, привешиваем обработчик на данные из канала
  // Это нужно делать только после инициализации всего, чтобы данные поступали к нам, когда
  // мы целиком инициализированы
  FStream.OnData.Add(OnDataSourceData);
  FStream.Start;
end;

procedure TMediaServerSourceBasedOnMediaStream.OnConnectionOKThreaded
  (aParams: POpenConnectionOkParams);
begin

end;

function ThreadConnectionProc(aSource: TMediaServerSourceBasedOnMediaStream): integer;
const
  aMethodName = 'ThreadConnectionProc';
type
  TOkSynchandler = procedure(aParams: POpenConnectionOkParams) of object;
var
  aOpenOk: TOpenConnectionOkParams;
  aOpenFailed: TOpenConnectionFailedParams;
  aOkSyncHandler: TOkSynchandler;
begin
  result := -1;

  if not(aSource.Destroying or aSource.Closing) then
    if (gLastCommand = rslcStart) then
      if not Application.Terminated then
      begin
        result := 0;
      end;

  if result <> 0 then
    exit;

  ZeroMemory(@aOpenOk, sizeof(aOpenOk));

  // Одновременно может выполняться только одно подключение, поэтому блокируем
  // повторные запросы
  aSource.FConnect_Lock.Enter;
  try
    // открываемся
    try
      // открываем канал
      aOpenOk.Stream := aSource.FStreamClass.Create;
      aOpenOk.Stream.Connect(aSource.FConnectParams);
    except
      on E: Exception do
      begin
        FreeAndNil(aOpenOk.Stream);
        aOpenFailed.E := E;
        Sync.Synchronize(aSource.OnConnectionFailedSync, @aOpenFailed);
      end;
    end;

    if aOpenOk.Stream <> nil then
    begin
      try
        aSource.OnConnectionOKThreaded(@aOpenOk);
      except
        on E:Exception do
          Workspace.HandleException(nil,E,aMethodName);
      end;

      aOkSyncHandler := aSource.OnConnectionOKSync;
      try
        Sync.Synchronize(TSynchronizeObjectProcedure1(aOkSyncHandler), @aOpenOk);
      except
        on E:Exception do
          Workspace.HandleException(nil,E,aMethodName);
      end;
    end;
  finally
    aSource.FConnect_ThreadHandle := 0;
    aSource.FConnect_Lock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.OnDataSourceData
  (Sender: TMediaStreamDataSource; const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if not Sender.Connected then
    exit;
  if (aFormat.biMediaType = mtAudio) and not FTransmitAudio then
    exit;

  DoDataReceived(aFormat, aData, aDataSize, aInfo, aInfoSize,true);
end;

procedure TMediaServerSourceBasedOnMediaStream.OnDataSourceNotification(
  Sender: TMediaStreamDataSource;
  const aData: TMediaStreamDataSourceNotificationParams);
var
  aFormat: TMediaStreamDataHeader;
  aData2: TNotificationArgs;
begin
  if not Sender.Connected then
    exit;

  aFormat.Clear;
  aFormat.biMediaType:=mtSysData;
  aFormat.biStreamType:=stNotification;
  aFormat.TimeStamp:=GetTickCount;

  aData2.Notification:=cardinal(aData.Notification);
  aData2.SourceId:=aData.SourceId;

  DoDataReceived(aFormat,@aData2,sizeof(aData2),nil,0,true);

  if Assigned(FOnNotification) then
    FOnNotification(self, aData);
end;

procedure TMediaServerSourceBasedOnMediaStream.DoOpen(aSync: boolean);
var
  aThreadID: cardinal;
begin
  if Opened or (FConnect_ThreadHandle <> 0) then
    exit;

  Close;

  if aSync then
    ThreadConnectionProc(self)
  else
  begin
    if FConnect_ThreadHandle = 0 then
    // Если <>0, значит, уже выполняется подключение. Возможно в многопоточной среде
      FConnect_ThreadHandle := BeginThread(nil, 0, @ThreadConnectionProc, self,
        0, aThreadID);
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.DoClose;
const
  aMethodName = 'TMediaServerSourceBasedOnMediaStream.DoClose';
var
  aTraceID: cardinal;
begin
  aTraceID := TraceProcBegin(aMethodName, ConnectionString);
  try
    if FLock = nil then
      exit; // Вызывается из деструктора, и если не было нормальной инициализации, может быть nil

    // Вычистим очередь
    if FConnect_ThreadHandle <> 0 then
    begin
      TraceLine('Source is still connecting (FConnect_ThreadHandle<>0)');
      Sync.PeekSynchronizationMessages;
      Sync.WaitWhileSynchronizationMessagesProcessed(FConnect_ThreadHandle);
      FConnect_ThreadHandle := 0;
    end;

    FreeStream;
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

function ThreadFreeStreamProc(aStream: TMediaStreamDataSource): integer;
const
  aMethodName = 'ThreadFreeStreamProc';
begin
  SetCurrentThreadName('ThreadFreeStreamProc for '+aStream.ConnectionString);
  try
    aStream.Free;
  except
    on E:Exception do
      Workspace.HandleException(nil,E,aMethodName);
  end;
  result:=0;
end;

procedure TMediaServerSourceBasedOnMediaStream.FreeStream;
const
  aMethodName = 'TMediaServerSourceBasedOnMediaStream.FreeStream';
var
  aTraceID: cardinal;
  aThreadID: cardinal;
begin
  aTraceID := TraceProcBegin(aMethodName, ConnectionString);
  try
    FLock.Enter;
    try
      if FStream<>nil then
      begin
        FStream.Stop;

        if (not CanFreeStreamAsync) or (Application.Terminated) or (IsApplicationShutdowning) or (FDestroying) then
        begin
          FStream.Disconnect;
          FreeAndNil(FStream);
        end
        else begin
          FStream.OnNotification:=OnDataSourceNotification;
          FStream.OnData.Remove(OnDataSourceData);
          BeginThread(nil, 0, @ThreadFreeStreamProc, FStream, 0, aThreadID);
          FStream:=nil;
        end;
      end;
    finally
      FLock.Leave;
    end;
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

function TMediaServerSourceBasedOnMediaStream.Opened: boolean;
begin
  FLock.Enter;
  try
    result := (FStream <> nil) and (FStream.Connected)
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceBasedOnMediaStream.StreamInfo: TBytes;
begin
  result := nil;
end;

function TMediaServerSourceBasedOnMediaStream.GetStreamType
  (aMediaType: TMediaType): TStreamType;
begin
  result := FLastStreamTypes[aMediaType];
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzApertureDecrease;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzApertureDecrease(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzApertureDecreaseStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzApertureDecreaseStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzApertureIncrease;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzApertureIncrease(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzApertureIncreaseStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzApertureIncreaseStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzFocusIn;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzFocusIn(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzFocusInStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzFocusInStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzFocusOut;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzFocusOut(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzFocusOutStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzFocusOutStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzInit;
begin
  CheckConnected;

  FLock.Enter;
  try
    FStream.PtzInit;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveDown(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveDown(0, aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveDownStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveDownStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveLeft(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveLeft(0, aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveLeftStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveLeftStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveRight(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveRight(0, aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveRightStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveRightStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveToPoint(aId: cardinal);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveToPoint(aId);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveToPosition
  (const aPositionPan, aPositionTilt: double);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveToPosition(aPositionPan, aPositionTilt);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveUp(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveUp(0, aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzMoveUpStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveUpStop;
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceBasedOnMediaStream.PtzSupported: boolean;
begin
  FLock.Enter;
  try
    result := (FStream <> nil) and (FStream.PtzSupported);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzZoomIn;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzZoomIn(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzZoomInStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzZoomInStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzZoomOut;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzZoomOut(0);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.PtzZoomOutStop;
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzZoomOutStop;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceBasedOnMediaStream.WaitWhileConnecting
  (aTimeout: integer);
begin
  inherited;
  WaitForSingleObject(FConnect_ThreadHandle, aTimeout);
end;

initialization

RecordSourceRegistry;

finalization

FreeAndNil(gRegistry);

end.
