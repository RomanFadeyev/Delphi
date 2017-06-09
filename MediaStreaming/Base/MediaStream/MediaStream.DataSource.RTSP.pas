{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Источник медиа-потока, обеспечивающий получение данных через  }
{                протокол RTSP                                                 }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        14.01.2011                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaStream.DataSource.RTSP;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base, MediaProcessing.Definitions,
       RTSP.Definitions,RTSP.Client,RTSP.MediaSession,RTP.StreamFramer,uBaseClasses;

type
  TAckAsyncThread = class;

  TRTSPClientThreadSafeContainer = class
  strict private
    FClient: TRTSPClient;
    FLock: TSRWLock;

    function  ClientSafe: TRTSPClient;
  public
    procedure FreeClient;
    procedure CreateClient(const aUrl: string; const aUserAgentName: string);
    function  IsClientCreated: boolean;

    function  Connected: boolean;
    procedure Disconnect;

    procedure SendAlive;
    procedure SendCommandOptions(out aAvailableCommands: string);
    procedure SendCommandDescribe(var aSdpDescription: string);
    procedure SendCommandSetup(aMediaSubSession: TRTSPMediaSubSession);
    procedure SendCommandPlay(aMediaSession: TRTSPMediaSession);
    procedure SendCommandTearDown(aMediaSession: TRTSPMediaSession; aCheckResponce: boolean);

    function  CreateMediaSession(const aSdpDescription: string; aDesiredProtocol: TRtpStreamTransportType; aSubSessionClass: TRTSPMediaSubSessionClass=nil):TRTSPMediaSession;

    function  ReadTimeout: integer;


    constructor Create;
    destructor Destroy; override;
  end;

  TMediaStreamDataSource_RTSP = class (TMediaStreamDataSource)
  private
    FStarted: boolean;
    FDataLock : TCriticalSection;
    FSession : TRTSPMediaSession;
    FClientContainer  : TRTSPClientThreadSafeContainer;
    FLastStreamDateTime: TThreadVar<TDateTime>;
    FLastAckDateTime: TDateTime;
    FDisconnecting: integer;
    FAckAsyncThread: TAckAsyncThread;
  private
    procedure OnFrame(aSender:TRtpStreamFramer; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
    procedure OnFrameDelayed(aSender:TRtpStreamFramer; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
    procedure CheckConnectedOrRaise;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure LockData;
    procedure UnlockData;
    procedure SendAliveAsync;
    procedure SendAlive;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;

    function GetChannelInfos: TArray<TMediaStreamChannelInfo>; override;

    property Channel: TRTSPMediaSession read FSession;
  end;

  TMediaStreamDataSourceConnectParams_RTSP = class (TMediaStreamDataSourceConnectParams)
  private
    FURL: string;
    FOverTCP: boolean;
    FTransmitVideo, FTransmitAudio: boolean;
  public
    constructor Create; overload;

    constructor Create(
      const aURL: string;
      aOverTCP: boolean;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true); overload;

    constructor Create(
      const aServerIp: string;
      aServerPort: word;
      const aSourceName: string;
      const aUserName: string;
      const aUserPassword: string;
      aOverTCP: boolean;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    property Url: string read FURL write FURL;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
  end;

  TAckAsyncThread = class (TThread)
  private
    FOwner: TMediaStreamDataSource_RTSP;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TMediaStreamDataSource_RTSP);
  end;



implementation
  uses SrwLock,ThreadNames, MediaStream.UrlFormats, Patterns.Workspace;


{ TMediaStreamDataSourceConnectParams_RTSP }

procedure TMediaStreamDataSourceConnectParams_RTSP.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_RTSP;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_RTSP) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_RTSP;

  FURL:=aSrc.FURL;
  FOverTCP:=aSrc.FOverTCP;
  FTransmitVideo:=aSrc.FTransmitVideo;
  FTransmitAudio:=aSrc.FTransmitAudio;
end;

constructor TMediaStreamDataSourceConnectParams_RTSP.Create;
begin

end;

constructor TMediaStreamDataSourceConnectParams_RTSP.Create(const aURL: string;
      aOverTCP: boolean;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true);
begin
  Create;

  FURL:=aURL;
  FOverTCP:=aOverTCP;

  FTransmitVideo:=aTransmitVideo;
  FTransmitAudio:=aTransmitAudio;
end;

constructor TMediaStreamDataSourceConnectParams_RTSP.Create(
  const aServerIp: string; aServerPort: word; const aSourceName, aUserName,
  aUserPassword: string; aOverTCP, aTransmitVideo, aTransmitAudio: boolean);
begin
  Create(MakeRtspUrl(aServerIp,aServerPort,aSourceName,aUserName,aUserPassword),aOverTCP,aTransmitVideo,aTransmitAudio);
end;

procedure TMediaStreamDataSourceConnectParams_RTSP.Parse(const aUrl: string);
begin
  FURL:=aUrl;
end;

function TMediaStreamDataSourceConnectParams_RTSP.ToString: string;
begin
  result:=FURL;
end;

function TMediaStreamDataSourceConnectParams_RTSP.ToUrl(aIncludeAuthorizationInfo: boolean): string;
var
 aAddress: string;
 aPort: Word;
 aUrlSuffix: string;
 aUserName: string;
 aUserPassword: string;
begin
  if aIncludeAuthorizationInfo then
    result:=FURL
  else begin
    if ParseRtspUrl(FURL,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword) then
    begin
      result:=MakeMs3sUrl(aAddress,aPort,aUrlSuffix);
    end
    else
      result:=FURL;
  end;
end;

{ TMediaStreamDataSource_RTSP }

procedure TMediaStreamDataSource_RTSP.SendAlive;
begin
  if FDisconnecting=0 then
    if FClientContainer.Connected then
      FClientContainer.SendAlive;
end;

procedure TMediaStreamDataSource_RTSP.SendAliveAsync;
begin
  if FClientContainer.IsClientCreated then
    if FAckAsyncThread<>nil then
      FAckAsyncThread.TerminateInTime(FClientContainer.ReadTimeout+10);

  FreeAndNil(FAckAsyncThread);
  FAckAsyncThread:=TAckAsyncThread.Create(self);
end;

procedure TMediaStreamDataSource_RTSP.Start;
var
  i: integer;
begin
  LockData;
  try
    CheckConnectedOrRaise;
    FStarted:=true;
    for i:=0 to FSession.SubSessionCount-1 do
      FSession.SubSessions[i].StreamFramer.OnFrame:=OnFrame;
  finally
    UnlockData;
  end;
end;

procedure TMediaStreamDataSource_RTSP.Stop;
var
  i: integer;
begin
  LockData;
  try
    if FSession<>nil then
      for i:=0 to FSession.SubSessionCount-1 do
        if FSession.SubSessions[i].StreamFramer<>nil then
          FSession.SubSessions[i].StreamFramer.OnFrame:=nil;
  finally
    UnlockData;
  end;
end;

procedure TMediaStreamDataSource_RTSP.UnlockData;
begin
  Assert(FDataLock<>nil);
  FDataLock.Leave;
end;

procedure TMediaStreamDataSource_RTSP.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_ : TMediaStreamDataSourceConnectParams_RTSP;
  aAvailableCommands: string;
  aSDP: string;
  i: integer;
  aTT: TRtpStreamTransportType;
begin
  aConnectParams_:=aConnectParams as TMediaStreamDataSourceConnectParams_RTSP;

  FreeAndNil(FSession);
  FClientContainer.FreeClient;
  FLastStreamDateTime.Value:=0;

  try
    FClientContainer.CreateClient(aConnectParams_.FURL,ExtractFileName(ParamStr(0)));
    FClientContainer.SendCommandOptions(aAvailableCommands);
    FClientContainer.SendCommandDescribe(aSDP);


    if aConnectParams_.FOverTCP then
      aTT:=rtpRTP_TCP
    else
      aTT:=rtpRTP_UDP;

    FSession:=FClientContainer.CreateMediaSession(aSDP,aTT);

    for i := 0 to FSession.SubSessionCount-1 do
    begin
      if (FSession.SubSessions[i].MediaType=mtVideo) then
        FSession.SubSessions[i].TransmittingEnabled:=aConnectParams_.FTransmitVideo
      else if (FSession.SubSessions[i].MediaType=mtAudio) then
        FSession.SubSessions[i].TransmittingEnabled:=aConnectParams_.FTransmitAudio;

      if FSession.SubSessions[i].TransmittingEnabled then
        FClientContainer.SendCommandSetup(FSession.SubSessions[i]);

      FSession.SubSessions[i].CreateDefaultStreamFramer;
      FSession.SubSessions[i].StreamFramer.OnFrame:=OnFrameDelayed;
      FLastStreamFrames[FSession.SubSessions[i].MediaType].Format.biStreamType:=FSession.SubSessions[i].StreamType;

    end;

    FLastAckDateTime:=Now;
    FClientContainer.SendCommandPlay(FSession);

  except
    FClientContainer.FreeClient;
    FreeAndNil(FSession);
    raise;
  end;

end;

procedure TMediaStreamDataSource_RTSP.DoDisconnect;
begin
  InterlockedIncrement(FDisconnecting);
  try
    if (FClientContainer.IsClientCreated) and (FSession<>nil) then
    begin
      try
        if FClientContainer.Connected then
          FClientContainer.SendCommandTearDown(FSession,false);
      except
      end;
    end;

    try
      if FClientContainer.IsClientCreated then
        FClientContainer.Disconnect;
    except
    end;

    FreeAndNil(FSession);
    FClientContainer.FreeClient;
    FLastStreamDateTime.Value:=0;
  finally
    InterlockedDecrement(FDisconnecting);
  end;
end;

function TMediaStreamDataSource_RTSP.GetChannelInfos: TArray<TMediaStreamChannelInfo>;
var
  i: Integer;
begin
  SetLength(Result,FSession.SubSessionCount);
  for i := 0 to High(result) do
  begin

    result[i].Data:=Format('Id=%s, PlayUrl=%s, ConnectionEndPointName=%s, Protocol=%s, ControlPath=%s, StreamTypeName=%s, MediaType=%s, StreamType=%s, FramerClass=%s, RawPackets:Total/WithErrors/LastSeqNo: %d/%d/%d; Frames:Count/TotalSize: %d/%d',
    [
      FSession.SubSessions[i].SessionId,
      FSession.SubSessions[i].PlayUrl,
      FSession.SubSessions[i].ConnectionEndPointName,
      RtpStreamTransportTypeNames[FSession.SubSessions[i].Protocol],
      FSession.SubSessions[i].ControlPath,
      FSession.SubSessions[i].StreamTypeName,
      MediaTypeNames[FSession.SubSessions[i].MediaType],
      GetStreamTypeName(FSession.SubSessions[i].StreamType),
      FSession.SubSessions[i].StreamFramer.ClassName,
      FSession.SubSessions[i].TotalReceivedPackets,
      FSession.SubSessions[i].ErrorReceivedPackets,
      FSession.SubSessions[i].LastSeqNo,
      FSession.SubSessions[i].StreamFramer.FrameCount,
      FSession.SubSessions[i].StreamFramer.TotalFrameSize
    ])
  end;

end;

procedure TMediaStreamDataSource_RTSP.CheckConnectedOrRaise;
begin
  if FSession=nil then
    raise Exception.Create('Источник потока не открыт');
end;

constructor TMediaStreamDataSource_RTSP.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
  FLastStreamDateTime:=TThreadVar<TDateTime>.Create;
  FClientContainer:=TRTSPClientThreadSafeContainer.Create;
end;

class function TMediaStreamDataSource_RTSP.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_RTSP.Create;
end;

destructor TMediaStreamDataSource_RTSP.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FAckAsyncThread);
  FreeAndNil(FDataLock);
  FreeAndNil(FLastStreamDateTime);
  FreeAndNil(FClientContainer);
end;

function TMediaStreamDataSource_RTSP.LastStreamDataTime: TDateTime;
begin
  result:=FLastStreamDateTime.Value;
end;

procedure TMediaStreamDataSource_RTSP.LockData;
begin
  Assert(FDataLock<>nil);
  FDataLock.TryEnterOrRaise(10000);
end;

procedure TMediaStreamDataSource_RTSP.OnFrame(aSender:TRtpStreamFramer; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
var
  aNeedSendAlive: boolean;
begin
  //Можно вынести за Lock потому что все переменные - локальные
  RaiseOnData(aFormat,aData,aDataSize,aInfo,aInfoSize);

  //FIX: перенсено ниже RaiseOnData
  //посылаем сигнал Alive только после обработки. Иначе в процессе отправки Alive
  //в случае использования RTP+RTSP будет получен еще один фрейм, и он уйдет ДО этого фрейма, это неправильно

  if (FDisconnecting=0) then
  begin
    aNeedSendAlive:=false;
    LockData;
    try
      FLastStreamDateTime.Value:=Now;

      if (Now-FLastAckDateTime)*SecsPerDay>40 then
      begin
        if (FDisconnecting<>0) or (FSession=nil) or (not FClientContainer.IsClientCreated) or (FSession.SubSessionCount=0) or (not FClientContainer.Connected) then
          //Do nothing
        else begin
          FLastAckDateTime:=Now;
          aNeedSendAlive:=true;
        end;
      end;
    finally
      UnlockData;
    end;

    //Вынес за LockData
    if (FDisconnecting=0) and (aNeedSendAlive) then
    begin
      try
        SendAliveAsync;
      except
        //
      end;
    end;
  end;
end;

procedure TMediaStreamDataSource_RTSP.OnFrameDelayed(aSender: TRtpStreamFramer;
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal);
var
  i: Integer;
begin
  //FIX. Используем специальную задержку в 2 секунды максимум для того, чтобы дать возможность
  //инициализироваться полностью источнику (см. Start) . Если этого не сделать, то часть начальных кадров
  //уйдет в никуда, а это, как правило, опорные кадры.
  for i := 0 to 200 do
  begin
    if FStarted then
      break;
    if FDisconnecting>0 then
      break;
    sleep(10);
  end;

  if FDisconnecting>0 then
    exit;

  LockData;
  try
    if FStarted then
      OnFrame(aSender,aFormat,aData,aDataSize,aInfo,aInfoSize);
  finally
    UnlockData;
  end;
end;

{ TAckAsyncThread }

constructor TAckAsyncThread.Create(aOwner: TMediaStreamDataSource_RTSP);
begin
  FOwner:=aOwner;
  inherited Create(false);
end;

procedure TAckAsyncThread.Execute;
const
  aMethodName = 'TAckAsyncThread.Execute';
begin
  SetCurrentThreadName(ClassName);
  try
    FOwner.SendAlive;
  except
    on E:Exception do
    begin
      FOwner.TraceLine(E.Message);
      //TWorkspaceBase.Current.HandleException(self,E,aMethodName+'('+FOwner.ConnectionString+')');
    end;
  end;
end;

{ TRTSPClientThreadSafeContainer }

function TRTSPClientThreadSafeContainer.ClientSafe: TRTSPClient;
begin
  if FClient=nil then
    raise EAlgoError.Create('Экземпляр клиента не создан');
  Result:=FClient;
end;

function TRTSPClientThreadSafeContainer.Connected: boolean;
begin
  FLock.EnterReading;
  try
    result:=(FClient<>nil) and (FClient.Connected);
  finally
    FLock.LeaveReading;
  end;
end;

constructor TRTSPClientThreadSafeContainer.Create;
begin
  FLock:=TSRWLock.Create;
end;

procedure TRTSPClientThreadSafeContainer.CreateClient(const aUrl,
  aUserAgentName: string);
begin
  FLock.EnterWriting;
  try
    FreeClient;
    FClient:=TRTSPClient.Create(aUrl,aUserAgentName);
  finally
    FLock.LeaveWriting;
  end;
end;

function TRTSPClientThreadSafeContainer.CreateMediaSession(
  const aSdpDescription: string; aDesiredProtocol: TRtpStreamTransportType;
  aSubSessionClass: TRTSPMediaSubSessionClass): TRTSPMediaSession;
begin
  FLock.EnterReading;
  try
    result:=ClientSafe.CreateMediaSession(aSdpDescription,aDesiredProtocol,aSubSessionClass);
  finally
    FLock.LeaveReading;
  end;
end;

destructor TRTSPClientThreadSafeContainer.Destroy;
begin
  FreeAndNil(FLock);
end;

procedure TRTSPClientThreadSafeContainer.Disconnect;
begin
  FLock.EnterReading;
  try
    ClientSafe.Disconnect;
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.FreeClient;
begin
  FLock.EnterWriting;
  try
    FreeAndNil(FClient);
  finally
    FLock.LeaveWriting;
  end;
end;

function TRTSPClientThreadSafeContainer.IsClientCreated: boolean;
begin
  FLock.EnterReading;
  try
    result:=(FClient<>nil);
  finally
    FLock.LeaveReading;
  end;
end;

function TRTSPClientThreadSafeContainer.ReadTimeout: integer;
begin
  FLock.EnterReading;
  try
    result:=ClientSafe. ReadTimeout;
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendAlive;
begin
  FLock.EnterReading;
  try
    ClientSafe.SendAlive;
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendCommandDescribe(
  var aSdpDescription: string);
begin
  FLock.EnterReading;
  try
    ClientSafe.SendCommandDescribe(aSdpDescription);
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendCommandOptions(
  out aAvailableCommands: string);
begin
  FLock.EnterReading;
  try
    ClientSafe.SendCommandOptions(aAvailableCommands);
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendCommandPlay(
  aMediaSession: TRTSPMediaSession);
begin
  FLock.EnterReading;
  try
    ClientSafe.SendCommandPlay(aMediaSession);
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendCommandSetup(
  aMediaSubSession: TRTSPMediaSubSession);
begin
  FLock.EnterReading;
  try
    ClientSafe.SendCommandSetup(aMediaSubSession);
  finally
    FLock.LeaveReading;
  end;
end;

procedure TRTSPClientThreadSafeContainer.SendCommandTearDown(
  aMediaSession: TRTSPMediaSession; aCheckResponce: boolean);
begin
  FLock.EnterReading;
  try
    ClientSafe.SendCommandTearDown(aMediaSession,aCheckResponce);
  finally
    FLock.LeaveReading;
  end;
end;

initialization
  MediaStreamDataSourceFactory.Register('rtsp',TMediaStreamDataSource_RTSP);
end.

