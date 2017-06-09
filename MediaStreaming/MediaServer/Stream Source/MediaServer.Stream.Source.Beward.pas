{***********************************<_INFO>************************************}
{  <Проект>      Медиа-сервер                                                  }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Медиа-источник, предоставляющий связь с IP-камерами Beward    }
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

unit MediaServer.Stream.Source.Beward;

interface
  uses Windows, SysUtils, SyncObjs, Classes, ExtCtrls,HHNet, HHCommon,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions,MediaStream.DataSource.Base, MediaStream.DataSource.HH,uBaseClasses;

type
  TQueryPositionThread = class;
  //Класс, выполняющий непосредственно получение данных (видеопотока) из камеры
  TMediaServerSourceBeward = class (TMediaServerSourceBasedOnMediaStream)
  private
    FConnectParams: THHNetChannelConnectionParams;

    FQueryPositionThread: TThreadObjectVar<TQueryPositionThread>;
    FPtzPositionPan,FPtzPositionTilt: TThreadVar<double>;

    procedure OnChannelClose(Sender: THHNetChannel);
    procedure OnPtzPostPositionQueryResult(Sender: THHNetServer; aPositionType: TPtzPositionType; aPositionValue: double);
  protected
    procedure OnConnectionOKThreaded(aParams: POpenConnectionOkParams); override;
  public
    constructor Create(const aIP: string;
                       aPort: Word;
                       aChannelNo,aChannelProfile: Word;
                       const aProtocol: {THHNetProtocol} cardinal;
                       const aUserName, aUserPassword: string;
                       aTransmitAudio: boolean; //Записывать ли аудио
                       aDataReceiveTimeout: integer //таймаут получения данных от канала
                       ); overload;

    destructor Destroy; override;
    procedure  DoClose; override;

    property  ConnectParams: THHNetChannelConnectionParams read FConnectParams;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;
    function StateInfo: string; override;
  end;

  TBewardThreadExceptionEvent = procedure (aExceptionSender: TObject; E:Exception) of object;
  TBewardTraceEvent = procedure (aSender: TObject; const aTraceMessage: string) of object;

  TBewardWorkspace = class
  private
    FOnThreadException: TBewardThreadExceptionEvent;
    FOnTrace: TBewardTraceEvent;
    FEnableTracing : boolean;
    FProxyUsingPolicy: THHNetProxyUsingPolicy;

   // Обработка исключений, возникающих в потоках каналов
    procedure OnHHNetThreadException(aExceptionSender: TObject; E:Exception);
    //Трассировка из HHNet
    procedure OnHHNetTrace(aSender:  THHNetManager; const aTraceMessage: string);

    procedure InitializeEnvironment;
  public
    procedure Finalize;

    property EnableTracing: boolean read FEnableTracing write FEnableTracing;
    property ProxyUsingPolicy: THHNetProxyUsingPolicy  read FProxyUsingPolicy write FProxyUsingPolicy;

    property OnThreadException:TBewardThreadExceptionEvent read FOnThreadException write FOnThreadException;
    property OnTrace: TBewardTraceEvent read FOnTrace write FOnTrace;
  end;

  TQueryPositionThread = class(TThread)
  private
    FOwner: TMediaServerSourceBeward;
  protected
    procedure Execute; override;

    constructor Create(aOwner: TMediaServerSourceBeward);
  end;

function BewardWorkspace: TBewardWorkspace;

implementation
  uses Math,Forms,MediaStream.UrlFormats, MediaServer.Workspace,uTrace;

var
  gWorkspace: TBewardWorkspace;

function BewardWorkspace: TBewardWorkspace;
begin
  if gWorkspace=nil then
    gWorkspace:=TBewardWorkspace.Create;

  result:=gWorkspace;
end;

{ TQueryPositionThread }

constructor TQueryPositionThread.Create(aOwner: TMediaServerSourceBeward);
begin
  FOwner:=aOwner;
  inherited Create(false);
end;

procedure TQueryPositionThread.Execute;
var
  i: Integer;
  aDS:TMediaStreamDataSource_HH;
begin
  while not Terminated do
  begin
    try
      if  (FOwner<>nil) and (FOwner.Opened) then
      begin
        aDS:=(FOwner.Stream as TMediaStreamDataSource_HH);
        aDS.EnsureServerCreated.PtzPostPositionQuery(aDS.Channel.ChannelNo);
      end;
    except
    end;

    for i := 0 to 20 do
    begin
      Sleep(100);
      if Terminated then
        break;
    end;
  end;
end;


{ TMediaServerSourceBeward }

constructor TMediaServerSourceBeward.Create(
                       const aIP: string;
                       aPort: Word;
                       aChannelNo,aChannelProfile: Word;
                       const aProtocol: {THHNetProtocol} cardinal;
                       const aUserName, aUserPassword: string;
                       aTransmitAudio: boolean; //Записывать ли аудио
                       aDataReceiveTimeout: integer //таймаут получения данных от канала
                       );
var
  aParams: TMediaStreamDataSourceConnectParams_HH;
begin
  FPtzPositionPan:=TThreadVar<double>.Create;
  FPtzPositionTilt:=TThreadVar<double>.Create;
  FQueryPositionThread:=TThreadObjectVar<TQueryPositionThread>.Create;

  try
    BewardWorkspace.InitializeEnvironment;
  finally
    //Обязательно нужно доконструировать объект
    aParams:=TMediaStreamDataSourceConnectParams_HH.Create(aIP,aPort,aChannelNo,aChannelProfile,THHNetProtocol(aProtocol),aUserName,aUserPassword,true,aTransmitAudio,cvfDefault,false);
    inherited Create(aParams, TMediaStreamDataSource_HH, aTransmitAudio, aDataReceiveTimeout);
  end;

  FConnectParams.Init(
        aIP,
        aPort,
        aChannelNo,
        aChannelProfile,
        THHnetProtocol(aProtocol),
        aUserName,
        aUserPassword);
end;

destructor TMediaServerSourceBeward.Destroy;
begin
  inherited;
  FreeAndNil(FQueryPositionThread);
  FreeAndNil(FPtzPositionPan);
  FreeAndNil(FPtzPositionTilt);
end;

function TMediaServerSourceBeward.DeviceType: string;
begin
  result:='Камера Beward';
end;

procedure TMediaServerSourceBeward.DoClose;
begin
  FQueryPositionThread.FreeValue;
  inherited;
end;

function TMediaServerSourceBeward.Name: string;
begin
  Result := Format('%s:%d, канал %d', [FConnectParams.Ip, FConnectParams.Port, FConnectParams.ChannelNo])
end;

procedure TMediaServerSourceBeward.OnChannelClose(Sender: THHNetChannel);
const
  aMethodName = 'TMediaServerSourceBeward.OnChannelClose';
begin
  try
    FQueryPositionThread.FreeValue;
    CloseInternal;
    FQueryPositionThread.FreeValue;

    if not Destroying then
      if Assigned(OnClosed) then
        OnClosed(self);
  except
    on E:Exception do
      Workspace.HandleException(self,E,aMethodName+':1');
  end;

  try
    if not Destroying then
      StartReconnect;
  except
    on E:Exception do
      Workspace.HandleException(self,E,aMethodName+':2');
  end;
end;

procedure TMediaServerSourceBeward.OnConnectionOKThreaded(aParams: POpenConnectionOkParams);
var
  aDataSource:TMediaStreamDataSource_HH;
begin
  inherited;
  aDataSource:=aParams.Stream as TMediaStreamDataSource_HH;
  aDataSource.Channel.OnClose:=OnChannelClose;
  if aDataSource.EnsureServerCreated.PtzSupported then
  begin
    aDataSource.EnsureServerCreated.OnPtzPositionQueryResult:=OnPtzPostPositionQueryResult;
    aDataSource.EnsureServerCreated.PtzPostPositionQuery(aDataSource.Channel.ChannelNo);
  end;
end;

procedure TMediaServerSourceBeward.OnPtzPostPositionQueryResult(
  Sender: THHNetServer; aPositionType: TPtzPositionType;
  aPositionValue: double);
var
  aFormat: TMediaStreamDataHeader;
  aData: TPtzPositionArgs;
begin
  if aPositionType=ptzPan then
    FPtzPositionPan.Value:=aPositionValue;

  if aPositionType=ptzTilt then
    FPtzPositionTilt.Value:=aPositionValue;

  aFormat.Clear;
  aFormat.biMediaType:=mtSysData;
  aFormat.biStreamType:=stPtzPosition;
  aFormat.TimeStamp:=GetTickCount;

  aData.Pan:=FPtzPositionPan.Value;
  aData.Tilt:=FPtzPositionTilt.Value;

  DoDataReceived(aFormat,@aData,sizeof(aData),nil,0);
  if Opened and not Closing then
  begin
    FQueryPositionThread.Lock;
    try
      if FQueryPositionThread.Value=nil then
        FQueryPositionThread.Value:=TQueryPositionThread.Create(self);
    finally
      FQueryPositionThread.Unlock;
    end;
  end;
end;

function TMediaServerSourceBeward.ConnectionString: string;
begin
  result:=MakeBewardUrl(
  FConnectParams.Ip,FConnectParams.Port,
  FConnectParams.ChannelNo,
  FConnectParams.ChannelProfile);
end;

function TMediaServerSourceBeward.StateInfo: string;
var
  s: string;
begin
  result:=inherited StateInfo;
  if FQueryPositionThread.Value<>nil then
  begin
    s:=Format('Поддерживается получение текущих координат. Текущ. поворот=%d%%, текущ. наклон=%d%%',[Trunc(FPtzPositionPan.Value),Trunc(FPtzPositionTilt.Value)]);
    if result='' then
      result:=s
    else
      result:=result+'; '+s;
  end;
end;

function TMediaServerSourceBeward.StreamInfo: TBytes;
var
  aAVInfo: HHAV_INFO;
begin
  CheckConnected;
  aAVInfo := TMediaStreamDataSource_HH(self.Stream).Channel.AVInfo;

  SetLength(result,sizeof(aAVInfo));
  CopyMemory(result,@aAVInfo,sizeof(aAVInfo));
end;

{ TBewardWorkspace }

procedure TBewardWorkspace.Finalize;
begin
  //Destroy может вызываться прямо из конструктора, так что мы можем и не проинициализироваться
  if THHNetEnvironment.IsInitialized then
  begin
    THHNetEnvironment.Manager.OnThreadException:=nil;
    THHNetEnvironment.Release;
  end;
end;

procedure TBewardWorkspace.InitializeEnvironment;
begin
  if not THHNetEnvironment.IsInitialized then
  begin
    // Инициализация среды
    THHNetEnvironment.Initialize(wmVoluntaryConnect,false, '',FProxyUsingPolicy);
    THHNetEnvironment.Manager.OnThreadException := OnHHNetThreadException;
    THHNetEnvironment.Manager.OnTrace:=OnHHNetTrace;

    THHNetEnvironment.EnableTracing(uTrace.GetTraceEnabled);
  end;
end;

procedure TBewardWorkspace.OnHHNetThreadException(aExceptionSender: TObject;
  E: Exception);
begin
  if Assigned(FOnThreadException) then
     FOnThreadException(aExceptionSender,E);
end;

procedure TBewardWorkspace.OnHHNetTrace(aSender: THHNetManager;const aTraceMessage: string);
begin
  if Assigned(FOnTrace) then
    FOnTrace(self,aTraceMessage);
end;

initialization

finalization
  FreeAndNil(gWorkspace);


end.

