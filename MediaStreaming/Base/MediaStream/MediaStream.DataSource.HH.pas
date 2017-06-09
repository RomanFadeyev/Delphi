{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Источник медиа-потока, обеспечивающий получение данных c      }
{                камер видеонаблюдения Beward (HH)                             }
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

unit MediaStream.DataSource.HH;

interface
  uses Windows,Classes,SysUtils, MediaStream.DataSource.Base, MediaProcessing.Definitions,
  MediaStream.PtzProtocol.HH, HHCommon,HHNet,HHNetAPI;

type
  //========================== HH (Beward) =====================================
  THHNetPaneChannelDataSink = class;
  THHNetEventHandler = class;

  TMediaStreamDataSource_HH = class (TMediaStreamDataSource)
  private
    FChannelDataSink : THHNetPaneChannelDataSink;
    FEventHandler : THHNetEventHandler;

    FIp: string;
    FPort: word;
    FChannelNo: integer;

    FChannel : THHNetChannel;
    FServer  : THHNetServer;
    FServerNotifications: TMediaStreamDataSourceNotificationSet;
    FUserName, FUserPassword: string;
    FTransmitVideo: boolean;
    FTransmitAudio: boolean;
    FConvertHHVIToH264IfPossible: boolean;

    procedure OnMotionAlarmHandler (Sender: THHNetServer; const Args: THHNetMotionAlarmEventArgs);
    procedure OnSensorAlarmOutputHandler   (Sender : THHNetServer; const Args : THHNetSensorAlarmEventArgs);
    function CheckAvailabilityViaPing:boolean;
    function GetPtz: TPtzProtocol_HH;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    procedure BeginTransaction; override;
    procedure EndTransaction; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;

    procedure PtzInit; override;
    function  PtzSupported: boolean; override;
    property  Ptz: TPtzProtocol_HH read GetPtz;

    property Channel: THHNetChannel read FChannel;
    property Server  : THHNetServer read FServer;

    function EnsureServerCreated: THHNetServer;
  end;

  THHNetPaneChannelDataSink = class(THHNetChannelDataSink)
  private
    FOwner: TMediaStreamDataSource_HH;
  public
    constructor Create(aOwner:TMediaStreamDataSource_HH);
    procedure OnFrameReceived(aSender: THHNetChannel; aStreamData: PHV_FRAME; aEncodeVideoType: ENCODE_VIDEO_TYPE; const aAVInfo: HHAV_INFO); override;
  end;

  THHNetEventHandler = class (THHNetEventHandlerBase)
  private
    FOwner: TMediaStreamDataSource_HH;
  protected
    procedure OnChannelClose(const aChannelInfo: HH_CHANNEL_INFO); override;
  end;

  TMediaStreamDataSourceConnectParams_HH = class (TMediaStreamDataSourceConnectParams)
  private
    FServerIP: string;
    FServerPort: word;
    FChannelNo: integer;
    FChannelProfile: integer;
    FProtocol: THHNetProtocol;
    FUserName,FUserPassword: string;
    FTransmitVideo, FTransmitAudio: boolean;
    FVideoFormat: THHChannelVideoFormat;
    FConvertHHVIToH264IfPossible: boolean;
  public
    constructor Create; overload;

    constructor Create(
      const aUrl: string;
      aProtocol: THHNetProtocol;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true;
      aVideoFormat: THHChannelVideoFormat=cvfDefault;
      aConvertHHVIToH264IfPossible:boolean = true); overload;

    constructor Create(
      const aServerIP: string;
      aServerPort: word;
      aChannelNo: integer;
      aChannelProfile: integer;
      aProtocol: THHNetProtocol;
      const aUserName,aUserPassword: string;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true;
      aVideoFormat: THHChannelVideoFormat=cvfDefault;
      aConvertHHVIToH264IfPossible:boolean = true); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    property ServerIp: string read FServerIP write FServerIP;
    property ServerPort: Word read FServerPort write FServerPort;
    property ChannelNo: integer read FChannelNo write FChannelNo;
    property UserName: string read FUserName write FUserName;
    property UserPassword: string read FUserPassword write FUserPassword;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
 end;



implementation
  uses VFW, MediaStream.UrlFormats, uPing,uBaseClasses;
{ THHNetPaneChannelDataSink }

constructor THHNetPaneChannelDataSink.Create(aOwner: TMediaStreamDataSource_HH);
begin
  FOwner:=aOwner;
end;

procedure THHNetPaneChannelDataSink.OnFrameReceived(aSender: THHNetChannel;aStreamData: PHV_FRAME; aEncodeVideoType: ENCODE_VIDEO_TYPE; const aAVInfo: HHAV_INFO);
var
  aFormat: TMediaStreamDataHeader;
  aH264Data: pointer;
  aH264DataSize: cardinal;
  pExtHead: PEXT_FRAME_HEAD;

  aTransmitData: pointer;
  aTransmitDataSize: cardinal;

  aTransmitInfo: pointer;
  aTransmitInfoSize: cardinal;

begin
  aFormat.Clear;

  if FOwner<>nil then
  begin
    aTransmitData:=aStreamData;
    aTransmitDataSize:=sizeof(HV_FRAME_HEAD)+aStreamData.nByteNum;
    aTransmitInfo:=@aAVInfo;
    aTransmitInfoSize:=sizeof(aAVInfo);

    if aStreamData.streamFlag=FRAME_FLAG_A then
    begin
      if not FOwner.FTransmitAudio then
        exit;

      aFormat.biMediaType:=mtAudio;
      aFormat.biStreamType:=stHHAU;
      aFormat.biStreamSubType:=aAVInfo.nAudioEncodeType;
      aFormat.TimeStamp:=aStreamData.nTimestamp;
      aFormat.TimeKoeff:=40;
      //aFormat.DataSize:=sizeof(HV_FRAME_HEAD)+aStreamData.nByteNum;
      aFormat.AudioChannels:=aAVInfo.nAudioChannels;
      aFormat.AudioBitsPerSample:=aAVInfo.nAudioBits;
      aFormat.AudioSamplesPerSec:=aAVInfo.nAudioSamples;
    end
    else if aStreamData.streamFlag in [FRAME_FLAG_VI,FRAME_FLAG_VP] then
    begin
      if not FOwner.FTransmitVideo then
        exit;

      aFormat.biMediaType:=mtVideo;
      aFormat.biStreamType:=stHHVI;
      aFormat.biStreamSubType:=aAVInfo.nVideoEncodeType;
      aFormat.TimeStamp:=aStreamData.nTimestamp;
      aFormat.TimeKoeff:=40;
      aFormat.VideoWidth:=aAVInfo.nVideoWidth;
      aFormat.VideoHeight:=aAVInfo.nVideoHeight;
      //aFormat.DataSize:=sizeof(HV_FRAME_HEAD)+aStreamData.nByteNum;
      if aStreamData.streamFlag=FRAME_FLAG_VI then
        Include(aFormat.biFrameFlags,ffKeyFrame);

      //старые камеры присылают ПОРЯДКОВЫЕ номера вместо TimeStamp
      //так мы не сможем рассчитать FPS, поэтому переходим на свои часы
      if not GetExtFrameHead(aStreamData, pExtHead) then
      begin
        aFormat.TimeStamp:=GetTickCount;
        aFormat.TimeKoeff:=1;
      end;

      if (aStreamData.streamFlag = FRAME_FLAG_VI) and (aAVInfo.nVideoEncodeType=stRGB) then
      begin
        aFormat.biStreamType:=stRGB;
        aFormat.VideoBitCount:=24;
        aFormat.VideoReversedVertical:=true;
        aTransmitData:=PAnsiChar(aStreamData)+sizeof(HV_FRAME_HEAD);
        aTransmitDataSize:=aStreamData.nByteNum;
        aTransmitInfo:=nil;
        aTransmitInfoSize:=0;
      end

      else if (aStreamData.streamFlag = FRAME_FLAG_VI) and (aAVInfo.nVideoEncodeType=stYUV420) then
      begin
        aFormat.biStreamType:=stYUV420;
        aFormat.VideoBitCount:=24;
        aFormat.VideoReversedVertical:=true;
        aTransmitData:=PAnsiChar(aStreamData)+sizeof(HV_FRAME_HEAD);
        aTransmitDataSize:=aStreamData.nByteNum;
        aTransmitInfo:=nil;
        aTransmitInfoSize:=0;
      end

      else if FOwner.FConvertHHVIToH264IfPossible then
        if GetH264Data(aStreamData,aH264Data,aH264DataSize) then
        begin
          aFormat.biStreamType:=stH264;
          aTransmitData:=aH264Data;
          aTransmitDataSize:=aH264DataSize;
          aTransmitInfo:=nil;
          aTransmitInfoSize:=0;
        end;
    end
    else begin
      exit; //raise ???
    end;

    FOwner.RaiseOnData(aFormat, aTransmitData,aTransmitDataSize,aTransmitInfo,aTransmitInfoSize);
  end;
end;

{ TMediaStreamDataSourceConnectParams_HH }

procedure TMediaStreamDataSourceConnectParams_HH.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_HH;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_HH) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_HH;

  FServerIP:=aSrc.FServerIP;
  FServerPort:=aSrc.FServerPort;
  FChannelNo:=aSrc.FChannelNo;
  FChannelProfile:=aSrc.FChannelProfile;
  FProtocol:=aSrc.FProtocol;
  FUserName:=aSrc.FUserName;
  FUserPassword:=aSrc.FUserPassword;
  FTransmitVideo:=aSrc.FTransmitVideo;
  FTransmitAudio:=aSrc.FTransmitAudio;
  FVideoFormat:=aSrc.FVideoFormat;
  FConvertHHVIToH264IfPossible:=aSrc.FConvertHHVIToH264IfPossible;
end;

constructor TMediaStreamDataSourceConnectParams_HH.Create;
begin

end;

constructor TMediaStreamDataSourceConnectParams_HH.Create(const aServerIP: string;
  aServerPort: word; aChannelNo, aChannelProfile: integer; aProtocol: THHNetProtocol;
  const aUserName, aUserPassword: string;
  aTransmitVideo, aTransmitAudio: boolean;
  aVideoFormat: THHChannelVideoFormat;
  aConvertHHVIToH264IfPossible:boolean);
begin
  Create;

  FServerIP:=aServerIP;
  FServerPort:=aServerPort;
  FChannelNo:=aChannelNo;
  FChannelProfile:=aChannelProfile;
  FProtocol:=aProtocol;
  FUserName:=aUserName;
  FUserPassword:=aUserPassword;

  FTransmitVideo:=aTransmitVideo;
  FTransmitAudio:=aTransmitAudio;
  FVideoFormat:=aVideoFormat;
  FConvertHHVIToH264IfPossible:=aConvertHHVIToH264IfPossible;
end;

constructor TMediaStreamDataSourceConnectParams_HH.Create(const aUrl: string;
  aProtocol: THHNetProtocol; aTransmitVideo, aTransmitAudio: boolean;
  aVideoFormat: THHChannelVideoFormat; aConvertHHVIToH264IfPossible: boolean);
begin
  Create;
  ParseBewardUrl(aUrl,FServerIP,FServerPort,FChannelNo,FChannelProfile,FUserName,FUserPassword);
  FProtocol:=aProtocol;

  FTransmitVideo:=aTransmitVideo;
  FTransmitAudio:=aTransmitAudio;
  FVideoFormat:=aVideoFormat;
  FConvertHHVIToH264IfPossible:=aConvertHHVIToH264IfPossible;
end;

procedure TMediaStreamDataSourceConnectParams_HH.Parse(const aUrl: string);
begin
  if not ParseBewardUrl(aUrl,FServerIP,FServerPort,FChannelNo,FChannelProfile,FUserName,FUserPassword) then
    RaiseParseError(aUrl);
end;

function TMediaStreamDataSourceConnectParams_HH.ToString: string;
begin
  result:=MakeBewardUrl(FServerIP,FServerPort,FChannelNo,FChannelProfile);
end;


function TMediaStreamDataSourceConnectParams_HH.ToUrl(aIncludeAuthorizationInfo: boolean): string;
begin
  if aIncludeAuthorizationInfo then
    result:=MakeBewardUrl(FServerIP,FServerPort,FChannelNo,FChannelProfile,FUserName,FUserPassword)
  else
    result:=MakeBewardUrl(FServerIP,FServerPort,FChannelNo,FChannelProfile)
end;

{ THHNetEventHandler }

procedure THHNetEventHandler.OnChannelClose(const aChannelInfo: HH_CHANNEL_INFO);
begin
  if FOwner=nil then
    exit;

  if FOwner.FChannel=nil then
    exit;

  if FOwner.FChannel.Handle<>aChannelInfo.hOpenChannel then
    exit;

  FOwner.RaiseOnClose;
end;


{ TMediaStreamDataSource_HH }

procedure TMediaStreamDataSource_HH.Start;
begin
//  FreeAndNil(FChannel); FChannel:=aContext_HH.Channel; aContext_HH.Channel:=nil;
//  FreeAndNil(FServer); FServer:=aContext_HH.Server; aContext_HH.Server:=nil;

  if FServer<>nil then
  begin
    if dsnMotionAlarms in FServerNotifications then
      FServer.OnMotionAlarmOutput:=OnMotionAlarmHandler;
    if dsnSensor in FServerNotifications then
      FServer.OnSensorAlarmOutput:=OnSensorAlarmOutputHandler;
  end;

  FChannel.AddDataSink(FChannelDataSink);
end;

procedure TMediaStreamDataSource_HH.Stop;
begin
  if FChannel<>nil then
    FChannel.DeleteAllDataSinks;
end;

procedure TMediaStreamDataSource_HH.BeginTransaction;
begin
  inherited;
  THHNetEnvironment.IncrementUsing;
end;

procedure TMediaStreamDataSource_HH.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aHHNetParams: THHNetChannelConnectionParams;
  aConnectParams_ : TMediaStreamDataSourceConnectParams_HH;
begin
  aConnectParams_:=aConnectParams as TMediaStreamDataSourceConnectParams_HH;
  FIp:=aConnectParams_.FServerIP;
  FPort:=aConnectParams_.FServerPort;
  FChannelNo:=aConnectParams_.FChannelNo;
  FUserName:=aConnectParams_.FUserName;
  FUserPassword:=aConnectParams_.FUserPassword;
  FTransmitVideo:=aConnectParams_.FTransmitVideo;
  FTransmitAudio:=aConnectParams_.FTransmitAudio;
  FConvertHHVIToH264IfPossible:=aConnectParams_.FConvertHHVIToH264IfPossible;

  FreeAndNil(FChannel);
  FreeAndNil(FServer);

  HHNetCheckIP(aConnectParams_.FServerIP);

  aHHNetParams.Init(aConnectParams_.FServerIP,
               aConnectParams_.FServerPort,
               aConnectParams_.FChannelNo,
               aConnectParams_.FChannelProfile,
               aConnectParams_.FProtocol,
               aConnectParams_.FUserName,
               aConnectParams_.FUserPassword);

  FChannel:=THHNetEnvironment.Manager.CreateChannel(aHHNetParams,aConnectParams_.FTransmitVideo,aConnectParams_.FTransmitAudio,aConnectParams_.FVideoFormat);

  if aConnectParams_.HandleNotifications*[dsnMotionAlarms,dsnSensor]<>[] then
  begin
    FServerNotifications:=aConnectParams_.HandleNotifications;
    FServer:=THHNetEnvironment.Manager.CreateServer(
      aConnectParams_.FServerIP,
      aConnectParams_.FServerPort,
      aConnectParams_.FUserName,
      aConnectParams_.FUserPassword);
  end;
end;

procedure TMediaStreamDataSource_HH.DoDisconnect;
begin
  FreeAndNil(FChannel);
  FreeAndNil(FServer);
end;

function TMediaStreamDataSource_HH.CheckAvailabilityViaPing: boolean;
var
  aPing: TPing;
begin
  aPing:=TPing.Create(FIp,100);
  try
    result:=aPing.IsHostReachable;
  except
    result:=false;
  end;

  aPing.Free;
end;

constructor TMediaStreamDataSource_HH.Create;
begin
  inherited;
  FChannelDataSink:=THHNetPaneChannelDataSink.Create(self);
  FLastStreamFrames[mtVideo].Format.biStreamType:=stHHVI;
  FLastStreamFrames[mtAudio].Format.biStreamType:=stHHAU;

  FEventHandler:=THHNetEventHandler.Create;
  IUnknown(FEventHandler)._AddRef;
  FEventHandler.FOwner:=self;

  SetPtz(TPtzProtocol_HH.Create);
  THHNetEnvironment.Manager.RegisterEventHandler(FEventHandler);
end;

class function TMediaStreamDataSource_HH.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_HH.Create;
end;

destructor TMediaStreamDataSource_HH.Destroy;
begin
  Disconnect;

  if THHNetEnvironment.IsInitialized then
    THHNetEnvironment.Manager.UnregisterEventHandler(FEventHandler);

  IUnknown(FEventHandler)._Release;
  FEventHandler:=nil;

  FChannelDataSink.FOwner:=nil;
  FreeAndNil(FChannelDataSink);

  inherited;
end;

procedure TMediaStreamDataSource_HH.EndTransaction;
begin
  inherited;
  THHNetEnvironment.DecrementUsing;
end;

function TMediaStreamDataSource_HH.EnsureServerCreated: THHNetServer;
begin
  if FServer=nil then
    FServer:=THHNetEnvironment.Manager.CreateServer(
      FIP,
      FPort,
      FUserName,
      FUserPassword);
  result:=FServer;
end;

function TMediaStreamDataSource_HH.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';

  if aError is EHHNetApiException then
  begin
    if EHHNetApiException(aError).ErrorCode in [HHERR_FAILURE,HHERR_TIME_OUT,HHERR_SOCKET_ERR] then
    begin
      if not CheckAvailabilityViaPing then
        result:='Камера не подключена к сети'
      else if EHHNetApiException(aError).ErrorCode=HHERR_TIME_OUT then
        result:='Камера не отвечает на запрос'
    end
    else if EHHNetApiException(aError).ErrorCode  = HHERR_PREVIEW_FULL then
    begin
      result:='Камера перегружена соединениями';
    end
    else if EHHNetApiException(aError).ErrorCode  = HHERR_USER_PASSWORD then
    begin
      result:='Авторизация не выполнена';
    end;
  end;
end;

function TMediaStreamDataSource_HH.GetPtz: TPtzProtocol_HH;
begin
  result:=inherited DirectPtz as TPtzProtocol_HH;
end;

function TMediaStreamDataSource_HH.LastStreamDataTime: TDateTime;
begin
  if FChannel<>nil then
    result:=FChannel.LastStreamDataTime
  else
    result:=0;
end;

procedure TMediaStreamDataSource_HH.OnMotionAlarmHandler(Sender: THHNetServer;
  const Args: THHNetMotionAlarmEventArgs);
var
  aNotification: TMediaStreamDataSourceNotificationParams;
begin
  if FChannel<>nil then
  begin
    Assert(FChannel.ChannelNo<cardinal(Length(Args.Motions)));
    if Args.Motions[FChannel.ChannelNo] then
    begin
      aNotification.Notification:=dsnMotionAlarms;
      RaiseOnNotification(aNotification);
    end;
  end;
end;


procedure TMediaStreamDataSource_HH.OnSensorAlarmOutputHandler(Sender: THHNetServer; const Args: THHNetSensorAlarmEventArgs);
var
  i: integer;
  aNotification: TMediaStreamDataSourceNotificationParams;
begin
  aNotification.Notification:=dsnSensor;
  for i := 0 to High(Args.Sensors) do
    if Args.Sensors[i] then
    begin
      //Sender.ResetSensorAlarm(i);
      aNotification.SourceId:=i;
      RaiseOnNotification(aNotification);
    end;
end;


procedure TMediaStreamDataSource_HH.PtzInit;
var
  aServerInfo : THHNetServerInfo;
begin
  if FServer=nil then
  begin
    ZeroMemory(@aServerInfo,sizeof(aServerInfo));
    aServerInfo.Ip:=FIp;
    aServerInfo.Port:=FPort;
    FServer:=THHNetEnvironment.Manager.CreateServer(aServerInfo,FUserName,FUserPassword);
  end;

  Ptz.Init(FServer,FChannelNo);
end;


function TMediaStreamDataSource_HH.PtzSupported: boolean;
begin
  if (FServer<>nil) and (FServer.Opened) then
    result:=FServer.PtzSupported
  else
    result:=true;
end;


initialization
  MediaStreamDataSourceFactory.Register('bwrd',TMediaStreamDataSource_HH);

end.

