{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Источник медиа-потока, обеспечивающий получение данных через  }
{                протокол Mscp                                                 }
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

unit MediaStream.DataSource.Mscp;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base,
  MediaProcessing.Definitions,MediaServer.Net.Mscp.Definitions, MediaServer.Net.Mscp.Client,
  uRemoteClient_B, Jpeg;

type
  TMediaStreamDataSourceConnectParams_Mscp = class;


  TMediaStreamDataSource_Mscp = class (TMediaStreamDataSource)
  private
    FFileReadThread : TThread;
    FLastStreamDataTime: TDateTime;
    FDataLock: TCriticalSection;
  protected
    procedure OnStreamDataReceived(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
    procedure OnStreamDataError(Sender: TObject; E: Exception);

    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    procedure Connect(aConnectParams: TMediaStreamDataSourceConnectParams_Mscp);
    function CheckConnected:boolean; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
  end;

  TMediaStreamDataSourceConnectParams_Mscp = class (TMediaStreamDataSourceConnectParams)
  private
    FServerIP: string;
    FServerPort: Word;
    FSourceName: string;
    FUserName : string;
    FPassword: string;
    FPeriodSecs: cardinal;
    FDesiredWidth: integer;
    FDesiredHeight: integer;
  public
    constructor Create; overload;

    constructor Create( const aUrl: string; aPeriodSecs: cardinal; aDesiredWidth: integer=-1; aDesiredHeight: integer=-1); overload;

    constructor Create(
      const aServerIP: string;
      aServerPort: Word;
      const aSourceName: string;
      const aUserName, aPassword: string;
      aPeriodSecs: cardinal;
      aDesiredWidth: integer=-1;
      aDesiredHeight: integer=-1); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;

    property SourceName: string read FSourceName write FSourceName;
    property ServerIp: string read FServerIP write FServerIP;

    property DesiredWidth: integer read FDesiredWidth;
    property DesiredHeight: integer read FDesiredHeight;
 end;

  EMediaServerUnreachable = class(Exception);

implementation

uses Math, DateUtils, uPing, uBaseClasses, MediaServer.StreamSerializer, MediaStream.UrlFormats,
  MediaServer.Net.Definitions;

type
  TStreamThread = class (TThread)
  private
    FOwner: TMediaStreamDataSource_Mscp;
    FClient : TMscpClient;

    FSourceName: string;
    FUserName : string;
    FPassword: string;
    FDesiredWidth: integer;
    FDesiredHeight: integer;
    FPeriodSecs: cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(const  aConnectParams_: TMediaStreamDataSourceConnectParams_Mscp; aOwner: TMediaStreamDataSource_Mscp);
    destructor Destroy; override;

    function Connected: boolean;
  end;

function GetReadThread(aThread : TThread): TStreamThread;
begin
  result:=aThread as TStreamThread;
  Assert(result<>nil);
end;

{ TMediaStreamDataSourceConnectParams_Mscp }

procedure TMediaStreamDataSourceConnectParams_Mscp.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_Mscp;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_Mscp) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_Mscp;

  FServerIP:=aSrc.FServerIP;
  FServerPort:=aSrc.FServerPort;
  FSourceName:=aSrc.FSourceName;
  FUserName:=aSrc.FUserName;
  FPassword:=aSrc.FPassword;
  FPeriodSecs:=aSrc.FPeriodSecs;
  FDesiredWidth:=aSrc.FDesiredWidth;
  FDesiredHeight:=aSrc.FDesiredHeight;
end;

constructor TMediaStreamDataSourceConnectParams_Mscp.Create;
begin
  FPeriodSecs:=60;
  FDesiredWidth:=-1;
  FDesiredHeight:=-1;
end;

constructor TMediaStreamDataSourceConnectParams_Mscp.Create(const aServerIP
  : string; aServerPort: Word; const aSourceName: string;
  const aUserName, aPassword: string; aPeriodSecs: cardinal;
  aDesiredWidth: integer=-1; aDesiredHeight: integer=-1);
begin
  Create;

  FServerIP := aServerIP;
  FServerPort := aServerPort;
  FSourceName := aSourceName;
  FUserName := aUserName;
  FPassword := aPassword;
  FPeriodSecs := aPeriodSecs;
  FDesiredWidth:= aDesiredWidth;
  FDesiredHeight:=aDesiredHeight;
end;

constructor TMediaStreamDataSourceConnectParams_Mscp.Create(const aUrl: string; aPeriodSecs: cardinal; aDesiredWidth: integer=-1; aDesiredHeight: integer=-1);
begin
  Parse(aUrl);
  FPeriodSecs:=aPeriodSecs;
  FDesiredWidth:=aDesiredWidth;
  FDesiredHeight:=aDesiredHeight;
end;

procedure TMediaStreamDataSourceConnectParams_Mscp.Parse(const aUrl: string);
begin
  if not ParseMscpUrl(aUrl, FServerIP, FServerPort, FSourceName, FUserName, FPassword) then
    RaiseParseError(aUrl);

  // Совместимости версий
  if FServerPort = 0 then
    FServerPort := MediaServer.Net.Definitions.icMscpServerPort;
  // if aConnectionParams.UserName='' then
  // aConnectionParams.UserName:='root';
end;

function TMediaStreamDataSourceConnectParams_Mscp.ToString: string;
begin
  result := ToUrl(false);
end;

function TMediaStreamDataSourceConnectParams_Mscp.ToUrl(aIncludeAuthorizationInfo: boolean): string;
begin
  if aIncludeAuthorizationInfo then
    result := MakeMscpUrl(FServerIP, FServerPort, FSourceName, FUserName, FPassword)
  else
    result := MakeMscpUrl(FServerIP, FServerPort, FSourceName)
end;

{ TMediaStreamDataSource_Mscp }

procedure TMediaStreamDataSource_Mscp.Start;
begin
  FDataLock.Enter;
  try
    FFileReadThread.Resume;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Mscp.Stop;
begin
  FDataLock.Enter;
  try
    if FFileReadThread<>nil then
    begin
      if GetCurrentThread=FFileReadThread.Handle then
        raise Exception.Create('Нельзя остановить поток из него же');
      if not FFileReadThread.Finished then
        FFileReadThread.Suspend;
    end;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Mscp.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_: TMediaStreamDataSourceConnectParams_Mscp;
begin
  aConnectParams_ := aConnectParams as TMediaStreamDataSourceConnectParams_Mscp;

  FreeAndNil(FFileReadThread);
  FFileReadThread:=TStreamThread.Create(aConnectParams_,self);
end;

procedure TMediaStreamDataSource_Mscp.DoDisconnect;
begin
  FreeAndNil(FFileReadThread);
  // FreeAndNil(FServer);
end;

function TMediaStreamDataSource_Mscp.CheckConnected: boolean;
begin
  inherited;
  result := (FFileReadThread <> nil) and (GetReadThread(FFileReadThread).Connected);
end;

procedure TMediaStreamDataSource_Mscp.Connect(aConnectParams: TMediaStreamDataSourceConnectParams_Mscp);
begin
  inherited Connect(aConnectParams);
end;

constructor TMediaStreamDataSource_Mscp.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
end;

class function TMediaStreamDataSource_Mscp.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result := TMediaStreamDataSourceConnectParams_Mscp.Create;
end;

destructor TMediaStreamDataSource_Mscp.Destroy;
begin
  Disconnect;
  // if THHNetEnvironment.IsInitialized then
  // THHNetEnvironment.Manager.UnregisterEventHandler(FEventHandler);

  // IUnknown(FEventHandler)._Release;
  // FEventHandler:=nil;

  inherited;
  FreeAndNil(FDataLock);
end;

function TMediaStreamDataSource_Mscp.GetConnectionErrorDescription(aError: Exception): string;
begin
  result := '';

  if aError is EMediaServerUnreachable then
    result := 'Медиа-сервер не доступен в сети';
end;

function TMediaStreamDataSource_Mscp.LastStreamDataTime: TDateTime;
begin
  if FFileReadThread<>nil then
  begin
    FDataLock.Enter;
    try
      result:=FLastStreamDataTime;
    finally
      FDataLock.Leave;
    end;
  end
  else
    result:=0;
end;

procedure TMediaStreamDataSource_Mscp.OnStreamDataReceived(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  FDataLock.Enter;
  try
    if (FFileReadThread=nil) then
      exit;

    FLastStreamDataTime:=Now;
    RaiseOnData(aFormat,aData,aDataSize,aInfo,aInfoSize);
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Mscp.OnStreamDataError(Sender: TObject;E: Exception);
begin
  RaiseOnClose;
end;

(* procedure TMediaStreamDataSource_Mscp.OnMotionAlarmHandler(Sender: THHNetServer;
  const Args: THHNetMotionAlarmEventArgs);
  begin
  if FStream<>nil then
  begin
  Assert(FStream.ChannelNo<cardinal(Length(Args.Motions)));
  if Args.Motions[FStream.ChannelNo] then
  RaiseOnMotionAlarm;
  end;
  end;
*)

{ TStreamThread }

function TStreamThread.Connected: boolean;
begin
  result:=false;
  try
    result:=FClient.NetClient.Connected;
  except
  end;
end;

constructor TStreamThread.Create(const aConnectParams_: TMediaStreamDataSourceConnectParams_Mscp; aOwner: TMediaStreamDataSource_Mscp);
begin
  inherited Create(true);
  FOwner:=aOwner;
  FSourceName:=aConnectParams_.FSourceName;
  FUserName:=aConnectParams_.FUserName;
  FPassword:=aConnectParams_.FPassword;
  FPeriodSecs:=aConnectParams_.FPeriodSecs;
  FDesiredWidth:=aConnectParams_.FDesiredWidth;
  FDesiredHeight:=aConnectParams_.FDesiredHeight;

  FClient:=TMscpClient.Create(aConnectParams_.FServerIP,aConnectParams_.FServerPort,true);
end;

destructor TStreamThread.Destroy;
begin
  inherited;
  FreeAndNil(FClient);
end;

procedure TStreamThread.Execute;
var
  aJpeg: TJPEGImage;
  aStart,aStop: TDateTime;
  aBuffer: TMemoryStream;
  aFormat: TMediaStreamDataHeader;
  aPause: int64;
  aPingArgs: TPingArgs;
begin
  aBuffer:=TMemoryStream.Create;
  try
    try
      while not Terminated do
      begin
        aStart:=Now;
        aJpeg:=FClient.SendCommandGetImage(FSourceName,FUserName,FPassword,FDesiredWidth,FDesiredHeight,Max(FPeriodSecs*1000,1000));
        try
          aBuffer.Position:=0;
          aJpeg.SaveToStream(aBuffer);
          aFormat.Clear;
          aFormat.biMediaType:=mtVideo;
          aFormat.biStreamType:=stMJPEG;
          aFormat.biFrameFlags:=[ffKeyFrame];
          aFormat.TimeStamp:=GetTickCount;
          aFormat.VideoWidth:=aJpeg.Width;
          aFormat.VideoHeight:=aJpeg.Height;
        finally
          aJpeg.Free;
        end;
        aStop:=Now;

        FOwner.OnStreamDataReceived(aFormat,aBuffer.Memory,aBuffer.Position,nil,0);
        aPause:=int64(FPeriodSecs*1000)-MilliSecondsBetween(aStart,aStop);
        if aPause>0 then
        begin
          while (aPause>0) and not Terminated do
          begin
            if aPause<1000 then
            begin
              Pause(aPause);
              aPause:=0;
            end
            else begin
              Pause(1000);
              dec(aPause,1000);
            end;

            if not Connected then
              raise Exception.Create('No connection');

            aFormat.Clear;
            aFormat.biMediaType:=mtSysData;
            aFormat.biStreamType:=stPING;
            aFormat.TimeStamp:=GetTickCount;
            aPingArgs.Reserved:=0;
            FOwner.OnStreamDataReceived(aFormat,@aPingArgs,sizeof(aPingArgs),nil,0);
          end;
        end;
      end;
    except
      on E:Exception do
        FOwner.OnStreamDataError(self,E);
    end;
  finally
    FreeAndNil(aBuffer);
  end;

end;

initialization

MediaStreamDataSourceFactory.Register('mscp', TMediaStreamDataSource_Mscp);

end.
