{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Источник медиа-потока, обеспечивающий получение данных через  }
{                протокол Ms3s                                                 }
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

unit MediaStream.DataSource.Ms3s;

interface
  uses Windows,Classes,SysUtils, MediaStream.DataSource.Base,
  MediaProcessing.Definitions,MediaServer.Net.Ms3s.Definitions,
  MediaServer.Net.Ms3s.Stream,MediaServer.Net.Ms3s.Login, MediaStream.PtzProtocol.Ms3s;

type
  TMediaStreamDataSourceConnectParams_Ms3s = class;


  TMediaStreamDataSource_Ms3s = class (TMediaStreamDataSource)
  private
    FIp: string;

    FStream : TMediaServerStream;
    function GetPtz: TPtzProtocol_Ms3s;
  protected
    procedure OnStreamDataReceiving(aSender: TMediaServerStream; var aDataPacketHeader: TStreamDataPacketHeader);
    procedure OnStreamDataReceived(aSender: TMediaServerStream; const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);
    procedure OnStreamDataError(Sender: TObject; E: Exception);

    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    procedure Connect(aConnectParams: TMediaStreamDataSourceConnectParams_Ms3s);
    function CheckConnected:boolean; override;

    procedure BeginTransaction; override;
    procedure EndTransaction; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
    function  StreamLength: integer; override; //msec, -1 = INFINITE

    function PtzSupported: boolean; override;
    procedure PtzInit; override;

    function  PtzGetPoints: TMediaServerPtzPointInfoArray;

    function ArchiveSupported: boolean; override;
    function ArchivePoints: TArray<TMediaStreamDataSourceArchivePointInfo>; override;

    property Channel: TMediaServerStream read FStream;
    property Ptz: TPtzProtocol_Ms3s read GetPtz;
  end;

  TMediaStreamDataSourceConnectParams_Ms3s = class (TMediaStreamDataSourceConnectParams)
  private
    FServerIP: string;
    FServerPort: Word;
    FSourceName: string;
    FUserName : string;
    FPassword: string;
    FMediaProcessors: TBytes;
    FBandWidth: cardinal;
    FMediaTypes: TMediaTypeSet;
    FParams: string;
  public
    constructor Create; overload;

    constructor Create(
      const aUrl: string;
      aMediaProcessors: TBytes;
      aBandWidth: cardinal;
      aMediaTypes: TMediaTypeSet=AllMediaTypes;
      aParams: string=''); overload;

    constructor Create(
      const aServerIP: string;
      aServerPort: Word;
      const aSourceName: string;
      const aUserName, aPassword: string;
      aMediaProcessors: TBytes;
      aBandWidth: cardinal;
      aMediaTypes: TMediaTypeSet=AllMediaTypes;
      aParams: string=''); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;

    property SourceName: string read FSourceName write FSourceName;
    property ServerIp: string read FServerIP write FServerIP;
    property MediaTypes: TMediaTypeSet read FMediaTypes write FMediaTypes;
    property Params: string read FParams write FParams;
 end;

  EMediaServerUnreachable = class(Exception);

implementation

uses DateUtils, uPing, uBaseClasses, MediaServer.StreamSerializer, MediaStream.UrlFormats,
  MediaServer.Net.Definitions, Interconnection.TcpClient_B;

{ TMediaStreamDataSourceConnectParams_Ms3s }

procedure TMediaStreamDataSourceConnectParams_Ms3s.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_Ms3s;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_Ms3s) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_Ms3s;

  FServerIP:=aSrc.FServerIP;
  FServerPort:=aSrc.FServerPort;
  FSourceName:=aSrc.FSourceName;
  FUserName:=aSrc.FUserName;
  FPassword:=aSrc.FPassword;
  FMediaProcessors:=aSrc.FMediaProcessors;
  FBandWidth:=aSrc.FBandWidth;
  FMediaTypes:=aSrc.FMediaTypes;
end;


constructor TMediaStreamDataSourceConnectParams_Ms3s.Create;
begin
  FMediaTypes:=AllMediaTypes;
  FBandWidth:=INFINITE;
end;

constructor TMediaStreamDataSourceConnectParams_Ms3s.Create(const aServerIP
  : string; aServerPort: Word; const aSourceName: string;
  const aUserName, aPassword: string; aMediaProcessors: TBytes;
  aBandWidth: cardinal; aMediaTypes: TMediaTypeSet; aParams: string);
begin
  Create;

  FServerIP := aServerIP;
  FServerPort := aServerPort;
  FSourceName := aSourceName;
  FUserName := aUserName;
  FPassword := aPassword;
  FMediaProcessors := aMediaProcessors;
  FBandWidth := aBandWidth;
  FMediaTypes := aMediaTypes;
  FParams := aParams;
end;

constructor TMediaStreamDataSourceConnectParams_Ms3s.Create(const aUrl: string;
  aMediaProcessors: TBytes; aBandWidth: cardinal; aMediaTypes: TMediaTypeSet;
  aParams: string);
begin
  Parse(aUrl);
  FMediaProcessors := aMediaProcessors;
  FBandWidth := aBandWidth;
  FMediaTypes := aMediaTypes;
  FParams := aParams;
end;

procedure TMediaStreamDataSourceConnectParams_Ms3s.Parse(const aUrl: string);
begin
  if not ParseMs3sUrl(aUrl, FServerIP, FServerPort, FSourceName, FUserName, FPassword) then
    RaiseParseError(aUrl);

  // Совместимости версий
  if FServerPort = 0 then
    FServerPort := MediaServer.Net.Definitions.icCommandServerPort;
  // if aConnectionParams.UserName='' then
  // aConnectionParams.UserName:='root';
end;

function TMediaStreamDataSourceConnectParams_Ms3s.ToString: string;
begin
  result := ToUrl(false);
end;

function TMediaStreamDataSourceConnectParams_Ms3s.ToUrl(aIncludeAuthorizationInfo: boolean): string;
begin
  if aIncludeAuthorizationInfo then
    result := MakeMs3sUrl(FServerIP, FServerPort, FSourceName, FUserName, FPassword)
  else
    result := MakeMs3sUrl(FServerIP, FServerPort, FSourceName)
end;

{ TMediaStreamDataSource_Ms3s }

procedure TMediaStreamDataSource_Ms3s.Start;
begin
  // if FServer<>nil then
  // FServer.OnMotionAlarmOutput:=OnMotionAlarmHandler;

  FStream.OnDataReceived.Add(OnStreamDataReceived);
  FStream.OnDataReceiving.Add(OnStreamDataReceiving);
  FStream.OnReadError.Add(OnStreamDataError);
  FStream.Suspended := false;
end;

procedure TMediaStreamDataSource_Ms3s.Stop;
var
  i: integer;
begin
  if FStream <> nil then
    if FStream.OnDataReceived <> nil then
    begin
      FStream.OnDataReceived.Clear(false); // Указываем false, что означает, что комнада не синхронизируется с рассылкой
      for i := 0 to 10 do
      begin
        if not FStream.OnDataReceived.IsCurrentEmission then
          break;
        sleep(10);
      end;
    end;
end;

function TMediaStreamDataSource_Ms3s.StreamLength: integer;
begin
  if FStream = nil then
    raise Exception.Create('Источник потока не открыт');

  result := FStream.Duration;
end;

function TMediaStreamDataSource_Ms3s.ArchivePoints: TArray<TMediaStreamDataSourceArchivePointInfo>;
begin
  result := nil;
  if ArchiveSupported then
    result := TLinq.Select<TMediaServerStreamArchiveSourceInfo, TMediaStreamDataSourceArchivePointInfo>
     (Channel.ArchiveSources,
      function(x: TMediaServerStreamArchiveSourceInfo)
        : TMediaStreamDataSourceArchivePointInfo
      begin
        result.Id := x.Id;
        result.Text := x.Description;
      end);
end;

function TMediaStreamDataSource_Ms3s.ArchiveSupported: boolean;
begin
  result := (Channel <> nil) and (Channel.ArchiveSupported);
end;

procedure TMediaStreamDataSource_Ms3s.BeginTransaction;
begin
  inherited;
end;

procedure TMediaStreamDataSource_Ms3s.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_: TMediaStreamDataSourceConnectParams_Ms3s;
begin
  aConnectParams_ := aConnectParams as TMediaStreamDataSourceConnectParams_Ms3s;
  FIp := aConnectParams_.FServerIP;

  FreeAndNil(FStream);
  // FreeAndNil(FServer);

  // HHNetCheckIP(aConnectParams_.FServerIP);

  try
    FStream := TMediaServerStream.CreateAndOpenWithLogin
      (aConnectParams_.FServerIP, aConnectParams_.FServerPort,
      aConnectParams_.FSourceName, aConnectParams_.FUserName,
      aConnectParams_.FPassword, aConnectParams_.FMediaProcessors,
      aConnectParams_.FBandWidth, true, aConnectParams_.FMediaTypes,
      aConnectParams_.FParams
    // Изначально поток не активный, работать он должен только после вызова Start
    // Иначе получается, что первоначальная часть данных уходит в никуда, а это обычно опорные кадры
      );
    Ptz.Init(FStream);
  except
    on E: ERemoteClientConnectionError do
      raise EMediaServerUnreachable.Create(E.Message);
  end;

  (*
    if aHandleMotionAlarms then
    FServer:=THHNetEnvironment.Manager.CreateServer(
    aConnectParams_.FServerIP,
    aConnectParams_.FServerPort,
    aConnectParams_.FUserName,
    aConnectParams_.FUserPassword);
  *)
end;

procedure TMediaStreamDataSource_Ms3s.DoDisconnect;
begin
  Ptz.Init(nil);
  FreeAndNil(FStream);
  // FreeAndNil(FServer);
end;

function TMediaStreamDataSource_Ms3s.CheckConnected: boolean;
begin
  inherited;
  result := (FStream <> nil) and (FStream.Opened);
end;

procedure TMediaStreamDataSource_Ms3s.Connect(aConnectParams: TMediaStreamDataSourceConnectParams_Ms3s);
begin
  inherited Connect(aConnectParams);
end;

constructor TMediaStreamDataSource_Ms3s.Create;
begin
  inherited;
  SetPtz(TPtzProtocol_Ms3s.Create);
end;

class function TMediaStreamDataSource_Ms3s.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result := TMediaStreamDataSourceConnectParams_Ms3s.Create;
end;

destructor TMediaStreamDataSource_Ms3s.Destroy;
begin
  Disconnect;
  // if THHNetEnvironment.IsInitialized then
  // THHNetEnvironment.Manager.UnregisterEventHandler(FEventHandler);

  // IUnknown(FEventHandler)._Release;
  // FEventHandler:=nil;

  inherited;
end;

procedure TMediaStreamDataSource_Ms3s.EndTransaction;
begin
  inherited;
end;

function TMediaStreamDataSource_Ms3s.GetConnectionErrorDescription(aError: Exception): string;
begin
  result := '';

  if aError is EMediaServerUnreachable then
    result := 'Медиа-сервер не доступен в сети';
end;

function TMediaStreamDataSource_Ms3s.GetPtz: TPtzProtocol_Ms3s;
begin
  result:=DirectPtz as TPtzProtocol_Ms3s;
end;

function TMediaStreamDataSource_Ms3s.LastStreamDataTime: TDateTime;
begin
  if FStream <> nil then
    result := FStream.LastStreamDataTime
  else
    result := 0;
end;

procedure TMediaStreamDataSource_Ms3s.OnStreamDataError(Sender: TObject;
E: Exception);
begin
  RaiseOnClose;
end;

procedure TMediaStreamDataSource_Ms3s.OnStreamDataReceived(aSender: TMediaServerStream; const aFormat: TMediaStreamDataHeader;
const aData, aInfo: TBytes);
begin
  RaiseOnData(aFormat, aData, Length(aData), aInfo, Length(aInfo));
end;

procedure TMediaStreamDataSource_Ms3s.OnStreamDataReceiving(
  aSender: TMediaServerStream; var aDataPacketHeader: TStreamDataPacketHeader);
begin
  if ForcedVideoFrameIntervalMs>0 then
    aDataPacketHeader.DateTime:=IncMilliSecond(aDataPacketHeader.DateTime,-ForcedVideoFrameIntervalMs);
end;

(* procedure TMediaStreamDataSource_Ms3s.OnMotionAlarmHandler(Sender: THHNetServer;
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


function TMediaStreamDataSource_Ms3s.PtzSupported: boolean;
begin
  if FStream = nil then
    raise Exception.Create('Источник потока не открыт');

  result := FStream.PtzSupported;
end;

function TMediaStreamDataSource_Ms3s.PtzGetPoints: TMediaServerPtzPointInfoArray;
begin
  result := FStream.PtzGetPoints;
end;

procedure TMediaStreamDataSource_Ms3s.PtzInit;
begin
end;


initialization

MediaStreamDataSourceFactory.Register('ms3s', TMediaStreamDataSource_Ms3s);

end.
