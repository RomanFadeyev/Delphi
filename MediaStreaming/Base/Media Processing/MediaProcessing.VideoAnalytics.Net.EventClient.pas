unit MediaProcessing.VideoAnalytics.Net.EventClient;

interface
uses
  Windows, Messages, SysUtils, Classes,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,IdStreamVCL, IdGlobal,SyncObjs,
  MediaProcessing.VideoAnalytics.Net.EventDefinitions,MediaProcessing.Definitions;

type
  TVaepClient = class
  private
    FClient : TIdTCPClient;
    FLock   : TCriticalSection;
    FHost   : string;
    FPort   : word;
    FUserName: string;
    FUserPassword: string;

    function  GetHost: string;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: word);

    procedure DestroyClient;
  protected
    procedure Lock;
    procedure Unlock;

    procedure CheckConnected;

    procedure InitClient; virtual;
    function  Client: TIdTCPClient;
  public
    constructor Create(const aHost: string; aPort: Word=VaepProtocolDefaultPort; const aUserName: string=VaepSuperUserName; aUserPassword: string=VaepSuperUserPassword);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    function  Connected:boolean;

    property  Host: string read GetHost write SetHost;
    property  Port: word read FPort write SetPort;

    function  Listen(aTimeout: integer=integer(INFINITE)):TVaepNotify;
  end;


  EVaepClientError = class (Exception);

implementation
  uses NetworkUtils,CryptUtils;

{ TVaepClient }

procedure TVaepClient.CheckConnected;
begin
  Lock;
  try
    if (FClient=nil) or (not FClient.Connected) then
      raise EVaepClientError.Create('��� ����������');
  finally
    Unlock;
  end;
end;

procedure TVaepClient.Connect;
var
  aLoginParams: TVaepLoginParams;
  aLoginResult: TVaepLoginResult;
  aBytes: TBytes;
  aDigest: string;
  aSize: integer;
begin
  DestroyClient; //���� ��������,��������� � ���, ��� ����� ������ ���������� ����� ������ �� �����������������. ��������, ��� ������� � �����������. ������� ��������� ��� ������ ���
  if Client.Host='' then
    raise EVaepClientError.Create('�� ������ ����� �������');
  try
    Client.Connect;

    aDigest:=MD5Encode(Format('%s:%s',[FUserName,FUserPassword]));
    aLoginParams:=TVaepLoginParams.Create(VaepProtocolVerion,FUserName,aDigest,ExtractFileName(ParamStr(0)));
    try
      aLoginParams.SaveToBytes(aBytes);
      Client.IOHandler.Write(Length(aBytes));
      Client.IOHandler.Write(aBytes);
    finally
      aLoginParams.Free;
    end;


    aSize:=Client.IOHandler.ReadLongInt();
    aBytes:=nil;
    Client.IOHandler.ReadBytes(aBytes,aSize);

    aLoginResult:=TVaepLoginResult.Create();
    try
      aLoginResult.LoadFromBytes(aBytes);
      if aLoginResult.Code<>iceOK then
        raise EVaepClientError.Create(VaepLoginResultCodeToString(aLoginResult.Code));
    finally
      aLoginResult.Free;
    end;
  except
    on E:Exception do
      raise EVaepClientError.Create('������ ����������: '+E.Message);
  end;
  Assert(Client.Connected,'������� ������ �� �������� ������� ��������');
end;
//------------------------------------------------------------------------------
function TVaepClient.Connected: boolean;
begin
  result:=false;
  try
    result:=(FClient<>nil) and FClient.Connected;
  except
    Disconnect;
  end;
end;
//------------------------------------------------------------------------------
constructor TVaepClient.Create(const aHost: string; aPort: Word=VaepProtocolDefaultPort; const aUserName: string=VaepSuperUserName; aUserPassword: string=VaepSuperUserPassword);
begin
  FLock:=TCriticalSection.Create;

  self.Host:=aHost;
  self.Port:=aPort;
  self.FUserName:=aUserName;
  self.FUserPassword:=aUserPassword;
end;
//------------------------------------------------------------------------------
destructor TVaepClient.Destroy;
begin
  inherited;
  FreeAndNil(FClient);
  FreeAndNil(FLock);
end;
//------------------------------------------------------------------------------
function TVaepClient.GetHost: string;
begin
  result:=FHost;
end;
//------------------------------------------------------------------------------
procedure TVaepClient.SetHost(const Value: string);
begin
  if Host=Value then
    exit;

  FHost:=Value;
  DestroyClient;
end;
//------------------------------------------------------------------------------
procedure TVaepClient.Lock;
begin
  FLock.Enter;
end;
//------------------------------------------------------------------------------
function TVaepClient.Listen(aTimeout: integer):TVaepNotify;
var
  aMarker: cardinal;
  aDataSize: cardinal;
  aData: TBytes;

  aNotifyType: TVaepNotifyType;
begin
  CheckConnected;

  if aTimeout<0 then
    FClient.ReadTimeout:=IdTimeoutInfinite
  else
    FClient.ReadTimeout:=aTimeout;

  aMarker:=FClient.IOHandler.ReadLongWord;
  if aMarker<>VaepFrameBeginMarker then
    raise EVaepClientError.Create('������ ������ ������ � �������');

  aNotifyType:=TVaepNotifyType(FClient.IOHandler.ReadLongWord);
  aDataSize:=FClient.IOHandler.ReadLongWord;
  aData:=nil;
  FClient.IOHandler.ReadBytes(aData,aDataSize);

  aMarker:=FClient.IOHandler.ReadLongWord;
  if aMarker<>VaepFrameEndMarker then
    raise EVaepClientError.Create('������ ������ ������ � �������');


  result:=GetNotifyClass(aNotifyType).CreateFromBytes(aData);
end;
//------------------------------------------------------------------------------
procedure TVaepClient.Unlock;
begin
  FLock.Leave;
end;
//------------------------------------------------------------------------------
procedure TVaepClient.DestroyClient;
begin
  FreeAndNil(FClient);
end;
//------------------------------------------------------------------------------
procedure TVaepClient.Disconnect;
begin
  try
    FClient.Disconnect;
  except
    //���� ���������� ��� ��������, ����� ������. ��� ��� �� ���������
  end;
end;

//------------------------------------------------------------------------------
procedure TVaepClient.InitClient;
begin
  FClient.Port:=FPort;
  FClient.Host:=AddressToIp(FHost);
end;
//------------------------------------------------------------------------------
procedure TVaepClient.SetPort(const Value: word);
begin
  FPort := Value;
end;
//------------------------------------------------------------------------------
function TVaepClient.Client: TIdTCPClient;
begin
  if FClient=nil then
  begin
    FClient:=TIdTCPClient.Create(nil);
    InitClient;
  end;

  result:=FClient;
end;

end.
