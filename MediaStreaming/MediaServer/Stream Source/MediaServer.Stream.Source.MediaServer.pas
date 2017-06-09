{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� ������ ������ �� ���������,   }
{                ��������������� �������� 3S MediaServer                       }
{                                                                              }
{  <�����>       ������ �.�.                                                   }
{                                                                              }
{  <����>        14.01.2011                                                    }
{                                                                              }
{  <����������>  ��� ����������.                                               }
{                                                                              }
{  <��������>    ��� ��� "���������-�����", ��� "�������"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaServer.Stream.Source.MediaServer;

interface
  uses Windows, SysUtils, SyncObjs, Classes, ExtCtrls,
  MediaServer.Stream.Source, MediaServer.Net.Ms3s.Stream,MediaServer.Net.Ms3s.Login,
  MediaProcessing.Definitions;

type
  //�����, ����������� ��������������� ��������� ������ (�����������) �� ������
  TMediaServerSourceMediaServer = class (TMediaServerSource)
  private
    FLock   : TCriticalSection;
    FStream : TMediaServerStream;

    FConnectParams_Ip: string;
    FConnectParams_Port: Word;
    FConnectParams_UserName: string;
    FConnectParams_UserPassword: string;
    FConnectParams_SourceName: string;
    FConnectParams_BandWidth: cardinal;
    FConnect_Lock: TCriticalSection;

    FLastStreamTypes : TAllMediaStreamTypes;

    FTransmitAudio : boolean; //���������� �� �����
    FConnect_ThreadHandle: THandle;

    procedure OnStreamDataReceived(aSender: TMediaServerStream; const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);
    procedure OnConnectionOKSync(aParams: pointer);
    procedure OnConnectionFailedSync(aParams: pointer);

    procedure PtzInit;
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(const aIP: string;
                       aPort: Word;
                       const aSourceName: string;
                       const aProtocol: {3S, reserved} cardinal;
                       const aUserName, aUserPassword: string;
                       aTransmitAudio: boolean; //���������� �� �����
                       aDataReceiveTimeout: integer //������� ��������� ������ �� ������
                       ); overload;

    destructor Destroy; override;

    procedure OnFrameReceived(const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);

    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;

//    property  ConnectParams: THHNetChannelConnectionParams read FConnectParams;

    //property  AVInfo: HHAV_INFO read GetChannelAVInfo;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
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

    //===== ����������� �� �������� �������
    //������
    procedure PtzMoveToPoint(aId: cardinal);override;
    //�������� � ��������� �������. ������� ����������� �� ��� X � Y � ��������
    procedure PtzMoveToPosition(const aPositionPan,aPositionTilt: double); override;
  end;

  TBewardThreadExceptionEvent = procedure (aExceptionSender: TObject; E:Exception) of object;
  TBewardTraceEvent = procedure (aSender: TObject; const aTraceMessage: string) of object;

implementation
  uses Math,Forms,MediaStream.UrlFormats, MediaServer.Workspace,ThreadNames, uTrace,uSync;

type
  TOpenConnectionOkParams = record
    Channel: TMediaServerStream;
  end;
  POpenConnectionOkParams = ^TOpenConnectionOkParams;

  TOpenConnectionFailedParams = record
    E: Exception;
  end;
  POpenConnectionFailedParams = ^TOpenConnectionFailedParams;

{ TMediaServerSourceMediaServer }

constructor TMediaServerSourceMediaServer.Create(
                       const aIP: string;
                       aPort: Word;
                       const aSourceName: string;
                       const aProtocol: {THHNetProtocol} cardinal;
                       const aUserName, aUserPassword: string;
                       aTransmitAudio: boolean; //���������� �� �����
                       aDataReceiveTimeout: integer //������� ��������� ������ �� ������
                       );
var
  i: TMediaType;
begin
  Create(aDataReceiveTimeout);

  FLock:=TCriticalSection.Create;
  FConnect_Lock:=TCriticalSection.Create;

  FConnectParams_Ip:=aIP;
  FConnectParams_Port:=aPort;
  FConnectParams_UserName:=aUserName;
  FConnectParams_UserPassword:=aUserPassword;
  FConnectParams_SourceName:=aSourceName;
  FConnectParams_BandWidth:=INFINITE; //TODO

  FTransmitAudio:=aTransmitAudio;

  for i := Low(TMediaType) to High(TMediaType) do
    FLastStreamTypes[i]:=stUNIV;
end;

destructor TMediaServerSourceMediaServer.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
  FreeAndNil(FConnect_Lock);
end;

function TMediaServerSourceMediaServer.DeviceType: string;
begin
  result:='�����-������';
end;

function TMediaServerSourceMediaServer.Name: string;
begin
  Result := MakeMs3sUrl(FConnectParams_Ip, FConnectParams_Port, FConnectParams_SourceName);
end;

procedure TMediaServerSourceMediaServer.OnFrameReceived(const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);
begin
  //���� �� ����� ���������� ����� ������, �� �������
  if not FTransmitAudio and (aFormat.biMediaType=mtAudio) then
    exit;

  LockStream;
  try
    FLastStreamTypes[aFormat.biMediaType]:=aFormat.biStreamType;
  finally
    UnlockStream;
  end;

  DoDataReceived(aFormat, @aData[0],Length(aData),@aInfo[0],Length(aInfo));
end;

procedure TMediaServerSourceMediaServer.OnStreamDataReceived(
  aSender: TMediaServerStream; const aFormat: TMediaStreamDataHeader;
  const aData, aInfo: TBytes);
begin
  Assert(self<>nil);
  OnFrameReceived(aFormat,aData,aInfo);
end;

procedure TMediaServerSourceMediaServer.OnConnectionFailedSync(aParams: pointer);
begin
  //������� ���� � ������ ����������� �� ��������� ����������
  StartReconnect;

  if Assigned(OnConnectionFailed) then
    OnConnectionFailed(self,POpenConnectionFailedParams(aParams).E);
end;

procedure TMediaServerSourceMediaServer.OnConnectionOKSync(aParams: pointer);
begin
  FreeAndNil(FStream); //�� ������ ������ ���� ��������� ��� ��� �������� �����������
  FStream:=POpenConnectionOkParams(aParams).Channel;

  DoConnectionOK;
  Assert(FStream<>nil);

  //���� ��� ������� ���������, ����������� ���������� �� ������ �� ������
  //��� ����� ������ ������ ����� ������������� �����, ����� ������ ��������� � ���, �����
  //�� ������� ����������������
  FStream.OnDataReceived.Add(OnStreamDataReceived);
end;

function ThreadConnectionProc(aRecordSource: TMediaServerSourceMediaServer): Integer;
var
  aOpenOk : TOpenConnectionOkParams;
  aOpenFailed: TOpenConnectionFailedParams;
  aLogin : TMediaServerLogin;
begin
  SetCurrentThreadName('MediaServer.Stream.Source.MediaServer.ThreadConnectionProc');
  result:=-1;

  if not (aRecordSource.Destroying or aRecordSource.Closing) then
    if (gLastCommand=rslcStart) then
      if not Application.Terminated then
      begin
        result:=0;
      end;

  if result<>0 then
    exit;

  ZeroMemory(@aOpenOk,sizeof(aOpenOk));

  //������������ ����� ����������� ������ ���� �����������, ������� ���������
  //��������� �������
  aRecordSource.FConnect_Lock.Enter;
  try

    //�����������
    try
      // ��������� �����
      aLogin:=TMediaServerLogin.Create(aRecordSource.FConnectParams_IP,aRecordSource.FConnectParams_Port);
      try
        //��� �� �� ����
        if IsEqualGUID(aLogin.ServerID,Workspace.ApplicationGUID) then
          raise Exception.Create('����������� ��������� � ������������ �����-������� ���������');

        aOpenOk.Channel:= aLogin.CreateStream(aRecordSource.FConnectParams_SourceName,aRecordSource.FConnectParams_UserName,aRecordSource.FConnectParams_UserPassword,nil,aRecordSource.FConnectParams_BandWidth);
      finally
        aLogin.Free;
      end;
    except
      on E:Exception do
      begin
        FreeAndNil(aOpenOk.Channel);
        aOpenFailed.E:=E;
        Sync.Synchronize(aRecordSource.OnConnectionFailedSync,@aOpenFailed);
      end;
    end;

    if aOpenOk.Channel<>nil then
      Sync.Synchronize(aRecordSource.OnConnectionOkSync,@aOpenOk);
  finally
    aRecordSource.FConnect_ThreadHandle:=0;
    aRecordSource.FConnect_Lock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.DoOpen(aSync: boolean);
var
  aThreadID: cardinal;
begin
  if Opened or (FConnect_ThreadHandle <> 0) then
    exit;

  if aSync then
    ThreadConnectionProc(self)
  else begin
    if FConnect_ThreadHandle=0 then //���� <>0, ������, ��� ����������� �����������
      FConnect_ThreadHandle:=BeginThread(nil, 0, @ThreadConnectionProc, self, 0, aThreadID);
  end;
end;

procedure TMediaServerSourceMediaServer.DoClose;
var
  i: TMediaType;
begin
  FLock.Enter;

  try
    //�������� �������
    if FConnect_ThreadHandle<>0 then
    begin
      Sync.PeekSynchronizationMessages;
      Sync.WaitWhileSynchronizationMessagesProcessed(FConnect_ThreadHandle);
      FConnect_ThreadHandle:=0;
    end;

    FreeAndNil(FStream);
    for i := Low(TMediaType) to High(TMediaType) do
      FLastStreamTypes[i]:=stUNIV;
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceMediaServer.ConnectionString: string;
begin
  result:=MakeMs3sUrl(FConnectParams_Ip,FConnectParams_Port,FConnectParams_SourceName);
end;

function TMediaServerSourceMediaServer.Opened: Boolean;
begin
  FLock.Enter;
  try
    result:=(FStream<>nil) and (FStream.Opened)
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceMediaServer.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TMediaServerSourceMediaServer.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  result:=FLastStreamTypes[aMediaType];
end;

procedure TMediaServerSourceMediaServer.PtzApertureDecrease;
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

procedure TMediaServerSourceMediaServer.PtzApertureDecreaseStop;
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

procedure TMediaServerSourceMediaServer.PtzApertureIncrease;
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

procedure TMediaServerSourceMediaServer.PtzApertureIncreaseStop;
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

procedure TMediaServerSourceMediaServer.PtzFocusIn;
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

procedure TMediaServerSourceMediaServer.PtzFocusInStop;
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

procedure TMediaServerSourceMediaServer.PtzFocusOut;
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

procedure TMediaServerSourceMediaServer.PtzFocusOutStop;
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

procedure TMediaServerSourceMediaServer.PtzInit;
begin
  CheckConnected;

  FLock.Enter;
  try
    //FStream.Ptz
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveDown(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveDown(0,aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveDownStop;
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

procedure TMediaServerSourceMediaServer.PtzMoveLeft(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveLeft(0,aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveLeftStop;
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

procedure TMediaServerSourceMediaServer.PtzMoveRight(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveRight(0,aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveRightStop;
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

procedure TMediaServerSourceMediaServer.PtzMoveToPoint(aId: cardinal);
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

procedure TMediaServerSourceMediaServer.PtzMoveToPosition(const aPositionPan,aPositionTilt: double);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;
    FStream.PtzMoveToPosition(aPositionPan,aPositionTilt);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveUp(aSpeed: byte);
begin
  FLock.Enter;
  try
    inherited;
    PtzInit;

    FStream.PtzMoveUp(0,aSpeed);
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzMoveUpStop;
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

function TMediaServerSourceMediaServer.PtzSupported: boolean;
begin
  FLock.Enter;

  try
    result:=true;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaServerSourceMediaServer.PtzZoomIn;
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

procedure TMediaServerSourceMediaServer.PtzZoomInStop;
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

procedure TMediaServerSourceMediaServer.PtzZoomOut;
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

procedure TMediaServerSourceMediaServer.PtzZoomOutStop;
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

procedure TMediaServerSourceMediaServer.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
  WaitForSingleObject(FConnect_ThreadHandle,aTimeout);
end;

end.

