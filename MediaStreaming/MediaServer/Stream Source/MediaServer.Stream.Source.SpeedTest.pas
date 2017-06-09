{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� �������� ������               }
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

unit MediaServer.Stream.Source.SpeedTest;

interface
  uses Windows, SysUtils, Classes, SyncObjs,uBaseClasses,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  //�����, ����������� ��������������� ��������� ������ (�����������) �� ������
  TMediaServerSourceSpeedTest = class (TMediaServerSource)
  private
    FEmitThread : TThreadObjectVar<TThread>;

    procedure OnFrameReceived(aMediaType: TMediaType;
                              aData: pointer; aDataSize:cardinal;
                              const aFormat: TMediaStreamDataHeader;
                              aInfo: pointer; aInfoSize: cardinal);
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create; overload;
    destructor Destroy; override;


    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;


    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    //property Framer: TStreamFramer read FFramer;
  end;

implementation
  uses Math,Forms,MediaServer.Workspace,uTrace,MediaStream.FramerFactory,ThreadNames;

type
  TSpeedTestThread = class (TThread)
  private
    FOwner: TMediaServerSourceSpeedTest;
    FOpenLock: TCriticalSection;

    function StreamInfo: TBytes;
    //function StreamType: TStreamType;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TMediaServerSourceSpeedTest);
    destructor Destroy; override;
  end;

{ TSpeedTestThread }

constructor TSpeedTestThread.Create(aOwner: TMediaServerSourceSpeedTest);
begin
  FOwner:=aOwner;
  FOpenLock:=TCriticalSection.Create;

  inherited Create(false);
end;

destructor TSpeedTestThread.Destroy;
begin
  inherited;
  FreeAndNil(FOpenLock);
end;

function TSpeedTestThread.StreamInfo: TBytes;
begin
  FOpenLock.Enter;
  try
    result:=nil;
  finally
    FOpenLock.Leave;
  end;
end;

procedure TSpeedTestThread.Execute;
var
  gDummyData : TBytes;
  gDummyFormat : TMediaStreamDataHeader;
begin
  SetCurrentThreadName('Source: '+ClassName);
  SetLength(gDummyData,1024*100); //100 �����

  gDummyFormat.Clear;
  gDummyFormat.biStreamType:=stBinary;
  Include(gDummyFormat.biFrameFlags,ffKeyFrame);

  while not Terminated do
  begin
    try
      FOwner.OnFrameReceived(mtVideo,gDummyData,Length(gDummyData),gDummyFormat,nil,0);
      sleep(1);
    except
      on E:Exception do
        ;
    end;
  end;
end;

{ TMediaServerSourceSpeedTest }

constructor TMediaServerSourceSpeedTest.Create();
begin
  Create(-1);
  FEmitThread:=TThreadObjectVar<TThread>.Create;
  //UsePrebuffer:=false;
end;

destructor TMediaServerSourceSpeedTest.Destroy;
begin
  inherited;
  FreeAndNil(FEmitThread);
end;

function TMediaServerSourceSpeedTest.DeviceType: string;
begin
  result:='Virtual';
end;

function TMediaServerSourceSpeedTest.Name: string;
begin
  result:='Speed Test';
end;

procedure TMediaServerSourceSpeedTest.OnFrameReceived(aMediaType: TMediaType;
                                  aData: pointer; aDataSize:cardinal;
                                  const aFormat: TMediaStreamDataHeader;
                                  aInfo: pointer; aInfoSize: cardinal);
begin
  DoDataReceived(aFormat, aData,aDataSize, aInfo,aInfoSize);
end;

procedure TMediaServerSourceSpeedTest.DoOpen(aSync: boolean);
begin
  if Opened then
    exit;

  Close;
  FEmitThread.Value:=TSpeedTestThread.Create(self);

  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSourceSpeedTest.DoClose;
begin
  FEmitThread.FreeValue;
end;

function TMediaServerSourceSpeedTest.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  result:=stBinary;
end;

function TMediaServerSourceSpeedTest.ConnectionString: string;
begin
  result:='';
end;

function TMediaServerSourceSpeedTest.Opened: Boolean;
begin
  result:=FEmitThread.Value<>nil;
end;

function TMediaServerSourceSpeedTest.StreamInfo: TBytes;
begin
  FEmitThread.Lock;
  try
    CheckConnected;
    result:=TSpeedTestThread(FEmitThread.Value).StreamInfo;
  finally
    FEmitThread.Unlock;
  end;
end;

function TMediaServerSourceSpeedTest.PtzSupported: boolean;
begin
  result:=false;
end;

procedure TMediaServerSourceSpeedTest.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;



end.

