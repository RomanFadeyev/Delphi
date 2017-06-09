{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� ������ � Web-������           }
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
unit MediaServer.Stream.Source.WebCamera;

interface
  uses Windows, SysUtils, SyncObjs, Classes, ExtCtrls,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  //�����, ����������� ��������������� ��������� ������ (�����������) �� ������
  TMediaServerSourceWebCamera = class (TMediaServerSource)
  private
    FLock : TCriticalSection;

    FDeviceName: string;
    FTransmitAudio : boolean; //���������� �� �����
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(const aDeviceName: string;
                       aTransmitAudio: boolean //���������� �� �����
                       ); overload;

    destructor Destroy; override;


    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;
  end;

implementation
  uses Math,Forms,MediaServer.Workspace,uTrace;


{ TMediaServerSourceWebCamera }

constructor TMediaServerSourceWebCamera.Create(const aDeviceName: string;
                       aTransmitAudio: boolean //���������� �� �����
                       );
begin
  Create(-1);

  FLock:=TCriticalSection.Create;
  FTransmitAudio:=aTransmitAudio;
end;

destructor TMediaServerSourceWebCamera.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
end;

function TMediaServerSourceWebCamera.DeviceType: string;
begin
  result:='Web-������';
end;

function TMediaServerSourceWebCamera.Name: string;
begin
  Result := Format('WebCam %s', [FDeviceName])
end;

procedure TMediaServerSourceWebCamera.DoOpen(aSync: boolean);
begin
  if Opened then
    exit;

  Close;

  {TODO}
end;

procedure TMediaServerSourceWebCamera.DoClose;
begin
  FLock.Enter;

  try
    {TODO}
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceWebCamera.ConnectionString: string;
begin
  result:=Format('%s:%s',['WebCam',FDeviceName]);
end;

function TMediaServerSourceWebCamera.Opened: Boolean;
begin
  FLock.Enter;
  try
    result:=false; {TODO}
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceWebCamera.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TMediaServerSourceWebCamera.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  result:=0; //TODO
end;


procedure TMediaServerSourceWebCamera.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;

end.

