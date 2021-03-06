{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� �����-������ ����� �������    }
{                ������                                                        }
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

unit MediaServer.Stream.Source.Relay;

interface
  uses Windows, SysUtils, Classes, SyncObjs,uBaseClasses,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  TMediaServerSourceRelay = class;
  TOpeningEvent = procedure (aSender: TMediaServerSourceRelay) of object;

  TMediaServerSourceRelay = class (TMediaServerSource)
  private
    FLastStreamTypes : TAllMediaStreamTypes;
    FOpened: boolean;
    FConnectionString: string;
    FOnOpening: TOpeningEvent;
    FName: string;
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(aDataReceiveTimeout: integer); overload;
    destructor Destroy; override;

    procedure OnFrameReceived(const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);

    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    procedure SetConnectionString(const aString: string);
    procedure SetName(const aName: string);

    property  OnOpening: TOpeningEvent read FOnOpening write FOnOpening;
  end;

implementation
  uses Math,MediaServer.Workspace,uTrace,MediaStream.FramerFactory,ThreadNames,MediaServer.Net.Ms3s.StreamClient;


{ TMediaServerSourceRelay }

constructor TMediaServerSourceRelay.Create(
  aDataReceiveTimeout: integer //������� ��������� ������ �� ������
  );
var
  i: TMediaType;
begin
  inherited Create(aDataReceiveTimeout);

  for i := Low(TMediaType) to High(TMediaType) do
    FLastStreamTypes[i]:=stUNIV;
end;

destructor TMediaServerSourceRelay.Destroy;
begin
  inherited;
end;

function TMediaServerSourceRelay.DeviceType: string;
begin
  result:='Stream Relay';
end;

function TMediaServerSourceRelay.Name: string;
begin
  result:=FName;
end;

procedure TMediaServerSourceRelay.OnFrameReceived(const aFormat: TMediaStreamDataHeader; const aData,aInfo: TBytes);
begin
//  //���� �� ����� ���������� ����� ������, �� �������
//if not FTransmitAudio and (aFormat.biMediaType=mtAudio) then
//    exit;

  LockStream;
  try
    FLastStreamTypes[aFormat.biMediaType]:=aFormat.biStreamType;
  finally
    UnlockStream;
  end;

  DoDataReceived(aFormat, @aData[0],Length(aData),@aInfo[0],Length(aInfo));
end;

procedure TMediaServerSourceRelay.DoOpen(aSync: boolean);
begin
  if Opened then
    exit;

  Close;
  if Assigned(FOnOpening) then
    FOnOpening(self);

  FOpened:=true;

  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSourceRelay.DoClose;
begin
  FOpened:=false;
end;

function TMediaServerSourceRelay.ConnectionString: string;
begin
  result:=FConnectionString;
end;

function TMediaServerSourceRelay.Opened: Boolean;
begin
  result:=FOpened;
end;

procedure TMediaServerSourceRelay.SetConnectionString(const aString: string);
begin
  FConnectionString:=aString;
end;

procedure TMediaServerSourceRelay.SetName(const aName: string);
begin
  FName:=aName;
end;

function TMediaServerSourceRelay.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TMediaServerSourceRelay.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  result:=FLastStreamTypes[aMediaType];
end;

function TMediaServerSourceRelay.PtzSupported: boolean;
begin
  result:=false;
end;

procedure TMediaServerSourceRelay.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;

end.

