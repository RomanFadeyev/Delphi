{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� ������ ������ �� ���������,   }
{                ��������������� �������� RTSP                                 }
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
unit MediaServer.Stream.Source.RTSP;

interface
  uses Windows, SysUtils, Classes, MediaStream.DataSource.RTSP, MediaServer.Stream.Source;

type
  //�����, ����������� ��������������� ��������� ������ (�����������) �� ������
  TMediaServerSourceRTSP = class (TMediaServerSourceBasedOnMediaStream)
  private
    FURL: string;
    FOverTCP: boolean;
  public
    constructor Create(const aURL: string;
                       aOverTCP: boolean;
                       aTransmitAudio: boolean; //���������� �� �����
                       aDataReceiveTimeout: integer //������� ��������� ������ �� ������
                       ); overload;

    destructor Destroy; override;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
  end;

implementation


{ TMediaServerSourceRTSP }

function TMediaServerSourceRTSP.ConnectionString: string;
begin
  result:=FURL;
  if FOverTCP then
    result:=result+' (RTP over TCP)';
end;

constructor TMediaServerSourceRTSP.Create(const aURL: string; aOverTCP: boolean;
                       aTransmitAudio: boolean; //���������� �� �����
                       aDataReceiveTimeout: integer //������� ��������� ������ �� ������
                       );
var
  aParams: TMediaStreamDataSourceConnectParams_RTSP;
begin
  aParams:=TMediaStreamDataSourceConnectParams_RTSP.Create(aURL,aOverTCP,true,aTransmitAudio);
  inherited Create(aParams, TMediaStreamDataSource_RTSP, aTransmitAudio, aDataReceiveTimeout);

  FURL:=aURL;
  FOverTCP:=aOverTCP;
end;

destructor TMediaServerSourceRTSP.Destroy;
begin
  inherited;
end;

function TMediaServerSourceRTSP.DeviceType: string;
begin
  result:='RTSP';
end;

function TMediaServerSourceRTSP.Name: string;
begin
  Result := FURL;
end;

end.

