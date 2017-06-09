{***********************************<_INFO>************************************}
{  <Проект>      Медиа-сервер                                                  }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Медиа-источник, предоставляющий чтение данных из источника,   }
{                поддерживающего протокол RTSP                                 }
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
unit MediaServer.Stream.Source.RTSP;

interface
  uses Windows, SysUtils, Classes, MediaStream.DataSource.RTSP, MediaServer.Stream.Source;

type
  //Класс, выполняющий непосредственно получение данных (видеопотока) из камеры
  TMediaServerSourceRTSP = class (TMediaServerSourceBasedOnMediaStream)
  private
    FURL: string;
    FOverTCP: boolean;
  public
    constructor Create(const aURL: string;
                       aOverTCP: boolean;
                       aTransmitAudio: boolean; //Записывать ли аудио
                       aDataReceiveTimeout: integer //таймаут получения данных от канала
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
                       aTransmitAudio: boolean; //Записывать ли аудио
                       aDataReceiveTimeout: integer //таймаут получения данных от канала
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

