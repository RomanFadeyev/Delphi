{***********************************<_INFO>************************************}
{  <Проект>      Медиа-сервер                                                  }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Медиа-источник, предоставляющий чтение данных из файла        }
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
unit MediaServer.Stream.Source.File_;

interface
  uses Windows, SysUtils, Classes, SyncObjs,uBaseClasses,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions,MediaStream.Framer, MediaStream.DataSource.File_;

type
  TMediaServerSourceFile = class;
  TFileFinishedEvent = procedure (Sender: TMediaServerSourceFile; const aFileName: string) of object;

  //Класс, выполняющий непосредственно получение данных (видеопотока)
  TMediaServerSourceFile = class (TMediaServerSourceBasedOnMediaStream)
  private
    FFileName: string;
    //FOnFileFinished: TFileFinishedEvent;
  public
    constructor Create(const aFileName: string;
                       aTransmitAudio: boolean //Записывать ли аудио
                      ); overload;

    destructor Destroy; override;


    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;

    function PtzSupported: boolean; override;


    //property OnFileFinished: TFileFinishedEvent read FOnFileFinished write FOnFileFinished;
    //property Framer: TStreamFramer read FFramer;
  end;

implementation
  uses Math,Forms,ThreadNames, VFW, MediaServer.Workspace, uTrace,
       MediaStream.FramerFactory,MediaStream.UrlFormats;

{ TMediaServerSourceFile }

constructor TMediaServerSourceFile.Create(const aFileName: string;aTransmitAudio: boolean);
var
  aParams: TMediaStreamDataSourceConnectParams_File;
begin
  aParams:=TMediaStreamDataSourceConnectParams_File.Create(aFileName,true,aTransmitAudio);
  inherited Create(aParams, TMediaStreamDataSource_File, aTransmitAudio, -1);

  FFileName:=aFileName;
end;

destructor TMediaServerSourceFile.Destroy;
begin
  inherited;
end;

function TMediaServerSourceFile.DeviceType: string;
begin
  result:='Файл';
end;

function TMediaServerSourceFile.Name: string;
begin
  result:=FFileName;
end;

function TMediaServerSourceFile.ConnectionString: string;
begin
  result:=MakeFileUrl(FFileName);
end;

function TMediaServerSourceFile.PtzSupported: boolean;
begin
  result:=false;
end;


end.

