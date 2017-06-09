{***********************************<_INFO>************************************}
{  <Проект>      Медиа-сервер                                                  }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Медиа-источник, предоставляющий связь с платой                }
{                видео-захвата HikVision                                       }
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

unit MediaServer.Stream.Source.Hikvision;

interface
  uses Windows, SysUtils, SyncObjs, Classes, ExtCtrls, Hikvision,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  //Класс, выполняющий непосредственно получение данных (видеопотока)
  TMediaServerSourceHikvision = class (TMediaServerSource)
  private
    FLock : TCriticalSection;
    FChannelNo: integer;
    FChannelProfile: integer;
    FChannel: THVChannel;
    FTransmitAudio : boolean; //Записывать ли аудио

    procedure OnDataReceived(aSender: THVChannel; aData: PByte; aDataSize: cardinal; const aFormat: THVChannelDataFormat);
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(aChannelNo: integer;
                       aChannelProfile: integer;
                       aTransmitAudio: boolean //Записывать ли аудио
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

{ TMediaServerSourceHikvision }

constructor TMediaServerSourceHikvision.Create(aChannelNo: integer; aChannelProfile: integer;
                       aTransmitAudio: boolean //Записывать ли аудио
                       );
begin
  Create(-1);
  FLock:=TCriticalSection.Create;

  FChannelNo:=aChannelNo;
  FChannelProfile:=aChannelProfile;
  FTransmitAudio:=aTransmitAudio;
end;

destructor TMediaServerSourceHikvision.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
end;

function TMediaServerSourceHikvision.DeviceType: string;
begin
  result:='Компр. карта Hikvision';
end;

function TMediaServerSourceHikvision.Name: string;
begin
  Result := Format('PCI, канал %d/%d', [FChannelNo+1,FChannelProfile+1])
end;

procedure TMediaServerSourceHikvision.OnDataReceived(aSender: THVChannel; aData: PByte; aDataSize: cardinal; const aFormat: THVChannelDataFormat);
var
  aFormat_: TMediaStreamDataHeader;
begin
  if not FTransmitAudio and (aFormat.DataType=dtAudio) then
    exit;

  //из-за эфекта гонок закрытие потока может совпать с текущей обработкой
  if not (aSender.Opened) then
    exit;

  aFormat_.Clear;
  aFormat_.biMediaType:=mtVideo;
  aFormat_.biStreamType:=stH264;
  aFormat_.VideoWidth:=aFormat.VideoWidth;
  aFormat_.VideoHeight:=aFormat.VideoHeight;
  //aFormat_.DataSize:=aDataSize;
  //aFormat_.biBitCount:=24;
  if aFormat.IFrame then
    Include(aFormat_.biFrameFlags,ffKeyFrame);

  DoDataReceived(aFormat_, aData,aDataSize, nil,0);
end;

procedure TMediaServerSourceHikvision.DoOpen(aSync: boolean);
begin
  if Opened then
    exit;

  Close;
  try
    FChannel:=THVChannel.Create(FChannelNo,FChannelProfile);
    FChannel.OnData:=OnDataReceived;

    if Assigned(OnConnectionOk) then
      OnConnectionOk(self);
  except
    if aSync then
      raise
    else
      StartReconnect;
  end;
end;

procedure TMediaServerSourceHikvision.DoClose;
begin
  FLock.Enter;

  try
    FreeAndNil(FChannel);
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceHikvision.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  if aMediaType=mtVideo then
    result:=stH264
  else
    result:=stUNIV; //TODO Audio
end;

function TMediaServerSourceHikvision.ConnectionString: string;
begin
  result:=Format('%s %d/%d',['PCI',FChannelNo+1,FChannelProfile+1]);
end;

function TMediaServerSourceHikvision.Opened: Boolean;
begin
  FLock.Enter;
  try
    result:=FChannel<>nil;
  finally
    FLock.Leave;
  end;
end;

function TMediaServerSourceHikvision.StreamInfo: TBytes;
begin
  result:=nil;
end;

procedure TMediaServerSourceHikvision.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;

end.

