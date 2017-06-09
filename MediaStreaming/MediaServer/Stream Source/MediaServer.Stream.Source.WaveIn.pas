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
unit MediaServer.Stream.Source.WaveIn;

interface
  uses Windows, SysUtils, Classes, SyncObjs, MMSystem,uBaseClasses,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions,MediaStream.Framer;

type
  TMediaServerSourceWaveIn = class;

  //Класс, выполняющий непосредственно получение данных (видеопотока)
  TMediaServerSourceWaveIn = class (TMediaServerSource)
  private
    FDeviceID: NativeUInt;
    FWaveFormat:TWaveFormatEx;
    FWaveInHeader : WAVEHDR;
    FWaveInBuff   : PAnsiChar;
    FWaveInHandleLock: TCriticalSection;
    FWaveInHandle : HWAVEIN;
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
    procedure OnReceiveData;
  public
    constructor Create(aChannels,aSamplesPerSec,aBitsPerSample: cardinal; aDeviceID: NativeUInt=INVALID_HANDLE_VALUE);

    destructor Destroy; override;

    procedure OnFrameReceived(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);

    procedure Open(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;


    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    property WaveFormat:TWaveFormatEx read FWaveFormat;
  end;

implementation
  uses Math,Forms,ThreadNames, VFW, MediaServer.Workspace, uTrace,
       MediaStream.FramerFactory,MediaStream.UrlFormats;

{ TMediaServerSourceWaveIn }

constructor TMediaServerSourceWaveIn.Create(aChannels,aSamplesPerSec,aBitsPerSample: cardinal; aDeviceID: NativeUInt=INVALID_HANDLE_VALUE);
begin
  inherited Create(-1);
  FDeviceID:=aDeviceID;
  FWaveInHandleLock:=TCriticalSection.Create;
  FWaveFormat.wFormatTag:=WAVE_FORMAT_PCM;
  FWaveFormat.nChannels :=aChannels;
  FWaveFormat.wBitsPerSample:=aBitsPerSample;
  FWaveFormat.nSamplesPerSec:=aSamplesPerSec;
  FWaveFormat.nBlockAlign:=(FWaveFormat.wBitsPerSample div 8) * FWaveFormat.nChannels;
  FWaveFormat.nAvgBytesPerSec:=FWaveFormat.nBlockAlign * FWaveFormat.nSamplesPerSec;
  FWaveFormat.cbSize:= 0;
end;

destructor TMediaServerSourceWaveIn.Destroy;
begin
  inherited;
  FreeAndNil(FWaveInHandleLock);
end;

function TMediaServerSourceWaveIn.DeviceType: string;
begin
  result:='Файл';
end;

function TMediaServerSourceWaveIn.Name: string;
begin
  result:='Wave In';
end;

procedure waveInProc(hwi: HWAVEIN; uMsg:UINT; dwInstance:DWORD ;dwParam1:DWORD ;dwParam2:DWORD ); stdcall;
var
  aInstance : TMediaServerSourceWaveIn;
begin
  aInstance:=pointer(dwInstance);
  Assert(aInstance<>nil);

  if (uMsg=WIM_DATA) then
  begin
    aInstance.OnReceiveData;
  end;
end;

procedure TMediaServerSourceWaveIn.OnFrameReceived(
                                  const aFormat: TMediaStreamDataHeader;
                                  aData: pointer; aDataSize:cardinal;
                                  aInfo: pointer; aInfoSize: cardinal);
begin
  //Выносим за Lock потому что все операнды - локальные переменные
  DoDataReceived(aFormat, aData,aDataSize, aInfo,aInfoSize);
end;

procedure TMediaServerSourceWaveIn.OnReceiveData;
var
  aFormat: TMediaStreamDataHeader;
begin
  try
//      aProcessed:=true;
    aFormat.Assign(FWaveFormat);
    OnFrameReceived(aFormat,FWaveInBuff,FWaveInHeader.dwBufferLength,nil,0);

    FWaveInHandleLock.Enter;
    try
      //поведение по умолчанию
      waveInAddBuffer(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader));
    finally
      FWaveInHandleLock.Leave;
    end;
  except
    on E:Exception do
    begin
    end;
  end;
end;

procedure TMediaServerSourceWaveIn.Open(aSync: boolean);
var
  aWaveInDevCaps: WAVEINCAPS;
  res: integer;
  aBuffSize: cardinal;

  aDenom : cardinal;
begin
  if Opened then
    exit;

  Close;

  try
    if FDeviceID=INVALID_HANDLE_VALUE then
    begin
      res:=waveInGetNumDevs;
      if res=0 then
        raise Exception.Create('Не обнаружено входное Audio-устройство');

      res:=waveInGetDevCaps(0,@aWaveInDevCaps,sizeof(aWaveInDevCaps));
    end
    else begin
      res:=waveInGetDevCaps(FDeviceID,@aWaveInDevCaps,sizeof(aWaveInDevCaps));
    end;

    if(res<>MMSYSERR_NOERROR) then
       raise Exception.Create('Не обнаружено входное Audio-устройство');

    Win32Check(waveInOpen(@FWaveInHandle,WAVE_MAPPER,@FWaveFormat,dword(@waveInProc),dword(self),CALLBACK_FUNCTION)=MMSYSERR_NOERROR);

    //Размер буфера, используемый при записи входного потока аудио (с микрофона, линейного входа и прочее)
    {$IFDEF HHNET_LARGE_AUDIO_IN_BUFFER}
    aDenom:=12;
    {$ELSE}
    aDenom:=25;
    {$ENDIF}
    aBuffSize:=(FWaveFormat.nChannels*FWaveFormat.nSamplesPerSec* FWaveFormat.wBitsPerSample div 8) div  aDenom; //Буфер на один кадр видео, должно получиться 640
    FWaveInBuff:=pointer(GlobalAlloc(GMEM_FIXED or GMEM_NOCOMPACT or GMEM_NODISCARD, aBuffSize));
    if FWaveInBuff=nil then
      RaiseLastOSError;

    FWaveInHeader.lpData := FWaveInBuff;
    FWaveInHeader.dwBufferLength := aBuffSize;
    FWaveInHeader.dwFlags:=0;
    FWaveInHeader.dwLoops:=0;
    Win32Check(waveInPrepareHeader(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader))=MMSYSERR_NOERROR);

    if FWaveInHandle=0 then
      raise Exception.Create('Не инициализировано входное аудио');

    Win32Check(waveInAddBuffer(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader))=MMSYSERR_NOERROR);
    Win32Check(waveInStart(FWaveInHandle)=MMSYSERR_NOERROR);

    try
      DoConnectionOK;
    except
       //?????
    end;
  except
    on E:Exception do
      DoConnectionFailed(E);
  end;
end;

procedure TMediaServerSourceWaveIn.DoClose;
begin
  if FWaveInHandle<>0 then
  begin
    FWaveInHandleLock.Enter;
    try
      waveInStop(FWaveInHandle);
      sleep(50);
    finally
      FWaveInHandleLock.Leave;
    end;
  end;

  if FWaveInHandle<>0 then
  begin
    Stop;
    FWaveInHandleLock.Enter;
    try
      waveInUnprepareHeader(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader));
      waveInClose(FWaveInHandle);
      FWaveInHandle:=0;
    finally
      FWaveInHandleLock.Leave;
    end;
  end;

  if FWaveInBuff<>nil then
    GlobalFree(HGLOBAL(FWaveInBuff));
  FWaveInBuff:=nil;
end;

function TMediaServerSourceWaveIn.ConnectionString: string;
begin
  result:='Wave In';
end;

function TMediaServerSourceWaveIn.Opened: Boolean;
begin
  result:=FWaveInHandle<>0;
end;

function TMediaServerSourceWaveIn.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TMediaServerSourceWaveIn.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  if aMediaType=mtAudio then
    result:=stPCM
  else
    result:=0;
end;

function TMediaServerSourceWaveIn.PtzSupported: boolean;
begin
  result:=false;
end;

procedure TMediaServerSourceWaveIn.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;



end.

