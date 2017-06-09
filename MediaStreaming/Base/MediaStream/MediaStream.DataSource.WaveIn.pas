{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Источник медиа-потока, обеспечивающий получение данных c      }
{                платы видеозахвата HikVision                                  }
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

unit MediaStream.DataSource.WaveIn;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base, MediaProcessing.Definitions, MMSystem;

type
  TMediaStreamDataSource_WaveIn = class (TMediaStreamDataSource)
  private
    FWaveInHeader : WAVEHDR;
    FWaveInBuff   : PAnsiChar;
    FWaveInHandleLock: TCriticalSection;
    FWaveInHandle : HWAVEIN;
    FLastStreamDataTime : TDateTime;
    FWaveFormat:TWaveFormatEx;
    FStarted: boolean;
    FReceivingDataCounter: integer;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure OnReceiveData;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
  end;


  TMediaStreamDataSourceConnectParams_WaveIn = class (TMediaStreamDataSourceConnectParams)
  private
    FChannels,FSamplesPerSec,FBitsPerSample: cardinal;
    FDeviceID: NativeUInt;
  public
    constructor Create; overload;
    constructor Create(aChannels,aSamplesPerSec,aBitsPerSample: cardinal; aDeviceId: NativeUInt=INVALID_HANDLE_VALUE); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
 end;



implementation
  uses uBaseClasses, MediaStream.UrlFormats;

{ TMediaStreamDataSourceConnectParams_WaveIn }

procedure TMediaStreamDataSourceConnectParams_WaveIn.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_WaveIn;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_WaveIn) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_WaveIn;

  FChannels:=aSrc.FChannels;
  FSamplesPerSec:=aSrc.FSamplesPerSec;
  FBitsPerSample:=aSrc.FBitsPerSample;

end;

constructor TMediaStreamDataSourceConnectParams_WaveIn.Create;
begin

end;

constructor TMediaStreamDataSourceConnectParams_WaveIn.Create(aChannels,aSamplesPerSec,aBitsPerSample: cardinal; aDeviceId: NativeUInt=INVALID_HANDLE_VALUE);
begin
  Create;
  FChannels:=aChannels;
  FSamplesPerSec:=aSamplesPerSec;
  FBitsPerSample:=aBitsPerSample;
  FDeviceID:=aDeviceId;
end;

procedure TMediaStreamDataSourceConnectParams_WaveIn.Parse(const aUrl: string);
begin
  //if not ParseHikVisionCompCardUrl(aUrl,FChannelNo,FChannelProfile) then
  //  RaiseParseError(aUrl);
end;

function TMediaStreamDataSourceConnectParams_WaveIn.ToString: string;
begin
  result:=Format('Wave In %d/%d/%d',[FChannels,FSamplesPerSec,FBitsPerSample]);
end;

function TMediaStreamDataSourceConnectParams_WaveIn.ToUrl(
  aIncludeAuthorizationInfo: boolean): string;
begin

end;

{ TMediaStreamDataSource_WaveIn }

procedure TMediaStreamDataSource_WaveIn.Start;
begin
  if FWaveInHandle=0 then
    raise Exception.Create('Не инициализировано входное аудио');

  Win32Check(waveInAddBuffer(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader))=MMSYSERR_NOERROR);
  Win32Check(waveInStart(FWaveInHandle)=MMSYSERR_NOERROR);
  FStarted:=true;
end;

procedure TMediaStreamDataSource_WaveIn.Stop;
var
  i: Integer;
begin
  FStarted:=false;
  if FWaveInHandle<>0 then
  begin
    //TODO есть подозрение, что это нужно, чтоб выйти из обработчика OnReceiveData
    for i := 0 to 1000 do
    begin
      if FReceivingDataCounter=0 then
        break;
      sleep(1);
    end;


    waveInStop(FWaveInHandle);
    for i := 0 to 1000 do
    begin
      if FReceivingDataCounter=0 then
        break;
      sleep(1);
    end;
  end;
end;

procedure waveInProc(hwi: HWAVEIN; uMsg:UINT; dwInstance:DWORD ;dwParam1:DWORD ;dwParam2:DWORD ); stdcall;
var
  aInstance : TMediaStreamDataSource_WaveIn;
begin
  aInstance:=pointer(dwInstance);
  Assert(aInstance<>nil);

  if (uMsg=WIM_DATA) then
  begin
    aInstance.OnReceiveData;
  end;
end;

procedure TMediaStreamDataSource_WaveIn.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_ : TMediaStreamDataSourceConnectParams_WaveIn;
  aWaveInDevCaps: WAVEINCAPS;
  res: integer;
  aBuffSize: cardinal;
  aDenom : cardinal;
begin
  aConnectParams_:=aConnectParams as TMediaStreamDataSourceConnectParams_WaveIn;

  if aConnectParams_.FDeviceID=INVALID_HANDLE_VALUE then
  begin
    res:=waveInGetNumDevs;
    if res=0 then
      raise Exception.Create('Не обнаружено входное Audio-устройство');

    res:=waveInGetDevCaps(0,@aWaveInDevCaps,sizeof(aWaveInDevCaps));
  end
  else begin
    res:=waveInGetDevCaps(aConnectParams_.FDeviceID,@aWaveInDevCaps,sizeof(aWaveInDevCaps));
  end;

  if(res<>MMSYSERR_NOERROR) then
     raise Exception.Create('Не обнаружено входное Audio-устройство');

  FWaveFormat.wFormatTag:=WAVE_FORMAT_PCM;
  FWaveFormat.nChannels :=aConnectParams_.FChannels;
  FWaveFormat.wBitsPerSample:=aConnectParams_.FBitsPerSample;
  FWaveFormat.nSamplesPerSec:=aConnectParams_.FSamplesPerSec;
  FWaveFormat.nBlockAlign:=(FWaveFormat.wBitsPerSample div 8) * FWaveFormat.nChannels;
  FWaveFormat.nAvgBytesPerSec:=FWaveFormat.nBlockAlign * FWaveFormat.nSamplesPerSec;
  FWaveFormat.cbSize:= 0;

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
end;

procedure TMediaStreamDataSource_WaveIn.DoDisconnect;
begin
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

constructor TMediaStreamDataSource_WaveIn.Create;
begin
  inherited;
  FWaveInHandleLock:=TCriticalSection.Create;
end;

class function TMediaStreamDataSource_WaveIn.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_WaveIn.Create;
end;

destructor TMediaStreamDataSource_WaveIn.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FWaveInHandleLock);
end;

function TMediaStreamDataSource_WaveIn.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:=aError.Message;
end;

function TMediaStreamDataSource_WaveIn.LastStreamDataTime: TDateTime;
begin
  result:=FLastStreamDataTime;
end;


procedure TMediaStreamDataSource_WaveIn.OnReceiveData;
var
  aFormat: TMediaStreamDataHeader;
begin
  FLastStreamDataTime:=Now;
  if FStarted then
  begin
  //      aProcessed:=true;
    aFormat.Assign(FWaveFormat);
    RaiseOnData(aFormat,FWaveInBuff,FWaveInHeader.dwBufferLength,nil,0);

    if FStarted then
    begin
      inc(FReceivingDataCounter);
      try
        FWaveInHandleLock.Enter;
        try
          //поведение по умолчанию
          if FStarted then
            waveInAddBuffer(FWaveInHandle,@FWaveInHeader,sizeof(FWaveInHeader));
        finally
          FWaveInHandleLock.Leave;
        end;
      finally
        dec(FReceivingDataCounter);
      end;
    end;
  end
  else begin
  end;
end;

end.


