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

unit MediaStream.DataSource.HikVision;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base, MediaProcessing.Definitions, HikVision,HikVisionAPI;

type
  TMediaStreamDataSource_HV = class (TMediaStreamDataSource)
  private
    FChannelNo: integer;
    FChannelProfile: integer;
    FChannel: THVChannel ;
    FDataLock: TCriticalSection;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure OnDataReceived(aSender: THVChannel; aData: PByte; aDataSize: cardinal; const aFormat: THVChannelDataFormat);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;

    property Channel: THVChannel read FChannel;
  end;


  TMediaStreamDataSourceConnectParams_HV = class (TMediaStreamDataSourceConnectParams)
  private
    FChannelNo: integer;
    FChannelProfile: integer;
    FTransmitVideo: boolean;
    FTransmitAudio: boolean;
  public
    constructor Create; overload;
    constructor Create(
      aChannelNo: integer;
      aChannelProfile: integer;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
 end;



implementation
  uses uBaseClasses, MediaStream.UrlFormats;

{ TMediaStreamDataSourceConnectParams_HV }

procedure TMediaStreamDataSourceConnectParams_HV.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_HV;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_HV) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_HV;

  FChannelNo:=aSrc.FChannelNo;
  FChannelProfile:=aSrc.FChannelProfile;
  FTransmitVideo:=aSrc.FTransmitVideo;
  FTransmitAudio:=aSrc.FTransmitAudio;
end;

constructor TMediaStreamDataSourceConnectParams_HV.Create;
begin

end;

constructor TMediaStreamDataSourceConnectParams_HV.Create(aChannelNo: integer;  aChannelProfile: integer; aTransmitVideo, aTransmitAudio: boolean);
begin
  Create;

  FChannelNo:=aChannelNo;
  FChannelProfile:=aChannelProfile;
  FTransmitVideo:=aTransmitVideo;
  FTransmitAudio:=aTransmitAudio;
end;

procedure TMediaStreamDataSourceConnectParams_HV.Parse(const aUrl: string);
begin
  if not ParseHikVisionCompCardUrl(aUrl,FChannelNo,FChannelProfile) then
    RaiseParseError(aUrl);
end;

function TMediaStreamDataSourceConnectParams_HV.ToString: string;
begin
  MakeHikVisionCompCardUrl(FChannelNo,FChannelProfile)
end;

function TMediaStreamDataSourceConnectParams_HV.ToUrl(
  aIncludeAuthorizationInfo: boolean): string;
begin

end;

{ TMediaStreamDataSource_HV }

procedure TMediaStreamDataSource_HV.Start;
begin
  FDataLock.Enter;
  try
    FChannel.OnData:=OnDataReceived;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_HV.Stop;
begin
  FDataLock.Enter;
  try
    if FChannel<>nil then
      FChannel.OnData:=nil;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_HV.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_ : TMediaStreamDataSourceConnectParams_HV;
begin
  aConnectParams_:=aConnectParams as TMediaStreamDataSourceConnectParams_HV;
  FChannelNo:=aConnectParams_.FChannelNo;
  FChannelProfile:=aConnectParams_.FChannelProfile;
  FreeAndNil(FChannel);
  FChannel:=THVChannel.Create(FChannelNo,FChannelProfile);
end;

procedure TMediaStreamDataSource_HV.DoDisconnect;
begin
  FreeAndNil(FChannel);
end;

constructor TMediaStreamDataSource_HV.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
end;

class function TMediaStreamDataSource_HV.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_HV.Create;
end;

destructor TMediaStreamDataSource_HV.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FDataLock);
end;

function TMediaStreamDataSource_HV.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';

  if aError is EHikVisionAPIException then
  begin
    if EHikVisionAPIException(aError).ErrorCode=integer(ERR_DSP_BUSY) then
    begin
      result:='Карта видеозахвата перегружена запросами'
    end
    else if EHikVisionAPIException(aError).ErrorCode  = integer(ERR_OUTOF_MEMORY) then
    begin
      result:='Недостаточно оперативной памяти';
    end
  end;
end;

function TMediaStreamDataSource_HV.LastStreamDataTime: TDateTime;
begin
  if FChannel<>nil then
    result:=FChannel.LastStreamDataTime
  else
    result:=0;
end;


procedure TMediaStreamDataSource_HV.OnDataReceived(aSender: THVChannel;
  aData: PByte; aDataSize: cardinal; const aFormat: THVChannelDataFormat);
var
  aFormat_: TMediaStreamDataHeader;
begin
  FDataLock.Enter;
  try
    if (FChannel=nil) or not Assigned(FChannel.OnData) then
      exit;

    if aFormat.DataType=dtAudio then
    begin
      aFormat_.Clear;
      aFormat_.biMediaType:=mtAudio;
      aFormat_.biStreamType:=stPCM;
      Include(aFormat_.biFrameFlags,ffKeyFrame);
      aFormat_.AudioChannels:=aFormat.AudioChannels;
      aFormat_.AudioBitsPerSample:=aFormat.AudioBitsPerSample;
      aFormat_.AudioSamplesPerSec:=aFormat.AudioSamplesPerSec;
      RaiseOnData(aFormat_,aData,aDataSize,nil,0);
    end
    else if aFormat.DataType=dtVideo then
    begin
      Assert(PDWORD(aData)^=$1000000); //NAL_START_MARKER

      aFormat_.Clear;
      aFormat_.biMediaType:=mtVideo;
      aFormat_.biStreamType:=stH264;
      aFormat_.VideoWidth:=aFormat.VideoWidth;
      aFormat_.VideoHeight:=aFormat.VideoHeight;
      //aFormat_.biSizeImage:=aDataSize;
      //aFormat_.biBitCount:=24;
      if aFormat.IFrame then
        Include(aFormat_.biFrameFlags,ffKeyFrame);
      RaiseOnData(aFormat_,aData,aDataSize,nil,0);
    end;
  finally
    FDataLock.Leave;
  end;
end;

initialization
  MediaStreamDataSourceFactory.Register('hvcc',TMediaStreamDataSource_HV);

end.

