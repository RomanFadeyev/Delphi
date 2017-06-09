{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат H264                         }
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

unit Player.VideoOutput.H264;

interface
  uses Windows,SysUtils,Classes, Player.VideoOutput.Base,MediaProcessing.Definitions,
       Avc,Avc.avcodec, H264Def;

type
  TVideoOutputDecoder_H264 = class (TVideoOutputDecoder)
  private
    FTempBuffer : TBytes;
    FDecodedBufferSize: integer;
    FVideoDecoder: IVideoDecoder;
    FLastErrorCode: FFMPEG_RESULT;

    procedure RecreateDecoder;
  protected
    procedure OnUseHardwareAccelerationChanged; override;
    function DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;

    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean; override;

    function GetCurrentDecodedBuffer(out aFormat: TMediaStreamDataHeader;
       out aData: pointer; out aDataSize: cardinal;
       out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess; override;

    constructor Create; override;
    destructor Destroy; override;

    function  StatusInfo: string; override;
  end;

implementation
  uses Math, uTrace;

procedure AvcTraceCallback(aType: cardinal; aLevel: cardinal; aMessage: PAnsiChar); stdcall;
begin
  TraceLine('AVC',Format('Type=%d; Level=%d; Message=%s',[aType,aLevel,aMessage]));
end;

//{$DEFINE TRACE_PROCESSING}

{ TVideoOutputDecoder_H264 }

constructor TVideoOutputDecoder_H264.Create;
begin
  inherited Create;
  RegisterCustomTrace(ClassName,'','.vo.h264');

  RecreateDecoder;
end;

destructor TVideoOutputDecoder_H264.Destroy;
begin
  inherited;
  FVideoDecoder:=nil;
end;

function TVideoOutputDecoder_H264.GetCurrentDecodedBuffer(
  out aFormat: TMediaStreamDataHeader; out aData: pointer;
  out aDataSize: cardinal;
  out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess;
begin
  aFormat.Clear;
  aFormat.biMediaType:=mtVideo;
  aFormat.biStreamType:=stRGB;
  aFormat.VideoWidth:=ImageWidth;
  aFormat.VideoHeight:=ImageHeight;
  aFormat.VideoBitCount:=24;
  aData:=FTempBuffer;
  aDataSize:=FDecodedBufferSize;
  aInfo:=nil;
  aInfoSize:=0;
  result:=dbaOK;
end;

function TVideoOutputDecoder_H264.IsDataValid(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal): boolean;
begin
  result:=inherited IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize);
  (*
  if result then
  begin
    result:=(aDataSize>4) and (PDword(aData)^=$1000000); //NAL_START_MARKER
    if result then
    begin
      result:=(PByte(aData)+4)^ shr 7=0;
    end;
  end;
  *)
end;

procedure TVideoOutputDecoder_H264.OnUseHardwareAccelerationChanged;
begin
  inherited;
  RecreateDecoder;
end;

procedure TVideoOutputDecoder_H264.RecreateDecoder;
begin
  FVideoDecoder:=nil;
  AvcCheck(CreateVideoDecoder(AV_CODEC_ID_H264, FVideoDecoder));
  Assert(FVideoDecoder<>nil);
end;

function TVideoOutputDecoder_H264.StatusInfo: string;
var
  aDecoderInfo: string;
begin
  result:=inherited StatusInfo;

  if FVideoDecoder<>nil then
  begin
    SetLength(aDecoderInfo,255);
    FVideoDecoder.GetDecoderInfo(PChar(aDecoderInfo),Length(aDecoderInfo));
    result:=result+'  DecoderInfo: '+PChar(aDecoderInfo)+#13#10;
  end;


end;

class function TVideoOutputDecoder_H264.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stH264;
end;

function TVideoOutputDecoder_H264.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
var
  aNeedSize: integer;
  aParams: IVideoDecoderParams;
  aNalType: TNalUnitType;

  {$IFDEF TRACE_PROCESSING}
  s: AnsiString;
  i: Integer;
  {$ENDIF}
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtVideo);

  result:=drError;

  if (aDataSize<4) then
  begin
    SetLastError('Размер фрейма меньше 4 байт');
    exit;
  end;
  (*
  if PInteger(aData)^<>$1000000 then
  begin
    SetLastError('Отсутствует лидирующий флаг (0x1000000). Возможно, это не H.264');
    exit;
  end;
  *)

  if ((cardinal(aFormat.VideoWidth)<>ImageWidth) or (cardinal(aFormat.VideoHeight)<>ImageHeight)) then
  begin
    AvcCheck(FVideoDecoder.GetParams(aParams));
    aParams.SetWidth(aFormat.VideoWidth);
    aParams.SetHeight(aFormat.VideoHeight);
    if aInfo<>nil then
      aParams.SetExtraData(aInfo,aInfoSize);

    aParams:=nil;
    try
      AvcCheck(FVideoDecoder.Open(UseHardwareAcceleration));
      SetImageSize(aFormat.VideoWidth,aFormat.VideoHeight);
    except
      on E:Exception do
      begin
        SetLastError(E.Message);
      end;
    end;
  end;


  //Assert(aFormat.VideoBitCount=24);
  aNeedSize:=aFormat.VideoWidth*aFormat.VideoHeight*3;
  if Length(FTempBuffer)<aNeedSize then
    SetLength(FTempBuffer,aNeedSize);

  FLastErrorCode:=FVideoDecoder.DecodeRGB24(aData,aDataSize, @FTempBuffer[0], aNeedSize, FDecodedBufferSize);
  if (FLastErrorCode=FR_DECODING_FAIL) and (ffKeyFrame in aFormat.biFrameFlags) then
    FLastErrorCode:=FVideoDecoder.DecodeRGB24(aData,aDataSize, @FTempBuffer[0], aNeedSize, FDecodedBufferSize);

  aNalType:=GetNalType(aData);

  {$IFDEF TRACE_PROCESSING}
  s:='';
  for i := 0 to Min(16,aDataSize) do
    s:=s+IntToHex(PByte(aData)[i],1);
  TraceLine(self.ClassName,Format('NalType=%d(%s); Result=%d; TimeStamp=%d; Prebuffer=%d; Size=%d; Bytes=$%s',[integer(aNalType),NalTypeToString(aNalType),FLastErrorCode,aFormat.TimeStamp,integer(ffPrebuffer in aFormat.biFrameFlags),aDataSize,s]));
  {$ENDIF}

  if FLastErrorCode=FR_SUCCESSFUL then
  begin
    if (FDecodedBufferSize>aNeedSize) then
      SetLastError('Decoded Buffer Size>Need Size')
       //do nothing, it's error
    else if (FDecodedBufferSize<>0) then
      result:=drSuccess;
  end
  else if (aNalType in [NAL_UT_SEQ_PARAM,NAL_UT_PIC_PARAM]) then
  begin
    result:=drWait;
  end
  else
    SetLastError('Декодер сообщил: '+GetErrorMessage(FLastErrorCode)+' [NAL='+IntToStr(integer(aNalType))+']');
end;

procedure TVideoOutputDecoder_H264.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  //FIX проверяем, чтобы декодированный буфер был НЕ больше рассчетного
  //Раньше было строгое равенство, но была ситуация, когда декодер обрабатывал только часть изображения
  //Из-за сложного формата H264 рассчитать правильно размеры картинки представляется сложным, поэтому идем на уступки
  Assert(cardinal(FDecodedBufferSize)<=ImageWidth*ImageHeight*3);
  aSurface.DrawRGB24(@FTempBuffer[0],ImageWidth*ImageHeight*3);
end;

initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_H264);

end.


