{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат AvcAny                         }
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

unit Player.VideoOutput.AvcAny;

interface
  uses Windows,SysUtils,Classes, Player.VideoOutput.Base,MediaProcessing.Definitions,
       Avc, Avc.avcodec;

type
  TVideoOutputDecoder_AvcAny = class (TVideoOutputDecoder)
  private
    FTempBuffer : TBytes;
    FDecodedBufferSize: integer;
    FVideoDecoder: IVideoDecoder;
    FVideoDecoderStreamType: TStreamType;

    FLastErrorCode: FFMPEG_RESULT;

    procedure RecreateDecoder(aStreamType: TStreamType);
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
  uses uTrace;

procedure AvcTraceCallback(aType: cardinal; aLevel: cardinal; aMessage: PAnsiChar); stdcall;
begin
  TraceLine('AVC',Format('Type=%d; Level=%d; Message=%s',[aType,aLevel,aMessage]));
end;

//{$DEFINE TRACE_PROCESSING}
{ TVideoOutputDecoder_AvcAny }

constructor TVideoOutputDecoder_AvcAny.Create;
begin
  inherited Create;
  RegisterCustomTrace(ClassName,'','.vo.AvcAny');

//  RecreateDecoder;
end;

destructor TVideoOutputDecoder_AvcAny.Destroy;
begin
  inherited;
  FVideoDecoder:=nil;
end;

function TVideoOutputDecoder_AvcAny.GetCurrentDecodedBuffer(
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

function TVideoOutputDecoder_AvcAny.IsDataValid(
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

procedure TVideoOutputDecoder_AvcAny.OnUseHardwareAccelerationChanged;
begin
  inherited;
  FVideoDecoder:=nil;
end;

procedure TVideoOutputDecoder_AvcAny.RecreateDecoder(aStreamType: TStreamType);
var
  aCodecId: TAVCodecID;
begin
  FVideoDecoder:=nil;
  FVideoDecoderStreamType:=0;

  AvcCheck(GetDecoderIdByFourcc(aStreamType,aCodecId));
  if aCodecId=AV_CODEC_ID_NONE then
    raise Exception.CreateFmt('Нет кодека для обработки потока %s',[StreamTypeToFourccString(aStreamType)]);

  AvcCheck(CreateVideoDecoder(aCodecId, FVideoDecoder));
  FVideoDecoderStreamType:=aStreamType;
  Assert(FVideoDecoder<>nil);
end;

function TVideoOutputDecoder_AvcAny.StatusInfo: string;
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

class function TVideoOutputDecoder_AvcAny.SupportedStreamTypes: TArray<TStreamType>;
//var
//  i: integer;
begin
  result:=Avc.GetKnownVideoFourccArray;
  //for i := 0 to High(result) do
  //  TraceLine('Register Codec: '+StreamTypeToFourccString(result[i]));
end;

function TVideoOutputDecoder_AvcAny.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
var
  aNeedSize: integer;
  aParams: IVideoDecoderParams;
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtVideo);

  result:=drError;

  if (aDataSize<4) then
  begin
    SetLastError('Размер фрейма меньше 4 байт');
    exit;
  end;

  if FVideoDecoderStreamType<>aFormat.biStreamType then
    FVideoDecoder:=nil;

  if ((FVideoDecoder=nil) or (cardinal(aFormat.VideoWidth)<>ImageWidth) or (cardinal(aFormat.VideoHeight)<>ImageHeight)) then
  begin
    if FVideoDecoder=nil then
      RecreateDecoder(aFormat.biStreamType);

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

  {$IFDEF TRACE_PROCESSING}
  TraceLine(self.ClassName,Format('NalType=%d(%s); Result=%d; TimeStamp=%d; Prebuffer=%d',[integer(aNalType),NalTypeToString(aNalType),FLastErrorCode,aFormat.TimeStamp,integer(ffPrebuffer in aFormat.biFrameFlags)]));
  {$ENDIF}

  if FLastErrorCode=FR_SUCCESSFUL then
  begin
    if (FDecodedBufferSize>aNeedSize) then
      SetLastError('Decoded Buffer Size>Need Size')
       //do nothing, it's error
    else if (FDecodedBufferSize<>0) then
      result:=drSuccess;
  end
  else
    SetLastError('Декодер сообщил: '+GetErrorMessage(FLastErrorCode));
end;

procedure TVideoOutputDecoder_AvcAny.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  //FIX проверяем, чтобы декодированный буфер был НЕ больше рассчетного
  //Раньше было строгое равенство, но была ситуация, когда декодер обрабатывал только часть изображения
  //Из-за сложного формата AvcAny рассчитать правильно размеры картинки представляется сложным, поэтому идем на уступки
  Assert(cardinal(FDecodedBufferSize)<=ImageWidth*ImageHeight*3);
  aSurface.DrawRGB24(@FTempBuffer[0],ImageWidth*ImageHeight*3);
end;



initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_AvcAny);
end.


