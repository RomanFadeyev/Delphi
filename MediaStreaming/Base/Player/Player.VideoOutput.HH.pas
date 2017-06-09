{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат HH(Beward)                   }
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

unit Player.VideoOutput.HH;

interface
  uses Windows,SysUtils,Classes, Player.VideoOutput.Base,HHDecoder,HHCommon,MediaProcessing.Definitions;

type

  TVideoOutputDecoder_HH = class (TVideoOutputDecoder)
  private
    FVideoDecoder : THHVideoDecoder;
    FUncodedRgbBuffer: pointer;
    FUncodedRgbBufferSize: cardinal;

    FUncodedYuv420Buffer: pointer;
    FUncodedYuv420BufferSize: cardinal;
  protected
    function  DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;

    function GetCurrentDecodedBuffer(out aFormat: TMediaStreamDataHeader;
       out aData: pointer; out aDataSize: cardinal;
       out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess; override;

    procedure ResetBuffer; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ THHVideoOutputDirectX }

constructor TVideoOutputDecoder_HH.Create;
begin
  inherited Create;
  FVideoDecoder:=THHVideoDecoder.Create;
end;

destructor TVideoOutputDecoder_HH.Destroy;
begin
  inherited;
  FreeAndNil(FVideoDecoder);
end;

function TVideoOutputDecoder_HH.GetCurrentDecodedBuffer(
  out aFormat: TMediaStreamDataHeader; out aData: pointer;
  out aDataSize: cardinal; out aInfo: pointer;
  out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess;
begin
  aFormat.Clear;
  aFormat.biMediaType:=mtVideo;
  aFormat.VideoWidth:=ImageWidth;
  aFormat.VideoHeight:=ImageHeight;
  aFormat.VideoBitCount:=24;
  if FUncodedRgbBuffer<>nil then
  begin
    aFormat.biStreamType:=stRGB;
    aData:=FUncodedRgbBuffer;
    aDataSize:=FUncodedRgbBufferSize;
  end
  else if FUncodedYuv420Buffer<>nil then
  begin
    aFormat.biStreamType:=stYUV420;
    aData:=FUncodedYuv420Buffer;
    aDataSize:=FUncodedYuv420BufferSize;
  end
  else begin
    aFormat.biStreamType:=stRGB;
    aData:=FVideoDecoder.CurrentBmpDIB;
    aDataSize:=FVideoDecoder.CurrentBmpInfoHeader.biSizeImage;
  end;

  aInfo:=nil;
  aInfoSize:=0;
  result:=dbaOK;
end;

procedure TVideoOutputDecoder_HH.ResetBuffer;
begin
  inherited;
  FVideoDecoder.ResetBuffer;
end;

class function TVideoOutputDecoder_HH.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stHHVI;
end;

function TVideoOutputDecoder_HH.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
var
  aTempAVInfo:HHAV_INFO;
  pAVInfo: PHHAV_INFO;
  pFrameData: PHV_FRAME;
  aBuferReset: boolean;
begin
  result:=drError;

  Assert(PHV_FRAME_HEAD(aData).nByteNum+sizeof(HV_FRAME_HEAD)=aDataSize);
  pFrameData:=aData;

  if (aInfoSize=0) then
  begin
    pAVInfo:=@aTempAvInfo;
    ZeroMemory(@aTempAvInfo,sizeof(aTempAVInfo));

    if aFormat.biMediaType=mtVideo then
    begin
      PHHAV_INFO(pAVInfo).nVideoHeight:=aFormat.VideoHeight;
      PHHAV_INFO(pAVInfo).nVideoWidth:=aFormat.VideoWidth;
      PHHAV_INFO(pAVInfo).nVideoEncodeType:=aFormat.biStreamSubType;
    end
    else if aFormat.biMediaType=mtAudio then
    begin
      PHHAV_INFO(pAVInfo).nAudioEncodeType:=aFormat.biStreamSubType;
      PHHAV_INFO(pAVInfo).nAudioChannels:=aFormat.AudioChannels;
      PHHAV_INFO(pAVInfo).nAudioBits:=aFormat.AudioBitsPerSample;
      PHHAV_INFO(pAVInfo).nAudioSamples:=aFormat.AudioSamplesPerSec;
    end;
  end
  else begin
    Assert(aInfoSize=sizeof(HHAV_INFO));
    pAVInfo:=aInfo;
  end;

  Assert(aFormat.biMediaType=mtVideo);
  Assert(cardinal(aFormat.VideoWidth)=pAVInfo.nVideoWidth);
  Assert(cardinal(aFormat.VideoHeight)=pAVInfo.nVideoHeight);


  //Мусор?
  if pFrameData.zeroFlag<>0 then
    exit;
  if pFrameData.oneFlag<>1 then
    exit;

  FUncodedRgbBuffer:=nil;
  FUncodedYuv420Buffer:=nil;

  //Специальный режим, когда передается нежатая картинка RGB
  if (pFrameData.streamFlag = FRAME_FLAG_VI) and (pAVInfo.nVideoEncodeType=stRGB) then
  begin
    FUncodedRgbBufferSize:=pFrameData.nByteNum;

    if FUncodedRgbBufferSize<>cardinal(aFormat.VideoWidth*aFormat.VideoHeight*3) then
      exit;

    FUncodedRgbBuffer:=PAnsiChar(pFrameData)+sizeof(HV_FRAME_HEAD);
    SetImageSize(aFormat.VideoWidth,aFormat.VideoHeight);
  end
  //Специальный режим, когда передается нежатая картинка
  else if (pFrameData.streamFlag = FRAME_FLAG_VI) and (pAVInfo.nVideoEncodeType=stYUV420) then
  begin
    FUncodedYuv420BufferSize:=pFrameData.nByteNum;

    if FUncodedYuv420BufferSize<>cardinal(Trunc(aFormat.VideoWidth*aFormat.VideoHeight*1.5)) then
      exit;

    FUncodedYuv420Buffer:=PAnsiChar(pFrameData)+sizeof(HV_FRAME_HEAD);
    SetImageSize(aFormat.VideoWidth,aFormat.VideoHeight);
  end
  //Сжатая картинка, декодируем стандартным образом
  else begin
    aBuferReset:=false;
    if ((cardinal(aFormat.VideoWidth)<>FVideoDecoder.CurrentWidth) or (cardinal(aFormat.VideoHeight)<>FVideoDecoder.CurrentHeight)) then
    begin
      FVideoDecoder.ResetBuffer;
      aBuferReset:=true;
    end;

    if not FVideoDecoder.DecodeToBitmap(pFrameData,pAVInfo^,false) then
    begin
      //FIX: Если не удалось декодировать кадр, и при этом кадр опорный, то пытаемся
      //заставить декодер работать еще 2 раза. Как правило, это помогает
      //Чтобы не завести себя в 100% загрузки процессора, используем этот метод только
      //при флаге "был переинициализирован буфер декодера"
      if aBuferReset and (pFrameData.streamFlag = FRAME_FLAG_VI) then
      begin
        if not FVideoDecoder.DecodeToBitmap(pFrameData,pAVInfo^,false) then
          if not FVideoDecoder.DecodeToBitmap(pFrameData,pAVInfo^,false) then
            exit;
      end
      else begin
        exit;
      end;
    end;

    SetImageSize(FVideoDecoder.CurrentWidth,FVideoDecoder.CurrentHeight);
  end;

  result:=drSuccess;
end;

procedure TVideoOutputDecoder_HH.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  if FUncodedRgbBuffer<>nil then
  begin
    Assert(FUncodedRgbBufferSize=ImageWidth*ImageHeight*3);
    aSurface.DrawRGB24(FUncodedRgbBuffer,FUncodedRgbBufferSize,false);
  end
  else if FUncodedYuv420Buffer<>nil then
  begin
    Assert(FUncodedYuv420BufferSize=Trunc(ImageWidth*ImageHeight*1.5));
    aSurface.DrawYUV420(FUncodedYuv420Buffer,FUncodedYuv420BufferSize,false);
  end
  else begin
    Assert(FVideoDecoder.CurrentBmpDIB<>nil);
    Assert(cardinal(FVideoDecoder.CurrentBmpInfoHeader.biWidth)=ImageWidth);
    Assert(cardinal(FVideoDecoder.CurrentBmpInfoHeader.biHeight)=ImageHeight);
    Assert(FVideoDecoder.CurrentBmpInfoHeader.biBitCount=24);
    Assert(FVideoDecoder.CurrentBmpInfoHeader.biSizeImage=ImageWidth*ImageHeight*3);

    aSurface.DrawRGB24(FVideoDecoder.CurrentBmpDIB,FVideoDecoder.CurrentBmpInfoHeader.biSizeImage,false);
  end;
end;


initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_HH);

end.
