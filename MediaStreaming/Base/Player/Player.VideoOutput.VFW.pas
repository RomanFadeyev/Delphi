{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат-любой, использует            }
{                установленные в системе кодеки                                }
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

unit Player.VideoOutput.VFW;

interface
  uses Windows, SysUtils, Classes, Graphics, Messages, VFW,Player.VideoOutput.Base,MediaProcessing.Definitions,BitPlane;


type
  TVideoOutputDecoder_VFW = class (TVideoOutputDecoder)
  private
    FVideoDecoder: THandle;
    //FCVInited: boolean;
    FInInfo : TBitmapInfo;
    FOutInfo: PBitmapInfo;

    FDibBuffer: TBytes;
    FActualDibBufferSize: cardinal;
    FLastFailedDecompressorFCC: cardinal;
    FDecompressorDescription: string;
    FDecompressorName: string;
    FDecompressorDriver: string;
    FDecompressorFCC: string;

    procedure InitDecompressor(const aInInfo: TMediaStreamDataHeader; aImageSize: cardinal);
    procedure CloseDecompressor;
  protected
    function DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;

    function  StatusInfo: string; override;

    property  DecompressorFCC: string read FDecompressorFCC;
    property  DecompressorName: string read FDecompressorName;
    property  DecompressorDescription: string read FDecompressorDescription;
    property  DecompressorDriver: string read FDecompressorDriver;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses DirectImage;

{ TVideoOutputDecoder_VFW }

constructor TVideoOutputDecoder_VFW.Create;
begin
  inherited Create;
end;

destructor TVideoOutputDecoder_VFW.Destroy;
begin
  inherited;
  CloseDecompressor;
end;

procedure TVideoOutputDecoder_VFW.InitDecompressor(const aInInfo: TMediaStreamDataHeader;aImageSize: cardinal);
var
  aOutFormatSize: cardinal;
  aDecompressorFourCCString: string;

  aInputFormat : TBitmapInfoHeader;
  aOutputFormat: TBitmapInfoHeader;
  aBitCount: integer;

  aDecoderInfo: TICINFO;
begin
  aDecompressorFourCCString:=FOURCCTOSTR(aInInfo.biStreamType);
  Assert(FVideoDecoder=0);

  //Запоминаем входной формат
  FInInfo.bmiHeader:=aInInfo.ToBitmapInfoHeader(aImageSize);

  if aInInfo.biStreamType<>stRGB then
  begin
    //Чтобы при каждом кадре не было тормозов, запоминаем, что кодек не удалось создать, и ругаемся сразу
    if aInInfo.biStreamType=FLastFailedDecompressorFCC then
      raise Exception.CreateFmt('Ошибка открытия декодера для видео потока %s',[aDecompressorFourCCString]);

    Assert(aInInfo.biStreamType<>0);
    Assert(aInInfo.biMediaType=mtVideo);

    //Прежде чем обращаться к Drawer, нам нужно корректно проинициализировать контекст для рисования
    if (cardinal(aInInfo.VideoWidth)<>ImageWidth) or (cardinal(aInInfo.VideoHeight)<>ImageHeight) then
        SetImageSize(aInInfo.VideoWidth,aInInfo.VideoHeight);

    aBitCount:=24;

    if OutputSurface<>nil then
    begin
      //FEATURE! Нормальная ситуация, когда еще не создана поверхность. Но так как мы только что выставили размеры,
      //то на следующей итерации поверхность уже будет
      case OutputSurface.SurfaceFormat of
        RGB32:aBitCount:=32;
        RGB16: aBitCount:=16;
        RGB15: aBitCount:=15;
      end;
    end;

    aInputFormat:=aInInfo.ToBitmapInfoHeader(aImageSize);


    //Попробуем напрямую создать декодер по исходящему формату RGB24
    if FVideoDecoder=0 then
    begin
      ZeroMemory(@aOutputFormat,sizeof(aOutputFormat));
      aOutputFormat.biSize:=sizeof(aOutputFormat);
      aOutputFormat.biWidth:=aInInfo.VideoWidth;
      aOutputFormat.biHeight:=aInInfo.VideoHeight;
      aOutputFormat.biPlanes:=1;
      aOutputFormat.biBitCount:=aBitCount; //Ставим нашу глубину
      aOutputFormat.biCompression:=BI_RGB;

      FVideoDecoder:=ICDecompressOpen(ICTYPE_VIDEO,aInInfo.biStreamType,@aInputFormat,@aOutputFormat);
    end;

    //Попробуем напрямую создать декодер по исходящему формату RGB24
    if (FVideoDecoder=0) and (aOutputFormat.biBitCount<>24) then
    begin
      aOutputFormat.biBitCount:=24;
      aOutputFormat.biCompression:=BI_RGB;

      FVideoDecoder:=ICDecompressOpen(ICTYPE_VIDEO,aInInfo.biStreamType,@aInputFormat,@aOutputFormat);
    end;

    if FVideoDecoder=0 then
      FVideoDecoder:=ICOpen(ICTYPE_VIDEO,aInInfo.biStreamType,ICMODE_DECOMPRESS {ICMODE_FASTDECOMPRESS} );

    if FVideoDecoder=0 then
    begin
      FLastFailedDecompressorFCC:=aInInfo.biStreamType;
      raise Exception.CreateFmt('Ошибка открытия декодера для видео потока %s',[aDecompressorFourCCString]);
    end
    else begin
      if ICGetInfo(FVideoDecoder,@aDecoderInfo,sizeof(aDecoderInfo))<>0 then
      begin
        FDecompressorFCC:=FOURCCTOSTR(aDecoderInfo.fccHandler);
        FDecompressorName:=aDecoderInfo.szName;
        FDecompressorDescription:=aDecoderInfo.szDescription;
        FDecompressorDriver:=aDecoderInfo.szDriver;
      end
    end;

    aOutFormatSize:=ICDecompressGetFormatSize(FVideoDecoder,@FInInfo.bmiHeader);
    if integer(aOutFormatSize)<=0 then
      RaiseLastOSError;

    FOutInfo:=AllocMem(aOutFormatSize);
    FOutInfo.bmiHeader:=aOutputFormat;
  end
  else begin
    aOutFormatSize:=sizeof(TBitmapInfo);

    FOutInfo:=AllocMem(aOutFormatSize);
    //в cлучае RGB входной и выходной формат одинаковые
    FOutInfo.bmiHeader:=aInInfo.ToBitmapInfoHeader(aImageSize);
  end;

  //считаем размер картинки и выделяем буфер
  Assert(FOutInfo.bmiHeader.biWidth=aInInfo.VideoWidth);
  Assert(FOutInfo.bmiHeader.biHeight=aInInfo.VideoHeight);

  FOutInfo.bmiHeader.biSizeImage:=FOutInfo.bmiHeader.biWidth*FOutInfo.bmiHeader.biHeight*FOutInfo.bmiHeader.biBitCount div 8;
  SetLength(FDibBuffer,FOutInfo.bmiHeader.biSizeImage);
  FActualDibBufferSize:=FOutInfo.bmiHeader.biSizeImage;
  ZeroMemory(FDibBuffer,Length(FDibBuffer));

  try
    if aInInfo.biStreamType<>stRGB then
      ICCheck(ICDecompressBegin(FVideoDecoder,@FInInfo, @FOutInfo));
  except
    on E:Exception do
    begin
      ICClose(FVideoDecoder);
      FVideoDecoder:=0;
      raise Exception.CreateFmt('Ошибка инициализации декодера %s: %s',[aDecompressorFourCCString,E.Message]);
    end;
  end;
end;

function TVideoOutputDecoder_VFW.StatusInfo: string;
begin
  result:=inherited StatusInfo;

  result:=result+'VFW:'#13#10;
  result:=result+  Format('  Input Type:%s, %dx%d'#13#10,[FOURCCTOSTR(FInInfo.bmiHeader.biCompression),FInInfo.bmiHeader.biWidth,FInInfo.bmiHeader.biHeight]);
  result:=result+  Format('  Decoder created:%s'#13#10,[BoolToStr(FVideoDecoder<>0,true)]);

  if FVideoDecoder<>0 then
  begin
    result:=result+  Format('  Decoder FCC:%s'#13#10,[DecompressorFCC]);
    result:=result+  Format('  Decoder Name:%s'#13#10,[DecompressorName]);
    result:=result+  Format('  Decoder Desc:%s'#13#10,[DecompressorDescription]);
    result:=result+  Format('  Decoder Driver:%s'#13#10,[DecompressorDriver]);
  end;
end;

class function TVideoOutputDecoder_VFW.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stUNIV;
end;

function TVideoOutputDecoder_VFW.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
var
  aBitPlaneDesc: TBitPlaneDesc;
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtVideo);
  result:=drError;


  if (FInInfo.bmiHeader.biWidth<>aFormat.VideoWidth) or
     (FInInfo.bmiHeader.biHeight<>aFormat.VideoHeight) or
     (FInInfo.bmiHeader.biBitCount<>aFormat.VideoBitCount) or
     (FInInfo.bmiHeader.biCompression<>aFormat.biStreamType) then
    CloseDecompressor;

  if FVideoDecoder=0 then
    InitDecompressor(aFormat,aDataSize);

  Assert(FOutInfo.bmiHeader.biSizeImage=FActualDibBufferSize);

  if (aFormat.biStreamType=stRGB) then
  begin
    Assert(Length(FDibBuffer)>=integer(aDataSize));
    CopyMemory(@FDibBuffer[0],aData,aDataSize);

    SetImageSize(FOutInfo.bmiHeader.biWidth,FOutInfo.bmiHeader.biHeight);
    result:=drSuccess;
  end
  else if ICDecompress(FVideoDecoder,0,@FInInfo.bmiHeader,aData,@FOutInfo.bmiHeader,FDibBuffer)=ICERR_OK then
  begin
    SetImageSize(FOutInfo.bmiHeader.biWidth,FOutInfo.bmiHeader.biHeight);

    //TODO перенести функционал переворота непосредственно при рисовании! Это ускорит работу
    aBitPlaneDesc.Init(FDibBuffer,FActualDibBufferSize,FOutInfo.bmiHeader.biWidth,FOutInfo.bmiHeader.biHeight,FOutInfo.bmiHeader.biBitCount);
    aBitPlaneDesc.Upturn;

    result:=drSuccess;
  end;
end;

procedure TVideoOutputDecoder_VFW.CloseDecompressor;
begin
  if FVideoDecoder<>0 then
    ICClose(FVideoDecoder);
  FVideoDecoder:=0;
  FreeMem(FOutInfo);
  FOutInfo:=nil;
  FDibBuffer:=nil;
  FActualDibBufferSize:=0;

  FDecompressorFCC:='';
  FDecompressorName:='';
  FDecompressorDescription:='';
  FDecompressorDriver:='';
end;

procedure TVideoOutputDecoder_VFW.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  Assert(FActualDibBufferSize>0);
  //Assert(FOutInfo.bmiHeader.biBitCount=24,'Wrong PixelFormat (<>24bit)');

  Assert(FActualDibBufferSize=FOutInfo.bmiHeader.biSizeImage);

  if FOutInfo.bmiHeader.biBitCount=24 then
    aSurface.DrawRGB24(FDibBuffer,FActualDibBufferSize)
  else if FOutInfo.bmiHeader.biBitCount=32 then
    aSurface.DrawRGB32(FDibBuffer,FActualDibBufferSize)
  else if FOutInfo.bmiHeader.biBitCount=16 then
    aSurface.DrawRGB16(FDibBuffer,FActualDibBufferSize)
  else if FOutInfo.bmiHeader.biBitCount=15 then
    aSurface.DrawRGB15(FDibBuffer,FActualDibBufferSize)
  else
    Assert(false);
end;

initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_VFW);


end.
