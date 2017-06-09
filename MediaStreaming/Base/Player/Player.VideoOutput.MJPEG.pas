{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат MJPEG                        }
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

unit Player.VideoOutput.MJPEG;

interface
  uses Windows, SysUtils, Classes, Messages,Controls,Graphics,JPeg,Player.VideoOutput.Base,MediaProcessing.Definitions;


type
  TVideoDecoder = class (TJPEGImage)
  end;

  TVideoOutputDecoder_MJPEG = class (TVideoOutputDecoder)
  private
    FVideoDecoder : TVideoDecoder;
    FReverseVertical:boolean;
  protected
    function DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;

    function GetCurrentDecodedBuffer(out aFormat: TMediaStreamDataHeader;
      out aData: pointer; out aDataSize: cardinal;
      out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess; override;


    constructor Create; override;
    destructor Destroy; override;
  end;

  TStreamToMemoryMediator = class (TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; Size: Longint);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
  end;

implementation

{ TStreamToMemoryMediator }

constructor TStreamToMemoryMediator.Create(Ptr: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Ptr,Size);
end;

destructor TStreamToMemoryMediator.Destroy;
begin
  SetPointer(nil,0);
  inherited;
end;

function TStreamToMemoryMediator.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('Not supported');
end;

{ TVideoOutputDecoder_MJPEG }

constructor TVideoOutputDecoder_MJPEG.Create;
begin
  inherited Create;
  FVideoDecoder:=TVideoDecoder.Create;
end;

destructor TVideoOutputDecoder_MJPEG.Destroy;
begin
  inherited;
  FreeAndNil(FVideoDecoder);
end;

function TVideoOutputDecoder_MJPEG.GetCurrentDecodedBuffer(
  out aFormat: TMediaStreamDataHeader; out aData: pointer;
  out aDataSize: cardinal; out aInfo: pointer;
  out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess;
var
  aDIB1,aDIB2: PAnsiChar;
  aLine: integer;
begin
  aFormat.Clear;

  if (FVideoDecoder.Empty) or (FVideoDecoder.Bitmap=nil) then
    exit(dbaNoBuffer);

  aFormat.biMediaType:=mtVideo;
  aFormat.biStreamType:=stRGB;
  aFormat.VideoWidth:=ImageWidth;
  aFormat.VideoHeight:=ImageHeight;
  aFormat.VideoReversedVertical:=FReverseVertical;

  case FVideoDecoder.Bitmap.PixelFormat of
    pf1bit: aFormat.VideoBitCount:=1;
    pf4bit: aFormat.VideoBitCount:=4;
    pf8bit: aFormat.VideoBitCount:=8;
    pf15bit: aFormat.VideoBitCount:=15;
    pf16bit: aFormat.VideoBitCount:=16;
    pf24bit: aFormat.VideoBitCount:=24;
    pf32bit: aFormat.VideoBitCount:=32;
    else
      raise Exception.Create('Wrong PixelFormat');
  end;

  Assert(not FVideoDecoder.Empty);
  Assert(FVideoDecoder.Bitmap<>nil);

  aDIB1:=FVideoDecoder.Bitmap.ScanLine[0];
  aDIB2:=FVideoDecoder.Bitmap.ScanLine[FVideoDecoder.Bitmap.Height-1];

  //Изображение может быть перевернуто
  if cardinal(aDIB1)<cardinal(aDIB2) then
    aData:=aDIB1
  else
    aData:=aDIB2;

  aLine:=Abs(int64(aDIB2)-int64(aDIB1)) div (FVideoDecoder.Bitmap.Height-1);
  aDataSize:=aLine*FVideoDecoder.Bitmap.Height;

  aInfo:=nil;
  aInfoSize:=0;
  result:=dbaOK;
end;

class function TVideoOutputDecoder_MJPEG.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stMJPEG;
end;

function TVideoOutputDecoder_MJPEG.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
var
  aStream : TStreamToMemoryMediator;
begin
  aStream:=TStreamToMemoryMediator.Create(aData,aDataSize);
  try
    aStream.Position:=0;
    FVideoDecoder.LoadFromStream(aStream);
    FReverseVertical:=not aFormat.VideoReversedVertical;

    SetImageSize(FVideoDecoder.Width,FVideoDecoder.Height);
  finally
    aStream.Free;
  end;

  result:=drSuccess;
end;

procedure TVideoOutputDecoder_MJPEG.CopyDecoderImageToSurface(aSurface: TSurface);
var
  aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal;
begin
  if GetCurrentDecodedBuffer(aFormat,aData,aDataSize,aInfo,aInfoSize)<>dbaOK then
    exit;

  case FVideoDecoder.Bitmap.PixelFormat of
    pf16bit: aSurface.DrawRGB16(aData,aDataSize,FReverseVertical);
    pf24bit: aSurface.DrawRGB24(aData,aDataSize,FReverseVertical);
    pf32bit: aSurface.DrawRGB32(aData,aDataSize,FReverseVertical);
  end;
end;

initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_MJPEG);


end.
