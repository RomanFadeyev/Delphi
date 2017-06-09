{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат RGB                          }
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

unit Player.VideoOutput.BMP;

interface
  uses Windows, SysUtils, Classes, Player.VideoOutput.Base,MediaProcessing.Definitions;


type
  TVideoOutputDecoder_BMP = class (TVideoOutputDecoder)
  private
    FDibBuffer: pointer;
    FBmpInfoHeader: BITMAPINFOHEADER;
    FReverseVertical:boolean;
  protected
    function DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;

    function GetCurrentDecodedBuffer(out aFormat: TMediaStreamDataHeader;
      out aData: pointer; out aDataSize: cardinal;
      out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess; override;

    procedure SaveDecoderImageToStream(aStream: TStream);
    procedure SaveDecoderImageToFile(const aFileName:string);
  end;

implementation
  uses BitPlane;

{ TVideoOutputDecoder_BMP }

function TVideoOutputDecoder_BMP.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtVideo);

  FDibBuffer:=aData;
  FBmpInfoHeader:=aFormat.ToBitmapInfoHeader(aDataSize);
  Assert(FBmpInfoHeader.biSizeImage=aDataSize);
  FReverseVertical:=not aFormat.VideoReversedVertical;

  SetImageSize(aFormat.VideoWidth,aFormat.VideoHeight);
  result:=drSuccess;
end;

function TVideoOutputDecoder_BMP.GetCurrentDecodedBuffer(
  out aFormat: TMediaStreamDataHeader; out aData: pointer;
  out aDataSize: cardinal; out aInfo: pointer;
  out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess;
begin
  aFormat.Assign(FBmpInfoHeader);
  aData:=FDibBuffer;
  aDataSize:=FBmpInfoHeader.biSizeImage;
  aInfo:=nil;
  aInfoSize:=0;
  result:=dbaOK;
end;

procedure TVideoOutputDecoder_BMP.SaveDecoderImageToFile(const aFileName: string);
var
  aStream: TFileStream;
begin
  aStream:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveDecoderImageToStream(aStream);
  finally
    aStream.Free;
  end;
end;

procedure TVideoOutputDecoder_BMP.SaveDecoderImageToStream(aStream: TStream);
var
  FBmpFileHeader: BITMAPFILEHEADER;
begin
  FBmpFileHeader.bfType := $4d42; //"BM"
  FBmpFileHeader.bfReserved1 := 0;
  FBmpFileHeader.bfReserved2 := 0;
  FBmpFileHeader.bfOffBits   := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
  FBmpFileHeader.bfSize :=   FBmpInfoHeader.biSizeImage + sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

  aStream.WriteBuffer(FBmpFileHeader, SizeOf(FBmpFileHeader));
  aStream.WriteBuffer(FBmpInfoHeader, SizeOf(FBmpInfoHeader));
  aStream.WriteBuffer(FDibBuffer^, FBmpInfoHeader.biSizeImage)
end;

class function TVideoOutputDecoder_BMP.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stRGB;
end;

procedure TVideoOutputDecoder_BMP.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  case FBmpInfoHeader.biBitCount of
    15: aSurface.DrawRGB15(FDibBuffer,FBmpInfoHeader.biSizeImage,FReverseVertical);
    16: aSurface.DrawRGB15(FDibBuffer,FBmpInfoHeader.biSizeImage,FReverseVertical);
    24: aSurface.DrawRGB24(FDibBuffer,FBmpInfoHeader.biSizeImage,FReverseVertical);
    32: aSurface.DrawRGB32(FDibBuffer,FBmpInfoHeader.biSizeImage,FReverseVertical);
  end;
end;

initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_BMP);


end.
