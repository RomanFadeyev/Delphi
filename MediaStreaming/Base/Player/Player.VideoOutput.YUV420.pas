{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Декодер для видео-вывода, формат YUV 4:2:0                    }
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

unit Player.VideoOutput.YUV420;

interface
  uses Windows, SysUtils, Classes, Player.VideoOutput.Base,MediaProcessing.Definitions;


type
  TVideoOutputDecoder_YUV420 = class (TVideoOutputDecoder)
  private
    FDibBuffer: pointer;
    FYUV420InfoHeader: BITMAPINFOHEADER;
    FReverseVertical:boolean;

  protected
    function DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; override;
  public
    class function SupportedStreamTypes: TArray<TStreamType>; override;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); override;
  end;

implementation

{ TVideoOutputDecoder_YUV420 }


function TVideoOutputDecoder_YUV420.DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtVideo);

  FDibBuffer:=aData;
  FYUV420InfoHeader:=aFormat.ToBitmapInfoHeader(aDataSize);
  Assert(aDataSize=FYUV420InfoHeader.biSizeImage);

  SetImageSize(aFormat.VideoWidth,aFormat.VideoHeight);
  FReverseVertical:=aFormat.VideoReversedVertical;

  result:=drSuccess;
end;

class function TVideoOutputDecoder_YUV420.SupportedStreamTypes: TArray<TStreamType>;
begin
  SetLength(result,1);
  result[0]:=stYUV420;
end;

procedure TVideoOutputDecoder_YUV420.CopyDecoderImageToSurface(aSurface: TSurface);
begin
  aSurface.DrawYUV420(FDibBuffer,FYUV420InfoHeader.biSizeImage,FReverseVertical);
end;


initialization
  PlayerVideoOutputDecoderFactory.RegisterDecoderClass(TVideoOutputDecoder_YUV420);

end.
