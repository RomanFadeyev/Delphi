{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Класс,обеспечивающий невизуальное декодирование в RGB входного}
{                потока. Декодер подбирает сам                                 }
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

unit Player.VideoDecoding;

interface
 uses Windows, Messages, Classes, SysUtils, Math,BitPlane,
      Player.VideoOutput.Base,
      MediaProcessing.Definitions;

type
  TVideoDecoding = class
  var
    FDecoder      : TVideoOutputDecoder;
    FDecodedFormat: TMediaStreamDataHeader;
    FStreamType   : TStreamType;
    FSurface      : TSurfaceRGB;

    procedure SetStreamType(const Value: TStreamType);
  public
    constructor Create;
    destructor Destroy; override;

    function  DecodeData(const aFormat: TMediaStreamDataHeader;
                         aData: pointer; aDataSize: cardinal;
                         aInfo: pointer; aInfoSize: cardinal):boolean;

    function DecodedData: TSurfaceRGB;
    property DecodedFormat: TMediaStreamDataHeader read FDecodedFormat;
  end;

implementation
  uses DirectImage, Player.VideoOutput.AllTypes;

type
  TVideoOutput = class (TPlayerVideoOutput_AllTypes);

{ TVideoDecoding }

constructor TVideoDecoding.Create;
begin
  FSurface:=TSurfaceRGB.Create(RGB24);
end;

function TVideoDecoding.DecodeData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal): boolean;
begin
  if FStreamType<>aFormat.biStreamType then
  begin
    SetStreamType(aFormat.biStreamType);
  end;

  result:=FDecoder.DecodeData(aFormat,aData,aDataSize,aInfo,aInfoSize)=drSuccess;
  if not result then
  begin
  end
  else begin
    FSurface.SetSize(aFormat.VideoWidth,aFormat.VideoHeight);
    FDecoder.CopyDecoderImageToSurface(FSurface);
    FDecodedFormat:=aFormat;
    FDecodedFormat.biStreamType:=stRGB;
    FDecodedFormat.VideoBitCount:=FSurface.SurfaceBits;
  end;
end;

function TVideoDecoding.DecodedData: TSurfaceRGB;
begin
  result:=FSurface;
end;

destructor TVideoDecoding.Destroy;
begin
  FreeAndNil(FSurface);
  FreeAndNil(FDecoder);
  inherited;
end;

procedure TVideoDecoding.SetStreamType(const Value: TStreamType);
var
  aClass: TVideoOutputDecoderClass;
  aOutput: TVideoOutput;
begin
  if FStreamType<>Value then
    FreeAndNil(FDecoder);

  if FDecoder<>nil then
    exit;

  FStreamType:=INVALID_HANDLE_VALUE;

  aOutput:=TVideoOutput.Create(0);
  try
    aClass:=aOutput.GetStreamTypeHandlerClass(Value);
  finally
    aOutput.Free;
  end;

  if aClass=nil then
      raise Exception.Create(Format('Неподдерживаемый тип потока %s',[GetStreamTypeName(Value)]));

  FDecoder:=aClass.Create;
  FDecoder.UseHardwareAcceleration:=false; //TODO
  FStreamType:=Value;
end;


end.
