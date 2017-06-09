{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь медиа-потока в формате BMP. Склеивает кадры   }
{                от разных каналов в один, панорамный                          }
{                Декларация                                                    }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        21.01.2011                                                    }
{                                                                              }
{  <Примечание>  Отсутствует                                                   }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaProcessing.Panorama.RGB;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,Graphics;

type
  TMediaProcessor_Panorama_Rgb=class (TMediaProcessor,IMediaProcessor_Panorama_Rgb)
  protected
    FFPSValue: integer;

    //Автоматическое совмещение кадров
    FAutoImageStitching: boolean;
    FAutoImageStitchingIntervalSecs: integer;
    // насколько процентов по горизонтали от свой ширины изображения могут пересекаться.
    FAutoImageStitchingMaxWinWidthPercent,
    //насколько процентов по вертикали изображение может сдвигаться
    FAutoImageStitchingMaxYDispPercent: byte;


    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    class function MetaInfo:TMediaProcessorInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  HasCustomProperties: boolean; override;
    procedure ShowCustomProperiesDialog;override;
  end;

implementation
  uses Controls,uBaseClasses,MediaProcessing.Panorama.RGB.SettingsDialog;


{ TMediaProcessor_Panorama_Rgb }

constructor TMediaProcessor_Panorama_Rgb.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Panorama_Rgb.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Panorama_Rgb.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Panorama_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Panorama_Rgb;
  result.Name:='Панорама';
  result.Description:='Выполняет объединение нескольких видео-потоков в единое панорамное видео ';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Panorama_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FFPSValue:=aReader.ReadInteger('FPSValue',25);
  FAutoImageStitching:=aReader.ReadBool('ImageStitching',true);
  FAutoImageStitchingIntervalSecs:=aReader.ReadInteger('ImageStitchingInterval',60);
  FAutoImageStitchingMaxWinWidthPercent:=aReader.ReadInteger('AutoImageStitchingMaxWinWidthPercent',40);
  FAutoImageStitchingMaxYDispPercent:=aReader.ReadInteger('AutoImageStitchingMaxYDispPercent',25);
end;

procedure TMediaProcessor_Panorama_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('FPSValue',FFPSValue);
  aWriter.WriteBool('ImageStitching',FAutoImageStitching);
  aWriter.WriteInteger('ImageStitchingInterval',FAutoImageStitchingIntervalSecs);
  aWriter.WriteInteger('AutoImageStitchingMaxWinWidthPercent',FAutoImageStitchingMaxWinWidthPercent);
  aWriter.WriteInteger('AutoImageStitchingMaxYDispPercent',FAutoImageStitchingMaxYDispPercent);
end;

procedure TMediaProcessor_Panorama_Rgb.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsPanorama_Rgb;
begin
  aDialog:=TfmMediaProcessingSettingsPanorama_Rgb.Create(nil);
  try
    aDialog.edFPSValue.Value:=FFPSValue;
    aDialog.ckImageStitchingEnabled.Checked:=FAutoImageStitching;
    aDialog.edImageStitchingInterval.Value:=FAutoImageStitchingIntervalSecs;
    //aDialog.edImageStitchingSnapInterval.Value:=FAutoImageStitchingSnapIntervalSecs;
    if aDialog.ShowModal=mrOK then
    begin
      FFPSValue:=aDialog.edFPSValue.Value;
      FAutoImageStitching:=aDialog.ckImageStitchingEnabled.Checked;
      FAutoImageStitchingIntervalSecs:=aDialog.edImageStitchingInterval.Value;
      //FAutoImageStitchingSnapIntervalSecs:=aDialog.edImageStitchingSnapInterval.Value;
    end;
  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Panorama_Rgb);

end.
