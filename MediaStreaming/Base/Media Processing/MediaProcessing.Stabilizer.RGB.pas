{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь медиа-потока в формате BMP. Стабилизирует     }
{                дрожание кадров                                               }
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
unit MediaProcessing.Stabilizer.RGB;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,Graphics;

type
  TMediaProcessor_Stabilizer_Rgb=class (TMediaProcessor,IMediaProcessor_Stabilizer_Rgb)
  protected
    FBuildImageOutOfBorders: boolean;
    FDetectMovements: boolean;
    FDetectMovementsPeriod: integer;

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
  uses Controls,uBaseClasses,MediaProcessing.Stabilizer.RGB.SettingsDialog;


{ TMediaProcessor_Stabilizer_Rgb }

constructor TMediaProcessor_Stabilizer_Rgb.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Stabilizer_Rgb.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Stabilizer_Rgb.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Stabilizer_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Stabilizer_Rgb;
  result.Name:='Стабилизатор';
  result.Description:='Выполняет стабилизацию кадров в случае дрожания изображения';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Stabilizer_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FBuildImageOutOfBorders:=aReader.ReadBool('BuildImageOutOfBorders',true);
  FDetectMovements:=aReader.ReadBool('DetectMovements',true);
  FDetectMovementsPeriod:=aReader.ReadInteger('DetectMovementsPeriod',10);
end;

procedure TMediaProcessor_Stabilizer_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteBool('BuildImageOutOfBorders',FBuildImageOutOfBorders);
  aWriter.WriteBool('DetectMovements',FDetectMovements);
  aWriter.WriteInteger('DetectMovementsPeriod',FDetectMovementsPeriod);
end;

procedure TMediaProcessor_Stabilizer_Rgb.ShowCustomProperiesDialog;
var
  aDialog: TfmStabilizer_Rgb_Settings;
begin
  aDialog:=TfmStabilizer_Rgb_Settings.Create(nil);
  try
    aDialog.ckBuildImageOutOfBorders.Checked:=FBuildImageOutOfBorders;
    aDialog.ckDetectMovements.Checked:=FDetectMovements;
    aDialog.edMovementPeriod.Value:=FDetectMovementsPeriod;
    if aDialog.ShowModal=mrOK then
    begin
      FBuildImageOutOfBorders:=aDialog.ckBuildImageOutOfBorders.Checked;
      FDetectMovements:=aDialog.ckDetectMovements.Checked;
      FDetectMovementsPeriod:=aDialog.edMovementPeriod.Value;
    end;
  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Stabilizer_Rgb);

end.
