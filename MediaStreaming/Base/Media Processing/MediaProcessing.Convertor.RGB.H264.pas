unit MediaProcessing.Convertor.RGB.H264;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,AVC,
  MediaProcessing.Common.Processor.CustomSettings,MediaProcessing.Convertor.RGB.H264.SettingsDialog;

type
  TMediaProcessor_Convertor_Rgb_H264=class (TMediaProcessor_CustomSettings<TfmMediaProcessingSettingsRgb_H264>,IMediaProcessor_Convertor_Rgb_H264)
  protected
    FKeyFrameInterval: integer; // интервал между ключевыми кадрами (NAL_SEI вроде, но надо уточнить)
    FPreset: TEncoderPreset;
    FCRF: integer;
    FMaxBitrateKbit: integer; //Кбит

    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    class function MetaInfo:TMediaProcessorInfo; override;

    procedure OnLoadPropertiesToDialog(aDialog: TfmMediaProcessingSettingsRgb_H264); override;
    procedure OnSavePropertiesFromDialog(aDialog: TfmMediaProcessingSettingsRgb_H264);override;
  public
    constructor Create; override;
    destructor Destroy; override;


    property H264Preset: TEncoderPreset read FPreset write FPreset;
  end;

implementation
  uses Controls,uBaseClasses;

{ TMediaProcessor_Convertor_Rgb_H264 }

constructor TMediaProcessor_Convertor_Rgb_H264.Create;
begin
  inherited;
  FKeyFrameInterval:=25;
  FPreset:=epmedium;

  FCRF:=23;
  FMaxBitrateKbit:=500;
end;

destructor TMediaProcessor_Convertor_Rgb_H264.Destroy;
begin
  inherited;
end;

class function TMediaProcessor_Convertor_Rgb_H264.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_Rgb_H264;
  result.Name:='Конвертор BMP->H.264';
  result.Description:='Преобразует видеокадры формата BMP в формат H.264';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stH264;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_Rgb_H264.OnLoadPropertiesToDialog(
  aDialog: TfmMediaProcessingSettingsRgb_H264);
begin
  inherited;
  aDialog.edKeyFrameInterval.Value := FKeyFrameInterval;
  aDialog.Preset:=FPreset;

  aDialog.edCRF.Value:=FCRF;
  aDialog.edMaxRate.Value:=FMaxBitrateKbit;
end;

procedure TMediaProcessor_Convertor_Rgb_H264.OnSavePropertiesFromDialog(
  aDialog: TfmMediaProcessingSettingsRgb_H264);
begin
  inherited;
  FKeyFrameInterval := aDialog.edKeyFrameInterval.Value;
  FPreset:=aDialog.Preset;

  FCRF:=aDialog.edCRF.Value;
  FMaxBitrateKbit:=aDialog.edMaxRate.Value;
end;

procedure TMediaProcessor_Convertor_Rgb_H264.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FKeyFrameInterval:=aReader.ReadInteger('KeyFrameInterval',FKeyFrameInterval);

  FPreset:=TEncoderPreset(aReader.ReadInteger('Preset',integer(FPreset)));
  FCRF:=aReader.ReadInteger('CRF',FCRF);
  FMaxBitrateKbit:=aReader.ReadInteger('MaxBitrate',FMaxBitrateKbit);
end;

procedure TMediaProcessor_Convertor_Rgb_H264.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('KeyFrameInterval',FKeyFrameInterval);
  aWriter.WriteInteger('Preset',integer(FPreset));

  aWriter.WriteInteger('CRF',FCRF);
  aWriter.WriteInteger('MaxBitrate',FMaxBitrateKbit);
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_Rgb_H264);

end.
