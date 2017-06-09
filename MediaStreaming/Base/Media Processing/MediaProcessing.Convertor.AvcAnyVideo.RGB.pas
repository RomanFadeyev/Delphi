unit MediaProcessing.Convertor.AvcAnyVideo.RGB;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Common.Processor.FPS, MediaProcessing.Common.Processor.FPS.ImageSize, Avc,
  MediaProcessing.Convertor.AvcAnyVideo.RGB.SettingsDialog;

type
  TMediaProcessor_Convertor_AvcAnyVideo_Rgb=class (TMediaProcessor_FpsImageSize<TfmMediaProcessingSettingsAvcAny_Rgb>,IMediaProcessor_Convertor_AvcAnyVideo_Rgb)
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    class function MetaInfo:TMediaProcessorInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Controls,uBaseClasses;


{ TMediaProcessor_Convertor_AvcAnyVideo_Rgb }

constructor TMediaProcessor_Convertor_AvcAnyVideo_Rgb.Create;
begin
  inherited;
  FImageSizeScale:=2;
  FImageSizeMode:=cismNone;
  FImageSizeWidth:=640;
  FImageSizeHeight:=480;
end;

destructor TMediaProcessor_Convertor_AvcAnyVideo_Rgb.Destroy;
begin
  inherited;
end;

class function TMediaProcessor_Convertor_AvcAnyVideo_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_AvcAnyVideo_Rgb;
  result.Name:='Конвертор Any Video->RGB';
  result.Description:='Преобразует видеокадры любого видеоформата  в формат RGB';
  result.InputStreamTypes:=Avc.GetKnownVideoFourccArray;

  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_AvcAnyVideo_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
end;

procedure TMediaProcessor_Convertor_AvcAnyVideo_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_AvcAnyVideo_Rgb);

end.
