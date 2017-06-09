unit MediaProcessing.Convertor.H264.RGB;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Common.Processor.FPS, MediaProcessing.Common.Processor.FPS.ImageSize,
  MediaProcessing.Convertor.H264.RGB.SettingsDialog;

type
  TMediaProcessor_Convertor_H264_Rgb=class (TMediaProcessor_FpsImageSize<TfmMediaProcessingSettingsH264_Rgb>,IMediaProcessor_Convertor_H264_Rgb)
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


{ TMediaProcessor_Convertor_H264_Rgb }

constructor TMediaProcessor_Convertor_H264_Rgb.Create;
begin
  inherited;
  FImageSizeScale:=2;
  FImageSizeMode:=cismNone;
  FImageSizeWidth:=640;
  FImageSizeHeight:=480;
end;

destructor TMediaProcessor_Convertor_H264_Rgb.Destroy;
begin
  inherited;
end;

class function TMediaProcessor_Convertor_H264_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_H264_Rgb;
  result.Name:='Конвертор H.264->RGB';
  result.Description:='Преобразует видеокадры формата H.264 в формат RGB';
  result.SetInputStreamTypes(
    [stH264,
    $34363268, //h264
    $34363258, //X264
    $34363278, //x264
    $31637661, //avc1
    $43564144, //DAVC
    $48535356, //VSSH
    $34363251 //Q264
    ]);

  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_H264_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
end;

procedure TMediaProcessor_Convertor_H264_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_H264_Rgb);

end.
