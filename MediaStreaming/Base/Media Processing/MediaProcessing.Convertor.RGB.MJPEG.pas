unit MediaProcessing.Convertor.RGB.MJPEG;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global;

type
  TPerformance = (pBestQuality, pBestSpeed);

  TMediaProcessor_Convertor_Rgb_MJpeg=class (TMediaProcessor,IMediaProcessor_Convertor_Rgb_MJpeg)
  protected
    FCompressionQuality: integer;
    FPerformance: TPerformance;

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
  uses Controls,uBaseClasses,MediaProcessing.Convertor.RGB.MJPEG.SettingsDialog;

{ TMediaProcessor_Convertor_Rgb_MJpeg }

constructor TMediaProcessor_Convertor_Rgb_MJpeg.Create;
begin
  inherited;
  FCompressionQuality:=50;
  FPerformance:=pBestSpeed;
end;

destructor TMediaProcessor_Convertor_Rgb_MJpeg.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Convertor_Rgb_MJpeg.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Convertor_Rgb_MJpeg.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_Rgb_MJpeg;
  result.Name:='Конвертор RGB->MJPG';
  result.Description:='Преобразует видеокадры формата RGB в формат Motion Jpeg';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stMJpeg;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_Rgb_MJpeg.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FCompressionQuality:=aReader.ReadInteger('CompressionQuality',FCompressionQuality);
  FPerformance:=TPerformance(aReader.ReadInteger('Performance',integer(FPerformance)));
end;

procedure TMediaProcessor_Convertor_Rgb_MJpeg.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('CompressionQuality',FCompressionQuality);
  aWriter.WriteInteger('Performance',integer(FPerformance));
end;

procedure TMediaProcessor_Convertor_Rgb_MJpeg.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsRgb_MJpeg;
begin
  aDialog:=TfmMediaProcessingSettingsRgb_MJpeg.Create(nil);
  try
    aDialog.cbCompression.ItemIndex:=aDialog.cbCompression.Items.IndexOfData(FCompressionQuality);
    if aDialog.cbCompression.ItemIndex=-1 then
      aDialog.cbCompression.ItemIndex:=0;

    aDialog.buPerformanceSpeed.Checked:=FPerformance=pBestSpeed;
    aDialog.buPerformanceQuality.Checked:=FPerformance=pBestQuality;

    if aDialog.ShowModal=mrOK then
    begin
      FCompressionQuality:=aDialog.cbCompression.CurrentItemData;
      if aDialog.buPerformanceSpeed.Checked then
        FPerformance:=pBestSpeed
      else
        FPerformance:=pBestQuality;
    end;

  finally
    aDialog.Free;
  end;

end;


initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_Rgb_MJpeg);

end.
