unit MediaProcessing.Convertor.RGB.VFW;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,VFW;

type
  TCodecMode = (cmRealTime,cmStandard);

  TMediaProcessor_Convertor_Rgb_Vfw=class (TMediaProcessor,IMediaProcessor_Convertor_Rgb_Vfw)
  protected
    FCodecFCC: cardinal;
    FCodecState: TBytes;
    FCodecMode : TCodecMode;
  protected
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
  uses Controls,uBaseClasses,MediaProcessing.Convertor.RGB.Vfw.SettingsDialog;

{ TMediaProcessor_Convertor_Rgb_Vfw }

constructor TMediaProcessor_Convertor_Rgb_Vfw.Create;
begin
  inherited;
  FCodecMode:=cmRealTime;
end;

destructor TMediaProcessor_Convertor_Rgb_Vfw.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Convertor_Rgb_Vfw.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Convertor_Rgb_Vfw.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_Rgb_Vfw;
  result.Name:='Конвертор RGB->VFW';
  result.Description:='Преобразует видеокадры формата RGB в любой формат, имеющийся в операционной системе (технология Video For Windows)';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stUNIV;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FCodecFCC:=aReader.ReadInteger('Codec FCC',FCodecFCC);
  FCodecMode:=TCodecMode(aReader.ReadInteger('Codec Mode',integer(FCodecMode)));
  FCodecState:=aReader.ReadBytes('Codec State');
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('Codec FCC',FCodecFCC);
  aWriter.WriteInteger('Codec Mode',integer(FCodecMode));
  aWriter.WriteBytes('Codec State',FCodecState);
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsRgb_Vfw;
begin
  aDialog:=TfmMediaProcessingSettingsRgb_Vfw.Create(nil);
  try
    aDialog.SetCodec(FCodecFCC,FCodecState);
    aDialog.edCodec.Text:=LowerCase(string(FOURCCTOSTR(FCodecFCC)));

    aDialog.buPerformanceSpeed.Checked:=FCodecMode=cmRealTime;
    aDialog.buPerformanceQuality.Checked:=FCodecMode=cmStandard;

    if aDialog.ShowModal=mrOK then
    begin
      aDialog.GetCodec(FCodecFCC,FCodecState);
      if aDialog.buPerformanceSpeed.Checked then
        FCodecMode:=cmRealTime
      else
        FCodecMode:=cmStandard;
    end;

  finally
    aDialog.Free;
  end;

end;


initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_Rgb_Vfw);

end.
