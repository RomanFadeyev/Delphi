unit MediaProcessing.Transformer.RGB;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Transformer.RGB.SettingsDialog,MediaProcessing.Common.Processor.FPS.ImageSize;

type
  TMediaProcessor_Transformer_Rgb=class (TMediaProcessor_FpsImageSize<TfmMediaProcessingSettingsTransformer_Rgb>,IMediaProcessor_Transformer_Rgb)
  protected
    FVerticalReversePhysical : boolean;
    FVerticalReverseLogical : boolean;
    FRotate: boolean;
    FRotateAngle: integer;

    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    procedure OnLoadPropertiesToDialog(aDialog: TfmMediaProcessingSettingsTransformer_Rgb); override;
    procedure OnSavePropertiesFromDialog(aDialog: TfmMediaProcessingSettingsTransformer_Rgb); override;

    class function MetaInfo:TMediaProcessorInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Controls,uBaseClasses;


{ TMediaProcessor_Transformer_Rgb }

constructor TMediaProcessor_Transformer_Rgb.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Transformer_Rgb.Destroy;
begin
  inherited;
end;

class function TMediaProcessor_Transformer_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Transformer_Rgb;
  result.Name:='Преобразование';
  result.Description:='Выполняет различные преобразования изображения';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=0;
end;

procedure TMediaProcessor_Transformer_Rgb.OnLoadPropertiesToDialog(
  aDialog: TfmMediaProcessingSettingsTransformer_Rgb);
begin
  inherited;
  aDialog.buVerticalReverse.Checked:=FVerticalReversePhysical;
  aDialog.ckRotate.Checked:=FRotate;
  if FRotateAngle<0 then
  begin
    aDialog.edCounterClockwiseRotate.Value:=-FRotateAngle;
    aDialog.edClockwiseRotate.Value:=-FRotateAngle;
    aDialog.buCounterClockwiseRotate.Checked:=true;
  end
  else begin
    aDialog.edCounterClockwiseRotate.Value:=FRotateAngle;
    aDialog.edClockwiseRotate.Value:=FRotateAngle;
    aDialog.buClockwiseRotate.Checked:=true;
  end;
end;

procedure TMediaProcessor_Transformer_Rgb.OnSavePropertiesFromDialog(
  aDialog: TfmMediaProcessingSettingsTransformer_Rgb);
begin
  inherited;
  FVerticalReversePhysical:=aDialog.buVerticalReverse.Checked;
  FRotate:=aDialog.ckRotate.Checked;
  if aDialog.buCounterClockwiseRotate.Checked then
    FRotateAngle:=-aDialog.edCounterClockwiseRotate.Value
  else
    FRotateAngle:=aDialog.edClockwiseRotate.Value;
end;

procedure TMediaProcessor_Transformer_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FVerticalReversePhysical:=aReader.ReadBool('VerticalReverse',false);
  FRotate:=aReader.ReadBool('Rotate', false);
  FRotateAngle:= aReader.ReadInteger('RotateAngle', 1);
end;

procedure TMediaProcessor_Transformer_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteBool('VerticalReverse', FVerticalReversePhysical);
  aWriter.WriteBool('Rotate', FRotate);
  aWriter.WriteInteger('RotateAngle', FRotateAngle);
end;


initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Transformer_Rgb);

end.
