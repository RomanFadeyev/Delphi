unit MediaProcessing.Convertor.HH.RGB;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global;

type
  TChangeFPSMode = (cfmNone,cfmAbsolute, cfmVIFrameOnly);
  TChangeImageSizeMode = (cismNone,cismScale,cismCustomSize);

  TMediaProcessor_Convertor_Hh_Rgb=class (TMediaProcessor,IMediaProcessor_Convertor_Hh_Rgb)
  protected
    FChangeFPSMode : TChangeFPSMode;
    FFPSValue: integer;
    FVIFramesOnly: boolean;


    FImageSizeMode   : TChangeImageSizeMode;
    FImageSizeScale  : integer;
    FImageSizeWidth  : integer;
    FImageSizeHeight : integer;
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
  uses Controls,uBaseClasses,MediaProcessing.Convertor.HH.RGB.SettingsDialog;


{ TMediaProcessor_Convertor_Hh_Rgb }

constructor TMediaProcessor_Convertor_Hh_Rgb.Create;
begin
  inherited;
  FImageSizeScale:=2;
  FImageSizeMode:=cismNone;
  FImageSizeWidth:=640;
  FImageSizeHeight:=480;
end;

destructor TMediaProcessor_Convertor_Hh_Rgb.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Convertor_Hh_Rgb.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Convertor_Hh_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_Hh_Rgb;
  result.Name:='Конвертор HH->RGB';
  result.Description:='Преобразует видеокадры формата HH (камеры Beward) в формат RGB';
  result.SetInputStreamType(stHHVI);
  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_Hh_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FImageSizeMode:=TChangeImageSizeMode(aReader.ReadInteger('ImageSize.ChangeMode',integer(FImageSizeMode)));
  FImageSizeScale:=aReader.ReadInteger('ImageSize.Scale',FImageSizeScale);
  FImageSizeWidth:=aReader.ReadInteger('ImageSize.Width',FImageSizeWidth);
  FImageSizeHeight:=aReader.ReadInteger('ImageSize.Height',FImageSizeHeight);

  FChangeFPSMode:=TChangeFPSMode(aReader.ReadInteger('ChangeFPSMode',0));
  FFPSValue:=aReader.ReadInteger('FPSValue',25);
  FVIFramesOnly:=aReader.ReadBool('VIFramesOnly',false)
end;

procedure TMediaProcessor_Convertor_Hh_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;

  aWriter.WriteInteger('ImageSize.ChangeMode',integer(FImageSizeMode));
  aWriter.WriteInteger('ImageSize.Scale',FImageSizeScale);
  aWriter.WriteInteger('ImageSize.Width',FImageSizeWidth);
  aWriter.WriteInteger('ImageSize.Height',FImageSizeHeight);

  aWriter.WriteInteger('ChangeFPSMode',integer(FChangeFPSMode));
  aWriter.WriteInteger('FPSValue',FFPSValue);
  aWriter.WriteBool('VIFramesOnly',FVIFramesOnly);
end;

procedure TMediaProcessor_Convertor_Hh_Rgb.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsHh_Rgb;
begin
  aDialog:=TfmMediaProcessingSettingsHh_Rgb.Create(nil);
  try
    aDialog.ckChangeImageSize.Checked:=FImageSizeMode<>cismNone;
    aDialog.cbImageSizeScale.ItemIndex:=aDialog.cbImageSizeScale.Items.IndexOfData(FImageSizeScale);
    if aDialog.cbImageSizeScale.ItemIndex=-1 then
      aDialog.cbImageSizeScale.ItemIndex:=0;

    aDialog.buImageSizeScale.Checked:=FImageSizeMode=cismScale;
    aDialog.buImageSizeCustomSize.Checked:=FImageSizeMode=cismCustomSize;
    if FImageSizeMode=cismNone then
      aDialog.buImageSizeScale.Checked:=true;


    aDialog.edImageSizeWidth.Value:=FImageSizeWidth;
    aDialog.edimageSizeHeight.Value:=FImageSizeHeight;

    aDialog.ckChangeFPS.Checked:=FChangeFPSMode<>cfmNone;
    aDialog.ckChangeFPSAbsolute.Checked:=FChangeFPSMode in [cfmAbsolute,cfmNone];
    aDialog.ckFPSVIFramesOnly.Checked:=FChangeFPSMode=cfmVIFrameOnly;
    aDialog.edFPSValue.Value:=FFPSValue;

    if aDialog.ShowModal=mrOK then
    begin
      if not aDialog.ckChangeImageSize.Checked then
        FImageSizeMode:=cismNone
      else if aDialog.buImageSizeScale.Checked then
        FImageSizeMode:=cismScale
      else if aDialog.buImageSizeCustomSize.Checked then
        FImageSizeMode:=cismCustomSize
      else
        Assert(false);

      FImageSizeScale:=aDialog.cbImageSizeScale.CurrentItemData;
      FImageSizeWidth:=aDialog.edImageSizeWidth.Value;
      FImageSizeHeight:=aDialog.edimageSizeHeight.Value;

      FChangeFPSMode:=cfmNone;
      if aDialog.ckChangeFPS.Checked then
      begin
        if aDialog.ckChangeFPSAbsolute.Checked then
        begin
          FChangeFPSMode:=cfmAbsolute;
          FFPSValue:=aDialog.edFPSValue.Value
        end
        else begin
          FChangeFPSMode:=cfmVIFrameOnly;
        end;
      end;
    end;


  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_Hh_Rgb);

end.

