unit MediaProcessing.Common.Processor.FPS;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Common.SettingsDialog.FPS,MediaProcessing.Common.Processor.CustomSettings;

type
  TChangeFPSMode = (cfmNone,cfmAbsolute, cfmVIFrameOnly);

  TMediaProcessor_Fps<T: TfmMediaProcessingSettingsFps,constructor>=class (TMediaProcessor_CustomSettings<T>)
  protected
    FChangeFPSMode : TChangeFPSMode;
    FFPSValue: integer;
    FVIFramesOnly: boolean;

    //Process
    FPrevFrameTimeStampMs: int64;
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    procedure OnLoadPropertiesToDialog(aDialog: T); override;
    procedure OnSavePropertiesFromDialog(aDialog: T); override;

    function Process_Fps(const aInFormat: TMediaStreamDataHeader): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Controls,uBaseClasses;


{ TMediaProcessor_Fps }

constructor TMediaProcessor_Fps<T>.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Fps<T>.Destroy;
begin
  inherited;
end;

procedure TMediaProcessor_Fps<T>.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FChangeFPSMode:=TChangeFPSMode(aReader.ReadInteger('FPS.ChangeMode',0));
  FFPSValue:=aReader.ReadInteger('FPS.Value',25);
  FVIFramesOnly:=aReader.ReadBool('FPS.VIFramesOnly',false)
end;

procedure TMediaProcessor_Fps<T>.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;

  aWriter.WriteInteger('FPS.ChangeMode',integer(FChangeFPSMode));
  aWriter.WriteInteger('FPS.Value',FFPSValue);
  aWriter.WriteBool   ('FPS.VIFramesOnly',FVIFramesOnly);
end;

procedure TMediaProcessor_Fps<T>.OnLoadPropertiesToDialog(aDialog: T);
begin
  aDialog.ckChangeFPS.Checked:=FChangeFPSMode<>cfmNone;
  aDialog.ckChangeFPSAbsolute.Checked:=FChangeFPSMode in [cfmAbsolute,cfmNone];
  aDialog.ckFPSVIFramesOnly.Checked:=FChangeFPSMode=cfmVIFrameOnly;
  aDialog.edFPSValue.Value:=FFPSValue;
end;

procedure TMediaProcessor_Fps<T>.OnSavePropertiesFromDialog(
  aDialog: T);
begin
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


function TMediaProcessor_Fps<T>.Process_Fps(const aInFormat: TMediaStreamDataHeader): boolean;
var
  aMinDelta: double;
begin
  result:=false;

  //Прореживание кадров
  if FChangeFPSMode=cfmVIFrameOnly then
  begin
    if not (ffKeyFrame in aInFormat.biFrameFlags) then
      exit;
  end
  else if FChangeFPSMode=cfmAbsolute then
  begin
    if (FPrevFrameTimeStampMs<>0) then
    begin
      aMinDelta:=1000/FFPSValue;
      if (aInFormat.TimeStampMs-FPrevFrameTimeStampMs)<aMinDelta then
        exit;
    end;
  end;

  FPrevFrameTimeStampMs:=aInFormat.TimeStampMs;
  result:=true;
end;

end.
