unit MediaProcessing.Convertor.HHH264.H264;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global;

type
  TChangeFPSMode = (cfmNone,cfmAbsolute, cfmVIFrameOnly);

  TMediaProcessor_Convertor_Hh_H264_H624=class (TMediaProcessor,IMediaProcessor_Convertor_HhH264_H264)
  protected
    FChangeFPSMode : TChangeFPSMode;
    FFPSValue: integer;
    FVIFramesOnly: boolean;
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
  uses Controls,uBaseClasses,MediaProcessing.Convertor.HhH264.H264.SettingsDialog;


{ TMediaProcessor_Convertor_Hh_H264_H624 }

constructor TMediaProcessor_Convertor_Hh_H264_H624.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Convertor_Hh_H264_H624.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Convertor_Hh_H264_H624.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Convertor_Hh_H264_H624.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_HhH264_H264;
  result.Name:='Конвертор HH/H.264->H.264';
  result.Description:='Преобразует видеокадры формата HH/H.264 (камеры Beward) в формат H.264';
  result.SetInputStreamType(stHHVI);
  result.InputStreamSubType:='H.264';
  result.OutputStreamType:=stH264;
  result.ConsumingLevel:=0;
end;

procedure TMediaProcessor_Convertor_Hh_H264_H624.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FChangeFPSMode:=TChangeFPSMode(aReader.ReadInteger('ChangeFPSMode',0));
  FFPSValue:=aReader.ReadInteger('FPSValue',25);
  FVIFramesOnly:=aReader.ReadBool('VIFramesOnly',false)
end;

procedure TMediaProcessor_Convertor_Hh_H264_H624.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;

  aWriter.WriteInteger('ChangeFPSMode',integer(FChangeFPSMode));
  aWriter.WriteInteger('FPSValue',FFPSValue);
  aWriter.WriteBool('VIFramesOnly',FVIFramesOnly);
end;

procedure TMediaProcessor_Convertor_Hh_H264_H624.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsHhH264_H264;
begin
  aDialog:=TfmMediaProcessingSettingsHhH264_H264.Create(nil);
  try
    aDialog.ckChangeFPS.Checked:=FChangeFPSMode<>cfmNone;
    aDialog.ckChangeFPSAbsolute.Checked:=FChangeFPSMode in [cfmAbsolute,cfmNone];
    aDialog.ckFPSVIFramesOnly.Checked:=FChangeFPSMode=cfmVIFrameOnly;
    aDialog.edFPSValue.Value:=FFPSValue;

    if aDialog.ShowModal=mrOK then
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


  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_Hh_H264_H624);

end.
