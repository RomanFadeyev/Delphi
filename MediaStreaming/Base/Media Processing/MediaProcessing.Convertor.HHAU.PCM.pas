unit MediaProcessing.Convertor.HHAU.PCM;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global;

type
  TMediaProcessor_Convertor_HhAu_Pcm=class (TMediaProcessor,IMediaProcessor_Convertor_HhAu_Pcm)
  protected
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
  uses Controls,uBaseClasses,MediaProcessing.Convertor.HHAU.PCM.SettingsDialog;


{ TMediaProcessor_Convertor_HhAu_Pcm }

constructor TMediaProcessor_Convertor_HhAu_Pcm.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Convertor_HhAu_Pcm.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Convertor_HhAu_Pcm.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Convertor_HhAu_Pcm.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Convertor_HhAu_Pcm;
  result.Name:='Конвертор HH->PCM';
  result.Description:='Преобразует аудиокадры формата HH (камеры Beward) в формат PCM';
  result.SetInputStreamType(stHHAU);
  result.OutputStreamType:=stPCM;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_Convertor_HhAu_Pcm.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
end;

procedure TMediaProcessor_Convertor_HhAu_Pcm.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
end;

procedure TMediaProcessor_Convertor_HhAu_Pcm.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsHhAu_Pcm;
begin
  aDialog:=TfmMediaProcessingSettingsHhAu_Pcm.Create(nil);
  try
    if aDialog.ShowModal=mrOK then
    begin
    end;

  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Convertor_HhAu_Pcm);

end.
