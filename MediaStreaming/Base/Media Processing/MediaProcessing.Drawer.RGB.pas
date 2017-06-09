unit MediaProcessing.Drawer.RGB;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,Graphics;

type
  TMediaProcessor_Drawer_Rgb=class (TMediaProcessor,IMediaProcessor_Drawer_Rgb)
  private
    FTextFont : TFontData;
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
  uses Controls,UITypes, uBaseClasses,MediaProcessing.Drawer.RGB.SettingsDialog;

type
  PFontStylesBase = ^TFontStylesBase;


{ TMediaProcessor_Drawer_Rgb }

constructor TMediaProcessor_Drawer_Rgb.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Drawer_Rgb.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Drawer_Rgb.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Drawer_Rgb.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Drawer_Rgb;
  result.Name:='Рисование';
  result.Description:='Выполняет рисование различных примитивов повех видео';
  result.SetInputStreamType(stRGB);
  result.OutputStreamType:=stRGB;
  result.ConsumingLevel:=0;
end;

procedure TMediaProcessor_Drawer_Rgb.LoadCustomProperties(const aReader: IPropertiesReader);
var
  t: Integer;
begin
  inherited;
  ZeroMemory(@FTextFont,sizeof(FTextFont));
  FTextFont.Height:=aReader.ReadInteger('TextFont.Font.Size', 10);
  t:=aReader.ReadInteger('TextFont.Font.Style', 0);
  FTextFont.Style:=PFontStylesBase(@t)^;
  FTextFont.Name:=AnsiString(aReader.ReadString('TextFont.Font.Name', 'MS Sans Serif'));
  FTextFont.Charset:=DEFAULT_CHARSET;
end;

procedure TMediaProcessor_Drawer_Rgb.SaveCustomProperties(const aWriter: IPropertiesWriter);
var
  b: byte;
begin
  inherited;
  aWriter.WriteInteger('TextFont.Font.Size', FTextFont.Height);
  b:=PByte(@FTextFont.Style)^;
  aWriter.WriteInteger('TextFont.Font.Style', b);
  aWriter.WriteString('TextFont.Font.Name', string(FTextFont.Name));
end;

procedure TMediaProcessor_Drawer_Rgb.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsRgb;
begin
  aDialog:=TfmMediaProcessingSettingsRgb.Create(nil);
  try
    if aDialog.ShowModal=mrOK then
    begin
    end;
  finally
    aDialog.Free;
  end;

end;

//initialization
//  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Drawer_Rgb);

end.
