{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь медиа-потока в формате Beward (MP6 или HH)    }
{                Декларация                                                    }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        21.01.2011                                                    }
{                                                                              }
{  <Примечание>  Отсутствует                                                   }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaProcessing.Transformer.HH;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global;

type
  TChangeFPSMode = (cfmNone,cfmAbsolute, cfmVIFrameOnly);

  TMediaProcessor_Transformer_Hh=class (TMediaProcessor,IMediaProcessor_Transformer_Hh)
  protected
    FChangeFPSMode : TChangeFPSMode;
    FFrameIntervalValue: integer;
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
  uses Controls,uBaseClasses,MediaProcessing.Transformer.Hh.SettingsDialog;


{ TMediaProcessor_Transformer_Hh }

constructor TMediaProcessor_Transformer_Hh.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Transformer_Hh.Destroy;
begin
  inherited;
end;

function TMediaProcessor_Transformer_Hh.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_Transformer_Hh.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Transformer_Hh;
  result.Name:='Преобразователь HH';
  result.Description:='Изменяет параметры формата HH (камеры Beward)';
  result.SetInputStreamType(stHHVI);
  result.OutputStreamType:=stHHVI;
  result.ConsumingLevel:=0;
end;

procedure TMediaProcessor_Transformer_Hh.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FChangeFPSMode:=TChangeFPSMode(aReader.ReadInteger('ChangeFPSMode',0));
  FFrameIntervalValue:=aReader.ReadInteger('FrameIntervalValue',10);
  FVIFramesOnly:=aReader.ReadBool('VIFramesOnly',false)
end;

procedure TMediaProcessor_Transformer_Hh.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('ChangeFPSMode',integer(FChangeFPSMode));
  aWriter.WriteInteger('FrameIntervalValue',FFrameIntervalValue);
  aWriter.WriteBool('VIFramesOnly',FVIFramesOnly);
end;

procedure TMediaProcessor_Transformer_Hh.ShowCustomProperiesDialog;
var
  aDialog: TfmHh_Settings;
begin
  aDialog:=TfmHh_Settings.Create(nil);
  try
    aDialog.ckChangeFPS.Checked:=FChangeFPSMode<>cfmNone;
    aDialog.ckChangeFPSAbsolute.Checked:=FChangeFPSMode in [cfmAbsolute,cfmNone];
    aDialog.ckFPSVIFramesOnly.Checked:=FChangeFPSMode=cfmVIFrameOnly;
    aDialog.edFPSValue.Value:=FFrameIntervalValue;

    if aDialog.ShowModal=mrOK then
    begin
      FChangeFPSMode:=cfmNone;
      if aDialog.ckChangeFPS.Checked then
      begin
        if aDialog.ckChangeFPSAbsolute.Checked then
        begin
          FChangeFPSMode:=cfmAbsolute;
          FFrameIntervalValue:=aDialog.edFPSValue.Value
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
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Transformer_Hh);

end.
