{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь PCM. Декларация                               }
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

unit MediaProcessing.Transformer.PCM;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global;

type
  TMediaProcessor_Transformer_Pcm=class (TMediaProcessor,IMediaProcessor_Transformer_Pcm)
  private
    FSomeSetting : integer; //Пример настройки
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    class function MetaInfo:TMediaProcessorInfo; override;

    property SomeSetting : integer read FSomeSetting;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  HasCustomProperties: boolean; override;
    procedure ShowCustomProperiesDialog;override;
  end;

implementation
  uses Controls,MediaProcessing.Transformer.Pcm.SettingsDialog;


{ TMediaProcessor_Transformer_Pcm }

constructor TMediaProcessor_Transformer_Pcm.Create;
begin
  inherited;
end;
//------------------------------------------------------------------------------
destructor TMediaProcessor_Transformer_Pcm.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------
function TMediaProcessor_Transformer_Pcm.HasCustomProperties: boolean;
begin
  result:=true;
end;
//------------------------------------------------------------------------------
class function TMediaProcessor_Transformer_Pcm.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Transformer_Pcm;
  result.Name:='Преобразование';
  result.Description:='Выполняет различные преобразования аудиопотока';
  result.SetInputStreamType(stPCM);
  result.OutputStreamType:=stPCM;
  result.ConsumingLevel:=0;
end;
//------------------------------------------------------------------------------
procedure TMediaProcessor_Transformer_Pcm.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FSomeSetting:=aReader.ReadInteger('SomeSetting',0);
end;
//------------------------------------------------------------------------------
procedure TMediaProcessor_Transformer_Pcm.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('SomeSetting', FSomeSetting);
end;
//------------------------------------------------------------------------------
procedure TMediaProcessor_Transformer_Pcm.ShowCustomProperiesDialog;
var
  aDialog: TfmPcmTransformer_Settings;
begin
  aDialog:=TfmPcmTransformer_Settings.Create(nil);
  try
    if aDialog.ShowModal=mrOK then
    begin
      //Разместите здесь свой код по сохранению настроек
      FSomeSetting:=1;
    end;
  finally
    aDialog.Free;
  end;

end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_Transformer_Pcm);

end.
