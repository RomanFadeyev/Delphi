unit MediaProcessing.Common.Processor.CustomSettings;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Common.SettingsDialog;

type
  TMediaProcessor_CustomSettings<T: TfmMediaProcessingSettings,constructor>=class (TMediaProcessor)
  protected
    procedure OnLoadPropertiesToDialog(aDialog: T); virtual;
    procedure OnSavePropertiesFromDialog(aDialog: T); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  HasCustomProperties: boolean; override;
    procedure ShowCustomProperiesDialog;override;
  end;

implementation
  uses Controls,uBaseClasses;


{ TMediaProcessor_CustomSettings }

constructor TMediaProcessor_CustomSettings<T>.Create;
begin
  inherited;
end;

destructor TMediaProcessor_CustomSettings<T>.Destroy;
begin
  inherited;
end;

function TMediaProcessor_CustomSettings<T>.HasCustomProperties: boolean;
begin
  result:=true;
end;

procedure TMediaProcessor_CustomSettings<T>.OnLoadPropertiesToDialog(
  aDialog: T);
begin

end;

procedure TMediaProcessor_CustomSettings<T>.OnSavePropertiesFromDialog(
  aDialog: T);
begin

end;

procedure TMediaProcessor_CustomSettings<T>.ShowCustomProperiesDialog;
var
  aDialog: T;
begin
  aDialog:=T.Create;
  try
    OnLoadPropertiesToDialog(aDialog);

    if aDialog.ShowModal=mrOK then
    begin
      OnSavePropertiesFromDialog(aDialog);
    end;
  finally
    aDialog.Free;
  end;

end;


end.
