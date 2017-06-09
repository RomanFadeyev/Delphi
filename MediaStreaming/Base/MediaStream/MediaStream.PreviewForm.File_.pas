unit MediaStream.PreviewForm.File_;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MediaStream.PreviewForm.Base,
  MediaStream.PreviewControl;

type
  TfmMediaStreamPreviewFile = class(TfmMediaStreamPreviewBase)
  private
    { Private declarations }
  public
    class procedure Run(const aFileName: string; aAudioEnabled: boolean); overload;
  end;


implementation
  uses MediaStream.DataSource.File_,MediaStream.UrlFormats;

{$R *.dfm}

{ TfmMediaStreamPreviewFile }

class procedure TfmMediaStreamPreviewFile.Run(const aFileName: string; aAudioEnabled: boolean);
var
  aConnectParams: TMediaStreamDataSourceConnectParams_File;
begin
  with TfmMediaStreamPreviewFile.Create(nil) do
  begin
    Caption:=Format('Просмотр: %s',[aFileName]);

    with frmVideoViewControl do
    begin
      edProperties.Lines.Clear;
      edProperties.Lines.Add('Свойства соединения:');
      edProperties.Lines.Add('  FileName = '+aFileName);
    end;

    aConnectParams:=TMediaStreamDataSourceConnectParams_File.Create(aFileName);
    Show(TMediaStreamDataSource_File,aConnectParams,aAudioEnabled);
  end;
end;

end.
