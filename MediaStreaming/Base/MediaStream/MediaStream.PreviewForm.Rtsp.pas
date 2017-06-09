{***********************************<_INFO>************************************}
{  <Проект>      ???                                                           }
{                                                                              }
{  <Область>     ???                                                           }
{                                                                              }
{  <Задача>      Форма для проверки соединения с устройством                   }
{                                                                              }
{  <Автор>       Р.В. Фадеев                                                   }
{                                                                              }
{  <Дата>        10.09.2007                                                    }
{                                                                              }
{***********************************</_INFO>***********************************}


unit MediaStream.PreviewForm.Rtsp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ufmForm_B, StdCtrls, MediaStream.PreviewForm.Base,
  MediaStream.PreviewControl;

type
  TfmMediaStreamPreviewRtsp = class(TfmMediaStreamPreviewBase)
  public
    class procedure Run(const aURL: string; aOverTCP: boolean); overload;
    class procedure Run(const aIP: string; aPort: integer; const aSourceName: string; const aUserName,aPassword: string; aOverTCP: boolean); overload;
  end;


implementation
  uses MediaStream.DataSource.RTSP,MediaStream.UrlFormats;
{$R *.DFM}

{ TfmMediaStreamPreviewRtsp }

class procedure TfmMediaStreamPreviewRtsp.Run(const aURL: string; aOverTCP: boolean);
var
  aConnectParams: TMediaStreamDataSourceConnectParams_RTSP;
begin
  with TfmMediaStreamPreviewRtsp.Create(nil) do
  begin
    Caption:=Format('Просмотр: %s',[aURL]);

    with frmVideoViewControl do
    begin
      edProperties.Lines.Clear;
      edProperties.Lines.Add('Свойства соединения:');
      edProperties.Lines.Add('  URL = '+aURL);
      if aOverTCP then
        edProperties.Lines.Add('  Протокол = RTP over TCP')
      else
        edProperties.Lines.Add('  Протокол = UDP');
    end;

    aConnectParams:=TMediaStreamDataSourceConnectParams_RTSP.Create(aURL,aOverTCP);
    Show(TMediaStreamDataSource_RTSP,aConnectParams,true);
  end;
end;

class procedure TfmMediaStreamPreviewRtsp.Run(const aIP: string; aPort: integer;
  const aSourceName, aUserName, aPassword: string; aOverTCP: boolean);
begin
  Run(MakeRtspUrl(aIp, aPort,aSourceName,aUserName,aPassword),aOverTCP);
end;

end.