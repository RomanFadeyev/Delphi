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


unit MediaStream.PreviewForm.HH;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ufmForm_B, HHCommon,StdCtrls, MediaStream.PreviewForm.Base,
  MediaStream.PreviewControl;

type
  TfmMediaStreamPreviewHH = class(TfmMediaStreamPreviewBase)
  public
    class procedure Run(const aIP: string; aPort: integer; aProtocol: THHNetProtocol; aChannelNo,aSubChannelNo: integer; const aUserName,aPassword: string; aTransmitAudio: boolean);
  end;


implementation
  uses MediaStream.DataSource.HH,MediaStream.UrlFormats;
{$R *.DFM}

{ TfmMediaStreamPreviewHH }

class procedure TfmMediaStreamPreviewHH.Run(const aIP: string; aPort: integer; aProtocol: THHNetProtocol; aChannelNo,aSubChannelNo: integer; const aUserName,aPassword: string; aTransmitAudio: boolean);
var
  aConnectParams: TMediaStreamDataSourceConnectParams_HH;
begin
  with TfmMediaStreamPreviewHH.Create(Application) do
  begin
    Caption:='Просмотр: '+MakeBewardUrl(aIP,aPort,aChannelNo,aSubChannelNo);

    with frmVideoViewControl do
    begin
      edProperties.Lines.Clear;
      edProperties.Lines.Add('Свойства соединения:');
      edProperties.Lines.Add('  IP = '+aIP);
      edProperties.Lines.Add('  Порт = '+IntToStr(aPort));
      edProperties.Lines.Add(Format('  Канал = %d/%d',[aChannelNo+1,aSubChannelNo+1]));
      edProperties.Lines.Add('  Протокол = '+NetProtocolNames[aProtocol]);
    end;

    aConnectParams:=TMediaStreamDataSourceConnectParams_HH.Create(aIP,aPort,aChannelNo,aSubChannelNo, aProtocol,aUserName,aPassword,true,aTransmitAudio);
    Show(TMediaStreamDataSource_HH,aConnectParams,aTransmitAudio);
  end;
end;


end.