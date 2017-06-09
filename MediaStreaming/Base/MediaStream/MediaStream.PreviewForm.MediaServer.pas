{***********************************<_INFO>************************************}
{  <Проект>      VScreen                                                       }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Форма для проверки соединения с устройством. Открывает        }
{                соединение с указанным каналом и отображает видеопоток на     }
{                экране.                                                       }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        10.09.2007                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний                                                }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}


unit MediaStream.PreviewForm.MediaServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ufmForm_B, StdCtrls, ExtendControls, ExtCtrls,VideoPane.Control, Buttons, MediaServer.Net.Mscp.Client,MediaServer.Net.Definitions,
  ComCtrls, MediaStream.PtzControl;

type
  TfmMediaStreamPreviewMediaServer = class(TfmForm_B)
    paScreen: TPanel;
    Panel2: TPanel;
    paMonitor: TPanel;
    Bevel1: TBevel;
    Panel1: TPanel;
    edProperties: TExtendMemo;
    paPtz: TfrmPtzControl;
    paArchive: TGroupBox;
    buGetArchive: TButton;
    dtArchiveFrom: TDateTimePicker;
    dtArchiveTo: TDateTimePicker;
    ckPtzViaMscp: TExtendCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);


    procedure ckUseMscpClick(Sender: TObject);
    procedure buGetArchiveClick(Sender: TObject);
  private
    FPane : TVideoPane;
    FMscpClient: TMscpClient;
    FServerIp: string;
    FServerPort: Word;
    FSourceName: string;
    FUserName: string;
    FUserPassword: string;

    procedure OnVideoPaneError(aSender: TVideoPane; aError: Exception);
    procedure OnVideoPaneConnected(Sender: TVideoPane);
    procedure OnVideoPaneDisconnected(Sender: TVideoPane);

    procedure OnPtzControlError(aSender: TfrmPtzControl; aError: Exception);
    procedure OnPtzControlCommandExecuting(aSender: TfrmPtzControl; aCommand: TPtzCommand; var aSpeed: TPtzSpeed);

    procedure SendPtzCommandViaMscp(const aCommand: string; aSpeed: integer);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy; override;

    class procedure Run(const aServerIP: string; aServerPort: Word; const aSourceName: string; const aUserName,aPassword: string; aCustomSettings: TBytes);
  end;


implementation
  uses MediaStream.DataSource.Ms3s,Player.VideoOutput.Base, Player.VideoOutput.AllTypes,Player.AudioOutput.AllTypes;
{$R *.DFM}

{ TfmMediaStreamPreviewMediaServer }


procedure TfmMediaStreamPreviewMediaServer.buGetArchiveClick(Sender: TObject);
var
  aSourceName: string;
begin
  inherited;
  if FMscpClient=nil then
    FMscpClient:=TMscpClient.Create(FServerIp,icMSCPServerPort);

  FMscpClient.SendCommandHistoryView(FSourceName,'',dtArchiveFrom.DateTime,dtArchiveTo.DateTime,aSourceName);

  TfmMediaStreamPreviewMediaServer.Create(nil).Run(FServerIp, FServerPort, aSourceName,'root','',nil);
end;

procedure TfmMediaStreamPreviewMediaServer.ckUseMscpClick(Sender: TObject);
begin
  inherited;
  FreeAndNil(FMscpClient);
  if ckPtzViaMscp.Checked then
  try
    FMscpClient:=TMscpClient.Create(FServerIp,icMSCPServerPort);
  except
    ckPtzViaMscp.Checked:=false;
    raise;
  end;
end;

constructor TfmMediaStreamPreviewMediaServer.Create(AOwner: TComponent);
begin
  inherited;

  dtArchiveTo.DateTime:=Now;
  dtArchiveFrom.DateTime:=dtArchiveTo.DateTime-7;
end;

destructor TfmMediaStreamPreviewMediaServer.Destroy;
begin
  FreeAndNil(FMscpClient);
  FreeAndNil(FPane);
  inherited;
end;

procedure TfmMediaStreamPreviewMediaServer.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
end;

procedure TfmMediaStreamPreviewMediaServer.SendPtzCommandViaMscp(const aCommand: string; aSpeed: integer);
begin
  Assert(FMscpClient<>nil);
  FMscpClient.SendCommandPtzExecute(FSourceName,FUserName,FUserPassword, aCommand,'',0,aSpeed,true);
end;

class procedure TfmMediaStreamPreviewMediaServer.Run(const aServerIP: string; aServerPort: Word; const aSourceName: string; const aUserName,aPassword: string; aCustomSettings: TBytes);
var
  aConnectionParams_MediaServer: TMediaStreamDataSourceConnectParams_Ms3s;
begin
  with TfmMediaStreamPreviewMediaServer.Create(nil) do
  begin
    FServerIp:=aServerIP;
    FServerPort:=aServerPort;
    FSourceName:=aSourceName;
    FUserName:=aUserName;
    FUserPassword:=aPassword;

    FPane:=TVideoPane.Create(paScreen, TPlayerVideoOutput_AllTypes,TPlayerAudioOutput_AllTypes);
    FPane.SynchronousDisplay:=true; // For Debugging


    Caption:=Format('Просмотр: %s:%d/%s',[aServerIP,aServerPort,aSourceName]);
    Color:=clBlack;
    ParentBackground:=false;

    edProperties.Lines.Clear;
    edProperties.Lines.Add('Свойства соединения:');
    edProperties.Lines.Add('  Имя = '+aSourceName);
    edProperties.Lines.Add('  IP = '+aServerIP);
    edProperties.Lines.Add('  Порт = '+IntToStr(aServerPort));
    edProperties.Lines.Add('  Протокол = '+'3S');
    edProperties.Lines.Add('  Пользователь = '+aUserName);

    paPtz.Visible:=false;
    paArchive.Visible:=false;


    Show;
    FPane.Parent:=paScreen;
    FPane.Align:=alClient;
    FPane.Visible:=true;
    FPane.OnConnectFailed:=OnVideoPaneError;
    FPane.OnConnectSucceed:=OnVideoPaneConnected;
    FPane.OnDisconnected:=OnVideoPaneDisconnected;
    FPane.AudioEnabled:=true;
    FPane.KeepAspectRatio:=true;
    FPane.ShowMonitor(paMonitor);
    paMonitor.AutoSize:=true;

    paPtz.OnError:=OnPtzControlError;
    paPtz.OnCommandExecuting:=OnPtzControlCommandExecuting;

    aConnectionParams_MediaServer:=TMediaStreamDataSourceConnectParams_Ms3s.Create(
          aServerIP,aServerPort, aSourceName,aUserName,aPassword,aCustomSettings,INFINITE);

    try
      FPane.Connect(TMediaStreamDataSource_Ms3s,aConnectionParams_MediaServer);
    finally
      FreeAndNil(aConnectionParams_MediaServer);
    end;
  end;
end;

procedure TfmMediaStreamPreviewMediaServer.OnPtzControlCommandExecuting(aSender: TfrmPtzControl; aCommand: TPtzCommand; var aSpeed: TPtzSpeed);
begin
  inherited;

  if ckPtzViaMscp.Checked then
  begin
    case aCommand of
      pmLeft: begin
        SendPtzCommandViaMscp('Left',aSpeed)
      end;
      pmUp: begin
        SendPtzCommandViaMscp('Up',aSpeed)
      end;
      pmRight:begin
        SendPtzCommandViaMscp('Right',aSpeed)
      end;
      pmDown:
      begin
        SendPtzCommandViaMscp('Down',aSpeed)
      end;
      pcZoomIn: begin
        SendPtzCommandViaMscp('ZoomIn',0)
      end;
      pcZoomOut: begin
        SendPtzCommandViaMscp('ZoomOut',0)
      end;
      pcFocusIn: begin
        SendPtzCommandViaMscp('FocusIn',0)
      end;
      pcFocusOut: begin
        SendPtzCommandViaMscp('FocusOut',0)
      end;
      pcApertureInc: begin
        SendPtzCommandViaMscp('ApertureInc',0)
      end;
      pcApertureDec: begin
        SendPtzCommandViaMscp('ApertureDec',0)
      end;
    end;

    aSpeed:=0;
  end;
end;

procedure TfmMediaStreamPreviewMediaServer.OnPtzControlError(aSender: TfrmPtzControl;
  aError: Exception);
begin
  OnVideoPaneError(nil,aError);
end;

procedure TfmMediaStreamPreviewMediaServer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
  FreeAndNil(FMscpClient);
  FreeAndNil(FPane);
end;

procedure TfmMediaStreamPreviewMediaServer.OnVideoPaneConnected(Sender: TVideoPane);
begin
  paPtz.Visible:=FPane.DataSource.PtzSupported;
  paPtz.DataSource:=FPane.DataSource;
  if FPane.DataSource.PtzSupported then
    FPane.DataSource.PtzInit;

  paArchive.Visible:=MediaStream.DataSource.Ms3s.TMediaStreamDataSource_Ms3s(FPane.DataSource).Channel.ArchiveSupported;
end;

procedure TfmMediaStreamPreviewMediaServer.OnVideoPaneDisconnected(Sender: TVideoPane);
begin
  paPtz.DataSource:=nil;
end;

procedure TfmMediaStreamPreviewMediaServer.OnVideoPaneError(aSender: TVideoPane; aError: Exception);
begin
  edProperties.Lines.Add('** ОШИБКА: '+aError.Message);
end;


end.


