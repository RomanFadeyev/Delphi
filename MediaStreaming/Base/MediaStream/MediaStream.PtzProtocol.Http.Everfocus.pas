unit MediaStream.PtzProtocol.Http.EverFocus;

interface
uses SysUtils, MediaStream.PtzProtocol.Base,MediaStream.PtzProtocol.Http;

type
  TPtzProtocol_Http_EverFocus = class (TPtzProtocol_Http)
  private
  public
    procedure PtzApertureDecrease(aDuration: cardinal); override;
    procedure PtzApertureDecreaseStop; override;
    procedure PtzApertureIncrease(aDuration: cardinal); override;
    procedure PtzApertureIncreaseStop; override;
    procedure PtzFocusIn(aDuration: cardinal); override;
    procedure PtzFocusInStop; override;
    procedure PtzFocusOut(aDuration: cardinal); override;
    procedure PtzFocusOutStop; override;
    procedure PtzZoomIn(aDuration: cardinal); override;
    procedure PtzZoomInStop; override;
    procedure PtzZoomOut(aDuration: cardinal); override;
    procedure PtzZoomOutStop; override;

    procedure PtzMoveDown(aDuration: cardinal; aSpeed: byte); override;
    procedure PtzMoveDownStop; override;
    procedure PtzMoveLeft(aDuration: cardinal; aSpeed: byte); override;
    procedure PtzMoveLeftStop; override;
    procedure PtzMoveRight(aDuration: cardinal; aSpeed: byte); override;
    procedure PtzMoveRightStop; override;
    procedure PtzMoveUp(aDuration: cardinal; aSpeed: byte); override;
    procedure PtzMoveUpStop; override;

    //===== Перемещение на заданную позицию
    //Движение на указанную точку-пресет
    procedure PtzMoveToPoint(aId: cardinal);override;
    //Движение в указанную позицию. Позиция указывается по оси X и Y в градусах
    procedure PtzMoveToPosition(const aPositionPan,aPositionTilt: double); override;

    class function Name: string; override;
  end;

implementation

class function TPtzProtocol_Http_EverFocus.Name: string;
begin
  result:='Http EverFocus';
end;

procedure TPtzProtocol_Http_EverFocus.PtzApertureDecrease(aDuration: cardinal);
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzApertureDecreaseStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzApertureIncrease(aDuration: cardinal);
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzApertureIncreaseStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzFocusIn(aDuration: cardinal);
begin
  ExecutePost('com/ptz.cgi?rfocus=100');
end;

procedure TPtzProtocol_Http_EverFocus.PtzFocusInStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzFocusOut(aDuration: cardinal);
begin
  ExecutePost('com/ptz.cgi?rfocus=-100');
end;

procedure TPtzProtocol_Http_EverFocus.PtzFocusOutStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveDown(aDuration: cardinal;aSpeed: byte);
begin
  ExecutePost('com/ptz.cgi?move=down');
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveDownStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveLeft(aDuration: cardinal;aSpeed: byte);
begin
  ExecutePost('com/ptz.cgi?move=left');
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveLeftStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveRight(aDuration: cardinal;aSpeed: byte);
begin
  ExecutePost('com/ptz.cgi?move=right');
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveRightStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveToPoint(aId: cardinal);
begin
  ExecutePost('com/ptz.cgi?gotoserverpresetno='+IntToStr(aId));
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveToPosition(const aPositionPan,aPositionTilt: double);
begin
  ExecutePost('com/ptz.cgi?pan='+StringReplace(FloatToStr(aPositionPan),DecimalSeparator,'.',[]));
  ExecutePost('com/ptz.cgi?tilt='+StringReplace(FloatToStr(aPositionTilt),DecimalSeparator,'.',[]));
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveUp(aDuration: cardinal;aSpeed: byte);
begin
  ExecutePost('com/ptz.cgi?move=up');
end;

procedure TPtzProtocol_Http_EverFocus.PtzMoveUpStop;
begin
end;


procedure TPtzProtocol_Http_EverFocus.PtzZoomIn(aDuration: cardinal);
begin
  ExecutePost('com/ptz.cgi?rzoom=100');
end;

procedure TPtzProtocol_Http_EverFocus.PtzZoomInStop;
begin
end;

procedure TPtzProtocol_Http_EverFocus.PtzZoomOut(aDuration: cardinal);
begin
  ExecutePost('com/ptz.cgi?rzoom=-100');
end;

procedure TPtzProtocol_Http_EverFocus.PtzZoomOutStop;
begin
end;

initialization
  ProtocolRegistry.AddProtocol(TPtzProtocol_Http_EverFocus);


end.
