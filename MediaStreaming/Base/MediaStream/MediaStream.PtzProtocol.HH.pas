unit MediaStream.PtzProtocol.HH;

interface
  uses SysUtils, MediaStream.PtzProtocol.Base,MediaServer.Net.Ms3s.Stream, HHCommon,HHNet,HHNetAPI;

type
  TPtzProtocol_HH = class (TPtzProtocol)
  private
    FServer  : THHNetServer;
    FChannelNo: integer;

    procedure CheckValid;
  public
    procedure Init(aServer: THHNetServer; aChannelNo: integer);

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
  end;

implementation

procedure TPtzProtocol_HH.CheckValid;
begin
  if FServer=nil then
    raise Exception.Create('Не инициализирован канал');
end;

procedure TPtzProtocol_HH.Init(aServer: THHNetServer; aChannelNo: integer);
begin
  FServer:=aServer;
  FChannelNo:=aChannelNo;
end;

procedure TPtzProtocol_HH.PtzApertureDecrease;
begin
  CheckValid;
  FServer.PtzApertureDecrease(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzApertureDecreaseStop;
begin
  CheckValid;
  FServer.PtzApertureDecreaseStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzApertureIncrease;
begin
  CheckValid;
  FServer.PtzApertureIncrease(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzApertureIncreaseStop;
begin
  CheckValid;
  FServer.PtzApertureIncreaseStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzFocusIn;
begin
  CheckValid;
  FServer.PtzFocusIn(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzFocusInStop;
begin
  CheckValid;
  FServer.PtzFocusInStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzFocusOut;
begin
  CheckValid;
  FServer.PtzFocusOut(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzFocusOutStop;
begin
  CheckValid;
  FServer.PtzFocusOutStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzMoveDown(aDuration: cardinal; aSpeed: byte);
begin
  CheckValid;
  FServer.PtzMoveDown(FChannelNo,aSpeed);
end;

procedure TPtzProtocol_HH.PtzMoveDownStop;
begin
  CheckValid;
  FServer.PtzMoveDownStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzMoveLeft(aDuration: cardinal; aSpeed: byte);
begin
  CheckValid;
  FServer.PtzMoveLeft(FChannelNo,aSpeed);
end;

procedure TPtzProtocol_HH.PtzMoveLeftStop;
begin
  CheckValid;
  FServer.PtzMoveLeftStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzMoveRight(aDuration: cardinal; aSpeed: byte);
begin
  CheckValid;
  FServer.PtzMoveRight(FChannelNo,aSpeed);
end;

procedure TPtzProtocol_HH.PtzMoveRightStop;
begin
  CheckValid;
  FServer.PtzMoveRightStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzMoveToPoint(aId: cardinal);
begin
  if aId>255 then
    raise Exception.Create('Недопустимый идентификатор точки');

  FServer.PtzMoveToPreset(FChannelNo,aId);
end;

procedure TPtzProtocol_HH.PtzMoveToPosition(const aPositionPan,
  aPositionTilt: double);
begin
  CheckValid;
  FServer.PtzMoveToPosition(FChannelNo,aPositionPan,aPositionTilt);
end;

procedure TPtzProtocol_HH.PtzMoveUp(aDuration: cardinal; aSpeed: byte);
begin
  CheckValid;
  FServer.PtzMoveUp(FChannelNo,aSpeed);
end;

procedure TPtzProtocol_HH.PtzMoveUpStop;
begin
  CheckValid;
  FServer.PtzMoveUpStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzZoomIn(aDuration: cardinal);
begin
  CheckValid;
  FServer.PtzZoomIn(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzZoomInStop;
begin
  CheckValid;
  FServer.PtzZoomInStop(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzZoomOut(aDuration: cardinal);
begin
  CheckValid;
  FServer.PtzZoomOut(FChannelNo);
end;

procedure TPtzProtocol_HH.PtzZoomOutStop;
begin
  CheckValid;
  FServer.PtzZoomOutStop(FChannelNo);
end;
end.
