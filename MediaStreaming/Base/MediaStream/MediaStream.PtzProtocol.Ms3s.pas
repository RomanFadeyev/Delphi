unit MediaStream.PtzProtocol.Ms3s;

interface
uses SysUtils, MediaStream.PtzProtocol.Base,MediaServer.Net.Ms3s.Stream;

type
  TPtzProtocol_Ms3s = class (TPtzProtocol)
  private
    FStream: TMediaServerStream;

    procedure CheckValid;
  public
    procedure Init(aStream: TMediaServerStream);

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

procedure TPtzProtocol_Ms3s.CheckValid;
begin
  if FStream=nil then
    raise Exception.Create('Не инициализирован канал');
end;

procedure TPtzProtocol_Ms3s.Init(aStream: TMediaServerStream);
begin
  FStream:=aStream;
end;

procedure TPtzProtocol_Ms3s.PtzApertureDecrease(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzApertureDecrease(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzApertureDecreaseStop;
begin
  CheckValid;
  FStream.PtzApertureDecreaseStop;
end;

procedure TPtzProtocol_Ms3s.PtzApertureIncrease(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzApertureIncrease(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzApertureIncreaseStop;
begin
  CheckValid;
  FStream.PtzApertureIncreaseStop;
end;

procedure TPtzProtocol_Ms3s.PtzFocusIn(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzFocusIn(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzFocusInStop;
begin
  CheckValid;
  FStream.PtzFocusInStop;
end;

procedure TPtzProtocol_Ms3s.PtzFocusOut(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzFocusOut(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzFocusOutStop;
begin
  CheckValid;
  FStream.PtzFocusOutStop;
end;

procedure TPtzProtocol_Ms3s.PtzMoveDown(aDuration: cardinal;aSpeed: byte);
begin
  CheckValid;
  FStream.PtzMoveDown(aDuration, aSpeed);
end;

procedure TPtzProtocol_Ms3s.PtzMoveDownStop;
begin
  CheckValid;
  FStream.PtzMoveDownStop;
end;

procedure TPtzProtocol_Ms3s.PtzMoveLeft(aDuration: cardinal;aSpeed: byte);
begin
  CheckValid;
  FStream.PtzMoveLeft(aDuration, aSpeed);
end;

procedure TPtzProtocol_Ms3s.PtzMoveLeftStop;
begin
  CheckValid;
  FStream.PtzMoveLeftStop;
end;

procedure TPtzProtocol_Ms3s.PtzMoveRight(aDuration: cardinal;aSpeed: byte);
begin
  CheckValid;
  FStream.PtzMoveRight(aDuration, aSpeed);
end;

procedure TPtzProtocol_Ms3s.PtzMoveRightStop;
begin
  CheckValid;
  FStream.PtzMoveRightStop;
end;

procedure TPtzProtocol_Ms3s.PtzMoveToPoint(aId: cardinal);
begin
  CheckValid;
  FStream.PtzMoveToPoint(aId);
end;

procedure TPtzProtocol_Ms3s.PtzMoveToPosition(const aPositionPan,aPositionTilt: double);
begin
  CheckValid;
  FStream.PtzMoveToPosition(aPositionPan, aPositionTilt);
end;

procedure TPtzProtocol_Ms3s.PtzMoveUp(aDuration: cardinal;aSpeed: byte);
begin
  CheckValid;
  FStream.PtzMoveUp(aDuration, aSpeed);
end;

procedure TPtzProtocol_Ms3s.PtzMoveUpStop;
begin
  CheckValid;
  FStream.PtzMoveUpStop;
end;


procedure TPtzProtocol_Ms3s.PtzZoomIn(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzZoomIn(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzZoomInStop;
begin
  CheckValid;
  FStream.PtzZoomInStop;
end;

procedure TPtzProtocol_Ms3s.PtzZoomOut(aDuration: cardinal);
begin
  CheckValid;
  FStream.PtzZoomOut(aDuration);
end;

procedure TPtzProtocol_Ms3s.PtzZoomOutStop;
begin
  CheckValid;
  FStream.PtzZoomOutStop;
end;

end.
