unit MediaStream.PtzProtocol.Base;

interface

type
  TPtzProtocol = class
    //Движение
    procedure PtzMoveUp(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveUpStop; virtual; abstract;
    procedure PtzMoveDown(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveDownStop; virtual; abstract;
    procedure PtzMoveLeft(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveLeftStop; virtual; abstract;
    procedure PtzMoveRight(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveRightStop; virtual; abstract;

    //===== Перемещение на заданную позицию
    //Движение на указанную точку-пресет
    procedure PtzMoveToPoint(aId: cardinal); virtual; abstract;
    //Движение в указанную позицию. Позиция указывается по оси X и Y в градусах
    procedure PtzMoveToPosition(const aPositionPan,aPositionTilt: double); virtual; abstract;

    //Масштаб
    procedure PtzZoomIn(aDuration: cardinal); virtual; abstract;
    procedure PtzZoomInStop; virtual; abstract;
    procedure PtzZoomOut(aDuration: cardinal); virtual; abstract;
    procedure PtzZoomOutStop; virtual; abstract;
    //Фокус
    procedure PtzFocusIn(aDuration: cardinal); virtual; abstract;
    procedure PtzFocusInStop; virtual; abstract;
    procedure PtzFocusOut(aDuration: cardinal); virtual; abstract;
    procedure PtzFocusOutStop; virtual; abstract;

    //Апертура (способность собирать свет и противостоять дифракционному размытию деталей изображения)
    procedure PtzApertureIncrease(aDuration: cardinal); virtual; abstract;
    procedure PtzApertureIncreaseStop; virtual; abstract;
    procedure PtzApertureDecrease(aDuration: cardinal); virtual; abstract;
    procedure PtzApertureDecreaseStop; virtual; abstract;
  end;

implementation

end.
