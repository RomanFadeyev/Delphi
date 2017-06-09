unit MediaStream.PtzProtocol.Base;

interface

type
  TPtzProtocol = class
    //��������
    procedure PtzMoveUp(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveUpStop; virtual; abstract;
    procedure PtzMoveDown(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveDownStop; virtual; abstract;
    procedure PtzMoveLeft(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveLeftStop; virtual; abstract;
    procedure PtzMoveRight(aDuration: cardinal; aSpeed: byte); virtual; abstract;
    procedure PtzMoveRightStop; virtual; abstract;

    //===== ����������� �� �������� �������
    //�������� �� ��������� �����-������
    procedure PtzMoveToPoint(aId: cardinal); virtual; abstract;
    //�������� � ��������� �������. ������� ����������� �� ��� X � Y � ��������
    procedure PtzMoveToPosition(const aPositionPan,aPositionTilt: double); virtual; abstract;

    //�������
    procedure PtzZoomIn(aDuration: cardinal); virtual; abstract;
    procedure PtzZoomInStop; virtual; abstract;
    procedure PtzZoomOut(aDuration: cardinal); virtual; abstract;
    procedure PtzZoomOutStop; virtual; abstract;
    //�����
    procedure PtzFocusIn(aDuration: cardinal); virtual; abstract;
    procedure PtzFocusInStop; virtual; abstract;
    procedure PtzFocusOut(aDuration: cardinal); virtual; abstract;
    procedure PtzFocusOutStop; virtual; abstract;

    //�������� (����������� �������� ���� � ������������� �������������� �������� ������� �����������)
    procedure PtzApertureIncrease(aDuration: cardinal); virtual; abstract;
    procedure PtzApertureIncreaseStop; virtual; abstract;
    procedure PtzApertureDecrease(aDuration: cardinal); virtual; abstract;
    procedure PtzApertureDecreaseStop; virtual; abstract;
  end;

implementation

end.
