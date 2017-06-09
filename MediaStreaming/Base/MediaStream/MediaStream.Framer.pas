unit MediaStream.Framer;

interface
  uses Classes, SysUtils, MediaProcessing.Definitions;

type
  TStreamFramerRandomAccess = class;

  TInfoState =
    (isNotSupported, //��������� �������� �� ��������������
     isNotFound, //�������� �� �������
     isOK //��� ������, �������� ��������
    );

  TVideoInfo = record
    State : TInfoState;
    Width : integer;
    Height: integer;
  end;

  TAudioInfo = record
    State : TInfoState;
    Channels: cardinal;
    BitsPerSample: cardinal;
    SamplesPerSec: cardinal;
  end;

  TStreamFramer = class
  public
    constructor Create; virtual;

    procedure OpenStream(aStream: TStream); virtual; abstract;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; virtual; abstract;

    function StreamInfo: TBytes; virtual; abstract;
    function VideoStreamType: TStreamType; virtual; abstract;
    function VideoInfo: TVideoInfo; virtual;

    function AudioStreamType: TStreamType; virtual; abstract;
    function AudioInfo: TAudioInfo; virtual;

    function RandomAccess: TStreamFramerRandomAccess; virtual;
  end;

  TStreamInfo = record
    //������������ ����� � �������������
    Length:int64;

    //���-�� ������ �����
    VideoFrameCount: int64;
  end;

  //���������� �������� ��� ��������� ������������� �������
  TStreamFramerRandomAccess = class
  protected
    function GetPosition: int64; virtual; abstract;
    procedure SetPosition(const Value: int64); virtual; abstract;
  public
    function StreamInfo: TStreamInfo; virtual; abstract;

    //������������� �� ���������� ������� ����� ����, ������� ����� ����� ����� �������� � ������� GetNextFrame
    function SeekToPrevVideoKeyFrame: boolean; virtual; abstract;

    //������� � �������������
    property Position: int64 read GetPosition write SetPosition;
  end;

  TStreamFramerClass = class of TStreamFramer;

implementation

{ TStreamFramer }

function TStreamFramer.AudioInfo: TAudioInfo;
begin
  result.State:=isNotSupported;
end;

constructor TStreamFramer.Create;
begin

end;


function TStreamFramer.RandomAccess: TStreamFramerRandomAccess;
begin
  result:=nil;
end;

function TStreamFramer.VideoInfo: TVideoInfo;
begin
  result.State:=isNotSupported;
end;

end.
