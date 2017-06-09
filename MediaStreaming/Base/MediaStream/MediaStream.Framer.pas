unit MediaStream.Framer;

interface
  uses Classes, SysUtils, MediaProcessing.Definitions;

type
  TStreamFramerRandomAccess = class;

  TInfoState =
    (isNotSupported, //Получение сведений не поддерживается
     isNotFound, //Сведения не найдены
     isOK //Все хорошо, сведения доступны
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
    //Длительность файла в миллисекундах
    Length:int64;

    //Кол-во кадров видео
    VideoFrameCount: int64;
  end;

  //Расширение фреймера для поддержки произвольного доступа
  TStreamFramerRandomAccess = class
  protected
    function GetPosition: int64; virtual; abstract;
    procedure SetPosition(const Value: int64); virtual; abstract;
  public
    function StreamInfo: TStreamInfo; virtual; abstract;

    //Переместиться на предыдущий опорный видео кадр, который затем можно будет получить с помощью GetNextFrame
    function SeekToPrevVideoKeyFrame: boolean; virtual; abstract;

    //Позиция в миллисекундах
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
