unit MediaStream.Framer.Jpeg;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer,MediaStream.Framer.Picture,
       Jpeg;

type
  TStreamFramerJpeg = class (TStreamFramerPicture)
  private
    FJpeg: TBytes;
    FWidth,FHeight: integer;
    FFrameNo: int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; override;

    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function StreamInfo: TBytes; override;

  end;

implementation

{ TStreamFramerJpeg }

constructor TStreamFramerJpeg.Create;
begin
  inherited;
end;

destructor TStreamFramerJpeg.Destroy;
begin
  inherited;
end;

function TStreamFramerJpeg.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
begin
  result:=true;


  aOutData:=@FJpeg[0];
  aOutDataSize:=Length(FJpeg);

  aOutInfo:=nil;
  aOutInfoSize:=0;

  aOutFormat.Clear;
  aOutFormat.biMediaType:=mtVideo;
  aOutFormat.biStreamType:=stMJPEG;
  aOutFormat.biFrameFlags:=[ffKeyFrame];
  aOutFormat.VideoWidth:=FWidth;
  aOutFormat.VideoHeight:=FHeight;
  aOutFormat.VideoBitCount:=24;
  aOutFormat.TimeStamp:=FFrameNo*FrameInterval;
  inc(FFrameNo);
  Assert(aOutFormat.biStreamType=VideoStreamType);
end;

procedure TStreamFramerJpeg.OpenStream(aStream: TStream);
var
  aJpeg: TJPEGImage;
  aPos: int64;
begin
  aPos:=aStream.Position;
  aJpeg:=TJPEGImage.Create;
  try
    aJpeg.LoadFromStream(aStream);
    SetLength(FJpeg,aStream.Position-aPos);
    aStream.Seek(aPos,soFromBeginning);
    aStream.ReadBuffer(FJpeg[0],Length(FJpeg));

    FWidth:=aJpeg.Width;
    FHeight:=aJpeg.Height;
  finally
    aJpeg.Free;
  end;
  FFrameNo:=0;
end;

function TStreamFramerJpeg.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerJpeg.VideoStreamType: TStreamType;
begin
  result:=stMJPEG;
end;

function TStreamFramerJpeg.AudioStreamType: TStreamType;
begin
  result:=0;
end;


end.


