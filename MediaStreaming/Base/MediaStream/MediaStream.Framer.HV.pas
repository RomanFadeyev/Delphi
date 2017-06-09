unit MediaStream.Framer.HV;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer,H264Parser;

type
  TStreamFramerHV = class (TStreamFramer)
  private
    FStream : TStream;
    FH264Parser: TH264Parser;
    FBuffer: TBytes;
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
  uses HikVisionAPI;

{ TStreamFramerHV }

constructor TStreamFramerHV.Create;
begin
  inherited;
  FH264Parser:=TH264Parser.Create;
end;

destructor TStreamFramerHV.Destroy;
begin
  FStream:=nil;
  FreeAndNil(FH264Parser);
  inherited;
end;

function TStreamFramerHV.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
var
  aFrameType: FrameType_t;
  aDataSize: cardinal;
begin
  Assert(FStream<>nil);

  if FStream.Read(aFrameType,sizeof(aFrameType))<sizeof(aFrameType) then
    exit(false);

  FStream.ReadBuffer(aDataSize,sizeof(aDataSize));
  if cardinal(Length(FBuffer))<aDataSize then
    SetLength(FBuffer,aDataSize*3 div 2);
  FStream.ReadBuffer(FBuffer[0],aDataSize);


  aOutFormat.Clear;

  if aFrameType = PktAudioFrames then
    aOutFormat.biMediaType:=mtAudio
  else if aFrameType in [PktIFrames,PktPFrames] then
    aOutFormat.biMediaType:=mtVideo
  else begin
    result:=GetNextFrame(aOutFormat,aOutData,aOutDataSize,aOutInfo,aOutInfoSize);
    exit;
  end;

  aOutData:=FBuffer;
  aOutDataSize:=aDataSize;

  if aOutFormat.biMediaType=mtVideo then
  begin
    result:=HikVisionGetH264Data(aOutData,aOutDataSize);
    if not result then
      exit;

    aOutFormat.biStreamType:=stH264;
    aOutFormat.VideoWidth:=FH264Parser.LastParams.PictureWidth;
    aOutFormat.VideoHeight:=FH264Parser.LastParams.PictureHeight;
    //aOutFormat.biBitCount:=24; //??
    //aOutFormat.DataSize:=aDataSize;

    Assert(PDWORD(aOutData)^=$1000000); //NAL_START_MARKER
    FH264Parser.Parse(PByte(aOutData)+4,aDataSize-4);
  end
  else begin
    result:=true; //??
    Assert(false); //TODO
  end;

  aOutInfo:=nil;
  aOutInfoSize:=0;

  if aFrameType=PktIFrames then
    Include(aOutFormat.biFrameFlags,ffKeyFrame);
end;

procedure TStreamFramerHV.OpenStream(aStream: TStream);
var
  c: array [0..3] of AnsiChar;
begin
  FStream:=aStream;

  FStream.ReadBuffer(c[0],4);
  if (c[0]<>'H') or (c[1]<>'V') or (c[2]<>'D') or (c[3]<>'P') then
    raise Exception.Create('Неверный формат файла');
end;

function TStreamFramerHV.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerHV.VideoStreamType: TStreamType;
begin
  result:=stH264;
end;

function TStreamFramerHV.AudioStreamType: TStreamType;
begin
  result:=0; //TODO
end;

end.


