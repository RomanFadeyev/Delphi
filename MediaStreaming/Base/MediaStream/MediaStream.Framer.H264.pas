unit MediaStream.Framer.H264;

interface
  uses Windows,SysUtils,Classes, BufferedStream, MediaProcessing.Definitions,
  MediaStream.Framer,H264Parser,H264Def;

type
  TStreamFramerH264 = class (TStreamFramer)
  private
    FStream: TStream;
    FReader: TBufferedStream;
    FBuffer: TMemoryStream;
    FParser : TH264Parser;
    FProcessedFrameCount: int64;

    function GetUpToNextNAL: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; overload; override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal; out aNal: TNalUnitType): boolean; reintroduce; overload;


    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function StreamInfo: TBytes; override;

    property Parser: TH264Parser read FParser;
  end;

implementation

{ TStreamFramerH264 }

constructor TStreamFramerH264.Create;
begin
  inherited;
  FBuffer:=TMemoryStream.Create;
  FParser:=TH264Parser.Create;
end;

destructor TStreamFramerH264.Destroy;
begin
  inherited;
  FreeAndNil(FBuffer);
  FreeAndNil(FReader);
  FreeAndNil(FParser);
  FStream:=nil;
end;

function TStreamFramerH264.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal; out aNal: TNalUnitType): boolean;
var
  aPayload: byte;
  aTmp: integer;
  aFrameRate: double;
begin
  result:=GetUpToNextNAL;

  if result then
  begin
    //
    //|0|1|2|3|4|5|6|7|
    //+-+-+-+-+-+-+-+-+
    //|F|NRI| Type |
    //+---------------+

    aTmp:=(PByte(FBuffer.Memory)+4)^;
    Assert(aTmp shr 7=0);
    aPayload:=aTmp and $1F;
    Assert(aPayload in [1..23]);

    aNal:=TNalUnitType(aPayload);

    //SEQ Param
    if aNal = NAL_UT_SEQ_PARAM then
      FParser.Parse(PByte(FBuffer.Memory)+4,FBuffer.Size-4);

    aOutData:=FBuffer.Memory;
    aOutDataSize:=FBuffer.Position;

    Assert(PCardinal(aOutData)^=NAL_START_MARKER);
    Assert(aOutDataSize>4);
    Assert(PCardinal((PByte(aOutData)+aOutDataSize-4))^<>NAL_START_MARKER);


    aOutInfo:=nil;
    aOutInfoSize:=0;

    aOutFormat.Clear;
    aOutFormat.biMediaType:=mtVideo;
    aOutFormat.biStreamType:=VideoStreamType;
    aOutFormat.VideoWidth:=FParser.LastParams.PictureWidth;
    aOutFormat.VideoHeight:=FParser.LastParams.PictureHeight;
    //aOutFormat.biBitCount:=24; //TODO ??
    //aOutFormat.DataSize:=aOutDataSize;

    if aNal in [NAL_UT_SLICE_IDR,NAL_UT_SEQ_PARAM, NAL_UT_PIC_PARAM] then
      Include(aOutFormat.biFrameFlags,ffKeyFrame);

    if aNal in [NAL_UT_SEQ_PARAM,NAL_UT_PIC_PARAM] then
      Include(aOutFormat.biFrameFlags,ffInitParamsFrame);

    aFrameRate:=25;
    if FParser.LastParams.frame_rate<>0 then
      aFrameRate:=FParser.LastParams.frame_rate;

    if aFrameRate>0 then
      aOutFormat.TimeStamp:=Trunc(FProcessedFrameCount*1000/aFrameRate);
    inc(FProcessedFrameCount);
  end;
end;

function TStreamFramerH264.GetNextFrame(out aOutFormat: TMediaStreamDataHeader;
  out aOutData: pointer; out aOutDataSize: cardinal;
  out aOutInfo: pointer;
  out aOutInfoSize: cardinal): boolean;
var
 aNal: TNalUnitType;
begin
  result:=GetNextFrame(aOutFormat,aOutData,aOutDataSize,aOutInfo,aOutInfoSize,aNal);
end;

function TStreamFramerH264.GetUpToNextNAL: boolean;
var
  i: byte;
  aMarker: cardinal;
  aPtr: PCardinal;
begin
  FBuffer.Position:=0;

  aMarker:=NAL_START_MARKER;
  FBuffer.Write(aMarker,4);

  result:=false;

  while true do
  begin
    if FReader.Read(i,1)<>1 then
      break;

    FBuffer.Write(i,1);

    if FBuffer.Position>=8 then
    begin
      aPtr:=PCardinal(PByte(FBuffer.Memory)+(FBuffer.Position-4));
      if aPtr^=NAL_START_MARKER then
      begin
        FBuffer.Position:=FBuffer.Position-4;
        result:=true;
        break;
      end;
    end;
  end;
end;

procedure TStreamFramerH264.OpenStream(aStream: TStream);
begin
  FreeAndNil(FReader);
  FStream:=aStream;
  FReader:=TBufferedStream.Create(aStream);
  FReader.BufferSize:=65*1024;
  FProcessedFrameCount:=0;
  GetUpToNextNAL;
end;

function TStreamFramerH264.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerH264.VideoStreamType: TStreamType;
begin
  result:=stH264;
end;

function TStreamFramerH264.AudioStreamType: TStreamType;
begin
  result:=0;
end;

end.


