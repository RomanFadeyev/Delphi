unit MediaStream.Framer.AvcAny;

interface
  uses Windows,SysUtils,Classes, BufferedStream, MediaProcessing.Definitions,
  MediaStream.Framer,Avc, Avc.avcodec, Avc.avformat, Avc.samplefmt;

type
  TStreamFramerAvcAny = class (TStreamFramer)
  private
    FStream: TStream;
    FAvcMediaStream: AVC.IMediaStream;
    FAvcMediaStreamContext: Avc.TMediaStreamContext;

    FProcessedFrameCount: int64;
    FProcessedVideoFrameCount : int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; overload; override;


    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function StreamInfo: TBytes; override;
  end;

implementation

{ TStreamFramerAvcAny }

constructor TStreamFramerAvcAny.Create;
begin
  inherited;
end;

destructor TStreamFramerAvcAny.Destroy;
begin
  inherited;
  FStream:=nil;
  FAvcMediaStream:=nil;
end;

function TStreamFramerAvcAny.GetNextFrame(out aOutFormat: TMediaStreamDataHeader;
  out aOutData: pointer; out aOutDataSize: cardinal;
  out aOutInfo: pointer;
  out aOutInfoSize: cardinal): boolean;
var
  aAvPacket: PAVPacket;
  aStream: PAVStream;
begin
  aOutFormat.Clear;
  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;

  result:=true;
  while result do
  begin
    AvcCheck(FAvcMediaStream.ReadFrame(aAvPacket));
    result:=(aAvPacket<>nil) and (aAvPacket.stream_index>=0) and (aAvPacket.stream_index<FAvcMediaStreamContext.StreamCount);
    if not result then
      exit;

    aStream:=FAvcMediaStreamContext.Streams[aAvPacket.stream_index];
    aOutFormat.biStreamType:=aStream.codec.codec_tag;
    if (AV_PKT_FLAG_KEY and aAvPacket.flags)<>0 then
      Include(aOutFormat.biFrameFlags,ffKeyFrame);

    aOutFormat.TimeKoeff:=1;

    aOutData:=aAvPacket.data;
    aOutDataSize:=aAvPacket.size;
    aOutInfo:=aStream.codec.extradata;
    aOutInfoSize:=aStream.codec.extradata_size;

    if aAvPacket.stream_index=FAvcMediaStreamContext.PreferredVideoStreamIndex then
    begin
      aOutFormat.biMediaType:=mtVideo;
      aOutFormat.VideoWidth:=aStream.codec.coded_width;
      aOutFormat.VideoHeight:=aStream.codec.coded_height;
      aOutFormat.VideoBitCount:=24; //???
      if (aStream.r_frame_rate.num<>0) and (aStream.r_frame_rate.den<>0) then
      begin
        aOutFormat.TimeStamp:=Trunc(FProcessedVideoFrameCount*1000*aStream.r_frame_rate.den/aStream.r_frame_rate.num);
      end
      else begin
        aOutFormat.TimeStamp:=aAvPacket.pts* aStream.time_base.num div aStream.time_base.den;
      end;
      inc(FProcessedVideoFrameCount);
      break;
    end
    else if aAvPacket.stream_index=FAvcMediaStreamContext.PreferredAudioStreamIndex then
    begin
      aOutFormat.biMediaType:=mtAudio;

      aOutFormat.AudioChannels:=aStream.codec.channels;
      aOutFormat.AudioSamplesPerSec:=aStream.codec.sample_rate;

      if aStream.codec.sample_fmt in [AV_SAMPLE_FMT_U8,AV_SAMPLE_FMT_U8P] then
        aOutFormat.AudioBitsPerSample:=8
      else if aStream.codec.sample_fmt in [AV_SAMPLE_FMT_S16,AV_SAMPLE_FMT_S16P] then
        aOutFormat.AudioBitsPerSample:=16
      else if aStream.codec.sample_fmt in [AV_SAMPLE_FMT_S32,AV_SAMPLE_FMT_S32P,AV_SAMPLE_FMT_FLT,AV_SAMPLE_FMT_FLTP] then
        aOutFormat.AudioBitsPerSample:=32
      else if aStream.codec.sample_fmt in [AV_SAMPLE_FMT_DBL,AV_SAMPLE_FMT_DBLP] then
        aOutFormat.AudioBitsPerSample:=64
      else
        aOutFormat.AudioBitsPerSample:=8; //???

      break;
    end;
  end;
end;

procedure TStreamFramerAvcAny.OpenStream(aStream: TStream);

begin
  FStream:=aStream;
  AvcCheck(CreateMediaStream(FAvcMediaStream));

  if aStream is TFileStream then
  begin
    AvcCheck(FAvcMediaStream.OpenFile(PAnsiChar(AnsiString(TFileStream(aStream).FileName)),FAvcMediaStreamContext));
  end
  else begin
    Assert(false);
  end;

  FProcessedFrameCount:=0;
end;

function TStreamFramerAvcAny.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerAvcAny.VideoStreamType: TStreamType;
begin
  result:=stUNIV;
end;

function TStreamFramerAvcAny.AudioStreamType: TStreamType;
begin
  result:=stUNIV;
end;

end.


