unit MediaStream.Framer.Mp6;

interface
  uses Windows,SysUtils,Classes,HHCommon,HHReader, MediaProcessing.Definitions,MediaStream.Framer;

type
  TStreamFramerMp6RandomAccess = class;

  TStreamFramerMp6 = class (TStreamFramer)
  private
    FReader: THHReaderMpeg6;
    FRandomAccess: TStreamFramerMp6RandomAccess;
    FFirstVideoFrameTimeStamp: int64;
    FFirstAudioFrameTimeStamp: int64;

    FVideoFramesRead: int64;
    FAudioFramesRead: int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; override;

    function VideoStreamType: TStreamType; override;
    function VideoInfo: TVideoInfo; override;

    function AudioStreamType: TStreamType; override;
    function AudioInfo: TAudioInfo; override;

    function StreamInfo: TBytes; override;

    property Reader: THHReaderMpeg6 read FReader;

    function RandomAccess: TStreamFramerRandomAccess; override;
  end;

  TStreamFramerMp6RandomAccess = class (TStreamFramerRandomAccess)
  private
    FOwner: TStreamFramerMp6;
    FStreamInfo: TStreamInfo;
    FFirstVideoTimeStamp,  FLastVideoTimeStamp: int64;
  protected
    function GetPosition: int64; override;
    procedure SetPosition(const Value: int64); override;
  public
    function StreamInfo: TStreamInfo; override;
    function SeekToPrevVideoKeyFrame: boolean; override;

    constructor Create(aOwner: TStreamFramerMp6);
  end;

implementation

{ TStreamFramerMp6 }

constructor TStreamFramerMp6.Create;
begin
  inherited;
  FReader:=THHReaderMpeg6.Create;
end;

destructor TStreamFramerMp6.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FRandomAccess);
  inherited;
end;

function TStreamFramerMp6.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
var
  aH264Data: pointer;
  aH264DataSize: cardinal;
begin
  Assert(FReader<>nil);
  result:=FReader.ReadFrame;
  if result then
  begin
    aOutData:=FReader.CurrentFrame;
    aOutDataSize:=FReader.CurrentFrame.nByteNum+sizeof(HV_FRAME_HEAD);

    aOutInfo:=@FReader.AVInfo;
    aOutInfoSize:=sizeof(FReader.AVInfo);

    aOutFormat.Clear;
    //aOutFormat.DataSize:=aOutDataSize;


    if FReader.CurrentFrame.streamFlag = FRAME_FLAG_A then
    begin
      aOutFormat.biMediaType:=mtAudio;
      aOutFormat.biStreamType:= AudioStreamType;
      aOutFormat.biStreamSubType:=FReader.AVInfo.nAudioEncodeType;
      aOutFormat.AudioChannels:=FReader.AVInfo.nAudioChannels;
      aOutFormat.AudioBitsPerSample:=FReader.AVInfo.nAudioBits;
      aOutFormat.AudioSamplesPerSec:=FReader.AVInfo.nAudioSamples;
      if (FFirstAudioFrameTimeStamp=0) and (FAudioFramesRead=0) then
        FFirstAudioFrameTimeStamp:=FReader.CurrentFrame.nTimestamp;

      aOutFormat.TimeStamp:=FReader.CurrentFrame.nTimestamp-FFirstAudioFrameTimeStamp;
      inc(FAudioFramesRead);
    end
    else begin
      aOutFormat.biMediaType:=mtVideo;
      aOutFormat.biStreamType:= VideoStreamType;
      aOutFormat.biStreamSubType:=FReader.AVInfo.nVideoEncodeType;
      aOutFormat.VideoWidth:=FReader.AVInfo.nVideoWidth;
      aOutFormat.VideoHeight:=FReader.AVInfo.nVideoHeight;

      if (FFirstVideoFrameTimeStamp=0) and (FVideoFramesRead=0) then
        FFirstVideoFrameTimeStamp:=FReader.CurrentFrame.nTimestamp;

      //aOutFormat.biBitCount:=0;

      if FReader.CurrentFrame.streamFlag=FRAME_FLAG_VI then
        Include(aOutFormat.biFrameFlags,ffKeyFrame);

      aOutFormat.TimeStamp:=FReader.CurrentFrame.nTimestamp-FFirstVideoFrameTimeStamp;
      if GetH264Data(aOutData,aH264Data,aH264DataSize) then
      begin
        aOutData:=aH264Data;
        aOutDataSize:=aH264DataSize;
        aOutInfo:=nil;
        aOutInfoSize:=0;
        aOutFormat.biStreamType:=stH264;
      end;

      inc(FVideoFramesRead);
    end;

    aOutFormat.TimeKoeff:=40;

    //if aOutFormat.TimeStamp<0 then
    //  aOutFormat.TimeStamp:=0; //?? „то делать в случае нарушенных временных меток?
  end;
end;

procedure TStreamFramerMp6.OpenStream(aStream: TStream);
begin
  FreeAndNil(FRandomAccess);
  FReader.Open(aStream);
end;

function TStreamFramerMp6.RandomAccess: TStreamFramerRandomAccess;
begin
  result:=FRandomAccess;
  if result<>nil then
    exit;

  if not FReader.Opened then
    exit;

  FReader.ReadIndexTable;
  if FReader.IndexTableExists then
  begin
    try FReader.IndexTable.CheckTimeStampOrder; except end;
    FRandomAccess:=TStreamFramerMp6RandomAccess.Create(self);
  end;

  result:=FRandomAccess;
end;

function TStreamFramerMp6.StreamInfo: TBytes;
var
  aAVInfo: HHAV_INFO;
begin
  aAVInfo:=FReader.AVInfo;

  SetLength(result,sizeof(aAVInfo));
  CopyMemory(result,@aAVInfo,sizeof(aAVInfo));
end;

function TStreamFramerMp6.VideoInfo: TVideoInfo;
begin
  result.Width:=FReader.AVInfo.nVideoWidth;
  result.Height:=FReader.AVInfo.nVideoHeight;
  if result.Width*Result.Height>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerMp6.VideoStreamType: TStreamType;
begin
  result:=stHHVI;
end;

function TStreamFramerMp6.AudioInfo: TAudioInfo;
begin
  result.Channels:=FReader.AVInfo.nAudioChannels;
  result.BitsPerSample:=FReader.AVInfo.nAudioBits;
  result.SamplesPerSec:=FReader.AVInfo.nAudioSamples;

  if result.Channels>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerMp6.AudioStreamType: TStreamType;
begin
  result:=stHHAU;
end;

{ TStreamFramerMp6RandomAccess }

constructor TStreamFramerMp6RandomAccess.Create(aOwner: TStreamFramerMp6);
var
  aDelta: int64;
  i: integer;
begin
  FOwner:=aOwner;

  FStreamInfo.VideoFrameCount:=FOwner.FReader.IndexTable.GetVideoFrameCount(FFirstVideoTimeStamp,FLastVideoTimeStamp);
  aDelta:=FLastVideoTimeStamp-FFirstVideoTimeStamp;
  FStreamInfo.Length:=DeviceTimeStampToMs(aDelta);

  i:=FOwner.Reader.IndexTable.FindFirstVideoFrame;
  if i<>-1 then
    FOwner.FFirstVideoFrameTimeStamp:=FOwner.FReader.IndexTable[i].Timestamp;

  i:=FOwner.Reader.IndexTable.FindFirstAudioFrame;
  if i<>-1 then
    FOwner.FFirstAudioFrameTimeStamp:=FOwner.FReader.IndexTable[i].Timestamp;
end;

function TStreamFramerMp6RandomAccess.GetPosition: int64;
var
//  i: integer;
  aDelta: int64;
begin
  //TODO не учитываетс€ переполнение в счетчике
  if FOwner.FReader.LastReadVideoTimestamp<FFirstVideoTimeStamp  then
    result:=0
  else begin
    aDelta:= FOwner.FReader.LastReadVideoTimestamp-FFirstVideoTimeStamp;
    result:=DeviceTimeStampToMs(aDelta);
  end;

(*  i:=FOwner.FReader.IndexTable.FindNearestFrame(FOwner.FReader.Position);
  if i>=0 then
  begin
    while (i>=0) and (not (FOwner.FReader.IndexTable[i].StreamFlag in [FRAME_FLAG_VI,FRAME_FLAG_VP])) do
      dec(i);
  end;

  if i>=0 then
  begin



  end;
*)
end;

function TStreamFramerMp6RandomAccess.StreamInfo: TStreamInfo;
begin
  result:=FStreamInfo;
end;

function TStreamFramerMp6RandomAccess.SeekToPrevVideoKeyFrame: boolean;
begin
  result:=FOwner.FReader.SeekToPrevIVideoFrame;
end;

procedure TStreamFramerMp6RandomAccess.SetPosition(const Value: int64);
begin
  if Value<=0 then
     FOwner.FReader.ResetPos
  else begin
    if FOwner.FReader.SeekToVideoFrameGE(FFirstVideoTimeStamp+MsToDeviceTimeStamp(Value)) then
      FOwner.FReader.SeekToPrevIVideoFrame
    else
      FOwner.FReader.SeekToEnd;
  end;
end;

end.


