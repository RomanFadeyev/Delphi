unit MediaStream.Framer.Tsm;

interface
  uses Windows,SysUtils,Classes,TsmFile,MediaProcessing.Definitions,MediaStream.Framer;

type
  TStreamFramerTsmRandomAccess = class;

  TStreamFramerTsm = class (TStreamFramer)
  private
    FReader: TTsmFileReader;
    FRandomAccess: TStreamFramerTsmRandomAccess;
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

    property Reader: TTsmFileReader read FReader;

    function RandomAccess: TStreamFramerRandomAccess; override;
  end;

  TStreamFramerTsmRandomAccess = class (TStreamFramerRandomAccess)
  private
    FOwner: TStreamFramerTsm;
    FStreamInfo: TStreamInfo;
  protected
    function GetPosition: int64; override;
    procedure SetPosition(const Value: int64); override;
  public
    function StreamInfo: TStreamInfo; override;
    function SeekToPrevVideoKeyFrame: boolean; override;

    constructor Create(aOwner: TStreamFramerTsm);
  end;

implementation

{ TStreamFramerTsm }

constructor TStreamFramerTsm.Create;
begin
  inherited;
  FReader:=TTsmFileReader.Create;
end;

destructor TStreamFramerTsm.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FRandomAccess);
  inherited;
end;

function TStreamFramerTsm.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
begin
  Assert(FReader<>nil);
  result:=FReader.ReadFrame;
  if result then
  begin
    aOutData:=FReader.CurrentFrameData;
    aOutDataSize:=FReader.CurrentFrame.Header.DataSize;

    aOutInfo:=nil;
    aOutInfoSize:=0;

    aOutFormat.Clear;

    if FReader.CurrentFrame.Header.DataType = TsmDataTypeAudio then
    begin
      aOutFormat.biMediaType:=mtAudio;
      aOutFormat.biStreamType:= AudioStreamType;
      aOutFormat.biStreamSubType:=FReader.Header.AudioStreamSubType;
      aOutFormat.AudioChannels:=FReader.Header.AudioChannels;
      aOutFormat.AudioBitsPerSample:=FReader.Header.AudioBitsPerSample;
      aOutFormat.AudioSamplesPerSec:=FReader.Header.AudioSamplesPerSec;
      aOutFormat.TimeStamp:=FReader.CurrentFrame.Header.TimestampOffs;
      inc(FAudioFramesRead);
    end
    else if FReader.CurrentFrame.Header.DataType = TsmDataTypeVideo then
    begin
      aOutFormat.biMediaType:=mtVideo;
      aOutFormat.biStreamType:= VideoStreamType;
      aOutFormat.biStreamSubType:=FReader.Header.VideoStreamSubType;
      aOutFormat.VideoWidth:=FReader.Header.VideoWidth;
      aOutFormat.VideoHeight:=FReader.Header.VideoHeight;
      aOutFormat.VideoBitCount:=FReader.Header.VideoBitCount;

      //aOutFormat.biBitCount:=0;

      if FReader.CurrentFrame.Header.IsKeyFrame then
        Include(aOutFormat.biFrameFlags,ffKeyFrame);

      aOutFormat.TimeStamp:=FReader.CurrentFrame.Header.TimestampOffs;
      inc(FVideoFramesRead);
    end
    else if FReader.CurrentFrame.Header.DataType = TsmDataTypeSysData then
    begin
      aOutFormat.biMediaType:=mtSysData;
      aOutFormat.biStreamType:= FReader.CurrentFrame.Extension.StreamType;
      aOutFormat.biStreamSubType:=FReader.CurrentFrame.Extension.StreamSubType;
      aOutFormat.VideoWidth:=FReader.Header.VideoWidth;
      aOutFormat.VideoHeight:=FReader.Header.VideoHeight;
      aOutFormat.VideoBitCount:=FReader.Header.VideoBitCount;

      if FReader.CurrentFrame.Header.IsKeyFrame then
        Include(aOutFormat.biFrameFlags,ffKeyFrame);

      aOutFormat.TimeStamp:=(int64(FReader.CurrentFrame.Extension.TimestampHigh) shl 32) or FReader.CurrentFrame.Header.TimestampOffs;
      //inc(FVideoFramesRead);*)
    end;

    aOutFormat.TimeKoeff:=1;

    //if aOutFormat.TimeStamp<0 then
    //  aOutFormat.TimeStamp:=0; //?? „то делать в случае нарушенных временных меток?
  end;
end;

procedure TStreamFramerTsm.OpenStream(aStream: TStream);
begin
  FreeAndNil(FRandomAccess);
  FReader.Open(aStream);
end;

function TStreamFramerTsm.RandomAccess: TStreamFramerRandomAccess;
begin
  result:=FRandomAccess;
  if result<>nil then
    exit;

  if not FReader.Opened then
    exit;

  if not FReader.Header.Valid then
    exit;

  FReader.ReadIndexTable;
  if FReader.IndexTableExists then
  begin
    try
      FReader.IndexTable.CheckTimeStampOrder;
    except
      exit;
    end;

    FRandomAccess:=TStreamFramerTsmRandomAccess.Create(self);
  end;

  result:=FRandomAccess;
end;

function TStreamFramerTsm.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerTsm.VideoInfo: TVideoInfo;
begin
  result.Width:=FReader.Header.VideoWidth;
  result.Height:=FReader.Header.VideoHeight;
  if result.Width*Result.Height>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerTsm.VideoStreamType: TStreamType;
begin
  if (FReader=nil) then
    result:=0
  else
    result:=FReader.Header.VideoStreamType;
end;

function TStreamFramerTsm.AudioInfo: TAudioInfo;
begin
  result.Channels:=FReader.Header.AudioChannels;
  result.BitsPerSample:=FReader.Header.AudioBitsPerSample;
  result.SamplesPerSec:=FReader.Header.AudioSamplesPerSec;

  if result.Channels>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerTsm.AudioStreamType: TStreamType;
begin
  if (FReader=nil) then
    result:=0
  else
    result:=FReader.Header.AudioStreamType;
end;

{ TStreamFramerTsmRandomAccess }

constructor TStreamFramerTsmRandomAccess.Create(aOwner: TStreamFramerTsm);
begin
  FOwner:=aOwner;
  Assert(FOwner.Reader.Header.Valid);

  FStreamInfo.VideoFrameCount:=FOwner.FReader.Header.VideoFrameCount;
  FStreamInfo.Length:=FOwner.FReader.Header.VideoLastTimeStamp-FOwner.FReader.Header.VideoFirstTimeStamp;
end;

function TStreamFramerTsmRandomAccess.GetPosition: int64;
begin
  result:= FOwner.FReader.LastReadVideoTimestamp;
end;

function TStreamFramerTsmRandomAccess.StreamInfo: TStreamInfo;
begin
  result:=FStreamInfo;
end;

function TStreamFramerTsmRandomAccess.SeekToPrevVideoKeyFrame: boolean;
begin
  result:=FOwner.FReader.SeekToPrevIVideoFrame;
end;

procedure TStreamFramerTsmRandomAccess.SetPosition(const Value: int64);
begin
  if Value<=0 then
     FOwner.FReader.ResetPos
  else begin
    if FOwner.FReader.SeekToVideoFrameGE(Value) then
    begin
      if not FOwner.FReader.CurrentFrame.Header.IsKeyFrame then
        FOwner.FReader.SeekToPrevIVideoFrame
    end
    else
      FOwner.FReader.SeekToEnd;
  end;
end;

end.


