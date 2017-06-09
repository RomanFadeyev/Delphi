{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Фреймер, обеспечивающий чтение фреймов из файла AVI           }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        14.01.2011                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaStream.Framer.AVI;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer,VFW,MMSystem,Generics.Collections;

type
  TStreamFramerAVI_File = class (TStreamFramer)
  private
    FAviFile: IAVIFile;
    FVideoStream: IAVIStream;
    FVideoCurrentFrame: integer;
    FVideoFirstFrame: integer;
    FVideoFrameCount: integer;

    FVideoStreamInfo: TAVIStreamInfo;
    FVideoFormat: TBitmapInfoHeader;
    FVideoFrameDuration: double;

    FAudioStream: IAVIStream;
    FAudioCurrentFrame: integer;
    FAudioFirstFrame: integer;
    FAudioFrameCount: integer;
    FAudioStreamInfo: TAVIStreamInfo;
    FAudioFormat: TPCMWaveFormat;

    FBuffer : TBytes;
    FDataBufferPtr:PByte;

    FFormatBuffer: TBytes;

    procedure Close;
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
  end;


type
  tid  = array [0..1] of AnsiChar;

  TStreamFormatVideo = TBitmapInfoHeader;
  TStreamFormatAudio = TPCMWaveFormat;

  TIndexItem = packed record
    MediaType:TMediaType;
    Number: DWORD;
    KeyFrame: boolean;
    StreamPosition: DWORD;
    //DataSize: DWORD;
    //DataFlags : DWORD;
  end;

  TAviStream = class
    FId:tid;
    FStreamHeader:TAVIStreamHeader;
    FStreamFormatVideo: TStreamFormatVideo;
    FStreamFormatAudio: TStreamFormatAudio;
    FStreamFormatAudioEx: TBytes;
  end;

  TStreamFramerAviRandomAccess = class;

  TStreamFramerAVI = class (TStreamFramer)
  private
    FStream : TStream;
    FRandomAccess: TStreamFramerAviRandomAccess;

    FAviHeader: TMainAVIHeader;
    FAviStreams : TObjectList<TAviStream>;

    FMainVideoStreamId : tid;
    FMainAudioStreamId : tid;

    FMainVideoStream : TAviStream;
    FMainAudioStream : TAviStream;
    FVideoFrameDuration: double;

    FMoviChunkPosition: int64;
    FMoviChunkSize: cardinal;

    FCurrentFrameBuffer: TBytes;
    FCurrentFrameIndex: integer;
    FCurrentFrameOutInfo: TBytes;

    FFrameIndexTableExists: boolean;
    FFrameIndexTable : array of TIndexItem;
    FFrameIndexTableSize: integer;
    FFrameIndexTableLastVideoFrameNumber: cardinal;

    procedure ProcessChunks(aBlockSize: longint);
    procedure Process_STRF(aBlockSize: longint);
    procedure Process_IDX1(aBlockSize: longint);

    procedure OnDataChunkFound(aFCC: FOURCC; var aMediaType: FOURCC);
    procedure Close;
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

    function RandomAccess: TStreamFramerRandomAccess; override;

    property AviHeader: TMainAVIHeader read FAviHeader;
    property VideoFrameDuration:double read FVideoFrameDuration;


    //длина в мс
    function TotalLength: int64;

    property CurrentFrameIndex: integer read FCurrentFrameIndex;
 end;

  TStreamFramerAviRandomAccess = class (TStreamFramerRandomAccess)
  private
    FOwner: TStreamFramerAvi;
    FStreamInfo: TStreamInfo;
  protected
    function GetPosition: int64; override;
    procedure SetPosition(const Value: int64); override;
  public
    function StreamInfo: TStreamInfo; override;
    function SeekToPrevVideoKeyFrame: boolean; override;

    constructor Create(aOwner: TStreamFramerAvi);
  end;

  EInvalidFormat = class (Exception)
  public
    constructor Create;
  end;


implementation
  uses HHCommon;

type
  tfourcc = array[0..3] of AnsiChar;
  TChunk = record
    fourcc: Tfourcc;
    Size:   longint;
  end;

function cmp(const f1, f2: tfourcc): boolean; inline;
begin
  result := PDWORD(@f1)^=PDWORD(@f2)^;
end;

function cmp2(const f1: FOURCC; f2: tfourcc): boolean; inline;
begin
  result := f1 =PDWORD(@f2)^;
end;

{ EInvalidFormat }

constructor EInvalidFormat.Create;
begin
  inherited Create('Неверный формат');
end;

{ TStreamFramerAVI_File }

procedure TStreamFramerAVI_File.Close;
begin
  FVideoStream:=nil;
  FAudioStream:=nil;
  FAviFile:=nil;
end;

constructor TStreamFramerAVI_File.Create;
begin
  inherited;
end;

destructor TStreamFramerAVI_File.Destroy;
begin
  Close;
  inherited;
end;

function TStreamFramerAVI_File.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
const
  PreBufferSize=255;
var
  aBytes,aSamples: integer;
  pFormat: PBitmapInfo;

  aPayload: byte;
  aPtr: PByte;
  aSize: integer;
begin
  result:=false;
  if FVideoCurrentFrame>=FVideoFrameCount then
    exit;

  if FVideoStream.Read(FVideoCurrentFrame,1,nil,0,aBytes,aSamples)<>0 then
    exit;

  if aBytes+PreBufferSize>Length(FBuffer) then
    SetLength(FBuffer, Round(aBytes*1.5)+PreBufferSize);

  FDataBufferPtr:=@FBuffer[PreBufferSize];

  if FVideoStream.Read(FVideoCurrentFrame,1,FDataBufferPtr,Length(FBuffer)-PreBufferSize,aBytes,aSamples)<>0 then
    exit;

  if FVideoStream.ReadFormat(FVideoCurrentFrame,nil,aSamples)<>0 then
    exit;

  if aSamples>Length(FFormatBuffer) then
    SetLength(FFormatBuffer,aSamples);

  if FVideoStream.ReadFormat(FVideoCurrentFrame,FFormatBuffer,aSamples)<>0 then
    exit;

  inc(FVideoCurrentFrame);

  aOutData:=FDataBufferPtr;
  aOutDataSize:=aBytes;

  aOutInfo:=nil;
  aOutInfoSize:=0;


  pFormat:=pointer(FFormatBuffer);
  aOutFormat.Assign(pFormat.bmiHeader);
  aOutFormat.TimeStamp:=Trunc((FVideoCurrentFrame-1)*FVideoFrameDuration);
  aOutFormat.TimeKoeff:=1;
  //aOutFormat.DataSize:=aBytes;
  Assert(aOutFormat.biMediaType=mtVideo);
  Include(aOutFormat.biFrameFlags,ffKeyFrame);

  if pFormat.bmiHeader.biCompression = stH264 then
  begin
    aPtr:=FDataBufferPtr;
    aSize:=aBytes;
    while (aPtr^=0) and (aSize>0) do
    begin
      inc(aPtr);
      dec(aSize);
    end;

    if (aPtr^=1) and (aSize>1) then
    begin
      inc(aPtr);
      aPayload:=aPtr^;
      aPayload:=aPayload and $1F;

      if aPayload in [5,7] then
        Include(aOutFormat.biFrameFlags,ffKeyFrame)
      else
        Exclude(aOutFormat.biFrameFlags,ffKeyFrame);
    end;
  end;
  result:=true;
end;

procedure TStreamFramerAVI_File.OpenStream(aStream: TStream);
var
  x: integer;
begin
  Close;
  Assert(aStream is TFileStream);

  AVIFileInit;
  Win32Check(AVIFileOpen(FAviFile,PChar((aStream as  TFileStream).FileName),OF_READ,nil)=0);
  if FAviFile.GetStream(FVideoStream,streamtypeVIDEO,0)<>AVIERR_OK then
    FVideoStream:=nil;

  if FAviFile.GetStream(FAudioStream,streamtypeAUDIO,0)<>AVIERR_OK then
    FAudioStream:=nil;


  if FVideoStream<>nil then
  begin
    FVideoFirstFrame:=AVIStreamStart(FVideoStream);
    FVideoCurrentFrame:=FVideoFirstFrame;
    FVideoFrameCount:=AVIStreamLength(FVideoStream);

    if FVideoStream.Info(FVideoStreamInfo,sizeof(FVideoStreamInfo))<>0 then
      RaiseLastOSError;

    if (FVideoCurrentFrame=-1) or (FVideoFirstFrame=-1) then
      raise Exception.Create('Ошибка чтения файла: не удалось определить длину файла');


    if (FVideoStreamInfo.dwScale=0) or (FVideoStreamInfo.dwRate=0) then
      FVideoFrameDuration:=40
    else
      FVideoFrameDuration:=1000*FVideoStreamInfo.dwScale/FVideoStreamInfo.dwRate;


    FVideoFormat.biSize:=sizeof(FVideoFormat);
    x:=sizeof(FVideoFormat);
    if FVideoStream.ReadFormat(0,@FVideoFormat,x)<>0 then
       RaiseLastOSError;
  end;

  if FAudioStream<>nil then
  begin
    if FAudioStream.Info(FAudioStreamInfo,sizeof(FAudioStreamInfo))<>0 then
      RaiseLastOSError;

    FAudioFirstFrame:=AVIStreamStart(FAudioStream);
    FAudioCurrentFrame:=FAudioFirstFrame;
    FAudioFrameCount:=AVIStreamLength(FAudioStream);

    if (FAudioCurrentFrame=-1) or (FAudioFirstFrame=-1) then
      raise Exception.Create('Ошибка чтения файла: не удалось определить длину файла');

    //FAudioFormat.cbSize:=sizeof(FAudioFormat);
    x:=sizeof(FAudioFormat);
    if FAudioStream.ReadFormat(0,@FAudioFormat,x)<>0 then
       ;//RaiseLastOSError;
  end;
end;

function TStreamFramerAVI_File.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerAVI_File.VideoInfo: TVideoInfo;
begin
  result.Width:=FVideoFormat.biWidth;
  result.Height:=FVideoFormat.biHeight;
  if result.Width*result.Height>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerAVI_File.VideoStreamType: TStreamType;
var
  aFormat: TMediaStreamDataHeader;
begin
  if FVideoStream=nil then
    exit(0);

  aFormat.Assign(FVideoFormat);

  result:=aFormat.biStreamType;
end;

function TStreamFramerAVI_File.AudioInfo: TAudioInfo;
begin
  result.Channels:=FAudioFormat.wf.nChannels;
  result.BitsPerSample:=FAudioFormat.wBitsPerSample;
  result.SamplesPerSec:=FAudioFormat.wf.nSamplesPerSec;

  if result.Channels>0 then
    result.State:=isOK
  else
    result.State:=isNotFound;
end;

function TStreamFramerAVI_File.AudioStreamType: TStreamType;
var
  aFormat: TMediaStreamDataHeader;
begin
  if FAudioStream=nil then
    exit(0);

  aFormat.Assign(FAudioFormat);
  result:=aFormat.biStreamType;
end;

//==============================================================================
procedure TStreamFramerAVI.Close;
begin
  FStream:=nil;

  FAviStreams.Clear;
  FCurrentFrameIndex := 0;
  FFrameIndexTableSize:=0;
  FFrameIndexTableExists:=false;
  FMoviChunkPosition:=0;
  FMoviChunkSize:=0;
  FMainVideoStreamId[0]:=#0;
  FMainAudioStreamId[0]:=#0;
  FMainVideoStream :=nil;
  FMainAudioStream :=nil;
end;

constructor TStreamFramerAVI.Create;
begin
  inherited;
  FAviStreams:=TObjectList<TAviStream>.Create;
end;

destructor TStreamFramerAVI.Destroy;
begin
  Close;
  inherited;

  FreeAndNil(FAviStreams);
  FFrameIndexTable:=nil;
end;

function TStreamFramerAVI.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerAVI.TotalLength: int64;
begin
  result:=FAviHeader.dwMicroSecPerFrame*FAviHeader.dwTotalFrames div 1000;
end;

function TStreamFramerAVI.VideoInfo: TVideoInfo;
begin
  if FMainVideoStream=nil then
  begin
    result.State:=isNotFound;
  end
  else begin
    result.State:=isOK;
    result.Width:=FMainVideoStream.FStreamFormatVideo.biWidth;
    result.Height:=FMainVideoStream.FStreamFormatVideo.biHeight;
  end;
end;

function TStreamFramerAVI.VideoStreamType: TStreamType;
var
  aFormat: TMediaStreamDataHeader;
begin
  if FMainVideoStream=nil then
    exit(0);

  aFormat.Assign(FMainVideoStream.FStreamFormatVideo);
  result:=aFormat.biStreamType;
end;

procedure TStreamFramerAVI.Process_IDX1(aBlockSize: longint);
var
  idx1entry:  TAVIINDEXENTRY;
  addofs: cardinal;
  aMediaType: FOURCC;
begin
  FFrameIndexTableExists:=true; //помечаем, что файл индексированный

  addofs   := cardinal(-1);
  while (aBlockSize > 0) do
  begin
    if aBlockSize mod sizeof(idx1entry) <>0 then
      raise EInvalidFormat.Create;

    FStream.ReadBuffer(idx1entry, sizeof(idx1entry));
    if (addofs = cardinal(-1)) then
      if (idx1entry.dwChunkOffset <= 4) then
        addofs := FMoviChunkPosition
      else
        addofs := 0;

    OnDataChunkFound(idx1entry.ckid,aMediaType);

    if (aMediaType=streamtypeVIDEO) or (aMediaType=streamtypeAUDIO) then
    begin
      if FFrameIndexTableSize+1>Length(FFrameIndexTable) then
        SetLength(FFrameIndexTable,(Length(FFrameIndexTable)+1)*2);

      if aMediaType=streamtypeAUDIO then
      begin
        FFrameIndexTable[FFrameIndexTableSize].MediaType:=mtAudio;
        //FFrameIndexTable[FFrameIndexTableSize].Number:=FFrameIndexTableLastVideoFrameNumber;
      end
      else begin
        FFrameIndexTable[FFrameIndexTableSize].MediaType:=mtVideo;
        FFrameIndexTable[FFrameIndexTableSize].Number:=FFrameIndexTableLastVideoFrameNumber;
        inc(FFrameIndexTableLastVideoFrameNumber);
      end;

      FFrameIndexTable[FFrameIndexTableSize].StreamPosition := idx1entry.dwChunkOffset+ addofs;
      FFrameIndexTable[FFrameIndexTableSize].KeyFrame:=(idx1entry.dwFlags and AVIIF_KEYFRAME)<>0;
      //FFrameIndexTable[FFrameIndexTableSize].DataSize:=idx1entry.dwChunkLength;
      //FFrameIndexTable[FFrameIndexTableSize].DataFlags:=idx1entry.dwFlags;
      inc(FFrameIndexTableSize);
    end;

    Dec(aBlockSize, sizeof(idx1entry));
  end;
end;


procedure TStreamFramerAVI.ProcessChunks(aBlockSize: longint);
var
  chunk: tchunk;
  list:  tfourcc;
  aAviStream: TAviStream;

  aStartPosition,aEndPosition: int64;
  s: AnsiString;
begin
  while aBlockSize>0 do
  begin
    Assert(aBlockSize>=8);

    FStream.ReadBuffer(chunk, 8);
    chunk.Size := (chunk.Size + 1) and $FFFFFFFE;

    aStartPosition:=FStream.Position;
    if cmp(Chunk.fourcc, 'LIST') then
    begin
      FStream.ReadBuffer(list, 4);
      //

      if cmp(list, 'hdrl') then
      begin
      end

      //
      else if cmp(list, 'strl') then
      begin
        aAviStream:=TAviStream.Create;
        s:=IntToStr(FAviStreams.Count);
        if Length(s)=1 then
        begin
          aAviStream.FId[0]:='0';
          aAviStream.FId[1]:=s[1];
        end
        else begin
          aAviStream.FId[0]:=s[1];
          aAviStream.FId[1]:=s[2];
        end;

        FAviStreams.Add(aAviStream);
      end

      //
      else if cmp(list, 'movi') then
      begin
        FMoviChunkPosition := FStream.Position - 4;
        if (chunk.Size<4) then
          raise EInvalidFormat.Create;

        FMoviChunkSize:=chunk.Size-4;
      end;

      ProcessChunks(chunk.Size - 4);
    end
(*
    else if cmp(Chunk.fourcc, '00db') or cmp(Chunk.fourcc, '00dc') or
      cmp(Chunk.fourcc, '00id') or cmp(Chunk.fourcc, '00iv') then
    begin
      //ЧТо это????
      //FFrameIndexTable[FCurrentFrameIndex].StreamPosition := FStream.Position - 8;
      //Inc(FCurrentFrameIndex);
      FStream.Seek(chunk.Size,soFromCurrent);
    end

    else if cmp(Chunk.fourcc, '01wb') then
    begin
      FStream.Seek(chunk.Size,soFromCurrent)
    end
*)
    else if cmp(Chunk.fourcc, 'avih') then
    begin
      if (chunk.Size>sizeof(FAviHeader)) then
      begin
        FStream.ReadBuffer(FAviHeader, sizeof(FAviHeader));
        FStream.Seek(chunk.Size-sizeof(FAviHeader),soFromCurrent);
      end
      else begin
        FStream.ReadBuffer(FAviHeader, chunk.Size);
      end;
    end

    //
    else if cmp(Chunk.fourcc, 'strh') then
    begin
      if FAviStreams.Count=0 then
        raise EInvalidFormat.Create;

      aAviStream:=FAviStreams[FAviStreams.Count-1];
      if chunk.Size>sizeof(aAviStream.FStreamHeader) then
      begin
        FStream.ReadBuffer(aAviStream.FStreamHeader,sizeof(aAviStream.FStreamHeader));
        FStream.Seek(chunk.Size-sizeof(aAviStream.FStreamHeader),soFromCurrent);
      end
      else begin
        FStream.ReadBuffer(aAviStream.FStreamHeader,chunk.Size);
      end;
    end

    //
    else if cmp(Chunk.fourcc, 'strf') then
    begin
      Process_STRF(chunk.Size);
    end


    else if cmp(Chunk.fourcc, 'idx1') then
    begin
      Process_IDX1(chunk.Size)
    end

    else if cmp(Chunk.fourcc, 'JUNK') then
    begin
      FStream.Seek(chunk.Size,soFromCurrent)
    end

    else begin
      FStream.Seek(chunk.Size,soFromCurrent);
    end;

    aEndPosition:=FStream.Position;
    if (aEndPosition-aStartPosition)<>chunk.Size then
      raise EInvalidFormat.Create;

    Dec(aBlockSize, chunk.Size + 8);
  end; //of while
end;

procedure TStreamFramerAVI.Process_STRF(aBlockSize: Integer);
var
  aAviStream: TAviStream;
begin
  if FAviStreams.Count=0 then
    raise EInvalidFormat.Create;

  aAviStream:=FAviStreams[FAviStreams.Count-1];

  if cmp2(aAviStream.FStreamHeader.fccType, 'vids') then
  begin
    if aBlockSize<sizeof(TBitmapInfoHeader) then
      raise EInvalidFormat.Create;


    if aBlockSize<sizeof(aAviStream.FStreamFormatVideo) then
      FStream.ReadBuffer(aAviStream.FStreamFormatVideo,aBlockSize)
    else begin
      FStream.ReadBuffer(aAviStream.FStreamFormatVideo,sizeof(aAviStream.FStreamFormatVideo));
      FStream.Seek(aBlockSize-sizeof(aAviStream.FStreamFormatVideo),soFromCurrent);
    end;
  end
  else if cmp2(aAviStream.FStreamHeader.fccType, 'auds') then
  begin
    if aBlockSize<sizeof(aAviStream.FStreamFormatAudio) then
      raise EInvalidFormat.Create;

    SetLength(aAviStream.FStreamFormatAudioEx,aBlockSize);
    FStream.ReadBuffer(aAviStream.FStreamFormatAudioEx[0],aBlockSize);
    CopyMemory(@aAviStream.FStreamFormatAudio,@aAviStream.FStreamFormatAudioEx[0],sizeof(aAviStream.FStreamFormatAudio));
  end
  else
  begin
    FStream.Seek(aBlockSize,soFromCurrent);
  end;
end;

function TStreamFramerAVI.RandomAccess: TStreamFramerRandomAccess;
begin
  result:=FRandomAccess;
  if result<>nil then
    exit;

  if FFrameIndexTableExists then
    FRandomAccess:=TStreamFramerAviRandomAccess.Create(self);

  result:=FRandomAccess;
end;

function TStreamFramerAVI.AudioInfo: TAudioInfo;
begin
  if FMainAudioStream=nil then
  begin
    result.State:=isNotFound;
  end
  else begin
    result.State:=isOK;
    result.Channels:=FMainAudioStream.FStreamFormatAudio.wf.nChannels;
    result.BitsPerSample:=FMainAudioStream.FStreamFormatAudio.wBitsPerSample;
    result.SamplesPerSec:=FMainAudioStream.FStreamFormatAudio.wf.nSamplesPerSec;
  end;
end;

function TStreamFramerAVI.AudioStreamType: TStreamType;
var
  aFormat: TMediaStreamDataHeader;
begin
  if FMainAudioStream=nil then
    exit(0);


  aFormat.Assign(FMainAudioStream.FStreamFormatAudio);
  result:=aFormat.biStreamType;
end;

procedure TStreamFramerAVI.OnDataChunkFound(aFCC: FOURCC;var aMediaType: FOURCC);
var
  a,v: boolean;
  fcc: tfourcc;
begin
  a:=false;
  v:=false;
  PDWORD(@fcc)^:=aFCC;

(*
##db is Uncompressed video frame
##dc is Compressed video frame
##pc is Palette change
##wb is Audio data
##tx is Subtitle data
*)

  if ((fcc[2]='w') and (fcc[3]='b')) {audio} then
  begin
    if FMainAudioStreamId[0]=#0 then
    begin
      FMainAudioStreamId[0]:=fcc[0];
      FMainAudioStreamId[1]:=fcc[1];
    end;
    a:=(FMainAudioStreamId[0]=fcc[0]) and (FMainAudioStreamId[1]=fcc[1]);
  end;

  if ((fcc[2]='d') and (fcc[3]='c')) {video compressed}
  or ((fcc[2]='d') and (fcc[3]='b')) {video uncompressed} then
  begin
    if FMainVideoStreamId[0]=#0 then
    begin
      FMainVideoStreamId[0]:=fcc[0];
      FMainVideoStreamId[1]:=fcc[1];
    end;
    v:=(FMainVideoStreamId[0]=fcc[0]) and (FMainVideoStreamId[1]=fcc[1]);
  end;

  if v then
    aMediaType:=streamtypeVIDEO
  else if a then
    aMediaType:=streamtypeAUDIO
  else
    aMediaType:=0;
end;

procedure TStreamFramerAVI.OpenStream(aStream: TStream);
var
  chunk: tfourcc;
  Size:  longint;
  i: Integer;
begin
  Close;
  FStream:=aStream;

  FStream.ReadBuffer(Chunk, 4);
  if not cmp(Chunk, 'RIFF') then
    raise EInvalidFormat.Create;

  FStream.ReadBuffer(Size, 4);
  FStream.ReadBuffer(Chunk, 4);
  if not cmp(Chunk, 'AVI ') then
    raise EInvalidFormat.Create;

  ProcessChunks(Size - 4);

  for i := 0 to FAviStreams.Count-1 do
  begin
    if cmp2(FAviStreams[i].FStreamHeader.fccType,'vids') and (FAviStreams[i].FId=FMainVideoStreamId) then
      FMainVideoStream:=FAviStreams[i]
    else if cmp2(FAviStreams[i].FStreamHeader.fccType,'auds') and (FAviStreams[i].FId=FMainAudioStreamId) then
      FMainAudioStream:=FAviStreams[i];
  end;

  if (FMainVideoStream=nil) or (FMainVideoStream.FStreamHeader.dwScale=0) or (FMainVideoStream.FStreamHeader.dwRate=0) then
    FVideoFrameDuration:=40
  else
    FVideoFrameDuration:=1000*FMainVideoStream.FStreamHeader.dwScale/FMainVideoStream.FStreamHeader.dwRate;

  //Ставим курсор на начало секции MOVI
  FStream.Seek(FMoviChunkPosition+4,soFromBeginning);
end;

function TStreamFramerAVI.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean;
var
  chunk:   tchunk;
  aPtr: PByte;
  aSize: integer;
  aPayload:byte;
  aMediaStreamType: FOURCC;
  aIndexItem : TIndexItem;
label
  start;
begin
  result:=false;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutData:=nil;
  aOutDataSize:=0;

  if FMoviChunkSize=0 then //В файле нет секции MOVI
    exit;

start:

  if FFrameIndexTableExists then
  begin
    if (FCurrentFrameIndex >= FFrameIndexTableSize) then
      exit;

    aIndexItem:=FFrameIndexTable[FCurrentFrameIndex];
  end
  else begin
    if FStream.Position>FMoviChunkPosition+FMoviChunkSize then //Секция MOVI кончилась
      exit;

    if FStream.Read(chunk,sizeof(chunk))<>sizeof(chunk) then
      exit;

    OnDataChunkFound(PDWORD(@chunk.fourcc)^,aMediaStreamType);
    if (aMediaStreamType=streamtypeVIDEO) or (aMediaStreamType=streamtypeAUDIO) then
    begin
      FStream.Seek(-sizeof(chunk),soFromCurrent);
      aIndexItem.StreamPosition:=FStream.Position;
      if aMediaStreamType=streamtypeVIDEO then
        aIndexItem.MediaType:=mtVideo
      else
        aIndexItem.MediaType:=mtAudio;

      aIndexItem.KeyFrame:=true; //TODO как определить опорный кадр?
    end
    else begin
      FStream.Seek(chunk.Size,soFromCurrent);
      goto start;
    end;
  end;

  //Так как мы читаем фрейм за фреймом, корректировка позции маркера в большинстве случаев не требуется
  if FStream.Position<>aIndexItem.StreamPosition then
    FStream.Seek(aIndexItem.StreamPosition,soFromBeginning);

  FStream.ReadBuffer(chunk, sizeof(chunk));
  Inc(FCurrentFrameIndex);
  result:=true;

  chunk.Size := (chunk.Size + 1) and $FFFFFFFE;
  if (chunk.Size<=0) then
    exit;

  if Length(FCurrentFrameBuffer)<chunk.Size then
  begin
    FCurrentFrameBuffer:=nil;
    SetLength(FCurrentFrameBuffer, chunk.Size);
  end;
  FStream.ReadBuffer(FCurrentFrameBuffer[0], chunk.Size);
  aOutData:=FCurrentFrameBuffer;
  aOutDataSize:=chunk.Size;

  //Video --
  if (aIndexItem.MediaType=mtVideo) then
  begin
    aOutFormat.Assign(FMainVideoStream.FStreamFormatVideo);

    if FAviHeader.dwTotalFrames=0 then
      aOutFormat.TimeStamp:=0
    else
      aOutFormat.TimeStamp:=Trunc(TotalLength*aIndexItem.Number/FAviHeader.dwTotalFrames);

    aOutFormat.TimeKoeff:=1;
    //inc(FVideoTimeStamp,trunc(FVideoFrameDuration));

    Assert(aOutFormat.biMediaType=mtVideo);
    if aIndexItem.KeyFrame then
      Include(aOutFormat.biFrameFlags,ffKeyFrame)
    else
      Exclude(aOutFormat.biFrameFlags,ffKeyFrame);

    //Для формата H264 проведем анализ и выставим флаги собственноручно, так надежнее
    if aOutFormat.biStreamType = stH264 then
    begin
      aPtr:=aOutData;
      aSize:=aOutDataSize;
      while (aPtr^=0) and (aSize>0) do
      begin
        inc(aPtr);
        dec(aSize);
      end;

      if (aPtr^=1) and (aSize>1) then
      begin
        inc(aPtr);
        aPayload:=aPtr^;
        aPayload:=aPayload and $1F;

        if aPayload in [5,7] then
        begin
          if not (ffKeyFrame in aOutFormat.biFrameFlags) then
            Include(aOutFormat.biFrameFlags,ffKeyFrame)
        end
        else begin
          if (ffKeyFrame in aOutFormat.biFrameFlags) then
            Exclude(aOutFormat.biFrameFlags,ffKeyFrame);
        end;
      end;
    end
  end
  //Audio --
  else if (aIndexItem.MediaType=mtAudio) then
  begin
    aOutFormat.Assign(FMainAudioStream.FStreamFormatAudio);
    if aIndexItem.KeyFrame then
      Include(aOutFormat.biFrameFlags,ffKeyFrame);

    if FMainAudioStream.FStreamFormatAudio.wf.wFormatTag=85 then  //MP3
    begin
      aOutInfoSize:=Length(FMainAudioStream.FStreamFormatAudioEx);
      aOutInfo:=@FMainAudioStream.FStreamFormatAudioEx[0];
    end;
    //aOutFormat.TimeStamp:=FVideoTimeStamp; inc(FVideoTimeStamp,40);//TODO FMainVideoStream.FStreamHeader.
  end
  else
    Assert(false);


  //Временнная мера, чтобы сохранить совместимость с существующим кодом
  if result then
  begin
    if (aOutFormat.biStreamType=stHHVI)  then
    begin
      SetLength(FCurrentFrameOutInfo,sizeof(HHAV_INFO));
      aOutInfo:=@FCurrentFrameOutInfo[0];
      aOutInfoSize:=sizeof(HHAV_INFO);

      PHHAV_INFO(aOutInfo).nVideoHeight:=aOutFormat.VideoHeight;
      PHHAV_INFO(aOutInfo).nVideoWidth:=aOutFormat.VideoWidth;

      if (aOutFormat.VideoWidth=704) and (aOutFormat.VideoHeight=576)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_PAL_D1 // H264_2,PAL D1	704 * 576
      else if (aOutFormat.VideoWidth=704) and (aOutFormat.VideoHeight=288)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_PAL_HD1 // H264_2,PAL lfD1	704 * 288
      else if (aOutFormat.VideoWidth=352) and (aOutFormat.VideoHeight=288)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_PAL_CIF     // H264_2,PAL CIF	352 * 288
      else if (aOutFormat.VideoWidth=640) and (aOutFormat.VideoHeight=480)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_VGA         // H264_2,VGA		640 * 480
      else if (aOutFormat.VideoWidth=640) and (aOutFormat.VideoHeight=240)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_HVGA        // H264_2,HVGA		640 * 240
      else if (aOutFormat.VideoWidth=320) and (aOutFormat.VideoHeight=240)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_CVGA        // H264_2,CVGA		320 * 240
      else if (aOutFormat.VideoWidth=176) and (aOutFormat.VideoHeight=144)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_PAL_QCIF    // H264_2,PAL QCIF	176 * 144
      else if (aOutFormat.VideoWidth=160) and (aOutFormat.VideoHeight=120)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_QVGA        // H264_2,QVGA		160 * 120
      else if (aOutFormat.VideoWidth=704) and (aOutFormat.VideoHeight=480)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_NTSC_D1     // H264_2,D1		704 * 480
      else if (aOutFormat.VideoWidth=704) and (aOutFormat.VideoHeight=240)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_NTSC_HD1    // H264_2,HalfD1	704 * 240
      else if (aOutFormat.VideoWidth=352) and (aOutFormat.VideoHeight=240)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_NTSC_CIF    // H264_2,CIF		352 * 240
      else if (aOutFormat.VideoWidth=176) and (aOutFormat.VideoHeight=120)  then
        PHHAV_INFO(aOutInfo).nVideoEncodeType:=EV_H264_NTSC_QCIF;   // H264_2,QCIF		176 * 120


      Assert(PHV_FRAME_HEAD(aOutData).zeroFlag=0);
      Assert(PHV_FRAME_HEAD(aOutData).oneFlag=1);
      Assert(PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD)<=aOutDataSize);
      if PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD)<aOutDataSize then
        aOutDataSize:=PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD);
    end
    else if (aOutFormat.biStreamType=stHHAU)  then
    begin
      SetLength(FCurrentFrameOutInfo,sizeof(HHAV_INFO));
      aOutInfo:=@FCurrentFrameOutInfo[0];
      aOutInfoSize:=sizeof(HHAV_INFO);

      PHHAV_INFO(aOutInfo).nAudioChannels:=aOutFormat.AudioChannels;
      PHHAV_INFO(aOutInfo).nAudioBits:=aOutFormat.AudioBitsPerSample;
      PHHAV_INFO(aOutInfo).nAudioSamples:=aOutFormat.AudioSamplesPerSec;

      Assert(PHV_FRAME_HEAD(aOutData).zeroFlag=0);
      Assert(PHV_FRAME_HEAD(aOutData).oneFlag=1);
      Assert(PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD)<=aOutDataSize);
      if PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD)<aOutDataSize then
        aOutDataSize:=PHV_FRAME_HEAD(aOutData).nByteNum+sizeof(HV_FRAME_HEAD);


    end;
  end;
end;


{ TStreamFramerAviRandomAccess }

constructor TStreamFramerAviRandomAccess.Create(aOwner: TStreamFramerAvi);
begin
  FOwner:=aOwner;

  FStreamInfo.VideoFrameCount:=0;
  if FOwner.FMainVideoStream<>nil then
    FStreamInfo.VideoFrameCount:=FOwner.FMainVideoStream.FStreamHeader.dwLength;

  FStreamInfo.Length:=FOwner.TotalLength;
end;

function TStreamFramerAviRandomAccess.GetPosition: int64;
begin
  result:=Trunc(FStreamInfo.Length*FOwner.CurrentFrameIndex/FOwner.FAviHeader.dwTotalFrames);
end;

procedure TStreamFramerAviRandomAccess.SetPosition(const Value: int64);
var
  k: double;
  j,i: Integer;
begin
  if Value<=0 then
     FOwner.FCurrentFrameIndex:=0
  else begin
    k:=Value/FStreamInfo.Length;
    FOwner.FCurrentFrameIndex:=Trunc(FOwner.FAviHeader.dwTotalFrames*k);

    j :=FOwner.FCurrentFrameIndex;
    while j>=0 do
    begin
      if FOwner.FFrameIndexTable[j].MediaType=mtVideo then
        if FOwner.FFrameIndexTable[j].KeyFrame then
          break;
      dec(j);
    end;

    i:=FOwner.FCurrentFrameIndex+1;
    while i<FOwner.FFrameIndexTableSize do
    begin
      if FOwner.FFrameIndexTable[i].MediaType=mtVideo then
        if FOwner.FFrameIndexTable[i].KeyFrame then
          break;
      inc(i);
    end;

    if j=-1 then
    begin
     if i<FOwner.FFrameIndexTableSize then
       FOwner.FCurrentFrameIndex:=i;
    end
    else if i>=FOwner.FFrameIndexTableSize then
    begin
      if (j>=0) then
       FOwner.FCurrentFrameIndex:=j;
    end
    else begin
      if Abs(FOwner.FCurrentFrameIndex-j)<Abs(FOwner.FCurrentFrameIndex-i) then
        FOwner.FCurrentFrameIndex:=j
      else
        FOwner.FCurrentFrameIndex:=i;
    end;

  end;
end;

function TStreamFramerAviRandomAccess.StreamInfo: TStreamInfo;
begin
  result:=FStreamInfo;
end;

function TStreamFramerAviRandomAccess.SeekToPrevVideoKeyFrame: boolean;
var
  j: integer;
begin
  result:=false;

  if FOwner.FCurrentFrameIndex>=FOwner.FFrameIndexTableSize then
    FOwner.FCurrentFrameIndex:=FOwner.FFrameIndexTableSize-1;


  //Сначала находим начало текущей цепочки фреймов
  for j:=FOwner.FCurrentFrameIndex downto 0 do
  begin
    if FOwner.FFrameIndexTable[j].MediaType=mtVideo then
      if FOwner.FFrameIndexTable[j].KeyFrame then
      begin
        FOwner.FCurrentFrameIndex:=j;
        break;
      end;
  end;

  //Потом ищем предыдущий
  for j:=FOwner.FCurrentFrameIndex-1 downto 0 do
  begin
    if FOwner.FFrameIndexTable[j].MediaType=mtVideo then
      if FOwner.FFrameIndexTable[j].KeyFrame then
      begin
        FOwner.FCurrentFrameIndex:=j;
        result:=true;
        break;
      end;
  end;
end;


end.



