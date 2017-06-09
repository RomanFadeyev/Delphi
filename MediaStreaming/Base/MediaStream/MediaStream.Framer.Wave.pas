unit MediaStream.Framer.Wave;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer,MMSystem;

type
  TStreamFramerWave = class (TStreamFramer)
  private
    FStream : TStream;
    FBuffer: TBytes;
    FFormat: TPCMWaveFormat;
    FDataSize: DWORD;
    FTimeStamp: int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; override;

    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function StreamInfo: TBytes; override;

    property Format: TPCMWaveFormat read FFormat;
  end;

implementation

type
  TWaveHeader = packed record
    //Contains the letters "RIFF" in ASCII form (0x52494646 big-endian form).
    IdRiff: array[0..3] of AnsiChar;

    //This is the size of the rest of the chunk
    //following this number.  This is the size of the
    //entire file in bytes minus 8 bytes for the
    //two fields not included in this count:
    //IdRiff and RiffLenth.
    RiffLenth: longint;

    //Contains the letters "WAVE"
    //0x57415645 big-endian form).
    IdWave: array[0..3] of AnsiChar;
  end;

  TChunkHeader = packed record
    Id: array[0..3] of AnsiChar;
    Size: DWORD;
  end;

  //Chunks ==========

  //id = "fmt "
  //size 16;
  TFormatChunk = TPCMWaveFormat;

  TDataChunk = packed record
  //Data
  end;

{ TStreamFramerWave }

constructor TStreamFramerWave.Create;
begin
  inherited;
end;

destructor TStreamFramerWave.Destroy;
begin
  FStream:=nil;
  inherited;
end;

function TStreamFramerWave.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
var
  aPacketSize: cardinal;

const
  aFrameLengthMsec = 40;
begin
  result:=false;

  if FDataSize=0 then
    exit;

  aPacketSize:=FFormat.wf.nSamplesPerSec*(FFormat.wBitsPerSample div 8)*FFormat.wf.nChannels;
  //Читаем по 1-й секунде
  //if aFrameLengthMsec>1000 then
  //  aPacketSize:=aPacketSize * (aFrameLengthMsec div 1000)
  //else
    aPacketSize:=aPacketSize * aFrameLengthMsec div 1000;

  if aPacketSize>FDataSize then
    aPacketSize:=FDataSize;

  dec(FDataSize,aPacketSize);
  if cardinal(Length(FBuffer))<aPacketSize then
    SetLength(FBuffer,aPacketSize);

  result:=cardinal(FStream.Read(FBuffer[0],aPacketSize))=aPacketSize;
  if result then
  begin
    aOutData:=FBuffer;
    aOutDataSize:=aPacketSize;
    aOutInfo:=nil;
    aOutInfoSize:=0;
    aOutFormat.Assign(FFormat);
    aOutFormat.TimeStamp:=FTimeStamp;
    inc(FTimeStamp,aFrameLengthMsec);
  end;
end;

procedure TStreamFramerWave.OpenStream(aStream: TStream);
const
  MsgBadFileFormat ='Неверный формат файла';
var
  aWaveHeader: TWaveHeader;
  aChunkHeader : TChunkHeader;
begin
  FStream:=aStream;
  FTimeStamp:=0;

  FStream.ReadBuffer(aWaveHeader,sizeof(aWaveHeader));
  if aWaveHeader.IdRiff<>'RIFF' then
    raise Exception.Create(MsgBadFileFormat);


  if aWaveHeader.IdWave<>'WAVE' then
    raise Exception.Create(MsgBadFileFormat);


  //format chunk
  FStream.ReadBuffer(aChunkHeader,sizeof(aChunkHeader));
  if aChunkHeader.Id<>'fmt ' then
    raise Exception.Create(MsgBadFileFormat);

  if aChunkHeader.Size<sizeof(FFormat) then
    raise Exception.Create(MsgBadFileFormat);

  FStream.ReadBuffer(FFormat,sizeof(FFormat));

  //Какие-то доп. данные, которых мы не знаем
  if aChunkHeader.Size>sizeof(FFormat) then
    FStream.Seek(aChunkHeader.Size-sizeof(FFormat),soFromCurrent);

  //0 (0x0000) 	Unknown
  //1 (0x0001) 	PCM/uncompressed
  //2 (0x0002) 	Microsoft ADPCM
  //6 (0x0006) 	ITU G.711 a-law
  //7 (0x0007) 	ITU G.711 Aч-law
  //17 (0x0011) 	IMA ADPCM
  //20 (0x0016) 	ITU G.723 ADPCM (Yamaha)
  //49 (0x0031) 	GSM 6.10
  //64 (0x0040) 	ITU G.721 ADPCM
  //80 (0x0050) 	MPEG
  //65,536 (0xFFFF) 	Experimental
  if not FFormat.wf.wFormatTag in [WAVE_FORMAT_PCM,7] then
    raise Exception.Create('Неподдерживаемый тип компрессии аудио');


  //lookup for fact
  FStream.ReadBuffer(aChunkHeader,sizeof(aChunkHeader));
  if aChunkHeader.Id='chunk' then
  begin
    FStream.Seek(aChunkHeader.Size,soFromCurrent);
  end
  else if aChunkHeader.Id='data' then
    FDataSize:=aChunkHeader.Size
  else
    raise Exception.Create(MsgBadFileFormat);


  if FFormat.wBitsPerSample mod 8<>0 then
    raise Exception.Create(MsgBadFileFormat);
end;

function TStreamFramerWave.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerWave.VideoStreamType: TStreamType;
begin
  result:=0;
end;

function TStreamFramerWave.AudioStreamType: TStreamType;
var
  aFormat: TMediaStreamDataHeader;
begin
  Assert(FStream<>nil);
  aFormat.Assign(FFormat);
  result:=aFormat.biStreamType;
end;

end.


