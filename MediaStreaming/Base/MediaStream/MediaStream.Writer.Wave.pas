unit MediaStream.Writer.Wave;

interface
  uses Windows,Classes,SysUtils,MMSystem,MediaProcessing.Definitions;

type
  //Запись в Wav-файл (только Audio-потока)
  TMediaStreamWriter_Wave = class
  private
    FStream: TStream;
    FFileName: string;
    FDataBytesWritten : integer;
    FFormat: TPCMWaveFormat;
    FOwnStream: boolean;


    procedure SetFileName(const Value: string);
    procedure WriteHeader;

  protected
    procedure OpenInternal;
    //Закрыть файл
    procedure CloseInternal;

    procedure CheckOpened;
  public
    //Открыть файл для записи
    procedure Open(const aFileName: string;
                  nChannels: Word;
                  nSamplesPerSec: DWORD;
                  wBitsPerSample: Word); overload;

    procedure Open(const aStream: TStream;
                  nChannels: Word;
                  nSamplesPerSec: DWORD;
                  wBitsPerSample: Word); overload;


    procedure Close;
    function  Opened: boolean;
    procedure WritePcmData(aData: pointer; aDataSize: cardinal);

    property  FileName: string read FFileName write SetFileName;

    function  DefaultExtension: string;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

type
(*
The canonical WAVE format starts with the RIFF header:

0         4   ChunkID          Contains the letters "RIFF" in ASCII form
                               (0x52494646 big-endian form).
4         4   ChunkSize        36 + SubChunk2Size, or more precisely:
                               4 + (8 + SubChunk1Size) + (8 + SubChunk2Size)
                               This is the size of the rest of the chunk
                               following this number.  This is the size of the
                               entire file in bytes minus 8 bytes for the
                               two fields not included in this count:
                               ChunkID and ChunkSize.
8         4   Format           Contains the letters "WAVE"
                               (0x57415645 big-endian form).

The "WAVE" format consists of two subchunks: "fmt " and "data":
The "fmt " subchunk describes the sound data's format:

12        4   Subchunk1ID      Contains the letters "fmt "
                               (0x666d7420 big-endian form).
16        4   Subchunk1Size    16 for PCM.  This is the size of the
                               rest of the Subchunk which follows this number.
20        2   AudioFormat      PCM = 1 (i.e. Linear quantization)
                               Values other than 1 indicate some
                               form of compression.
22        2   NumChannels      Mono = 1, Stereo = 2, etc.
24        4   SampleRate       8000, 44100, etc.
28        4   ByteRate         == SampleRate * NumChannels * BitsPerSample/8
32        2   BlockAlign       == NumChannels * BitsPerSample/8
                               The number of bytes for one sample including
                               all channels. I wonder what happens when
                               this number isn't an integer?
34        2   BitsPerSample    8 bits = 8, 16 bits = 16, etc.
          2   ExtraParamSize   if PCM, then doesn't exist
          X   ExtraParams      space for extra parameters

The "data" subchunk contains the size of the data and the actual sound:

36        4   Subchunk2ID      Contains the letters "data"
                               (0x64617461 big-endian form).
40        4   Subchunk2Size    == NumSamples * NumChannels * BitsPerSample/8
                               This is the number of bytes in the data.
                               You can also think of this as the size
                               of the read of the subchunk following this
                               number.
44        *   Data             The actual sound data.
*)
  TWaveHeader = record
     Marker1:        Array[0..3] of AnsiChar;  //Contains the letters "RIFF" in ASCII form
     BytesFollowing: LongInt;
     Marker2:        Array[0..3] of AnsiChar;  //Contains the letters "WAVE"
     Marker3:        Array[0..3] of AnsiChar;  //Contains the letters "fmt "
     Fixed1:         LongInt;

     //Скопировано из WAVEFORMATEX
     wFormatTag: Word;         { format type }
     nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
     nSamplesPerSec: DWORD;  { sample rate }
     nAvgBytesPerSec: DWORD; { for buffer estimation }
     nBlockAlign: Word;      { block size of data }
     wBitsPerSample: Word;   { number of bits per sample of mono data }

     Marker4:        Array[0..3] of AnsiChar;   // Contains the letters "data"
     DataBytes:      LongInt;
  end;
{ TMediaStreamWriter_Wave }

constructor TMediaStreamWriter_Wave.Create;
begin
  inherited Create;
end;

destructor TMediaStreamWriter_Wave.Destroy;
begin
  Close;
  inherited;
end;

procedure TMediaStreamWriter_Wave.CheckOpened;
begin
  if not Opened then
    raise Exception.Create('Записыватель еще не инициализирован');
end;

procedure TMediaStreamWriter_Wave.Close;
begin
  CloseInternal;
end;

procedure TMediaStreamWriter_Wave.CloseInternal;
begin
  if FStream<>nil then
  begin
    FStream.Seek(0,soFromBeginning);
    WriteHeader;
    if FOwnStream then
      FreeAndNil(FStream)
    else
      FStream:=nil;
    FFileName:='';
  end;
end;

procedure TMediaStreamWriter_Wave.Open(const aFileName: string;
                  nChannels: Word;
                  nSamplesPerSec: DWORD;
                  wBitsPerSample: Word);
begin
  Close();

  FFormat.wf.wFormatTag := WAVE_FORMAT_PCM;
  FFormat.wf.nChannels :=nChannels;
  FFormat.wBitsPerSample:=wBitsPerSample;
  FFormat.wf.nSamplesPerSec:=nSamplesPerSec;

  FFormat.wf.nBlockAlign:=(wBitsPerSample div 8) * nChannels;
  FFormat.wf.nAvgBytesPerSec:=FFormat.wf.nBlockAlign * nSamplesPerSec;

  FFileName:=aFileName;

  OpenInternal;
end;

procedure TMediaStreamWriter_Wave.Open(const aStream: TStream; nChannels: Word;
  nSamplesPerSec: DWORD; wBitsPerSample: Word);
begin
  Close();

  FFormat.wf.wFormatTag := WAVE_FORMAT_PCM;
  FFormat.wf.nChannels :=nChannels;
  FFormat.wBitsPerSample:=wBitsPerSample;
  FFormat.wf.nSamplesPerSec:=nSamplesPerSec;

  FFormat.wf.nBlockAlign:=(wBitsPerSample div 8) * nChannels;
  FFormat.wf.nAvgBytesPerSec:=FFormat.wf.nBlockAlign * nSamplesPerSec;

  FStream:=aStream;
  FOwnStream:=false;
  WriteHeader
end;

function TMediaStreamWriter_Wave.Opened: boolean;
begin
  result:=FStream<>nil;
end;

procedure TMediaStreamWriter_Wave.WritePcmData(aData: pointer; aDataSize: cardinal);
begin
  CheckOpened;
  FStream.WriteBuffer(aData^,aDataSize);
  inc(FDataBytesWritten,aDataSize);
end;

procedure TMediaStreamWriter_Wave.SetFileName(const Value: string);
begin
  if Opened then
    raise Exception.Create('Уже открыт');

  FFileName := Value;
end;

procedure TMediaStreamWriter_Wave.OpenInternal;
begin
  FDataBytesWritten:=0;

  FStream:=TFileStream.Create(FFileName,fmCreate or fmShareDenyWrite);
  //Заголовок
  WriteHeader;
end;

function TMediaStreamWriter_Wave.DefaultExtension: string;
begin
  result:='.wav'
end;

procedure TMediaStreamWriter_Wave.WriteHeader;
var
  WaveHeader: TWaveHeader;
begin
  WaveHeader.Marker1 := 'RIFF';
  WaveHeader.BytesFollowing := FDataBytesWritten + sizeof(TWaveHeader)-8;
  WaveHeader.Marker2 := 'WAVE';
  WaveHeader.Marker3 := 'fmt ';
  WaveHeader.Fixed1 := 16;


  WaveHeader.wFormatTag := WAVE_FORMAT_PCM;

  WaveHeader.nChannels :=self.FFormat.wf.nChannels;
  WaveHeader.wBitsPerSample:=self.FFormat.wBitsPerSample;
  WaveHeader.nSamplesPerSec:=self.FFormat.wf.nSamplesPerSec;

  WaveHeader.nBlockAlign:=self.FFormat.wf.nBlockAlign;
  WaveHeader.nAvgBytesPerSec:=Self.FFormat.wf.nAvgBytesPerSec;

  WaveHeader.Marker4 := 'data';
  WaveHeader.DataBytes := FDataBytesWritten;

  FStream.WriteBuffer(WaveHeader, sizeof(WaveHeader));
end;

end.
