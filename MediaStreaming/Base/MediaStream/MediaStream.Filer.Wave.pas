unit MediaStream.Filer.Wave;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaStream.Filer, MediaProcessing.Definitions,MediaStream.Writer.Wave;

type
  TStreamFilerWave = class (TStreamFiler)
  private
    FWriter: TMediaStreamWriter_Wave;
  protected
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Close; override;

    class function DefaultExt: string; override;
  end;

implementation

{ TStreamFilerWave }

procedure TStreamFilerWave.Close;
begin
  FWriter.Close;
  inherited;
end;

constructor TStreamFilerWave.Create;
begin
  inherited;
  FWriter:=TMediaStreamWriter_Wave.Create;
end;

class function TStreamFilerWave.DefaultExt: string;
begin
  result:='.wav';
end;

destructor TStreamFilerWave.Destroy;
begin
  inherited;
  FreeAndNil(FWriter);
end;

procedure TStreamFilerWave.DoWriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if (aFormat.biMediaType=mtAudio) and (aFormat.biStreamType=stPCM) then
  begin
    if not FWriter.Opened then
      FWriter.Open(self.FStream,aFormat.AudioChannels,aFormat.AudioSamplesPerSec,aFormat.AudioBitsPerSample);

    FWriter.WritePcmData(aData,aDataSize);
  end;
end;

end.
