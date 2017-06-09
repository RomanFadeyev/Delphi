unit MediaStream.Filer.MP6;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaStream.Filer, MediaProcessing.Definitions,HHRecorder,HHCommon;

type
  TStreamFilerMp6 = class (TStreamFiler)
  private
    FWriter: THHRecorderMpeg6File;
  protected
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Close; override;

    class function DefaultExt: string; override;
  end;

implementation

{ TStreamFilerMp6 }

procedure TStreamFilerMp6.Close;
begin
  FWriter.Close;
  inherited;
end;

constructor TStreamFilerMp6.Create;
begin
  inherited;
  FWriter:=THHRecorderMpeg6File.Create;
end;

class function TStreamFilerMp6.DefaultExt: string;
begin
  result:='.mp6';
end;

destructor TStreamFilerMp6.Destroy;
begin
  inherited;
  FreeAndNil(FWriter);
end;

procedure TStreamFilerMp6.DoWriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  Assert(aInfo<>nil);
  Assert(aInfoSize=sizeof(HHAV_INFO));
  if FWriter.Stream=nil then
    FWriter.AssignToStream(FStream,PHHAV_INFO(aInfo)^);

  FWriter.WriteFrame(aData);
end;

end.
