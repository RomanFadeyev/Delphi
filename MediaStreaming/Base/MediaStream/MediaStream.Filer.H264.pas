unit MediaStream.Filer.H264;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaStream.Filer, MediaProcessing.Definitions,H264Parser;

type
  TStreamFilerH264 = class (TStreamFiler)
  private
    FParser:  TH264Parser;
  protected
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function DefaultExt: string; override;
  end;

implementation

{ TStreamFilerH264 }

constructor TStreamFilerH264.Create;
begin
  inherited;
  FParser:=TH264Parser.Create;
end;

class function TStreamFilerH264.DefaultExt: string;
begin
  result:='.h264';
end;

destructor TStreamFilerH264.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

procedure TStreamFilerH264.DoWriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  inherited;
  Assert(PDWORD(aData)^=$1000000); //NAL_START_MARKER

  FParser.Parse(aData,aDataSize);
  if FParser.LastParams.PictureWidth<>0 then
    FStream.WriteBuffer(aData^,aDataSize);
end;

end.
