unit MediaStream.Filer.Tsm;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaStream.Filer, MediaProcessing.Definitions,MediaStream.Writer.Tsm;

type
  TStreamFilerTsm = class (TStreamFiler)
  private
    FWriter: TMediaStreamWriter_Tsm;
  protected
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Close; override;

    class function DefaultExt: string; override;
  end;

implementation

{ TStreamFilerTsm }

procedure TStreamFilerTsm.Close;
begin
  FWriter.Close;
  inherited;
end;

constructor TStreamFilerTsm.Create;
begin
  inherited;
  FWriter:=TMediaStreamWriter_Tsm.Create;
end;

class function TStreamFilerTsm.DefaultExt: string;
begin
  result:='.tsm';
end;

destructor TStreamFilerTsm.Destroy;
begin
  inherited;
  FreeAndNil(FWriter);
end;

procedure TStreamFilerTsm.DoWriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if FWriter.Stream=nil then
    FWriter.AssignToStream(FStream);

  FWriter.WriteData(aFormat,aData,aDataSize);
end;

end.
