unit MediaStream.Filer;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaProcessing.Definitions;

type
  TStreamFiler = class
  protected
    FStream: TFileStream;
    FStreamLock: TCriticalSection;

    procedure CheckOpened;
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); virtual; abstract;
  public
    constructor Create; overload; virtual;
    constructor Create(const aFileName: string); overload;
    destructor Destroy; override;

    procedure Open(const aFileName: string); virtual;
    procedure Close; virtual;

    function Opened: boolean;
    function FileName: string;

    procedure WriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
    class function DefaultExt: string; virtual; abstract;
  end;

implementation

{ TStreamFiler }

procedure TStreamFiler.CheckOpened;
begin
  if not Opened then
    raise Exception.Create('Файл для записи не открыт');
end;

procedure TStreamFiler.Close;
begin

  FreeAndNil(FStream);
end;

constructor TStreamFiler.Create(const aFileName: string);
begin
  Create;
  Open(aFileName);
end;

destructor TStreamFiler.Destroy;
begin
  Close;
  FreeAndNil(FStreamLock);
  inherited;
end;

function TStreamFiler.FileName: string;
begin
  CheckOpened;
  result:=FStream.FileName;
end;

constructor TStreamFiler.Create;
begin
  FStreamLock:=TCriticalSection.Create;
end;

procedure TStreamFiler.Open(const aFileName: string);
begin
  Close;
  FStreamLock.Enter;
  try
    FStream:=TFileStream.Create(aFileName,fmCreate or fmShareDenyWrite);
  finally
    FStreamLock.Leave;
  end;
end;

function TStreamFiler.Opened: boolean;
begin
  result:=FStream<>nil;
end;

procedure TStreamFiler.WriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  FStreamLock.Enter;
  try
    CheckOpened;
    DoWriteData(aFormat,aData,aDataSize,aInfo,aInfoSize);
  finally
    FStreamLock.Leave;
  end;
end;

end.
