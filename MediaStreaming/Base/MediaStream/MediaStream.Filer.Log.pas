unit MediaStream.Filer.Log;

interface
  uses SysUtils,Classes, Windows, SyncObjs, MediaStream.Filer, MediaProcessing.Definitions;

type
  TStreamFilerLog = class (TStreamFiler)
  private
    FLastTimeStamps: array [TMediaType] of int64;
    FLastTickCounts: array [TMediaType] of int64;
  protected
    procedure DoWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Open(const aFileName: string); override;

    class function DefaultExt: string; override;
  end;

implementation

{ TStreamFilerLog }

constructor TStreamFilerLog.Create;
begin
  inherited;
end;

class function TStreamFilerLog.DefaultExt: string;
begin
  result:='.log';
end;

destructor TStreamFilerLog.Destroy;
begin
  inherited;
end;

procedure TStreamFilerLog.DoWriteData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
var
  s: AnsiString;
  x: cardinal;
  aTickCountStr: string;
begin
  inherited;

  x:=GetTickCount;
  aTickCountStr:=IntToStr(x);
  if (FLastTickCounts[aFormat.biMediaType]<>0) and (x>=FLastTickCounts[aFormat.biMediaType]) then
    aTickCountStr:=aTickCountStr+Format('(+%d)',[x-FLastTickCounts[aFormat.biMediaType]]);
  FLastTickCounts[aFormat.biMediaType]:=x;

  s:=Format('%s;%s;%s;%s;%s;%d;%d;%d;%d;%d'#13#10,[
    aTickCountStr,
    MediaTypeNames[aFormat.biMediaType],
    GetStreamTypeName(aFormat.biStreamType),
    GetStreamTypeName(aFormat.biStreamSubType),
    FrameFlagsToString(aFormat.biFrameFlags),
    aFormat.Channel,
    aFormat.TimeStamp,
    aFormat.TimeKoeff,
    aDataSize,
    aInfoSize
    ]);

  FStream.Write(PAnsiChar(s)^,Length(s));
end;

procedure TStreamFilerLog.Open(const aFileName: string);
var
  s: AnsiString;
begin
  inherited;
  s:='LocalTime;MediaType;StreamType;StreamSubType;FrameFlags;Channel;Timestamp;TimeKoeff;DataSize;InfoSize'#13#10;
  FStream.Write(PAnsiChar(s)^,Length(s));
  ZeroMemory(@FLastTimeStamps,sizeof(FLastTimeStamps));
  ZeroMemory(@FLastTickCounts,sizeof(FLastTickCounts));
end;

end.
