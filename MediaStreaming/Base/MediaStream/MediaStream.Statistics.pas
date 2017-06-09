unit MediaStream.Statistics;

interface
  uses SysUtils,Classes,SyncObjs,MediaStream.DataSource.Base,Generics.Collections,Player.VideoOutput.Base,MediaProcessing.Definitions;

type
  TBytesInfo = record
    Value:cardinal;
    DateTime: TDateTime;
  end;

  TFrameInfo = record
    DateTime: TDateTime;
  end;

  TMediaStreamStatistics = class
  private
    FBPSLock : TCriticalSection;
    FBPS: TQueue<TBytesInfo>;
    FFPSLock : TCriticalSection;
    FFPS: TQueue<TFrameInfo>;
    FBytesPerSecondTestIntervalSecs: cardinal;
    FFramesPerSecondTestIntervalSecs: cardinal;

    FAttrLock: TCriticalSection;
    FTotalBytes: int64;
    FTotalFrames: int64;

    FMaxFrameSize : int64;
    FMinFrameSize : int64;

    FFirstDateTime: TDateTime;
    FLastDateTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    property BytesPerSecondTestIntervalSecs:cardinal read FBytesPerSecondTestIntervalSecs write FBytesPerSecondTestIntervalSecs default 3;
    property FramesPerSecondTestIntervalSecs:cardinal read FFramesPerSecondTestIntervalSecs write FFramesPerSecondTestIntervalSecs default 3;

    procedure AddData(aDataSize: cardinal; aInfoSize: cardinal);

    function GetBytesPerSecond: cardinal;
    function GetFramesPerSecondFloat: double;
    function GetFramesPerSecond: cardinal;

    function GetTotalBytes: int64; inline;
    function GetTotalFrames: int64; inline;

    function GetAverageFrameSize: int64; inline;

    function GetMaxFrameSize: int64; inline;
    function GetMinFrameSize: int64; inline;

    function GetLastDateTime: TDateTime; inline;
    function GetFirstDateTime: TDateTime; inline;

    procedure Clear;
  end;

implementation
  uses Math;

{ TMediaStreamStatistics }

procedure TMediaStreamStatistics.AddData(aDataSize: cardinal; aInfoSize: cardinal);
var
  aNow: TDateTime;
  aBytesInfo: TBytesInfo;
  aFrameInfo: TFrameInfo;
  aFrameSize: integer;
begin
  aNow:=Now;
  FBPSLock.Enter;
  try
    aBytesInfo.Value:=aDataSize+aInfoSize;
    aBytesInfo.DateTime:=aNow;
    FBPS.Enqueue(aBytesInfo);
    while (FBPS.Count>1) and ((aNow-FBPS.Peek.DateTime)*SecsPerDay>FBytesPerSecondTestIntervalSecs) do
      FBPS.Dequeue;
  finally
    FBPSLock.Leave;
  end;

  FFPSLock.Enter;
  try
    aFrameInfo.DateTime:=aNow;
    FFPS.Enqueue(aFrameInfo);
    while (FFPS.Count>1) and ((aNow-FFPS.Peek.DateTime)*SecsPerDay>FFramesPerSecondTestIntervalSecs) do
      FFPS.Dequeue;
  finally
    FFPSLock.Leave;
  end;

  FAttrLock.Enter;
  try
    FLastDateTime:=Now;
    if FFirstDateTime=0 then
      FFirstDateTime:=FLastDateTime;
    aFrameSize:=aDataSize+aInfoSize;
    if FTotalFrames=0 then
    begin
      FMaxFrameSize:=aFrameSize;
      FMinFrameSize:=aFrameSize;
    end
    else begin
      FMaxFrameSize:=Max(FMaxFrameSize,aFrameSize);
      FMinFrameSize:=Min(FMinFrameSize,aFrameSize);
    end;

    inc(FTotalBytes,aFrameSize);
    inc(FTotalFrames);

  finally
    FAttrLock.Leave;
  end;
end;

procedure TMediaStreamStatistics.Clear;
begin
  FBPSLock.Enter;
  try
    FBPS.Clear;
  finally
    FBPSLock.Leave;
  end;

  FFPSLock.Enter;
  try
    FFPS.Clear;
  finally
    FFPSLock.Leave;
  end;

  FAttrLock.Enter;
  try
    FLastDateTime:=0;
    FMaxFrameSize:=0;
    FMinFrameSize:=0;

    FTotalBytes:=0;
    FTotalFrames:=0;
  finally
    FAttrLock.Leave;
  end;
end;

constructor TMediaStreamStatistics.Create;
begin
  FBPSLock:=TCriticalSection.Create;
  FBPS:=TQueue<TBytesInfo>.Create;
  FFPSLock:=TCriticalSection.Create;
  FFPS:=TQueue<TFrameInfo>.Create;
  FAttrLock:=TCriticalSection.Create;

  FBytesPerSecondTestIntervalSecs:=3;
  FFramesPerSecondTestIntervalSecs:=3;
end;

destructor TMediaStreamStatistics.Destroy;
begin
  inherited;
  FreeAndNil(FBPSLock);
  FreeAndNil(FBPS);
  FreeAndNil(FFPSLock);
  FreeAndNil(FFPS);
  FreeAndNil(FAttrLock);
end;

function TMediaStreamStatistics.GetAverageFrameSize: int64;
begin
  FAttrLock.Enter;
  try
    if FTotalFrames>0 then
      result:=FTotalBytes div FTotalFrames
    else
      result:=0;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetBytesPerSecond: cardinal;
var
  aEnd,aStart: TDateTime;
  aBPSEnumerator: TQueue<TBytesInfo>.TEnumerator;
begin
  result:=0;
  aStart:=0;
  aEnd:=0;

  FBPSLock.Enter;
  try
    if FBPS.Count>1 then
    begin
      aStart:=FBPS.Peek.DateTime;
      aEnd:=aStart;
      aBPSEnumerator:=FBPS.GetEnumerator;
      try
        while aBPSEnumerator.MoveNext do
        begin
          aEnd:=aBPSEnumerator.Current.DateTime;
          result:=result+aBPSEnumerator.Current.Value;
        end;
      finally
        aBPSEnumerator.Free;
      end;
    end;
  finally
    FBPSLock.Leave;
  end;

  if (aStart<>0) and (aEnd>aStart) then
    result:=Round(result/ ((aEnd-aStart)*SecsPerDay))
  else
    result:=0;
end;

function TMediaStreamStatistics.GetFirstDateTime: TDateTime;
begin
  FAttrLock.Enter;
  try
    result:=FFirstDateTime;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetFramesPerSecond: cardinal;
begin
  result:=Round(GetFramesPerSecondFloat);
end;

function TMediaStreamStatistics.GetFramesPerSecondFloat: double;
var
  aEnd,aStart: TDateTime;
  aFPSEnumerator: TQueue<TFrameInfo>.TEnumerator;
begin
  result:=0;
  aStart:=0;
  aEnd:=0;

  FFPSLock.Enter;
  try
    if FFPS.Count>1 then
    begin
      aStart:=FFPS.Peek.DateTime;
      aEnd:=aStart;
      aFPSEnumerator:=FFPS.GetEnumerator;
      try
        while aFPSEnumerator.MoveNext do
        begin
          aEnd:=aFPSEnumerator.Current.DateTime;
          result:=result+1;
        end;
      finally
        aFPSEnumerator.Free;
      end;
    end;
  finally
    FFPSLock.Leave;
  end;

  if (aStart<>0) and (aEnd>aStart) then
    result:=result/ ((aEnd-aStart)*SecsPerDay)
  else
    result:=0;
end;

function TMediaStreamStatistics.GetLastDateTime: TDateTime;
begin
  FAttrLock.Enter;
  try
    result:=FLastDateTime;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetMaxFrameSize: int64;
begin
  FAttrLock.Enter;
  try
    result:=FMaxFrameSize;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetMinFrameSize: int64;
begin
  FAttrLock.Enter;
  try
    result:=FMinFrameSize;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetTotalBytes: int64;
begin
  FAttrLock.Enter;
  try
    result:=FTotalBytes;
  finally
    FAttrLock.Leave;
  end;
end;

function TMediaStreamStatistics.GetTotalFrames: int64;
begin
  FAttrLock.Enter;
  try
    result:=FTotalFrames;
  finally
    FAttrLock.Leave;
  end;
end;

end.
