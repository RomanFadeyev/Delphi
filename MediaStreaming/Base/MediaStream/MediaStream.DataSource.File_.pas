unit MediaStream.DataSource.File_;

interface
  uses Windows,Classes,SysUtils, SyncObjs, uBaseClasses, MediaStream.DataSource.Base,
  MediaProcessing.Definitions,MediaStream.Framer;

type
  TMediaStreamDataSource_File = class;

  TMediaStreamDataSourceFileEndedHandler = procedure (Sender: TMediaStreamDataSource) of object;
  TMediaStreamDataSourceFileEndedEvent = TEventCast<TMediaStreamDataSourceFileEndedHandler>;

  TMediaStreamDataSource_File = class (TMediaStreamDataSource)
  private
    FFileReadThread : TThread;
    FFileName: string;
    FLastStreamDataTime: TDateTime;
    FDataLock: TCriticalSection;
    FTransmitVideo: boolean;
    FTransmitAudio: boolean;
    FLoop: boolean;
    FNoTimings: boolean;
    FOnFileEnded: TMediaStreamDataSourceFileEndedEvent;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure OnDataReceived(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
    function  StreamFramer: TStreamFramer;

    //Если true, то задержки между кадрами для обеспечения реального режима вопроизведения не выполняются
    property  NoTimings:boolean read FNoTimings write FNoTimings;

    //Возвращает текущую позицию в миллисекундах
    function CurrentPosition: int64;

    //Длина в миллисекундах
    function Length: int64;

    property OnFileEnded: TMediaStreamDataSourceFileEndedEvent read FOnFileEnded;
  end;


  TMediaStreamDataSourceConnectParams_File = class (TMediaStreamDataSourceConnectParams)
  private
    FFileName: string;
    FTransmitVideo: boolean;
    FTransmitAudio: boolean;
    FLoop: boolean;
  public
    constructor Create; overload;
    constructor Create(
      const aUrl: string;
      aTransmitVideo: boolean=true;
      aTransmitAudio: boolean=true;
      aLoop: boolean=true); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;

    function ToString: string; override;

    property FileName: string read FFileName write FFileName;
    property Loop: boolean read FLoop write FLoop;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
 end;



implementation
  uses ThreadNames, StrUtils, MediaStream.FramerFactory,MediaStream.UrlFormats;

type
  TFileReader = class
  private
    FFramer: TStreamFramer;
    FStream : TStream;
    FClean: boolean;
  public
    constructor Create(const aFileName: string; aFramerClass:TStreamFramerClass);
    destructor Destroy; override;

    procedure Reset;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean;

    function StreamInfo: TBytes;
    function VideoStreamType: TStreamType;
    function AudioStreamType: TStreamType;

    function CurrentPosition: int64;
    //Длина в миллисекундах
    function Length: int64;
  end;

  TReadFileThread = class (TThread)
  private
    FOwner: TMediaStreamDataSource_File;
    FReader: TFileReader;
    FOpenLock: TCriticalSection;
    FVideoTimeStampBase: int64;
    FAudioTimeStampBase: int64;

    FAudioTotalSleep: int64;
    FAudioSleepHits: int64;

    FStreamInfo: TBytes;
  protected
    procedure ProcessFile;
    procedure Execute; override;
  public
    constructor Create(const aFileName:string; aFramerClass:TStreamFramerClass; aOwner: TMediaStreamDataSource_File);
    destructor Destroy; override;

    //Текущая позиция в миллисекундах
    function CurrentPosition: int64;
    //Длина в миллисекундах
    function Length: int64;
  end;

{ TMediaStreamDataSourceConnectParams_File }

procedure TMediaStreamDataSourceConnectParams_File.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc:TMediaStreamDataSourceConnectParams_File;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_File) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=aSource as TMediaStreamDataSourceConnectParams_File;

  FFileName:=aSrc.FFileName;
  FTransmitVideo:=aSrc.FTransmitVideo;
  FTransmitAudio:=aSrc.FTransmitAudio;
  FLoop:=aSrc.FLoop;
end;

constructor TMediaStreamDataSourceConnectParams_File.Create;
begin
  FLoop:=true;
  FTransmitVideo:=true;
  FTransmitAudio:=true;
end;

constructor TMediaStreamDataSourceConnectParams_File.Create(const aUrl: string; aTransmitVideo, aTransmitAudio,aLoop: boolean);
var
  s: string;
begin
  Create;

  FLoop:=aLoop;
  if StartsText('file:',aUrl) and ParseFileUrl(aUrl,s) then
    FFileName:=s
  else
    FFileName:=aUrl;

  FTransmitVideo:=aTransmitVideo;
  FTransmitAudio:=aTransmitAudio;
end;

procedure TMediaStreamDataSourceConnectParams_File.Parse(const aUrl: string);
begin
  if not ParseFileUrl(aUrl,FFileName) then
    RaiseParseError(aUrl);
end;

function TMediaStreamDataSourceConnectParams_File.ToString: string;
begin
  result:=FFileName;
end;

function TMediaStreamDataSourceConnectParams_File.ToUrl(
  aIncludeAuthorizationInfo: boolean): string;
begin
  result:=MakeFileUrl(FFileName)
end;

{ TMediaStreamDataSource_File }

procedure TMediaStreamDataSource_File.Start;
begin
  FDataLock.Enter;
  try
    FFileReadThread.Resume;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_File.Stop;
begin
  FDataLock.Enter;
  try
    if FFileReadThread<>nil then
    begin
      if GetCurrentThread=FFileReadThread.Handle then
        raise Exception.Create('Нельзя остановить поток из него же');
      FFileReadThread.Suspend;
    end;
  finally
    FDataLock.Leave;
  end;
end;

function TMediaStreamDataSource_File.StreamFramer: TStreamFramer;
begin
  result:=nil;

  if FFileReadThread<>nil then
    result:=TReadFileThread(FFileReadThread).FReader.FFramer;
end;

procedure TMediaStreamDataSource_File.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
var
  aConnectParams_ : TMediaStreamDataSourceConnectParams_File;
begin
  aConnectParams_:=aConnectParams as TMediaStreamDataSourceConnectParams_File;
  FFileName:=aConnectParams_.FFileName;
  FTransmitVideo:=aConnectParams_.FTransmitVideo;
  FTransmitAudio:=aConnectParams_.FTransmitAudio;
  FLoop:=aConnectParams_.FLoop;
  FreeAndNil(FFileReadThread);
  FFileReadThread:=TReadFileThread.Create(FFileName,GetFramerClassFromFileName(FFileName),self);

end;

procedure TMediaStreamDataSource_File.DoDisconnect;
begin
  FreeAndNil(FFileReadThread);
end;

constructor TMediaStreamDataSource_File.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
  FOnFileEnded:=TMediaStreamDataSourceFileEndedEvent.Create;
end;

class function TMediaStreamDataSource_File.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_File.Create;
end;

function TMediaStreamDataSource_File.CurrentPosition: int64;
begin
  result:=0;
  if FFileReadThread<>nil then
    result:=TReadFileThread(FFileReadThread).CurrentPosition;
end;

destructor TMediaStreamDataSource_File.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FOnFileEnded);
  FreeAndNil(FDataLock);
end;

function TMediaStreamDataSource_File.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';
end;

function TMediaStreamDataSource_File.LastStreamDataTime: TDateTime;
begin
  if FFileReadThread<>nil then
  begin
    FDataLock.Enter;
    try
      result:=FLastStreamDataTime;
    finally
      FDataLock.Leave;
    end;
  end
  else
    result:=0;
end;


function TMediaStreamDataSource_File.Length: int64;
begin
  result:=0;
  if FFileReadThread<>nil then
    result:=TReadFileThread(FFileReadThread).Length;
end;

procedure TMediaStreamDataSource_File.OnDataReceived(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);
begin
  FDataLock.Enter;
  try
    if (FFileReadThread=nil) then
      exit;

    FLastStreamDataTime:=Now;
    if aFormat.biMediaType=mtAudio then
    begin
      if FTransmitAudio then
        RaiseOnData(aFormat,aData,aDataSize,aInfo,aInfoSize);
    end
    else if aFormat.biMediaType=mtVideo then
    begin
      if FTransmitVideo then
        RaiseOnData(aFormat,aData,aDataSize,aInfo,aInfoSize);
    end;
  finally
    FDataLock.Leave;
  end;
end;


function GetReadThread(aThread : TThread): TReadFileThread;
begin
  result:=aThread as TReadFileThread;
  Assert(result<>nil);
end;

{ TReadFileThread }

constructor TReadFileThread.Create(const aFileName:string; aFramerClass:TStreamFramerClass; aOwner: TMediaStreamDataSource_File);
begin
  FOwner:=aOwner;
  FOpenLock:=TCriticalSection.Create;

  FReader:=TFileReader.Create(aFileName,aFramerClass);
  FStreamInfo:=FReader.StreamInfo;
  inherited Create(true);
end;

function TReadFileThread.CurrentPosition: int64;
begin
  result:=FReader.CurrentPosition;
end;

destructor TReadFileThread.Destroy;
begin
  inherited;
  FreeAndNil(FReader);
  FreeAndNil(FOpenLock);
end;

procedure TReadFileThread.ProcessFile;
var
  aTicks: Cardinal;
  aDelay : int64;

  aData: pointer;
  aDataSize: cardinal;
  aInfo: pointer;
  aInfoSize: cardinal;
  aFormat: TMediaStreamDataHeader;

  aStartTicks: cardinal;
  aCurrentTicks : cardinal;
  aFirstVideoFrame,aFirstAudioFrame : boolean;

  aFirstVideoFrameTimestampMs,aFirstAudioFrameTimestampMs,aTimeStampMs,aTimestampDeltaMs: int64;
  aFirstVideoFrameTimestamp,aFirstAudioFrameTimestamp, aLastVideoFrameTimestamp,aLastAudioFrameTimestamp: int64;
begin
  FOpenLock.Enter;
  try
    FReader.Reset;
    FStreamInfo:=FReader.StreamInfo;
  finally
    FOpenLock.Leave;
  end;


  aFirstVideoFrame:=true;
  aFirstAudioFrame:=true;

  aFirstVideoFrameTimestampMs:=0;
  aFirstAudioFrameTimestampMs:=0;
  aFirstVideoFrameTimestamp:=0;
  aFirstAudioFrameTimestamp:=0;
  aLastVideoFrameTimestamp:=0;
  aLastAudioFrameTimestamp:=0;

  aStartTicks:=GetTickCount;

  while not Terminated do
  begin
    //----------- Читаем фрейм
    aFormat.Clear;
    if not FReader.GetNextFrame(aFormat,aData,aDataSize, aInfo,aInfoSize) then
      break;

    aTimeStampMs:=aFormat.TimeStamp*aFormat.TimeKoeff;

    //Прежде чем отдать фрейм наружу, посмотрим, сколько нужно подождать от предыдущего фрейма
    if (aFormat.biMediaType=mtVideo) then
    begin
      if (aFirstVideoFrame) then
      begin
        aFirstVideoFrameTimestampMs:=aTimeStampMs;
        aFirstVideoFrameTimestamp:=aFormat.TimeStamp;
      end
      else begin
        aCurrentTicks:=GetTickCount;
        if aCurrentTicks<aStartTicks then //Страховка
          break;

        aTicks:=aCurrentTicks-aStartTicks; //Считаем сколько прошло мс реального времени от момента старта файла
        aTimestampDeltaMs:=aTimeStampMs-aFirstVideoFrameTimestampMs;

        if (aTimestampDeltaMs>aTicks) {and (aTimestampDeltaMs-aTicks>10) } then //10 мс оставляем на всякие задержки в процессе передачи
          aDelay:=aTimestampDeltaMs-aTicks{-10} //Вычитаем из расчетного времени реальное время. Дельта - величина, на сколько надо притормозить
        else
          aDelay:=0; //Мы не успеваем

        //OutputDebugString(PChar(Format('TimeStamp: %d, Delay:%d',[aTimeStampMs,aDelay])));
        Assert(aDelay>=0);
        if aDelay>0 then
          if not FOwner.FNoTimings then
            Pause(aDelay)
      end;

      aLastVideoFrameTimestamp:=aFormat.TimeStamp;
      aFirstVideoFrame:=false;
    end
    else if aFormat.biMediaType=mtAudio then
    begin
      aLastAudioFrameTimestamp:=aFormat.TimeStamp;
      if aFirstAudioFrame then
      begin
        aFirstAudioFrameTimestamp:=aFormat.TimeStamp;
        aFirstAudioFrameTimestampMs:=aTimeStampMs;
      end
      else begin
        if aFirstVideoFrame then
        begin
          aCurrentTicks:=GetTickCount;
          if aCurrentTicks<aStartTicks then //Страховка
            break;

          aTicks:=aCurrentTicks-aStartTicks; //Считаем сколько прошло мс реального времени от момента старта файла
          aTimestampDeltaMs:=aTimeStampMs-aFirstAudioFrameTimestampMs;

          if (aTimestampDeltaMs>aTicks)  then
            aDelay:=aTimestampDeltaMs-aTicks //Вычитаем из расчетного времени реальное время. Дельта - величина, на сколько надо притормозить
          else
            aDelay:=0; //Мы не успеваем

          //OutputDebugString(PChar(Format('TimeStamp: %d, Delay:%d',[aTimeStampMs,aDelay])));
          Assert(aDelay>=0);
          if aDelay>0 then
            if not FOwner.FNoTimings then
            begin
              inc(FAudioTotalSleep,aDelay);
              inc(FAudioSleepHits);
              Sleep(aDelay)
            end;

        end;

      end;

      aFirstAudioFrame:=false;
    end;


    if FOwner<>nil then
    begin
      if aFormat.biMediaType=mtVideo then
        inc(aFormat.TimeStamp,FVideoTimeStampBase)
      else
        inc(aFormat.TimeStamp,FAudioTimeStampBase);

      FOwner.OnDataReceived(aFormat,aData,aDataSize,aInfo,aInfoSize);
    end;
  end;


  inc(FVideoTimeStampBase,aLastVideoFrameTimestamp-aFirstVideoFrameTimestamp);
  inc(FAudioTimeStampBase,aLastAudioFrameTimestamp-aFirstAudioFrameTimestamp);
end;

procedure TReadFileThread.Execute;
const
  aMethodName = 'TReadFileThread.Execute;';
var
  aHandler: TMediaStreamDataSourceFileEndedHandler;
begin
  SetCurrentThreadName('Source: MediaStream.DataSource.File_.'+ClassName);

  while not Terminated do
  begin
    try
      ProcessFile;
    except
      on E:Exception do
        if FOwner<>nil then
          ; //TODO
    end;

    for aHandler in FOwner.FOnFileEnded do
      aHandler(FOwner);

    if not FOwner.FLoop then
      break;
  end;
end;

function TReadFileThread.Length: int64;
begin
  result:=FReader.Length;
end;

{ TFileReader }

constructor TFileReader.Create(const aFileName: string;
  aFramerClass: TStreamFramerClass);
begin
  FStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  FFramer:=aFramerClass.Create;
  FClean:=true;

  //Эта команда нужна:
  // 1. она позволяет нам получить формат файла
  // 2. она позволяет нам убедиться, что поток валидного формата

  FFramer.OpenStream(FStream);
end;

function TFileReader.CurrentPosition: int64;
begin
  result:=0;
  if FFramer.RandomAccess<>nil then
    result:=FFramer.RandomAccess.Position;
end;

destructor TFileReader.Destroy;
begin
  FreeAndNil(FFramer);
  FreeAndNil(FStream);
  inherited;
end;

function TFileReader.GetNextFrame(
  out aOutFormat: TMediaStreamDataHeader;
  out aOutData: pointer; out aOutDataSize: cardinal;
  out aOutInfo: pointer;
  out aOutInfoSize: cardinal): boolean;
begin
  FClean:=false;
  result:=FFramer.GetNextFrame(aOutFormat,aOutData,aOutDataSize,aOutInfo,aOutInfoSize);
end;

function TFileReader.Length: int64;
begin
  result:=0;
  if FFramer.RandomAccess<>nil then
    result:=FFramer.RandomAccess.StreamInfo.Length;
end;

procedure TFileReader.Reset;
begin
  if not FClean then
  begin
    FClean:=true;
    FStream.Position:=0;
    FFramer.OpenStream(FStream); //Каждый раз заново открываем, потому что нужно сбросить позицию в потоке
  end;
end;

function TFileReader.StreamInfo: TBytes;
begin
  result:=FFramer.StreamInfo;
end;

function TFileReader.VideoStreamType: TStreamType;
begin
  result:=FFramer.VideoStreamType;
end;

function TFileReader.AudioStreamType: TStreamType;
begin
  result:=FFramer.AudioStreamType;
end;


initialization
  MediaStreamDataSourceFactory.Register('file',TMediaStreamDataSource_File);


end.

