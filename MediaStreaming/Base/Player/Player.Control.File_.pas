unit Player.Control.File_;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
  uses Messages, Windows, Forms, Graphics, SysUtils, Controls, Classes, FileCtrl, SyncObjs, Contnrs,
  Player.VideoOutput.Base,MediaProcessing.Definitions,MediaStream.Framer,Player.Control;

type
  TWindowFilePlayer = class;

  TPlayerState = (psEmpty, //Не указан файл
                  psStopped, //Файл указан, но не проигрывается
                  psPlaying, //Файл указан и проигрывается
                  psPaused //Файл указан, проигрывается, но сейчас находится в режиме паузы
                  );

  TWindowFilePlayerDirection = (pdForward,pdBackward);

  TWindowFilePlayerEvent = procedure(Sender: TWindowFilePlayer) of object;
  TWindowFilePlayerReadThreadErrorEvent = procedure(Sender: TWindowFilePlayer; E:Exception) of object;

  TWindowFilePlayer = class (TWindowPlayerBase)
  private
    FPlayThread: TThread;
    FPlayLock : TCriticalSection;

    FFramer: TStreamFramer;
    FStream: TStream;
    FOwnStream: boolean;
    FDestroyVideoContextOnStop: boolean;
    FLastReadPosition: int64;
    FLastReadPositionActual: boolean;

    FState: TPlayerState;
    FPlayOneVideoFrame: boolean;
    FAutoClose: boolean;
    FClosingFile : integer;

    FOnClosed: TWindowFilePlayerEvent;
    FOnFileEnding: TWindowFilePlayerEvent;
    FOnPlayThreadError: TWindowFilePlayerReadThreadErrorEvent;
    FOnFileEnd: TWindowFilePlayerEvent;
    FSpeed: double;
    FDirection: TWindowFilePlayerDirection;

    FAttrsLock: TCriticalSection;

    procedure Control_WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure DestroyVideoContextIfNeeded;
    procedure ClearBuffers;

    procedure CheckOpened;
    procedure CheckInMainThread;

    //Текущий файл закончился. Вызывается после того, как поток закроется
    procedure OnFilePlayingThreadEnd;
    //При проигрывании файла произошла ошибка
    procedure OnFilePlayingThreadError(aError: Exception);

    procedure OnCurrentFrameChanged;
    procedure SetDirection(const Value: TWindowFilePlayerDirection);
    function GetCurrPosition: int64;
    procedure SetCurrPosition(const Value: int64);
    procedure SetSpeed(const Value: double);
  public
    constructor Create(aControlClass: TWinControlClass=nil);
    destructor Destroy; override;

    procedure Close;
    procedure Open(const aFileName: string); overload;
    procedure Open(const aStream: TStream; aFramerClass: TStreamFramerClass; aOwnsStream: boolean=false); overload;

    function  FileName:string;

    procedure Play;
    procedure Stop;

    procedure Pause(aProcessOneFrameBefore: boolean=false);
    procedure Resume;

    procedure PlayOneFrame(aWaitForComplete: boolean);

    //При достижении конца файла автоматически выгружать файл и устанавливать состояние State=psEmpty
    property AutoClose: boolean read FAutoClose write FAutoClose;

    property State: TPlayerState read FState;

    // скорость воспроизведения. По умолчанию = 1.
    property Speed: double read FSpeed write SetSpeed;

    // направление проигрывания
    property Direction: TWindowFilePlayerDirection read FDirection write SetDirection;

    // текущая позиция в мс
    property CurrPosition: int64 read GetCurrPosition write SetCurrPosition;

    //длина файла в мс. Может вернуть -1, если не поддерживается
    function FileTimeLength: int64;

    //Очищать экран при останове проигрывания
    property DestroyVideoContextOnStop: boolean read FDestroyVideoContextOnStop write FDestroyVideoContextOnStop default true;

    //Событие о том, что файл был доигран до конца. Вызывается после того, как пройдет команда Stop или поток закроется (для AutoClose=true)
    property OnFileEnd: TWindowFilePlayerEvent read FOnFileEnd write FOnFileEnd;
    //Событие о том, что файл был доигран до конца. Вызывается ДО того, как пройдет команда Stop или поток закроется (для AutoClose=true)
    property OnFileEnding: TWindowFilePlayerEvent read FOnFileEnding write FOnFileEnding;

    property OnPlayThreadError: TWindowFilePlayerReadThreadErrorEvent read FOnPlayThreadError write FOnPlayThreadError;

    property OnClosed : TWindowFilePlayerEvent read FOnClosed write FOnClosed;
  end;

implementation
  uses uTrace, MediaStream.FramerFactory,Player.VideoOutput.AllTypes,Player.AudioOutput.AllTypes,uBaseClasses;

type
  TPlayerFileThread = class (TThread)
  private
    FOwner: TWindowFilePlayer;
    FFinished: boolean;
    FException : Exception;
    FInSynhronize : integer;

  protected
    procedure Execute; override;
    procedure Pause(aMilliseconds: int64);

    procedure OnEnd;
    procedure OnException;

    procedure Synchronize(Method: TThreadMethod; aCatchExceptions: boolean);
  public
    constructor Create(aOwner: TWindowFilePlayer);
    destructor Destroy; override;

    property Finished: boolean read FFinished;
  end;

{ TPlayerFileThread }

constructor TPlayerFileThread.Create(aOwner: TWindowFilePlayer);
begin
  FOwner:=aOwner;
  FreeOnTerminate:=true; //Поток разрушится сам

  inherited Create(false);
end;

destructor TPlayerFileThread.Destroy;
begin
  inherited;
end;

procedure TPlayerFileThread.Execute;
var
  aTickStart,aTickEnd,aTickDelta: Cardinal;
  aFrameDuration, aDelay : int64;

  aData: pointer;
  aDataSize: cardinal;
  aInfo: pointer;
  aInfoSize: cardinal;
  aFormat: TMediaStreamDataHeader;
  aFirstFrame : boolean;

  aDataCopy: TBytes;
  aInfoCopy: TBytes;
  b: boolean;
  aRA: TStreamFramerRandomAccess;

  aAudioPresent: boolean;
begin
  aFirstFrame:=true;
  aTickStart:=GetTickCount;
  aAudioPresent:=false;

  try
    while not Terminated do
    begin
      if (FOwner.FState=psPaused) and (not FOwner.FPlayOneVideoFrame) then
      begin
        sleep(40);
        //FOwner.UpdateImage;
        continue;
      end;

      //----------- Читаем фрейм
      aFormat.Clear;
      FOwner.FPlayLock.Enter;
      try
        b:=true;
        if FOwner.FDirection=pdBackward then
        begin
          aRA:=FOwner.FFramer.RandomAccess;
          b:=aRA<>nil;
          if b then
            b:=aRA.SeekToPrevVideoKeyFrame;
        end;

        while b do
        begin
          b:=FOwner.FFramer.GetNextFrame(aFormat,aData,aDataSize, aInfo,aInfoSize);
          if FOwner.FPlayOneVideoFrame and b and (aFormat.biMediaType<>mtVideo) then
            continue;
          break;
        end;

        if not b then
          break;

        SetLength(aDataCopy,aDataSize);
        CopyMemory(@aDataCopy[0],aData,aDataSize);

        SetLength(aInfoCopy,aInfoSize);
        CopyMemory(@aInfoCopy[0],aInfo,aInfoSize);
      finally
        FOwner.FPlayLock.Leave;
      end;

      aTickEnd:=GetTickCount;

      if aFormat.biMediaType=mtAudio then
      begin
        aAudioPresent:=true;
      end;

      //Прежде чем отдать фрейм наружу, посмотрим, сколько нужно подождать от предыдущего фрейма
      if not aFirstFrame then
      begin
        if aFormat.biMediaType=mtVideo then
        begin
          aTickDelta:=aTickEnd-aTickStart; //Считаем сколько прошло мс реального времени от момента старта файла

          FOwner.FAttrsLock.Enter;
          try
            aFrameDuration:=(aFormat.TimeStamp*aFormat.TimeKoeff-FOwner.FLastReadPosition);
            if (FOwner.Speed>0) and (FOwner.Speed<>1) then
            begin
              if FOwner.Speed>1 then
              begin
                if FOwner.Speed>5 then
                  aFrameDuration:=0
                else
                  aFrameDuration:=Trunc(aFrameDuration/FOwner.Speed);
              end
              else
                aFrameDuration:=Round(aFrameDuration/FOwner.Speed);
            end;

            if (FOwner.Direction=pdBackward) then
              aFrameDuration:=-aFrameDuration;

            if (FOwner.FLastReadPositionActual) and (aFrameDuration>aTickDelta) then
              aDelay:=aFrameDuration-aTickDelta-1 //Вычитаем из расчетного времени реальное время. Дельта - величина, на сколько надо притормозить
            else
              aDelay:=0; //Мы не успеваем

            //if aDelay>2000 then
            //begin
            //  try raise Exception.Create('Error Message'); except end;
            //end;

            FOwner.FLastReadPosition:=aFormat.TimeStamp*aFormat.TimeKoeff;
            FOwner.FLastReadPositionActual:=true;
          finally
            FOwner.FAttrsLock.Leave;
          end;

          if aDelay>0 then
            if (not aAudioPresent) or (not FOwner.AudioEnabled) then //Если звук есть, то он будет естественным регулятором скорости
              Pause(aDelay);
        end;
      end;

      aFirstFrame:=false;
      aTickStart:=GetTickCount;

      if FOwner<>nil then
      begin
        if (aFormat.biMediaType=mtAudio) and (FOwner.Speed<>1) then
          //Do nothing
        else if (aFormat.biMediaType=mtAudio) and (FOwner.FDirection=pdBackward) then
          //Do nothing
        else if (aFormat.biMediaType=mtAudio) and (not FOwner.AudioEnabled) then

        else begin
          if (FOwner.Speed>5) and (not (ffKeyFrame in aFormat.biFrameFlags)) then
            //Если большая скорость, то ничего не вопроизводим кроме опорных кадров
          else  if FOwner.FPlayThread=self  {эксперимент}then
            FOwner.ProcessFrame(aFormat,aDataCopy,aDataSize,aInfoCopy,aInfoSize);
        end;

        if aFormat.biMediaType=mtVideo then //ТОлько для видео
        begin
          FOwner.OnCurrentFrameChanged;
          FOwner.FPlayOneVideoFrame:=false;
        end;
      end;
    end;
  except
    on E:Exception do
    begin
      FException:=E;
      Synchronize(OnException,true);
      FException:=nil;
    end;
  end;

  //???FOwner.FPlayOneVideoFrame:=false; //на всякий случай
  Synchronize(OnEnd,true);
end;

procedure TPlayerFileThread.OnEnd;
begin
  FFinished:=true;
  FOwner.OnFilePlayingThreadEnd;
end;

procedure TPlayerFileThread.OnException;
begin
  if FOwner<>nil then
      FOwner.OnFilePlayingThreadError(FException);
end;

procedure TPlayerFileThread.Pause(aMilliseconds: int64);
begin
  while true do
  begin
    if aMilliseconds<100 then
    begin
      Sleep(aMilliseconds);
      break;
    end
    else begin
      Sleep(100);
      dec(aMilliseconds,100);
      if Terminated then
        break;
    end;
  end;
end;

procedure TPlayerFileThread.Synchronize(Method: TThreadMethod;
  aCatchExceptions: boolean);
begin
  inc(FInSynhronize);
  try
    if aCatchExceptions then
    try
      inherited Synchronize(Method);
    except
    end
    else
      inherited Synchronize(Method);
  finally
    dec(FInSynhronize);
  end;
end;

{ TWindowFilePlayer }

procedure TWindowFilePlayer.CheckInMainThread;
begin
  if GetCurrentThreadId<>MainThreadID then
    raise Exception.Create('Операция должна выполняться в основном потоке');
end;

procedure TWindowFilePlayer.CheckOpened;
begin
  if FState=psEmpty then
    raise EHHPlayerException.Create('Файл не загружен');

  Assert(FStream<>nil);
end;

procedure TWindowFilePlayer.ClearBuffers;
begin
  //FAudioOutputLock.Enter;
  try
    if AudioOutput<>nil then
      AudioOutput.DeallocateAudioContext;
  finally
    //FAudioOutputLock.Leave;
  end;

  //FFrameLock.Enter;
  try
    if VideoOutput<>nil then
      VideoOutput.ResetBuffer;
  finally
    //FFrameLock.Leave;
  end;
end;

procedure TWindowFilePlayer.Close;
begin
  if FState=psEmpty then
  begin
    DestroyVideoContextIfNeeded; //Может так получится, что контекст не был разрушен с прошлого раза из-за настроек

    //В случае неуспешного открытия часть полей может быть уже инициализирована. Все затираем
    FreeAndNil(FPlayThread);
    FreeAndNil(FFramer);

    if FOwnStream then
      FreeAndNil(FStream);
    FStream:=nil;

    exit;
  end;

  inc(FClosingFile);
  try
    Stop;

    FreeAndNil(FPlayThread);
    FreeAndNil(FFramer);
    if FOwnStream then
      FreeAndNil(FStream);
    FStream:=nil;

    FState:=psEmpty;

    if Assigned(FOnClosed) then
      FOnClosed(self);
  finally
    dec(FClosingFile);
  end;

end;

procedure TWindowFilePlayer.Control_WMPaint(var Message: TWMPaint);
begin
  UpdateImage;
end;

constructor TWindowFilePlayer.Create(aControlClass: TWinControlClass=nil);
begin
  inherited Create(TPlayerVideoOutput_AllTypes,TPlayerAudioOutput_AllTypes,aControlClass);
  FDestroyVideoContextOnStop:=true;
  FAttrsLock:=TCriticalSection.Create;
  FPlayLock:=TCriticalSection.Create;
  FSpeed:=1;
  RegisterCustomTrace(ClassName,'','.wfplyr');
end;

destructor TWindowFilePlayer.Destroy;
begin
  Close;
  Assert(FPlayThread=nil);
  Assert(FStream=nil);
  Assert(FFramer=nil);
  FreeAndNil(FAttrsLock);
  FreeAndNil(FPlayLock);

  inherited;
end;

procedure TWindowFilePlayer.DestroyVideoContextIfNeeded;
begin
  if FDestroyVideoContextOnStop then
  begin
    if VideoOutput.VideoContextAllocated then
    begin
      VideoOutput.ClearScreen;
      VideoOutput.DeallocateVideoContext;
    end;

    VideoCanvas.Invalidate;
  end;
end;

function TWindowFilePlayer.FileName: string;
begin
  result:='';
  if FStream is TFileStream then
    result:=TFileStream(FStream).FileName;
end;

function TWindowFilePlayer.FileTimeLength: int64;
var
  aRA: TStreamFramerRandomAccess;
begin
  result:=-1;
  aRA:=FFramer.RandomAccess;
  if aRA<>nil then
    result:=aRA.StreamInfo.Length;
end;

function TWindowFilePlayer.GetCurrPosition: int64;
begin
  FAttrsLock.Enter;
  try
    result:=FLastReadPosition;
  finally
    FAttrsLock.Leave;
  end;


  (*aRA:=FFramer.RandomAccess;
  if aRA=nil then
    result:=0
  else
    result:=aRA.Position
    *)
end;

procedure TWindowFilePlayer.OnCurrentFrameChanged;
begin

end;

procedure TWindowFilePlayer.OnFilePlayingThreadEnd;
var
  aThreadTerminated: boolean;
begin
  CheckInMainThread;

  aThreadTerminated:=(FPlayThread=nil) or TPlayerFileThread(FPlayThread).Terminated;

  //Предполагаем, что если поток не был прерван, то он добрался до конца файла и благополучно
  //закончился. А если это так - издаем соотв. событие
  if (not aThreadTerminated) and Assigned(FOnFileEnding) then
    FOnFileEnding(self);

  if FAutoClose then
    Close
  else
    Stop;

  ClearBuffers;
  if (not aThreadTerminated) and Assigned(FOnFileEnd) then
    FOnFileEnd(self);
end;

procedure TWindowFilePlayer.OnFilePlayingThreadError(aError: Exception);
begin
  if Assigned(FOnPlayThreadError) then
    FOnPlayThreadError(self,aError);
end;

procedure TWindowFilePlayer.Open(const aStream: TStream;aFramerClass: TStreamFramerClass; aOwnsStream: boolean);
const
  aMethodName = 'TWindowFilePlayer.Open(Stream)';
var
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(ClassName,aMethodName,'');

  try
    Close;

    FStream:=aStream;
    FOwnStream:=aOwnsStream;

    try
      TraceLine(ClassName,'Создание framer '+aFramerClass.ClassName+'...');
      FFramer:=aFramerClass.Create;
      TraceLine(ClassName,'Открытие в framer потока...');
      FFramer.OpenStream(FStream);
      TraceLine(ClassName,'Получение от framer интерфейса позиционирования...');
      FFramer.RandomAccess;
      //TPlayerVideoOutput_AllTypes(VideoOutput).StreamType:=FFramer.StreamType;

      VideoOutput.SynchronousDisplay:=true;
      AudioOutput.SyncronousPlay:=true;
    except
      Close;
      raise;
    end;

    FState:=psStopped;
  finally
    TraceProcEnd(ClassName,aMethodName,aTraceID);
  end;
end;

procedure TWindowFilePlayer.Open(const aFileName: string);
var
  aStream: TStream;
begin
  Close;
  aStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  Open(aStream,GetFramerClassFromFileName(aFileName),true);
end;

procedure TWindowFilePlayer.Pause(aProcessOneFrameBefore: boolean);
begin
  if FState <> psPlaying then
    Exit;

  FState := psPaused;
  //ClearBuffers;
  if aProcessOneFrameBefore then
    FPlayOneVideoFrame:=true;
end;

procedure TWindowFilePlayer.Play;
begin
  if FState=psPlaying then
    exit;

  CheckOpened;

  if FState in [psStopped,psPaused] then
  begin
    FState:=psPlaying;
    if FPlayThread=nil then
      FPlayThread:=TPlayerFileThread.Create(self);
  end;
end;

procedure TWindowFilePlayer.PlayOneFrame(aWaitForComplete: boolean);
var
  i: integer;
begin
  if FState=psEmpty then
    exit;

  if FState=psStopped then
    Play;

  Pause;
  FPlayOneVideoFrame:=true;

  if aWaitForComplete then
    for i:=0 to 300 do
    begin
      if not FPlayOneVideoFrame then
        break;
      Sleep(10);
    end;
end;

procedure TWindowFilePlayer.Resume;
begin
  if FState <> psPaused then
    Exit;
  FState := psPlaying;
end;

procedure TWindowFilePlayer.SetCurrPosition(const Value: int64);
var
  aRA: TStreamFramerRandomAccess;
begin
  FPlayLock.Enter;
  try
    aRA:=FFramer.RandomAccess;
    if aRA<>nil then
      aRA.Position:=Value;

    FAttrsLock.Enter;
    try
      FLastReadPositionActual:=false;
      FLastReadPosition:=Value;
    finally
      FAttrsLock.Leave;
    end;

    //VideoOutput.ResetBuffer;
    //VideoOutput.ClearScreen;
  finally
    FPlayLock.Leave;
  end;

end;

procedure TWindowFilePlayer.SetDirection(
  const Value: TWindowFilePlayerDirection);
begin
  if Value=FDirection then
    exit;

  FAttrsLock.Enter;
  try
    FDirection := Value;
    FLastReadPositionActual:=false;
  finally
    FAttrsLock.Leave;
  end;
end;

procedure TWindowFilePlayer.SetSpeed(const Value: double);
begin
  FAttrsLock.Enter;
  try
    FSpeed := Value;
  finally
    FAttrsLock.Leave;
  end;
end;

procedure TWindowFilePlayer.Stop;
var
  aHandle: THandle;
  aInSynchronize: integer;
  aRA: TStreamFramerRandomAccess;
begin
  if FState=psEmpty then
    exit;

  if FPlayThread<>nil then
  begin
    Assert(FPlayThread.FreeOnTerminate);

    //Если поток еще не завершился (Stop может вызываться именно по случаю завершения потока), то остановим его
    if not TPlayerFileThread(FPlayThread).Finished then
    begin
      aHandle:=FPlayThread.Handle;
      aInSynchronize:=TPlayerFileThread(FPlayThread).FInSynhronize;

      FPlayThread.Terminate;

      if aInSynchronize =0 then //Если мы не в режиме синхронизации
        TThread.WaitForSafe(aHandle); //Здесь поток должен уйти и произойдет повторно
    end;

    FPlayThread:=nil; //Поток разрушится сам
  end;

  //FreeAndNil(FPlayThread);

  ClearBuffers;
  FState:=psStopped;
  DestroyVideoContextIfNeeded;

  //Перемещаемся на самое начало файла
  if FClosingFile=0 then
  begin
    //Если автозакрытие, то останов приравнивается к выгрузке файла
    if FAutoClose then
      Close
    else begin
      //Reset
      aRA:=FFramer.RandomAccess;
      if aRA<>nil then
        aRA.Position:=0;
      OnCurrentFrameChanged;
    end;
  end;
end;

end.
