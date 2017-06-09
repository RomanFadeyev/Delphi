unit Player.AudioOutput.Base;
{$WARN SYMBOL_PLATFORM OFF}

interface
  uses Messages, Windows, Forms, Graphics, SysUtils, Controls, Classes, SyncObjs, Contnrs,Generics.Collections,
       MMSystem,MediaProcessing.Definitions;


type
  TPlayerAudioOutput = class;
  TAudioVolume = 0..100;

  TPCMFormat = packed record
    Channels: cardinal;
    BitsPerSample: cardinal;
    SamplesPerSec: cardinal;
  end;

  //Decode To PCM
  TAudioOutputDecoder = class
  public
    function DecodeToPCM(
      const aFormat: TMediaStreamDataHeader;
      aData: pointer; aDataSize: cardinal;
      aInfo: pointer; aInfoSize: cardinal;
      out aPcmFormat: TPCMFormat;
      out aPcmData: pointer; out aPcmDataSize: cardinal):boolean; virtual; abstract;

    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean; virtual;
    procedure ResetBuffer; virtual;


    constructor Create; virtual;
  end;
  TAudioOutputDecoderClass = class of TAudioOutputDecoder;

  TPlayerAudioOutput = class
  private
    FStreamType : TStreamType;
    FServiceWndHandle  : THandle;
    FDecoder : TAudioOutputDecoder;
    FDecoderClasses: TDictionary<TStreamType,TAudioOutputDecoderClass>;
    FLastUnspportedStreamType: TStreamType;
    FEnabled: boolean;
    FSyncronousPlay: boolean;

    procedure WndProc (var Message : TMessage);
  protected
    function  GetVolume: TAudioVolume; virtual; abstract;
    procedure SetVolume(const Value: TAudioVolume); virtual; abstract;

    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean;
    procedure SetStreamType(const Value: TStreamType);
    procedure RegisterDecoderClass(aStreamType:TStreamType;aClass: TAudioOutputDecoderClass);
    function  GetStreamTypeHandlerClass(aType: TStreamType): TAudioOutputDecoderClass; virtual;

    procedure PlayPCM(const aPcmFormat: TPCMFormat; aPcmData: pointer;aPcmDataSize: cardinal); virtual; abstract;
    procedure TraceLine(const aMessage: string);
  public
    procedure ResetBuffer; virtual; abstract;
    procedure DeallocateAudioContext; virtual; abstract;

    procedure WriteAudioData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); virtual;

    function  StatusInfo:string; virtual;
    //сколько в буфере сейчас лежит данных в мс
    function  GetBufferedDataDuration: cardinal; virtual;

    //
    property  SyncronousPlay: boolean read FSyncronousPlay write FSyncronousPlay;

    constructor Create; virtual;
    destructor Destroy; override;

    property  StreamType: TStreamType read FStreamType write SetStreamType;
    property  Volume: TAudioVolume read GetVolume write SetVolume;
    property  Enabled: boolean read FEnabled write FEnabled;
  end;
  TPlayerAudioOutputClass  = class of TPlayerAudioOutput;

  TWaveOutBuffer = record
    Buffer : PAnsiChar;
    BufferSize: cardinal;

    Header: WAVEHDR;
    Playing: boolean;
  end;

  TPcmInputBufferItem = class
  private
    FData: TBytes;
    FActualDataSize: cardinal;
  public
    constructor Create(aSize: integer);
    procedure Assign(aData: pointer; aDataSize: cardinal);
    procedure Append(aData: pointer; aDataSize: cardinal);
  end;

  TInputBuffer = class (TQueue<TPcmInputBufferItem>)
  private
    FLastAdded:TPcmInputBufferItem;
  protected
     procedure Notify(const Item: TPcmInputBufferItem; Action: TCollectionNotification); override;
  end;

  TPlayerAudioOutputWaveOut = class (TPlayerAudioOutput)
  private
    //2 - обязательный минимум для флипа, остальные - резервные на случай задержек.
    //Каждый буфер - 40 мс. 40*2(флип) = 80 - Это минимальное значение, когда еще не чувствуется отставание звука
    //Итого буферы держат n*40 мс данных, что позволяет не "захлебыватья" при небольших перебоях с поставками данных
    FWaveOutBuffers: array [0..2] of TWaveOutBuffer;
    FWaveOutBuffersLock: TCriticalSection;

    FAudioDeviceId: cardinal;
    FWaveOutHandle: HWAVEOUT;
    FWindowHandle: THandle;

    FInputBufferLock : TCriticalSection;
    FInputBuffer : TInputBuffer;
    FInputBufferItemPool: TList<TPcmInputBufferItem>;
    FInputBufferEmptyTriggered: boolean;

    FAudioChannels: cardinal;
    FAudioBitsPerSample: cardinal;
    FAudioSamplesPerSec: cardinal;
    FAvgBytesPerSec : cardinal;

    //FWaitIntervalWhenOutOfBuffer: cardinal;
    FNoAudioDevice : boolean;
    FInitialBufferedDataDuration: cardinal;
    FAdaptedInitialBufferedDataDuration: cardinal;

    FPlayedDataDuration: cardinal;
    FFirstPlayTimeStamp: cardinal;
    FLastPlayedDataTimeStamp: cardinal;
    FInputBufferEmptyEventCount: cardinal;

    FVolume : TAudioVolume;

    procedure OnWaveOutBufferDone(aBuffer: PWaveHdr);
    procedure LoadWaveOutBuffer(aBufferIndex: cardinal);
    procedure StartPcmDriverPlayIfPossible;
    procedure ClearInputBuffer;

    procedure WndProc(var Message: TMessage);
    procedure SetInitialBufferedDataDuration(const Value: cardinal);
  protected
    procedure OpenInternal(aAudioChannels: cardinal; aBitsPerSample: Cardinal; aSamplesPerSec: cardinal);
    procedure CloseInternal;
    function  Opened:boolean;
    procedure CheckOpened;

    function  GetVolume: TAudioVolume; override;
    procedure SetVolume(const Value: TAudioVolume); override;

    procedure PlayPCM(const aPcmFormat: TPCMFormat; aPcmData: pointer;aPcmDataSize: cardinal); override;

    procedure PlayPCMInternal(aPcmData: pointer;aPcmDataSize: cardinal);

    procedure OnInputBufferEmpty;
  public
    procedure ResetBuffer; override;
    procedure DeallocateAudioContext; override;

    procedure WriteAudioData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
    function  GetBufferedDataDuration: cardinal; override;
    function  GetInputBufferDuration: cardinal;
    function  GetOutputBufferDuration: cardinal;

    function  StatusInfo:string; override;

    //Сколько забуферизировать до начала проигрывания. чем больше значение, тем меньше шансов на "щелчки", но увеличиывается отставание звука
    property InitialBufferedDataDuration: cardinal read FInitialBufferedDataDuration write SetInitialBufferedDataDuration;

    constructor Create; override;
    destructor Destroy; override;

    property AudioDeviceId: cardinal read FAudioDeviceId write FAudioDeviceId;
  end;

  ENoSoundDeviceError = class (Exception)
  public
    constructor Create;
  end;

implementation
  uses Math,uTrace;

const
   WM_SETSTREAMTTYPE = WM_USER+$100;

const
  WaveOutDriverBufferSizeMsec = 40;

{ TPcmInputBufferItem }

procedure TPcmInputBufferItem.Append(aData: pointer; aDataSize: cardinal);
begin
  Assert(FActualDataSize+aDataSize<=cardinal(Length(FData)));
  CopyMemory(@FData[FActualDataSize],aData,aDataSize);
  inc(FActualDataSize,aDataSize);
end;

procedure TPcmInputBufferItem.Assign(aData: pointer; aDataSize: cardinal);
begin
  Assert(cardinal(Length(FData))>=aDataSize);
  FActualDataSize:=aDataSize;
  CopyMemory(@FData[0], aData,aDataSize);
end;

constructor TPcmInputBufferItem.Create(aSize: integer);
begin
  SetLength(FData,aSize);
end;

{ TPlayerAudioOutput }

constructor TPlayerAudioOutput.Create;
begin
  FDecoderClasses:=TDictionary<TStreamType,TAudioOutputDecoderClass>.Create;
  FStreamType:=INVALID_HANDLE_VALUE;
  FServiceWndHandle := AllocateHWnd(WndProc);
  FEnabled:=true;
  Assert(FServiceWndHandle<>0);
end;

destructor TPlayerAudioOutput.Destroy;
begin
  inherited;
  FreeAndNil(FDecoderClasses);

  DeallocateHWnd(FServiceWndHandle);
  FServiceWndHandle:=0;
end;

function TPlayerAudioOutput.GetBufferedDataDuration: cardinal;
begin
  result:=0;
end;

function TPlayerAudioOutput.GetStreamTypeHandlerClass(
  aType: TStreamType): TAudioOutputDecoderClass;
begin
  if not FDecoderClasses.TryGetValue(aType,result) then
    result:=nil;
end;

function TPlayerAudioOutput.IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal): boolean;
begin
  result:=(aFormat.biMediaType=mtAudio) and (aData<>nil) and (aDataSize<>0);

  if result then
    if FDecoder<>nil then
      result:=FDecoder.IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize);
end;

procedure TPlayerAudioOutput.RegisterDecoderClass(aStreamType: TStreamType;
  aClass: TAudioOutputDecoderClass);
begin
  FDecoderClasses.Add(aStreamType,aClass);
end;

procedure TPlayerAudioOutput.SetStreamType(const Value: TStreamType);
var
  aClass: TAudioOutputDecoderClass;
begin
  Assert(GetCurrentThreadId=MainThreadID);
  if FStreamType<>Value then
    FreeAndNil(FDecoder);

  if FDecoder<>nil then
    exit;

  FStreamType:=INVALID_HANDLE_VALUE;

  aClass:=GetStreamTypeHandlerClass(Value);
  if aClass=nil then
    raise Exception.Create(Format('Неподдерживаемый тип потока %s',[GetStreamTypeName(Value)]));

  FDecoder:=aClass.Create;
  FStreamType:=Value;
end;

function TPlayerAudioOutput.StatusInfo: string;
begin
  result:='General'+#13#10;
  result:=result+  Format('  Type:%s'#13#10,[ClassName]);

  //result:=result+'Settings: '#13#10;
  //result:=result+  Format('  SynchronousDisplay:%s'#13#10,[BoolToStr(SynchronousDisplay,true)]);
  //result:=result+  Format('  WaitOnOutOfBuffer:%s'#13#10,[BoolToStr(WaitOnOutOfBuffer,true)]);

  result:=result+'Statistics: '#13#10;
  result:=result+  Format('  BufferedData (secs):%.2f'#13#10,[GetBufferedDataDuration/1000]);
end;

procedure TPlayerAudioOutput.TraceLine(const aMessage: string);
begin
  uTrace.TraceLine(ClassName,aMessage);
end;

procedure TPlayerAudioOutput.WndProc(var Message: TMessage);
begin
  if Message.Msg=WM_SETSTREAMTTYPE then
  begin
    try
      FLastUnspportedStreamType:=0;
      SetStreamType(Message.WParam);
    except
      FLastUnspportedStreamType:=Message.WParam;
    end;
    exit;
  end;

  Message.result := DefWindowProc(FServiceWndHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TPlayerAudioOutput.WriteAudioData(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal);
var
  aPcmData: pointer;
  aPcmDataSize: cardinal;
  aPcmFormat: TPCMFormat;
begin
  if not FEnabled then
    exit;

  try
    if FStreamType<>aFormat.biStreamType then
    begin
      if FLastUnspportedStreamType=aFormat.biStreamType then //В случае если формат не поддерживается, не надо генерировать постоянно исключения. Будем тихо выходить
        exit;

      SendMessage(FServiceWndHandle,WM_SETSTREAMTTYPE,aFormat.biStreamType,0);
    end;

    if FDecoder=nil then
      exit;

    if (aDataSize=0) or not IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize)then
      exit;

    if not FDecoder.DecodeToPCM(aFormat, aData,aDataSize,aInfo,aInfoSize,aPcmFormat, aPcmData,aPcmDataSize) then
      exit;

    PlayPCM(aPcmFormat, aPcmData,aPcmDataSize);
  except
    on E:Exception do
      TraceLine('Exception: '+E.Message);
  end;
end;

{ TPlayerAudioOutputWaveOut }

procedure TPlayerAudioOutputWaveOut.CheckOpened;
begin
  if FWaveOutHandle=0 then
    raise Exception.Create('Вывод аудио еще не инициализирован');
end;

procedure TPlayerAudioOutputWaveOut.ClearInputBuffer;
begin
  FInputBufferLock.Enter;
  try
    while FInputBuffer.Count>0 do
      FInputBufferItemPool.Add(FInputBuffer.Dequeue);
  finally
    FInputBufferLock.Leave;
  end;
end;

procedure TPlayerAudioOutputWaveOut.CloseInternal;
var
  i: integer;
begin
  ClearInputBuffer;

  for i := 0 to FInputBufferItemPool.Count-1 do
    FInputBufferItemPool[i].Free;
  FInputBufferItemPool.Clear;

  if FWaveOutHandle<>0 then
  begin
    FWaveOutBuffersLock.Enter;
    try
      waveOutReset(FWaveOutHandle);
      for i:=0 to High(FWaveOutBuffers) do
      begin
        if FWaveOutBuffers[i].Buffer<>nil then
          GlobalFree(HGLOBAL(FWaveOutBuffers[i].Buffer));
        WaveOutUnprepareHeader(FWaveOutHandle,@FWaveOutBuffers[i].Header,sizeof(FWaveOutBuffers[i].Header));
        FWaveOutBuffers[i].Buffer:=nil;
        ZeroMemory(@FWaveOutBuffers[i].Header,sizeof(FWaveOutBuffers[i].Header));
      end;
      WaveOutClose(FWaveOutHandle);
      FWaveOutHandle:=0;
    finally
      FWaveOutBuffersLock.Leave;
    end;
  end;
end;

procedure TPlayerAudioOutputWaveOut.LoadWaveOutBuffer(aBufferIndex: cardinal);
var
  i: integer;
  aInputBufferItem: TPcmInputBufferItem;
begin
  FWaveOutBuffersLock.Enter;
  try
    //Если буфер еще занят - ничего не делаем
    if (FWaveOutBuffers[aBufferIndex].Playing) then
      exit;

    Assert(aBufferIndex<cardinal(Length(FWaveOutBuffers)));

    aInputBufferItem:=nil;
    FInputBufferLock.Enter;
    try
      //Если в очереди единственный элемент, то не будем его трогать пока он не наполниться под завязку. Иначе
      //мы можем "завязнуть" в частом переключении буферов драйвера
      if (FInputBuffer.Count=1) and (FInputBuffer.Peek.FActualDataSize<FWaveOutBuffers[0].BufferSize) then
        //Do nothing
      else if FInputBuffer.Count>0 then
        aInputBufferItem:=FInputBuffer.Dequeue
    finally
      FInputBufferLock.Leave;
    end;

    if aInputBufferItem<>nil then
    begin
      with FWaveOutBuffers[aBufferIndex] do
      begin
        Assert(aInputBufferItem.FActualDataSize<=BufferSize);

        Playing:=true;
        Header.dwFlags:=WHDR_PREPARED;
        Header.dwBufferLength:=aInputBufferItem.FActualDataSize;
        CopyMemory(Header.lpData,aInputBufferItem.FData,Header.dwBufferLength);
        i:=waveOutWrite(FWaveOutHandle,@Header,sizeof(Header));
        if i<>MMSYSERR_NOERROR then
          RaiseLastOSError;
      end;

      FInputBufferLock.Enter;
      try
        FInputBufferItemPool.Add(aInputBufferItem);
      finally
        FInputBufferLock.Leave;
      end;
    end;
  finally
    FWaveOutBuffersLock.Leave;
  end;
end;

constructor TPlayerAudioOutputWaveOut.Create;
begin
  inherited;
  FWindowHandle:=AllocateHWnd(WndProc);
  FInputBufferLock:=TCriticalSection.Create;
  FWaveOutBuffersLock:=TCriticalSection.Create;
  FInputBuffer := TInputBuffer.Create;
  FInputBufferItemPool:=TList<TPcmInputBufferItem>.Create;
  SetInitialBufferedDataDuration(80);
end;

procedure TPlayerAudioOutputWaveOut.DeallocateAudioContext;
begin
  CloseInternal;
end;

destructor TPlayerAudioOutputWaveOut.Destroy;
begin
  CloseInternal;
  inherited;

  FreeAndNil(FInputBuffer);
  FreeAndNil(FInputBufferItemPool);
  FreeAndNil(FInputBufferLock);
  FreeAndNil(FWaveOutBuffersLock);

  DeallocateHWnd(FWindowHandle);
end;

function TPlayerAudioOutputWaveOut.GetBufferedDataDuration: cardinal;
begin
  result:=GetInputBufferDuration+GetOutputBufferDuration;
end;

function TPlayerAudioOutputWaveOut.GetInputBufferDuration: cardinal;
var
  aIt: TEnumerator<TPcmInputBufferItem>;
begin
  result:=0;

  if FAvgBytesPerSec=0 then
    exit;

  FInputBufferLock.Enter;
  try
    aIt:=FInputBuffer.GetEnumerator;
    try
      while aIt.MoveNext do
        inc(result,aIt.Current.FActualDataSize);
    finally
      aIt.Free;
    end;
  finally
    FInputBufferLock.Leave;
  end;

  result:=int64(result)*1000 div FAvgBytesPerSec;
end;

function TPlayerAudioOutputWaveOut.GetOutputBufferDuration: cardinal;
var
  i: integer;
begin;
  result:=0;

  if FAvgBytesPerSec=0 then
    exit;

  FWaveOutBuffersLock.Enter;
  try
    for i := 0 to High(FWaveOutBuffers) do
    begin
      if FWaveOutBuffers[i].Playing then
        inc(result,FWaveOutBuffers[i].Header.dwBufferLength);
    end;
  finally
    FWaveOutBuffersLock.Leave;
  end;

  result:=int64(result)*1000 div FAvgBytesPerSec;
end;

function TPlayerAudioOutputWaveOut.GetVolume: TAudioVolume;
var
  aX: cardinal;
begin
  result:=FVolume;

  if FWaveOutHandle<>0 then
  begin
    if waveOutGetVolume(FWaveOutHandle,@aX)<>MMSYSERR_NOERROR then
      RaiseLastOSError;

    result:=Word(aX)*100 div $FFFF;
  end;
end;

procedure TPlayerAudioOutputWaveOut.OnInputBufferEmpty;
begin
  FInputBufferEmptyTriggered:=true;
  FAdaptedInitialBufferedDataDuration:=FAdaptedInitialBufferedDataDuration+100; //добавлям по 100 мс
  inc(FInputBufferEmptyEventCount);
end;

procedure TPlayerAudioOutputWaveOut.OnWaveOutBufferDone(aBuffer: PWaveHdr);
begin
  FWaveOutBuffersLock.Enter;
  try
    inc(FPlayedDataDuration,FWaveOutBuffers[aBuffer.dwUser].Header.dwBufferLength * 1000 div FAvgBytesPerSec);
    FLastPlayedDataTimeStamp:=GetTickCount;
    FWaveOutBuffers[aBuffer.dwUser].Playing:=false;

    if FInputBuffer.Count=0 then
      OnInputBufferEmpty
    else begin
      if FInputBufferEmptyTriggered then //Если есть щелчки - ничего не делаем пока не наберется достаточный буфер, иначе будет щелкать постоянно
        StartPcmDriverPlayIfPossible
      else begin
        LoadWaveOutBuffer(aBuffer.dwUser);
        if FInputBuffer.Count>0 then
        begin
          if FAdaptedInitialBufferedDataDuration>FInitialBufferedDataDuration then
            dec(FAdaptedInitialBufferedDataDuration);
        end;
      end;
    end;

  finally
    FWaveOutBuffersLock.Leave;
  end;
end;

function TPlayerAudioOutputWaveOut.Opened: boolean;
begin
  result:=FWaveOutHandle<>0;
end;


procedure waveOutProc(
  hwo: HWAVEOUT;
  uMsg: UINT;
  dwInstance: DWORD_PTR;
  dwParam1: DWORD_PTR;
  dwParam2: DWORD_PTR); stdcall;
begin
  if uMsg=WOM_DONE then
    TPlayerAudioOutputWaveOut(dwInstance).OnWaveOutBufferDone(PWaveHdr(dwParam1));
end;


procedure TPlayerAudioOutputWaveOut.OpenInternal(aAudioChannels: cardinal; aBitsPerSample: Cardinal; aSamplesPerSec: cardinal);
var
  aFormat:TWaveFormatEx;
  aWaveOutDevCaps: WAVEOUTCAPS;
  res: integer;
  aBuffSize: cardinal;

  i: integer;
begin
  ClearInputBuffer;
  res:=waveOutGetNumDevs;
  if res=0 then
    raise ENoSoundDeviceError.Create;

  res:=waveOutGetDevCaps(FAudioDeviceId,@aWaveOutDevCaps,sizeof(aWaveOutDevCaps));
  if(res<>MMSYSERR_NOERROR) then
     raise ENoSoundDeviceError.Create;

  aFormat.wFormatTag:=WAVE_FORMAT_PCM;
  aFormat.nChannels :=aAudioChannels; // один канал - МОНО, 2- СТЕРЕО
  aFormat.wBitsPerSample:=aBitsPerSample; //16;
  aFormat.nSamplesPerSec:=aSamplesPerSec;// 8000;
  aFormat.nBlockAlign:=(aFormat.wBitsPerSample div 8) * aFormat.nChannels;
  aFormat.nAvgBytesPerSec:=aFormat.nBlockAlign * aFormat.nSamplesPerSec;
  aFormat.cbSize:= 0;

  try
    Win32Check(waveOutOpen(@FWaveOutHandle,WAVE_MAPPER,@aFormat,FWindowHandle,dword(self),CALLBACK_WINDOW)=MMSYSERR_NOERROR);
  except
    on E:Exception do
      raise ENoSoundDeviceError.Create;

  end;

  //Нельзя использовать callback функцию потому что запрещено в callback догружать данные.
  //Win32Check(waveOutOpen(@FWaveOutHandle,WAVE_MAPPER,@aFormat,DWORD(@waveOutProc),dword(self),CALLBACK_FUNCTION)=MMSYSERR_NOERROR);

  FAvgBytesPerSec:=aFormat.nAvgBytesPerSec;
  aBuffSize:=(FAvgBytesPerSec*WaveOutDriverBufferSizeMsec) div 1000; //Держим буфер размером WaveOutDriverBufferSizeMsec

  for i:=0 to High(FWaveOutBuffers) do
  begin
    FWaveOutBuffers[i].Buffer:=pointer(GlobalAlloc(GMEM_FIXED or GMEM_NOCOMPACT or GMEM_NODISCARD, aBuffSize));
    if FWaveOutBuffers[i].Buffer=nil then
      RaiseLastOSError;
    FWaveOutBuffers[i].BufferSize:=aBuffSize;

    FWaveOutBuffers[i].Header.lpData := FWaveOutBuffers[i].Buffer;
    FWaveOutBuffers[i].Header.dwBufferLength := aBuffSize;
    FWaveOutBuffers[i].Header.dwFlags:=0;
    FWaveOutBuffers[i].Header.dwLoops:=0;
    FWaveOutBuffers[i].Header.dwUser:=i; //Номер буфера у нас
    Win32Check(waveOutPrepareHeader(FwaveOutHandle,@FWaveOutBuffers[i].Header,sizeof(FWaveOutBuffers[i].Header))=MMSYSERR_NOERROR);
    FWaveOutBuffers[i].Playing:=false;
  end;

  FAudioChannels:=aAudioChannels;
  FAudioBitsPerSample:=aBitsPerSample;
  FAudioSamplesPerSec:=aSamplesPerSec;
  FPlayedDataDuration:=0;
  FFirstPlayTimeStamp:=DWORD(-1);
  FLastPlayedDataTimeStamp:=DWORD(-1);
  FInputBufferEmptyEventCount:=0;
  FInputBufferEmptyTriggered:=true; //отмечаем, что начальный буфер пустой

  SetVolume(GetVolume); //Применим декларированное значение громкости к физическому устройству
end;


procedure TPlayerAudioOutputWaveOut.PlayPCMInternal(aPcmData: pointer;aPcmDataSize: cardinal);
var
  aInputBufferItem: TPcmInputBufferItem;
  x: integer;
begin
  Assert(aPcmDataSize<=FWaveOutBuffers[0].BufferSize);
  if aPcmDataSize=0 then
    exit;

  FInputBufferLock.Enter;
  try
    //Пытаемся забить под завязку последний элемент очереди
    //В основном, это необходимо для того, чтобы набрать первичный буфер в 80 мс, а это 2 элемента буфера)
    //TODO научиться доставать последний элемент. Пока достаем не последний, а первый на выход, работает только при Count=1
    if (FInputBuffer.Count<>0) and (FInputBuffer.FLastAdded<>nil) then
    begin
      aInputBufferItem:=FInputBuffer.FLastAdded;

      if (aInputBufferItem.FActualDataSize<FWaveOutBuffers[0].BufferSize) then
      begin
        x:=min(FWaveOutBuffers[0].BufferSize-aInputBufferItem.FActualDataSize,aPcmDataSize);
        aInputBufferItem.Append(aPcmData,x);
        PByte(aPcmData):=PByte(aPcmData)+x;
        Assert(x<=integer(aPcmDataSize));
        dec(aPcmDataSize,x);
      end;
    end;

    if (aPcmDataSize>0) then
    begin
      if FInputBufferItemPool.Count>0 then
      begin
        aInputBufferItem:=FInputBufferItemPool[FInputBufferItemPool.Count-1];
        FInputBufferItemPool.Delete(FInputBufferItemPool.Count-1);
      end
      else
        aInputBufferItem:=TPcmInputBufferItem.Create(FWaveOutBuffers[0].BufferSize);
    end
    else begin
      aInputBufferItem:=nil;
    end;
  finally
    FInputBufferLock.Leave;
  end;

  if aInputBufferItem<>nil then
  begin
    aInputBufferItem.Assign(aPcmData,aPcmDataSize);
    FInputBufferLock.Enter;
    try
      FInputBuffer.Enqueue(aInputBufferItem);
      //TODO здесь нужно добавить вырезание тишины, если мы "сильно" опаздываем
    finally
      FInputBufferLock.Leave;
    end;
  end;

  StartPcmDriverPlayIfPossible;

  x:=aPcmDataSize*1000 div FAvgBytesPerSec;
  if FSyncronousPlay then
    while (FInputBuffer.Count>1) do //and (GetInputBufferDuration>=MaxInputBufferDuration) do
    begin
      Sleep(1);
      dec(x);

      if x<-10 then
        break;

      if FWaveOutHandle=0 then
        break;
      if FInputBufferEmptyTriggered then
        break;
    end;
end;

procedure TPlayerAudioOutputWaveOut.PlayPCM(
        const aPcmFormat: TPCMFormat;
        aPcmData: pointer;aPcmDataSize: cardinal);
var
  aMaxAllowableSize: cardinal;
begin
  if Opened then
  begin
    if (FAudioChannels<>aPcmFormat.Channels) or
       (FAudioBitsPerSample<>aPcmFormat.BitsPerSample) or
       (FAudioSamplesPerSec<>aPcmFormat.SamplesPerSec) then
     CloseInternal;
  end;

  if not Opened then
  begin
    try
      OpenInternal(aPcmFormat.Channels,aPcmFormat.BitsPerSample,aPcmFormat.SamplesPerSec);
    except
      on E:ENoSoundDeviceError do
      begin
        FNoAudioDevice:=true;
        exit;
      end;
    end;
  end;


  if FFirstPlayTimeStamp=DWORD(-1) then
    FFirstPlayTimeStamp:=GetTickCount;

  while true do
  begin
    aMaxAllowableSize:=FWaveOutBuffers[0].BufferSize; //Так как все буферы одинаковой длины, то можно сверять с любым
    if (aPcmDataSize<=aMaxAllowableSize) then
    begin
      PlayPCMInternal(aPcmData,aPcmDataSize);
      break;
    end
    else begin
      PlayPCMInternal(aPcmData,aMaxAllowableSize);
      aPcmData:=PByte(aPcmData)+aMaxAllowableSize;
      Assert(aPcmDataSize>aMaxAllowableSize);
      dec(aPcmDataSize,aMaxAllowableSize);
    end;
  end;
end;

procedure TPlayerAudioOutputWaveOut.ResetBuffer;
var
  i: Integer;
begin
  FInputBufferLock.Enter;
  try
    for i := 0 to FInputBuffer.Count-1 do
      FInputBufferItemPool.Add(FInputBuffer.Dequeue);
  finally
    FInputBufferLock.Leave;
  end;
end;

procedure TPlayerAudioOutputWaveOut.SetInitialBufferedDataDuration(
  const Value: cardinal);
begin
  FInitialBufferedDataDuration := Value;
  FAdaptedInitialBufferedDataDuration:=Value;
end;

procedure TPlayerAudioOutputWaveOut.SetVolume(const Value: TAudioVolume);
var
  aX: cardinal;
begin
  FVolume:=Value;
  if FWaveOutHandle<>0 then
  begin
    aX:=Value*$FFFF div 100;
    waveOutSetVolume(FWaveOutHandle,MakeLong(aX,aX));
  end;
end;

procedure TPlayerAudioOutputWaveOut.StartPcmDriverPlayIfPossible;
var
  i: Integer;
  x: cardinal;
begin
  FWaveOutBuffersLock.Enter;
  try
    //Драйвер полностью разгружен
    if FInputBufferEmptyTriggered then
    begin
      x:=GetInputBufferDuration;

      Assert(FAdaptedInitialBufferedDataDuration>=FInitialBufferedDataDuration);
      //Только если в буфере есть как минимум 2 элемента, чтобы было чем заменять.
      //Итого мы всегда имеем задержку в 40 мс от настоящего времени
      if x>=FAdaptedInitialBufferedDataDuration then
      begin
        for i := 0 to High(FWaveOutBuffers)  do
          LoadWaveOutBuffer(i);
        FInputBufferEmptyTriggered:=false;
      end;
    end
    //Не все буферы драйвера задействованы

    else begin
      for i := 0 to High(FWaveOutBuffers)  do
        LoadWaveOutBuffer(i);
    end;
  finally
    FWaveOutBuffersLock.Leave;
  end;
end;

function TPlayerAudioOutputWaveOut.StatusInfo: string;
var
  x,y: int64;
begin
  result:=inherited StatusInfo;
  result:=result+  Format('    Input Buffer Length (ms): %d'#13#10,[GetInputBufferDuration]);
  result:=result+  Format('    Output Buffer Length (ms): %d'#13#10,[GetOutputBufferDuration]);
  result:=result+  Format('  Played Data Length (ms): %d'#13#10,[FPlayedDataDuration]);
  result:=result+  Format('  Played Time (ms): %d'#13#10,[FLastPlayedDataTimeStamp-FFirstPlayTimeStamp]);

  if FPlayedDataDuration>0 then
  begin
    x:=FPlayedDataDuration;
    y:=FLastPlayedDataTimeStamp-FFirstPlayTimeStamp;
    result:=result+  Format('  Synchronization Discrepancy (%%): %.1f'#13#10,[(x-y)*100/x]);
  end;
  result:=result+  Format('  Empty Input Buffer Events: %d'#13#10,[FInputBufferEmptyEventCount]);
end;

procedure TPlayerAudioOutputWaveOut.WndProc(var Message: TMessage);
begin
  if Message.Msg=WOM_DONE then
    OnWaveOutBufferDone(PWaveHdr(Message.LParam))
  else
    Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TPlayerAudioOutputWaveOut.WriteAudioData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if FNoAudioDevice then
    exit;

  inherited WriteAudioData(aFormat,aData,aDataSize,aInfo,aInfoSize);
end;


{ ENoSoundDeviceError }

constructor ENoSoundDeviceError.Create;
begin
  inherited Create('Не обнаружено входное Audio-устройство');
end;

{ TAudioOutputDecoder }

constructor TAudioOutputDecoder.Create;
begin

end;

function TAudioOutputDecoder.IsDataValid(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal): boolean;
begin
  result:=true;
end;

procedure TAudioOutputDecoder.ResetBuffer;
begin

end;

{ TInputBuffer }

procedure TInputBuffer.Notify(const Item: TPcmInputBufferItem;
  Action: TCollectionNotification);
begin
  inherited;
  case Action of
    cnAdded: FLastAdded:=Item;
    cnRemoved, cnExtracted:
      if FLastAdded=Item then
        FLastAdded:=nil;
  end;
end;

end.


