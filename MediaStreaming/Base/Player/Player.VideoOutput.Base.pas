{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Абстракция вывода видео-данных куда-то (например, на экран)   }
{                c предварительным декодированием                              }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        14.01.2011                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit Player.VideoOutput.Base;

interface
  uses Messages, Windows,  Graphics, SysUtils, Controls, Classes, SyncObjs, Contnrs,Generics.Collections,
       DirectDraw, DirectImage,MediaProcessing.Definitions, MediaStream.Frame;


//{$DEFINE FORCE_SYNC_DISPLAY}

type
  TSurface = class
  public
    function SurfaceFormat: TRGBFormat; virtual; abstract;
    function SurfaceBits: byte;

    procedure Prepare(const aSurfaceDesc : TSurfaceDesc); virtual;

    procedure DrawRGB15(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); virtual; abstract;
    procedure DrawRGB16(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); virtual; abstract;
    procedure DrawRGB24(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); virtual; abstract;
    procedure DrawRGB32(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); virtual; abstract;

    procedure DrawYUV420(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); virtual; abstract;
  end;

  TVideoOutputDecoderCurrentDecodedBufferAccess = (dbaNoSupported,dbaOK,dbaNoBuffer);
  TVideoOutputDecoderDecodeResult = (drWait, drError, drSuccess);

  TVideoOutputDecoderDecodeResultArray = array [TVideoOutputDecoderDecodeResult] of int64;


  TVideoOutputDecoder = class
  private
    FFrameStatistics: TVideoOutputDecoderDecodeResultArray;
    FImageHeight: cardinal;
    FImageWidth: cardinal;
    FOutputSurface: TSurface;
    FUseHardwareAcceleration: boolean;
    FLastError: string;


    procedure SetUseHardwareAcceleration(const Value: boolean);
  protected
    //procedure OnErrorFrame;
    procedure OnUseHardwareAccelerationChanged; virtual;
    procedure SetImageSize(aVideoWidth,aVideoHeight:cardinal);
    procedure SetLastError(const aError: string);

    function  DoDecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult; virtual; abstract;
  public
    function  DecodeData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):TVideoOutputDecoderDecodeResult;
    procedure CopyDecoderImageToSurface(aSurface: TSurface); virtual; abstract;

    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean; virtual;

    //
    procedure ResetBuffer; virtual;

    function GetCurrentDecodedBuffer(out aFormat: TMediaStreamDataHeader;
      out aData: pointer; out aDataSize: cardinal;
      out aInfo: pointer; out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess; virtual;

    //Параметры текущей декодированной картинки
    property  ImageWidth: cardinal read FImageWidth;
    property  ImageHeight: cardinal read FImageHeight;
    property  OutputSurface: TSurface read FOutputSurface write FOutputSurface;
    property  UseHardwareAcceleration: boolean read FUseHardwareAcceleration write SetUseHardwareAcceleration default false;
    property  FrameStatistics: TVideoOutputDecoderDecodeResultArray read FFrameStatistics;
    property  LastError: string read FLastError;

    class function SupportedStreamTypes: TArray<TStreamType>; virtual; abstract;

    function  StatusInfo: string; virtual;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TVideoOutputDecoderClass = class of TVideoOutputDecoder;


  //=========
  TPlayerVideoOutput = class;

  TPlayerVideoOutputBeforeDrawFrame = procedure (Sender: TPlayerVideoOutput; var aAccept: boolean) of object;
  TPlayerVideoOutputAfterDrawFrame = procedure (Sender: TPlayerVideoOutput) of object;
  TPlayerVideoOutputDrawFrame = procedure (Sender: TPlayerVideoOutput; aDC: HDC; aDCWidth,aDCHeight: cardinal) of object;
  TPlayerVideoOutputDrawCustomData = procedure (Sender: TPlayerVideoOutput; aDC: HDC; aDCWidth,aDCHeight: cardinal; aData: pointer) of object;
  TPlayerVideoOutputCopyDecodedDataToSurface = procedure (aSender: TPlayerVideoOutput; aSurface: TSurface; var aDefaultDrawing: boolean) of object;
  TPlayerVideoOutputImageSizeChanged = procedure (Sender: TPlayerVideoOutput) of object;
  TBuildFrameImageEvent = procedure (aSender: TPlayerVideoOutput; aDC: HDC; aDCWidth,aDCHeight: cardinal) of object;
  //TDrawFrameImageEvent = TBuildFrameImageEvent;
  TOutOfBufferEvent = procedure (aSender: TPlayerVideoOutput) of object;
  TImageSizeChangedEvent = procedure (aSender: TPlayerVideoOutput) of object;
  TFrameDecoded = procedure (aSender: TPlayerVideoOutput) of object;
  TFrameDecodeError = procedure (aSender: TPlayerVideoOutput; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal) of object;

  //{$DEFINE REUSE_FRAME} //Experiment
  TPlayerVideoOutputBuffer = class
  private
    FList: TObjectList<TMediaStreamFrame>;
    FLock : TCriticalSection;
    FDataPresentEvent: TEvent;

    //Один свободный фрейм
    FAddFrameCounter: cardinal;
    {$IFDEF REUSE_FRAME}
    FFreeFrame: TMediaStreamFrame;
    FReuseFrameCounter: cardinal;
    {$ENDIF}

    procedure OnItemsChanged;

    function GetItem(index: integer): TMediaStreamFrame; inline;
  public
    //Добавляет фрейм в список. И берет на себя управление жизненным циклом этого фрейма
    procedure AddFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); inline;

    //Извлекает из списка первый фрейм
    function  ExtractFirstFrame: TMediaStreamFrame; inline;
    //Извлекает из списка указанный фрейм
    function  ExtractFrame(index: integer): TMediaStreamFrame; inline;

    //Физически удаляет первый фрейм
    procedure DeleteFirstFrame; inline;
    //Физически удаляет указанный фрейм
    procedure DeleteFrame(index: integer); inline;

    //Осводождает память, выделенную под фрейм
    procedure DisposeFrame(aFrameData: TMediaStreamFrame);

    //Очищает список, физически удаляя все хранящиеся фреймы
    procedure Clear;

    function Count: integer; inline;
    //Длина буфера в мс
    function Duration: cardinal; inline;

    function Exists(aFlag: TFrameFlag): boolean;

    function WaitForData(aTimeout: cardinal):boolean;
    function WaitForAddedFrames(aHowManyFrames: cardinal; aTimeout: cardinal): boolean;

    procedure Lock;
    procedure TryLockOrRaise(aTimeout: cardinal);
    procedure Unlock;

    constructor Create;
    destructor Destroy; override;

    property Items[index: integer]: TMediaStreamFrame read GetItem; default;
  end;

  TPlayerVideoOutputDecoderFactory = class
  private
    FDecoderClasses: TList<TVideoOutputDecoderClass>;
  public
    procedure RegisterDecoderClass(aClass: TVideoOutputDecoderClass);
    procedure UnregisterDecoderClass(aClass: TVideoOutputDecoderClass);

    function  GetStreamTypeHandlerClass(aType: TStreamType): TVideoOutputDecoderClass; virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  TPlayerVideoOutput = class
  private
    FDecoder    : TVideoOutputDecoder;
    FStreamType : TStreamType;
    FServiceWndHandle  : THandle;

    FSynchronousDisplay: boolean;
    FWaitOnOutOfBuffer: boolean;
    FBackgroundColor: TColor;

    FWindowHandleLock: TCriticalSection;
    FWindowHandle: HWND;

    FVideoWidth: cardinal;
    FVideoHeight: cardinal;
    FFramesReceived: cardinal;
    FFramesPrebufferReceived: cardinal;
    FFramesDecoded: cardinal;
    FFirstFrameDecoded: integer;
    FLastTimeStamp: int64;
    FLastThreadId: Cardinal;

    FOnDrawFrameImage: TPlayerVideoOutputDrawFrame;
    FOnImageSizeChanged: TPlayerVideoOutputImageSizeChanged;
    FOnBeforeDrawFrameImage: TPlayerVideoOutputBeforeDrawFrame;
    FOnBuildFrameImage: TBuildFrameImageEvent;
    FOnOutOfBuffer: TOutOfBufferEvent;

    FBuffer       : TPlayerVideoOutputBuffer;
    FBufferMaxSize: integer;
    FBufferWaitForKeyFrame: boolean;

    FFrameDisplayedCount : int64;
    FDisplayThread : TThread;
    FDisplayThreadLock : TCriticalSection;
    FLastUnspportedStreamType: TStreamType;
    FOnFrameDecoded: TFrameDecoded;
    FOnAfterDrawFrameImage: TPlayerVideoOutputAfterDrawFrame;
    FOnCopyDecodedDataToSurface: TPlayerVideoOutputCopyDecodedDataToSurface;
    FUseHardwareAcceleration: boolean;
    FOnFrameDecodeError: TFrameDecodeError;
    FAccumulationPeriodMs: integer;

    procedure OnDisplayThread;
    procedure WndProc (var Message : TMessage);

    function GetSynchronousDisplay: boolean;
    procedure SetSynchronousDisplay(const Value: boolean);
    procedure SetWaitOnOutOfBuffer(const Value: boolean);
    procedure SetOnImageSizeChanged(const Value: TPlayerVideoOutputImageSizeChanged);
    procedure SetOnBeforeDrawFrameImage(const Value: TPlayerVideoOutputBeforeDrawFrame);
    procedure SetOnOutOfBuffer(const Value: TOutOfBufferEvent);
    procedure SetMaxBufferSize(const Value: integer);
    procedure SetOnBuildFrameImage(const Value: TBuildFrameImageEvent);
    procedure SetOnAfterDrawFrameImage(const Value: TPlayerVideoOutputAfterDrawFrame);
    procedure SetOnCopyDecodedDataToSurface(const Value: TPlayerVideoOutputCopyDecodedDataToSurface);

    procedure TraceLine(const aMessage: string);
    procedure SetUseHardwareAcceleration(const Value: boolean);
  protected
    FEventsLock : TCriticalSection;

    procedure SetWindowHandle(const Value: HWND); virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetOnDrawFrameImage(const Value: TPlayerVideoOutputDrawFrame); virtual;

    procedure DecodeAndDisplayFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); virtual;
    procedure DrawCurrentDecodedImage; virtual; abstract;

    procedure SetStreamType(const Value: TStreamType);
    function  GetStreamTypeHandlerClass(aType: TStreamType): TVideoOutputDecoderClass; virtual;

    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean;
  public
    property  WindowHandle: HWND read FWindowHandle write SetWindowHandle;
    property  BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;

    property  VideoWidth: cardinal read FVideoWidth;
    property  VideoHeight: cardinal read FVideoHeight;

    property  StreamType: TStreamType read FStreamType write SetStreamType;

    procedure UpdateBounds; virtual; abstract;
    procedure UpdateImage; virtual; abstract;

    procedure SetVideoSize(aVideoWidth,aVideoHeight:cardinal); virtual;

    function  VideoContextAllocated: boolean; virtual; abstract;
    procedure ClearScreen; virtual; abstract;
    //Для асинхронного режима отображения: очистить буфер входных фреймов
    procedure ResetBuffer; virtual;
    procedure DeallocateVideoContext; virtual;

    procedure LockWindowHandle; virtual;
    procedure UnlockWindowhandle; virtual;

    procedure WriteVideoData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); virtual;

    procedure CaptureCurrentImageToBitmap(aBitmap:TBitmap); virtual; abstract;
    procedure CaptureCurrentImageToStreamAsBitmap(aStream: TStream); virtual; abstract;

    function  StatusInfo:string; virtual;

    property Decoder: TVideoOutputDecoder read FDecoder;
    property CurrentStreamType: TStreamType read FStreamType write SetStreamType;

    //Использование аппаратного ускорения при декодировани
    property  UseHardwareAcceleration: boolean read FUseHardwareAcceleration write SetUseHardwareAcceleration default false;

    property FramesReceivedToDecoding: cardinal read FFramesReceived;
    property FramesDecoded: cardinal read FFramesDecoded;
    property FirstDecodedFrame: integer read FFirstFrameDecoded;

    //Синхронный или асинхронный режим вывода
    property SynchronousDisplay: boolean read GetSynchronousDisplay write SetSynchronousDisplay default false;
    //Размер аккумуляционного буфера: сколько кадров должно быть в буфере, чтобы избежать "дерганий" при "просадках" источника
    //Не совместим с режимом SynchronousDisplay=true (не будет работать)!
    property AccumulationPeriodMs: integer read FAccumulationPeriodMs write FAccumulationPeriodMs default 0;

    function IsOutOfBuffer: boolean; inline;
    //---- Блок настроек Асинхронного отображения

    //Если данные помещаются в буфер быстрее, чем их проигрывает драйвер, то чтобы предотвратить
    //переписывание еще не считанных данных, нужно установить WaitOnOutOfBuffer = true
    //По умолчанию стоит false, так как это экономичнее по отношению к ресурсам
    property WaitOnOutOfBuffer:boolean read FWaitOnOutOfBuffer write SetWaitOnOutOfBuffer default true;

    //Количество кадров, помещаемое в асинхронный буфер. Если буфер переполнится, то
    //дальнейшее поведение зависит от WaitOnOutOfBuffer
    property  MaxBufferSize: integer read FBufferMaxSize write SetMaxBufferSize;

    //В случае несихронного проигрывания данных используется промежуточный буфер для проигрывания. И этот буфер может переполниться
    //Вызывается не в основном потоке!
    property OnOutOfBuffer: TOutOfBufferEvent read FOnOutOfBuffer write SetOnOutOfBuffer;

    //---- Конец блока настроек асинхронного отображения


    property  OnFrameDecodeError: TFrameDecodeError read FOnFrameDecodeError write FOnFrameDecodeError;

    //1. Возникает сразу после декодирования
    property  OnFrameDecoded: TFrameDecoded read FOnFrameDecoded write FOnFrameDecoded;
    //2. Перед началом рисования кадра
    property  OnBeforeDrawFrameImage: TPlayerVideoOutputBeforeDrawFrame read FOnBeforeDrawFrameImage write SetOnBeforeDrawFrameImage;
    //3. Копирование буфера из декодера на поверхность
    property  OnCopyDecodedDataToSurface: TPlayerVideoOutputCopyDecodedDataToSurface read FOnCopyDecodedDataToSurface write SetOnCopyDecodedDataToSurface;
    //4. В процессе формирования из кадра буфера, который в дальнейшем используется при прорисовке. Передается DC буфера и его размеры, которые равны размерам кадра
    property  OnBuildFrameImage: TBuildFrameImageEvent read FOnBuildFrameImage write SetOnBuildFrameImage;
    //5. В процессе перерисовки картинки на экране (аналог OnPaint). Возвращается DC канваса для рисования и его размеры. При этом на канвасе уже нарисован видеокадр
    property  OnDrawFrameImage: TPlayerVideoOutputDrawFrame read FOnDrawFrameImage write SetOnDrawFrameImage;
    //6. После прорисовки кадра
    property  OnAfterDrawFrameImage:TPlayerVideoOutputAfterDrawFrame  read FOnAfterDrawFrameImage write SetOnAfterDrawFrameImage;


    property  OnImageSizeChanged: TPlayerVideoOutputImageSizeChanged read FOnImageSizeChanged write SetOnImageSizeChanged;


    constructor Create(aWindowHandle: HWND); virtual;
    destructor Destroy; override;
  end;
  TPlayerVideoOutputClass  = class of TPlayerVideoOutput;


  TPlayerVideoOutputSurface = class;
  TSurfaceDirectX = class (TSurface)
  private
    FOwner:TPlayerVideoOutputSurface;
    FSurfaceDesc : TSurfaceDesc;

    procedure Init(aOwner:TPlayerVideoOutputSurface);
  public
    function SurfaceFormat: TRGBFormat; override;

    procedure Prepare(const aSurfaceDesc : TSurfaceDesc); override;

    procedure DrawRGB15(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB16(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB24(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB32(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;

    procedure DrawYUV420(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
  end;

  //Одна из разновидностей поверхности - буфер для хранения RGB
  TSurfaceRGB = class (TSurface)
  private
    FSurfaceWidth: integer;
    FSurfaceHeight: integer;
    FSurfaceFormat: TRGBFormat;

    FDIB: TBytes;
  public
    constructor Create(aSurfaceFormat: TRGBFormat);

    procedure Prepare(const aSurfaceDesc : TSurfaceDesc); override;
    function SurfaceFormat: TRGBFormat; override;

    procedure SetSize(aWidth,aHeight: integer);

    procedure DrawRGB15(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB16(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB24(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;
    procedure DrawRGB32(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;

    procedure DrawYUV420(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean=false); override;

    property SurfaceWidth: integer read FSurfaceWidth;
    property SurfaceHeight: integer read FSurfaceHeight;
    property DIB: TBytes read FDIB;

    procedure CopyToBitmap(aBitmap: TBitmap);
  end;

  TPlayerVideoOutputSurface = class (TPlayerVideoOutput)
  private
    FDrawer  : TDirectImageRGB;
    FDrawerLock : TCriticalSection;
    FSurface: TSurface;

    procedure DestroyDrawer;
    function  Drawer: TDirectImageRGB;

    procedure LockDrawer;
    procedure UnlockDrawer;

    procedure OnDrawImageOnScreen(const aSurface: IDirectDrawSurface7; const aRect:TRect);
  protected
    procedure SetBackgroundColor(const Value: TColor); override;
    procedure SetOnDrawFrameImage(const Value: TPlayerVideoOutputDrawFrame); override;

    procedure DecodeAndDisplayFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal); override;
    procedure DrawCurrentDecodedImage; override;

    function CreateSurface: TSurface; virtual; abstract;

    property Surface: TSurface read FSurface;
  public
    procedure UpdateBounds; override;
    procedure UpdateImage; override;

    function  VideoContextAllocated: boolean; override;
    procedure ClearScreen; override;
    procedure DeallocateVideoContext; override;

    procedure LockWindowHandle; override;
    procedure UnlockWindowhandle; override;

    function  GetSurfaceDC: HDC;
    procedure ReleaseDC(aDC: HDC; aRedraw: boolean);

    procedure SetVideoSize(aVideoWidth,aVideoHeight:cardinal); override;

    //Рисует на экране образ, переданный как RGB24
    procedure DrawCustom(aData: pointer; aCallback: TPlayerVideoOutputDrawCustomData);
    procedure DrawRGB(aDIB: pointer; aSize: cardinal; aWidth,aHeight: integer; aRGBFormat: TRGBFormat; aReverseVertical: boolean);
    procedure DrawBitmap(aBitmap: TBitmap; aReverseVertical: boolean=true);

    //Текущее содержимое экрана возвращает как RGB24
    procedure CaptureCurrentImageToBitmap(aBitmap:TBitmap); override;
    procedure CaptureCurrentImageToStreamAsBitmap(aStream: TStream); override;

    function  StatusInfo:string; override;

    constructor Create(aWindowHandle: HWND); override;
    destructor Destroy; override;
  end;


  TPlayerVideoOutputDirectX = class (TPlayerVideoOutputSurface)
  protected
    function CreateSurface: TSurface; override;
  end;


  TPlayerVideoOutput_AllTypes = TPlayerVideoOutputDirectX deprecated;

  TPlayerVideoOutputRgb24Buffer = class (TPlayerVideoOutputSurface)
    function CreateSurface: TSurface; override;
  end;

  function PlayerVideoOutputDecoderFactory: TPlayerVideoOutputDecoderFactory;

implementation
  uses BitPlane,BitmapStreamMediator, ThreadNames,uBaseClasses,uTrace;

const
   WM_SETSTREAMTTYPE = WM_USER+$100;

type
  TVideoOutputDisplayThread = class (TThread)
  private
    FOwner : TPlayerVideoOutput;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TPlayerVideoOutput);
    destructor Destroy; override;
  end;

var
  gPlayerVideoOutputDecoderFactory: TPlayerVideoOutputDecoderFactory;

function PlayerVideoOutputDecoderFactory: TPlayerVideoOutputDecoderFactory;
begin
  if gPlayerVideoOutputDecoderFactory=nil then //TODO Thread safe!
    gPlayerVideoOutputDecoderFactory:=TPlayerVideoOutputDecoderFactory.Create;

  result:=gPlayerVideoOutputDecoderFactory;
end;

{ TPlayerVideoOutputBuffer }

procedure TPlayerVideoOutputBuffer.AddFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  FLock.Enter;
  try
    {$IFDEF REUSE_FRAME}
    //Пытаемся использовать ранее овобожденный фрейм
    if (FFreeFrame<>nil) and (Length(FFreeFrame.FData)>=integer(aDataSize)) then
    begin
      FFreeFrame.Assign(aFormat,aData,aDataSize,aInfo,aInfoSize);
      FList.Add(FFreeFrame);
      FFreeFrame:=nil;
      inc(FReuseFrameCounter);
    end
    else begin
      FList.Add(TMediaStreamFrame.Create(aFormat,aData,aDataSize,aInfo,aInfoSize));
    end;
    {$ELSE}
      FList.Add(TMediaStreamFrame.Create(aFormat,aData,aDataSize,aInfo,aInfoSize));
    {$ENDIF}

    inc(FAddFrameCounter);
    OnItemsChanged;
  finally
    FLock.Leave;
  end;
end;

procedure TPlayerVideoOutputBuffer.Clear;
begin
  FLock.Enter;
  try
    FList.Clear;
    OnItemsChanged;
  finally
    FLock.Leave;
  end;
end;

function TPlayerVideoOutputBuffer.Exists(aFlag: TFrameFlag): boolean;
var
  i: Integer;
begin
  Lock;
  try
    result:=false;
    for i := 0 to Count-1 do
      if aFlag in Items[i].Format.biFrameFlags then
        exit(true);
  finally
    Unlock;
  end;
end;

function TPlayerVideoOutputBuffer.Count: integer;
begin
  result:=FList.Count;
end;

constructor TPlayerVideoOutputBuffer.Create;
begin
  FList:=TObjectList<TMediaStreamFrame>.Create;
  FLock:=TCriticalSection.Create;
  FDataPresentEvent:=TEvent.Create(nil,true,false,'');
end;

procedure TPlayerVideoOutputBuffer.DeleteFirstFrame;
begin
  FLock.Enter;
  try
    DisposeFrame(ExtractFirstFrame);
  finally
    FLock.Leave;
  end;
end;

procedure TPlayerVideoOutputBuffer.DeleteFrame(index: integer);
begin
  FLock.Enter;
  try
    DisposeFrame(ExtractFrame(index));
  finally
    FLock.Leave;
  end;
end;

destructor TPlayerVideoOutputBuffer.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  FreeAndNil(FLock);
  FreeAndNil(FDataPresentEvent);
  {$IFDEF REUSE_FRAME}
  FreeAndNil(FFreeFrame);
  {$ENDIF}
  inherited;
end;

procedure TPlayerVideoOutputBuffer.DisposeFrame(aFrameData: TMediaStreamFrame);
begin
  if aFrameData=nil then
    exit;

  {$IFDEF REUSE_FRAME}
  FLock.Enter;
  try
    if FFreeFrame=nil then
      FFreeFrame:=aFrameData
    else if Length(FFreeFrame.FData)<Length(aFrameData.FData) then
    begin
      FreeAndNil(FFreeFrame);
      FFreeFrame:=aFrameData;
    end
    else
      aFrameData.Free;
  finally
    FLock.Leave;
  end;
  {$ELSE}
    aFrameData.Free;
  {$ENDIF}
end;

function TPlayerVideoOutputBuffer.Duration: cardinal;
begin
  FLock.Enter;
  try
    if Count<1 then
      result:=0
    else if Count<2 then
      result:=40
    else
      result:=GetItem(Count-1).Format.TimeStampMs-GetItem(0).Format.TimeStampMs+40;
  finally
    FLock.Leave;
  end;
end;

function TPlayerVideoOutputBuffer.ExtractFirstFrame: TMediaStreamFrame;
begin
  result:=ExtractFrame(0);
end;


function TPlayerVideoOutputBuffer.ExtractFrame(index: integer): TMediaStreamFrame;
begin
  FLock.Enter;
  try
    FList.OwnsObjects:=false;
    try
      result:=FList[index];
      FList.Delete(index);
    finally
      FList.OwnsObjects:=true;
    end;

    OnItemsChanged;
  finally
    FLock.Leave;
  end;
end;

function TPlayerVideoOutputBuffer.GetItem(index: integer): TMediaStreamFrame;
begin
  result:=FList[index];
end;

procedure TPlayerVideoOutputBuffer.Lock;
begin
  FLock.Enter;
end;

procedure TPlayerVideoOutputBuffer.OnItemsChanged;
begin
  if Count>0 then
    FDataPresentEvent.SetEvent
  else
    FDataPresentEvent.ResetEvent;
end;

procedure TPlayerVideoOutputBuffer.TryLockOrRaise(aTimeout: cardinal);
begin
  FLock.TryEnterOrRaise(aTimeout);
end;

procedure TPlayerVideoOutputBuffer.Unlock;
begin
  FLock.Leave;
end;

function TPlayerVideoOutputBuffer.WaitForAddedFrames(aHowManyFrames,aTimeout: cardinal): boolean;
var
  aStart,aStop: int64;
  aInitialCount: integer;
begin
  aInitialCount:=self.Count;
  aStart:=GetTickCount;
  result:=false;

  while true do
  begin
    if self.Count-aInitialCount>=integer(aHowManyFrames) then
      exit(true);

    aStop:=GetTickCount;
    if aStop-aStart>int64(aTimeout) then
      break;
  end;
end;

function TPlayerVideoOutputBuffer.WaitForData(aTimeout: cardinal): boolean;
begin
  result:=FDataPresentEvent.WaitFor(aTimeout)=wrSignaled;
end;

{ TVideoOutputDecoder }

constructor TVideoOutputDecoder.Create;
begin
  inherited Create;
end;

function TVideoOutputDecoder.DecodeData(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal): TVideoOutputDecoderDecodeResult;
begin
  FLastError:='';
  result:=DoDecodeData(aFormat,aData,aDataSize,aInfo,aInfoSize);
  inc(FFrameStatistics[result]);
end;

destructor TVideoOutputDecoder.Destroy;
begin
  inherited;
end;


function TVideoOutputDecoder.GetCurrentDecodedBuffer(
  out aFormat: TMediaStreamDataHeader; out aData: pointer;
  out aDataSize: cardinal;
  out aInfo: pointer;
  out aInfoSize: cardinal): TVideoOutputDecoderCurrentDecodedBufferAccess;
begin
  result:=dbaNoSupported;
end;

procedure TVideoOutputDecoder.SetImageSize(aVideoWidth, aVideoHeight: cardinal);
begin
  FImageWidth:=aVideoWidth;
  FImageHeight:=aVideoHeight;
end;

procedure TVideoOutputDecoder.SetLastError(const aError: string);
begin
  FLastError:=aError;
end;

procedure TVideoOutputDecoder.SetUseHardwareAcceleration(const Value: boolean);
begin
  if FUseHardwareAcceleration<>value then
  begin
    FUseHardwareAcceleration := Value;
    OnUseHardwareAccelerationChanged;
  end;
end;

function TVideoOutputDecoder.StatusInfo: string;
begin
  result:=Format('  WaitFrameCount: %d; ErrorFrameCount:%d; SuccessFrameCount: %d'#13#10,[FFrameStatistics[drWait],FFrameStatistics[drError],FFrameStatistics[drSuccess]]);
end;


procedure TVideoOutputDecoder.ResetBuffer;
begin
end;

function TVideoOutputDecoder.IsDataValid(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal):boolean;
begin
  result:=(aData<>nil) and (aDataSize>0) and (aFormat.biMediaType=mtVideo) and (aFormat.VideoWidth>0) and (aFormat.VideoHeight>0);
end;

(*
procedure TVideoOutputDecoder.OnErrorFrame;
begin
  inc(FFrameErrorCount);
end;
*)


procedure TVideoOutputDecoder.OnUseHardwareAccelerationChanged;
begin

end;

constructor TPlayerVideoOutputDecoderFactory.Create;
begin
  FDecoderClasses:=TList<TVideoOutputDecoderClass>.Create;
end;

destructor TPlayerVideoOutputDecoderFactory.Destroy;
begin
  FreeAndNil(FDecoderClasses);
  inherited;
end;

function TPlayerVideoOutputDecoderFactory.GetStreamTypeHandlerClass(aType: TStreamType): TVideoOutputDecoderClass;
var
  I: Integer;
  aTypes: TArray<TStreamType>;
  j: Integer;
begin
  result:=nil;

  for i := 0 to FDecoderClasses.Count-1 do
  begin
    aTypes:=FDecoderClasses[i].SupportedStreamTypes;
    for j := 0 to High(aTypes) do
      if aTypes[j]=aType then
        exit(FDecoderClasses[i]);
  end;
end;

procedure TPlayerVideoOutputDecoderFactory.RegisterDecoderClass(aClass: TVideoOutputDecoderClass);
begin
  FDecoderClasses.Add(aClass);
end;


procedure TPlayerVideoOutputDecoderFactory.UnregisterDecoderClass(
  aClass: TVideoOutputDecoderClass);
begin
  FDecoderClasses.Remove(aClass);
end;

{ TVideoOutputDisplayThread }

constructor TVideoOutputDisplayThread.Create(aOwner: TPlayerVideoOutput);
begin
  FOwner:=aOwner;
  inherited Create(false);
end;

destructor TVideoOutputDisplayThread.Destroy;
const
  aMethodName = 'TVideoOutputDisplayThread.Destroy';
var
  aTraceId: cardinal;
begin
  aTraceId:=TraceProcBegin(aMethodName);
  try
    inherited;
    FOwner:=nil;
  finally
    TraceProcEnd(aMethodName,aTraceId);
  end;
end;

procedure TVideoOutputDisplayThread.Execute;
begin
  SetCurrentThreadName(ClassName+':'+FOwner.ClassName);
  while not Terminated do
  begin
    try
      FOwner.OnDisplayThread;
    except
      on E:Exception do
        TraceLine('Ошибка в TVideoOutputDisplayThread.Execute: '+E.Message);
    end;
  end;
end;

{ TPlayerVideoOutput }

constructor TPlayerVideoOutput.Create(aWindowHandle: HWND);
begin
  inherited Create;

  RegisterCustomTrace(ClassName,'','.pvo');

  FStreamType:=INVALID_HANDLE_VALUE;
  FWindowHandleLock := TCriticalSection.Create;;

  FBuffer:= TPlayerVideoOutputBuffer.Create;
  FBufferMaxSize:=30;
  FFirstFrameDecoded:=-1;
  FEventsLock:=TCriticalSection.Create;

  FDisplayThreadLock:=TCriticalSection.Create;

  FServiceWndHandle := AllocateHWnd(WndProc);
  Assert(FServiceWndHandle<>0);

  WaitOnOutOfBuffer:=false;
  BackgroundColor:=clBlack;
  WindowHandle:=aWindowHandle;
end;

destructor TPlayerVideoOutput.Destroy;
begin
  if FDisplayThread<>nil then
    FDisplayThread.TerminateInTime(5000);

  FreeAndNil(FDisplayThread);
  inherited;

  DeallocateHWnd(FServiceWndHandle);
  FServiceWndHandle:=0;

  FreeAndNil(FWindowHandleLock);
  FreeAndNil(FBuffer);
  FreeAndNil(FDecoder);
  FreeAndNil(FEventsLock);
  FreeAndNil(FDisplayThreadLock);
end;

function TPlayerVideoOutput.GetStreamTypeHandlerClass(aType: TStreamType): TVideoOutputDecoderClass;
begin
  result:=PlayerVideoOutputDecoderFactory.GetStreamTypeHandlerClass(aType);
end;

function TPlayerVideoOutput.GetSynchronousDisplay: boolean;
begin
  result:= {$IFDEF FORCE_SYNC_DISPLAY} true {$ELSE}FSynchronousDisplay {$ENDIF};
end;

function TPlayerVideoOutput.IsDataValid(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal): boolean;
begin
  result:=(aFormat.biMediaType=mtVideo) and (aData<>nil) and (aDataSize<>0);

  if result then
    if FDecoder<>nil then
      result:=FDecoder.IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize);
end;

function TPlayerVideoOutput.IsOutOfBuffer: boolean;
begin
  result:=FBuffer.Count>=FBufferMaxSize+(FAccumulationPeriodMs div 40);
end;

procedure TPlayerVideoOutput.LockWindowHandle;
begin
  Assert(self<>nil);
  Assert(FWindowHandleLock<>nil);
  FWindowHandleLock.Enter;
end;

procedure TPlayerVideoOutput.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TPlayerVideoOutput.SetOnAfterDrawFrameImage(
  const Value: TPlayerVideoOutputAfterDrawFrame);
begin
  FEventsLock.Enter;
  try
    FOnAfterDrawFrameImage := Value;
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.SetOnBeforeDrawFrameImage(
  const Value: TPlayerVideoOutputBeforeDrawFrame);
begin
  FEventsLock.Enter;
  try
    FOnBeforeDrawFrameImage := Value;
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.SetOnBuildFrameImage(
  const Value: TBuildFrameImageEvent);
begin
  FEventsLock.Enter;
  try
    FOnBuildFrameImage := Value;
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.SetOnCopyDecodedDataToSurface(
  const Value: TPlayerVideoOutputCopyDecodedDataToSurface);
begin
  FOnCopyDecodedDataToSurface := Value;
end;

procedure TPlayerVideoOutput.SetOnDrawFrameImage(
  const Value: TPlayerVideoOutputDrawFrame);
begin
  FOnDrawFrameImage := Value;
end;

procedure TPlayerVideoOutput.SetOnImageSizeChanged(
  const Value: TPlayerVideoOutputImageSizeChanged);
begin
  FOnImageSizeChanged := Value;
end;

procedure TPlayerVideoOutput.SetStreamType(const Value: TStreamType);
var
  aClass: TVideoOutputDecoderClass;
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
  FDecoder.UseHardwareAcceleration:=FUseHardwareAcceleration;
  FStreamType:=Value;
end;

procedure TPlayerVideoOutput.SetSynchronousDisplay(const Value: boolean);
begin
  FSynchronousDisplay := Value;
end;

procedure TPlayerVideoOutput.SetUseHardwareAcceleration(const Value: boolean);
begin
  FUseHardwareAcceleration := Value;
  if FDecoder<>nil then
    FDecoder.UseHardwareAcceleration:=FUseHardwareAcceleration;
end;

procedure TPlayerVideoOutput.SetVideoSize(aVideoWidth, aVideoHeight: cardinal);
begin
  FVideoWidth:=aVideoWidth;
  FVideoHeight:=aVideoHeight;

  if Assigned(FOnImageSizeChanged) then
    FOnImageSizeChanged(self);
end;

procedure TPlayerVideoOutput.SetWaitOnOutOfBuffer(const Value: boolean);
begin
  FWaitOnOutOfBuffer := Value;
end;

procedure TPlayerVideoOutput.SetWindowHandle(const Value: HWND);
begin
  if FWindowHandle=Value then
    exit;

  //FIX блокируем изменение переменной потока до тех пор, пока не сменим окно, иначе проскакивает новое создание потока
  FDisplayThreadLock.Enter;
  try
    DeallocateVideoContext;

    LockWindowHandle;
    try
      FWindowHandle := Value;
    finally
      UnlockWindowhandle;
    end;

  finally
    FDisplayThreadLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.SetMaxBufferSize(const Value: integer);
begin
  FBufferMaxSize:=Value;
end;

function TPlayerVideoOutput.StatusInfo: string;
begin
  result:='General'+#13#10;
  result:=result+  Format('  Type:%s'#13#10,[ClassName]);
  if FDecoder<>nil then
   result:=result+  Format('  Decoder Type:%s'#13#10,[FDecoder.ClassName]);


  result:=result+'Settings: '#13#10;
  result:=result+  Format('  SynchronousDisplay:%s'#13#10,[BoolToStr(SynchronousDisplay,true)]);
  result:=result+  Format('  WaitOnOutOfBuffer:%s'#13#10,[BoolToStr(WaitOnOutOfBuffer,true)]);

  result:=result+'Statistics: '#13#10;
  result:=result+  Format('  Frames Received:%d'#13#10,[FFramesReceived]);
  result:=result+  Format('  Frames Received/Prebuffer:%d'#13#10,[FFramesPrebufferReceived]);
  result:=result+  Format('  Frames Decoded:%d'#13#10,[FFramesDecoded]);
  result:=result+  Format('  First Decoded Frame:%d'#13#10,[FFirstFrameDecoded]);
  result:=result+  Format('  Frames Displayed:%d'#13#10,[FFrameDisplayedCount]);


  result:=result+  Format('  Buffer.Count:%d'#13#10,[FBuffer.Count]);
  result:=result+  Format('  Buffer.Duration:%d'#13#10,[FBuffer.Duration]);

  if FDecoder<>nil then
    result:=result+FDecoder.StatusInfo
  else
    result:=result+'No decoder'#13#10;
end;

procedure TPlayerVideoOutput.TraceLine(const aMessage: string);
begin
  uTrace.TraceLine(ClassName,aMessage);
end;

procedure TPlayerVideoOutput.UnlockWindowhandle;
begin
  FWindowHandleLock.Leave;
end;

procedure TPlayerVideoOutput.WriteVideoData(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal);
var
  b: boolean;
  aSendMessageResult: DWORD_PTR;
  i,j: Integer;
begin
  Assert(aFormat.biMediaType=mtVideo);

  if FStreamType<>aFormat.biStreamType then
  begin
    if FLastUnspportedStreamType=aFormat.biStreamType then //В случае если формат не поддерживается, не надо генерировать постоянно исключения. Будем тихо выходить
      exit;

    //Защищаемся от локов, используем сообщение с таймаутом
    if (SendMessageTimeout(FServiceWndHandle,WM_SETSTREAMTTYPE,aFormat.biStreamType,0,SMTO_BLOCK,1000,@aSendMessageResult)=0) then
      exit;
  end;

  if FDecoder=nil then
    exit;

  if not IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize) then
    exit;

  if aDataSize=0 then
    exit;

  //Синхронное отображение
  if SynchronousDisplay then
  begin
    DecodeAndDisplayFrame(aFormat,aData,aDataSize,aInfo,aInfoSize);
    inc(FFrameDisplayedCount);
  end
  //Асинхронное отображение. Сначала копируется в очередь, а потом из отдельного потока на экран
  else begin
    FDisplayThreadLock.Enter;
    try
      if FDisplayThread=nil then
      begin
        FDisplayThread:=TVideoOutputDisplayThread.Create(self);
        FDisplayThread.Priority:=tpHighest;
      end;

    finally
      FDisplayThreadLock.Leave;
    end;

    //Проверяем переполнение буфера
    if FWaitOnOutOfBuffer then
    begin
      //Ждем пока буфер не освободится
      while true do
      begin
        FBuffer.Lock;
        try
          b:=not IsOutOfBuffer;
        finally
          FBuffer.Unlock;
        end;

        if b then
          break;

        Sleep(40);
      end;
    end;

    b:=true;
    FBuffer.Lock;
    try
      //В буфере еще есть место
      if (not IsOutOfBuffer) or
         (ffPrebuffer in aFormat.biFrameFlags) or //Специальный режим, когда нам резко насовывают данные, одним большим массивом. В этом случае мы должны забирать все что есть
         (FBuffer.Exists(ffPrebuffer) and (FFirstFrameDecoded=-1)) //Если еще не доработан предбуфер и декодер еще не получил полной картины, то удалять нельзя
         then
      begin
        if FBuffer.Count>(FBufferMaxSize+(FAccumulationPeriodMs div 40))*4 then
          b:=false //Защита: если буфер уже сильно переполнился, то просто будем игнорировать данные
        else if (ffKeyFrame in aFormat.biFrameFlags) or not FBufferWaitForKeyFrame then
        begin
          FBuffer.AddFrame(aFormat, aData,aDataSize,aInfo,aInfoSize);
          FBufferWaitForKeyFrame:=false;
        end
        else begin
          b:=false;
        end;
      end
      //Буфер полный.
      else begin
        //Если пришел опорный кадр, то можно выкинуть все, что накопилось в буфере
        if (ffKeyFrame in aFormat.biFrameFlags) then
        begin
          //Очищаем весь буфер
          for i := FBuffer.Count-1 downto 0 do
            //Кроме инициализационных кадров, без них видео не заработает
            if not (ffInitParamsFrame in FBuffer.Items[i].Format.biFrameFlags)  then
              FBuffer.DeleteFrame(i);


          FBuffer.AddFrame(aFormat, aData,aDataSize,aInfo,aInfoSize);
          FBufferWaitForKeyFrame:=false;
        end
        //Пытаемся отсечь слишком старые кадры
        else begin
          //Удаляем всех начальные кадры вплоть до последнего опорного
          for i := FBuffer.Count-1 downto 0 do
          begin
            if ffKeyFrame in FBuffer[i].Format.biFrameFlags then
            begin
              //Удаляем все фреймы с начала до найденного опорного
              for j:=i-1 downto 0 do
                //Кроме инициализационных кадров, без них видео не заработает
                if not (ffInitParamsFrame in FBuffer.Items[j].Format.biFrameFlags)  then
                  FBuffer.DeleteFrame(j);

              Assert(ffKeyFrame in FBuffer[0].Format.biFrameFlags);
              break;
            end;
          end;

          if not IsOutOfBuffer then
          begin
            FBuffer.AddFrame(aFormat, aData,aDataSize,aInfo,aInfoSize);
            FBufferWaitForKeyFrame:=false;
          end
          else begin
            //Буфер переполнен. Придется пропустить кадр. Его некуда деть.
            b:=false;
            FBufferWaitForKeyFrame:=true;
          end;
        end;
      end;
    finally
      FBuffer.Unlock;
    end;

    if not b then
    begin
      FEventsLock.Enter;
      try
        if Assigned(FOnOutOfBuffer) then
          FOnOutOfBuffer(self);
      finally
        FEventsLock.Leave;
      end;
    end;
  end;
end;

procedure TPlayerVideoOutput.OnDisplayThread;
var
  aFrame : TMediaStreamFrame;
  x: int64;
begin
  if not FBuffer.WaitForData(100) then
    exit;
  aFrame:=nil;

  try
    FBuffer.TryLockOrRaise(10*1000); //Подстраховываемся: пытаемся захватить секцию в течение 10 секунд, потом - ошибка
    try
      if FBuffer.Count>0 then
        aFrame:=FBuffer.ExtractFirstFrame;
    finally
      FBuffer.Unlock;
    end;

    x:=FBuffer.Duration;
    if x<FAccumulationPeriodMs then
    begin
      if x<FAccumulationPeriodMs div 4 then
        FBuffer.WaitForAddedFrames(3,125)
      else if x<FAccumulationPeriodMs*3 div 4 then
        FBuffer.WaitForAddedFrames(2,85)
      else
        FBuffer.WaitForAddedFrames(1,50)
    end;

    if aFrame=nil then
      //Do nothing
    else begin
      DecodeAndDisplayFrame(aFrame.Format,aFrame.DataPtr,aFrame.DataSize,aFrame.InfoPtr,aFrame.InfoSize);
      inc(FFrameDisplayedCount);
    end;
  finally
    if aFrame<>nil then
      FBuffer.DisposeFrame(aFrame);
  end;
end;

procedure TPlayerVideoOutput.ResetBuffer;
begin
  FBuffer.Lock;
  try
    FBuffer.Clear;
  finally
    FBuffer.Unlock;
  end;

  if FDecoder<>nil then
    FDecoder.ResetBuffer;
end;

procedure TPlayerVideoOutput.DeallocateVideoContext;
begin
  FDisplayThreadLock.Enter;
  try
    FreeAndNil(FDisplayThread);
  finally
    FDisplayThreadLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.DecodeAndDisplayFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
var
  aAccept: boolean;
  aDecodeResult: TVideoOutputDecoderDecodeResult;
begin
  inc(FFramesReceived);
  if (ffPrebuffer in aFormat.biFrameFlags) then
    inc(FFramesPrebufferReceived);

  if (FLastThreadId<>0) and (GetCurrentThreadId<>FLastThreadId) then
    TraceLine(Format('Executable thread changed from %d to %d',[FLastThreadId,GetCurrentThreadId]));

  FLastThreadId:=GetCurrentThreadId;

  if (FLastTimeStamp<>0) and (aFormat.TimeStampMs<FLastTimeStamp) then
    TraceLine(Format('Current timestamp=%d, prev timestamp=%d (prev larger)' ,[aFormat.TimeStampMs,FLastTimeStamp]));
  FLastTimeStamp:=aFormat.TimeStampMs;

  if FDecoder=nil then
  begin
    TraceLine('Cannot Decode Frame: Decoder=nil');
    exit;
  end;

  aDecodeResult:=FDecoder.DecodeData(aFormat, aData,aDataSize,aInfo,aInfoSize);
  if aDecodeResult in [drWait,drError] then
  begin
    if (aDecodeResult=drError) then
    begin
      if (uTrace.GetTraceEnabled) then
        TraceLine(Format('Cannot Decode Frame: Decoder failed; Decoder: Class=%s, SuccessCount=%d, ErrorCount=%d, FirstSuccessfull=%d; Frame: Index=%d, MediaType=%s; StreamType: %s; Flags = %s, Size=%d',
        [ FDecoder.ClassName, FDecoder.FrameStatistics[drSuccess], FDecoder.FrameStatistics[drError], FFirstFrameDecoded,
          FFramesReceived-1, MediaTypeNames[aFormat.biMediaType],  GetStreamTypeName(aFormat.biStreamType), FrameFlagsToString(aFormat.biFrameFlags),aDataSize]));
      if Assigned(FOnFrameDecodeError) then
        FOnFrameDecodeError(Self,aFormat,aData,aDataSize,aInfo,aInfoSize);
    end;

    exit;
  end;

  if FFirstFrameDecoded=-1 then
  begin
    FFirstFrameDecoded:=FFramesReceived-1;
    if uTrace.GetTraceEnabled then
      TraceLine(Format('First decoding success; Frame: Index=%d, Flags = %s, Size=%d',[FFramesReceived-1,FrameFlagsToString(aFormat.biFrameFlags),aDataSize]));

    //MessageBox(0,PChar(IntToStr(FFirstFrameDecoded)),'',0);
  end;

  inc(FFramesDecoded);
  if Assigned(FOnFrameDecoded) then
    FOnFrameDecoded(self);

  LockWindowHandle;
  try
    if FWindowHandle=0 then
      exit;
  finally
    UnlockWindowhandle;
  end;

  FEventsLock.Enter;
  try
    aAccept:=true;
    if Assigned(FOnBeforeDrawFrameImage) then
      FOnBeforeDrawFrameImage(self,aAccept);

    if not aAccept then
      exit;
  finally
    FEventsLock.Leave;
  end;

  LockWindowHandle;
  try
    if FWindowHandle<>0 then
      DrawCurrentDecodedImage;
  finally
    UnlockWindowhandle;
  end;
end;

procedure TPlayerVideoOutput.SetOnOutOfBuffer(const Value: TOutOfBufferEvent);
begin
  FEventsLock.Enter;
  try
    FOnOutOfBuffer := Value;
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutput.WndProc(var Message: TMessage);
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

  //При смене разрешения экрана нужно переинициализироваться
  if Message.Msg = WM_DISPLAYCHANGE then
  begin
    //FIX блокируем изменение переменной потока до тех пор, пока не сменим окно, иначе проскакивает новое создание потока
    FDisplayThreadLock.Enter;
    try
      DeallocateVideoContext;
    finally
      FDisplayThreadLock.Leave;
    end;
  end;

  Message.result := DefWindowProc(FServiceWndHandle, Message.Msg, Message.wParam, Message.lParam);
end;


//==============================================================================

{ TSurfaceDirectX }

procedure TSurfaceDirectX.DrawRGB15(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean);
begin
  FOwner.LockDrawer;
  try
    Assert(FOwner.FDrawer<>nil); //Этот код может вызван только при созданном Drawer
    FOwner.FDrawer.DrawRGB15(aDIB,aDIBSize,FSurfaceDesc,aReverseVertical);
  finally
    FOwner.UnlockDrawer;
  end;
end;

procedure TSurfaceDirectX.DrawRGB16(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean);
begin
  FOwner.LockDrawer;
  try
    Assert(FOwner.FDrawer<>nil); //Этот код может вызван только при созданном Drawer
    FOwner.FDrawer.DrawRGB16(aDIB,aDIBSize,FSurfaceDesc,aReverseVertical);
  finally
    FOwner.UnlockDrawer;
  end;
end;

procedure TSurfaceDirectX.DrawRGB24(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean);
begin
  FOwner.LockDrawer;
  try
    Assert(FOwner.FDrawer<>nil); //Этот код может вызван только при созданном Drawer
    FOwner.FDrawer.DrawRGB24(aDIB,aDIBSize,FSurfaceDesc,aReverseVertical);
  finally
    FOwner.UnlockDrawer;
  end;
end;

procedure TSurfaceDirectX.DrawRGB32(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean);
begin
  FOwner.LockDrawer;
  try
    Assert(FOwner.FDrawer<>nil); //Этот код может вызван только при созданном Drawer
    FOwner.FDrawer.DrawRGB32(aDIB,aDIBSize,FSurfaceDesc,aReverseVertical);
  finally
    FOwner.UnlockDrawer;
  end;
end;

procedure TSurfaceDirectX.DrawYUV420(aDIB: pointer; aDIBSize: cardinal; aReverseVertical: boolean);
begin
  FOwner.LockDrawer;
  try
    Assert(FOwner.FDrawer<>nil); //Этот код может вызван только при созданном Drawer
    FOwner.FDrawer.DrawYUV420(aDIB,aDIBSize,FSurfaceDesc,aReverseVertical);
  finally
    FOwner.UnlockDrawer;
  end;
end;

procedure TSurfaceDirectX.Init(aOwner:TPlayerVideoOutputSurface);
begin
  FOwner:=aOwner;
end;

procedure TSurfaceDirectX.Prepare(const aSurfaceDesc: TSurfaceDesc);
begin
  inherited;
  FSurfaceDesc:=aSurfaceDesc;
end;

function TSurfaceDirectX.SurfaceFormat: TRGBFormat;
var
  aBits: integer;
begin
  Assert(FOwner<>nil);
  //Спец хак: декодировщик вызывает эту функцию в надежде получить поверхность для рисования. И если ее еще нет, ее придется создать (временно).
  //Иначе декодер никогда не проинициализируется
  FOwner.LockDrawer;
  try
    if FOwner.FDrawer<>nil then
      result:=FOwner.FDrawer.SurfaceFormat
    else begin
      aBits := GetDeviceCaps(GetDc(GetDesktopWindow), BITSPIXEL);
      case aBits of
        15: result:=RGB15;
        16: result:=RGB16;
        24: result:=RGB24;
        32: result:=RGB32;
        else
          result:=RGB24;
      end;
    end;
  finally
    FOwner.UnlockDrawer;
  end;
end;

{ TPlayerVideoOutputDirectX }

procedure TPlayerVideoOutputSurface.CaptureCurrentImageToStreamAsBitmap(
  aStream: TStream);
begin
  LockDrawer;
  try
    if (FDrawer=nil) or (VideoWidth=0) or (VideoHeight=0)  then
    begin
    end
    else begin
      FDrawer.SnapToStreamAsBitmap(aStream);
    end;
  finally
    UnlockDrawer;
  end;
end;

procedure TPlayerVideoOutputSurface.ClearScreen;
begin
  LockDrawer;
  try
    if FDrawer<>nil then
    begin
      FDrawer.BackgroundColor:=ColorToRGB(self.BackgroundColor);
      FDrawer.ClearScreen;
    end;
  finally
    UnlockDrawer;
  end;
end;

constructor TPlayerVideoOutputSurface.Create(aWindowHandle: HWND);
begin
  // До базового create
  FSurface:=CreateSurface;
  FDrawerLock := TCriticalSection.Create;

  inherited Create(aWindowHandle);
end;

procedure TPlayerVideoOutputSurface.DeallocateVideoContext;
begin
  inherited;
  DestroyDrawer;
end;

procedure TPlayerVideoOutputSurface.DecodeAndDisplayFrame(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal);
var
  aErrorMessage: string;

  aSurfaceDesc:TSurfaceDesc;
  aDC : HDC;
  R:TRect;
  aD: TDirectImageRGB;
begin
  ZeroMemory(@aSurfaceDesc,sizeof(aSurfaceDesc));

  if FSurface is TSurfaceDirectX then //TODO
    TSurfaceDirectX(FSurface).Init(self);

  aSurfaceDesc.Init(nil,0,aFormat.VideoWidth,aFormat.VideoHeight,FSurface.SurfaceBits);


  FSurface.Prepare(aSurfaceDesc);

  if FDecoder<>nil then
    FDecoder.OutputSurface:=FSurface;

  aErrorMessage:='';
  try
    inherited DecodeAndDisplayFrame(aFormat,aData,aDataSize,aInfo,aInfoSize);
  except
    on E:Exception do
    begin
      aErrorMessage:=E.Message;
      TraceLine('Exception: '+E.Message);
    end;
  end;

  if aErrorMessage<>'' then
  begin
    LockDrawer;
    try
      if (VideoWidth=0) or (VideoHeight=0) then
      begin
        if GetClientRect(WindowHandle,R) then
          SetVideoSize(R.Right,R.Bottom)
        else
          SetVideoSize(640,480);
      end;
    finally
      UnlockDrawer;
    end;

    LockDrawer;
    try
      aD:=Drawer;
      aD.UpdateBounds; //TODO поискать простой способ определения движения родительского окна, и от него делать пересчет
      aD.EnterDrawSurface(aSurfaceDesc);
      try
        aDC:=aD.GetSurfaceDC;
        try
          Windows.ExtTextOut(aDC, 0, 0, 0, nil, PChar(aErrorMessage),Length(aErrorMessage),nil);
        finally
          aD.ReleaseDC(aDC,false);
        end;
      finally
        aD.LeaveDrawSurface;
      end;
    finally
      UnlockDrawer;
    end;
  end;
end;

destructor TPlayerVideoOutputSurface.Destroy;
begin
  inherited;
  DestroyDrawer;
  FreeAndNil(FDrawerLock);
  FreeAndNil(FSurface);
end;

procedure TPlayerVideoOutputSurface.DrawBitmap(aBitmap: TBitmap; aReverseVertical: boolean=true);
var
  aDIB1,aDIB2: pointer;
begin
  if aBitmap.Height=0 then
    exit;
  aDIB1:=aBitmap.ScanLine[0];
  aDIB2:=aBitmap.ScanLine[aBitmap.Height-1];

  if PAnsiChar(aDIB1)>PAnsiChar(aDIB2) then
    aDIB1:=aDIB2;

  case aBitmap.PixelFormat of
   pf15bit:DrawRGB(aDIB1,aBitmap.Width*aBitmap.Height*2,aBitmap.Width,aBitmap.Height,RGB15, aReverseVertical);
   pf16bit:DrawRGB(aDIB1,aBitmap.Width*aBitmap.Height*2,aBitmap.Width,aBitmap.Height,RGB16, aReverseVertical);
   pf24bit:DrawRGB(aDIB1,aBitmap.Width*aBitmap.Height*2,aBitmap.Width,aBitmap.Height,RGB24, aReverseVertical);
   pf32bit:DrawRGB(aDIB1,aBitmap.Width*aBitmap.Height*2,aBitmap.Width,aBitmap.Height,RGB32, aReverseVertical);
  end;

end;

procedure TPlayerVideoOutputSurface.DrawRGB(aDIB: pointer; aSize: cardinal; aWidth,aHeight: integer; aRGBFormat: TRGBFormat; aReverseVertical: boolean);
var
  aSurfaceDesc: TSurfaceDesc;
  aAccept: boolean;
  aD: TDirectImageRGB;
begin
  SetVideoSize(aWidth,aHeight);
  aAccept:=true;

  (*
  FEventsLock.Enter;
  try
    if Assigned(FOnBeforeDrawFrameImage) then
      FOnBeforeDrawFrameImage(self,aAccept);
  finally
    FEventsLock.Leave;
  end;
  *)


  if not aAccept then
    exit;

  try
    LockDrawer;
    try
      aD:=Drawer;
      aD.UpdateBounds; //TODO поискать простой способ определения движения родительского окна, и от него делать пересчет
      aD.EnterDrawSurface(aSurfaceDesc);
      try
        case aRGBFormat of
          RGB32: aD.DrawRGB32(aDIB,aSize,aSurfaceDesc,aReverseVertical);
          RGB24: aD.DrawRGB24(aDIB,aSize,aSurfaceDesc,aReverseVertical);
          RGB16: aD.DrawRGB16(aDIB,aSize,aSurfaceDesc,aReverseVertical);
          RGB15: aD.DrawRGB15(aDIB,aSize,aSurfaceDesc,aReverseVertical);
        end;

             (*
        //Внешняя прорисовка
        FEventsLock.Enter;
        try
          if Assigned(FOnBuildFrameImage) then
          begin
            aDC:=Drawer.GetSurfaceDC;
            try
              FOnBuildFrameImage(self,aDC,FDrawer.SurfaceWidth,FDrawer.SurfaceHeight);
            finally
              Drawer.ReleaseDC(aDC,false);
            end;
          end;
        finally
          FEventsLock.Leave;
        end;*)

      finally
        aD.LeaveDrawSurface;
      end;
    finally
      UnlockDrawer;
    end;

  except
    //Будем тушить возможные исключения от DirectX. Могут быть штатные проблемы при прорисовке, и пробрасывать их наверх как исключения нет особого смысла
  end;
end;

function TPlayerVideoOutputSurface.GetSurfaceDC: HDC;
begin
  LockDrawer;
  result:=Drawer.GetSurfaceDC;
end;

procedure TPlayerVideoOutputSurface.ReleaseDC(aDC: HDC; aRedraw: boolean);
begin
  Assert(FDrawer<>nil);
  FDrawer.ReleaseDC(aDC,aRedraw);
  UnlockDrawer;
end;

procedure TPlayerVideoOutputSurface.LockDrawer;
begin
  FDrawerLock.TryEnterOrRaise(10000); //Если в течение 10 секунд рисователь не освободился - это похоже на DeadLock
end;

procedure TPlayerVideoOutputSurface.LockWindowHandle;
begin
  Assert(self<>nil);
  inherited;
  LockDrawer;
end;

procedure TPlayerVideoOutputSurface.CaptureCurrentImageToBitmap(aBitmap:TBitmap);
begin
  LockDrawer;
  try
    if (FDrawer=nil) or (VideoWidth=0) or (VideoHeight=0)  then
    begin
      aBitmap.Width:=0;
      aBitmap.Height:=0;
    end
    else begin
      FDrawer.SnapToBitmap(aBitmap);
    end;
  finally
    UnlockDrawer;
  end;
end;


procedure TPlayerVideoOutputSurface.SetBackgroundColor(const Value: TColor);
begin
  inherited;
  LockDrawer;
  try
    if FDrawer<>nil then
      FDrawer.BackgroundColor:=Value;
  finally
    UnlockDrawer;
  end;
end;

procedure TPlayerVideoOutputSurface.SetOnDrawFrameImage(const Value: TPlayerVideoOutputDrawFrame);
begin
  if CompareMem(@TMethod(OnDrawFrameImage),@TMethod(Value),sizeof(TMethod)) then
    exit;

  inherited;

  LockDrawer;
  try
    if FDrawer<>nil then
      if Assigned(OnDrawFrameImage) then
        FDrawer.OnDrawFrontSurface:=OnDrawImageOnScreen
      else
        FDrawer.OnDrawFrontSurface:=nil;
  finally
    UnlockDrawer;
  end;
end;


function TPlayerVideoOutputSurface.StatusInfo: string;
begin
  result:=inherited StatusInfo;
end;

procedure TPlayerVideoOutputSurface.UnlockDrawer;
begin
  FDrawerLock.Leave;
end;

procedure TPlayerVideoOutputSurface.UnlockWindowhandle;
begin
  UnlockDrawer;
  inherited;
end;

procedure TPlayerVideoOutputSurface.UpdateBounds;
begin
  LockDrawer;
  try
    if FDrawer<>nil then
      FDrawer.UpdateBounds;
  finally
    UnlockDrawer;
  end;
end;

procedure TPlayerVideoOutputSurface.UpdateImage;
begin
  LockDrawer;
  try
    if FDrawer<>nil then
    begin
      UpdateBounds;
      if FDrawer<>nil then
        FDrawer.Redraw;
    end;
  finally
    UnlockDrawer;
  end;
end;

function TPlayerVideoOutputSurface.VideoContextAllocated: boolean;
begin
  result:=FDrawer<>nil;
end;

function TPlayerVideoOutputSurface.Drawer: TDirectImageRGB;
begin
  result:=nil; //Make compiler happy
  LockDrawer;
  try
    if (FDrawer<>nil) then
    begin
      if (FDrawer.SurfaceWidth<>VideoWidth) or (FDrawer.SurfaceHeight<>VideoHeight) then
        FDrawer.Resize(VideoWidth,VideoHeight)
      else if (not FDrawer.MonitorValid)  then
        DestroyDrawer;
    end;

    if FDrawer=nil then
    begin
      if WindowHandle=0 then
        raise Exception.Create('Нет созданного дескриптора окна');

      if VideoWidth=0 then
        raise Exception.Create('Не указана ширина видеоизображения');

      if VideoHeight=0 then
        raise Exception.Create('Не указана высота видеоизображения');


      FDrawer:=TDirectImageRGB.Create(WindowHandle,VideoWidth,VideoHeight);

      FDrawer.BackgroundColor:=FBackgroundColor;
      if Assigned(OnDrawFrameImage) then
        FDrawer.OnDrawFrontSurface:=OnDrawImageOnScreen
      else
        FDrawer.OnDrawFrontSurface:=nil;

    end;

    result:=FDrawer;
    Assert(result<>nil);
  finally
    UnlockDrawer;
  end;
end;

procedure TPlayerVideoOutputSurface.DestroyDrawer;
begin
  LockDrawer;
  try
    FreeAndNil(FDrawer);
  finally
    UnlockDrawer;
  end;
end;

procedure TPlayerVideoOutputSurface.OnDrawImageOnScreen(const aSurface: IDirectDrawSurface7; const aRect: TRect);
var
  aDC: HDC;
begin
  FEventsLock.Enter;
  try
    if Assigned(OnDrawFrameImage) then
    begin
      if Succeeded(aSurface.GetDC(aDC)) then
      try
        OnDrawFrameImage(self,aDC,aRect.Right-aRect.Left,aRect.Bottom-aRect.Top);
      finally
        aSurface.ReleaseDC(aDC);
      end;
    end;
  finally
    FEventsLock.Leave;
  end;
end;


procedure TPlayerVideoOutputSurface.DrawCurrentDecodedImage;
var
  aSurfaceDesc : TSurfaceDesc;
  aDC : HDC;
  aD: TDirectImageRGB;
  b: boolean;
begin
  if (Decoder.ImageWidth=0) or (Decoder.ImageHeight=0) then
    exit;

  LockDrawer;
  try
    if (VideoWidth<>Decoder.ImageWidth) or (VideoHeight<>Decoder.ImageHeight) then
      SetVideoSize(Decoder.ImageWidth,Decoder.ImageHeight);

    aD:=Drawer;
    aD.UpdateBounds; //TODO поискать простой способ определения движения родительского окна, и от него делать пересчет
    aD.EnterDrawSurface(aSurfaceDesc);
    try
      if FSurface is TSurfaceDirectX then //TODO
        TSurfaceDirectX(FSurface).Init(self);
      FSurface.Prepare(aSurfaceDesc);

      try
        //FIX. По непонятным причинам иногда при перетаскивании изображения с одного монитора на другой вываливается ошибка записи в память
        //Добавление этой проверки сводит вероятность возникновения такой ошибки практически к нулю
        if aD.MonitorValid then
        begin
          b:=true;
          if Assigned(FOnCopyDecodedDataToSurface) then
            FOnCopyDecodedDataToSurface(self,FSurface,b);
          if b then
            Decoder.CopyDecoderImageToSurface(FSurface);
        end;
      except
        on E:EAccessViolation do
        begin
          if IsBadWritePtr(aSurfaceDesc.Data,aSurfaceDesc.DataSize) then
            raise Exception.Create('Поверхность не доступна для записи изображения')
          else
            raise;
        end;
      end;


      //Внешняя прорисовка
      FEventsLock.Enter;
      try
        if Assigned(OnBuildFrameImage) then
        begin
          aDC:=aD.GetSurfaceDC;
          try
            OnBuildFrameImage(self,aDC,aD.SurfaceWidth,aD.SurfaceHeight);
          finally
            aD.ReleaseDC(aDC,false);
          end;
        end;
      finally
        FEventsLock.Leave;
      end;
    finally
      aD.LeaveDrawSurface;
    end;
  finally
    UnlockDrawer;
  end;

  FEventsLock.Enter;
  try
    if Assigned(OnAfterDrawFrameImage) then
      OnAfterDrawFrameImage(self);
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutputSurface.DrawCustom(aData: pointer; aCallback: TPlayerVideoOutputDrawCustomData);
var
  aDC : HDC;
  aD: TDirectImageRGB;
  aSurfaceDesc:TSurfaceDesc;
begin
  if VideoWidth*VideoHeight=0 then
   exit;

  LockDrawer;
  try
    aD:=Drawer;
    aD.UpdateBounds; //TODO поискать простой способ определения движения родительского окна, и от него делать пересчет
    aD.EnterDrawSurface(aSurfaceDesc);
    try
      if FSurface is TSurfaceDirectX then //TODO
        TSurfaceDirectX(FSurface).Init(self);
      FSurface.Prepare(aSurfaceDesc);

      //Внешняя прорисовка
      FEventsLock.Enter;
      try
        aDC:=aD.GetSurfaceDC;
        try
          aCallback(self,aDC,FDrawer.SurfaceWidth,FDrawer.SurfaceHeight,aData);
        finally
          aD.ReleaseDC(aDC,false);
        end;
      finally
        FEventsLock.Leave;
      end;
    finally
      aD.LeaveDrawSurface;
    end;
  finally
    UnlockDrawer;
  end;

  FEventsLock.Enter;
  try
    if Assigned(OnAfterDrawFrameImage) then
      OnAfterDrawFrameImage(self);
  finally
    FEventsLock.Leave;
  end;
end;

procedure TPlayerVideoOutputSurface.SetVideoSize(aVideoWidth, aVideoHeight: cardinal);
begin
  LockDrawer;
  try
    if (VideoWidth=aVideoWidth) and (VideoHeight=aVideoHeight) then
      exit;

    if FDrawer<>nil then
      FDrawer.Resize(aVideoWidth,aVideoHeight);
  finally
    UnlockDrawer;
  end;

  inherited SetVideoSize(aVideoWidth,aVideoHeight);
end;



{ TPlayerVideoOutputDirectX }

function TPlayerVideoOutputDirectX.CreateSurface: TSurface;
begin
  result:=TSurfaceDirectX.Create;
end;

{ TSurfaceRGB }

procedure TSurfaceRGB.CopyToBitmap(aBitmap: TBitmap);
var
  aStream: TBimapStreamMediator;
  aFileHeader: BITMAPFILEHEADER;
  aBitmapHeader : BITMAPINFOHEADER;
begin
  aStream:=TBimapStreamMediator.Create;
  try
    ZeroMemory(@aFileHeader,sizeof(aFileHeader));
    aFileHeader.bfType := $4d42; //"BM"
    aFileHeader.bfReserved1 := 0;
    aFileHeader.bfReserved2 := 0;
    aFileHeader.bfOffBits   := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
    aFileHeader.bfSize :=   Length(DIB) + sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

    ZeroMemory(@aBitmapHeader,sizeof(aBitmapHeader));
    aBitmapHeader.biSize:=sizeof(aBitmapHeader);
    aBitmapHeader.biWidth:=SurfaceWidth;
    aBitmapHeader.biHeight:=SurfaceHeight;
    aBitmapHeader.biPlanes:=1;
    aBitmapHeader.biBitCount:=RgbFormatToBits(SurfaceFormat);
    aBitmapHeader.biCompression:=BI_RGB;
    aBitmapHeader.biSizeImage:=Length(DIB);

    aStream.Init(@aFileHeader,@aBitmapHeader,@DIB[0],Length(DIB));
    aBitmap.LoadFromStream(aStream);
  finally
    aStream.Free;
  end;
end;

constructor TSurfaceRGB.Create(aSurfaceFormat: TRGBFormat);
begin
  FSurfaceFormat:=aSurfaceFormat;
end;

procedure TSurfaceRGB.DrawRGB15(aDIB: pointer; aDIBSize: cardinal;
  aReverseVertical: boolean);
var
  aSourcePlane: TBitPlaneDesc;
  aDestPlane: TBitPlaneDesc;
begin
  aSourcePlane.Init(aDIB,aDIBSize,FSurfaceWidth,FSurfaceHeight,15);
  aDestPlane.Init(@FDIB[0],Length(FDIB),FSurfaceWidth,FSurfaceHeight,RgbFormatToBits(FSurfaceFormat));
  aSourcePlane.CopyToBitPlane(aDestPlane,not aReverseVertical);
end;

procedure TSurfaceRGB.DrawRGB16(aDIB: pointer; aDIBSize: cardinal;
  aReverseVertical: boolean);
var
  aSourcePlane: TBitPlaneDesc;
  aDestPlane: TBitPlaneDesc;
begin
  aSourcePlane.Init(aDIB,aDIBSize,FSurfaceWidth,FSurfaceHeight,16);
  aDestPlane.Init(@FDIB[0],Length(FDIB),FSurfaceWidth,FSurfaceHeight,RgbFormatToBits(FSurfaceFormat));
  aSourcePlane.CopyToBitPlane(aDestPlane,not aReverseVertical);
end;

procedure TSurfaceRGB.DrawRGB24(aDIB: pointer; aDIBSize: cardinal;
  aReverseVertical: boolean);
var
  aSourcePlane: TBitPlaneDesc;
  aDestPlane: TBitPlaneDesc;
begin
  aSourcePlane.Init(aDIB,aDIBSize,FSurfaceWidth,FSurfaceHeight,24);
  aDestPlane.Init(@FDIB[0],Length(FDIB),FSurfaceWidth,FSurfaceHeight,RgbFormatToBits(FSurfaceFormat));
  aSourcePlane.CopyToBitPlane(aDestPlane,not aReverseVertical);
end;

procedure TSurfaceRGB.DrawRGB32(aDIB: pointer; aDIBSize: cardinal;
  aReverseVertical: boolean);
var
  aSourcePlane: TBitPlaneDesc;
  aDestPlane: TBitPlaneDesc;
begin
  aSourcePlane.Init(aDIB,aDIBSize,FSurfaceWidth,FSurfaceHeight,32);
  aDestPlane.Init(@FDIB[0],Length(FDIB),FSurfaceWidth,FSurfaceHeight,RgbFormatToBits(FSurfaceFormat));
  aSourcePlane.CopyToBitPlane(aDestPlane,not aReverseVertical);
end;

procedure TSurfaceRGB.DrawYUV420(aDIB: pointer; aDIBSize: cardinal;aReverseVertical: boolean);
begin
  Assert(aDibSize=Trunc(FSurfaceWidth*FSurfaceHeight*1.5));
  TDirectImageRGB.CopyYuv420ToRGBx(aDIB,aDIBSize,FSurfaceWidth,FSurfaceHeight,self.SurfaceFormat,@FDIB[0],Length(FDIB),aReverseVertical);
end;

procedure TSurfaceRGB.Prepare(const aSurfaceDesc: TSurfaceDesc);
begin
  inherited;
  SetSize(aSurfaceDesc.Width,aSurfaceDesc.Height);
end;

procedure TSurfaceRGB.SetSize(aWidth, aHeight: integer);
begin
  if (aWidth=FSurfaceWidth) and (aHeight=FSurfaceHeight) then
    exit;

  Assert(aWidth>=0,'Width<0');
  Assert(aWidth<10000,'Width>10000');

  Assert(aHeight>=0,'Height<0');
  Assert(aHeight<10000,'Height>10000');

  FSurfaceWidth:=aWidth;
  FSurfaceHeight:=aHeight;
  FDIB:=nil;
  SetLength(FDIB,GetRGBSize(FSurfaceWidth,FSurfaceHeight,RgbFormatToBits(FSurfaceFormat)));
end;

function TSurfaceRGB.SurfaceFormat: TRGBFormat;
begin
  result:=FSurfaceFormat;
end;


{ TPlayerVideoOutputRgbBuffer }

function TPlayerVideoOutputRgb24Buffer.CreateSurface: TSurface;
begin
  result:=TSurfaceRGB.Create(RGB24);
end;

{ TSurface }

procedure TSurface.Prepare(const aSurfaceDesc: TSurfaceDesc);
begin

end;

function TSurface.SurfaceBits: byte;
begin
  result:=RgbFormatToBits(SurfaceFormat);
end;

initialization
  PlayerVideoOutputDecoderFactory;

finalization
  FreeAndNil(gPlayerVideoOutputDecoderFactory);

end.





