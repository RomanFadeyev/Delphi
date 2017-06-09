unit MediaProcessing.VideoAnalytics;

interface
  uses SysUtils,Classes, UiTypes, SyncObjs, SvaLib,Windows, Types, Bitplane,MediaProcessing.Definitions,Player.VideoDecoding,
  MediaProcessing.VideoAnalytics.Definitions,
  OpenCV.HighGui,OpenCV.Types,OpenCV.Core,OpenCV.ImgProc;


type
  TVaParameters = record
    OtEnabled: boolean;
    OtEngine: TObjectTrackingEngine;
    OtFilters: TVaFilterArray;
    OtFiltersVisualizationMode:TVaFilterVisualizationMode;
    OtDrawTrajectory: boolean;
    OtDrawIdentifiers: boolean;
    OtDrawObjectTypes: boolean;

    LuEnabled: boolean;
    LuDrawHistogramms: boolean;
    LuMinY: integer;
    LuMaxY: integer;

    AsyncProcessing: boolean;

    procedure Clear;
  end;


  TVaProcessingResult = record
    TimeStamp: cardinal;
    Objects: TVaObjectArray;
    Events: TVaEventArray;
    PictureSize: TSize;

    YHistogramm: TChannelHistogramm;
    RHistogramm: TChannelHistogramm;
    GHistogramm: TChannelHistogramm;
    BHistogramm: TChannelHistogramm;
  end {sva_metadata_t};
  PVaProcessingResult = ^TVaProcessingResult;


  TVideoAnalytics = class;
  TTraceEventHandler = procedure (Sender: TVideoAnalytics; const aMessage: string) of object;
  TFrameProcessedEventHandler = procedure (Sender: TVideoAnalytics) of object;

  TVideoAnalytics = class
  private
    FHandle: sva_handle;
    FProcessedFrames: cardinal;
    FCurrentWidth,FCurrentHeight: integer;
    FCurrentReversedVertical: boolean;
    FCurrentResult : TVaProcessingResult;
    FCurrentResultLock: TCriticalSection;
    FPrevResult : TVaProcessingResult;
    FParameters: TVaParameters;
    FModel: AnsiString;
    FModelDirtyFlag: boolean;
    FOnTrace: TTraceEventHandler;
    FProcessingThread: TThread;
    FOnFrameProcessed: TFrameProcessedEventHandler;
    FVideoDecoding: TVideoDecoding;

    //FCurrentResultBlackout : boolean;
    //FCurrentResultOverExposure : boolean;

    greyImage,colourImage,movingAverage,difference,temp,motionHistory: PIplImage;
    first: boolean;

    procedure ProcessFrameInternal(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);

    procedure ProcessTracking(const aBitplane: TBitPlaneDesc);
    procedure ProcessTracking2(const aBitplane: TBitPlaneDesc);
    procedure ProcessLuminocity(const aBitplane: TBitPlaneDesc);

    procedure CreateVideoDecoding;
  public
    class procedure InitnializeEnvironment;

    constructor Create;
    destructor Destroy; override;
    procedure Init;

    procedure LoadModel(const aFileName: string);
    procedure LoadConfig(const aFileName: string);

    procedure ProcessFrame(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);


    procedure DrawCurrentResult(aDC: HDC;  aDCWidth, aDCHeight: cardinal);

    property  Parameters: TVaParameters read FParameters write FParameters;

    //Не потокобезопасно! Нужно вызывать CurrentResultLock
    property  CurrentResult: TVaProcessingResult read FCurrentResult;
    procedure CurrentResultLock;
    procedure CurrentResultUnlock;
    function  GetCurrentResultCopy: TVaProcessingResult;

    property VideoDecoding: TVideoDecoding read FVideoDecoding;

    property CurrentWidth: integer read FCurrentWidth;
    property CurrentHeight: integer read FCurrentHeight;

    property OnTrace:TTraceEventHandler read FOnTrace write FOnTrace;
    property OnFrameProcessed: TFrameProcessedEventHandler read FOnFrameProcessed write FOnFrameProcessed;
  end;


implementation
  uses uSync, uBaseClasses, Graphics,ThreadNames, Patterns.Workspace;

type
  TStreamHelper = class helper for TStream
  public
    procedure ReadAll(var aValue: TBytes); overload;
  end;

  TVaObjectHelper = record helper for TVaObject
    procedure CopyFrom(const aSvaObject: sva_object_t);
  end;

  TVaEventHelper = record helper for TVaEvent
    procedure CopyFrom(const aSvaEvent: sva_event_t);
  end;

  TFrame = class
  private
    FFormat: TMediaStreamDataHeader;
    FData: TBytes;
    FLock : TCriticalSection;
  public
    procedure Init(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);

    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
  end;

  TProcessThread = class (TThread)
  private
    FEvent: TEvent;
    FOwner: TVideoAnalytics;
    FFrameLock: TCriticalSection;
    FBuffer: array [0..1] of TFrame;
    FCurrentWrittenBufferIndex: integer;
    FCurrentProcessingBufferIndex: integer;

    procedure OnBufferOverflow;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TVideoAnalytics);
    destructor Destroy; override;

    procedure Add(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);
  end;

procedure TStreamHelper.ReadAll(var aValue: TBytes);
begin
  SetLength(aValue,Self.Size-self.Position);
  Self.Read(aValue[0],Length(aValue));
end;


{ TVideoAnalytics }

constructor TVideoAnalytics.Create;
begin
  inherited Create;
  FCurrentResultLock:=TCriticalSection.Create;
end;

procedure TVideoAnalytics.CreateVideoDecoding;
begin
  if FVideoDecoding=nil then
    FVideoDecoding:=TVideoDecoding.Create;
end;

procedure TVideoAnalytics.CurrentResultLock;
begin
  FCurrentResultLock.Enter;
end;

procedure TVideoAnalytics.CurrentResultUnlock;
begin
  FCurrentResultLock.Leave;
end;

destructor TVideoAnalytics.Destroy;
begin
  FreeAndNil(FProcessingThread);

  if FHandle<>nil then
    sva_release(FHandle);

  FreeAndNil(FVideoDecoding);
  FreeAndNil(FCurrentResultLock);
  inherited;
end;

procedure TVideoAnalytics.LoadConfig(const aFileName: string);
var
 aStream: TStream;
 aData: TBytes;
 aConfig: AnsiString;
begin
  if FParameters.OtEngine=otvSynesis then
  begin
    aStream:=TFileStream.Create(aFileName,fmOpenRead,fmShareDenyNone);
    try
      aStream.ReadAll(aData);
      aConfig:=PAnsiChar(@aData[0]);
      SvaCheck(sva_set_config(FHandle,PAnsiChar(aConfig),Length(aConfig)))
    finally
      aStream.Free;
    end;
  end;
end;

procedure TVideoAnalytics.LoadModel(const aFileName: string);
var
 aStream: TStream;
 aData: TBytes;
 aModel: AnsiString;
begin
  if FParameters.OtEngine=otvSynesis then
  begin
    aStream:=TFileStream.Create(aFileName,fmOpenRead,fmShareDenyNone);
    try
      aStream.ReadAll(aData);
      aModel:=PAnsiChar(@aData[0]);
      if aModel<>FModel then
      begin
        FModel:=aModel;
        if FCurrentWidth*FCurrentHeight<>0 then
          SvaCheck(sva_set_model(FHandle,PAnsiChar(FModel),Length(FModel)))
        else
          FModelDirtyFlag:=true;
      end;
    finally
      aStream.Free;
    end;
  end;
end;

procedure TVideoAnalytics.ProcessFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if aFormat.biMediaType<>mtVideo then
    exit;

  if FParameters.AsyncProcessing then
  begin
    if FProcessingThread=nil then
      FProcessingThread:=TProcessThread.Create(self);

    TProcessThread(FProcessingThread).Add(aFormat,aData,aDataSize,aInfo,aInfoSize);
  end
  else begin
    ProcessFrameInternal(aFormat,aData,aDataSize,aInfo,aInfoSize);
  end;
end;

procedure TVideoAnalytics.ProcessFrameInternal(
  const aFormat: TMediaStreamDataHeader; aData: pointer;
  aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
var
  aBitplane: TBitPlaneDesc;
  i: integer;
  s: string;
begin
  if aFormat.biStreamType<>stRGB then
  begin
     if FVideoDecoding=nil then
       Sync.Synchronize(CreateVideoDecoding);


    if FVideoDecoding.DecodeData(aFormat,aData,aDataSize,aInfo,aInfoSize) then
    begin
      if FVideoDecoding.DecodedFormat.biStreamType<>stRGB then
        raise Exception.Create('Недопустимый формат кадра');

      if FVideoDecoding.DecodedFormat.VideoBitCount>24 then
        raise Exception.Create('Недопустимая глубина цвета');

      ProcessFrameInternal(FVideoDecoding.DecodedFormat,pointer(FVideoDecoding.DecodedData.DIB),Length(FVideoDecoding.DecodedData.DIB),nil,0);
    end;

    exit;
  end;

{
  //Тест: сохраним в файл пришедшее изображение
  aBitPlane.Init(aData,aDataSize,aFormat.VideoWidth,aFormat.VideoHeight,24);
  aBitmap:=TBitmap.Create;
  try
    aBitPlane.CopyToBitmap(aBitmap,false);
    aBitmap.SaveToFile('current.bmp');
  finally
    aBitmap.Free;
  end;
}
  FCurrentReversedVertical:=aFormat.VideoReversedVertical;
  FCurrentResult.TimeStamp:=GetTickCount;
  FCurrentResult.PictureSize.cx:=CurrentWidth;
  FCurrentResult.PictureSize.cy:=CurrentHeight;

  FCurrentResult.Objects:=nil;
  FCurrentResult.Events:=nil;

  aBitplane.Init(aData,aDataSize,aFormat.VideoWidth,aFormat.VideoHeight,aFormat.VideoBitCount);


  if FParameters.OtEnabled then
  begin
    if FParameters.OtEngine=otvSynesis then
      ProcessTracking(aBitplane)
    else
      ProcessTracking2(aBitplane);
  end;

  if FParameters.LuEnabled then
    ProcessLuminocity(aBitplane);

  inc(FProcessedFrames);

  if Assigned(FOnTrace) then
    for i:=0 to High(FCurrentResult.Events) do
    begin
      s:=Format('Объект %d: событие %s',[FCurrentResult.events[i].object_id,VaFilterEventNames[FCurrentResult.events[i].type_]]);
      if FCurrentResult.events[i].description<>'' then
        s:=s+', '+FCurrentResult.events[i].description;
      FOnTrace(self,s);
    end;

  if Assigned(FOnFrameProcessed) then
    FOnFrameProcessed(self);
end;

procedure TVideoAnalytics.ProcessLuminocity(const aBitplane: TBitPlaneDesc);
var
  aBlackout,aOverExposure: integer;
begin
  FCurrentResultLock.Enter;
  try
    FCurrentWidth:=aBitplane.Width;
    FCurrentHeight:=aBitplane.Height;

    aBitplane.GetHistogramm(FCurrentResult.YHistogramm,htY);

    aBlackout:=100*FCurrentResult.YHistogramm.CountOf(10,FParameters.LuMinY) div 255;
    aOverExposure:=100*FCurrentResult.YHistogramm.CountOf(10,0,FParameters.LuMaxY) div 255;

    if (aBlackout<1) and (aOverExposure<1) then
    begin
      if FCurrentResult.YHistogramm.Sum(0,127)<FCurrentResult.YHistogramm.Sum(127,255) then
        aBlackout:=10000
      else
        aOverExposure:=10000;
    end;

    if aBlackout<1 then
    begin
      SetLength(FCurrentResult.Events,Length(FCurrentResult.Events)+1);
      FCurrentResult.Events[High(FCurrentResult.Events)].type_:=vaevBLACKOUT;
    end;

    if aOverExposure<1 then
    begin
      SetLength(FCurrentResult.Events,Length(FCurrentResult.Events)+1);
      FCurrentResult.Events[High(FCurrentResult.Events)].type_:=vaevOVEREXPOSURE;
    end;


    aBitplane.GetHistogramm(FCurrentResult.RHistogramm,htR, 0);
    aBitplane.GetHistogramm(FCurrentResult.GHistogramm,htG, 0);
    aBitplane.GetHistogramm(FCurrentResult.BHistogramm,htB, 0);
  finally
    FCurrentResultLock.Leave;
  end;
end;

procedure TVideoAnalytics.ProcessTracking(const aBitplane: TBitPlaneDesc);
var
  aSvaRes: sva_result;
  aSvaInput: SVA_FRAME_T;
  aCurrentResult: sva_metadata_t;

  i,t: integer;
begin
  //Вызываем VA
  if (FCurrentWidth<>aBitplane.Width) or (FCurrentHeight<>aBitplane.Height) then
  begin
    SvaCheck(sva_calibrate(FHandle,aBitplane.Width,aBitplane.Height));
    FCurrentResultLock.Enter;
    try
      FCurrentWidth:=aBitplane.Width;
      FCurrentHeight:=aBitplane.Height;
      FCurrentResult.Objects:=nil;
      FCurrentResult.Events:=nil;
    finally
      FCurrentResultLock.Leave;
    end;
  end;

  if FModelDirtyFlag then
  begin
    FModelDirtyFlag:=false;
    SvaCheck(sva_set_model(FHandle,PAnsiChar(FModel),Length(FModel)));
  end;

  ZeroMemory(@aSvaInput,sizeof(aSvaInput));
  aSvaInput.format:=integer(SVA_PIXEL_FORMAT_BGR24);
  aSvaInput.Width:=aBitplane.Width;
  aSvaInput.Height:=aBitplane.Height;
  aSvaInput.time:=FProcessedFrames/25;

  aSvaInput.planes[0].bytes:=aBitplane.Data;
  aSvaInput.planes[0].stride:=aBitplane.Pitch;
  aSvaInput.planes[0].size:=aBitplane.DataSize;
  Assert(aSvaInput.planes[0].size=aSvaInput.planes[0].stride*cardinal(aBitplane.Height));

  ZeroMemory(@aCurrentResult,sizeof(aCurrentResult));
  aSvaRes:=sva_next_frame(FHandle,aSvaInput,aCurrentResult,nil,nil);

  if (aSvaRes = SVA_ERROR_NOT_CALIBRATED) then
  begin
    SvaCheck(sva_calibrate(FHandle,aBitplane.Width,aBitplane.Height));
    aSvaRes:=sva_next_frame(FHandle,aSvaInput,aCurrentResult,nil,nil);
  end;

  FCurrentResultLock.Enter;
  try

    FPrevResult:=FCurrentResult;


    //Объекты
    SetLength(FCurrentResult.Objects,aCurrentResult.objects_count);
    for i := 0 to aCurrentResult.objects_count-1 do
      FCurrentResult.Objects[i].CopyFrom(aCurrentResult.objects[i]);

    //События
    SetLength(FCurrentResult.Events,aCurrentResult.events_count);  t:=0;
    for i := 0 to aCurrentResult.events_count-1 do
    begin
      FCurrentResult.Events[t].CopyFrom(aCurrentResult.events[i]);
      //Временно отключил события, так как они не относятся к слежению целей, а относятся к обработке картинки
      //из-за того, что синезис все делает в одной процедуре, мы временно просто игнорируем эти события
      if FCurrentResult.Events[t].type_ in [vaevBLACKOUT,vaevOVEREXPOSURE] then
        //do nothing
      else
        inc(t);
    end;

    if Length(FCurrentResult.Events)<>t then
      SetLength(FCurrentResult.Events,t);
  finally
    FCurrentResultLock.Leave;
  end;

  (*
      aBitmap:=TBitmap.Create;
      aSrcPlane.Data:=aSvaOutput.input_frame.planes[0].bytes;
      aSrcPlane.DataSize:=aSvaOutput.input_frame.planes[0].size;
      aSrcPlane.Pitch:=aSvaOutput.input_frame.planes[0].stride;
      aSrcPlane.BitCount:=8;
      aSrcPlane.Width:=aSvaOutput.input_frame.Width;
      aSrcPlane.Height:=aSvaOutput.input_frame.Height;
      CopyRGBToBitmap(aSrcPlane,aBitmap,true);
      aBitmap.SaveToFile('C:\1.bmp');
  *)

  SvaCheck(aSvaRes);
end;

procedure TVideoAnalytics.ProcessTracking2(const aBitplane: TBitPlaneDesc);
var
  storage: PCvMemStorage;
  contour: PCvSeq;
  bndRect:TCvRect;
  aNullPoint: TCvPoint;
//  pt1, pt2:TCvPoint;
  imgSize:TCvSize;
  aKoeff: integer;
  aTmpBitPlane: TBitPlaneDesc;
  i: integer;
begin
  //Вызываем VA
  if (FCurrentWidth<>aBitplane.Width) or (FCurrentHeight<>aBitplane.Height) then
  begin
    cvReleaseImage(colourImage);
    cvReleaseImage(greyImage);
    cvReleaseImage(movingAverage);
    cvReleaseImage(motionHistory);

    imgSize:=CvSize(aBitplane.Width,aBitplane.Height);

    while True do
    begin
      if (imgSize.width>640) and (imgSize.width mod 2=0) and (imgSize.height mod 2=0) then
      begin
        imgSize.width:=imgSize.width div 2;
        imgSize.height:=imgSize.height div 2;
      end
      else begin
        break;
      end;
    end;

    colourImage := cvCreateImage(imgSize, IPL_DEPTH_8U, 3);
    greyImage := cvCreateImage( imgSize, IPL_DEPTH_8U, 1);
    movingAverage := cvCreateImage( imgSize, IPL_DEPTH_32F, 3);
    motionHistory := cvCreateImage( imgSize, IPL_DEPTH_8U, 3);

    first:=true;

    FCurrentResultLock.Enter;
    try
      FCurrentWidth:=aBitplane.Width;
      FCurrentHeight:=aBitplane.Height;
      FCurrentResult.Objects:=nil;
      FCurrentResult.Events:=nil;
    finally
      FCurrentResultLock.Leave;
    end;
  end;

  aKoeff:=aBitplane.Width div cvGetSize(colourImage).width;
  if aKoeff=1 then
    CopyMemory(colourImage.imageData,aBitplane.Data,aBitplane.DataSize)
  else begin
    aTmpBitPlane.Init(colourImage.imageData,colourImage.imageSize,colourImage.width,colourImage.height,24,colourImage.widthStep);
    aBitplane.CopyToBitPlaneWithReduceScaleAsThin(aTmpBitPlane,aKoeff);
  end;

  if first then
  begin
    difference := cvCloneImage(colourImage);
    cvReleaseImage(temp);
    temp := cvCloneImage(colourImage);
    cvConvertScale(colourImage, movingAverage, 1.0, 0.0);
    first := false;
  end
  else begin
    cvRunningAvg(colourImage, movingAverage, 0.020, nil);
    cvConvertScale(movingAverage,temp, 1.0, 0.0);
  end;

  cvAbsDiff(colourImage,temp,difference);
  cvCvtColor(difference,greyImage,CV_RGB2GRAY);
  cvThreshold(greyImage, greyImage, 70, 255, CV_THRESH_BINARY);

  cvDilate(greyImage, greyImage, nil, 18);
  cvErode(greyImage, greyImage, nil, 10);

  storage := cvCreateMemStorage(0);
  try
    contour := nil;
    aNullPoint.x:=0;
    aNullPoint.y:=0;
    cvFindContours( greyImage, storage, contour, sizeof(CvContour), {CV_RETR_CCOMP}CV_RETR_LIST, CV_CHAIN_APPROX_SIMPLE,aNullPoint);

    if contour=nil then
      SetLength(FCurrentResult.Objects,0)
    else
      SetLength(FCurrentResult.Objects,contour.total);
    i:=0;

    imgSize:=cvGetSize(colourImage);

    while  contour<>nil do
    begin
      bndRect := cvBoundingRect(contour, 0);

      if (bndRect.x<>0) or (bndRect.Y<>0) or (bndRect.height<>0) or (bndRect.width<>0) then
      begin
        if i>High(FCurrentResult.Objects) then
          SetLength(FCurrentResult.Objects,Length(FCurrentResult.Objects)+1);

        FCurrentResult.Objects[i].rect.Width:=bndRect.width*aKoeff;
        FCurrentResult.Objects[i].rect.Top:=(imgSize.height- bndRect.y-bndRect.height)*aKoeff;
        FCurrentResult.Objects[i].rect.Height:=bndRect.height*aKoeff;

        //pt1.x := bndRect.x
        //pt1.y := bndRect.y
        //pt2.x := (bndRect.x + bndRect.width
        //pt2.y := (bndRect.y + bndRect.height)
        //cvRectangle(colourImage, pt1, pt2, CV_RGB(255,0,0), 1);
        FCurrentResult.Objects[i].id:=i;
        FCurrentResult.Objects[i].rect.Left:=bndRect.x*aKoeff;
        FCurrentResult.Objects[i].rect.Width:=bndRect.width*aKoeff;
        FCurrentResult.Objects[i].rect.Top:=(imgSize.height- bndRect.y-bndRect.height)*aKoeff;
        FCurrentResult.Objects[i].rect.Height:=bndRect.height*aKoeff;
        inc(i);
      end;

      contour := contour.h_next;
    end;

    SetLength(FCurrentResult.Objects,i);
  finally
    cvReleaseMemStorage(storage);
  end;

  //cvShowImage('My Window', colourImage);
end;

procedure TVideoAnalytics.Init;
begin
  if FParameters.OtEnabled then
  begin
    if FParameters.OtEngine=otvOpenCV then
    begin
      //cvNamedWindow('My Window', CV_WINDOW_AUTOSIZE);
    end
    else begin
      if FHandle=nil then
        SvaCheck(sva_create(FHandle));
    end;
  end;
end;

function SvaInternalEventTypeToMyEventType(const aSvaEvent: integer): TVaEventType; inline;
begin
  result:=TVaEventType(-1);
  case aSvaEvent of
    SVA_EVENT_TYPE_OBJECT_OUT: result:=vaevOBJECT_OUT; // объект покинул сцену.
    SVA_EVENT_TYPE_INTRUSION: result:=vaevINTRUSION; // новый объект на сцене.
    SVA_EVENT_TYPE_SABOTAGE: result:=vaevSABOTAGE; // сцена слишком сильно изменилась – возможно кто-то или что-то загораживает камеру.
    SVA_EVENT_TYPE_LONG_UNSTABLE: result:=vaevLONG_UNSTABLE; // длительное время на сцене наблюдается нестабильность – что-то не в порядке.

  {/// Bad illuminance: }
    SVA_EVENT_TYPE_BLACKOUT: result:=vaevBLACKOUT; // слишком темно
    SVA_EVENT_TYPE_OVEREXPOSURE: result:=vaevOVEREXPOSURE; // слишком яркое освещение
    SVA_EVENT_TYPE_DEFOCUSING: result:=vaevDEFOCUSING; // камера расфокусирована

  {/// Antishaker2: }
    SVA_EVENT_TYPE_UNSTABILIZABLE: result:=vaevUNSTABILIZABLE; // цифровой стабилизатор не может стабилизировать изображение (слишком сильная тряска камеры).

  {/// NoiseFilter: }
    SVA_EVENT_TYPE_TOO_LARGE_NOISE: result:=vaevTOO_LARGE_NOISE; // слишком сильный шум на изображении

  {/// RuleEngine: }
    SVA_EVENT_TYPE_OBJECT_ENTER: result:=vaevOBJECT_ENTER; // сработало правило «Объект вошел в зону»
    SVA_EVENT_TYPE_OBJECT_ESCAPE: result:=vaevOBJECT_ESCAPE; // сработало правило «Объект вышел из зоны»
    SVA_EVENT_TYPE_OBJECT_ILLEGAL_DIRECTION: result:=vaevOBJECT_ILLEGAL_DIRECTION; // сработало правило «Объект движется в запрещенном направлении в зоне» (правило не работает и в будущем будет исключено)
    SVA_EVENT_TYPE_OBJECT_STAY_OVER_TIME: result:=vaevOBJECT_STAY_OVER_TIME; // сработало правило «Объект в зоне находится больше положенного времени»
    SVA_EVENT_TYPE_OBJECT_RUNNING: result:=vaevOBJECT_RUNNING; // сработало правило «Объект превышает допустимую скорость в зоне» (правило не работает и в будущем будет исключено)
    SVA_EVENT_TYPE_OBJECT_LOITERING: result:=vaevOBJECT_LOITERING; // сработало правило «Объект занимается праздношатанием» (правило не работает и в будущем будет исключено)
    SVA_EVENT_TYPE_TRIPWIRE_ALARM: result:=vaevTRIPWIRE_ALARM; // сработало правило «Объект пересек сигнальную линию»

  {/// AbandonedDetector: }
    SVA_EVENT_TYPE_ABANDONED: result:=vaevABANDONED; // обнаружен оставленный предмет

  {/// BigObjectClassifier: }
    SVA_EVENT_TYPE_BIG_OBJECT: result:=vaevBIG_OBJECT; // обнаружен большой объект (поезд).
  end;
end;

procedure TVideoAnalytics.DrawCurrentResult(aDC: HDC; aDCWidth, aDCHeight: cardinal);
var
  aSvaObject: TVaObject;
  aSvaPosition: TVaPosition;
  i: Integer;
  j: Integer;
  kx,ky: double;
  aCanvas: TCanvas;
  b: boolean;
  w,h: integer;
  s,aObjectIdStr,aObjectTypeStr: string;
  aObjectRect: TRect;
  aObjectPoint: TPoint;
  k: Integer;
  aHist: TChannelHistogramm;
  aSquare: int64;

  b1,b2: boolean;
begin
  FCurrentResultLock.Enter;
  try
    b1:=FParameters.OtEnabled and (Length(FCurrentResult.Objects)>0);
    b2:=FParameters.LuEnabled and FParameters.LuDrawHistogramms;
    if not (b1 or b2) then
      exit;

    //kx:=1;//FCurrentResult.frame.Width/FCurrentResult.input_frame.Width;
    kx:=aDCWidth/FCurrentWidth;//kx*aDCWidth/FCurrentResult.frame.Width;

    //ky:=1;//FCurrentResult.frame.Height/FCurrentResult.input_frame.Height;
    ky:=aDCHeight/FCurrentHeight; //ky*aDCHeight/FCurrentResult.frame.Height;

    aCanvas:=TCanvas.Create;
    try
      aCanvas.Handle:=aDC;
      try
        if FParameters.LuEnabled then
        begin
          aCanvas.Pen.Color:=clWhite;
          aHist:=FCurrentResult.YHistogramm;

          //aK:=100/(FCurrentWidth*FCurrentHeight);
          //aSquare:=Round(aHist.Sum*aK);
          //aSquare:=;

          //aHist.Logarithm;
          aHist.Normalize(aDCHeight);
          aCanvas.MoveTo(0,aDCHeight);
          for i := 0 to High(aHist.Data) do
            aCanvas.LineTo(i,aDCHeight-aHist.Data[i]);

          aCanvas.TextOut(10,10,
            IntToStr(100*aHist.CountOf(10,FParameters.LuMinY) div 255)+'/'+
            IntToStr(100*aHist.CountOf(10,0,FParameters.LuMaxY) div 255));

          //RED

          aCanvas.Pen.Color:=clRed;
          aCanvas.Font.Color:=clRed;

          aHist:=FCurrentResult.RHistogramm;
          aSquare:=100*aHist.CountOf(10) div 255;
          aHist.Normalize(aDCHeight);
          aCanvas.MoveTo(0,aDCHeight);
          for i := 0 to High(aHist.Data) do
            aCanvas.LineTo(i,aDCHeight-aHist.Data[i]);
          aCanvas.TextOut(10,30,IntToStr(aSquare));

          //GREEN
          aCanvas.Pen.Color:=clGreen;
          aCanvas.Font.Color:=clGreen;
          aHist:=FCurrentResult.GHistogramm;
          aSquare:=100*aHist.CountOf(10) div 255;
          aHist.Normalize(aDCHeight);
          aCanvas.MoveTo(0,aDCHeight);
          for i := 0 to High(aHist.Data) do
            aCanvas.LineTo(i,aDCHeight-aHist.Data[i]);
          aCanvas.TextOut(10,50,IntToStr(aSquare));

          //BLUE
          aCanvas.Pen.Color:=clBlue;
          aCanvas.Font.Color:=clBlue;
          aHist:=FCurrentResult.BHistogramm;
          aSquare:=100*aHist.CountOf(10) div 255;
          aHist.Normalize(aDCHeight);
          aCanvas.MoveTo(0,aDCHeight);
          for i := 0 to High(aHist.Data) do
            aCanvas.LineTo(i,aDCHeight-aHist.Data[i]);
          aCanvas.TextOut(10,70,IntToStr(aSquare));
        end;

        if FParameters.OtEnabled  then
        begin
          for i := 0 to High(FCurrentResult.Objects) do
          begin
            aSvaObject:=FCurrentResult.objects[i];

            if FCurrentReversedVertical then
              aObjectRect:=Rect(aSvaObject.rect.left,aSvaObject.rect.top,aSvaObject.rect.right,aSvaObject.rect.bottom)
            else
              aObjectRect:=Rect(aSvaObject.rect.left,FCurrentHeight-aSvaObject.rect.bottom,aSvaObject.rect.right,FCurrentHeight-aSvaObject.rect.top);

            b:=true;
            for j := 0 to High(FParameters.OtFilters) do
            begin
              if FParameters.OtFilters[j].FilterType=vaftTresholds then
              begin
                w:=Abs(aObjectRect.left-aObjectRect.right);
                h:=Abs(aObjectRect.top-aObjectRect.bottom);
                //Пока только первый фильтр. Остальные на будущее
                if FParameters.OtFilters[0].MinSquarePx<>0 then
                begin
                  if w*h<FParameters.OtFilters[0].MinSquarePx then
                    b:=false;
                end;

                if b and (FParameters.OtFilters[0].MinWidthPx<>0) then
                  if w<FParameters.OtFilters[0].MinWidthPx then
                    b:=false;

                if b and (FParameters.OtFilters[0].MinHeightPx<>0) then
                  if h<FParameters.OtFilters[0].MinHeightPx then
                    b:=false;

                if b and (FParameters.OtFilters[0].MinPeriodMs>0) then
                  if (aSvaObject.position.time-aSvaObject.start_position.time)*1000<FParameters.OtFilters[0].MinPeriodMs then
                    b:=false;
              end
              else if FParameters.OtFilters[j].FilterType=vaftEvents then
              begin
                b:=false;

                for k := 0 to High(FCurrentResult.Events) do
                  if FCurrentResult.Events[k].object_id=aSvaObject.id then
                  begin
                    if FCurrentResult.Events[k].type_ in FParameters.OtFilters[j].Events then
                    begin
                      b:=true;
                      break;
                    end;
                  end;
              end;

              if not b then
                break;
            end;

            aCanvas.Brush.Color:=clWebOrange;

            for j := 0 to High(FCurrentResult.Events) do
            begin
              if (FCurrentResult.events[j].object_id = aSvaObject.id) then
              begin
                aCanvas.Brush.Color:=clRed;
                break;
              end;
            end;


            if not b then
            begin
              if FParameters.OtFiltersVisualizationMode=vafvmHide then
                continue
              else begin
                aCanvas.Brush.Color:=clGray;
              end;
            end;

            aCanvas.FrameRect(
             Rect(
                Round(aObjectRect.left*kx),
                Round(aObjectRect.top*ky),
                Round(aObjectRect.right*kx),
                Round(aObjectRect.bottom*ky)));

            if FParameters.OtDrawTrajectory then
              for j := 0 to High(aSvaObject.trajectory) do
              begin
                aSvaPosition:=aSvaObject.trajectory[j];
                if FCurrentReversedVertical then
                  aObjectPoint:=Point(Round(aSvaPosition.point.x*kx),Round(aSvaPosition.point.y*ky))
                else
                  aObjectPoint:=Point(Round(aSvaPosition.point.x*kx),Round((FCurrentHeight-aSvaPosition.point.y)*ky));

                aCanvas.Pixels[aObjectPoint.X,aObjectPoint.Y]:=aCanvas.Brush.Color;
              end;

            if FParameters.OtDrawIdentifiers or FParameters.OtDrawObjectTypes then
            begin
              s:='';aObjectIdStr:='';aObjectTypeStr:='';

              if FParameters.OtDrawIdentifiers then
                s:=IntToStr(aSvaObject.id);
              if FParameters.OtDrawObjectTypes then
              begin
                case aSvaObject.type_ of
                  SVA_OBJECT_TYPE_EMPTY: ;  // неинициализированный объект - служебный тип (объект такого типа не должен появляться снаружи библиотеки).
                  SVA_OBJECT_TYPE_STATIC: ; // кандидат в объекты - служебный тип (объект такого типа не должен появляться снаружи библиотеки).
                  SVA_OBJECT_TYPE_MOVING: aObjectTypeStr:=''; // движущийся объект (основной объект распознавания).
                  SVA_OBJECT_TYPE_ABANDONED: aObjectTypeStr:='О'; // оставленный предмет.
                  SVA_OBJECT_TYPE_BIGOBJECT: aObjectTypeStr:='Б'; // большой объект, например, поезд.
                  SVA_OBJECT_TYPE_INGROWN : ;// объект, который необходимо уничтожить на следующем кадре - служебный тип (объект такого типа не должен появляться снаружи библиотеки).
                end;
              end;

              if aObjectIdStr<>'' then
                s:=aObjectIdStr;
              if aObjectTypeStr<>'' then
              begin
                if s<>'' then
                  s:=s+' ';
                s:=s+aObjectTypeStr;
              end;

              aCanvas.TextOut(Round(aObjectRect.left*kx),Round(aObjectRect.top*ky),s);
            end;

          end;
        end;
      finally
        aCanvas.Handle:=0;
      end;
    finally
      aCanvas.Free;
    end;
  finally
    FCurrentResultLock.Leave;
  end;
end;

function TVideoAnalytics.GetCurrentResultCopy: TVaProcessingResult;
begin
  CurrentResultLock;
  try
    result.Objects:=Copy(FCurrentResult.Objects,0,Length(FCurrentResult.Objects));
    result.Events:=Copy(FCurrentResult.Events,0,Length(FCurrentResult.Events));
  finally
    CurrentResultUnlock;
  end;
end;

class procedure TVideoAnalytics.InitnializeEnvironment;
begin
  SvaEnsureLibraryLoaded;
end;

{ TVaParameters }

procedure TVaParameters.Clear;
begin
  OtFilters:=nil;
  OtDrawTrajectory:=true;
  OtDrawIdentifiers:=true;
  OtDrawObjectTypes:=false;
  AsyncProcessing:=true;
end;

{ TVaObjectHelper }

procedure CopyPosition(const aSrc: sva_position_t; var aDst: TVaPosition); inline;
begin
  aDst.point.x:=aSrc.point.x;
  aDst.point.y:=aSrc.point.y;
  aDst.time:=aSrc.time;
end;


procedure TVaObjectHelper.CopyFrom(const aSvaObject: sva_object_t);
var
  i: Integer;
begin
  self.id:=aSvaObject.id;
  self.type_:=aSvaObject.type_;
  CopyPosition(aSvaObject.position,self.position);

  self.rect.Create(aSvaObject.rect.left,aSvaObject.rect.top,aSvaObject.rect.right,aSvaObject.rect.bottom);
  CopyPosition(aSvaObject.start_position,self.start_position);

  SetLength(self.trajectory,aSvaObject.trajectory.positions_count);
  for i := 0 to High(self.trajectory) do
    CopyPosition(aSvaObject.trajectory.positions[i],self.trajectory[i]);

  self.mask_index:=aSvaObject.mask_index;
  self.mask_rect.Create(aSvaObject.mask_rect.left,aSvaObject.mask_rect.top,aSvaObject.mask_rect.right,aSvaObject.mask_rect.bottom);
end;

{ TVaEventHelper }

procedure TVaEventHelper.CopyFrom(const aSvaEvent: sva_event_t);
begin
  self.type_:=SvaInternalEventTypeToMyEventType(aSvaEvent.type_);
  self.level:=aSvaEvent.level;
  self.object_id:=aSvaEvent.object_id;
  self.rule_id:=aSvaEvent.rule_id;
  self.description:=aSvaEvent.description;
end;

{ TProcessThread }

procedure TProcessThread.Add(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal);
begin
  if FCurrentProcessingBufferIndex=0 then
  begin
    if FCurrentWrittenBufferIndex=1 then
      OnBufferOverflow;

    FBuffer[1].Init(aFormat,aData,aDataSize,aInfo,aInfoSize);
    FCurrentWrittenBufferIndex:=1;
  end
  else begin
    if FCurrentWrittenBufferIndex=0 then
      OnBufferOverflow;

    FBuffer[0].Init(aFormat,aData,aDataSize,aInfo,aInfoSize);
    FCurrentWrittenBufferIndex:=0;
  end;
  FEvent.SetEvent;
end;

constructor TProcessThread.Create(aOwner: TVideoAnalytics);
var
  i: Integer;
begin
  FCurrentProcessingBufferIndex:=-1;
  FCurrentWrittenBufferIndex:=-1;
  FOwner:=aOwner;
  FFrameLock:=TCriticalSection.Create;
  FEvent:=TEvent.Create(nil,false,false,'');

  for i := 0 to High(FBuffer) do
    FBuffer[i]:=TFrame.Create;

  inherited Create(false);
end;

destructor TProcessThread.Destroy;
var
  i: Integer;
begin
  Terminate;
  if FEvent<>nil then
    FEvent.SetEvent;

  inherited;

  FreeAndNil(FEvent);
  FreeAndNil(FFrameLock);
  for i := 0 to High(FBuffer) do
    FBuffer[i].Free;
end;

procedure TProcessThread.Execute;
const
  aMethodName = 'TProcessThread.Execute';
var
  aFrame:  TFrame;
begin
  SetCurrentThreadName('VideoAnalytics: '+ClassName);
  while not Terminated do
  begin
    if FEvent.WaitFor(INFINITE)=wrSignaled then
    begin
      if Terminated then
        break;

      try
        FCurrentProcessingBufferIndex:=FCurrentWrittenBufferIndex;
        Assert(FCurrentProcessingBufferIndex in [0..1]);

        aFrame:=FBuffer[FCurrentProcessingBufferIndex];
        aFrame.Lock;
        try
          FOwner.ProcessFrameInternal(
            aFrame.FFormat,
            @aFrame.FData[0],
            Length(aFrame.FData),
            nil,
            0);
        finally
          aFrame.Unlock;
        end;
      except
        on E:Exception do
          TWorkspaceBase.Current.HandleException(self,E,aMethodName);
      end;
    end;
  end;
end;

procedure TProcessThread.OnBufferOverflow;
begin
  if Assigned(FOwner.FOnTrace) then
    FOwner.FOnTrace(FOwner,'Обнаружено переполнение буфера, видеоаналитика не успевает обрабатывать входящий поток, кадр будет пропущен');
end;

{ TFrame }

constructor TFrame.Create;
begin
  FLock:=TCriticalSection.Create;
end;

destructor TFrame.Destroy;
begin
  FreeAndNil(FLock);
  FData:=nil;
  inherited;
end;

procedure TFrame.Init(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer;
  aInfoSize: cardinal);
begin
  Lock;
  try
    SetLength(FData,aDataSize);
    if aDataSize<>0 then
     CopyMemory(@FData[0],aData,aDataSize);

    FFormat:=aFormat;
  finally
    Unlock;
  end;
end;

procedure TFrame.Lock;
begin
  FLock.Enter;
end;

procedure TFrame.Unlock;
begin
  FLock.Leave;
end;

end.
