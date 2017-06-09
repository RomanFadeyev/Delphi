{-------------------------------------------------------------------------------

   Объектная модель проигрывателя (см. HH5PlayerSDK.dll).

 -------------------------------------------------------------------------------}

unit Player.Control;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface
  uses Messages, Windows, Forms, Graphics, SysUtils, Controls, Classes, FileCtrl, SyncObjs, Contnrs,
  Player.VideoOutput.Base,Player.VideoOutput.AllTypes,  Player.AudioOutput.Base,MediaProcessing.Definitions,MediaProcessing.VideoAnalytics.Definitions;

const
  CM_IMAGESIZECHANGED = CM_BASE+300;

type
  TAudioVolume = 0..100;

  TWindowPlayerBase = class;
  TWindowPlayerDrawFrameEvent = procedure (Sender: TWindowPlayerBase; aDC: HDC; aDCWidth,aDCHeight: cardinal) of object;
  TWindowPlayerBeforeDisplayFrameEvent = procedure (Sender: TWindowPlayerBase; var aFormat: TMediaStreamDataHeader; var aData: pointer; var aDataSize: cardinal; var aInfo: pointer; var aInfoSize: cardinal) of object;
  TWindowPlayerVideCanvasResizeEvent = procedure (Sender: TWindowPlayerBase) of object;
  TWindowPlayerDblClickEvent = procedure (Sender: TWindowPlayerBase) of object;
  TWindowPlayerClickEvent = procedure (Sender: TWindowPlayerBase) of object;

  //Плейер совместо с окном для отображения
  TWindowPlayerBase = class
  private
    FControl : TWinControl;
    FContainer : TWinControl;

    FAudioOutputLock: TCriticalSection;
    FAudioEnabled: boolean;

    FVideoOutputClass : TPlayerVideoOutputClass;
    FAudioOutputClass : TPlayerAudioOutputClass;
    FVideoOutput  : TPlayerVideoOutput;
    FAudioOutput  : TPlayerAudioOutput;
    FFrameDisplayedCount : integer;
    FFrameSoundedCount : integer;
    FKeepAspectRatio: Boolean;
    FUpdatingBounds: integer;
    FResizingVideoOutput: integer;
    FProcessFrameLock: TCriticalSection;

    FAudioVolume: TAudioVolume;
    FOnDrawFrame: TWindowPlayerDrawFrameEvent;
    FEnabled: boolean;

    FVaLock: TCriticalSection;
    FVaObjects: TVaObjectArray;
    FVaEvents: TVaEventArray;
    FVaFormat: TMediaStreamDataHeader;

    FOnBeforeDisplayFrame: TWindowPlayerBeforeDisplayFrameEvent;
    FOnVideoCanvasResized: TWindowPlayerVideCanvasResizeEvent;
    FOnDblClick: TWindowPlayerDblClickEvent;
    FOnClick: TWindowPlayerClickEvent;

    procedure Control_CMImageSizeChanged(var Message: TWMNoParams); message CM_IMAGESIZECHANGED;
    procedure Control_WMMove(var Message: TWMMove); message WM_MOVE;
    procedure Control_WMSize(var Message: TWMSize); message WM_SIZE;
    procedure Control_WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure Control_WMCreate(var Message: TWMCreate); message WM_CREATE;
    procedure Control_WMDisplayChange(var Message: TWMDisplayChange); message WM_DISPLAYCHANGE;
    procedure Control_WMDblClick(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure Control_WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;

    procedure Control_WndProc(var Message: TMessage);
    procedure Container_WndProc(var Message: TMessage);

    function  GetAudioEnabled: boolean;
    procedure SetAudioEnabled(const Value: boolean);
    //procedure WaitForImageReady(aTimeout: cardinal);

    procedure SetKeepAspectRatio(const Value: Boolean);
    function  CanKeepAspectRatio:boolean;
    procedure SetAudioVolume(const Value: TAudioVolume);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetOnDrawFrame(const Value: TWindowPlayerDrawFrameEvent);
    procedure SetEnabled(const Value: boolean);
  protected
    procedure ApplyAudioVolume;
    procedure Reset;

    procedure OnControlDestroying; virtual;
    procedure OnControlDestroyed; virtual;

    procedure OnPlayerOutputDrawFrame(Sender: TPlayerVideoOutput; aDC: HDC; aDCWidth,aDCHeight: cardinal);
    procedure OnPlayerOutputImageSizeChanged(Sender: TPlayerVideoOutput);
  public
    constructor Create(aVideoOutputClass: TPlayerVideoOutputClass; aAudioOutputClass: TPlayerAudioOutputClass; aControlClass: TWinControlClass=nil);
    destructor  Destroy; override;

    procedure ProcessFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);

    procedure UpdateImage;
    procedure UpdateBounds;

//    procedure RedrawCurrentVideoFrame;

    //Показывает ли что-то сейчас плейер
    function  IsDisplayingNow: boolean; virtual;

    //получение снимков текущего кадра
    procedure CaptureCurrentFrame(const aFileName: string); overload;
    function  CaptureCurrentFrame: TBitmap; overload;

    function StatusInfo: string;

    property VideoOutput  : TPlayerVideoOutput read FVideoOutput;
    property AudioOutput  : TPlayerAudioOutput read FAudioOutput;


    //Реагировать на поступающие данные. Если false, данные игнорируются
    //(не выполняется их декодировка и отображение)
    property Enabled: boolean read FEnabled write SetEnabled default true;

    property VideoCanvas: TWinControl read FControl;
    property Control: TWinControl read FContainer;
    property AudioEnabled: boolean read GetAudioEnabled write SetAudioEnabled;
    property AudioVolume: TAudioVolume read FAudioVolume write SetAudioVolume;

    property BackgroundColor : TColor read GetBackgroundColor write SetBackgroundColor;
    property KeepAspectRatio: Boolean read FKeepAspectRatio write SetKeepAspectRatio;


    property OnDblClick: TWindowPlayerDblClickEvent read FOnDblClick write FOnDblClick;
    property OnClick:TWindowPlayerClickEvent read FOnClick write FOnClick;
    property OnBeforeDisplayFrame: TWindowPlayerBeforeDisplayFrameEvent read FOnBeforeDisplayFrame write FOnBeforeDisplayFrame;
    property OnDrawFrame: TWindowPlayerDrawFrameEvent read FOnDrawFrame write SetOnDrawFrame;
    property OnVideoCanvasResized: TWindowPlayerVideCanvasResizeEvent read FOnVideoCanvasResized write FOnVideoCanvasResized;
  end;


type
  TWindowStreamPlayer = class;

  //Класс для проигрывания потокового видео
  TWindowStreamPlayer = class (TWindowPlayerBase)
  private
    procedure Control_WMPaint(var Message: TWMPaint); message WM_PAINT;

    function  GetSynchronousDisplay: boolean;
    procedure SetSynchronousDisplay(const Value: boolean);
  public
    property SynchronousDisplay : boolean read GetSynchronousDisplay write SetSynchronousDisplay;
  end;


  //Базовый тип исключений плейера
  EHHPlayerException = class (Exception);

implementation
  uses uBaseClasses,uTrace;
type
  TFriendWinControl=class (TWinControl);

  TPlayerWinControl = class(TWinControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;




{ TWindowPlayerBase }

constructor TWindowPlayerBase.Create(aVideoOutputClass: TPlayerVideoOutputClass; aAudioOutputClass: TPlayerAudioOutputClass; aControlClass: TWinControlClass);
begin
  inherited Create;

  if (aVideoOutputClass=nil) then //По умолчанию DirectX
    aVideoOutputClass:=TPlayerVideoOutputDirectX;

  Assert(aVideoOutputClass<>nil);

  FProcessFrameLock:=TCriticalSection.Create;
  FEnabled:=true;
  FAudioEnabled:=true;
  FAudioVolume:=100;
  FAudioOutputLock:=TCriticalSection.Create;
  FVideoOutputClass:=aVideoOutputClass;
  FAudioOutputClass:=aAudioOutputClass;
  FVaLock:=TCriticalSection.Create;

  if aControlClass=nil then
    aControlClass:=TPlayerWinControl;

  FContainer:=TPlayerWinControl.Create(nil);
  FContainer.WindowProc:=Container_WndProc;
  FContainer.Align:=alClient;

  FControl:=aControlClass.Create(nil);
  FControl.WindowProc:=Control_WndProc;
  FControl.Parent:=FContainer;
  {$IFNDEF VER130}
  TFriendWinControl(FControl).ParentBackground:=false;
  {$ENDIF}


  FVideoOutput:=FVideoOutputClass.Create(TFriendWinControl(FControl).WindowHandle);
  FVideoOutput.WaitOnOutOfBuffer:=false;
  FVideoOutput.OnImageSizeChanged:=OnPlayerOutputImageSizeChanged;

  if FAudioOutputClass<>nil then
  begin
    FAudioOutput:=FAudioOutputClass.Create;
  end;

  BackgroundColor:=clBlack;
end;

destructor TWindowPlayerBase.Destroy;
begin
  FreeAndNil(FVideoOutput);
  FreeAndNil(FAudioOutput);

  FControl.Free;
  FControl:=nil; //Не использовать FreeAndNil из-за петли в WindowProc

  FContainer.Free;
  FContainer:=nil;
  inherited;

  FreeAndNil(FProcessFrameLock);
  FreeAndNil(FAudioOutputLock);
end;

procedure TWindowPlayerBase.UpdateImage;
begin
  if FVideoOutput.VideoContextAllocated  then
  begin
    FVideoOutput.UpdateBounds;
    FVideoOutput.UpdateImage;
  end;
end;


procedure TWindowPlayerBase.Control_CMImageSizeChanged(
  var Message: TWMNoParams);
begin
  inc(FResizingVideoOutput);
  try
    UpdateBounds;
  finally
    dec(FResizingVideoOutput);
  end;
end;

procedure TWindowPlayerBase.Control_WMCreate(var Message: TWMCreate);
begin
  FVideoOutput.WindowHandle:=TFriendWinControl(FControl).WindowHandle;
  Assert(FVideoOutput.WindowHandle<>0);
end;

procedure TWindowPlayerBase.Control_WMDblClick(var Message: TWMLButtonDblClk);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(self);
end;

procedure TWindowPlayerBase.Control_WMDestroy(var Message: TWMDestroy);
begin
  if FVideoOutput<>nil then //Это нормальная ситуация. При разрушении объекта сначала разрушается VideoOutput
    FVideoOutput.WindowHandle:=0;
end;

procedure TWindowPlayerBase.Control_WMDisplayChange(var Message: TWMDisplayChange);
begin
  //Пересоздадим контекст рисования
  FVideoOutput.DeallocateVideoContext;
end;

procedure TWindowPlayerBase.Control_WMLButtonUp(var Message: TWMLButtonUp);
begin
  if Assigned(FOnClick) then
    FOnClick(self);
end;

procedure TWindowPlayerBase.Control_WMMove(var Message: TWMMove);
begin
  if FUpdatingBounds=0 then
    UpdateBounds;
end;

procedure TWindowPlayerBase.Control_WMSize(var Message: TWMSize);
begin
  if FUpdatingBounds=0 then
    UpdateBounds;

  if Assigned(FOnVideoCanvasResized) then
    FOnVideoCanvasResized(self);

end;

procedure TWindowPlayerBase.Control_WndProc(var Message: TMessage);
begin
  if Message.Msg=WM_DESTROY then
    OnControlDestroying;

  //Ничего не делаем. Все будет рисовать DirectDraw
  if (Message.Msg=WM_ERASEBKGND) and IsDisplayingNow then
  begin
    DefWindowProc(FControl.Handle,Message.Msg,Message.WParam,Message.LParam);
  end
  //Ничего не делаем. Все будет рисовать DirectDraw
  else if (Message.Msg=WM_PAINT) and IsDisplayingNow then
  begin
    DefWindowProc(FControl.Handle,Message.Msg,Message.WParam,Message.LParam);
  end
  else
    TFriendWinControl(FControl).WndProc(Message);

  Dispatch(Message);

  if Message.Msg=WM_DESTROY then
    OnControlDestroyed;
end;


procedure TWindowPlayerBase.ProcessFrame(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
var
  aFormat2: TMediaStreamDataHeader;

  i: Integer;
  aStream: TMemoryStreamMediator;
begin
  if not FEnabled then
    exit;

  FProcessFrameLock.Enter; //Защишаемся от вызовы ProcessFrame из разных потоков одновременно
  try
    if Assigned(FOnBeforeDisplayFrame) then
    begin
      aFormat2:=aFormat;
      FOnBeforeDisplayFrame(self,aFormat2,aData,aDataSize,aInfo,aInfoSize);
    end;

    if (aData<>nil) and (aDataSize<>0) then
    begin
      if aFormat.biMediaType=mtVideo then
      begin
        FVideoOutput.WriteVideoData(aFormat,aData,aDataSize,aInfo,aInfoSize);
        inc(FFrameDisplayedCount);
      end
      else if (aFormat.biMediaType=mtAudio) then
      begin
        if (FAudioOutput<>nil) and (FAudioEnabled)  then
        begin
          FAudioOutput.WriteAudioData(aFormat,aData,aDataSize,aInfo,aInfoSize);
          inc(FFrameSoundedCount);
        end;
      end
      else if (aFormat.biMediaType=mtSysData) and (aFormat.biStreamType=stVideoAnalytics) then
      begin
        FVaLock.TryEnterOrRaise(10000);
        try
          aStream:=TMemoryStreamMediator.Create;
          try
            aStream.Init(aData,aDataSize);
            aStream.ReadInteger(i);
            SetLength(FVaObjects,i);

            for i := 0 to High(FVaObjects) do
              FVaObjects[i].Load(aStream);

            aStream.ReadInteger(i);
            SetLength(FVaEvents,i);
            for i := 0 to High(FVaEvents) do
            begin
              FVaEvents[i].Load(aStream);
              TraceLine('DrawAnalytics','Event: '+VaFilterEventNames[FVaEvents[i].type_]);
            end;
          finally
            aStream.Free;
          end;

          FVaFormat:=aFormat;
        finally
          FVaLock.Leave;
        end;

        if not Assigned(FVideoOutput.OnDrawFrameImage) then
          FVideoOutput.OnDrawFrameImage:=OnPlayerOutputDrawFrame;

        UpdateImage;
      end
      else
      begin
        //Assert(false);
      end;
    end;
  finally
    FProcessFrameLock.Leave;
  end;
end;

procedure TWindowPlayerBase.OnControlDestroying;
begin

end;

procedure TWindowPlayerBase.OnControlDestroyed;
begin
  Reset;
end;

function TWindowPlayerBase.GetAudioEnabled: boolean;
begin
  result:=FAudioEnabled;
end;

procedure TWindowPlayerBase.SetAudioEnabled(const Value: boolean);
begin
  FAudioOutputLock.Enter;
  try
    FAudioEnabled:=Value;
  finally
    FAudioOutputLock.Leave;
  end;

  ApplyAudioVolume;
end;

procedure TWindowPlayerBase.Reset;
begin
  FFrameDisplayedCount:=0;
  FFrameSoundedCount:=0;
end;

function TWindowPlayerBase.IsDisplayingNow: boolean;
begin
  result:=(FVideoOutput<>nil) and (FFrameDisplayedCount>0) and (FVideoOutput.VideoContextAllocated);
end;

procedure TWindowPlayerBase.CaptureCurrentFrame(const aFileName: string);
var
  aStream: TFileStream;
begin
  aStream:=TFileStream.Create(aFileName,fmCreate);
  try
    FVideoOutput.CaptureCurrentImageToStreamAsBitmap(aStream);
  finally
    aStream.Free;
  end;
end;

function TWindowPlayerBase.CaptureCurrentFrame: TBitmap;
begin
  result:=TBitmap.Create;
  try
    FVideoOutput.CaptureCurrentImageToBitmap(result);
  except
    result.Free;
    raise;
  end;
end;

procedure TWindowPlayerBase.UpdateBounds;
var
  aPlayerRatio: Real;
  aCanvasRatio: Real;
  aNewWidth,aNewHeight: integer;
begin
  inc(FUpdatingBounds);

  try
    if not CanKeepAspectRatio then
    begin
      FControl.SetBounds(0,0,FContainer.Width,FContainer.Height);
    end
    else begin
      aPlayerRatio := FVideoOutput.VideoWidth / FVideoOutput.VideoHeight;

      //Скорее всего, это interlace. Нужно увеличить высоту в 2 раза
      if (FVideoOutput.VideoWidth=704) and (FVideoOutput.VideoHeight=288) then
        aPlayerRatio:=aPlayerRatio/2;

      aCanvasRatio := FContainer.Width / FContainer.Height;

      (*if (Round(aPlayerRatio * 100) = Round(aCanvasRatio * 100)) then
      begin
        //ничего делать не надо
      end
      else
      begin *)

      if aPlayerRatio > aCanvasRatio then
      begin
        aNewWidth:=FContainer.Width;
        // уменьшаем высоту
        aNewHeight:=Round(FContainer.Width / aPlayerRatio);
      end
      else begin
        // уменьшаем ширину
        aNewWidth:=Round(FContainer.Height * aPlayerRatio);
        aNewHeight:=FControl.Parent.Height;
      end;
      FControl.SetBounds((FContainer.Width - aNewWidth) div 2,
                         (FContainer.Height - aNewHeight) div 2,
                         aNewWidth,aNewHeight);
    end;

    if FResizingVideoOutput=0 then
      FVideoOutput.UpdateBounds;
    if Assigned(FOnVideoCanvasResized) then
      FOnVideoCanvasResized(self);
  finally
    dec(FUpdatingBounds);
  end;
end;

function TWindowPlayerBase.CanKeepAspectRatio: boolean;
begin
  result:=(FKeepAspectRatio) and
          (FVideoOutput<>nil) and
          (FControl<>nil) and
          (FVideoOutput.VideoWidth<>0) and
          (FVideoOutput.VideoHeight<>0) and
          (self.FControl.Height>0);
end;

procedure TWindowPlayerBase.SetKeepAspectRatio(const Value: Boolean);
begin
  FKeepAspectRatio := Value;
  UpdateBounds;
end;

procedure TWindowPlayerBase.Container_WndProc(var Message: TMessage);
//var
//  r: TRect;
begin

  //07.07.2011 Закомментировал, потому что при изменении размеров остаются неочищенные области
  //Не перерисовываем в родителе ту область, которую закрывает плейер
  {
  if (Message.Msg=WM_ERASEBKGND) and (FControl<>nil) and (FControl.Parent=FContainer) and IsDisplayingNow then
  begin
    r:=FControl.BoundsRect;
    ExcludeClipRect(TWMEraseBkgnd(Message).DC,r.Left,r.Top,r.Right,r.Bottom);
  end;
  }

  TFriendWinControl(FContainer).WndProc(Message);
  if Message.Msg=WM_SIZE then
  begin
    if FUpdatingBounds=0 then
      UpdateBounds
  end
  else if Message.Msg=WM_LBUTTONUP then
  begin
    if Assigned(FOnClick) then
      FOnClick(self);
  end;
end;

procedure TWindowPlayerBase.SetAudioVolume(const Value: TAudioVolume);
begin
  if FAudioVolume=Value then
    exit;

  FAudioVolume := Value;
  ApplyAudioVolume;
end;

procedure TWindowPlayerBase.ApplyAudioVolume;
begin
  if (FAudioOutput<>nil) then
  begin
    FAudioOutput.Enabled:=FAudioEnabled;
    FAudioOutput.Volume:=FAudioVolume;
  end;
end;

function TWindowPlayerBase.GetBackgroundColor: TColor;
begin
  result:=TFriendWinControl(FControl).Color;
end;

procedure TWindowPlayerBase.SetBackgroundColor(const Value: TColor);
begin
  TFriendWinControl(FControl).Color:=Value;
  TFriendWinControl(FContainer).Color:=Value;
  FVideoOutput.BackgroundColor:=Value;
end;

procedure TWindowPlayerBase.SetEnabled(const Value: boolean);
begin
  InterlockedExchange(PInteger(@FEnabled)^,integer(Value));
  if not Value then
    FVideoOutput.ResetBuffer;
end;

procedure TWindowPlayerBase.SetOnDrawFrame(const Value: TWindowPlayerDrawFrameEvent);
begin
  if CompareMem(@TMethod(FOnDrawFrame),@TMethod(Value),sizeof(TMethod)) then
    exit;

  FOnDrawFrame := Value;
  if Assigned(FOnDrawFrame) then
    FVideoOutput.OnDrawFrameImage:=OnPlayerOutputDrawFrame
  else
    FVideoOutput.OnDrawFrameImage:=nil;
end;

function TWindowPlayerBase.StatusInfo: string;
begin
  result:=Format('Player Type:%s'#13#10+
  'Video'#13#10'%s'#13#10+
  'Audio'#13#10'%s'#13#10,
  [ClassName,Trim(FVideoOutput.StatusInfo),Trim(FAudioOutput.StatusInfo)]);
end;

procedure DrawVideoAnalytics(aDC:HDC; aDCWidth, aDCHeight: cardinal; const aFormat: TMediaStreamDataHeader; aObjects: TVaObjectArray; aEvents: TVaEventArray);
var
  aSvaObject: TVaObject;
  aSvaPosition: TVaPosition;
  i: Integer;
  j: Integer;
  kx,ky: double;
  aCanvas: TCanvas;
  b: boolean;
  aObjectRect: TRect;
  aObjectPoint: TPoint;
begin
  if (Length(aObjects)=0) then
    exit;

  //kx:=1;//FCurrentResult.frame.Width/FCurrentResult.input_frame.Width;
  kx:=aDCWidth/aFormat.VideoWidth;//kx*aDCWidth/FCurrentResult.frame.Width;

  //ky:=1;//FCurrentResult.frame.Height/FCurrentResult.input_frame.Height;
  ky:=aDCHeight/aFormat.VideoHeight; //ky*aDCHeight/FCurrentResult.frame.Height;

  aCanvas:=TCanvas.Create;
  try
    aCanvas.Handle:=aDC;
    try
      for i := 0 to High(aObjects) do
      begin
        aSvaObject:=aobjects[i];

        if not aFormat.VideoReversedVertical then
          aObjectRect:=Rect(aSvaObject.rect.left,aSvaObject.rect.top,aSvaObject.rect.right,aSvaObject.rect.bottom)
        else
          aObjectRect:=Rect(aSvaObject.rect.left,aFormat.VideoHeight-aSvaObject.rect.bottom,aSvaObject.rect.right,aFormat.VideoHeight-aSvaObject.rect.top);

        b:=true;

        aCanvas.Brush.Color:=clWebOrange;

        for j := 0 to High(aEvents) do
        begin
          if (aevents[j].object_id = aSvaObject.id) then
          begin
            aCanvas.Brush.Color:=clRed;
            break;
          end;
        end;


        if not b then
        begin
          //if FParameters.FiltersVisualizationMode=vafvmHide then
          //  continue
          //else begin
            aCanvas.Brush.Color:=clGray;
          //end;
        end;

        aCanvas.FrameRect(
         Rect(
            Round(aObjectRect.left*kx),
            Round(aObjectRect.top*ky),
            Round(aObjectRect.right*kx),
            Round(aObjectRect.bottom*ky)));

        if {FParameters.DrawTrajectory} true then
          for j := 0 to High(aSvaObject.trajectory) do
          begin
            aSvaPosition:=aSvaObject.trajectory[j];
            if not aFormat.VideoReversedVertical then
              aObjectPoint:=Point(Round(aSvaPosition.point.x*kx),Round(aSvaPosition.point.y*ky))
            else
              aObjectPoint:=Point(Round(aSvaPosition.point.x*kx),Round((aFormat.VideoHeight-aSvaPosition.point.y)*ky));

            aCanvas.Pixels[aObjectPoint.X,aObjectPoint.Y]:=aCanvas.Brush.Color;
          end;

        (*
        if true FParameters.DrawIdentifiers or FParameters.DrawObjectTypes then
        begin
          s:='';aObjectIdStr:='';aObjectTypeStr:='';

          if FParameters.DrawIdentifiers then
            s:=IntToStr(aSvaObject.id);
          if FParameters.DrawObjectTypes then
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
        *)

      end;

      for i := 0 to High(aEvents) do
      begin
        if aEvents[i].type_ in [vaevBLACKOUT,vaevOVEREXPOSURE] then
        begin

          if aEvents[i].type_=vaevBLACKOUT then
            aCanvas.Pen.Color:=clRed
          else
            aCanvas.Pen.Color:=clWhite;

          aCanvas.MoveTo(0,0);
          aCanvas.LineTo(aDCWidth,aDCHeight);

          aCanvas.MoveTo(aDCWidth,0);
          aCanvas.LineTo(0,aDCHeight);
        end;
      end;

    finally
      aCanvas.Handle:=0;
    end;
  finally
    aCanvas.Free;
  end;
end;

procedure TWindowPlayerBase.OnPlayerOutputDrawFrame(Sender: TPlayerVideoOutput; aDC: HDC;aDCWidth,aDCHeight: cardinal);
begin
  FVaLock.TryEnterOrRaise(10000);
  try
    if Length(FVaObjects)+Length(FVaEvents)>0 then
      DrawVideoAnalytics(aDC,aDCWidth,aDCHeight,FVaFormat,FVaObjects,FVaEvents);
  finally
    FVaLock.Leave;
  end;

  if Assigned(FOnDrawFrame) then
    FOnDrawFrame(self,aDC,aDCWidth,aDCHeight);
end;


procedure TWindowPlayerBase.OnPlayerOutputImageSizeChanged(Sender: TPlayerVideoOutput);
var
  aRes: DWORD_PTR;
  aThreadId: DWORD;
begin
  aThreadId:=GetWindowThreadProcessId(TFriendWinControl(FControl).WindowHandle);
  if aThreadId=GetCurrentThreadId then
  begin
    TFriendWinControl(FControl).Perform(CM_IMAGESIZECHANGED,0,0);
  end
  else begin
    //21.12.2012 Достали взаимные блокировки. Попробуем на свой страх и риск отсылать в PostMessage
    if SendMessageTimeout(TFriendWinControl(FControl).WindowHandle,CM_IMAGESIZECHANGED,0,0,SMTO_BLOCK,100,@aRes)=0 then
      PostMessage(TFriendWinControl(FControl).WindowHandle,CM_IMAGESIZECHANGED,0,0);
  end;
end;


{ TWindowStreamPlayer }

procedure TWindowStreamPlayer.Control_WMPaint(var Message: TWMPaint);
begin
  UpdateImage;
end;

function TWindowStreamPlayer.GetSynchronousDisplay: boolean;
begin
  result:=FVideoOutput.SynchronousDisplay;
end;

procedure TWindowStreamPlayer.SetSynchronousDisplay(const Value: boolean);
begin
  FVideoOutput.SynchronousDisplay:=Value;
end;

{ TPlayerWinControl }

procedure TPlayerWinControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;



initialization
  RegisterCustomTrace('DrawAnalytics','','.voa');
end.


