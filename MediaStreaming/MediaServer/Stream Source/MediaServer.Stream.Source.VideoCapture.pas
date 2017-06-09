unit MediaServer.Stream.Source.VideoCapture;

interface
  uses Windows, SysUtils, Classes, SyncObjs, VFW,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  //Класс, выполняющий непосредственно получение данных (видеопотока) из камеры
  TMediaServerSourceCapture = class (TMediaServerSource)
  private
    FCaptureWnd: HWND;
    FCurrentBmpInfo: TBitmapInfo;

    procedure OnFrameReceived(aMediaType: TMediaType;
                              aData: pointer; aDataSize:cardinal;
                              const aFormat: TMediaStreamDataHeader;
                              aInfo: pointer; aInfoSize: cardinal);

  public
    constructor Create; overload;

    destructor Destroy; override;

    procedure Open(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;


    function Name: string; override;
    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    //property Framer: TStreamFramer read FFramer;
  end;

implementation
  uses Math,Forms,uWorkspace,uTrace,MediaStream.FramerFactory,IdGlobal;

{ TMediaServerSourceCapture }

constructor TMediaServerSourceCapture.Create();
begin
  Create(-1);
end;

destructor TMediaServerSourceCapture.Destroy;
begin
  inherited;
end;

function TMediaServerSourceCapture.DeviceType: string;
begin
  result:='Capture';
end;

function TMediaServerSourceCapture.Name: string;
begin
  result:='Capture';
end;

procedure TMediaServerSourceCapture.OnFrameReceived(aMediaType: TMediaType;
                                  aData: pointer; aDataSize:cardinal;
                                  const aFormat: TMediaStreamDataHeader;
                                  aInfo: pointer; aInfoSize: cardinal);
var
  aLSCP : TDateTime;
  aStatisticsFrameLength: int64;
begin
  Lock;
  try
    Inc(FBytesReceived, aDataSize);
    Inc(FFramesReceived);
    if aMediaType=mtAudio then
      inc(FAFramesReceived)
    else if ffKeyFrame in aFormat.biFrameFlags then
      inc(FVIFramesReceived)
    else
      inc(FVPFramesReceived);

    FAverageFrameSize:=FBytesReceived div FFramesReceived;
    FMaxFrameSize := max(FMaxFrameSize,aDataSize);
    if FMinFrameSize<0 then
      FMinFrameSize:=aDataSize
    else
      FMinFrameSize := min(FMinFrameSize,aDataSize);

    FLastStreamDateTime:=Now;
    aLSCP:=Now;
    aStatisticsFrameLength:=Trunc((aLSCP-FLastStatisticsCheckpoint)*SecsPerDay);
    if (aStatisticsFrameLength>10) then
    begin
      FBytesReceivedSpeed:=(FBytesReceived-FBytesReceivedCheckPoint) div aStatisticsFrameLength;
      FBytesReceivedCheckPoint:=FBytesReceived;
      FLastStatisticsCheckpoint:=aLSCP;
    end;

    DoDataReceived(aFormat, aData,aDataSize, aInfo,aInfoSize);
  finally
    Unlock;
  end;
end;

function CaptureCallback(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD; stdcall;
begin
  result:=0;
end;


procedure TMediaServerSourceCapture.Open(aSync: boolean);
begin
  if Opened then
    exit;

  Close;

	FCurrentBmpInfo.bmiHeader.biBitCount:=24;
	FCurrentBmpInfo.bmiHeader.biClrImportant:=0;
  FCurrentBmpInfo.bmiHeader.biClrUsed:=0;
	FCurrentBmpInfo.bmiHeader.biCompression:=BI_RGB;
	FCurrentBmpInfo.bmiHeader.biHeight:=240;
	FCurrentBmpInfo.bmiHeader.biPlanes:=1;
	FCurrentBmpInfo.bmiHeader.biSize:=sizeof(BITMAPINFOHEADER);
	FCurrentBmpInfo.bmiHeader.biSizeImage:=0;
	FCurrentBmpInfo.bmiHeader.biWidth:=320;
	FCurrentBmpInfo.bmiHeader.biXPelsPerMeter:=0;
  FCurrentBmpInfo.bmiHeader.biYPelsPerMeter:=0;

	FCaptureWnd:=capCreateCaptureWindow('Capture Window',WS_VISIBLE or WS_CHILD,0,0,320,240,Application.MainFormHandle,1);
  if FCaptureWnd=0 then
    RaiseLastOSError;

  SetWindowLong(FCaptureWnd,GWL_USERDATA,integer(self));
	capDriverConnect(FCaptureWnd,0);

	//set the video format
	capSetVideoFormat(FCaptureWnd,@FCurrentBmpInfo,sizeof(FCurrentBmpInfo));

	capPreviewRate(FCaptureWnd,40);
//	capDlgVideoFormat(this->m_hWndCapture);

	capPreview(FCaptureWnd,TRUE);
	capSetCallbackOnFrame(FCaptureWnd,@CaptureCallback);

  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSourceCapture.DoClose;
begin
  capDriverDisconnect(FCaptureWnd);
end;

function TMediaServerSourceCapture.ConnectionString: string;
begin
  result:='';
end;

function TMediaServerSourceCapture.Opened: Boolean;
begin
  result:=FCaptureWnd<>0;
end;

function TMediaServerSourceCapture.StreamInfo: TBytes;
begin
  Lock;
  try
    CheckConnected;
    result:=RawToBytes(FCurrentBmpInfo.bmiHeader,sizeof(FCurrentBmpInfo.bmiHeader));
  finally
    Unlock;
  end;
end;

function TMediaServerSourceCapture.VideoStreamType: TStreamType;
begin
  result:=stRGB;
end;

function TMediaServerSourceCapture.AudioStreamType: TStreamType;
begin
  result:=0;
end;


function TMediaServerSourceCapture.PtzSupported: boolean;
begin
  result:=false;
end;

procedure TMediaServerSourceCapture.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;


end.

