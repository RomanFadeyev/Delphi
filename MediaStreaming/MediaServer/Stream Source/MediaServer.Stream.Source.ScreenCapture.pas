{***********************************<_INFO>************************************}
{  <������>      �����-������                                                  }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      �����-��������, ��������������� �����-������ ����� �������    }
{                ������                                                        }
{                                                                              }
{  <�����>       ������ �.�.                                                   }
{                                                                              }
{  <����>        14.01.2011                                                    }
{                                                                              }
{  <����������>  ��� ����������.                                               }
{                                                                              }
{  <��������>    ��� ��� "���������-�����", ��� "�������"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaServer.Stream.Source.ScreenCapture;

interface
  uses Windows, SysUtils, Classes, SyncObjs,uBaseClasses,
  MediaServer.Stream.Source,
  MediaProcessing.Definitions;

type
  //�����, ����������� ��������������� ��������� ������ (�����������) �� ������
  TMediaServerSourceScreenCapture = class (TMediaServerSource)
  private
    FEmitThread : TThreadObjectVar<TThread>;
    FFps: integer;

    procedure OnFrameReceived(const aFormat: TMediaStreamDataHeader;
                              aData: pointer; aDataSize:cardinal;
                              aInfo: pointer; aInfoSize: cardinal);

  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(aFps: integer); overload;
    destructor Destroy; override;

    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;

    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    //property Framer: TStreamFramer read FFramer;
  end;

implementation
  uses Math,Forms,Graphics, MediaServer.Workspace,uTrace,MediaStream.FramerFactory,ThreadNames,
  IdGlobal;

type
  TScreenCaptureThread = class (TThread)
  private
    FCurrentBmpInfoHeader: TBitmapInfoHeader;
    FOwner: TMediaServerSourceScreenCapture;
    FOpenLock: TCriticalSection;
    FMemDC : HDC;
    FBitmap : HBITMAP;
    FDib: pointer;

    procedure PrepareBitmap;
    function StreamInfo: TBytes;
    //function StreamType: TStreamType;
  protected
    procedure Capture;
    procedure Execute; override;
  public
    constructor Create(aOwner: TMediaServerSourceScreenCapture);
    destructor Destroy; override;
  end;

function GetCaptureThread(aThread : TThreadObjectVar<TThread>): TScreenCaptureThread;
begin
  result:=aThread.Value as TScreenCaptureThread;
  Assert(result<>nil);
end;

{ TScreenCaptureThread }


procedure TScreenCaptureThread.Capture;
var
  aScreenDC: HDC;
  hbmpOldTarget: HGDIOBJ;
  aFormat: TMediaStreamDataHeader;
begin
  Assert(FMemDC<>0);
  PrepareBitmap;

  aScreenDC := GetDC (0);
  try
    // 3. �������� ������ � ��������
    hbmpOldTarget := SelectObject(FMemDC, FBitmap);
    try
      // 4. ������ ����
      Win32Check(BitBlt(FMemDC,
        0, 0,
        FCurrentBmpInfoHeader.biWidth,
        FCurrentBmpInfoHeader.biHeight,
        aScreenDC,
        0, 0, // ���������� ���������� ��������� ������
        SRCCOPY
        ));
    finally
      // 5. ��������� ������
      SelectObject(FMemDC, hbmpOldTarget);
    end;

      // 7. ������������� ��������
    //UpturnImage(FDib,FCurrentBmpInfoHeader);
    aFormat.Assign(FCurrentBmpInfoHeader);
    aFormat.VideoReversedVertical:=false;
    Include(aFormat.biFrameFlags,ffKeyFrame);
    FOwner.OnFrameReceived(aFormat, FDib,FCurrentBmpInfoHeader.biSizeImage,@FCurrentBmpInfoHeader,sizeof(FCurrentBmpInfoHeader));
  finally
    ReleaseDC (0, aScreenDC );
  end;
end;

constructor TScreenCaptureThread.Create(aOwner: TMediaServerSourceScreenCapture);
begin
  FOwner:=aOwner;
  FOpenLock:=TCriticalSection.Create;

	FCurrentBmpInfoHeader.biBitCount:=24;
	FCurrentBmpInfoHeader.biClrImportant:=0;
  FCurrentBmpInfoHeader.biClrUsed:=0;
	FCurrentBmpInfoHeader.biCompression:=BI_RGB;
	FCurrentBmpInfoHeader.biWidth:=0;
	FCurrentBmpInfoHeader.biHeight:=0; //���������� �����
	FCurrentBmpInfoHeader.biPlanes:=1;
	FCurrentBmpInfoHeader.biSize:=sizeof(BITMAPINFOHEADER);
	FCurrentBmpInfoHeader.biXPelsPerMeter:=0;
  FCurrentBmpInfoHeader.biYPelsPerMeter:=0;

  inherited Create(false);
end;

destructor TScreenCaptureThread.Destroy;
begin
  inherited;
  FreeAndNil(FOpenLock);
end;

function TScreenCaptureThread.StreamInfo: TBytes;
begin
  FOpenLock.Enter;
  try
    result:=RawToBytes(FCurrentBmpInfoHeader,sizeof(FCurrentBmpInfoHeader));
  finally
    FOpenLock.Leave;
  end;
end;

procedure TScreenCaptureThread.Execute;
var
  aStart,aStop,aDelta:cardinal;
  aInterval: cardinal;
begin
  SetCurrentThreadName('Source: '+ClassName);
  if FOwner.FFps=0 then
    aInterval:=40
  else
    aInterval:=1000 div FOwner.FFps;

  PrepareBitmap;
  FMemDC:=CreateCompatibleDC(0);
  try
    while not Terminated do
    begin
      try
        aStart:=GetTickCount;

        Capture;

        aStop:=GetTickCount;
        aDelta:=aStop-aStart;
        if aDelta<aInterval then
          Sleep(aInterval-aDelta);
      except
        on E:Exception do
        begin
          sleep(100);
          //TODO ����� ��������?
        end;
      end;
    end;
  finally
    DeleteDC(FMemDC);
    DeleteObject(FBitmap);
  end;
end;

procedure TScreenCaptureThread.PrepareBitmap;
var
  w,h: integer;
  aBitmapInfo: TBitmapInfo;
begin
  w:=Screen.Width;
  h:=Screen.Height;

  // 1. ������� ��������
  if (FCurrentBmpInfoHeader.biWidth<>W) or (FCurrentBmpInfoHeader.biHeight<>H) then
  begin
    FOpenLock.Enter;
    try
      if FBitmap<>0 then
        DeleteObject(FBitmap);
      FBitmap:=0;

      FCurrentBmpInfoHeader.biWidth:=W;
      FCurrentBmpInfoHeader.biHeight:=H;
      FCurrentBmpInfoHeader.biSizeImage:=W*H*3;

      aBitmapInfo.bmiHeader:=FCurrentBmpInfoHeader;
      FBitmap :=CreateDIBSection(FMemDC, aBitmapInfo, DIB_RGB_COLORS, FDib, 0, 0); // true-color
      Win32Check(FBitmap<>0);
    finally
      FOpenLock.Leave;
    end;
  end;
end;

{ TMediaServerSourceScreenCapture }

constructor TMediaServerSourceScreenCapture.Create(aFps: integer);
begin
  inherited Create(-1);
  FEmitThread:=TThreadObjectVar<TThread>.Create;
  FFps:=aFps;
end;

destructor TMediaServerSourceScreenCapture.Destroy;
begin
  inherited;
  FreeAndNil(FEmitThread);
end;

function TMediaServerSourceScreenCapture.DeviceType: string;
begin
  result:='������� ����';
end;

function TMediaServerSourceScreenCapture.Name: string;
begin
  result:='Screen Capture';
end;

procedure TMediaServerSourceScreenCapture.OnFrameReceived(
                                  const aFormat: TMediaStreamDataHeader;
                                  aData: pointer; aDataSize:cardinal;
                                  aInfo: pointer; aInfoSize: cardinal);
begin
//  aB.Init(aData,aDataSize,aFormat.VideoWidth,aFormat.VideoHeight,aFormat.VideoBitCount);
//  aBitmap:=TBitmap.Create;
//  aB.CopyToBitmap(aBitmap,false);
//  aBitmap.SaveToFile('C:\1.bmp');


  DoDataReceived(aFormat, aData,aDataSize, aInfo,aInfoSize);
end;

procedure TMediaServerSourceScreenCapture.DoOpen(aSync: boolean);
begin
  if Opened then
    exit;

  Close;

  FEmitThread.Value:=TScreenCaptureThread.Create(self);

  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSourceScreenCapture.DoClose;
begin
  FEmitThread.FreeValue;
end;

function TMediaServerSourceScreenCapture.ConnectionString: string;
begin
  result:='';
end;

function TMediaServerSourceScreenCapture.Opened: Boolean;
begin
  result:=FEmitThread.Value<>nil;
end;

function TMediaServerSourceScreenCapture.StreamInfo: TBytes;
begin
  FEmitThread.Lock;
  try
    CheckConnected;
    result:=GetCaptureThread(FEmitThread).StreamInfo;
  finally
    FEmitThread.Unlock;
  end;
end;

function TMediaServerSourceScreenCapture.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  if aMediaType=mtVideo then
    result:=stRGB
  else
    result:=0;
end;

function TMediaServerSourceScreenCapture.PtzSupported: boolean;
begin
  result:=false;
end;

procedure TMediaServerSourceScreenCapture.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
end;

end.

