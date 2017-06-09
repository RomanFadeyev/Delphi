unit MediaStream.Writer.AVI;

interface
  uses Windows,Classes,SysUtils,VFW,MMSystem,MediaProcessing.Definitions;

type
  //���������� ���������� � ������� AVI �������� ��������� ������ �� ������
  TMediaStreamWriter_AVI = class
  private
    FFileName: string;
    FHeaderWritten: boolean;

    FVideoEnabled: boolean;
    FVideoFCC: cardinal;
    FVideoFormat: TBitmapInfoHeader ;
    FVideoFrameRate: double;

    FAudioEnabled: boolean;
    FAudioFCC: cardinal;
    FAudioFormat: TPCMWaveFormat;

    FVideoFramesWrittenCount: cardinal;
    FAudioFramesWrittenCount :cardinal;
    FWrittenDataSize: int64;

    FVideoStream,FAudioStream: IAVIStream;
    FAviFile: IAVIFile;

    procedure WriteHeader;
    procedure CheckOpened;
  public
    constructor Create; overload;
    destructor Destroy; override;

    //������� ���� ��� ������
    procedure EnableVideo(aFCC: cardinal; const aVideoFormat: TBitmapInfoHeader; const aFrameRate: double);
    procedure EnableAudio(aFCC: cardinal; const aAudioFormat: TPCMWaveFormat);

    procedure Open(const aFileName: string);


    procedure OpenFileOnly(const aFileName: string);
    procedure BeginWriting;

    //������� ����
    procedure Close;

    function  Opened: boolean;
    property  FileName: string read FFileName;
    property  WrittenDataSize: int64 read FWrittenDataSize;

    procedure WriteData(const aFormat: TMediaStreamDataHeader;aData: pointer; aDataSize: cardinal);

    property  AviStream: IAVIStream read FVideoStream;

    property  AudioEnabled: boolean read FAudioEnabled;
    property  AudioType: TStreamType read FAudioFCC;

    property  VideoEnabled: boolean read FVideoEnabled;
    property  VideoType: TStreamType read FVideoFCC;
    property  VideoFramesWrittenCount: cardinal read FVideoFramesWrittenCount;
    property  AudioFramesWrittenCount :cardinal read FAudioFramesWrittenCount;
  end;

implementation
  uses BitPlane;
{ TMediaStreamWriter_AVI }

constructor TMediaStreamWriter_AVI.Create;
begin
  inherited Create;
end;

destructor TMediaStreamWriter_AVI.Destroy;
begin
  Close;
  inherited;
end;

procedure TMediaStreamWriter_AVI.EnableAudio(aFCC: cardinal; const aAudioFormat: TPCMWaveFormat);
begin
  FAudioEnabled:=true;
  FAudioFCC:=aFCC;
  if aFCC=stPCM then
    FAudioFCC:=0;

  FAudioFormat:=aAudioFormat;
end;

procedure TMediaStreamWriter_AVI.EnableVideo(aFCC: cardinal; const aVideoFormat: TBitmapInfoHeader; const aFrameRate: double);
begin
  FVideoEnabled:=true;
  FVideoFCC:=aFCC;
  FVideoFrameRate:=aFrameRate;
  if FVideoFCC=stRGB then
    FVideoFCC:=0; //!!!

  FVideoFormat:=aVideoFormat;
end;

procedure TMediaStreamWriter_AVI.BeginWriting;
begin
  if FHeaderWritten then
    exit;

  WriteHeader;
  FHeaderWritten:=true;
end;

procedure TMediaStreamWriter_AVI.CheckOpened;
begin
  if not Opened then
    raise Exception.Create('���� ��� �� ������');
end;

procedure TMediaStreamWriter_AVI.Close;
begin
  inherited;

  //AVIStreamRelease(FVideoStream);
  FVideoStream:=nil;
  FAudioStream:=nil;
  FAviFile:=nil;
  FHeaderWritten:=false;

  //AVIStreamRelease(FCompressedStream);

  FVideoFramesWrittenCount:=0;
  FAudioFramesWrittenCount:=0;
end;

procedure TMediaStreamWriter_AVI.WriteHeader;
var
  aStreamInfo: TAVIStreamInfoW;
begin
  if FVideoEnabled or FAudioEnabled = false then
    raise Exception. Create('�� ������������� �� ���� �� ������� (Audio, Video)');

  //VIDEO
  if FVideoEnabled then
  begin
    ZeroMemory(@aStreamInfo, SizeOf(aStreamInfo));
    aStreamInfo.fccType := streamtypeVIDEO;
    aStreamInfo.fccHandler := FVideoFCC;
    aStreamInfo.dwScale := 10; //�������� �� 10, ����� ������� ���-�� ������ � ��������� �� �������
    aStreamInfo.dwRate := Round(FVideoFrameRate*10);
    aStreamInfo.dwSuggestedBufferSize := FVideoFormat.biWidth*FVideoFormat.biHeight*4; //aBih.biSizeImage; // size of 1 frame
    SetRect(aStreamInfo.rcFrame, 0, 0, FVideoFormat.biWidth, FVideoFormat.biHeight);

    if FAviFile.CreateStream(FVideoStream, aStreamInfo) <> 0 then
      RaiseLastOSError;

    if FVideoStream.SetFormat(0, @FVideoFormat, sizeof(FVideoFormat)) <> 0 then
      RaiseLastOSError;
  end;

  if FAudioEnabled then
  begin
    // AUDIO
    ZeroMemory(@aStreamInfo, SizeOf(aStreamInfo));
    aStreamInfo.fccType := streamtypeAUDIO;
    aStreamInfo.fccHandler := FAudioFCC;
    aStreamInfo.dwScale := FAudioFormat.wf.nBlockAlign; //���-�� ���� �� ���� ����� �� ���� �������.;
    aStreamInfo.dwRate :=  FAudioFormat.wf.nSamplesPerSec * aStreamInfo.dwScale; // ���� � ���.
    //aStreamInfo.dwInitialFrames := 1; //����� ������� �����.
    aStreamInfo.dwSampleSize := aStreamInfo.dwScale; //���-�� ���� �� ����� �� ���� �������.
    aStreamInfo.dwSuggestedBufferSize := aStreamInfo.dwScale*FAudioFormat.wf.nSamplesPerSec;
    //aStreamInfo.dwQuality := cardinal(-1); //��������, -1 - �� ���������.

    if FAviFile.CreateStream(FAudioStream, aStreamInfo) <> 0 then
      RaiseLastOSError;

    if FAudioStream.SetFormat(0,@FAudioFormat,sizeof(FAudioFormat))<>0 then
      RaiseLastOSError;
  end;
end;

procedure TMediaStreamWriter_AVI.WriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal);
var
  x,i,j:integer;
  aFlags : cardinal;
  aBitPlane: TBitPlaneDesc;
begin
  if aDataSize=0 then
    exit;

  CheckOpened;

  aFlags:=0;
  if ffKeyFrame in aFormat.biFrameFlags then
    aFlags:=aFlags or AVIIF_KEYFRAME;

  if aFormat.biMediaType=mtVideo then
  begin
    if FVideoStream=nil then
      exit;

    if aFormat.biStreamType=stRGB then
      if not aFormat.VideoReversedVertical then
      begin
        aBitPlane.Init(aData,aDataSize,aFormat.VideoWidth,aFormat.VideoHeight,aFormat.VideoBitCount);
        aBitPlane.Upturn;
      end;

    if FVideoStream.Write(FVideoFramesWrittenCount, 1, aData, aDataSize, aFlags, i, j) <> 0 then
      RaiseLastOSError;

    Assert(i=1);
    Assert(j=integer(aDataSize));
    inc(FVideoFramesWrittenCount);
  end
  else if aFormat.biMediaType=mtAudio then
  begin
    if FAudioStream=nil then
      exit;

    x:=aDataSize div FAudioFormat.wf.nChannels div (FAudioFormat.wBitsPerSample div 8);
    if FAudioStream.Write(FAudioFramesWrittenCount, x, aData, aDataSize, aFlags, i, j) <> 0 then
      RaiseLastOSError;

    Assert(i=x);
    Assert(j=integer(aDataSize));
    inc(FAudioFramesWrittenCount,x);
  end;

  inc(FWrittenDataSize,aDataSize);
end;

procedure TMediaStreamWriter_AVI.Open(const aFileName: string);
begin
  OpenFileOnly(aFileName);
  BeginWriting;
end;

procedure TMediaStreamWriter_AVI.OpenFileOnly(const aFileName: string);
var
  aDirectory: string;
begin
  Close;

  FFileName:=aFileName;
  AviFileInit;

  aDirectory:=ExtractFileDir(aFileName);
  if not DirectoryExists(aDirectory) then
    raise Exception.CreateFmt('�� ������� ����� ���� "%s"',[aDirectory]);

  DeleteFile(PChar(FFileName));
  if AviFileOpen(FAviFile, PChar(FFileName), OF_WRITE or OF_CREATE, nil) <> 0 then
    RaiseLastOSError;
end;

function TMediaStreamWriter_AVI.Opened: boolean;
begin
  result:=FAviFile<>nil;
end;


end.

