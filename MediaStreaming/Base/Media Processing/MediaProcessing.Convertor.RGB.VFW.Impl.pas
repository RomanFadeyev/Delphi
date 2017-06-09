unit MediaProcessing.Convertor.RGB.VFW.Impl;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global, MediaProcessing.Convertor.RGB.VFW, VFW,JPeg;


type
  TMediaProcessor_Convertor_Rgb_Vfw_Impl =class (TMediaProcessor_Convertor_Rgb_Vfw,IMediaProcessorImpl)
  private
    FInInfo : TBitmapInfo;
    FOutInfo: PBitmapInfo;

    FCVInited: boolean;
    FCV: TCOMPVARS;

    procedure InitCompressor(const aSourceInfo: TMediaStreamDataHeader;aInDataSize:cardinal);
    procedure DeinitCompressor;
  protected
    procedure Prepare; override;

    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;


constructor TMediaProcessor_Convertor_Rgb_Vfw_Impl.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Convertor_Rgb_Vfw_Impl.Destroy;
var
  aHandle: THandle;
begin
  DeinitCompressor;

  if FCV.hic<>0 then
  try
    aHandle:=FCV.hic;
    ICCompressorFree(@FCV);
    ICClose(aHandle);
  except
  end;
  inherited;
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw_Impl.InitCompressor(const aSourceInfo: TMediaStreamDataHeader;aInDataSize:cardinal);
var
  aOutFormatSize : cardinal;
begin
  Assert(FCV.hic<>0);

  //Инициализируем ввод
  ZeroMemory(@FInInfo,sizeof(FInInfo));
  FInInfo.bmiHeader:=aSourceInfo.ToBitmapInfoHeader(aInDataSize);

  //Получаем размер выходного буфера
  aOutFormatSize:=ICCompressGetFormatSize(FCV.hic,@FInInfo);
  if integer(aOutFormatSize)<=0 then
    RaiseLastOSError;

  //... и сам буфер для формата
  FOutInfo:=AllocMem(aOutFormatSize);

  ICCheck(ICCompressGetFormat(FCV.hic,@FInInfo.bmiHeader,@FOutInfo.bmiHeader));

  //FOutBufferSize:=ICCompressGetSize(aHic,@FInInfo.bmiHeader,@FOutInfo.bmiHeader);

  //Запускаем процесс
  FCV.dwFlags:=ICMF_COMPVARS_VALID;
  FCV.cbSize:=sizeof(FCV);
  FCV.cbState:=0;
  FCV.fccHandler:=FCodecFCC;
  FCV.fccType:=ICTYPE_VIDEO;

  FCV.lDataRate:=0;//780;
  FCV.lFrame:=0;
  FCV.lKey:=ICGetDefaultKeyFrameRate(FCV.hic);
  FCV.lKeyCount:=0;
  FCV.lpbiIn:=nil;
  FCV.lpBitsOut:=nil;
  FCV.lpBitsPrev:=nil;
  FCV.lpState:=nil;
  FCV.lQ:=ICGetDefaultQuality(FCV.hic);


  if not ICSeqCompressFrameStart(@FCV,@FInInfo) then
    RaiseLastOSError;

  Set8087CW($133f);
  FCVInited:=true;
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw_Impl.DeinitCompressor;
begin
  if FCVInited then
  begin
    ICSeqCompressFrameEnd(@FCV);
  end;
  FreeMem(FOutInfo);
  FOutInfo:=nil;
  FCVInited:=false;
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  bKeyFrame: boolean;
begin
  TArgumentValidation.NotNil(aInData);
  Assert(aInFormat.biStreamType=stRGB);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;

  if (FInInfo.bmiHeader.biWidth<>aInFormat.VideoWidth) or
     (FInInfo.bmiHeader.biHeight<>aInFormat.VideoHeight) or
     (FInInfo.bmiHeader.biBitCount<>aInFormat.VideoBitCount) then
    DeinitCompressor;

  if not FCVInited then
    InitCompressor(aInFormat,aInDataSize);

	//m_OutActSize:=FInInfo.bmiHeader.biSizeImage;
	aOutData:=ICSeqCompressFrame(@FCV,0,aInData,@bKeyFrame,@aOutDataSize);
  aOutFormat.Assign(FOutInfo.bmiHeader);
  aOutFormat.Channel:=aInFormat.Channel;
  aOutFormat.TimeStamp:=aInFormat.TimeStamp;
  aOutFormat.TimeKoeff:=aInFormat.TimeKoeff;

  if bKeyFrame then
    Include(aOutFormat.biFrameFlags,ffKeyFrame)
  else
    Exclude(aOutFormat.biFrameFlags,ffKeyFrame);
end;

procedure TMediaProcessor_Convertor_Rgb_Vfw_Impl.Prepare;
var
  aHic: THandle;
  x: cardinal;
begin
  inherited;

  if FCodecFCC=0 then
    raise Exception.Create('Не указан кодек');

  //Открываем компрессор
  if FCodecMode=cmRealTime then
    x:=ICMODE_FASTCOMPRESS
  else
    x:=ICMODE_COMPRESS;

	aHic:=ICOpen(ICTYPE_VIDEO,FCodecFCC,x);
  if aHic=0 then
    RaiseLastOSError;

  try
    if Length(FCodecState)>0 then
      ICSetState(aHic,FCodecState,Length(FCodecState));
  except
		ICClose(FCV.hic);
    raise;
  end;

  ZeroMemory(@FCV,sizeof(FCV));
  FCV.hic:=aHic;
end;


initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Convertor_Rgb_Vfw_Impl);

end.

