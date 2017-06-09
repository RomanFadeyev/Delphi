unit MediaProcessing.Convertor.H264.RGB.Impl;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Convertor.H264.RGB,MediaProcessing.Definitions,MediaProcessing.Global,AVC,Avc.avcodec, H264Def,
  MediaProcessing.Common.Processor.FPS;

type
  TMediaProcessor_ConvertorH264_Rgb_Impl =class (TMediaProcessor_Convertor_H264_Rgb,IMediaProcessorImpl)
  private
    FDecoder: IVideoDecoder;
    FBmpInfoHeader: TBitmapInfoHeader;
    FPrevFrameImageWidth,FPrevFrameImageHeight: integer;
    FConsequtiveErrors: integer;

    FDibBuffer : TBytes;
    //FLastVideoFrameTimeStamp: cardinal;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;

{ TMediaProcessor_ConvertorH264_Rgb_Impl }

constructor TMediaProcessor_ConvertorH264_Rgb_Impl.Create;
begin
  inherited;
  FBmpInfoHeader.biSize:=sizeof(FBmpInfoHeader);
  FBmpInfoHeader.biPlanes:=1;
  FBmpInfoHeader.biBitCount:=24;
  SetTraceIdentifier('h264-rgb');
end;

destructor TMediaProcessor_ConvertorH264_Rgb_Impl.Destroy;
begin
  FDecoder:=nil;
  inherited;
end;

procedure TMediaProcessor_ConvertorH264_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  //aMinDelta : cardinal;

  aNalUnitType: TNalUnitType;
  aTmp: integer;

  aNeedSize,aDecodedSize: integer;

  aParams: IVideoDecoderParams;
  aDecodeResult: FFMPEG_RESULT;
begin
  CheckIfChannel0(aInFormat);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;

  Assert(aInFormat.biStreamType=stH264);

  //
  //|0|1|2|3|4|5|6|7|
  //+-+-+-+-+-+-+-+-+
  //|F|NRI| Type |
  //+---------------+

  Assert(PDWORD(aInData)^=$1000000); //NAL_START_MARKER
  aTmp:=(PByte(aInData)+4)^;
  Assert(aTmp shr 7=0);

  aNalUnitType:=ExtractNalType(aTmp);
  Assert(integer(aNalUnitType) in [1..23]);

  //if not (aPayload in [1,5]) then
  //  exit;

  //Только опорные кадры
  if (FChangeFPSMode = cfmVIFrameOnly) then
    if (aNalUnitType in [NAL_UT_SLICE_NON_IDR,NAL_UT_SLICE_DPA,NAL_UT_SLICE_DPB,NAL_UT_SLICE_DPC]) then
      exit;

  if (FPrevFrameImageWidth<>aInFormat.VideoWidth) or (FPrevFrameImageHeight<>aInFormat.VideoHeight) then
  begin
    if FDecoder<>nil then
      TraceLine('Изменился формат кадра, удаляем старый декодер');
    FDecoder:=nil;
  end;

  if (FConsequtiveErrors>100) then
  begin
    if FDecoder<>nil then
      TraceLine('Декодер не в состоянии выполнить декодирование 100 фреймов подряд, удаляем декодер');
    FConsequtiveErrors:=0;
    FDecoder:=nil;
  end;


  FPrevFrameImageWidth:=aInFormat.VideoWidth;
  FPrevFrameImageHeight:=aInFormat.VideoHeight;

  FBmpInfoHeader.biWidth:=aInFormat.VideoWidth;
  FBmpInfoHeader.biHeight:=aInFormat.VideoHeight;

  if FDecoder=nil then
  begin
    TraceLine('Создаем декодер...');
    AvcCheck(CreateVideoDecoder(AV_CODEC_ID_H264, FDecoder));
    AvcCheck(FDecoder.GetParams(aParams));
    aParams.SetWidth(FBmpInfoHeader.biWidth);
    aParams.SetHeight(FBmpInfoHeader.biHeight);
    FDecoder.Open(false);
    TraceLine('Декодер успешно создан');
  end;

  //FVideoDecoder.Lock;
  try
    aNeedSize:=aInFormat.VideoWidth*aInFormat.VideoHeight*3;

    if Length(FDibBuffer)<aNeedSize then
      SetLength(FDibBuffer,aNeedSize);

    aDecodeResult:=FDecoder.DecodeRGB24(aInData,aInDataSize, @FDibBuffer[0],aNeedSize,aDecodedSize);

    if (aDecodeResult=FR_SUCCESSFUL) and (aDecodedSize>0) then
    begin
      FConsequtiveErrors:=0;

      if not Process_Fps(aInFormat) then
        exit;

      Process_ImageSizeRGB(@FDibBuffer[0],FBmpInfoHeader.biWidth,FBmpInfoHeader.biHeight);

      FBmpInfoHeader.biSizeImage:=FBmpInfoHeader.biWidth*FBmpInfoHeader.biHeight*3;

      aOutData:=@FDibBuffer[0];
      aOutDataSize:=FBmpInfoHeader.biSizeImage;

      aOutInfo:=@FBmpInfoHeader;
      aOutInfoSize:=sizeof(FBmpInfoHeader);

      aOutFormat.Assign(FBmpInfoHeader);
      aOutFormat.Channel:=aInFormat.Channel;
      aOutFormat.TimeStamp:=aInFormat.TimeStamp;
      aOutFormat.TimeKoeff:=aInFormat.TimeKoeff;
      aOutFormat.VideoReversedVertical:=true; //Декодер дает перевернутую картинку
      Include(aOutFormat.biFrameFlags,ffKeyFrame);
    end
    else begin
      inc(FConsequtiveErrors);
      SetLastError(AVC.GetErrorMessage(aDecodeResult));
      if aDecodeResult<>FR_NO_PICTURE_DECOMPRESSED then
        TraceLine('Декодер вернул ошибку: '+AVC.GetErrorMessage(aDecodeResult));
    end;
  finally
    //FVideoDecoder.Unlock;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_ConvertorH264_Rgb_Impl);


end.
