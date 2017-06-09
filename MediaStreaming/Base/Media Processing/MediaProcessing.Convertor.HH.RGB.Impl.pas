unit MediaProcessing.Convertor.HH.RGB.Impl;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Convertor.HH.RGB,MediaProcessing.Definitions,MediaProcessing.Global,HHCommon, HHDecoder;

type
  TMediaProcessor_ConvertorHh_Rgb_Impl =class (TMediaProcessor_Convertor_Hh_Rgb,IMediaProcessorImpl)
  private
    FVideoDecoder: THHVideoDecoder;
    FLastVideoFrameTimeStamp: cardinal;

  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;

{ TMediaProcessor_ConvertorHh_Rgb_Impl }

constructor TMediaProcessor_ConvertorHh_Rgb_Impl.Create;
begin
  inherited;
  FVideoDecoder:=THHVideoDecoder.Create;
end;

destructor TMediaProcessor_ConvertorHh_Rgb_Impl.Destroy;
begin
  FreeAndNil(FVideoDecoder);
  inherited;
end;

procedure TMediaProcessor_ConvertorHh_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aStreamData: PHV_FRAME;
  aMetaInfo: PHHAV_INFO;
  aMinDelta : cardinal;

  aKX,aKY: integer;
begin
  CheckIfChannel0(aInFormat);
  TArgumentValidation.NotNil(aInData);

  Assert(aInfoSize=sizeof(HHAV_INFO));

  aStreamData:=aInData;
  Assert(aStreamData.zeroFlag=0);
  Assert(aStreamData.oneFlag=1);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;

  if aStreamData.streamFlag=FRAME_FLAG_A then
    exit;

  Assert(aStreamData.streamFlag in [FRAME_FLAG_VP,FRAME_FLAG_VI]);

  aMetaInfo:=PHHAV_INFO(aInfo);

  //Только опорные кадры
  if (FChangeFPSMode = cfmVIFrameOnly) then
    if aStreamData.streamFlag<>FRAME_FLAG_VI then
      exit;

  FVideoDecoder.Lock;
  try
    if FVideoDecoder.DecodeToBitmap(aStreamData,aMetaInfo^,false) then
    begin
      //Прореживание кадров
      if FChangeFPSMode=cfmAbsolute then
      begin
        if (FLastVideoFrameTimeStamp<>0) and (FLastVideoFrameTimeStamp<aStreamData.nTimestamp) then
        begin
          aMinDelta:=25 div FFPSValue;
          if aStreamData.nTimestamp-FLastVideoFrameTimeStamp<aMinDelta then
            exit;
        end;
      end;

      if FImageSizeMode=cismScale then
      begin
        if FImageSizeScale<>1 then
          FVideoDecoder.ScaleCurrentBmp(FImageSizeScale,FImageSizeScale);
      end
      else if FImageSizeMode=cismCustomSize then
      begin
        aKX:=1;
        if FImageSizeWidth>0 then
        begin
          aKX:=Round(FVideoDecoder.CurrentBmpInfoHeader.biWidth/FImageSizeWidth);
          if aKX<1 then
            aKX:=1;
        end;

        aKY:=1;
        if FImageSizeWidth>0 then
        begin
          aKY:=Round(FVideoDecoder.CurrentBmpInfoHeader.biHeight/FImageSizeHeight);
          if aKY<1 then
            aKY:=1;
        end;

        FVideoDecoder.ScaleCurrentBmp(aKX,aKY);
      end;

      aOutData:=FVideoDecoder.CurrentBmpDIB;
      aOutDataSize:=FVideoDecoder.CurrentBmpInfoHeader.biSizeImage;

      aOutInfo:=@FVideoDecoder.CurrentBmpInfoHeader;
      aOutInfoSize:=sizeof(FVideoDecoder.CurrentBmpInfoHeader);

      aOutFormat.Assign(FVideoDecoder.CurrentBmpInfoHeader);
      aOutFormat.Channel:=aInFormat.Channel;
      aOutFormat.TimeStamp:=aInFormat.TimeStamp;
      aOutFormat.TimeKoeff:=aInFormat.TimeKoeff;
      aOutFormat.VideoReversedVertical:=true; //Декодер дает перевернутую картинку
      Include(aOutFormat.biFrameFlags,ffKeyFrame);

      FLastVideoFrameTimeStamp:=aStreamData.nTimestamp;
    end;
  finally
    FVideoDecoder.Unlock;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_ConvertorHh_Rgb_Impl);


end.
