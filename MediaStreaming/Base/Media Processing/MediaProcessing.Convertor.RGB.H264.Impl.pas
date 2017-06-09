unit MediaProcessing.Convertor.RGB.H264.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics,
       MediaProcessing.Definitions,MediaProcessing.Global,
       MediaProcessing.Convertor.RGB.H264,avc, H264Def;

type
  TMediaProcessor_Convertor_Rgb_H264_Impl= class (TMediaProcessor_Convertor_Rgb_H264,IMediaProcessorImpl, IMediaProcessorImpl2)
  private
    FYUVBuffer: TBytes;
    FH264Buffer : TBytes;

    FWidth,FHeight: integer;
    FEncoder    : IH264Encoder;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;

    procedure ProcessData2(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal;
                      aAuxResults: TMediaProcessorResultList);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;



implementation
  uses uBaseClasses,BitPlane,Yuv, MediaProcessing.Convertor.RGB.H264.SettingsDialog;


{ TMediaProcessor_Convertor_Rgb_H264_Impl }

constructor TMediaProcessor_Convertor_Rgb_H264_Impl.Create;
begin
  inherited;
  avc.Version;
end;

destructor TMediaProcessor_Convertor_Rgb_H264_Impl.Destroy;
begin
  FEncoder:=nil;
  inherited;
end;

procedure TMediaProcessor_Convertor_Rgb_H264_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aYuvSize,aEncodedSize: integer;

  aPayload: byte;

  aParams: IH264EncoderParams;
  aEncodeResult: integer;
  aNal: TNalUnitType;
begin
  TArgumentValidation.NotNil(aInData);
  Assert(aInFormat.biStreamType=stRGB);
  Assert(aInFormat.VideoBitCount=24);
  Assert(aInFormat.VideoWidth*aInFormat.VideoHeight*3=integer(aInDataSize));

  //Assert(aInfoSize=sizeof(TBitmapInfoHeader));
  //Assert(PBitmapInfoHeader(aInfo).biSizeImage=aInDataSize);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;


  if (FWidth<>aInFormat.VideoWidth) or (FHeight<>aInFormat.VideoHeight) then
    FEncoder:=nil;

  FWidth:=aInFormat.VideoWidth;
  FHeight:=aInFormat.VideoHeight;

  if FEncoder=nil then
  begin
    Set8087CW($133f);
    AvcCheck(AVC.CreateH264Encoder(FEncoder));
    try
      Assert(FEncoder<>nil);
      AvcCheck(FEncoder.GetParams(aParams));
      AvcCheck(aParams.SetWidth(FWidth));
      AvcCheck(aParams.SetHeight(FHeight));

      IH264EncoderParamsHelper.ApplyOption(aParams, 'preset',H264EncoderPresetNames[FPreset]);
      IH264EncoderParamsHelper.ApplyOption(aParams, 'crf',IntToStr(FCRF));
      if FMaxBitrateKbit<>0 then
        IH264EncoderParamsHelper.ApplyOption(aParams, 'maxrate',Format('%dk',[FMaxBitrateKbit]));

      IH264EncoderParamsHelper.ApplyOption(aParams, 'keyint_min', IntToStr(FKeyFrameInterval));

      AvcCheck(FEncoder.Open);
    except
      on E:Exception do
      begin
        FEncoder:=nil;
        raise;
      end;
    end;
  end;

  //aYuvSize:=AVC.GetPictureSize(YUV420, FWidth, FHeight);
  aYuvSize:=GetYuv420ImageSize(FWidth, FHeight);

  if aYuvSize<=0 then
  begin
    SetLastError('Не удалось определить размер YUV-кадра');
    exit;
  end;


  if Length(FYUVBuffer)<aYuvSize then
    SetLength(FYUVBuffer,aYuvSize);

  if Length(FH264Buffer)<aYuvSize then //TODO как правильно посчитать?
    SetLength(FH264Buffer,aYuvSize);

  Assert(integer(aInDataSize)=FWidth*FHeight*3);

  aEncodeResult:=FEncoder.EncodeRGB24(aInData,aInDataSize,@FH264Buffer[0],aYuvSize,aEncodedSize);

  if aEncodeResult<>FR_SUCCESSFUL then
  begin
    SetLastError(AVC.GetErrorMessage(aEncodeResult));
    exit;
  end;

  if aEncodedSize<=0 then
  begin
    SetLastError('Нет готового фрейма');
    exit;
  end;

  Assert(PInteger(@FH264Buffer[0])^=$1000000); //NAL_START_MARKER

  Assert(FH264Buffer[4] shr 7=0);
  aPayload:=FH264Buffer[4] and $1F;
  Assert(aPayload in [1..23]);

  aOutData:=@FH264Buffer[0];
  aOutDataSize:=aEncodedSize;
  aOutFormat:=aInFormat;
  aOutFormat.biStreamType:=stH264;
  aNal:=GetNalType(aOutData);

  aOutFormat.biFrameFlags:=[];
  if aNal in [NAL_UT_SLICE_IDR,NAL_UT_SEQ_PARAM, NAL_UT_PIC_PARAM] then
    Include(aOutFormat.biFrameFlags,ffKeyFrame);

  if aNal in [NAL_UT_SEQ_PARAM,NAL_UT_PIC_PARAM] then
    Include(aOutFormat.biFrameFlags,ffInitParamsFrame);
end;

type
  TNalSignature = array [0..2] of byte;
  PNalSignature = ^TNalSignature;

function FindNextNal(aData: pointer; aDataSize: integer): pointer;
var
  i: integer;
  aTmp: integer;
begin
  result:=nil;

  for i := 0 to aDataSize do
  begin
    if IsNalMarker(aData) then
    begin
      aTmp:=(PByte(aData)+4)^;
      if (aTmp shr 7=0) then
      begin
        exit(aData);
      end;
    end;
    aData:=PByte(aData)+1;
  end;
end;

procedure TMediaProcessor_Convertor_Rgb_H264_Impl.ProcessData2(aInData: pointer;
  aInDataSize: cardinal; const aInFormat: TMediaStreamDataHeader;
  aInfo: pointer; aInfoSize: cardinal; out aOutData: pointer;
  out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader;
  out aOutInfo: pointer; out aOutInfoSize: cardinal;
  aAuxResults: TMediaProcessorResultList);
var
  aNal: TNalUnitType;
  aData: pointer;
  aRes: TMediaProcessorResult;
  aSizeLeft: integer;
begin
  ProcessData(aInData,aInDataSize,aInFormat,aInfo,aInfoSize,aOutData,aOutDataSize,aOutFormat,aOutInfo,aOutInfoSize);

  if aOutData<>nil then
  begin
    aSizeLeft:=aOutDataSize;
    //Split to frames
    aData:=FindNextNal(PByte(aOutData)+4,aOutDataSize-4);
    if aData<>nil then
    begin
      //
      aOutDataSize:=PByte(aData)-PByte(aOutData);
      aOutFormat.biFrameFlags:=[];
      aNal:=GetNalType(aOutData);
      if aNal in [NAL_UT_SLICE_IDR,NAL_UT_SEQ_PARAM, NAL_UT_PIC_PARAM] then
        Include(aOutFormat.biFrameFlags,ffKeyFrame);

      if aNal in [NAL_UT_SEQ_PARAM,NAL_UT_PIC_PARAM] then
        Include(aOutFormat.biFrameFlags,ffInitParamsFrame);

      Dec(aSizeLeft,PByte(aData)-PByte(aOutData));
      aRes.Info:=nil;
      aRes.InfoSize:=0;
      while aData<>nil do
      begin
        aRes.Format:=aOutFormat;
        aRes.Data:=aData;
        aRes.Format.biFrameFlags:=[];

        aNal:=GetNalType(aData);
        if aNal in [NAL_UT_SLICE_IDR,NAL_UT_SEQ_PARAM, NAL_UT_PIC_PARAM] then
          Include(aRes.Format.biFrameFlags,ffKeyFrame);

        if aNal in [NAL_UT_SEQ_PARAM,NAL_UT_PIC_PARAM] then
          Include(aRes.Format.biFrameFlags,ffInitParamsFrame);

        aData:=FindNextNal(PByte(aData)+4,aSizeLeft-4);
        if aData=nil then
          aRes.DataSize:=aSizeLeft
        else begin
          dec(aSizeLeft,PByte(aData)-PByte(aRes.Data));
          aRes.DataSize:=PByte(aData)-PByte(aRes.Data);
        end;
        aAuxResults.Add(aRes);
      end;
    end;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Convertor_Rgb_H264_Impl);

end.

