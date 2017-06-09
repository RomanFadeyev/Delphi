unit MediaProcessing.Transformer.RGB.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics, BitmapStreamMediator,
       MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Transformer.RGB;

type
  TMediaProcessor_Transformer_Rgb_Impl =class (TMediaProcessor_Transformer_Rgb,IMediaProcessorImpl)
  private
    FDibBuffer: TBytes;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses,BitPlane,MediaProcessing.Common.Processor.FPS.ImageSize;

constructor TMediaProcessor_Transformer_Rgb_Impl.Create;
begin
  inherited;
end;

destructor TMediaProcessor_Transformer_Rgb_Impl.Destroy;
begin
  inherited;
end;

procedure TMediaProcessor_Transformer_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aBitPlane: TBitPlaneDesc;
  aBitPlane2: TBitPlaneDesc;
  aTmpBuffer: TBytes;

procedure PrepareDib;
begin
  if aOutData<>FDibBuffer then
  begin
    if cardinal(Length(FDibBuffer))<aInDataSize then
    begin
      FDibBuffer:=nil;
      SetLength(FDibBuffer,aInDataSize);
    end;

    CopyMemory(@FDibBuffer[0],aInData,aInDataSize);
    aOutData:=@FDibBuffer[0];
  end;
end;

begin
  TArgumentValidation.NotNil(aInData);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutFormat:=aInFormat;

  if not Process_Fps(aInFormat) then
    exit;

  aOutData:=aInData;
  aOutDataSize:=aInDataSize;
  aOutInfo:=aInfo;
  aOutInfoSize:=aInfoSize;

  if FImageSizeMode<>cismNone then
  begin
    PrepareDib;
    Process_ImageSizeRGB(@FDibBuffer[0],aOutFormat.VideoWidth,aOutFormat.VideoHeight);
    aOutDataSize:=GetRGBSize(aOutFormat.VideoWidth,aOutFormat.VideoHeight,aInFormat.VideoBitCount);
  end;

  if FVerticalReversePhysical and (aOutData<>nil) then
  begin
    PrepareDib;
    aBitPlane.Init(pointer(FDibBuffer),aOutDataSize,aOutFormat.VideoWidth,aOutFormat.VideoHeight,aOutFormat.VideoBitCount);
    aBitPlane.Upturn;
  end;

  if FVerticalReverseLogical and (aOutData<>nil) then
  begin
    aOutFormat.VideoReversedVertical:=not aOutFormat.VideoReversedVertical;
  end;

  if FRotate then
  begin
    PrepareDib;
    SetLength(aTmpBuffer,Length(FDibBuffer));
    CopyMemory(aTmpBuffer,FDibBuffer, Length(FDibBuffer));

    aBitPlane.Init(pointer(aTmpBuffer),aOutDataSize,aOutFormat.VideoWidth,aOutFormat.VideoHeight,aOutFormat.VideoBitCount);
    aBitPlane2.Init(pointer(FDibBuffer),aOutDataSize,aOutFormat.VideoWidth,aOutFormat.VideoHeight,aOutFormat.VideoBitCount);

    aBitPlane.RotateToBitPlane(aBitPlane2,-FRotateAngle,false)
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Transformer_Rgb_Impl);

end.
