unit MediaProcessing.Convertor.HHAU.PCM.Impl;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Convertor.HHAU.PCM,MediaProcessing.Definitions,MediaProcessing.Global,HHCommon, HHDecoder;

type
  TMediaProcessor_ConvertorHhAu_Pcm_Impl =class (TMediaProcessor_Convertor_HhAu_Pcm,IMediaProcessorImpl)
  private
    FAudioDecoder: THHAudioDecoder;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;

{ TMediaProcessor_ConvertorHhAu_Pcm_Impl }

constructor TMediaProcessor_ConvertorHhAu_Pcm_Impl.Create;
begin
  inherited;
  FAudioDecoder:=THHAudioDecoder.Create;
end;

destructor TMediaProcessor_ConvertorHhAu_Pcm_Impl.Destroy;
begin
  FreeAndNil(FAudioDecoder);
  inherited;
end;

procedure TMediaProcessor_ConvertorHhAu_Pcm_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aStreamData: PHV_FRAME;
  aMetaInfo: PHHAV_INFO;
  b: boolean;
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

  Assert(aStreamData.streamFlag = FRAME_FLAG_A);

  aMetaInfo:=PHHAV_INFO(aInfo);

  FAudioDecoder.Lock;
  try
    b:=false;
    try
      FAudioDecoder.DecodeToPCM(aStreamData,aMetaInfo^);
      b:=true;
    except
    end;

    if b then
    begin
      aOutData:=FAudioDecoder.CurrentBuffer;
      aOutDataSize:=FAudioDecoder.CurrentBufferSize;

      aOutInfo:=nil;
      aOutInfoSize:=0;

      aOutFormat.Clear;
      aOutFormat.biMediaType:=mtAudio;
      aOutFormat.biStreamType:=stPCM;
      aOutFormat.biFrameFlags:=[ffKeyFrame];
      aOutFormat.AudioChannels:=aMetaInfo.nAudioChannels;
      aOutFormat.AudioBitsPerSample:=aMetaInfo.nAudioBits;
      aOutFormat.AudioSamplesPerSec:=aMetaInfo.nAudioSamples;
      Include(aOutFormat.biFrameFlags,ffKeyFrame);
    end;
  finally
    FAudioDecoder.Unlock;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_ConvertorHhAu_Pcm_Impl);


end.
