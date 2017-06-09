unit Player.AudioOutput.HH;

interface
  uses Windows,SysUtils,Classes,Graphics,SyncObjs, Player.AudioOutput.Base,HHDecoder,HHCommon,MediaProcessing.Definitions;

type

  TAudioOutputDecoder_HH = class (TAudioOutputDecoder)
  private
    FAudioDecoder : THHAudioDecoder;
  public
    function  DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
    out aPcmFormat: TPCMFormat;
    out aPcmData: pointer; out aPcmDataSize: cardinal):boolean; override;

    procedure ResetBuffer; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

  TPlayerAudioOutput_HH = class (TPlayerAudioOutputWaveOut)
  public
    constructor Create; override;
  end;

implementation

{ THHAudioOutputDirectX }

constructor TAudioOutputDecoder_HH.Create;
begin
  inherited Create;
  FAudioDecoder:=THHAudioDecoder.Create;
end;

destructor TAudioOutputDecoder_HH.Destroy;
begin
  inherited;
  FreeAndNil(FAudioDecoder);
end;

procedure TAudioOutputDecoder_HH.ResetBuffer;
begin
  inherited;
  //FAudioDecoder.ResetBuffer;
end;

function TAudioOutputDecoder_HH.DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
  out aPcmFormat: TPCMFormat;
  out aPcmData: pointer; out aPcmDataSize: cardinal):boolean;
var
  pFrameData: PHV_FRAME;

  aAvInfo: HHAV_INFO;
  aExtFrame: PEXT_FRAME_HEAD;
begin
  result:=false;

  if aInfo=nil then
  begin
    aAvInfo.nAudioChannels:=aFormat.AudioChannels;
    aAvInfo.nAudioBits:=aFormat.AudioBitsPerSample;
    aAvInfo.nAudioSamples:=aFormat.AudioSamplesPerSec;
    aAvInfo.nAudioEncodeType:=aFormat.biStreamSubType;
  end
  else begin
    Assert(aInfoSize=sizeof(HHAV_INFO));
    aAVInfo:= PHHAV_INFO(aInfo)^;
  end;

  Assert(aFormat.biMediaType=mtAudio);

  Assert(PHV_FRAME_HEAD(aData).nByteNum+sizeof(HV_FRAME_HEAD)=aDataSize);
  pFrameData:=aData;

  //Мусор?
  if pFrameData.zeroFlag<>0 then
    exit;
  if pFrameData.oneFlag<>1 then
    exit;

  if GetExtFrameHead(pFrameData,aExtFrame) then
  begin
    aAVInfo.nAudioEncodeType:=aExtFrame.szFrameInfo.szFrameAudio.nAudioEncodeType;
    aAVInfo.nAudioChannels:=aExtFrame.szFrameInfo.szFrameAudio.nAudioChannels;
    aAVInfo.nAudioBits:=aExtFrame.szFrameInfo.szFrameAudio.nAudioBits;
    aAVInfo.nAudioBitrate:=aExtFrame.szFrameInfo.szFrameAudio.nAudioBitrate;
    aAVInfo.nAudioSamples:=aExtFrame.szFrameInfo.szFrameAudio.nAudioSamples;
  end;

  Assert(pFrameData.streamFlag=FRAME_FLAG_A);
  try
    FAudioDecoder.DecodeToPCM(pFrameData,aAvInfo);
    result:=true;
  except
  end;

  if result then
  begin
    aPcmFormat.Channels:=aFormat.AudioChannels;
    aPcmFormat.BitsPerSample:=aFormat.AudioBitsPerSample;
    aPcmFormat.SamplesPerSec:=aFormat.AudioSamplesPerSec;
    aPcmData:=FAudioDecoder.CurrentBuffer;
    aPcmDataSize:=FAudioDecoder.CurrentBufferSize;
  end;
end;


{ TPlayerAudioOutput_HH }

constructor TPlayerAudioOutput_HH.Create;
begin
  inherited Create;
  RegisterDecoderClass(stHHAU,TAudioOutputDecoder_HH);
  SetStreamType(stHHAU);
end;

end.
