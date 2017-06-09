unit Player.AudioOutput.PCM;

interface
  uses Windows,SysUtils,Classes,Player.AudioOutput.Base,MediaProcessing.Definitions;

type

  TAudioOutputDecoder_PCM = class (TAudioOutputDecoder)
  public
    function  DecodeToPCM(
      const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
      out aPcmFormat: TPCMFormat;
      out aPcmData: pointer; out aPcmDataSize: cardinal):boolean; override;
  end;

  TPlayerAudioOutput_PCM = class (TPlayerAudioOutputWaveOut)
  public
    constructor Create; override;
  end;

implementation

{ THHAudioOutputDirectX }

function TAudioOutputDecoder_PCM.DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;out aPcmFormat: TPCMFormat; out aPcmData: pointer; out aPcmDataSize: cardinal):boolean;
begin
  aPcmData:=aData;
  aPcmDataSize:=aDataSize;
  aPcmFormat.Channels:=aFormat.AudioChannels;
  aPcmFormat.BitsPerSample:=aFormat.AudioBitsPerSample;
  aPcmFormat.SamplesPerSec:=aFormat.AudioSamplesPerSec;
  result:=true;
end;


{ TPlayerAudioOutput_PCM }

constructor TPlayerAudioOutput_PCM.Create;
begin
  inherited Create;
  RegisterDecoderClass(stPCM,TAudioOutputDecoder_PCM);
  SetStreamType(stPCM);
end;

end.
