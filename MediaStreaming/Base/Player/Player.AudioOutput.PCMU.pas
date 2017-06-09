unit Player.AudioOutput.PCMU;

interface
  uses Windows,SysUtils,Classes,Types,Player.AudioOutput.Base,MediaProcessing.Definitions, ULaw;

type

  TAudioOutputDecoder_PCMU= class (TAudioOutputDecoder)
  private
    FDecodeBuffer:  TWordDynArray;
  public
    function  IsDataValid(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal):boolean; override;

    function  DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
    out aPcmFormat: TPCMFormat;
    out aPcmData: pointer; out aPcmDataSize: cardinal):boolean; override;
  end;

  TPlayerAudioOutput_PCMU= class (TPlayerAudioOutputWaveOut)
  public
    constructor Create; override;
  end;

implementation

{ TAudioOutputDecoder_PCMU }

function TAudioOutputDecoder_PCMU.DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
        out aPcmFormat: TPCMFormat;
        out aPcmData: pointer; out aPcmDataSize: cardinal):boolean;
var
  i: integer;
begin
  Assert(aFormat.AudioBitsPerSample=8);
  if cardinal(Length(FDecodeBuffer))<aDataSize then
    SetLength(FDecodeBuffer,aDataSize);

  for i:=0 to aDataSize-1 do
    FDecodeBuffer[i]:=ULaw.audio_u2s(PByte(aData)[i]);

  aPcmData:=@FDecodeBuffer[0];
  aPcmDataSize:=aDataSize*2;
  aPcmFormat.Channels:=aFormat.AudioChannels;
  aPcmFormat.BitsPerSample:=16;
  aPcmFormat.SamplesPerSec:=aFormat.AudioSamplesPerSec;
  result:=true;
end;


{ TPlayerAudioOutput_PCM }

constructor TPlayerAudioOutput_PCMU.Create;
begin
  inherited Create;
  RegisterDecoderClass(stPCMU,TAudioOutputDecoder_PCMU);
  SetStreamType(stPCMU);
end;

function TAudioOutputDecoder_PCMU.IsDataValid(
  const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal;
  aInfo: pointer; aInfoSize: cardinal): boolean;
begin
  result:=inherited IsDataValid(aFormat,aData,aDataSize,aInfo,aInfoSize);

  if result then
    result:=aFormat.AudioBitsPerSample=8;
end;

end.
