unit Player.AudioOutput.AllTypes;

interface
  uses Windows,SysUtils,Classes,Graphics,SyncObjs, Player.AudioOutput.Base,MediaProcessing.Definitions;

type
  TPlayerAudioOutput_AllTypes = class (TPlayerAudioOutputWaveOut)
  protected
    function  GetStreamTypeHandlerClass(aType: TStreamType): TAudioOutputDecoderClass; override;
  public
    constructor Create; override;
  end;

implementation
  uses Player.AudioOutput.PCM, 
       Player.AudioOutput.PCMU, 
    {$IFNDEF DISABLE_HH}
       Player.AudioOutput.HH,
    {$ENDIF}
       Player.AudioOutput.ACM;

{ TPlayerAudioOutput_AllTypes }

constructor TPlayerAudioOutput_AllTypes.Create;
begin
  inherited Create;
  RegisterDecoderClass(stPCM,TAudioOutputDecoder_PCM);
  RegisterDecoderClass(stPCMU,TAudioOutputDecoder_PCMU);

{$IFNDEF DISABLE_HH}
  RegisterDecoderClass(stHHAU,TAudioOutputDecoder_HH);
{$ENDIF}
//  SetStreamType(stHH);
end;

function TPlayerAudioOutput_AllTypes.GetStreamTypeHandlerClass(
  aType: TStreamType): TAudioOutputDecoderClass;
begin
  result:=inherited GetStreamTypeHandlerClass(aType);

  if result=nil then
    result:=TAudioOutputDecoder_ACM;
end;

end.

