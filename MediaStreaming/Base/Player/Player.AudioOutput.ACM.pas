unit Player.AudioOutput.ACM;

interface
  uses Windows, SysUtils, Classes, Graphics, Messages, MSACM,Player.AudioOutput.Base,MediaProcessing.Definitions;


type
  TAudioOutputDecoder_ACM = class (TAudioOutputDecoder)
  private
    FAudioDecoder: THandle;

    FInBuffer: TBytes;
    FOutBuffer: TBytes;
    FLastFailedDecompressorFCC: cardinal;
    FInStreamHead: TACMSTREAMHEADER;
    FLastUnspportedStreamType: TStreamType;

    FInFormat: TMediaStreamDataHeader;
    FOutFormat: TPCMFormat;

    //FDecompressorDescription: string;
    //FDecompressorName: string;
    //FDecompressorDriver: string;
    //FDecompressorFCC: string;

    procedure InitDecompressor(const aInInfo: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal);
    procedure CloseDecompressor;
  public
    function  DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
                          out aPcmFormat: TPCMFormat;
                          out aPcmData: pointer; out aPcmDataSize: cardinal):boolean; override;

//    function  StatusInfo: string; override;

   //property  DecompressorFCC: string read FDecompressorFCC;
   // property  DecompressorName: string read FDecompressorName;
   // property  DecompressorDescription: string read FDecompressorDescription;
   // property  DecompressorDriver: string read FDecompressorDriver;

    constructor Create; override;
    destructor Destroy; override;
  end;

  TPlayerAudioOutput_ACM = class (TPlayerAudioOutputWaveOut)
  public
    constructor Create; override;
  end;

implementation
  uses MMSystem, MMReg, Math;

{ TAudioOutputDecoder_ACM }

constructor TAudioOutputDecoder_ACM.Create;
begin
  inherited Create;
end;

destructor TAudioOutputDecoder_ACM.Destroy;
begin
  inherited;
  CloseDecompressor;
end;

procedure TAudioOutputDecoder_ACM.InitDecompressor(const aInInfo: TMediaStreamDataHeader;aInfo: pointer; aInfoSize: cardinal);
var
  aOutFormatSize: cardinal;
  aDecompressorFourCCString: string;


  aOutFormat : TWAVEFORMATEX;
  aErrorCode: integer;
  aInFormatMp3: TMpegLayer3WaveFormat;
  pInFormat: PWaveFormatEx;
begin
  ZeroMemory(@aOutFormat,sizeof(aOutFormat));
  aOutFormat.wFormatTag:=WAVE_FORMAT_PCM;
  aOutFormat.cbSize:=sizeof(aOutFormat);
  aOutFormat.nChannels:=aInInfo.AudioChannels;
  aOutFormat.nSamplesPerSec:=aInInfo.AudioSamplesPerSec;
  aOutFormat.wBitsPerSample:=16;
  aOutFormat.nBlockAlign:=(aOutFormat.wBitsPerSample div 8) * aOutFormat.nChannels;
  aOutFormat.nAvgBytesPerSec:=aOutFormat.nBlockAlign * aOutFormat.nSamplesPerSec;

  aDecompressorFourCCString:=StreamTypeToFourccString(aInInfo.biStreamType);
  Assert(FAudioDecoder=0);
  if aInInfo.biStreamType=stPCM then
  begin
    FAudioDecoder:=INVALID_HANDLE_VALUE;
    FInFormat:=aInInfo;
    FOutFormat.Channels:=aOutFormat.nChannels;
    FOutFormat.BitsPerSample:=aOutFormat.wBitsPerSample;
    FOutFormat.SamplesPerSec:=aOutFormat.nSamplesPerSec;
    exit;
  end;

  pInFormat:=nil;

  if aInInfo.biStreamType=WAVE_FORMAT_MPEGLAYER3 then
  begin
    aDecompressorFourCCString:='MP3';
    if (aInfo<>nil) and (aInfoSize=sizeof(aInFormatMp3)) then
    begin
      aInFormatMp3:=PMpegLayer3WaveFormat(aInfo)^;
      Assert(aInFormatMp3.wfx.wFormatTag=WAVE_FORMAT_MPEGLAYER3);
    end
    else begin
      aInFormatMp3.wfx:=aInInfo.ToWaveFormatEx();
      //TODO!!!!
      aInFormatMp3.wfx.cbSize:=MPEGLAYER3_WFX_EXTRA_BYTES;
      aInFormatMp3.fdwFlags:=MPEGLAYER3_FLAG_PADDING_OFF;
      //aInFormatMp3.nBlockSize:=???;
      aInFormatMp3.nFramesPerBlock := 1; // MUST BE ONE
      aInFormatMp3.nCodecDelay := 0; // 0 for LAME or 1393 for fraunh..
      aInFormatMp3.wID := MPEGLAYER3_ID_MPEG;
    end;

    pInFormat:=pointer(@aInFormatMp3);
  end;

  if (pInFormat<>nil) then
  begin
    aErrorCode:=acmStreamOpen(
     @FAudioDecoder,    // Open an ACM conversion stream
     0,                 // Query all ACM drivers
     pInFormat^,        // input format :  mp3
     aOutFormat,       // output format : pcm
     nil,              // No filters
     0,                 // No async callback
     0,                 // No data for callback
     0                  // No flags
     );

   Win32Check(aErrorCode=MMSYSERR_NOERROR);
  end;

  if FAudioDecoder=0 then
  begin
    FLastFailedDecompressorFCC:=aInInfo.biStreamType;
    raise Exception.CreateFmt('Ошибка открытия декодера для аудио потока %s: не найден соответствующий декодер',[aDecompressorFourCCString]);
  end
  else begin
{      if ICGetInfo(FAudioDecoder,@aDecoderInfo,sizeof(aDecoderInfo))<>0 then
    begin
      FDecompressorFCC:=FOURCCTOSTR(aDecoderInfo.fccHandler);
      FDecompressorName:=aDecoderInfo.szName;
      FDecompressorDescription:=aDecoderInfo.szDescription;
      FDecompressorDriver:=aDecoderInfo.szDriver;
    end
}
  end;

  if pInFormat.wFormatTag=WAVE_FORMAT_MPEGLAYER3 then
  begin
    SetLength(FInBuffer,aInFormatMp3.nBlockSize);
    acmStreamSize(FAudioDecoder, aInFormatMp3.nBlockSize, aOutFormatSize, ACM_STREAMSIZEF_SOURCE);
  end
  else //TODO????
  begin
    SetLength(FInBuffer,0);
    acmStreamSize(FAudioDecoder, pInFormat.nAvgBytesPerSec, aOutFormatSize, ACM_STREAMSIZEF_SOURCE);
  end;

  if integer(aOutFormatSize)<=0 then
    RaiseLastOSError;

  SetLength(FOutBuffer,aOutFormatSize);

  // prepare the decoder
  // memset( &mp3streamHead, 0, sizeof(ACMSTREAMHEADER ) );
  FInStreamHead.cbStruct := sizeof(FInStreamHead);
  FInStreamHead.pbSrc := @FInBuffer[0];
  FInStreamHead.cbSrcLength := Length(FInBuffer);
  FInStreamHead.pbDst := @FOutBuffer[0];
  FInStreamHead.cbDstLength := Length(FOutBuffer);
  Win32Check(acmStreamPrepareHeader(FAudioDecoder, FInStreamHead, 0 )=MMSYSERR_NOERROR);

  FInFormat:=aInInfo;
  FOutFormat.Channels:=aOutFormat.nChannels;
  FOutFormat.BitsPerSample:=aOutFormat.wBitsPerSample;
  FOutFormat.SamplesPerSec:=aOutFormat.nSamplesPerSec;
end;


{
function TAudioOutputDecoder_ACM.StatusInfo: string;
begin
  result:=inherited StatusInfo;

  result:=result+'ACM:'#13#10;
  result:=result+  Format('  Input Type:%s, %dx%d'#13#10,[FOURCCTOSTR(FInFormat.bmiHeader.biCompression),FInFormat.bmiHeader.biWidth,FInFormat.bmiHeader.biHeight]);
  result:=result+  Format('  Decoder created:%s'#13#10,[BoolToStr(FAudioDecoder<>0,true)]);

  if FAudioDecoder<>0 then
  begin
    result:=result+  Format('  Decoder FCC:%s'#13#10,[DecompressorFCC]);
    result:=result+  Format('  Decoder Name:%s'#13#10,[DecompressorName]);
    result:=result+  Format('  Decoder Desc:%s'#13#10,[DecompressorDescription]);
    result:=result+  Format('  Decoder Driver:%s'#13#10,[DecompressorDriver]);
  end;
end;
}

function TAudioOutputDecoder_ACM.DecodeToPCM(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
                          out aPcmFormat: TPCMFormat;
                          out aPcmData: pointer; out aPcmDataSize: cardinal):boolean;
begin
  Assert(aData<>nil);
  Assert(aFormat.biMediaType=mtAudio);
  result:=false;


  if (FInFormat.biStreamType<>aFormat.biStreamType) or
     (FInFormat.AudioChannels<>aFormat.AudioChannels) or
     (FInFormat.AudioBitsPerSample<>aFormat.AudioBitsPerSample) or
     (FInFormat.AudioSamplesPerSec<>aFormat.AudioSamplesPerSec) then
    CloseDecompressor;

  if FAudioDecoder=0 then
  try
    if FLastUnspportedStreamType=aFormat.biStreamType then
      exit; //Не поддерживам такой формат
    InitDecompressor(aFormat,aInfo,aInfoSize);
  except
    FLastUnspportedStreamType:=aFormat.biStreamType;
    exit;
  end;

  if (aFormat.biStreamType=stPCM) then
  begin
    aPcmData:=aData;
    aPcmDataSize:=aDataSize;
    aPcmFormat.Channels:=aFormat.AudioChannels;
    aPcmFormat.BitsPerSample:=aFormat.AudioBitsPerSample;
    aPcmFormat.SamplesPerSec:=aFormat.AudioSamplesPerSec;
    result:=true;
  end
  else begin

    begin
      CopyMemory(@FInBuffer[0],aData,Min(aDataSize,Length(FInBuffer)));
      if acmStreamConvert(FAudioDecoder, FInStreamHead, ACM_STREAMCONVERTF_BLOCKALIGN )=MMSYSERR_NOERROR  then
      begin
        aPcmData:=@FOutBuffer[0];
        aPcmDataSize:=FInStreamHead.cbDstLengthUsed;
        aPcmFormat:=FOutFormat;
        result:=true;
      end;
    end;
  end;
end;

procedure TAudioOutputDecoder_ACM.CloseDecompressor;
begin
  if (FAudioDecoder<>0) and (FAudioDecoder<>INVALID_HANDLE_VALUE) then
  begin
    acmStreamUnprepareHeader(FAudioDecoder, FInStreamHead, 0 );
    acmStreamClose(FAudioDecoder,0);
  end;

  FAudioDecoder:=0;
  FInBuffer:=nil;
  FOutBuffer:=nil;
  ZeroMemory(@FInFormat,sizeof(FInFormat));

(*
  FDecompressorFCC:='';
  FDecompressorName:='';
  FDecompressorDescription:='';
  FDecompressorDriver:='';
  *)
end;

{ TPlayerAudioOutput_ACM }

constructor TPlayerAudioOutput_ACM.Create;
begin
  inherited Create;
  RegisterDecoderClass(stUNIV,TAudioOutputDecoder_ACM);
  SetStreamType(stUNIV);
end;


end.
