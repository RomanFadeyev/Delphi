unit MediaProcessing.Convertor.HHH264.H264.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics, MediaProcessing.Definitions,MediaProcessing.Global,HHCommon,MediaProcessing.Convertor.HHH264.H264;

type
  TMediaProcessor_Convertor_Hh_H264_H624_Impl=class (TMediaProcessor_Convertor_Hh_H264_H624,IMediaProcessorImpl)
  private
    FLastVideoFrameTimeStamp: TDateTime;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
  end;

implementation
  uses uBaseClasses;

procedure TMediaProcessor_Convertor_Hh_H264_H624_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
type
  TNALSignature = array [0..2] of byte;

var
  aStreamData: PHV_FRAME;
  aMinDelta : cardinal;
  b: boolean;
//  aNALSignature: ^TNALSignature;
begin
  TArgumentValidation.NotNil(aInData);
  Assert(aInfoSize=sizeof(HHAV_INFO));

  aStreamData:=aInData;
  Assert(aStreamData.zeroFlag=0);
  Assert(aStreamData.oneFlag=1);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutFormat:=aInFormat;
  aOutFormat.biStreamType:=stH264;

  if not (aStreamData.streamFlag in [FRAME_FLAG_VP,FRAME_FLAG_VI]) then
    exit;

  if aStreamData.streamFlag=FRAME_FLAG_VI then
    Assert(ffKeyFrame in aOutFormat.biFrameFlags)
  else if aStreamData.streamFlag=FRAME_FLAG_VP then
    Assert(not (ffKeyFrame in aOutFormat.biFrameFlags));

  b:=false;
  if HHCommon.GetH264Data(aStreamData,aOutData,aOutDataSize) then
  begin
    if FChangeFPSMode=cfmNone then
    begin
      b:=true;
    end
    //Только опорные кадры
    else if (FChangeFPSMode = cfmVIFrameOnly) and (aStreamData.streamFlag=FRAME_FLAG_VI) then
    begin
      b:=true;
    end

    //Прореживание кадров
    else if FChangeFPSMode=cfmAbsolute then
    begin
      if (FLastVideoFrameTimeStamp<>0) and (FLastVideoFrameTimeStamp<aStreamData.nTimestamp) then
      begin
        aMinDelta:=25 div FFPSValue;
        if aStreamData.nTimestamp-FLastVideoFrameTimeStamp>=aMinDelta then
          b:=true;
      end
      else
        FLastVideoFrameTimeStamp:=aStreamData.nTimestamp;
    end;
  end;

  if not b then
  begin
    aOutData:=nil;
    aOutDataSize:=0;
  end
  else begin

(*  if b then
  begin
    aOutData:=PAnsiChar(aStreamData)+sizeof(HV_FRAME_HEAD)+sizeof(EXT_FRAME_HEAD);
    aOutDataSize:=aStreamData.nByteNum-sizeof(EXT_FRAME_HEAD);

    Assert(PAnsiChar(aOutData)^=#0);
    Assert((PAnsiChar(aOutData)+1)^=#0);
    Assert((PAnsiChar(aOutData)+2)^=#0);
    Assert((PAnsiChar(aOutData)+3)^=#1);
*)

    FLastVideoFrameTimeStamp:=aStreamData.nTimestamp;
  end;
end;


initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Convertor_Hh_H264_H624_Impl);


end.
