{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь медиа-потока в формате Beward (MP6 или HH)    }
{                Реализация                                                    }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        21.01.2011                                                    }
{                                                                              }
{  <Примечание>  Отсутствует                                                   }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaProcessing.Transformer.HH.Impl;

interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Transformer.HH,HHCommon;

type
  TMediaProcessor_Transformer_Hh_Impl =class (TMediaProcessor_Transformer_Hh,IMediaProcessorImpl)
  private
    FLastIVideoFrameTimeStamp: cardinal;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
  end;

implementation
  uses uBaseClasses;

procedure TMediaProcessor_Transformer_Hh_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aStreamData: PHV_FRAME;
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

  if aStreamData.streamFlag=FRAME_FLAG_A then
    exit;

  Assert(aStreamData.streamFlag in [FRAME_FLAG_VP,FRAME_FLAG_VI]);

  if FChangeFPSMode=cfmNone then
  begin
    aOutData:=aInData;
    aOutDataSize:=aInDataSize;
  end
  //Только опорные кадры
  else if (FChangeFPSMode = cfmVIFrameOnly) and (aStreamData.streamFlag=FRAME_FLAG_VI) then
  begin
    aOutData:=aInData;
    aOutDataSize:=aInDataSize;
  end

  //Прореживание кадров
  else if (FChangeFPSMode=cfmAbsolute) and (aStreamData.streamFlag=FRAME_FLAG_VI) then
  begin
    if (FLastIVideoFrameTimeStamp=0) or (int64(GetTickCount)-FLastIVideoFrameTimeStamp>=int64(FFrameIntervalValue*1000)) then
    begin
      aOutData:=aInData;
      aOutDataSize:=aInDataSize;
    end;
  end;

  if aOutData<>nil then
  begin
    aOutInfo:=aInfo;
    aOutInfoSize:=aInfoSize;
    if aStreamData.streamFlag=FRAME_FLAG_VI then
      FLastIVideoFrameTimeStamp:=GetTickCount;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Transformer_Hh_Impl);

end.
