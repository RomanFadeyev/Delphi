{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь медиа-потока в формате BMP. Стабилизирует     }
{                дрожание кадров                                               }
{                Реализация.                                                   }
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

unit MediaProcessing.Stabilizer.RGB.Impl;

interface
  uses SysUtils,Windows,Classes, Generics.Collections, SyncObjs,
       MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Stabilizer.RGB,
       CMSA,Collections.Aggregates;

type
  //Собственно реализация медиа-процессора
  TMediaProcessor_Stabilizer_Rgb_Impl =class (TMediaProcessor_Stabilizer_Rgb,IMediaProcessorImpl)
  private
    FResultBitmapDIB: TBytes;
    FDF : TFrameDispFinder;
    FXMovementStat: TAggregate;
    FYMovementStat: TAggregate;


  protected
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Math,BitPlane,uBaseClasses;

{ TMediaProcessor_Stabilizer_Rgb_Impl }

constructor TMediaProcessor_Stabilizer_Rgb_Impl.Create;
begin
  inherited;
  FDF:=TFrameDispFinder.Create(0,0,100,100,12.5,7.5,FDF);
end;

destructor TMediaProcessor_Stabilizer_Rgb_Impl.Destroy;
begin
  inherited;
  FResultBitmapDIB:=nil;
  FreeAndNil(FXMovementStat);
  FreeAndNil(FYMovementStat);
  FreeAndNil(FDF);
end;

procedure TMediaProcessor_Stabilizer_Rgb_Impl.LoadCustomProperties(
  const aReader: IPropertiesReader);
begin
  inherited;

  FreeAndNil(FXMovementStat);
  FreeAndNil(FYMovementStat);

  if FDetectMovements then
  begin
    FXMovementStat:=TAggregate.Create(FDetectMovementsPeriod);
    FYMovementStat:=TAggregate.Create(FDetectMovementsPeriod);
  end;
end;

procedure TMediaProcessor_Stabilizer_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
//  aStartTicks: cardinal;
  aRes: CMSA_RESULT;
  aBih: TBitmapInfoHeader;
  aX,aY: single;
  aMX,aMY: double;

  aPlaneS,aPlaneD: TBitPlaneDesc;
begin
//  aStartTicks:=GetTickCount;
  TArgumentValidation.NotNil(aInData);

  Assert(aInFormat.biMediaType=mtVideo);
  Assert(aInFormat.biStreamType=stRGB);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutFormat:=aInFormat;

  if (aInFormat.VideoBitCount<>24) then
  begin
    SetLastError(Format('Формат RGB должен имет глубину цвета 24 бита. Фактичесий формат - %d бит',[aInFormat.VideoBitCount]));
    aOutFormat.Clear;
    exit;
  end;

  aBih:=aInFormat.ToBitmapInfoHeader(aInDataSize);
  aRes:=FDF.Process(aBih, aInData, aX,aY);
  if aRes<>CMSARES_SUCCESS then
  begin
     SetLastError(CmsaGetErrorMessage(aRes));
     exit;
  end;

  if (FXMovementStat<>nil) and (FXMovementStat.Period>0) then
  begin
    FXMovementStat.Add(aX);
    if FXMovementStat.Count=FXMovementStat.Period then
    begin
      aMX:=FXMovementStat.GetAverage;
      aX:=aX-aMX;
    end;
  end;

  if (FYMovementStat<>nil) and (FYMovementStat.Period>0) then
  begin
    FYMovementStat.Add(aY);
    if FYMovementStat.Count=FYMovementStat.Period then
    begin
      aMY:=FYMovementStat.GetAverage;
      aY:=aY-aMY;
    end;
  end;

  if cardinal(Length(FResultBitmapDIB))<aInDataSize then
  begin
    FResultBitmapDIB:=nil;
    SetLength(FResultBitmapDIB,aInDataSize);
  end;

  aOutData:=FResultBitmapDIB;
  aOutDataSize:=aInDataSize;
  aOutFormat:=aInFormat;

  //Вставляем картинку
  aPlaneS.Init(aInData,aInDataSize,aInFormat.VideoWidth,aInFormat.VideoHeight,aInFormat.VideoBitCount);
  aPlaneD.Init(aOutData,aOutDataSize,aOutFormat.VideoWidth,aOutFormat.VideoHeight,aOutFormat.VideoBitCount);

  aPlaneS.CopyToBitPlane(aPlaneD,Round(aX),Round(aY),not FBuildImageOutOfBorders);
  //CopyRGB(aPlaneS,aPlaneD,Round(0),Round(0));
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Stabilizer_Rgb_Impl);

end.
