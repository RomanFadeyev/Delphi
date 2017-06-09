unit MediaProcessing.Processor.VA.Any.Impl;

interface
  uses SysUtils,Windows,Classes, SyncObjs, Generics.Collections, MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Processor.VA.Any,MediaProcessing.VideoAnalytics,MediaProcessing.VideoAnalytics.Definitions;

type

  TMediaProcessor_VA_Any_Impl= class (TMediaProcessor_VA_Any,IMediaProcessorImpl,IMediaProcessorImpl2)
  private
    FVideoAnalytics:  TVideoAnalytics;
    FVideoAnalyticsFailure: string;
    FCurrentResult: TList<TVaProcessingResult>;
    FCurrentResultLock: TCriticalSection;

    FCache: TObjectList<TBytesStream>;

    procedure OnVideoAnalyticsFrameProcessed(Sender: TVideoAnalytics);
    function  IsAllowedBySchedule: boolean;
  protected
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;

    procedure ProcessData2(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal;
                      aAuxResults: TMediaProcessorResultList);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Prepare; override;
  end;

implementation
   uses uBaseClasses,uTrace;
{ TMediaProcessor_VA_Any_Impl }

constructor TMediaProcessor_VA_Any_Impl.Create;
begin
  inherited;
  try
    FVideoAnalytics := TVideoAnalytics.Create;
    FVideoAnalytics.OnFrameProcessed:=OnVideoAnalyticsFrameProcessed;
  except
    on E:Exception do
    begin
      TraceLine(Format('Œ¯Ë·Í‡: ',[E.Message]));
      FVideoAnalyticsFailure:=E.Message;
      FreeAndNil(FVideoAnalytics);
    end;
  end;

  FCache:=TObjectList<TBytesStream>.Create;
  FCurrentResultLock:=TCriticalSection.Create;
  FCurrentResult:=TList<TVaProcessingResult>.Create;

  //FVideoAnalytics.OnTrace := OnVideoAnalyticsTrace;
  //FVideoAnalytics.OnFrameProcessed := OnVideoAnalyticsFrameProcessed;
end;

destructor TMediaProcessor_VA_Any_Impl.Destroy;
begin
  inherited;
  FreeAndNil(FVideoAnalytics);
  FreeAndNil(FCurrentResult);
  FreeAndNil(FCurrentResultLock);
  FreeAndNil(FCache);
end;

function TMediaProcessor_VA_Any_Impl.IsAllowedBySchedule: boolean;
var
  i: Integer;
  aNowTime: TTime;
begin
  if not FConfig_ScheduleEnabled then
    exit(true);

  result:=false;
  aNowTime:=Frac(Now);
  for i := 0 to High(FConfig_Schedule) do
  begin
    if (aNowTime>=FConfig_Schedule[i].From) and (aNowTime<=FConfig_Schedule[i].To_) then
    begin
      result:=true;
      break;
    end;
  end;

end;

procedure TMediaProcessor_VA_Any_Impl.LoadCustomProperties(const aReader: IPropertiesReader);
var
  aParameters: TVaParameters;
begin
  inherited;
  if FConfig_VaConfigFileName <> '' then
    FVideoAnalytics.LoadConfig(FConfig_VaConfigFileName);
  if FConfig_VaModelFileName <> '' then
    FVideoAnalytics.LoadModel(FConfig_VaModelFileName);

  aParameters:=FVideoAnalytics.Parameters;
  aParameters.OtEnabled:=FConfig_VaOtEnabled;
  aParameters.AsyncProcessing:=FConfig_VaAsyncProcessing;
  aParameters.OtFilters:=FConfig_VaFilters;
  aParameters.OtEngine:=FConfig_VaOtEngine;
  aParameters.LuEnabled:=FConfig_VaLuEnabled;
  aParameters.LuMinY:=FConfig_VaLuMinY;
  aParameters.LuMaxY:=FConfig_VaLuMaxY;

  FVideoAnalytics.Parameters:=aParameters;
end;

procedure TMediaProcessor_VA_Any_Impl.OnVideoAnalyticsFrameProcessed(
  Sender: TVideoAnalytics);
begin
  FCurrentResultLock.Enter;
  try
    FCurrentResult.Add(Sender.GetCurrentResultCopy);
  finally
    FCurrentResultLock.Leave;
  end;
end;

procedure TMediaProcessor_VA_Any_Impl.Prepare;
begin
  inherited;
  FVideoAnalytics.Init;
end;

procedure TMediaProcessor_VA_Any_Impl.Process(aInData: pointer;
  aInDataSize: cardinal; const aInFormat: TMediaStreamDataHeader;
  aInfo: pointer; aInfoSize: cardinal; out aOutData: pointer;
  out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader;
  out aOutInfo: pointer; out aOutInfoSize: cardinal);
begin
  aOutData:=aInData;
  aOutDataSize:=aInDataSize;
  aOutFormat:=aInFormat;
  aOutInfo:=aInfo;
  aOutInfoSize:=aInfoSize;
  aOutFormat:=aInFormat;

  if IsAllowedBySchedule then
  begin
    if FVideoAnalytics=nil then
    begin
      SetLastError(FVideoAnalyticsFailure);
    end
    else begin
      FVideoAnalytics.ProcessFrame(aInFormat,aInData,aInDataSize,aInfo,aInfoSize);
    end;
  end;
end;

procedure TMediaProcessor_VA_Any_Impl.ProcessData2(aInData: pointer;
  aInDataSize: cardinal; const aInFormat: TMediaStreamDataHeader;
  aInfo: pointer; aInfoSize: cardinal; out aOutData: pointer;
  out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader;
  out aOutInfo: pointer; out aOutInfoSize: cardinal;
  aAuxResults: TMediaProcessorResultList);
var
  i,j,k: Integer;
  aStream: TBytesStream;
  aObject: ^TVaObject;
  aEvent: ^TVaEvent;
  aCurrResult: TVaProcessingResult;
  aMPResult: TMediaProcessorResult;
begin
  Process(aInData,aInDataSize,aInFormat,aInfo,aInfoSize,aOutData,aOutDataSize,aOutFormat,aOutInfo,aOutInfoSize);


  if IsAllowedBySchedule and (FVideoAnalytics<>nil) then
  begin
    FCurrentResultLock.Enter;
    try
      if FCurrentResult.Count=0 then
        exit;

      for k := 0 to FCurrentResult.Count-1 do
      begin
        aCurrResult:=FCurrentResult[k];

        if k>=FCache.Count then
          FCache.Add(TBytesStream.Create());

        aStream:=FCache[k];
        aStream.Position:=0;


        aStream.WriteInteger(Length(aCurrResult.Objects));
        for i := 0 to High(aCurrResult.Objects) do
        begin
          aObject:=@aCurrResult.Objects[i];
          aStream.WriteInteger(aObject.id);
          aStream.WriteInteger(aObject.type_);
          aStream.WriteBuffer(aObject.position,sizeof(aObject.position));
          aStream.WriteBuffer(aObject.rect,sizeof(aObject.rect));
          aStream.WriteBuffer(aObject.start_position,sizeof(aObject.start_position));

          aStream.WriteInteger(Length(aObject.trajectory));
          for j := 0 to High(aObject.trajectory) do
            aStream.WriteBuffer(aObject.trajectory[j],sizeof(aObject.trajectory[j]));

          aStream.WriteInteger(aObject.mask_index);
          aStream.WriteBuffer(aObject.mask_rect,sizeof(aObject.mask_rect));

          aStream.WriteInteger(Length(aObject.all_events_deprecated));
          for j := 0 to High(aObject.all_events_deprecated) do
            aStream.WriteInteger(aObject.all_events_deprecated[j]);
        end;

        aStream.WriteInteger(Length(aCurrResult.Events));
        for i := 0 to High(aCurrResult.Events) do
        begin
          aEvent:=@aCurrResult.Events[i];
          aStream.WriteInteger(integer(aEvent.type_));
          aStream.WriteInteger(aEvent.level);
          aStream.WriteInteger(aEvent.object_id);
          aStream.WriteInteger(aEvent.rule_id);
          aStream.WriteString(aEvent.description);
        end;

        aMPResult.Data:=aStream.Bytes;
        aMPResult.DataSize:=aStream.Position;
        aMPResult.Info:=nil;
        aMPResult.InfoSize:=0;
        aMPResult.Format.Clear;
        aMPResult.Format.biMediaType:=mtSysData;
        aMPResult.Format.biStreamType:=stVideoAnalytics;
        aMPResult.Format.TimeStamp:=GetTickCount;
        aMPResult.Format.VideoWidth:=FVideoAnalytics.CurrentWidth;
        aMPResult.Format.VideoHeight:=FVideoAnalytics.CurrentHeight;

        aAuxResults.Add(aMPResult);
      end;

      FCurrentResult.Clear;
    finally
      FCurrentResultLock.Leave;
    end;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_VA_Any_Impl);

end.
