unit MediaProcessing.Processor.VA.Any;


interface
  uses SysUtils,Windows,Classes, MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.VideoAnalytics.Definitions;

type
  TScheduleItem = packed record
    From: TTime;
    To_: TTime;
  end;

  TMediaProcessor_VA_Any=class (TMediaProcessor,IMediaProcessor_Convertor_Rgb_H264)
  protected
    FConfig_VaConfigFileName: string;
    FConfig_VaModelFileName: string;
    FConfig_VaAsyncProcessing: boolean;
    FConfig_VaFilters : TVaFilterArray;
    FConfig_ScheduleEnabled: boolean;
    FConfig_Schedule: TArray<TScheduleItem>;

    FConfig_VaLuEnabled: boolean;
    FConfig_VaLuMinY: integer;
    FConfig_VaLuMaxY: integer;
    FConfig_VaOtEnabled: boolean;
    FConfig_VaOtEngine: TObjectTrackingEngine;
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    class function MetaInfo:TMediaProcessorInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function  HasCustomProperties: boolean; override;
    procedure ShowCustomProperiesDialog;override;
  end;

implementation
  uses IdGlobal, Controls,uBaseClasses,MediaProcessing.Convertor.RGB.H264.SettingsDialog,MediaProcessing.Processor.VA.Any.SettingsDialog;

{ TMediaProcessor_VA_Any }

constructor TMediaProcessor_VA_Any.Create;
begin
  inherited;
end;

destructor TMediaProcessor_VA_Any.Destroy;
begin
  inherited;
end;

function TMediaProcessor_VA_Any.HasCustomProperties: boolean;
begin
  result:=true;
end;

class function TMediaProcessor_VA_Any.MetaInfo: TMediaProcessorInfo;
begin
  result.Clear;
  result.TypeID:=IMediaProcessor_Processor_Va_Any;
  result.Name:='Видеоаналитика';
  result.Description:='Обрабатывает кадры (в любом формате) и идентифицирует объекты';
  result.SetInputStreamType(stCOPY);
  result.OutputStreamType:=stCOPY;
  result.ConsumingLevel:=9;
end;

procedure TMediaProcessor_VA_Any.LoadCustomProperties(const aReader: IPropertiesReader);
var
  aBytes: TBytes;
begin
  inherited;
  FConfig_VaConfigFileName:=aReader.ReadString('ConfigFileName','');
  FConfig_VaModelFileName:=aReader.ReadString('ModelFileName','');
  FConfig_VaAsyncProcessing:=aReader.ReadBool('AsyncProcessing',false);
  FConfig_VaOtEnabled:=aReader.ReadBool('Ot.Enabled',true);

  FConfig_VaLuEnabled:=aReader.ReadBool('Lu.Enabled',false);
  FConfig_VaLuMinY:=aReader.ReadInteger('Lu.MinY',50);
  FConfig_VaLuMaxY:=aReader.ReadInteger('Lu.MaxY',200);
  FConfig_VaOtEngine:=TObjectTrackingEngine(aReader.ReadInteger('Ot.Engine',1));

  aBytes:=aReader.ReadBytes('Filters');
  SetLength(FConfig_VaFilters,Length(aBytes) div sizeof(TVaFilter));
  CopyMemory(FConfig_VaFilters,aBytes,Length(FConfig_VaFilters)*sizeof(TVaFilter));

  FConfig_ScheduleEnabled:=aReader.ReadBool('Schedule.Enabled',false);
  aBytes:=aReader.ReadBytes('Schedule.Items');
  SetLength(FConfig_Schedule,Length(aBytes) div sizeof(TScheduleItem));
  CopyMemory(FConfig_Schedule,aBytes,Length(FConfig_Schedule)*sizeof(TScheduleItem));
end;

procedure TMediaProcessor_VA_Any.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteString('ConfigFileName',FConfig_VaConfigFileName);
  aWriter.WriteString('ModelFileName',FConfig_VaModelFileName);
  aWriter.WriteBool('AsyncProcessing',FConfig_VaAsyncProcessing);
  aWriter.WriteBool('Ot.Enabled',FConfig_VaOtEnabled);

  aWriter.WriteBool('Lu.Enabled',FConfig_VaLuEnabled);
  aWriter.WriteInteger('Lu.MinY',FConfig_VaLuMinY);
  aWriter.WriteInteger('Lu.MaxY',FConfig_VaLuMaxY);
  aWriter.WriteInteger('Ot.Engine',integer(FConfig_VaOtEngine));

  aWriter.WriteBytes('Filters',RawToBytes(pointer(FConfig_VaFilters)^,Length(FConfig_VaFilters)*sizeof(TVaFilter)));

  aWriter.WriteBool('Schedule.Enabled',FConfig_ScheduleEnabled);
  aWriter.WriteBytes('Schedule.Items',RawToBytes(pointer(FConfig_Schedule)^,Length(FConfig_Schedule)*sizeof(TScheduleItem)));
end;

procedure TMediaProcessor_VA_Any.ShowCustomProperiesDialog;
var
  aDialog: TfmMediaProcessingSettingsVa_Any;
  i,k: Integer;
  j: TVaEventType;
begin
  aDialog:=TfmMediaProcessingSettingsVa_Any.Create(nil);
  try
    aDialog.edVideoAnalyticsConfigFilePath.Path:=FConfig_VaConfigFileName;
    aDialog.edVideoAnalyticsModelFilePath.Path:=FConfig_VaModelFileName;
    aDialog.ckVideoAnalyticsAsyncProcess.Checked:=FConfig_VaAsyncProcessing;
    aDialog.ckVideoAnalyticsOtEnabled.Checked:=FConfig_VaOtEnabled;

    aDialog.ckSchedule.Checked:=FConfig_ScheduleEnabled;

    aDialog.taSchedule.DisableControls;
    try
      aDialog.taSchedule.EmptyTable;
      aDialog.taSchedule.Open;
      for i:=0 to High(FConfig_Schedule) do
      begin
        aDialog.taSchedule.Append;
        aDialog.taScheduleFROM.Value:=FConfig_Schedule[i].From;
        aDialog.taScheduleTO.Value:=FConfig_Schedule[i].To_;
        aDialog.taSchedule.Post;
      end;
      aDialog.taSchedule.First;
    finally
      aDialog.taSchedule.EnableControls;
    end;

    aDialog.ckVaLuEnabled.Checked:=FConfig_VaLuEnabled;
    aDialog.edVaLuMinLevel.Value:=FConfig_VaLuMinY;
    aDialog.edVaLuMaxLevel.Value:=FConfig_VaLuMaxY;
    aDialog.ckOtUseSynesis.Checked:=FConfig_VaOtEngine=otvSynesis;

    if Length(FConfig_VaFilters)>0 then
    begin
      for i := 0 to High(FConfig_VaFilters) do
      begin
        if FConfig_VaFilters[i].FilterType=vaftTresholds then
        begin
          aDialog.ckVideoAnalyticsFiltersTresholds.Checked:=true;
          aDialog.edMinSquarePx.Value:=FConfig_VaFilters[i].MinSquarePx;
          aDialog.edMinWidthPx.Value:=FConfig_VaFilters[i].MinWidthPx;
          aDialog.edMinHeightPx.Value:=FConfig_VaFilters[i].MinHeightPx;
          aDialog.edMinPeriodMs.Value:=FConfig_VaFilters[i].MinPeriodMs;
        end
        else if FConfig_VaFilters[i].FilterType=vaftEvents then
        begin
          aDialog.ckVideoAnalyticsFiltersEvents.Checked:=true;
          for j:=Low(TVaEventType) to High(TVaEventType) do
            aDialog.lvVideoAnalyticsFiltersEvents.FindData(0,pointer(j),true,false).Checked:=j in FConfig_VaFilters[i].Events;
        end;
      end;
    end;

    aDialog.UpdateControlStates;

    if aDialog.ShowModal=mrOk then
    begin
      FConfig_VaConfigFileName:=aDialog.edVideoAnalyticsConfigFilePath.Path;
      FConfig_VaModelFileName:=aDialog.edVideoAnalyticsModelFilePath.Path;
      FConfig_VaAsyncProcessing:=aDialog.ckVideoAnalyticsAsyncProcess.Checked;
      FConfig_VaOtEnabled:=aDialog.ckVideoAnalyticsOtEnabled.Checked;

      FConfig_ScheduleEnabled:=aDialog.ckSchedule.Checked;

      FConfig_VaLuEnabled:=aDialog.ckVaLuEnabled.Checked;
      FConfig_VaLuMinY:=aDialog.edVaLuMinLevel.Value;
      FConfig_VaLuMaxY:=aDialog.edVaLuMaxLevel.Value;

      if aDialog.ckOtUseSynesis.Checked then
        FConfig_VaOtEngine:=otvSynesis
      else
        FConfig_VaOtEngine:=otvOpenCV;

      aDialog.taSchedule.DisableControls;
      try
        aDialog.taSchedule.First;
        SetLength(FConfig_Schedule,aDialog.taSchedule.RecordCount);
        i:=0;
        while not aDialog.taSchedule.Eof do
        begin
          FConfig_Schedule[i].From:=aDialog.taScheduleFROM.Value;
          FConfig_Schedule[i].To_:=aDialog.taScheduleTO.Value;
          inc(i);
          aDialog.taSchedule.Next;
        end;
      finally
        aDialog.taSchedule.EnableControls;
      end;

      SetLength(FConfig_VaFilters,0);
      if aDialog.ckVideoAnalyticsFiltersTresholds.Checked then
      begin
        i:=Length(FConfig_VaFilters);
        SetLength(FConfig_VaFilters,i+1);
        FConfig_VaFilters[i].FilterType:=vaftTresholds;
        FConfig_VaFilters[i].MinSquarePx:=aDialog.edMinSquarePx.Value;
        FConfig_VaFilters[i].MinWidthPx:=aDialog.edMinWidthPx.Value;
        FConfig_VaFilters[i].MinHeightPx:=aDialog.edMinHeightPx.Value;
        FConfig_VaFilters[i].MinPeriodMs:=aDialog.edMinPeriodMs.Value;
      end;

      if aDialog.ckVideoAnalyticsFiltersEvents.Checked then
      begin
        i:=Length(FConfig_VaFilters);
        SetLength(FConfig_VaFilters,i+1);
        FConfig_VaFilters[i].FilterType:=vaftEvents;
        FConfig_VaFilters[i].Events:=[];
        for k := 0 to aDialog.lvVideoAnalyticsFiltersEvents.Items.Count-1 do
        begin
          if aDialog.lvVideoAnalyticsFiltersEvents.Items[k].Checked then
            include(FConfig_VaFilters[i].Events,TVaEventType(aDialog.lvVideoAnalyticsFiltersEvents.Items[k].Data));
        end;
      end;

    end;
  finally
    aDialog.Free;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessor(TMediaProcessor_VA_Any);

end.
