{***********************************<_INFO>************************************}
{  <Проект>      Видеоплейер                                                   }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Обертка для доступа к файловому хранилищу                     }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        16.12.2008                                                    }
{                                                                              }
{  <Атрибуты>    ООО "Трисофт"                                                 }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaStorage.RecordStorage.LocalCopy;

//{$DEFINE STATIC_LINK_FILESTORAGE}

interface

uses
  Windows, SysUtils, Classes, IniFiles,
  MediaStorage.Transport,
  MediaStorage.Consts,MediaStorage.RecordStorage, MediaStorage.RecordStorage.DirectDB;

type
  TRecordStorageLocalCopy = class (TRecordStorage)
  private
    FRecordSources:TrsRecordSourceInfoArray;
    FLoaded: boolean;
    FTransportFactory: IRecordObjectTransportFactory;

    function  FindRecordSourceById(const aSourceId: TrsRecordSourceId; out aInfo: TrsRecordSourceInfo): boolean;
    procedure GetRecordSourceAttributesById(const aSourceId: TrsRecordSourceId; out aName,aUrl: string; out aConnectionString: string);

    function GetFileStorageById(const aSourceId: TrsRecordSourceId): TRecordStorageDirectDB;
    procedure ReleaseFileStorage(aFileStorage: TRecordStorageDirectDB);
  public
    constructor Create(const aTransportFactory: IRecordObjectTransportFactory);
    destructor Destroy; override;

    procedure CheckAvailable; override;
    function IsAvailable: boolean; override;

    function GetConnectionString: string; override;

    //Дать самую максимальную и самую минимальную из всех имеюшихся дат файлов
    procedure GetMinMaxDate(out aMin,aMax: TDateTime); override;

    //получить информацию обо всех источниках, хранимых в БД
    function  GetAllRecordSources:TrsRecordSourceInfoArray; override;
    //получить информацию обо всех файлах в указанном источнике
    function  GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime; aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray; overload; override;

    //получить информацию о событиях датчика движения
    function  GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray; override;

    procedure GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime); override;

    //Достает из хранилища конфигурацию плейера. Если ее нет, возвращает nil
    function  GetFile(const aFileName: string):TrsFileLink; override;

    procedure SetAllRecordSources(aValues: TrsRecordSourceInfoArray);
  end;


function GetLocalConfigurationFolder: string;

implementation

uses
  uBaseUtils,uTrace;

function GetLocalConfigurationFolder: string;
begin
  result:=ExtractFileDir(ParamStr(0))+'\FS';
end;

{ TRecordStorageLocalCopy }

procedure TRecordStorageLocalCopy.CheckAvailable;
begin
  if not IsAvailable then

    raise Exception.Create('Файловое хранилище недоступно');
end;

constructor TRecordStorageLocalCopy.Create(const aTransportFactory: IRecordObjectTransportFactory);
const
  aMethodName = 'TRecordStorageLocalCopy.GetAllRecordSources';
var
  aPath: string;
  aIni: TIniFile;
  aTraceID : cardinal;
  i: integer;
  aSectionName: string;
  aId: TrsRecordSourceId;
  aInfo: TrsRecordSourceInfo;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    FTransportFactory:=aTransportFactory;
    ForceDirectories(GetLocalConfigurationFolder);

    aPath:=GetLocalConfigurationFolder+'\LocalCopy.cfg';
    if FileExists(aPath) then
    begin
      FLoaded:=true;
      aIni:=TIniFile.Create(aPath);
      try
        i:=aIni.ReadInteger('General','RecordSourceCount',0);
        SetLength(FRecordSources,i);
        for i:=0 to i-1 do
        begin
          aSectionName:='Record Source '+IntToStr(i+1);

          FRecordSources[i].Name:=aIni.ReadString(aSectionName,'Name','');
          FRecordSources[i].Url:=aIni.ReadString(aSectionName,'Address','');

          aId.Init(aIni.ReadString(aSectionName,'Id',''));
          if FindRecordSourceById(aId,aInfo) then
          begin
            TraceLine(Format('Обнаружен повторяющийся идентификатор источника %s. Он уже используется для источника c именем %s. Источнику %s будет выдан другой идентификатор',[aId.Name,aInfo.Name,FRecordSources[i].Name]));
            CreateGUID(aId.Guid);
          end;

          FRecordSources[i].Id:=aId;
          FRecordSources[i].ReserveConnectionString:=StringReplace(aIni.ReadString(aSectionName,'ReserveConnectionString',''),'#13#10',#13#10,[rfReplaceAll]);
          FRecordSources[i].Available:=FRecordSources[i].ReserveConnectionString<>'';
        end;
      finally
        aIni.Free;
      end;
    end;
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

destructor TRecordStorageLocalCopy.Destroy;
begin
  inherited;
end;

function TRecordStorageLocalCopy.FindRecordSourceById(
  const aSourceId: TrsRecordSourceId; out aInfo: TrsRecordSourceInfo): boolean;
var
  i: Integer;
begin
  result:=false;
  for i := 0 to High(FRecordSources) do
  begin
    if IsEqualGUID(FRecordSources[i].Id.Guid,aSourceId.Guid) or (FRecordSources[i].Id.Name=aSourceId.Name) then
    begin
      aInfo:=FRecordSources[i];
      result:=true;
      exit;
    end;
  end;
end;

function TRecordStorageLocalCopy.GetAllRecordSources: TrsRecordSourceInfoArray;
const
  aMethodName = 'TRecordStorageLocalCopy.GetAllRecordSources';
var
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    result:=FRecordSources;
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordStorageLocalCopy.GetConnectionString: string;
begin
  result:='Local Copy';
end;

function TRecordStorageLocalCopy.GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime, aToTime: TDateTime): TrsEventInfoArray;
var
  aRS: TRecordStorageDirectDB;
  aName,aUrl: string;
  aConnectionString: string;
begin
  GetRecordSourceAttributesById(aSourceId,aName,aUrl,aConnectionString);

  aRS:=GetFileStorageById(aSourceId);
  try
    result:=aRS.GetEvents(aRS.GetRecordSourceIdByAddress(aUrl),aFromTime,aToTime);
  finally
    ReleaseFileStorage(aRS);
  end;
end;

function TRecordStorageLocalCopy.GetFileStorageById(const aSourceId: TrsRecordSourceId): TRecordStorageDirectDB;
const
  aMethodName = 'TRecordStorageLocalCopy.GetFileStorageById';
var
  aTraceID : cardinal;
  aName,aUrl: string;
  aConnectionString: string;
begin
  aTraceID:=TraceProcBegin(aMethodName,aSourceId.Name);
  result:=nil;
  try
    GetRecordSourceAttributesById(aSourceId,aName,aUrl,aConnectionString);
    result:=TRecordStorageDirectDB.Create(
      FTransportFactory,
      TRecordStorageDirectDbConnector.CreateAsConnectionString(aConnectionString));

    try
      result.CheckAvailable;
    except
      FreeAndNil(result);
      raise;
    end;
  finally
    if result=nil then
      TraceProcEnd(aMethodName+' К указанному хранилищу подключиться не удалось',aTraceID)
    else
      TraceProcEnd(aMethodName+' Хранилище найдено и успешно инициализировано. ConnectionString='+StringReplace(result.GetConnectionString,#13#10,';',[rfReplaceAll]),aTraceID)
  end;
end;

procedure TRecordStorageLocalCopy.GetMinMaxDate(out aMin, aMax: TDateTime);
begin
  raise Exception.Create('В режиме работы без центрального управляющего сервера данная операция невозможна');
end;

function TRecordStorageLocalCopy.GetFile(const aFileName: string):TrsFileLink;
const
  aMethodName = 'TRecordStorageLocalCopy.GetPlayerAdminConfig';
var
  aTraceID : cardinal;
  aFileName2: string;
begin
  result:=nil;
  aTraceID:=TraceProcBegin(aMethodName);
  try
    aFileName2:=GetLocalConfigurationFolder+'\'+aFileName;
    if not FileExists(aFileName2) then
      exit;

    result:=TrsFileLink.Create(aFileName2,false);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordStorageLocalCopy.GetRecords(const aSourceId: TrsRecordSourceId;const aFromTime, aToTime: TDateTime; aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray;
var
  aRS: TRecordStorageDirectDB;
  aName,aUrl: string;
  aConnectionString: string;
begin
  GetRecordSourceAttributesById(aSourceId,aName,aUrl,aConnectionString);

  aRS:=GetFileStorageById(aSourceId);
  try
    result:=aRS.GetRecords(aRS.GetRecordSourceIdByAddress(aUrl),aFromTime,aToTime,aHandler);
  finally
    ReleaseFileStorage(aRS);
  end;
end;

procedure TRecordStorageLocalCopy.GetRecordSourceAttributesById(
  const aSourceId: TrsRecordSourceId; out aName, aUrl: string;
  out aConnectionString: string);
var
  aInfo: TrsRecordSourceInfo;
begin
  if not FindRecordSourceById(aSourceId,aInfo) then
    raise Exception.Create('Неизвестный Id камеры');

  aName:=aInfo.Name;
  aUrl:=aInfo.Url;
  aConnectionString:=aInfo.ReserveConnectionString;
end;

procedure TRecordStorageLocalCopy.GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart, aEnd: TDateTime);
var
  aRS: TRecordStorageDirectDB;
  aName,aUrl: string;
  aConnectionString: string;
begin
  GetRecordSourceAttributesById(aSourceId,aName,aUrl,aConnectionString);

  aRS:=GetFileStorageById(aSourceId);
  try
    aRs.GetRecordSourceDateRanges(aRS.GetRecordSourceIdByAddress(aUrl),aStart,aEnd);
  finally
    ReleaseFileStorage(aRS);
  end;
end;

function TRecordStorageLocalCopy.IsAvailable: boolean;
begin
  result:=FLoaded;
end;

procedure TRecordStorageLocalCopy.ReleaseFileStorage(
  aFileStorage: TRecordStorageDirectDB);
begin
  aFileStorage.Free;
end;

procedure TRecordStorageLocalCopy.SetAllRecordSources(aValues: TrsRecordSourceInfoArray);
var
  i: Integer;
  aSectionName: string;

  aPath: string;
  aIni: TIniFile;

begin
  aPath:=GetLocalConfigurationFolder+'\LocalCopy.cfg';
  DeleteFile(aPath);

  aIni:=TIniFile.Create(aPath);
  try
    aIni.WriteInteger('General','RecordSourceCount',Length(aValues));

    for i := 0 to High(aValues) do
    begin
      aSectionName:='Record Source '+IntToStr(i+1);
      aIni.WriteString(aSectionName,'Id',aValues[i].Id.Name);
      aIni.WriteString(aSectionName,'Name',aValues[i].Name);
      aIni.WriteString(aSectionName,'Address',aValues[i].Url);
//      aIni.WriteInteger(aSectionName,'Channel',aValues[i].ChannelNo);
      aIni.WriteString(aSectionName,'ReserveConnectionString', StringReplace(aValues[i].ReserveConnectionString,#13#10,'#13#10',[rfReplaceAll]));
    end;
  finally
    aIni.Free;
  end;
end;

end.




