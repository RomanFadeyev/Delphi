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
unit MediaStorage.RecordStorage.MediaServer;

//{$DEFINE STATIC_LINK_FILESTORAGE}

interface

uses
  Windows, SysUtils, Classes,SyncObjs,
  MediaStorage.Transport, MediaStorage.RecordStorage, MediaStorage.Consts, MediaServer.Net.Mscp.Definitions, MediaServer.Net.Mscp.Client;

type
  TRecordStorageMediaServer = class (TRecordStorage)
  private
    FMscpClient: TMscpClient;

    FLock       : TCriticalSection;
    FConnectionString: string;
    FConnectionThread: TThread;
    FConnectionThreadSucceded: boolean;
    FTransportFactory: IRecordObjectTransportFactory;
  private
    FUserPassword: string;
    FUserName: string;
    FLongOperationTimeout: integer;
    function CreateRecordObjectTransport(const aFileName:string): IRecordObjectTransport;
    procedure FillRecordInfoFromFileObject(const aFileObject: TMscpHistoryFileInfo; var aInfo: TrsRecordObjectInfo);
    procedure SortRecords(var aRecords: TrsRecordObjectInfoArray);

    constructor CreateInternal;
  public
    constructor Create(const aTransportFactory: IRecordObjectTransportFactory; const aConnectionString: string;  aThrowIfNoConnection: boolean=true);
    destructor  Destroy; override;

    function GetConnectionString: string; override;

    function  IsAvailable: boolean; override;
    procedure CheckAvailable; override;

    //Дать самую максимальную и самую минимальную из всех имеюшихся дат файлов
    procedure GetMinMaxDate(out aMin,aMax: TDateTime); override;

    //получить информацию обо всех источниках, хранимых в БД
    function  GetAllRecordSources:TrsRecordSourceInfoArray; override;
    //получить информацию обо всех файлах в указанном источнике
    function  GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime; aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray; overload; override;

    //получить информацию о событиях датчика движения
    function  GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray; override;

    procedure GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime); override;

    //Достает из хранилища указанный файл и возвращает путь к нему. Если файл не найден, возвращает nil
    function  GetFile(const aFileName: string):TrsFileLink; override;

    procedure RunConnectionThread;
    //property  ConnectionThreadSucceded: boolean read FConnectionThreadSucceded;

    function  IsRecordSourceAvailable(aSourceId: TrsRecordSourceId):boolean;

    property  UserName: string read FUserName write FUserName;
    property  UserPassword: string read FUserPassword write FUserPassword;

    property  LongOperationTimeout: integer read FLongOperationTimeout write FLongOperationTimeout;
    property  ConnectionString: string read FConnectionString;
  end;

const
  ArchivePointDelimiter = '/';

implementation

uses
  StrUtils, uBaseClasses,uBaseUtils, uTrace,MediaServer.Net.Definitions;

type
  TMediaServerRecordStorageConnectionThread = class (TThread)
  private
    FOwner: TRecordStorageMediaServer;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TRecordStorageMediaServer);
  end;

  TMediaServerReadProgressHandlerMediator = class
  private
    FOuterHandler: TRecordStorageReadProgress;
    FOwner: TRecordStorageMediaServer;
  public
    procedure OnRead(aSender: TMscpClient; aPosition, aSize: int64; var aContinue: boolean);
    constructor Create(aOwner: TRecordStorageMediaServer; aHander: TRecordStorageReadProgress);
  end;


{ TMediaServerReadProgressHandlerMediator }

constructor TMediaServerReadProgressHandlerMediator.Create(
  aOwner: TRecordStorageMediaServer; aHander: TRecordStorageReadProgress);
begin
  FOwner:=aOwner;
  FOuterHandler:=aHander;
end;

procedure TMediaServerReadProgressHandlerMediator.OnRead(aSender: TMscpClient;
  aPosition, aSize: int64; var aContinue: boolean);
begin
  FOuterHandler(FOwner,aPosition,aSize,aContinue);
end;

// -----------------------------------------------------------------------------
{ TRecordStorageMediaServer }
// -----------------------------------------------------------------------------
constructor TRecordStorageMediaServer.CreateInternal;
begin
  FLock:=TCriticalSection.Create;
end;

constructor TRecordStorageMediaServer.Create(const aTransportFactory: IRecordObjectTransportFactory; const aConnectionString: string;  aThrowIfNoConnection: boolean=true);
begin
  CreateInternal;

  FLongOperationTimeout:=60*1000;
  FTransportFactory:=aTransportFactory;
  Assert(FTransportFactory<>nil);

  FConnectionString:=aConnectionString;

  try
    CheckAvailable;
  except
    if aThrowIfNoConnection then
      raise;
  end;

//  CheckAvailable;
end;
// -----------------------------------------------------------------------------
destructor TRecordStorageMediaServer.Destroy;
begin
  FreeAndNil(FConnectionThread);
  FreeAndNil(FMscpClient);
  FreeAndNil(FLock);
  inherited;
end;
// -----------------------------------------------------------------------------
function TRecordStorageMediaServer.GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime, aToTime: TDateTime;aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray;
const
  aMethodName = 'TRecordStorageMediaServer.GetRecords';
var
  aTraceID : cardinal;
  aFiles: TMscpHistoryFileInfoArray;
  i: Integer;
  aOldReadTimeout: integer;
  aHandlerMediator: TMediaServerReadProgressHandlerMediator;
  s1,s2: string;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    CheckAvailable;

    aOldReadTimeout:=FMscpClient.ReadTimeout;
    try
      FMscpClient.ReadTimeout:=FLongOperationTimeout;
      if Assigned(aHandler) then
      begin
        aHandlerMediator:=TMediaServerReadProgressHandlerMediator.Create(self,aHandler);
        try
          fsiBaseUtils.SplitString(aSourceId.Name,s1,s2, ArchivePointDelimiter);
          FMscpClient.SendCommandHistoryGetFiles(s1,FUserName,FUserPassword,s2,aFromTime,aToTime,aFiles,aHandlerMediator.OnRead);
        finally
          FreeAndNil(aHandlerMediator);
        end;
      end
      else begin
        fsiBaseUtils.SplitString(aSourceId.Name,s1,s2, ArchivePointDelimiter);
        FMscpClient.SendCommandHistoryGetFiles(s1,FUserName,FUserPassword,s2,aFromTime,aToTime,aFiles);
      end;
    finally
      FMscpClient.ReadTimeout:=aOldReadTimeout;
    end;
    SetLength(result,Length(aFiles));

    for i := 0 to High(aFiles) do
    begin
      TraceLine(Format('Файл %d: %s',[i+1,aFiles[i].Path]));
      FillRecordInfoFromFileObject(aFiles[i],result[i]);
    end;

    SortRecords(result);
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName+Format('. Найдено %d записей',[Length(result)]),aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.FillRecordInfoFromFileObject(const aFileObject: TMscpHistoryFileInfo; var aInfo: TrsRecordObjectInfo);
begin
  aInfo.Transport:=CreateRecordObjectTransport(aFileObject.Path);
  aInfo.Id:=aFileObject.Id;
  aInfo.StartDateTime:=aFileObject.StartDatetime;
  aInfo.EndDateTime:=aFileObject.EndDatetime;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.CheckAvailable;
begin
  try
    if FMscpClient<>nil then
      FMscpClient.SendCommandPing;
  except
    FreeAndNil(FMscpClient);
  end;

  //TODO выделить из адреса порт
  if FMscpClient=nil then
    FMscpClient:=TMscpClient.Create(FConnectionString,icMscpServerPort,GetTraceEnabled);
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime);
const
  aMethodName = 'TRecordStorageMediaServer.GetRecordSourceEndDate';
var
  aTraceID : cardinal;
  aOldReadTimeout: integer;
  s1,s2: string;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    CheckAvailable;

    aOldReadTimeout:=FMscpClient.ReadTimeout;
    try
      FMscpClient.ReadTimeout:=FLongOperationTimeout;
      fsiBaseUtils.SplitString(aSourceId.Name,s1,s2, ArchivePointDelimiter);
      FMscpClient.SendCommandHistoryGetRanges(s1,FUserName,FUserPassword,s2, aStart,aEnd);
    finally
      FMscpClient.ReadTimeout:=aOldReadTimeout;
    end;

    //Есть файлы с началом записи, но у них нет окончания
    if aEnd=0 then
      aStart:=0;
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageMediaServer.IsAvailable: boolean;
begin
  result:=FMscpClient<>nil;
end;
function TRecordStorageMediaServer.IsRecordSourceAvailable(
  aSourceId: TrsRecordSourceId): boolean;
var
  aMediaServerNodes: TMscpNodeInfoArray;
  i,j: integer;
begin
  FLock.Enter;
  try
    CheckAvailable;
    aMediaServerNodes:=FMscpClient.SendCommandGetNodeInfos;
    for i := 0 to High(aMediaServerNodes) do
      for j := 0 to High(aMediaServerNodes[i].ArchiveSources) do
        if (aMediaServerNodes[i].NodeName=aSourceId.Name) {OLD} or
          (aSourceId.Name= aMediaServerNodes[i].NodeName+ArchivePointDelimiter+aMediaServerNodes[i].ArchiveSources[j].Id) then
          exit(aMediaServerNodes[i].HasArchive);
  finally
    FLock.Leave;
  end;

  result:=false;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.RunConnectionThread;
begin
  FreeAndNil(FConnectionThread);
  FConnectionThreadSucceded:=false;
  FConnectionThread:=TMediaServerRecordStorageConnectionThread.Create(self);
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.GetMinMaxDate(out aMin,aMax: TDateTime);
const
  aMethodName = 'TRecordStorageMediaServer.GetMinMaxDate';
var
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    aMin:=0;
    aMax:=0;

    if aMax=0 then
      aMax:=aMin;

  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageMediaServer.GetAllRecordSources: TrsRecordSourceInfoArray;
const
  aMethodName = 'TRecordStorageMediaServer.GetAllRecordSources';
var
  aTraceID : cardinal;
  i,j,k:integer;
  aSourceInfos: TMscpNodeInfoArray;
begin
  result:=nil;

  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    CheckAvailable;
    aSourceInfos:=FMscpClient.SendCommandGetNodeInfos;
    Result:=nil;

    k:=0;
    for i := 0 to High(aSourceInfos) do
      for j:=0 to High(aSourceInfos[i].ArchiveSources) do
        inc(k);

    SetLength(result,k);

    k:=0;
    for i := 0 to High(aSourceInfos) do
    begin
      for j:=0 to High(aSourceInfos[i].ArchiveSources) do
      begin
        result[k].Name:=aSourceInfos[i].NodeName+ArchivePointDelimiter+aSourceInfos[i].ArchiveSources[j].Description;
        result[k].Id.Init(aSourceInfos[i].NodeName+ArchivePointDelimiter+aSourceInfos[i].ArchiveSources[j].Id);
        result[k].Available:=true;
        inc(k);
      end;
    end;

  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageMediaServer.GetFile(const aFileName: string): TrsFileLink;
const
  aMethodName = 'TRecordStorageMediaServer.GetFile';
var
  aTraceID : cardinal;
begin
  result:=nil;
  aTraceID:=TraceProcBegin(aMethodName);
  try
    raise ENotSupported.Create;
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageMediaServer.CreateRecordObjectTransport(const aFileName:string): IRecordObjectTransport;
begin
  Assert(FTransportFactory<>nil);
  result:=FTransportFactory.CreateTransport(aFileName);
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageMediaServer.SortRecords(var aRecords: TrsRecordObjectInfoArray);
var
  i,j: integer;
  aSwapped: boolean;
  p:TrsRecordObjectInfo;
begin
  //Сортируем
  for i:=0 to High(aRecords) do
  begin
    aSwapped:=false;
    for j:=0 to High(aRecords)-1 do
    begin
      if aRecords[j].StartDateTime>aRecords[j+1].StartDateTime then
      begin
        p:=aRecords[j];
        aRecords[j]:=aRecords[j+1];
        aRecords[j+1]:=p;
        aSwapped:=true;
      end;
    end;

    if not aSwapped then
      break;
  end;
end;

function TRecordStorageMediaServer.GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray;
const
  aMethodName = 'TRecordStorageMediaServer.GetEvents';
var
  aTraceID : cardinal;
begin
  FLock.Enter;
  aTraceID:=TraceProcBegin(aMethodName);
  try
    result:=nil;
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName+Format('. Найдено %d записей',[Length(result)]),aTraceID);
  end;
end;

function TRecordStorageMediaServer.GetConnectionString: string;
begin
  result:=FConnectionString;
end;

{ TMediaServerRecordStorageConnectionThread }

constructor TMediaServerRecordStorageConnectionThread.Create( aOwner: TRecordStorageMediaServer);
begin
  FOwner:=aOwner;

  inherited Create(false);
end;

procedure TMediaServerRecordStorageConnectionThread.Execute;
var
  i: Integer;
begin
  while not Terminated do
  begin
    FOwner.FLock.Enter;
    try
      try
        FOwner.CheckAvailable;
        FOwner.FConnectionThreadSucceded:=true;

        //Удалось подключиться. Завершаем работу
        break;
      except
      end;
    finally
      FOwner.FLock.Leave;
    end;

    //1 минуту отдыхаем
    for i := 0 to 60*10 do
    begin
      sleep(100);
      if Terminated then
        break;
    end;
  end;
end;



end.




