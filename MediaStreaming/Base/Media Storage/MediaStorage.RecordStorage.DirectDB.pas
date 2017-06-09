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
unit MediaStorage.RecordStorage.DirectDB;

//{$DEFINE STATIC_LINK_FILESTORAGE}

interface

uses
  Windows, SysUtils, Classes,SyncObjs, Generics.Collections, System.Generics.Defaults,
  dInterfacesObjectFileStorage, uObjectFileStorage, dInterfacesSysUtilsDB, MediaStorage.Transport,
  MediaStorage.RecordStorage, MediaStorage.Consts;

type
  TRecordStorageDirectDbConnector = class
  private
    FConnectionSource: string;
    FConnectionType: integer; //
  public
    constructor CreateAsUdl(const aUdlFileName: string);
    constructor CreateAsConnectionString(const aConnectionString: string);

    property ConnectionSource: string read FConnectionSource;
    property ConnectionType: integer read FConnectionType;
  end;

  TRecordStorageDirectDB = class (TRecordStorage,IRecordWriteableAccess,IRecordSourceWriteableAccess)
  private
    FFileStorage: IObjectFileStorageAPI;
    FLock       : TCriticalSection;
    FConnector    : TRecordStorageDirectDbConnector;
    FConnectionThread: TThread;
    FConnectionThreadSucceded: boolean;
    FTransportFactory: IRecordObjectTransportFactory;
  private
    function CreateRecordObjectTransport(const aFileObject: IFileObject): IRecordObjectTransport;
    procedure FillRecordInfoFromFileObject(const aFileObject: IFileObjectExtVideo; var aInfo: TrsRecordObjectInfo);
    procedure SortRecords(var aRecords: TrsRecordObjectInfoArray);

    constructor CreateInternal;
  protected
    property FileStorage: IObjectFileStorageAPI read FFileStorage;
  public
    constructor Create(const aTransportFactory: IRecordObjectTransportFactory; aConnector: TRecordStorageDirectDbConnector;  aThrowIfNoConnection: boolean=true);
    destructor  Destroy; override;

    function GetConnectionString: string; override;

    procedure CheckAvailable; override;
    function  IsAvailable: boolean; override;

    //Дать самую максимальную и самую минимальную из всех имеюшихся дат файлов
    procedure GetMinMaxDate(out aMin,aMax: TDateTime); override;

    //получить информацию обо всех источниках, хранимых в БД
    function  GetAllRecordSources:TrsRecordSourceInfoArray; override;
    //получить информацию обо всех файлах в указанном источнике
    function  GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime;aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray; overload; override;

    //получить информацию о событиях датчика движения
    function  GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray; override;

    procedure GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime); override;

    //Достает из хранилища указанный файл и возвращает путь к нему. Если файл не найден, возвращает nil
    function  GetFile(const aFileName: string):TrsFileLink; override;

    //Удаляет запись по ее Id
    procedure DeleteRecord(const aId: string);

    //вставить записи для указанного источника
    procedure MoveRecords(const aRecordSourceId, aDestRecordSourceId: TrsRecordSourceId);

    //Удаляет источник по его Id
    procedure DeleteRecordSource(const aId: TrsRecordSourceId);

    procedure RunConnectionThread;
    //property  ConnectionThreadSucceded: boolean read FConnectionThreadSucceded;

    function GetRecordSourceIdByAddress(const aAddress: string): TrsRecordSourceId;

    property  Connector: TRecordStorageDirectDbConnector read FConnector;

    function  RecordWriteableAccess: IRecordWriteableAccess; override;
    function  RecordSourceWriteableAccess: IRecordSourceWriteableAccess; override;
  end;

implementation

uses
  uBaseUtils,uTrace, uAppParams
  {$IFDEF STATIC_LINK_FILESTORAGE}
  ,uObjectFileStorage_impl
  {$ENDIF}
  ;

const
  ConnectionSourceType_Udl=1;
  ConnectionSourceType_ConnectionString=0;     // fix Пономаренко. было: =1

type
  TDirectDBRecordStorageConnectionThread = class (TThread)
  private
    FOwner: TRecordStorageDirectDB;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TRecordStorageDirectDB);
  end;


// -----------------------------------------------------------------------------
{ TRecordStorageDirectDB }
// -----------------------------------------------------------------------------
constructor TRecordStorageDirectDB.CreateInternal;
const
  NotInitializedMessage = 'Файловое хранилище не инициализировано';
var
  s: string;
begin
  FLock:=TCriticalSection.Create;

  {$IFDEF STATIC_LINK_FILESTORAGE}
  FFileStorage:=uObjectFileStorage_impl.GetXClassIface as IObjectFileStorageAPI;
  {$ELSE}
  FFileStorage:=fsiObjectFileStorage;
  {$ENDIF}

  if FFileStorage=nil then
    raise Exception.Create(NotInitializedMessage);


  FFileStorage.PingBeforeConnect:=GetAppParamBoolean(ClassName,'FileStorage.PingBeforeConnect',false);

  if GetTraceEnabled then
  begin
    s:=ChangeFileExt(GetTraceFileName,'.fs.trace');
    FFileStorage.SetTraceFile(s);
  end
  else
    FFileStorage.SetTraceFile('');

  //Защита от старой версии фабрики компонентов.
  //Assert(FFileStorage.GetGroupsCount>=0);
end;

constructor TRecordStorageDirectDB.Create(const aTransportFactory: IRecordObjectTransportFactory; aConnector: TRecordStorageDirectDbConnector; aThrowIfNoConnection: boolean=true);
begin
  CreateInternal;

  FTransportFactory:=aTransportFactory;
  Assert(FTransportFactory<>nil);

  FConnector:=aConnector;
  Assert(FConnector<>nil);

  try
    if FConnector.ConnectionType=ConnectionSourceType_Udl then
      FFileStorage.SetConnection(FConnector.ConnectionSource,true,'')
    else
      FFileStorage.SetConnection('',true,FConnector.ConnectionSource);
  except
    if aThrowIfNoConnection then
      raise;
  end;

//  CheckAvailable;
end;
// -----------------------------------------------------------------------------
destructor TRecordStorageDirectDB.Destroy;
begin
  FreeAndNil(FConnectionThread);
  try
    FFileStorage:=nil;
  except
  end;
  FreeAndNil(FConnector);
  FreeAndNil(FLock);
  inherited;
end;
// -----------------------------------------------------------------------------
function TRecordStorageDirectDB.GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime, aToTime: TDateTime;aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray;
const
  aMethodName = 'TRecordStorageDirectDB.GetRecords(2)';
var
  aTraceID : cardinal;
  aFileObjectList: IFileObjectExtVideoList;
  i: integer;
  aFileObject: IFileObjectExtVideo;
  aFilter: ISFilter;
  aExpr: ISFilterSubExpression;
begin
  aTraceID:=TraceProcBegin(aMethodName,'aSourceId='+aSourceId.Name);
  FLock.Enter;
  try
    Assert(aSourceId.IsGuidCompartible);
    aFileObjectList:=FFileStorage.FileStorageExtVideo.GetObjectsList;
    aFileObjectList.PageSizeMax:=50000;

    aFilter := aFileObjectList.Filter;
    aExpr:=aFilter.CreateSubExpression.opAND.ValGUID(aFileObjectList.Name_VideoCameraID, Equal, aSourceId.Guid).
      opAND.ValInteger(aFileObjectList.Name_ObjectType, Equal, MediaSignature);

    if aFromTime<>0 then
      aExpr:=aExpr.opAND.ValDateTime(aFileObjectList.Name_EndDatetime, GreaterEqual, aFromTime); //Окончание (а не начало! чтобы было перекрытие) файла должно быть после FromTime

    if aToTime<>0 then
      aExpr:=aExpr.opAND.ValDateTime(aFileObjectList.Name_StartDatetime, LowEqual, aToTime); //Начало (а не окончание! чтобы было перекрытие) файла должно быть до ToTime

    aFilter.Filtered := true;

  // fix Пономаренко. Делаем сразу обновление, чтобы потом не лезть в БД каждый раз
  //  за количеством записей по aFileObjectList.Count
    aFileObjectList.Refresh;

    SetLength(result,aFileObjectList.Count);

    for i:=0 to aFileObjectList.Count-1 do
    begin
      aFileObject:=aFileObjectList.Items[i];
      aFileObject.FullFileNameWithIP;
      //TraceLine(Format('RS:%d:%s',[i,aFileObject.FullFileNameWithIP]));
      FillRecordInfoFromFileObject(aFileObject,result[i]);
      //Assert(result[i].EndDateTime>=aFromTime);
      //Assert(result[i].StartDateTime<=aToTime);
    end;
    SortRecords(result);
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName+Format('. Найдено %d записей',[Length(result)]),aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.FillRecordInfoFromFileObject(const aFileObject: IFileObjectExtVideo; var aInfo: TrsRecordObjectInfo);
begin
  aInfo.Transport:=CreateRecordObjectTransport(aFileObject);
  aInfo.Id:=aFileObject.FileName; //В БД это поле называется NAMEOBJ и является естественным ключом. Именно по нему выполняются все операции. В БД также имеется поле ID, но оно нигде не используется
  aInfo.StartDateTime:=aFileObject.StartDatetime;
  aInfo.EndDateTime:=aFileObject.EndDatetime;
  aInfo.Owner:=aFileObject.HostIP;
  if aInfo.EndDateTime=0 then //Не закрытый файл
    aInfo.EndDateTime:=aInfo.StartDateTime;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.CheckAvailable;
begin
  if not FFileStorage.isConnected then
    FFileStorage.ReconnectDataBase;

  try
    FFileStorage.CheckConnect;
  except
    FFileStorage.ReconnectDataBase;
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime);
const
  aMethodName = 'TRecordStorageDirectDB.GetRecordSourceEndDate';
var
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    Assert(aSourceId.IsGuidCompartible);

    aStart:=0;
    aEnd:=0;
    with FFileStorage.FileStorageExtVideo.ObjectsList do
    begin
      Filter.CreateSubExpression.opAND.ValInteger(Name_ObjectType,Equal, MediaSignature);
      Filter.CreateSubExpression.opAND.ValGUID(Name_VideoCameraID,Equal,aSourceId.Guid);

      Filter.CreateAggregateSelect.opMin(Name_StartDatetime); // делаем аггрегацию по полю
      Filter.CreateAggregateSelect.opMax(Name_EndDatetime); // делаем аггрегацию по полю
      Refresh; // делаем сразу запрос с аггрегацией, чтобы не лезть в БД за кол. записей
               // по нижеследующ. условию.
      if Count > 0 then // можно и без условия, т.к. все равно вернет 1 строку, но на всякий для Items[*]...
      begin
        Assert(Count=1);
        aStart := Items[0].StartDatetime; // получаем значение аггрегата
        aEnd:= Items[0].EndDatetime; // получаем значение аггрегата
      end;
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
function TRecordStorageDirectDB.GetRecordSourceIdByAddress(const aAddress: string): TrsRecordSourceId;
const
  aMethodName = 'TRecordStorageDirectDB.GetRecordSourceIdByAttributes';
var
  aTraceID : cardinal;
  aCameraList  : IVideoCamerasList;
  aFilter: ISFilter;
begin
  aTraceID:=TraceProcBegin(aMethodName,aAddress);
  FLock.Enter;
  try
    aCameraList:=FFileStorage.FileStorageExtVideo.GetVideoCamerasList;
    aFilter:=aCameraList.Filter;
    //aFilter.CreateSubExpression.opAND.ValString(aCameraList.Name_Name,Equal,aName);
    aFilter.CreateSubExpression.opAND.ValString(aCameraList.Name_IP,Equal,aAddress);
    //aFilter.CreateSubExpression.opAND.ValString(aCameraList.Name_ConnectionString,Equal,aConnectionString);

    aFilter.Filtered := true;

  // fix Пономаренко. Делаем сразу обновление, чтобы потом не лезть в БД каждый раз
  //  за количеством записей по aFileObjectList.Count
    aCameraList.Refresh;

    if aCameraList.Count>0 then
    begin
      result.Init(aCameraList.Items[0].VideoCameraID);
    end
    else begin
      raise Exception.CreateFmt('Не найден источник записей по указанным атрибутам: Address=%s',[aAddress]);
    end;
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.MoveRecords(const aRecordSourceId, aDestRecordSourceId: TrsRecordSourceId);
var
  aFileObjectList: IFileObjectExtVideoList;
  aFilter: ISFilter;
  aExpr: ISFilterSubExpression;
  i: Integer;
  aFO: IFileObjectExtVideo;
begin
  aFileObjectList:=FFileStorage.FileStorageExtVideo.GetObjectsList;

  aFilter := aFileObjectList.Filter;
  aExpr:=aFilter.CreateSubExpression.opAND.ValGUID(aFileObjectList.Name_VideoCameraID, Equal, aRecordSourceId.Guid).
    opAND.ValInteger(aFileObjectList.Name_ObjectType, Equal, MediaSignature);


  aFilter.Filtered := true;

// fix Пономаренко. Делаем сразу обновление, чтобы потом не лезть в БД каждый раз
//  за количеством записей по aFileObjectList.Count
  aFileObjectList.Refresh;

  for i := 0 to aFileObjectList.Count-1 do
  begin
    aFO:=aFileObjectList.Items[i];
    aFO.VideoCameraID:=aDestRecordSourceId.Guid;
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageDirectDB.IsAvailable: boolean;
begin
  result:=FFileStorage.isConnected;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.RunConnectionThread;
begin
  FreeAndNil(FConnectionThread);
  FConnectionThreadSucceded:=false;
  FConnectionThread:=TDirectDBRecordStorageConnectionThread.Create(self);
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.GetMinMaxDate(out aMin,aMax: TDateTime);
const
  aMethodName = 'TRecordStorageDirectDB.GetMinMaxDate';
var
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    aMin:=0;
    aMax:=0;
    with FFileStorage.FileStorageExtVideo.ObjectsList do
    begin
      Filter.CreateSubExpression.opAND.ValInteger(Name_ObjectType,Equal, MediaSignature);

      Filter.CreateAggregateSelect.opMin(Name_StartDatetime); // делаем аггрегацию по полю
      Filter.CreateAggregateSelect.opMax(Name_EndDatetime); // делаем аггрегацию по полю
      Refresh; // делаем сразу запрос с аггрегацией, чтобы не лезть в БД за кол. записей
               // по нижеследующ. условию.
      if Count > 0 then // можно и без условия, т.к. все равно вернет 1 строку, но на всякий для Items[*]...
      begin
        Assert(Count=1);
        aMin := Items[0].StartDatetime; // получаем значение аггрегата
        aMax := Items[0].EndDatetime;
      end;
    end;

    if aMax=0 then
      aMax:=aMin;

  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
function TRecordStorageDirectDB.GetAllRecordSources: TrsRecordSourceInfoArray;
const
  aMethodName = 'TRecordStorageDirectDB.GetAllRecordSources';
var
  aTraceID : cardinal;
  i:integer;
  aCameraList  : IVideoCamerasList;
  aCamera : IVideoCamera;
  aFilter: ISFilter;
begin
  result:=nil;

  aTraceID:=TraceProcBegin(aMethodName);
  FLock.Enter;
  try
    aCameraList:=FFileStorage.FileStorageExtVideo.GetVideoCamerasList;

    //HACK. Временная мера. Из-за глюка в ФХ появились дубликаты камер с пустыми IP. Их надо исключить
    aFilter:=aCameraList.Filter;
    aFilter.CreateSubExpression.opAND.opNOT.ValIsNull(aCameraList.Name_IP).opAND.ValString(aCameraList.Name_IP, NonEqual, ' ');
    aFilter.Filtered := true;

    SetLength(result,aCameraList.Count);
    for i:=0 to aCameraList.Count-1 do
    begin
      aCamera:=aCameraList.Items[i];
      result[i].Name:=aCamera.Name;
      result[i].Url:=aCamera.URL;
      if result[i].Url='' then
        result[i].Url:=aCamera.IP;
      result[i].Id.Init(aCamera.VideoCameraID);
      result[i].ReserveConnectionString:=aCamera.ConnectionString;
      result[i].Available:=true;
    end;
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.DeleteRecord(const aId: string);
begin
  try
    FFileStorage.GetFileObject(aId).DeleteFileObject(fsfdDeleteFile);
  except
    on E:Exception do
      FFileStorage.GetFileObject(aId).DeleteFileObject(fsfdOnlyDeleteList);
  end;
end;
// -----------------------------------------------------------------------------
procedure TRecordStorageDirectDB.DeleteRecordSource(
  const aId: TrsRecordSourceId);
var
  aFileObjectList: IFileObjectExtVideoList;
  aFilter: ISFilter;
  aExpr: ISFilterSubExpression;
  aEventList: IEventExtVideoList;
  i: integer;
begin
  Assert(aId.IsGuidCompartible);

  aFileObjectList:=FFileStorage.FileStorageExtVideo.GetObjectsList;

  aFilter := aFileObjectList.Filter;
  aExpr:=aFilter.CreateSubExpression.opAND.ValGUID(aFileObjectList.Name_VideoCameraID, Equal, aId.Guid).
    opAND.ValInteger(aFileObjectList.Name_ObjectType, Equal, MediaSignature);

  aFilter.Filtered := true;
  for i:=0 to aFileObjectList.Count-1 do
  begin
    try
      aFileObjectList.Items[i].DeleteFileObject(fsfdDeleteFile)
    except
      aFileObjectList.Items[i].DeleteFileObject(fsfdOnlyDeleteList);
    end;
  end;


  aEventList:=FFileStorage.FileStorageExtVideo.GetEventsList;

  aFilter := aEventList.Filter;
  aFilter.CreateSubExpression.opAND.ValGUID(aEventList.Name_VideoCameraID, Equal, aId.Guid);
  aFilter.Filtered := true;
  aEventList.DeleteAll;

  FFileStorage.FileStorageExtVideo.VideoCamerasList.Delete(aId.Guid);
end;
// -----------------------------------------------------------------------------
function TRecordStorageDirectDB.GetFile(const aFileName: string): TrsFileLink;
const
  aMethodName = 'TRecordStorageDirectDB.GetFile';
var
  aTraceID : cardinal;
  aLocalFileName : string;
  aConfigFile: IFileObject;
  aLocalConfigFileStream : TFileStream;
  aTransportedFile : IRecordObjectReader;
begin
  result:=nil;
  aTraceID:=TraceProcBegin(aMethodName);
  try
    try
      aConfigFile:=FFileStorage.GetFileObject(aFileName,true);
      if aConfigFile=nil then
        exit;

      aTransportedFile:=CreateRecordObjectTransport(aConfigFile).GetReader;

      //Копируем файл конфигурации в локальный собственный файл
      aLocalFileName:=fsiBaseUtils.TempFileName('.cfg');
      aLocalConfigFileStream:=TFileStream.Create(aLocalFileName,fmCreate);

      try
        aLocalConfigFileStream.CopyFrom(aTransportedFile.GetStream,0);
      finally
        aLocalConfigFileStream.Free;
      end;

      //И отпускаем исходный файл
      aTransportedFile:=nil;
    except
      exit; //!!!!
    end;

    result:=TrsFileLink.Create(aLocalFileName,true);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

//{$DEFINE LOCAL_PATH_TO_NETWORK_PATH}

function TRecordStorageDirectDB.CreateRecordObjectTransport(const aFileObject: IFileObject): IRecordObjectTransport;
begin
  Assert(FTransportFactory<>nil);
  result:=FTransportFactory.CreateTransport(aFileObject.GetFullFileNameWithIP);
end;

procedure TRecordStorageDirectDB.SortRecords(var aRecords: TrsRecordObjectInfoArray);
begin
  TArray.Sort<TrsRecordObjectInfo>(aRecords,TDelegatedComparer<TrsRecordObjectInfo>.Create
  (function(const Left, Right: TrsRecordObjectInfo): Integer
   var
     d: TDateTime;
   begin
     d:=Left.StartDateTime-Right.StartDateTime;
     if d<0 then
       result:=-1
     else if d>0 then
       result:=1
     else
       result:=0;
   end
  ));
end;

function TRecordStorageDirectDB.RecordSourceWriteableAccess: IRecordSourceWriteableAccess;
begin
  result:=self;
end;

function TRecordStorageDirectDB.RecordWriteableAccess: IRecordWriteableAccess;
begin
  result:=self;
end;

function TRecordStorageDirectDB.GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray;
const
  aMethodName = 'TRecordStorageDirectDB.GetEvents';
var
  aTraceID : cardinal;
  aEventList: IEventExtVideoList;
  i: integer;
  aFilter: ISFilter;
begin
  FLock.Enter;
  aTraceID:=TraceProcBegin(aMethodName);
  try
    Assert(aSourceId.IsGuidCompartible);

    aEventList:=FFileStorage.FileStorageExtVideo.GetEventsList;

    aFilter := aEventList.Filter;
    aFilter.CreateSubExpression.opAND.ValGUID(aEventList.Name_VideoCameraID, Equal, aSourceId.Guid).
      opAND.ValDateTime(aEventList.Name_StartDatetime, GreaterEqual, aFromTime). //Рассматриваем только StartDateTime, потому что не все события имеют EndDateTime
      opAND.ValDateTime(aEventList.Name_StartDatetime, LowEqual, aToTime);

    aFilter.Filtered := true;

    SetLength(result,aEventList.Count);
    for i:=0 to High(result) do
    begin
      result[i].DateTime:=aEventList.Items[i].StartDateTime;
      result[i].EndDateTime:=aEventList.Items[i].EndDateTime;
      result[i].EventType:=aEventList.Items[i].EventKind;
    end;

    aEventList:=nil;
    //SortRecords(result);
  finally
    FLock.Leave;
    TraceProcEnd(aMethodName+Format('. Найдено %d записей',[Length(result)]),aTraceID);
  end;
end;

function TRecordStorageDirectDB.GetConnectionString: string;
begin
  result:=FFileStorage.DataBaseName;
end;

{ TDirectDBRecordStorageConnectionThread }

constructor TDirectDBRecordStorageConnectionThread.Create( aOwner: TRecordStorageDirectDB);
begin
  FOwner:=aOwner;

  inherited Create(false);
end;

procedure TDirectDBRecordStorageConnectionThread.Execute;
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


{ TRecordStorageDirectDbConnector }

constructor TRecordStorageDirectDbConnector.CreateAsConnectionString(
  const aConnectionString: string);
begin
  FConnectionSource:=aConnectionString;
  FConnectionType:=ConnectionSourceType_ConnectionString;
end;

constructor TRecordStorageDirectDbConnector.CreateAsUdl(const aUdlFileName: string);
begin
  FConnectionSource:=aUdlFileName;

  if FConnectionSource='' then
    FConnectionSource:='LinkDB.udl';

  FConnectionType:=ConnectionSourceType_Udl;
end;


end.




