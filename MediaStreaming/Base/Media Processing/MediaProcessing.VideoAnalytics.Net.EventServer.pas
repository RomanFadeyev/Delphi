unit MediaProcessing.VideoAnalytics.Net.EventServer;

interface
uses
  Windows, Messages, SysUtils, Classes, SyncObjs,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPServer,IdContext,IdGlobal,IdIOHandler,IdThread, IdCustomTCPServer, IdStack,
  Generics.Collections, Collections.Map, MediaProcessing.VideoAnalytics.Net.EventDefinitions,
  MediaProcessing.Definitions,Collections.Lists, MediaProcessing.VideoAnalytics,IndyWrappers;

type
  TVaepNotifies = class (TThreadSafeObjectListWrapper<TVaepNotify>)
  public
    procedure Add(const Item: TVaepNotify);
  end;

  TVaepServerSession = class
  private
    FNotifies: TVaepNotifies;
  public
    constructor Create;
    destructor Destroy; override;

    property Notifies: TVaepNotifies read FNotifies;
  end;

  TVaepServer = class
  private
    FServer : TTcpServer;
    FSuperUserPassword: string;
    FSessions: TThreadSafeObjectListWrapper<TVaepServerSession>;

    procedure OnServerExecute(AContext:TIdContext);
  public
    constructor Create(aPort: Word; const aSuperUserPassword: string);
    destructor Destroy; override;

    procedure OnEvent(
      const ConnectionUrl: string;
      const aProcessingResult: TVaProcessingResult);

    function Version: cardinal;
    function Port: Word;
  end;

implementation
  uses IdSchedulerOfThreadDefault, dInterconnection,ThreadNames,CryptUtils, uBaseClasses,Patterns.Workspace;

{ TVaepServerContext }

{ TVaepServer }

constructor TVaepServer.Create(aPort: Word; const aSuperUserPassword: string);
begin
  FSuperUserPassword:=aSuperUserPassword;

  FSessions:=TThreadSafeObjectListWrapper<TVaepServerSession>.Create;
  FServer:=TTcpServer.Create(nil);
//  FClient.ConnectTimeout:=2000;
  Assert(aPort<>0);
  FServer.DefaultPort:=aPort;
  //FServer.Scheduler:=TIdSchedulerOfThreadDefault.Create(FServer);
  //TIdSchedulerOfThreadDefault(FServer.Scheduler).ThreadClass:=TNetStreamThread;
  //FServer.ContextClass:=TVaepServerContext;
  FServer.OnExecute:=OnServerExecute;

  FServer.Active:=true;
end;
//------------------------------------------------------------------------------
destructor TVaepServer.Destroy;
begin
  inherited;
  FServer.Free; //Не использовать FreeAndNil! Не тот порядок!
  FServer:=nil;

  try
    if FSessions<>nil then
      Assert(FSessions.Count=0);
  except
  end;

  FreeAndNil(FSessions);
end;
//------------------------------------------------------------------------------
procedure TVaepServer.OnEvent(const ConnectionUrl: string; const aProcessingResult: TVaProcessingResult);
var
  i: integer;
  aList: TList<TVaepServerSession>;
  aProcessingResult_: TVaProcessingResult;
begin
  aList:=FSessions.LockList;
  try
    if aList.Count>0 then
    begin
      //Копируем к себе, так как это будет переложено в отдельный поток
      aProcessingResult_.Objects:=Copy(aProcessingResult.Objects,0,Length(aProcessingResult.Objects));
      aProcessingResult_.Events:=Copy(aProcessingResult.Events,0,Length(aProcessingResult.Events));
      for i := 0 to aList.Count - 1 do
      begin
        aList[i].Notifies.Add(
          TVaepEventNotify.Create(ConnectionUrl,aProcessingResult_));
      end;
    end;
  finally
    FSessions.UnlockList;
  end;
end;
//------------------------------------------------------------------------------
procedure TVaepServer.OnServerExecute(AContext: TIdContext);
var
  IO: TIdIOHandler;
  aCount: integer;
  aData: TBytes;

  aLoginParams: TVaepLoginParams;
  aLoginResult: TVaepLoginResult;
  aLoginResultCode: TVaepLoginResultCode;
  aHash: string;

  aSession: TVaepServerSession;
  aNotify:  TVaepNotify;
begin
  SetCurrentThreadName(ClassName);

  IO:=AContext.Connection.IOHandler;
  aCount:=IO.ReadLongInt();

  aData:=nil;
  IO.ReadBytes(aData,aCount);
  aLoginResultCode:=iceOK;
  aLoginParams:=TVaepLoginParams.Create(aData);
  try
    if aLoginParams.ProtocolVersion<>VaepProtocolVerion then
      aLoginResultCode:=iceWrongProtocolVersion;

    if aLoginResultCode=iceOK then
      if aLoginParams.UserName<>VaepSuperUserName then
        aLoginResultCode:=iceWrongUserNameOrPassword;

    if aLoginResultCode=iceOK then
    begin
      aHash:=MD5Encode(Format('%s:%s',[aLoginParams.UserName,FSuperUserPassword]));
      if aHash<>aLoginParams.UserPasswordDigest then
        aLoginResultCode:=iceWrongUserNameOrPassword;
    end;
  finally
    aLoginParams.Free;
  end;


  aLoginResult:=TVaepLoginResult.Create(aLoginResultCode,TWorkspaceBase.Current.ApplicationGUID);
  aLoginResult.SaveToBytes(aData);
  IO.Write(Length(aData));
  IO.Write(aData);

  if aLoginResultCode<>iceOK then
    abort;

  aSession:=TVaepServerSession.Create;

  FSessions.Add(aSession);
  try
    while IO.Connected do
    begin
      while true do
      begin
        try
          aNotify:=aSession.Notifies.ExtractFirstOrDefault;
          if aNotify=nil then
            break;

          aNotify.SaveToBytes(aData);

          IO.Write(VaepFrameBeginMarker);
          IO.Write(integer(aNotify.NotifyType));
          IO.Write(integer(Length(aData)));
          IO.Write(aData);
          IO.Write(VaepFrameEndMarker);
        finally
          FreeAndNil(aNotify);
        end;
      end;

      sleep(10);
    end;
  finally
    FSessions.Remove(aSession);
  end;
end;
//------------------------------------------------------------------------------
function TVaepServer.Port: Word;
begin
  result:=FServer.DefaultPort;
end;

function TVaepServer.Version: cardinal;
begin
  result:=VaepProtocolVerion;
end;

{ TVaepServerSession }

constructor TVaepServerSession.Create;
begin
  FNotifies:=TVaepNotifies.Create;
end;

destructor TVaepServerSession.Destroy;
begin
  inherited;
  FreeAndNil(FNotifies);
end;

{ TVaepNotifies }

procedure TVaepNotifies.Add(const Item: TVaepNotify);
begin
  self.LockList;
  try
    if Count>VaepMaxQueue then
      self.Delete(0);
    inherited Add(Item);
  finally
    self.UnlockList;
  end;
end;

end.

