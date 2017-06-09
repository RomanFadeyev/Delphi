{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Абстракция источника медиа-потока                             }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        14.01.2011                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaStream.DataSource.Base;

interface
  uses Windows,Classes,SysUtils,SyncObjs, MediaProcessing.Definitions,Generics.Collections,
       uBaseClasses,Collections.Map, MediaStream.PtzProtocol.Base;

type
  TMediaStreamDataSource = class;

  TMediaStreamDataSourceNotification = (dsnMotionAlarms,dsnSensor);
  TMediaStreamDataSourceNotificationSet = set of TMediaStreamDataSourceNotification;

  TMediaStreamDataSourceNotificationParams = record
    Notification:TMediaStreamDataSourceNotification;
    SourceId: cardinal;
  end;

  TMediaStreamDataSourceNotificationEvent = procedure (Sender: TMediaStreamDataSource; const aData: TMediaStreamDataSourceNotificationParams) of object;
  TMediaStreamDataSourceCloseEvent = procedure (Sender: TMediaStreamDataSource) of object;

  TMediaStreamDataSourceDestroyEventHandler = procedure (Sender: TMediaStreamDataSource) of object;
  TMediaStreamDataSourceDestroyEvent = TEventCast<TMediaStreamDataSourceDestroyEventHandler>;

  TMediaStreamDataSourceDataEventHandler = procedure (Sender: TMediaStreamDataSource; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal) of object;
  TMediaStreamDataSourceDataEvent = TEventCast<TMediaStreamDataSourceDataEventHandler>;



  TMediaStreamDataSourceConnectParams = class
  private
    FHandleNotifications: TMediaStreamDataSourceNotificationSet;
  protected
    procedure RaiseParseError(const aUrl: string);
  public
    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); virtual;

    function  ToUrl(aIncludeAuthorizationInfo: boolean): string; virtual; abstract;
    procedure Parse(const aUrl: string); virtual; abstract;

    property HandleNotifications: TMediaStreamDataSourceNotificationSet read FHandleNotifications write FHandleNotifications;
  end;

  TMediaStreamDataSourceArchivePointInfo = record
    Id: string;
    Text: string;
  end;

  TMediaStreamFrameInfo = record
    DateTime: TDateTime;
    Size: cardinal;
    Format: TMediaStreamDataHeader;
  end;
  TMediaStreamFrameInfoArray = array [TMediaType] of TMediaStreamFrameInfo;

  TMediaStreamDataSourceObjectState = set of (osDestroying);

  TMediaStreamChannelInfo = record
    Data: string;
  end;

  TMediaStreamDataSource = class
  private
    FOnClose: TMediaStreamDataSourceCloseEvent;
    FOnData: TMediaStreamDataSourceDataEvent;
    FOnNotification: TMediaStreamDataSourceNotificationEvent;
    FConnectionString: string;
    FConnected: boolean;
    FConnectionDateTime: TDateTime;
    FOnDestroy: TMediaStreamDataSourceDestroyEvent;
    FObjectState: TMediaStreamDataSourceObjectState;
    FForcedVideoFrameIntervalMs: integer;
    FPtz: TPtzProtocol;
    FPtzOwned: boolean;

    function GetStreamType(aMediaType: TMediaType): TStreamType;
    function GetLastStreamFrames: TMediaStreamFrameInfoArray; //TODO переделать на виртуальный абстрактный вызов?
  protected
    FLastStreamFrames: TMediaStreamFrameInfoArray;
    FLastStreamFramesLock: TCriticalSection;

    procedure RaiseOnClose;
    procedure RaiseOnData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
    procedure RaiseOnNotification(const aNotification: TMediaStreamDataSourceNotificationParams);

    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); virtual; abstract;

    procedure DoDisconnect; virtual; abstract;
    procedure TraceLine(const aMessage: string);

    procedure SetPtz(aPtz: TPtzProtocol; aOwned: boolean=true);
    property  DirectPtz: TPtzProtocol read FPtz;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure BeginTransaction; virtual;
    procedure EndTransaction; virtual;

    procedure Connect(aConnectParams: TMediaStreamDataSourceConnectParams);
    procedure Disconnect;
    function  GetConnectionErrorDescription(aError: Exception): string; virtual;

    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    function  ConnectionDateTime: TDateTime;
    function  LastStreamDataTime: TDateTime; virtual; abstract;
    property  LastStreamFrames:TMediaStreamFrameInfoArray read GetLastStreamFrames;

    function  StreamLength: integer; virtual; //msec, -1 = INFINITE


    property  StreamType[aMediaType: TMediaType]: TStreamType read GetStreamType;

    function  GetChannelInfos: TArray<TMediaStreamChannelInfo>; virtual;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; virtual; abstract;

    function  PtzSupported: boolean; virtual;
    procedure PtzInit; virtual;
    function  GetPtz: TPtzProtocol;
    procedure SetCustomPtz(aPtz: TPtzProtocol);

    //Движение
    procedure PtzMoveUp(aDuration: cardinal; aSpeed: byte); deprecated 'Use GetPtz instead';
    procedure PtzMoveUpStop; deprecated 'Use GetPtz instead';
    procedure PtzMoveDown(aDuration: cardinal; aSpeed: byte); deprecated 'Use GetPtz instead';
    procedure PtzMoveDownStop; deprecated 'Use GetPtz instead';
    procedure PtzMoveLeft(aDuration: cardinal; aSpeed: byte); deprecated 'Use GetPtz instead';
    procedure PtzMoveLeftStop; deprecated 'Use GetPtz instead';
    procedure PtzMoveRight(aDuration: cardinal; aSpeed: byte); deprecated 'Use GetPtz instead';
    procedure PtzMoveRightStop; deprecated 'Use GetPtz instead';

    //===== Перемещение на заданную позицию
    //Движение на указанную точку-пресет
    procedure PtzMoveToPoint(aId: cardinal); deprecated 'Use GetPtz instead';
    //Движение в указанную позицию. Позиция указывается по оси X и Y в градусах
    procedure PtzMoveToPosition(const aPositionPan,aPositionTilt: double); deprecated 'Use GetPtz instead';

    //Масштаб
    procedure PtzZoomIn(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzZoomInStop; deprecated 'Use GetPtz instead';
    procedure PtzZoomOut(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzZoomOutStop; deprecated 'Use GetPtz instead';
    //Фокус
    procedure PtzFocusIn(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzFocusInStop; deprecated 'Use GetPtz instead';
    procedure PtzFocusOut(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzFocusOutStop; deprecated 'Use GetPtz instead';
    //Апертура (способность собирать свет и противостоять дифракционному размытию деталей изображения)
    procedure PtzApertureIncrease(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzApertureIncreaseStop; deprecated 'Use GetPtz instead';
    procedure PtzApertureDecrease(aDuration: cardinal); deprecated 'Use GetPtz instead';
    procedure PtzApertureDecreaseStop; deprecated 'Use GetPtz instead';

    function ArchiveSupported: boolean; virtual;
    function ArchivePoints: TArray<TMediaStreamDataSourceArchivePointInfo>; virtual;

    property Connected: boolean read FConnected;
    property ConnectionString: string read FConnectionString;
    //Проверяет по возможности реальное наличие соединения. Может поддерживаться не всеми источниками
    function CheckConnected:boolean; virtual;

    //=0 - использовать какой есть >0 - принудительно замедлять кадры
    property ForcedVideoFrameIntervalMs: integer read FForcedVideoFrameIntervalMs write FForcedVideoFrameIntervalMs;

    property ObjectState: TMediaStreamDataSourceObjectState read FObjectState;

    property OnData: TMediaStreamDataSourceDataEvent read FOnData;

    property OnNotification: TMediaStreamDataSourceNotificationEvent read FOnNotification write FOnNotification;
    property OnClose: TMediaStreamDataSourceCloseEvent read FOnClose write FOnClose;
    property OnDestroy: TMediaStreamDataSourceDestroyEvent  read FOnDestroy;
  end;

  TMediaStreamDataSourceClass  = class of TMediaStreamDataSource;


  TMediaStreamDataSourceRegistryItem = TObjectRegistryItem<TMediaStreamDataSource>;
  TMediaStreamDataSourceRegistry = class (TObjectRegistry<TMediaStreamDataSource>);

  TMediaStreamDataSourceFactory = class
  private
    FUrlMap: TTextKeyMap<TMediaStreamDataSourceClass>;
  public
    procedure Register(const aUrl: string; aClass: TMediaStreamDataSourceClass);
    function CreateFromUrl(const aUrl: string): TMediaStreamDataSource;
    function GetClassFromUrl(const aUrl: string): TMediaStreamDataSourceClass;

    constructor Create;
    destructor Destroy; override;
  end;

function MediaStreamDataSourceRegistry: TMediaStreamDataSourceRegistry;
function MediaStreamDataSourceFactory: TMediaStreamDataSourceFactory;

implementation
  uses uTrace, Patterns.Workspace;

var
  gMediaStreamDataSourceRegistry:TMediaStreamDataSourceRegistry;
  gMediaStreamDataSourceFactory: TMediaStreamDataSourceFactory;

function MediaStreamDataSourceRegistry: TMediaStreamDataSourceRegistry;
begin
  Assert(gMediaStreamDataSourceRegistry<>nil);
  result:=gMediaStreamDataSourceRegistry;
end;

function MediaStreamDataSourceFactory: TMediaStreamDataSourceFactory;
begin
  Assert(gMediaStreamDataSourceFactory<>nil);
  result:=gMediaStreamDataSourceFactory;
end;

{ TMediaStreamDataSource }

function TMediaStreamDataSource.ArchivePoints: TArray<TMediaStreamDataSourceArchivePointInfo>;
begin
  result:=nil;
end;

function TMediaStreamDataSource.ArchiveSupported: boolean;
begin
  result:=false;
end;

procedure TMediaStreamDataSource.BeforeDestruction;
begin
  Include(FObjectState, osDestroying);
  inherited;
end;

procedure TMediaStreamDataSource.BeginTransaction;
begin

end;

function TMediaStreamDataSource.CheckConnected:boolean;
begin
  result:=self.Connected;
end;

procedure TMediaStreamDataSource.Connect(aConnectParams: TMediaStreamDataSourceConnectParams);
begin
  Disconnect;
  FConnectionString:=aConnectParams.ToString;
  DoConnect(aConnectParams);
  FConnectionDateTime:=Now;
  FConnected:=true;
end;

function TMediaStreamDataSource.ConnectionDateTime: TDateTime;
begin
  if not Connected then
    result:=0
  else
    result:=FConnectionDateTime;
end;

constructor TMediaStreamDataSource.Create;
var
  i: TMediaType;
begin
  FOnData:=TMediaStreamDataSourceDataEvent.Create;
  FOnDestroy:=TMediaStreamDataSourceDestroyEvent.Create;

  FLastStreamFramesLock:=TCriticalSection.Create;

  for i := Low(TMediaType) to High(TMediaType) do
    FLastStreamFrames[i].Format.biStreamType:=stUNIV;

  MediaStreamDataSourceRegistry.Register(self);
  RegisterCustomTrace(ClassName,'','.'+StringReplace(Copy(self.ClassName,2,High(Word)),'_','.',[rfReplaceAll]));
end;

destructor TMediaStreamDataSource.Destroy;
var
  aHandler: TMediaStreamDataSourceDestroyEventHandler;
begin
  Disconnect;

  for aHandler in FOnDestroy do
    aHandler(self);
  MediaStreamDataSourceRegistry.Unregister(self);
  inherited;

  SetPtz(FPtz);
  FreeAndNil(FOnData);
  FreeAndNil(FOnDestroy);
  FreeAndNil(FLastStreamFramesLock);
end;

procedure TMediaStreamDataSource.Disconnect;
begin
  DoDisconnect;
  FConnectionString:='';
  FConnected:=false;
end;

procedure TMediaStreamDataSource.EndTransaction;
begin

end;

function TMediaStreamDataSource.GetChannelInfos: TArray<TMediaStreamChannelInfo>;
begin

end;

function TMediaStreamDataSource.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';
end;

function TMediaStreamDataSource.GetLastStreamFrames: TMediaStreamFrameInfoArray;
begin
  FLastStreamFramesLock.Enter;
  try
   result:=FLastStreamFrames;
  finally
    FLastStreamFramesLock.Leave;
  end;
end;

function TMediaStreamDataSource.GetPtz: TPtzProtocol;
begin
  if not PtzSupported then
    raise Exception.Create('Управление PTZ не поддерживается');

  PtzInit;

  if FPtz=nil then
    raise Exception.Create('Управление PTZ не поддерживается');

  result:=FPtz;
end;

function TMediaStreamDataSource.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  result:=FLastStreamFrames[aMediaType].Format.biStreamType;
end;

procedure TMediaStreamDataSource.PtzApertureDecrease;
begin

end;

procedure TMediaStreamDataSource.PtzApertureDecreaseStop;
begin

end;

procedure TMediaStreamDataSource.PtzApertureIncrease;
begin

end;

procedure TMediaStreamDataSource.PtzApertureIncreaseStop;
begin

end;

procedure TMediaStreamDataSource.PtzFocusIn;
begin

end;

procedure TMediaStreamDataSource.PtzFocusInStop;
begin

end;

procedure TMediaStreamDataSource.PtzFocusOut;
begin

end;

procedure TMediaStreamDataSource.PtzFocusOutStop;
begin

end;

procedure TMediaStreamDataSource.PtzInit;
begin

end;

procedure TMediaStreamDataSource.PtzMoveDown(aDuration: cardinal;aSpeed: byte);
begin
  GetPtz.PtzMoveDown(aDuration,aSpeed);
end;

procedure TMediaStreamDataSource.PtzMoveDownStop;
begin
  GetPtz.PtzMoveDownStop;
end;

procedure TMediaStreamDataSource.PtzMoveLeft(aDuration: cardinal; aSpeed: byte);
begin
  GetPtz.PtzMoveLeft(aDuration,aSpeed);
end;

procedure TMediaStreamDataSource.PtzMoveLeftStop;
begin
  GetPtz.PtzMoveLeftStop;
end;

procedure TMediaStreamDataSource.PtzMoveRight(aDuration: cardinal; aSpeed: byte);
begin
  GetPtz.PtzMoveRight(aDuration,aSpeed);
end;

procedure TMediaStreamDataSource.PtzMoveRightStop;
begin
  GetPtz.PtzMoveRightStop;
end;

procedure TMediaStreamDataSource.PtzMoveToPoint(aId: cardinal);
begin
  GetPtz.PtzMoveToPoint(aId);
end;

procedure TMediaStreamDataSource.PtzMoveToPosition(const aPositionPan, aPositionTilt: double);
begin
  GetPtz.PtzMoveToPosition(aPositionPan,aPositionTilt);
end;

procedure TMediaStreamDataSource.PtzMoveUp(aDuration: cardinal; aSpeed: byte);
begin
  GetPtz.PtzMoveUp(aDuration,aSpeed);
end;

procedure TMediaStreamDataSource.PtzMoveUpStop;
begin
  GetPtz.PtzMoveUpStop;
end;

function TMediaStreamDataSource.PtzSupported: boolean;
begin
  result:=FPtz<>nil;
end;

procedure TMediaStreamDataSource.PtzZoomIn(aDuration: cardinal);
begin
  GetPtz.PtzZoomIn(aDuration);
end;

procedure TMediaStreamDataSource.PtzZoomInStop;
begin
  GetPtz.PtzZoomInStop;
end;

procedure TMediaStreamDataSource.PtzZoomOut(aDuration: cardinal);
begin
  GetPtz.PtzZoomOut(aDuration);
end;

procedure TMediaStreamDataSource.PtzZoomOutStop;
begin
  GetPtz.PtzZoomOutStop;
end;

procedure TMediaStreamDataSource.RaiseOnClose;
begin
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure TMediaStreamDataSource.RaiseOnData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
const
  aMethodName='TMediaStreamDataSource.RaiseOnData';
var
  aHandler: TMediaStreamDataSourceDataEventHandler;
begin
  if (aFormat.biMediaType=mtVideo) and (FForcedVideoFrameIntervalMs>0) then
    Sleep(FForcedVideoFrameIntervalMs);

  FLastStreamFramesLock.Enter;
  try
    FLastStreamFrames[aFormat.biMediaType].Format:=aFormat;
    FLastStreamFrames[aFormat.biMediaType].DateTime:=Now;
    FLastStreamFrames[aFormat.biMediaType].Format:=aFormat;
  finally
    FLastStreamFramesLock.Leave;
  end;

  for aHandler in FOnData do
    try
      aHandler(self,aFormat, aData,aDataSize,aInfo,aInfoSize);
    except
      on E:Exception do
      begin
        TraceLine(Format('Ошибка при обработке OnDataReceived: %s',[E.Message]));
        TWorkspaceBase.Current.HandleException(self,E,aMethodName);
      end;
    end;
end;

procedure TMediaStreamDataSource.RaiseOnNotification(const aNotification: TMediaStreamDataSourceNotificationParams);
begin
  if Assigned(FOnNotification) then
    FOnNotification(self,aNotification);
end;

procedure TMediaStreamDataSource.SetCustomPtz(aPtz: TPtzProtocol);
begin
  SetPtz(aPtz,false);
end;

procedure TMediaStreamDataSource.SetPtz(aPtz: TPtzProtocol; aOwned: boolean=true);
begin
  if FPtzOwned then
    FreeAndNil(FPtz);

  FPtz:=aPtz;
  FPtzOwned:=aOwned;
end;

function TMediaStreamDataSource.StreamLength: integer;
begin
  result:=-1;
end;

procedure TMediaStreamDataSource.TraceLine(const aMessage: string);
begin
  uTrace.TraceLine(self.ClassName,ConnectionString+': '+aMessage);
end;

{ TMediaStreamDataSourceConnectParams }

procedure TMediaStreamDataSourceConnectParams.Assign(
  aSource: TMediaStreamDataSourceConnectParams);
begin
  FHandleNotifications:=aSource.FHandleNotifications;
end;

procedure TMediaStreamDataSourceConnectParams.RaiseParseError(const aUrl: string);
begin
  raise Exception.CreateFmt('Не удалось разобрать строку подключения %s',[aUrl]);
end;

{ TMediaStreamDataSourceFactory }

constructor TMediaStreamDataSourceFactory.Create;
begin
  FUrlMap:=TTextKeyMap<TMediaStreamDataSourceClass>.Create;
end;

function TMediaStreamDataSourceFactory.CreateFromUrl(
  const aUrl: string): TMediaStreamDataSource;
begin
  result:=GetClassFromUrl(aUrl).Create;
end;

destructor TMediaStreamDataSourceFactory.Destroy;
begin
  FreeAndNil(FUrlMap);
  inherited;
end;

function TMediaStreamDataSourceFactory.GetClassFromUrl(
  const aUrl: string): TMediaStreamDataSourceClass;
var
  aPrefix: string;
  i: integer;
  aClass: TMediaStreamDataSourceClass;
begin
  i:=Pos(':',aUrl);
  if i=0 then
    raise Exception.Create('Указанная строка подключения имеет неверный формат');

  aPrefix:=Copy(aUrl,1,i-1);
  if FUrlMap.Lookup(aPrefix,aClass) then
    result:=aClass
  else
    raise Exception.Create('Для указанной строки подключения не существует источника');
end;

procedure TMediaStreamDataSourceFactory.Register(const aUrl: string;
  aClass: TMediaStreamDataSourceClass);
begin
  FUrlMap.Add(aUrl,aClass);
end;

initialization
  gMediaStreamDataSourceRegistry:=TMediaStreamDataSourceRegistry.Create;
  gMediaStreamDataSourceFactory:=TMediaStreamDataSourceFactory.Create;

finalization
  try
    Assert(gMediaStreamDataSourceRegistry.Count=0);
    FreeAndNil(gMediaStreamDataSourceRegistry);
  except
  end;

  try
    FreeAndNil(gMediaStreamDataSourceFactory);
  except
  end;

end.


