unit MediaStream.DataSource.Stub;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base, MediaProcessing.Definitions;

type
  TStubChannel = class;


  TMediaStreamDataSource_Stub = class (TMediaStreamDataSource)
  private
    FDataLock: TCriticalSection;
    FChannel : TStubChannel;
    FFormat: TMediaStreamDataHeader;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure OnDataReceived(aSender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
  end;


  TMediaStreamDataSourceConnectParams_Stub = class (TMediaStreamDataSourceConnectParams)
  private
    FFormat: TMediaStreamDataHeader;
    FInterval : integer;
  public
    constructor Create; overload;
    constructor Create(const aFormat: TMediaStreamDataHeader; aInterval: integer); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;

    function ToString: string; override;
    function ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
  end;

  TStubChannel = class (TThread)
  private
    FOnData: TNotifyEvent;
    FInterval: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(aInterval: integer);
    property OnData: TNotifyEvent read FOnData write FOnData;
  end;

implementation
  uses uBaseClasses;


{ TMediaStreamDataSourceConnectParams_Stub }

procedure TMediaStreamDataSourceConnectParams_Stub.Assign(aSource: TMediaStreamDataSourceConnectParams);
var
  aSrc: TMediaStreamDataSourceConnectParams_Stub;
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_Stub) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  aSrc:=TMediaStreamDataSourceConnectParams_Stub(aSource);
  Self.FFormat:=aSrc.FFormat;
end;

constructor TMediaStreamDataSourceConnectParams_Stub.Create;
begin
  FFormat.Clear;
  FFormat.biMediaType:=mtVideo;
  FFormat.biStreamType:=0;
  FFormat.VideoWidth:=0;
  FFormat.VideoHeight:=0;
  FInterval:=100;
end;

constructor TMediaStreamDataSourceConnectParams_Stub.Create(const aFormat: TMediaStreamDataHeader;aInterval: integer);
begin
  FFormat:=aFormat;
  FInterval:=aInterval;
end;

procedure TMediaStreamDataSourceConnectParams_Stub.Parse(const aUrl: string);
begin
  inherited;

end;

function TMediaStreamDataSourceConnectParams_Stub.ToString: string;
begin
  result:='';
end;

function TMediaStreamDataSourceConnectParams_Stub.ToUrl(
  aIncludeAuthorizationInfo: boolean): string;
begin

end;

{ TMediaStreamDataSource_Stub }

procedure TMediaStreamDataSource_Stub.Start;
begin
  FDataLock.Enter;
  try
    FChannel.OnData:=OnDataReceived;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Stub.Stop;
begin
  FDataLock.Enter;
  try
    if FChannel<>nil then
      FChannel.OnData:=nil;
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Stub.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
begin
  FreeAndNil(FChannel);
  FFormat:=(aConnectParams as TMediaStreamDataSourceConnectParams_Stub).FFormat;
  FChannel:=TStubChannel.Create((aConnectParams as TMediaStreamDataSourceConnectParams_Stub).FInterval);
end;

procedure TMediaStreamDataSource_Stub.DoDisconnect;
begin
  FreeAndNil(FChannel);
end;

constructor TMediaStreamDataSource_Stub.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
end;

class function TMediaStreamDataSource_Stub.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_Stub.Create;
end;

destructor TMediaStreamDataSource_Stub.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FDataLock);
end;

function TMediaStreamDataSource_Stub.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';
end;

function TMediaStreamDataSource_Stub.LastStreamDataTime: TDateTime;
begin
  if FChannel<>nil then
    result:=Now
  else
    result:=0;
end;


procedure TMediaStreamDataSource_Stub.OnDataReceived(aSender: TObject);
begin
  FDataLock.Enter;
  try
    if (FChannel=nil) or not Assigned(FChannel.OnData) then
      exit;

    FFormat.TimeStamp:=Trunc(Now*MSecsPerDay);
    RaiseOnData(FFormat,nil,0,nil,0);
  finally
    FDataLock.Leave;
  end;
end;

{ TStubChannel }

constructor TStubChannel.Create(aInterval: integer);
begin
  FInterval:=aInterval;
  inherited Create(false);
end;

procedure TStubChannel.Execute;
begin
  while not Terminated do
  begin
    Sleep(FInterval);
    if Assigned(FOnData) then
      FOnData(self);
  end;
end;

end.

