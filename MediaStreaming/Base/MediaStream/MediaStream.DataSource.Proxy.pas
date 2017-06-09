unit MediaStream.DataSource.Proxy;

interface
  uses Windows,Classes,SysUtils, SyncObjs, MediaStream.DataSource.Base, MediaProcessing.Definitions;

type
  TMediaStreamDataSource_Proxy = class (TMediaStreamDataSource)
  private
    FDataLock: TCriticalSection;
    FChannel : TMediaStreamDataSource;
  protected
    procedure DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams); override;
    procedure DoDisconnect; override;

    procedure OnDataSourceData(aSender: TMediaStreamDataSource; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateConnectParams: TMediaStreamDataSourceConnectParams; override;

    function  GetConnectionErrorDescription(aError: Exception): string; override;
    procedure Start; override;
    procedure Stop; override;
    function  LastStreamDataTime: TDateTime; override;
  end;


  TMediaStreamDataSourceConnectParams_Proxy = class (TMediaStreamDataSourceConnectParams)
  private
    FDataSource: TMediaStreamDataSource;
    FConnectionString: string;
  public
    constructor Create; overload;
    constructor Create(aDataSource: TMediaStreamDataSource); overload;

    procedure Assign(aSource: TMediaStreamDataSourceConnectParams); override;
    function ToString: string; override;
    function ToUrl(aIncludeAuthorizationInfo: boolean): string; override;
    procedure Parse(const aUrl: string); override;
  end;


implementation
  uses uBaseClasses;


{ TMediaStreamDataSourceConnectParams_Proxy }

procedure TMediaStreamDataSourceConnectParams_Proxy.Assign(aSource: TMediaStreamDataSourceConnectParams);
begin
  TArgumentValidation.NotNil(aSource);

  if not (aSource is TMediaStreamDataSourceConnectParams_Proxy) then
    raise EInvalidArgument.CreateFmt('Тип параметров %s не совместим с типом %s',[aSource.ClassName,self.ClassName]);

  FDataSource:=TMediaStreamDataSourceConnectParams_Proxy(aSource).FDataSource;
  FConnectionString:=TMediaStreamDataSourceConnectParams_Proxy(aSource).FConnectionString;
end;

constructor TMediaStreamDataSourceConnectParams_Proxy.Create;
begin

end;

constructor TMediaStreamDataSourceConnectParams_Proxy.Create(
  aDataSource: TMediaStreamDataSource);
begin
  Create;
  FDataSource:=aDataSource;
  if FDataSource<>nil then
    FConnectionString:=FDataSource.ConnectionString;
end;

procedure TMediaStreamDataSourceConnectParams_Proxy.Parse(const aUrl: string);
begin
  FConnectionString:=aUrl;
end;

function TMediaStreamDataSourceConnectParams_Proxy.ToString: string;
begin
  result:=FConnectionString;
end;

function TMediaStreamDataSourceConnectParams_Proxy.ToUrl(
  aIncludeAuthorizationInfo: boolean): string;
begin
  result:=FConnectionString;
end;

{ TMediaStreamDataSource_Proxy }

procedure TMediaStreamDataSource_Proxy.Start;
begin
  FDataLock.Enter;
  try
    if FChannel<>nil then
      FChannel.OnData.Add(OnDataSourceData);
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Proxy.Stop;
begin
  FDataLock.Enter;
  try
    if FChannel<>nil then
      FChannel.OnData.Remove(OnDataSourceData);
  finally
    FDataLock.Leave;
  end;
end;

procedure TMediaStreamDataSource_Proxy.DoConnect(aConnectParams: TMediaStreamDataSourceConnectParams);
begin
  FChannel:=nil;
  FChannel:=(aConnectParams as TMediaStreamDataSourceConnectParams_Proxy).FDataSource;
end;

procedure TMediaStreamDataSource_Proxy.DoDisconnect;
begin
  if FChannel<>nil then
    FChannel.OnData.Remove(OnDataSourceData);
  FChannel:=nil;
end;

constructor TMediaStreamDataSource_Proxy.Create;
begin
  inherited;
  FDataLock:=TCriticalSection.Create;
end;

class function TMediaStreamDataSource_Proxy.CreateConnectParams: TMediaStreamDataSourceConnectParams;
begin
  result:=TMediaStreamDataSourceConnectParams_Proxy.Create;
end;

destructor TMediaStreamDataSource_Proxy.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FDataLock);
end;

function TMediaStreamDataSource_Proxy.GetConnectionErrorDescription(aError: Exception): string;
begin
  result:='';
end;

function TMediaStreamDataSource_Proxy.LastStreamDataTime: TDateTime;
begin
  if FChannel<>nil then
    result:=Now
  else
    result:=0;
end;


procedure TMediaStreamDataSource_Proxy.OnDataSourceData(aSender: TMediaStreamDataSource; const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  FDataLock.Enter;
  try
    RaiseOnData(aFormat,aData,aDataSize,aInfo,aInfoSize);
  finally
    FDataLock.Leave;
  end;
end;

end.

