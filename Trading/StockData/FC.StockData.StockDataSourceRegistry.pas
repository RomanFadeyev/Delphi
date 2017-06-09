unit FC.StockData.StockDataSourceRegistry;

interface
uses Classes,SysUtils, FC.Definitions, FC.StockData.StockDataSource;

type
  TStockLoadedDataSourceRegistry = class
  private
    FDataSources: TList;
    function GetDataSource(index: integer): TStockDataSource_B;
  public
    function  DataSourceCount: integer;
    property  DataSources[index: integer]: TStockDataSource_B read  GetDataSource;

    procedure AddDataSource(aDataSource: TStockDataSource_B);
    procedure RemoveDataSource(aDataSource: TStockDataSource_B);

    function  FindDataSourceObject(const ConnectionString: string; aClass:TClass; const aSymbol: string; aInterval: TStockTimeInterval; const aTag: string): TStockDataSource_B;

    constructor Create;
    destructor Destroy; override;
  end;

  function StockLoadedDataSourceRegistry: TStockLoadedDataSourceRegistry;

implementation

var
  gSLDRegistry: TStockLoadedDataSourceRegistry;

function StockLoadedDataSourceRegistry: TStockLoadedDataSourceRegistry;
begin
  if gSLDRegistry = nil then
    gSLDRegistry:=TStockLoadedDataSourceRegistry.Create;
  result:=gSLDRegistry;
end;

{ TStockLoadedDataSourceRegistry }

constructor TStockLoadedDataSourceRegistry.Create;
begin
  FDataSources:=TList.Create;
end;

procedure TStockLoadedDataSourceRegistry.AddDataSource(aDataSource: TStockDataSource_B);
begin
  FDataSources.Add(aDataSource)
end;

function TStockLoadedDataSourceRegistry.DataSourceCount: integer;
begin
  result:=FDataSources.Count;
end;

function TStockLoadedDataSourceRegistry.GetDataSource(index: integer): TStockDataSource_B;
begin
  result:=FDataSources[index];
end;

destructor TStockLoadedDataSourceRegistry.Destroy;
begin
  FreeAndNil(FDataSources);
  inherited;
end;

procedure TStockLoadedDataSourceRegistry.RemoveDataSource(aDataSource: TStockDataSource_B);
begin
  FDataSources.Remove(aDataSource);
end;

function TStockLoadedDataSourceRegistry.FindDataSourceObject(const ConnectionString: string; aClass:TClass; const aSymbol: string; aInterval: TStockTimeInterval; const aTag: string): TStockDataSource_B;
var
  i: integer;
  aDS: TStockDataSource_B;
begin
  result:=nil;

  for i:=0 to DataSourceCount-1 do
  begin
    aDS:=GetDataSource(i);
    if (TObject(FDataSources[i]) is aClass) then
      if (aDS.Connection<>nil) and (aDS.Connection.ConnectionString=ConnectionString) and (aDS.Tag=aTag) then
        if aDS.StockSymbol.IsEqual(aSymbol,aInterval) then
        begin
          result:=FDataSources[i];
          break;
        end;
  end;
end;

initialization

finalization
  FreeAndNil(gSLDRegistry);

end.
