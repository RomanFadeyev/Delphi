{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Соединение, использующее для источника данных историю котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.EHC.StockDataConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Controls, Serialization, FC.Definitions,
       FC.StockData.StockDataSource,StockChart.Definitions,FC.Singletons,
       FC.StockData.StockDataConnectionFile;

type
  //---------------------------------------------------------------------------
  //Соединение с History Center

  TStockDataSourceConnection_EHC = class (TStockDataSourceConnectionFile)
  private
    FStartLoadingFrom: TDateTime;
  public
    //from IStockDataSourceConnection
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource; override;

    //TPersistentObject
    procedure OnDefineValues; override;

    constructor Create(const aSymbol: string; aInterval: TStockTimeInterval; const aStartLoadingFrom: TDateTime;const aConnectionString: string);
    destructor Destroy; override;
  end;



implementation
  uses SystemService,FC.StockData.EHC.StockDataSource,FC.StockData.StockDataSourceRegistry,
       FC.HistoryCenter.DataStorage ;

  { TStockDataSourceConnection_EHC }

constructor TStockDataSourceConnection_EHC.Create(const aSymbol: string; aInterval: TStockTimeInterval; const aStartLoadingFrom: TDateTime;const aConnectionString: string);
begin
  inherited Create(aSymbol,aInterval,aConnectionString);
  FStartLoadingFrom:=aStartLoadingFrom;
end;

function TStockDataSourceConnection_EHC.CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
var
  aObj: TObject;
  aStockDataStorage : IStockDataStorage;
begin
  aObj:=StockLoadedDataSourceRegistry.FindDataSourceObject(self.ConnectionString,TStockDataSource_EHC,self.Symbol,self.Interval,FloatToStr(FStartLoadingFrom));
  if aObj<>nil then
    result:=aObj as TStockDataSource_EHC
  else begin
    aStockDataStorage:=TStockDataStorageContainer.Create(Self.ConnectionString);
    aStockDataStorage.CheckConnected;
    result:=TStockDataSource_EHC.Create(self,self.Symbol,self.Interval,aStockDataStorage,FStartLoadingFrom);
  end;
end;

destructor TStockDataSourceConnection_EHC.Destroy;
begin
  inherited;
end;

procedure TStockDataSourceConnection_EHC.OnDefineValues;
begin
  inherited;
  DefValDateTime('StartLoadingFrom',FStartLoadingFrom);
end;

initialization
  Serialization.TClassFactory.RegisterClass(TStockDataSourceConnection_EHC);

end.
