{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Соединение, использующее для источника данных историю котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.HC.StockDataConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Controls, Serialization, FC.Definitions,
       FC.StockData.StockDataSource,StockChart.Definitions,FC.Singletons;

type
  //---------------------------------------------------------------------------
  //Соединение с History Center

  TStockDataSourceConnection_HistoryCenter = class (TPersistentObjectRefCounted,IStockDataSourceConnection)
  private
    FSymbol: string;
    FInterval: TStockTimeInterval;
    FStartLoadingFrom: TDateTime;
  public
    //from IStockDataSourceConnection
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
    function ConnectionString: string;

    //TPersistentObject
    procedure ReadData(const aReader: IDataReader);  override;
    procedure WriteData(const aWriter: IDataWriter); override;

    constructor Create(const aSymbol: string; aInterval: TStockTimeInterval; const aStartLoadingFrom: TDateTime);
  end;



implementation
  uses SystemService,FC.StockData.HC.StockDataSource,FC.StockData.StockDataSourceRegistry;

  { TStockDataSourceConnection_HistoryCenter }

function TStockDataSourceConnection_HistoryCenter.ConnectionString: string;
begin
  result:='HistoryCenter: '+FSymbol+IntToStr(StockTimeIntervalValues[FInterval]);
end;

constructor TStockDataSourceConnection_HistoryCenter.Create(const aSymbol: string; aInterval: TStockTimeInterval; const aStartLoadingFrom: TDateTime);
begin
  FSymbol:=aSymbol;
  FInterval:=aInterval;
  FStartLoadingFrom:=aStartLoadingFrom;
end;

function TStockDataSourceConnection_HistoryCenter.CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
var
  aObj: TObject;
begin
  result:=nil;
  if aUseCacheIfPossible then
  begin
    aObj:=StockLoadedDataSourceRegistry.FindDataSourceObject(self.ConnectionString,TStockDataSource_HistoryCenter,FSymbol,FInterval,FloatToStr(FStartLoadingFrom));
    if aObj<>nil then
      result:=aObj as TStockDataSource_HistoryCenter
  end;

  if result=nil then
    result:=TStockDataSource_HistoryCenter.Create(self,FSymbol,FInterval,StockDataStorage,FStartLoadingFrom);
end;

procedure TStockDataSourceConnection_HistoryCenter.ReadData(const aReader: IDataReader);
begin
  inherited;
  aReader.ReadString(FSymbol);
  aReader.Read(FInterval,sizeof(FInterval));
  aReader.ReadDateTime(FStartLoadingFrom);
end;

procedure TStockDataSourceConnection_HistoryCenter.WriteData(const aWriter: IDataWriter);
begin
  inherited;
  aWriter.WriteString(FSymbol);
  aWriter.Write(FInterval,sizeof(FInterval));
  aWriter.WriteDateTime(FStartLoadingFrom);
end;


initialization
  Serialization.TClassFactory.RegisterClass(TStockDataSourceConnection_HistoryCenter);

end.


