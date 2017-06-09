{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSourceConnection для файлов формата CSV

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.CSV.StockDataConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Serialization, FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile;

type
  //CSV File
  TStockDataSourceConnection_CSVFile = class (TStockDataSourceConnectionFile)
  public
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource; override;
  end;

implementation
  uses FC.StockData.CSV.StockDataSource;

{ TStockDataSourceConnection_CSVFile }

function TStockDataSourceConnection_CSVFile.CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
var
  aStream: TFileStream;
  aDS : TStockDataSource_B;
begin
  aStream:=TFileStreamEx.CreateForRead(ConnectionString);
  try
    aDS:=TStockDataSource_CSV.Create(self,Symbol,Interval, aStream,0,1,2,3,4,5,6);
    result:=aDS;
  finally
    aStream.Free;
  end;
end;

initialization
  Serialization.TClassFactory.RegisterClass(TStockDataSourceConnection_CSVFile);

end.
