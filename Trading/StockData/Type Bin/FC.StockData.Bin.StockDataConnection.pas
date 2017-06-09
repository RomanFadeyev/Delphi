{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSourceConnection для файлов формата Bin

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.Bin.StockDataConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Serialization, FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile;

type
  //Bin File
  TStockDataSourceConnection_BinFile = class (TStockDataSourceConnectionFile)
  public
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource; override;
  end;

implementation
  uses FC.StockData.Bin.StockDataSource;

{ TStockDataSourceConnection_BinFile }

function TStockDataSourceConnection_BinFile.CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
var
  aStream: TFileStream;
  aDS : TStockDataSource_B;
begin
  aStream:=TFileStreamEx.CreateForRead(ConnectionString);
  try
    aDS:=TStockDataSource_Bin.Create(self,Symbol,Interval, aStream);
    result:=aDS;
  finally
    aStream.Free;
  end;
end;

initialization
  Serialization.TClassFactory.RegisterClass(TStockDataSourceConnection_BinFile);

end.
