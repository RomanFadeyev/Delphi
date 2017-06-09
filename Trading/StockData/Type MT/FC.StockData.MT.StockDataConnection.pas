{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSourceConnection для файлов формата MetaTrader 4

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.MT.StockDataConnection;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Serialization, FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile;

type
  //MT$ File
  TStockDataSourceConnection_MTFile = class (TStockDataSourceConnectionFile)
  private
    FStartLoadingFrom: TDateTime;
  public
    constructor Create(const aSymbol: string; aInterval: TStockTimeInterval; aConnectionString: string; const aStartLoadingFrom: TDateTime=0);

    procedure OnDefineValues; override;
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource; override;
  end;

implementation
  uses FC.StockData.MT.StockDataSource;

{ TStockDataSourceConnection_MTFile }

constructor TStockDataSourceConnection_MTFile.Create(const aSymbol: string; aInterval: TStockTimeInterval; aConnectionString: string; const aStartLoadingFrom: TDateTime=0);
begin
  inherited Create(aSymbol,aInterval,aConnectionString);
  FStartLoadingFrom:=aStartLoadingFrom;
end;

function TStockDataSourceConnection_MTFile.CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource;
var
  aStream: TFileStream;
  aDS : TStockDataSource_B;
begin
  aStream:=TFileStreamEx.CreateForRead(ConnectionString);
  try
    aDS:=TStockDataSource_MT.Create(self,Symbol,Interval,aStream,FStartLoadingFrom);
    result:=aDS;
  finally
    aStream.Free;
  end;
end;

procedure TStockDataSourceConnection_MTFile.OnDefineValues;
begin
  inherited;
end;

initialization
  Serialization.TClassFactory.RegisterClass(TStockDataSourceConnection_MTFile);
end.
