{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSourceConnection для загрузки данных из файла
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.StockDataConnectionFile;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, Serialization, FC.Definitions,FC.StockData.StockDataSource;

type
  //Basic connection
  TStockDataSourceConnectionFile = class (TNameValuePersistentObjectRefCounted,IStockDataSourceConnection)
  private
    FConnectionString: string;
    FSymbol: string;
    FInterval : TStockTimeInterval;
  public
    constructor Create(const aSymbol: string; aInterval: TStockTimeInterval; const aConnectionString: string);

    property Symbol: string read FSymbol;
    property Interval : TStockTimeInterval read FInterval;

    //TPersistentObject
    procedure OnDefineValues; override;
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;

    //IStockDataSourceConnection
    function CreateDataSource(aUseCacheIfPossible: boolean=true): IStockDataSource; virtual; abstract;
    function ConnectionString: string;    
  end;

  TFileStreamEx = class(TFileStream)
  public
    constructor CreateForRead(const aFileName:string);
  end;

implementation
  uses RTLConsts;


{ TFileStreamEx }

constructor TFileStreamEx.CreateForRead(const aFileName:string);
var
  x: integer;
begin
  x:=SysUtils.FileOpen(aFileName, fmOpenRead or fmShareDenyNone);
  if x < 0 then
    raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(aFileName), SysErrorMessage(GetLastError)]);
  inherited Create(x);
end;

{ TStockDataSourceConnectionFile }

function TStockDataSourceConnectionFile.ConnectionString: string;
begin
  result:=FConnectionString;
end;

constructor TStockDataSourceConnectionFile.Create(const aSymbol: string; aInterval: TStockTimeInterval; const aConnectionString: string);
begin
  inherited Create;
  FSymbol:=aSymbol;
  FInterval:=aInterval;
  FConnectionString:=aConnectionString;
end;

procedure TStockDataSourceConnectionFile.OnDefineValues;
begin
  inherited;
  DefValString('ConnectionString',FConnectionString);
  DefValString('Symbol',FSymbol);
end;

procedure TStockDataSourceConnectionFile.OnReadValue(const aReader: INameValueDataReader;
  const aName: string; var aHandled: boolean);
begin
  inherited;
  if aName='Interval' then
  begin
    aReader.ReadEnum(FInterval);
    aHandled:=true;
  end;
end;

procedure TStockDataSourceConnectionFile.OnWriteValues(const aWriter: INameValueDataWriter);
begin
  inherited;
  aWriter.WriteEnum('Interval',FInterval);
end;


end.
