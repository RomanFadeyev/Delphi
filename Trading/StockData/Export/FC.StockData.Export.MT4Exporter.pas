{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный для экспорта котировок
 History:
-----------------------------------------------------------------------------}
unit FC.StockData.Export.MT4Exporter;

{$I Compiler.inc}

interface
  uses Windows,Forms,BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj;

type
  TStockDataFileExporterMT4 = class(TStockInterfacedObjectVirtual,IStockDataFileExporter)
  private
    FCopyright : AnsiString;
  public
    function  Filter: string;
    function  DefaultExt: string;    
    function  Description: string;

    procedure Run(const aDS: IStockDataSource; aStream: TStream);
    constructor Create;   override;

    property Copyright : AnsiString read FCopyright write FCopyright;
  end;

implementation
  uses DateUtils, SystemService,
       Application.Definitions,
       FC.Factory,
       FC.DataUtils,
       MetaTrader.HistoryFileStruct,
       FC.StockData.MT.StockDataSource;

{ TStockDataFileExporterMT4 }

constructor TStockDataFileExporterMT4.Create;
begin
  inherited;
  //FCopyright:=AnsiString('(C) '+Workspace.CompanyName);
  FCopyright:='(C)opyright 2003, MetaQuotes Software Corp.';
end;

function TStockDataFileExporterMT4.DefaultExt: string;
begin
  result:='.hst';
end;

function TStockDataFileExporterMT4.Description: string;
begin
  result:='MT4 files (*.hst)';
end;

function TStockDataFileExporterMT4.Filter: string;
begin
  result:='*.hst';
end;

procedure TStockDataFileExporterMT4.Run(const aDS: IStockDataSource; aStream: TStream);
var
  i: integer;
  aHeader: TMT4Header;
  s: AnsiString;
  aDate: time_t;
  aV: TStockRealNumber;
  aWriter: TWriter;
begin
  TWaitCursor.SetUntilIdle;

	//4 байта - версия
  i:=400;
  aStream.WriteBuffer(i,sizeof(i));

  //Дальше заголовок
  ZeroMemory(@aHeader,sizeof(aHeader));
  s:=FCopyright;
  StrLCopy(@aHeader.copyright[0],PAnsiChar(s),sizeof(aHeader.copyright));

  s:=AnsiString(aDS.StockSymbol.Name);
  StrLCopy(@aHeader.symbol[0],PAnsiChar(s),sizeof(aHeader.symbol));

  aHeader.period:=aDS.StockSymbol.GetTimeIntervalValue;
  aHeader.digits:=aDS.GetPricePrecision; 
  aStream.WriteBuffer(aHeader,sizeof(aHeader));

  //данные
  aWriter:=TWriter.Create(aStream,1024*1024); //использование врайтера дает оптимизацию за счет использования буфера
  try
    //данные
    for I := 0 to aDS.RecordCount - 1 do
    begin
      aDate:=DateTimeToUnix(aDS.GetDataDateTime(i));
      aWriter.Write(aDate,sizeof(aDate));

      //Open
      aV:=aDS.GetDataOpen(i);
      aWriter.Write(aV,sizeof(aV));

      //Low
      aV:=aDS.GetDataLow(i);
      aWriter.Write(aV,sizeof(aV));

      //High
      aV:=aDS.GetDataHigh(i);
      aWriter.Write(aV,sizeof(aV));

      //Close
      aV:=aDS.GetDataClose(i);
      aWriter.Write(aV,sizeof(aV));

      //Volume
      aV:=aDS.GetDataVolume(i);
      aWriter.Write(aV,sizeof(aV));
    end;
    aWriter.FlushBuffer;
  finally
    aWriter.Free;
  end;
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataFileExporterMT4));

end.
