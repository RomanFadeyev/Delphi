{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный для экспорта котировок
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.Export.CSVExporter;
{$I Compiler.inc}

interface
  uses Windows,Forms,BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj;

type
  TStockDataFileExporterSimpleCSV = class(TStockInterfacedObjectVirtual,IStockDataFileExporter)
  public
    function  Filter: string;
    function  DefaultExt: string;    
    function  Description: string;

    procedure Run(const aDS: IStockDataSource; aStream: TStream);
  end;

implementation
  uses FC.Factory,
       FC.DataUtils,
       FC.StockData.Export.CSVExporter.Dialog;

{ TStockDataFileExporterSimpleCSV }

function TStockDataFileExporterSimpleCSV.DefaultExt: string;
begin
  result:='.csv';
end;

function TStockDataFileExporterSimpleCSV.Description: string;
begin
  result:='CSV files (*.csv)';
end;

function TStockDataFileExporterSimpleCSV.Filter: string;
begin
  result:='*.csv';
end;

procedure TStockDataFileExporterSimpleCSV.Run(const aDS: IStockDataSource; aStream: TStream);
begin
  TfmExportDialog.Run(aDS,aStream);
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataFileExporterSimpleCSV));

end.
