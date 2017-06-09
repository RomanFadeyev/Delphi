unit FC.StockData.CSV.frmStockWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, FC.StockWizards.frmStockWizardFiles_B, Mask, ExtendControls;

type
  TfrmStockDataConnectionWizardCSV = class(TfrmStockDataConnectionWizardFiles_B)
  protected
    function FileExt: string; override;
    function CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection; override;
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardCSV = class (TStockDataConnectionWizard_B)
  protected
    function Title: string; override;
  public
    constructor Create; override;
  end;


implementation
   uses SystemService,BaseUtils,Application.Definitions, FC.Factory,
   FC.StockData.CSV.StockDataConnection;

{$R *.dfm}

{ TStockDataConnectionWizardCSV }

constructor TStockDataConnectionWizardCSV.Create;
begin
  inherited CreateTargeted(TfrmStockDataConnectionWizardCSV.Create(nil));
end;

function TStockDataConnectionWizardCSV.Title: string;
begin
  result:='CSV (D-OHLC-V) files';
end;

{ TfrmStockDataConnectionWizardCSV }

function TfrmStockDataConnectionWizardCSV.CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection;
begin
  result:=TStockDataSourceConnection_CSVFile.Create(aSymbol,aInterval,aFileName);
end;

function TfrmStockDataConnectionWizardCSV.FileExt: string;
begin
  result:='csv'
end;


initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataConnectionWizardCSV));

end.
