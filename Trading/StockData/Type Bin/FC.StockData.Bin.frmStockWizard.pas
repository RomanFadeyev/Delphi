unit FC.StockData.Bin.frmStockWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, FC.StockWizards.frmStockWizardFiles_B, Mask, ExtendControls;

type
  TfrmStockDataConnectionWizardBin = class(TfrmStockDataConnectionWizardFiles_B)
  protected
    function FileExt: string; override;
    function CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection; override;
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardBin = class (TStockDataConnectionWizard_B)
  protected
    function Title: string; override;
  public
    constructor Create; override;
  end;


implementation
   uses SystemService,BaseUtils,Application.Definitions, FC.Factory,
   FC.StockData.Bin.StockDataConnection;

{$R *.dfm}

{ TStockDataConnectionWizardBin }

constructor TStockDataConnectionWizardBin.Create;
begin
  inherited CreateTargeted(TfrmStockDataConnectionWizardBin.Create(nil));
end;

function TStockDataConnectionWizardBin.Title: string;
begin
  result:='Binary files';
end;

{ TfrmStockDataConnectionWizardBin }

function TfrmStockDataConnectionWizardBin.CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection;
begin
  result:=TStockDataSourceConnection_BinFile.Create(aSymbol,aInterval,aFileName);
end;

function TfrmStockDataConnectionWizardBin.FileExt: string;
begin
  result:='sbf'
end;


initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataConnectionWizardBin));

end.
