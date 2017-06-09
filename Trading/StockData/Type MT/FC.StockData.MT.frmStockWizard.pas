{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Страница выбора источника данных для формата MetaTrader 4

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.MT.frmStockWizard;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, FC.StockWizards.frmStockWizardFiles_B, Mask, ExtendControls;

type                                                                       
  TfrmStockDataConnectionWizardMT = class(TfrmStockDataConnectionWizardFiles_B)
  protected
    function FileExt: string; override;
    function CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection; override;
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardMT = class (TStockDataConnectionWizard_B)
  protected
    function Title: string; override;
  public
    constructor Create; override;
  end;


implementation
   uses SystemService,BaseUtils,Application.Definitions,
        FC.Factory,FC.StockData.MT.StockDataConnection;

{$R *.dfm}

{ TStockDataConnectionWizardMT }

constructor TStockDataConnectionWizardMT.Create;
begin
  inherited CreateTargeted(TfrmStockDataConnectionWizardMT.Create(nil));
end;

function TStockDataConnectionWizardMT.Title: string;
begin
  result:='MetaTrader files';
end;

{ TfrmStockDataConnectionWizardMT }

function TfrmStockDataConnectionWizardMT.CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection;
begin
  result:=TStockDataSourceConnection_MTFile.Create(aSymbol,aInterval,aFileName);
end;

function TfrmStockDataConnectionWizardMT.FileExt: string;
begin
  result:='hst'
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataConnectionWizardMT));

end.

