{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Страница выбора источника данных для истории котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.EHC.frmStockWizard;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, FC.StockWizards.frmStockWizardFiles_B, Mask, ExtendControls,
  FC.StockData.EHC.StockDataConnection, Buttons,
  FC.StockData.HC.frmStockWizard, AppEvnts;

type
  TfrmStockDataConnectionWizardEHC = class(TfrmStockDataConnectionWizardHistoryCenter)
    Panel1: TPanel;
    edPath: TExtendEdit;
    Label2: TLabel;
    buLoadRight: TExtendBitBtn;
    dlgOpen: TOpenDialog;
    procedure buLoadRightClick(Sender: TObject);
  private
    FExternalDataBase: IStockDataStorage;
  protected
    procedure LoadSymbols; override;
    function  CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval): IStockDataSourceConnection; override;    
  public
    function  Validate: boolean; override;

    destructor Destroy; override;
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardHistoryCenter = class (TComponentContainer,IStockDataConnectionWizard)
  private
    function Target : TfrmStockDataConnectionWizardEHC;
  protected
    //from IStockControl
    function  Control: TWinControl;
    //from IStockDataConnectionWizard
    function Title: string;
    function Validate: boolean;
    function GetSelectedSymbol: string;
    procedure SetSelectedSymbol(const aSymbol: string);

    //Запретить пользоватую смену валютной пары
    procedure Filter(const aStockSymbol: string); overload;

    procedure Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval); overload;

    procedure SetSingleMode;

    procedure OnOK(out aDataSources: TStockTimeIntervalDataSourceArray);overload;
    procedure OnOK(const aInterval: TStockTimeInterval; out aDataSource:IStockDataSource); overload;
  public
    constructor Create; override;
  end;


implementation
   uses SystemService,BaseUtils,Application.Definitions,
   FC.Factory,FC.HistoryCenter.DataStorage;

{$R *.dfm}

{ TStockDataConnectionWizardHistoryCenter }

constructor TStockDataConnectionWizardHistoryCenter.Create;
begin
  inherited CreateTargeted(TfrmStockDataConnectionWizardEHC.Create(nil));
end;

procedure TStockDataConnectionWizardHistoryCenter.Filter(const aStockSymbol: string);
begin
  Target.Filter(aStockSymbol);
end;

procedure TStockDataConnectionWizardHistoryCenter.Filter(const aStockSymbol: string;
  const aInterval: TStockTimeInterval);
begin
  Target.Filter(aStockSymbol,aInterval);
end;

function TStockDataConnectionWizardHistoryCenter.Control: TWinControl;
begin
  result:=Target;
end;

function TStockDataConnectionWizardHistoryCenter.Target: TfrmStockDataConnectionWizardEHC;
begin
  result:=TfrmStockDataConnectionWizardEHC(inherited Target);
end;

function TStockDataConnectionWizardHistoryCenter.Title: string;
begin
  result:='External History Database';
end;

function TStockDataConnectionWizardHistoryCenter.Validate: boolean;
begin
  result:=Target.Validate;
end;

procedure TStockDataConnectionWizardHistoryCenter.OnOK(const aInterval: TStockTimeInterval;
  out aDataSource: IStockDataSource);
begin
  Target.OnOK(aInterval,aDataSource);
end;

function TStockDataConnectionWizardHistoryCenter.GetSelectedSymbol: string;
begin
  result:=Target.GetSelectedSymbol;
end;

procedure TStockDataConnectionWizardHistoryCenter.SetSelectedSymbol(const aSymbol: string);
begin
  Target.SetSelectedSymbol(aSymbol);
end;

procedure TStockDataConnectionWizardHistoryCenter.SetSingleMode;
begin
  Target.SetSingleMode;
end;

procedure TStockDataConnectionWizardHistoryCenter.OnOK(out aDataSources: TStockTimeIntervalDataSourceArray);
begin
  Target.OnOK(aDataSources);
end;

procedure TfrmStockDataConnectionWizardEHC.buLoadRightClick(Sender: TObject);
begin
  inherited;
  dlgOpen.FileName:=edPath.Text;
  dlgOpen.InitialDir:=ExtractFileDir(edPath.Text);

  if dlgOpen.Execute then
  begin
    FExternalDataBase:=nil;
    try
      TWaitCursor.SetUntilIdle;
      FExternalDataBase:=TStockDataStorageContainer.Create(dlgOpen.FileName);
      FExternalDataBase.CheckConnected;
      edPath.Text:=dlgOpen.FileName;
      LoadSymbols;
    except
      on E:Exception do
      begin
        FExternalDataBase:=nil;
        MsgBox.MessageFailure(Handle,E.Message);
      end;
    end;
  end;

  FExternalDataBase:=nil;
end;

function TfrmStockDataConnectionWizardEHC.CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval): IStockDataSourceConnection;
var
  aStartLoadingFrom: TDateTime;
begin
  if ckE_LimitLoading.Checked then
    aStartLoadingFrom:=edE_StartFromDate.DateTime
  else
    aStartLoadingFrom:=-1;

  result:=TStockDataSourceConnection_EHC.Create(aSymbol,aInterval,aStartLoadingFrom,edPath.Text)
end;

destructor TfrmStockDataConnectionWizardEHC.Destroy;
begin
  FExternalDataBase:=nil;
  inherited;
end;

procedure TfrmStockDataConnectionWizardEHC.LoadSymbols();
begin
  lvSymbols.Clear;
  if FExternalDataBase=nil then
    exit;

  inherited;
end;

function TfrmStockDataConnectionWizardEHC.Validate: boolean;
begin
  result:=lvSymbols.Selected<>nil;
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataConnectionWizardHistoryCenter));

end.
