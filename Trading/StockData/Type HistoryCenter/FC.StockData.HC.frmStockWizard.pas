{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   —траница выбора источника данных дл€ истории котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.HC.frmStockWizard;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, FC.StockWizards.frmStockWizardFiles_B, Mask, ExtendControls,
  FC.StockData.HC.StockDataConnection, AppEvnts;

type
  TfrmStockDataConnectionWizardHistoryCenter = class(TfrmFrame_B)
    laSymbols: TLabel;
    lvSymbols: TExtendListView;
    ckE_LimitLoading: TExtendCheckBox;
    edE_StartFromDate: TExtendDateTimePicker;
    laStartFrom: TLabel;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure lvSymbolsDblClick(Sender: TObject);
  private
    FSavedSelectedSymbol: string;
    FFilterSymbol: string;
    FAllowedIntervals: array [TStockTimeInterval] of boolean;
    FSingleMode: boolean;
  protected
    procedure LoadSymbols; virtual;
    procedure CreateWnd; override;
    procedure DestroyHandle; override;

    function  CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval): IStockDataSourceConnection; virtual;
    function  GetStockDataStorage: IStockDataStorage; virtual;
  public
    function  Validate: boolean; virtual;
    function  GetSelectedSymbol: string;
    procedure SetSelectedSymbol(const aSymbol: string);

    procedure OnOK(out aDataSources: TStockTimeIntervalDataSourceArray); overload;
    procedure OnOK(const aInterval: TStockTimeInterval; out aDataSource:IStockDataSource); overload;

    //«апретить пользоватую смену валютной пары
    procedure Filter(const aStockSymbol: string); overload;

    procedure Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval);overload;

    procedure SetSingleMode;

    constructor Create(aOwner: TCOmponent); override;
    destructor Destroy; override;
  end;

  //Ќапр€мую TFrame нельз€ выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardHistoryCenter = class (TComponentContainer,IStockDataConnectionWizard,IStockDataConnectionWizardHistoryCenter)
  private
    function Target : TfrmStockDataConnectionWizardHistoryCenter;
  protected
    //from IStockControl
    function  Control: TWinControl;
    //from IStockDataConnectionWizard
    function Title: string;
    function Validate: boolean;
    function GetSelectedSymbol: string;
    procedure SetSelectedSymbol(const aSymbol: string);

    //«апретить пользоватую смену валютной пары
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
   FC.Factory,FC.Singletons;

{$R *.dfm}

{ TStockDataConnectionWizardHistoryCenter }

constructor TStockDataConnectionWizardHistoryCenter.Create;
begin
  inherited CreateTargeted(TfrmStockDataConnectionWizardHistoryCenter.Create(nil));
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

function TStockDataConnectionWizardHistoryCenter.Target: TfrmStockDataConnectionWizardHistoryCenter;
begin
  result:=TfrmStockDataConnectionWizardHistoryCenter(inherited Target);
end;

function TStockDataConnectionWizardHistoryCenter.Title: string;
begin
  result:='History Center';
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

procedure TfrmStockDataConnectionWizardHistoryCenter.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  edE_StartFromDate.Enabled:=ckE_LimitLoading.Checked;
  laStartFrom.Enabled:=ckE_LimitLoading.Checked;
end;

constructor TfrmStockDataConnectionWizardHistoryCenter.Create(aOwner: TCOmponent);
var
  i : TStockTimeInterval;
begin
  inherited;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FAllowedIntervals[i]:=true;

  ckE_LimitLoading.Checked:=Workspace.Storage(self).ReadBoolean(ckE_LimitLoading,'Value',false);
  edE_StartFromDate.DateTime:=Workspace.Storage(self).ReadDateTime(edE_StartFromDate,'Value',EncodeDate(2000,1,1));

  LoadSymbols;
end;

destructor TfrmStockDataConnectionWizardHistoryCenter.Destroy;
begin
  Workspace.Storage(self).WriteBoolean(ckE_LimitLoading,'Value',ckE_LimitLoading.Checked);
  Workspace.Storage(self).WriteDateTime(edE_StartFromDate,'Value',edE_StartFromDate.DateTime);
  inherited;
end;

function TfrmStockDataConnectionWizardHistoryCenter.CreateConnection(const aSymbol: string;
  aInterval: TStockTimeInterval): IStockDataSourceConnection;
begin
  if ckE_LimitLoading.Checked then
    result:=TStockDataSourceConnection_HistoryCenter.Create(aSymbol,aInterval,edE_StartFromDate.DateTime)
  else
    result:=TStockDataSourceConnection_HistoryCenter.Create(aSymbol,aInterval,-1);
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.CreateWnd;
begin
  inherited;
  if FSavedSelectedSymbol<>'' then
    SetSelectedSymbol(FSavedSelectedSymbol);
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.DestroyHandle;
begin
  if not (csDestroying in ComponentState)  then
    FSavedSelectedSymbol:=GetSelectedSymbol;
  inherited;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval);
var
  i             : TStockTimeInterval;
begin
  FFilterSymbol:=aStockSymbol;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FAllowedIntervals[i]:=false;
  FAllowedIntervals[aInterval]:=true;

  LoadSymbols;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.Filter(const aStockSymbol: string);
var
  i             : TStockTimeInterval;
begin
  FFilterSymbol:=aStockSymbol;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FAllowedIntervals[i]:=true;

  LoadSymbols;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.LoadSymbols;
var
  aSymbols: TStockSymbolInfoArray;
  aItem : TListItem;
  aSelectedSymbol: string;
  i: integer;
begin
  lvSymbols.HandleNeeded;
  aSelectedSymbol:=GetSelectedSymbol;
  lvSymbols.Items.Clear;

  aSymbols:=GetStockDataStorage.GetSymbols;

  for i := 0 to High(aSymbols) do
  begin
    if (FFilterSymbol='') or
       (AnsiSameText(FFilterSymbol,aSymbols[i].Name))  then
    begin
      aItem:=lvSymbols.Items.Add;
      aItem.Caption:=aSymbols[i].Name;
      aItem.SubItems.Add(aSymbols[i].Description);
    end;
  end;

  if aSelectedSymbol<>'' then
    SetSelectedSymbol(aSelectedSymbol);
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.lvSymbolsDblClick(Sender: TObject);
begin
  inherited;
  if lvSymbols.Selected<>nil then
    PostMessage(GetParentForm(self).Handle,WM_USER+$300,0,0);
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.OnOK(out aDataSources: TStockTimeIntervalDataSourceArray);
var
  i             : TStockTimeInterval;
  aConnection   : IStockDataSourceConnection;
begin
  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
  begin
    if FAllowedIntervals[i] then
    begin
      aConnection:=CreateConnection(lvSymbols.Selected.Caption,i);
      aDataSources[i]:=aConnection.CreateDataSource;
    end;
  end;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.OnOK(const aInterval: TStockTimeInterval; out aDataSource: IStockDataSource);
var
  aConnection   : IStockDataSourceConnection;
begin
  aConnection:=CreateConnection(lvSymbols.Selected.Caption,aInterval);
  aDataSource:=aConnection.CreateDataSource;
end;

function TfrmStockDataConnectionWizardHistoryCenter.GetSelectedSymbol: string;
begin
  if lvSymbols.Selected=nil then
    result:=''
  else
    result:=lvSymbols.Selected.Caption;
end;

function TfrmStockDataConnectionWizardHistoryCenter.GetStockDataStorage: IStockDataStorage;
begin
  result:=StockDataStorage;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.SetSelectedSymbol(const aSymbol: string);
var
  i: integer;
begin
  lvSymbols.HandleNeeded;

  for i := 0 to lvSymbols.Items.Count - 1 do
    if AnsiSameText(lvSymbols.Items[i].Caption,aSymbol) then
    begin
      lvSymbols.Items[i].Selected:=true;
      lvSymbols.Items[i].Focused:=true;
      break;
    end;
end;

procedure TfrmStockDataConnectionWizardHistoryCenter.SetSingleMode;
begin
  FSingleMode:=true;
  Filter('',sti1);
end;

function TfrmStockDataConnectionWizardHistoryCenter.Validate: boolean;
begin
  result:=lvSymbols.Selected<>nil;
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataConnectionWizardHistoryCenter));

end.
