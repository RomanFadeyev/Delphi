unit FC.StockWizards.fmStockWizardContainer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtCtrls,
  ComCtrls, ExtendControls, FC.Definitions;

type
  TfmStockDataConnectionWizardContainer = class(TfmDialogOkCancel_B)
    Splitter1: TSplitter;
    Panel1: TPanel;
    pcWizards: TPageControl;
    paHeader: TExtendPanel;
    Panel2: TPanel;
    lvWizards: TExtendListView;
    procedure lvWizardsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure acOKUpdate(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
  private
    FWizards: TInterfaceList;
    FDataSources: TStockTimeIntervalDataSourceArray;
    FStockSymbol: string;
    FSingleMode: boolean;

    procedure WMWizardDblClick (var Message: TMessage); message WM_USER+$300;

    function ActiveWizard: IStockDataConnectionWizard;
  public
    procedure SetSingleMode;
    procedure RemoveConnection(const aIID: TGUID);
    procedure SetDefaultSymbol(const aSymbol: string);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizardContainer = class (TComponentContainer,IStockDataConnectionWizardContainer)
  private
    function Target : TfmStockDataConnectionWizardContainer;
  protected
    //from IStockControl,IStockDialog
    function Control: TWinControl;

    procedure RemoveConnection(const aIID: TGUID);
    procedure SetDefaultSymbol(const aSymbol: string);
    function  Run(out aStockSymbol: string; out aDataSources: TStockTimeIntervalDataSourceArray): boolean;
    function  RunSingleMode(out aDataSource: IStockDataSource): boolean;
  public
    constructor Create; override;
  end;

implementation
   uses SystemService, FC.Factory, Application.Definitions, ufmDialogOK_B;

{$R *.dfm}


{ TStockDataConnectionWizardContainer }

constructor TStockDataConnectionWizardContainer.Create;
begin
  inherited CreateTargeted(TfmStockDataConnectionWizardContainer.Create(nil));
end;

function TStockDataConnectionWizardContainer.Target: TfmStockDataConnectionWizardContainer;
begin
  result:=TfmStockDataConnectionWizardContainer(inherited Target);
end;

function TStockDataConnectionWizardContainer.Control: TWinControl;
begin
  result:=Target;
end;

procedure TStockDataConnectionWizardContainer.RemoveConnection(const aIID: TGUID);
begin
  Target.RemoveConnection(aIID);
end;

function TStockDataConnectionWizardContainer.Run(out aStockSymbol: string; out aDataSources: TStockTimeIntervalDataSourceArray): boolean;
var
  i:TStockTimeInterval;
begin
  result:=Target.ShowModal=mrOK;
  if result then
  begin
    for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    begin
      aDataSources[i]:=Target.FDataSources[i];
      Target.FDataSources[i]:=nil;
    end;

    aStockSymbol:=Target.FStockSymbol;
  end;
end;

function TStockDataConnectionWizardContainer.RunSingleMode(out aDataSource: IStockDataSource): boolean;
begin
  Target.SetSingleMode;
  result:=Target.ShowModal=mrOK;
  if result then
    aDataSource:=Target.FDataSources[sti1];
end;

procedure TStockDataConnectionWizardContainer.SetDefaultSymbol(const aSymbol: string);
begin
  Target.SetDefaultSymbol(aSymbol);
end;

{ TfmStockDataConnectionWizardContainer }

constructor TfmStockDataConnectionWizardContainer.Create(aOwner: TComponent);
var
  aWizard : IStockDataConnectionWizard;
  i: integer;
  aTab: TTabSheet;
  aDSName : string;
  aLI: TListItem;
begin
  inherited;

  TWaitCursor.SetUntilIdle;

  FWizards:=TInterfaceList.Create;

  Factory.CreateObjects(IStockDataConnectionWizard,FWizards);
  for i:=0 to FWizards.Count-1 do
  begin
    aTab:=TTabSheet.Create(pcWizards);
    aTab.PageControl:=pcWizards;
    aWizard:=FWizards[i] as IStockDataConnectionWizard;
    aWizard.Control.Parent:=aTab;
    aTab.TabVisible:=false;
    aWizard.Control.Align:=alClient;
    with lvWizards.Items.Add do
    begin
      Caption:=aWizard.Title;
      Data:=pointer(i);
    end;
  end;

  //Глюк. Нужно пересоздать ListView, иначе надписи резанные
  TfmStockDataConnectionWizardContainer(lvWizards).RecreateWnd;
  lvWizards.HandleNeeded;

  if lvWizards.Items.Count>0 then
  begin
    lvWizards.Items[0].Focused:=true;
    lvWizards.Items[0].Selected:=true;
  end;

  aDSName:=Workspace.Storage(self).ReadString(self,'Last Stock Datasource','');
  aLI:=lvWizards.FindCaption(0,aDSName,false,true,false);
  if aLI<>nil then
  begin
    aLI.Focused:=true;
    aLI.Selected:=true;
  end;
end;

destructor TfmStockDataConnectionWizardContainer.Destroy;
begin
  lvWizards.OnSelectItem:=nil;
  FreeAndNil(FWizards);
  inherited;
end;

function TfmStockDataConnectionWizardContainer.ActiveWizard: IStockDataConnectionWizard;
begin
  if pcWizards.ActivePageIndex=-1 then
    result:=nil
  else
    result:= FWizards[pcWizards.ActivePageIndex] as IStockDataConnectionWizard;
end;

procedure TfmStockDataConnectionWizardContainer.lvWizardsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  inherited;
  if Item=nil then
  begin
    pcWizards.ActivePage:=nil;
    paHeader.Caption:='';
  end
  else begin
    pcWizards.ActivePageIndex:=integer(Item.Data);
    paHeader.Caption:=ActiveWizard.Title;
  end
end;

procedure TfmStockDataConnectionWizardContainer.RemoveConnection(const aIID: TGUID);
var
  i,j: integer;
begin
  for i:=0 to FWizards.Count-1 do
    if Supports(FWizards[i],aIID) then
    begin
       for j:= 0 to lvWizards.Items.Count - 1 do
       begin
         if lvWizards.Items[j].Data=pointer(i) then
         begin
           lvWizards.Items.Delete(j);
           break;
         end;
       end;
    end;

  if (lvWizards.Selected=nil) and (lvWizards.Items.Count>0) then
  begin
    lvWizards.Items[0].Focused:=true;
    lvWizards.Items[0].Selected:=true;
  end;
end;

procedure TfmStockDataConnectionWizardContainer.SetDefaultSymbol(const aSymbol: string);
var
  i: integer;
begin
  for i:= 0 to FWizards.Count - 1 do
    (FWizards[i] as IStockDataConnectionWizard).SetSelectedSymbol(aSymbol);
end;

procedure TfmStockDataConnectionWizardContainer.SetSingleMode;
var
  i: integer;
begin
  for i:= 0 to FWizards.Count - 1 do
    (FWizards[i] as IStockDataConnectionWizard).SetSingleMode;
  FSingleMode:=true;
end;

procedure TfmStockDataConnectionWizardContainer.WMWizardDblClick(var Message: TMessage);
begin
  acOK.Execute;
end;

procedure TfmStockDataConnectionWizardContainer.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(ActiveWizard<>nil) and (ActiveWizard.Validate);
end;

procedure TfmStockDataConnectionWizardContainer.acOKExecute(Sender: TObject);
var
  i:TStockTimeInterval;
begin
  try
    Workspace.Storage(self).WriteString(self,'Last Stock Datasource',lvWizards.Selected.Caption);

    ASSERT(ActiveWizard<>nil);
    FStockSymbol:=ActiveWizard.GetSelectedSymbol;
    if (FStockSymbol='') then
    begin
      ModalResult:=mrNone;
      MsgBox.MessageAttention(Handle,'Enter symbol name before finishing wizard.',[]);
      exit;
    end;


    TWaitCursor.SetUntilIdle;
    if FSingleMode then
      ActiveWizard.OnOK(sti1,FDataSources[sti1])
    else begin
      for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
        FDataSources[i]:=nil;
      ActiveWizard.OnOK(FDataSources);
    end;
  except
    on E:Exception do
    begin
      for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
        FDataSources[i]:=nil;
      MsgBox.Error(0,E);
      ModalResult:=mrNone;
      exit;
    end;
  end;

  inherited;
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Unique.Create(IStockDataConnectionWizardContainer,TStockDataConnectionWizardContainer));

end.
