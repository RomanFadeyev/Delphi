unit FC.StockWizards.frmStockWizardFiles_B;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ufrmFrame_B, StdCtrls, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, ImgList,
  ActnList, Menus, DB, MemoryDS, ComCtrls, ToolWin, ExtCtrls, DBClient,
  RecentlyList,FC.Definitions, Mask, ExtendControls;

type
  TfrmStockDataConnectionWizardFiles_B = class(TfrmFrame_B)
    grSources       : TEditDBGrid;
    paToolbar       : TPanel;
    laHeader        : TLabel;
    ToolBar1        : TToolBar;
    buBrowseForFolder: TToolButton;
    dlgFileOpen     : TOpenDialog;
    alMain          : TActionList;
    acInsertFromDir : TAction;
    ilImages        : TImageList;
    taFiles         : TClientDataSet;
    taFilesPATH     : TStringField;
    dsFiles         : TDataSource;
    rlRecentlyDirs  : TRecentlyList;
    pmRecentlyDirs  : TPopupMenu;
    taFilesINT: TIntegerField;
    edInstrument: TExtendComboBox;
    laSpace: TLabel;
    laSymbol: TLabel;
    procedure taFilesINTGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure acInsertFromDirExecute(Sender: TObject);
    procedure grSourcesEditorCreating(Sender: TObject; var InplaceEditor: TInplaceEditEx);
    procedure rlRecentlyDirsRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: String);
    procedure grSourcesCanEdit(Sender: TObject; var CanEdit: Boolean);
  private
    FFilterSymbol: string;
    FAllowedIntervals: array [TStockTimeInterval] of boolean;
    FSingleMode: boolean;

    procedure OnBtnEditClick(Sender:TObject; var Text:string);
    procedure LoadFromDir(const aDir: string);
    function  GetFiles: TStockTimeIntervalStringArray;
    procedure WMSize      (var Message: TWMSize); message WM_SIZE;
  protected
    procedure Init;

    function CreateConnection(const aSymbol: string; aInterval: TStockTimeInterval; const aFileName: string): IStockDataSourceConnection; virtual; abstract;
    function FileExt: string; virtual; abstract;
  public
    function  Validate: boolean;
    function  GetSelectedSymbol: string;
    procedure SetSelectedSymbol(const aSymbol: string);

    procedure OnOK(out aDataSources: TStockTimeIntervalDataSourceArray); overload;
    procedure OnOK(const aInterval: TStockTimeInterval; out aDataSource:IStockDataSource); overload;

    //«апретить пользоватую смену пары
    procedure Filter(const aStockSymbol: string); overload;

    procedure Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval); overload;

    procedure SetSingleMode;

    constructor Create (aOwner:TComponent); override;
    destructor Destroy;override;
  end;

  //Ќапр€мую TFrame нельз€ выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockDataConnectionWizard_B = class (TComponentContainer,IStockDataConnectionWizard)
  private
    function Target : TfrmStockDataConnectionWizardFiles_B;
  protected
    //from IStockControl
    function  Control: TWinControl;
    //from IStockDataConnectionWizard
    function  Title: string; virtual; abstract;
    function  Validate: boolean;

    function GetSelectedSymbol: string;
    procedure SetSelectedSymbol(const aSymbol: string);

    procedure SetSingleMode;

    procedure Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval); overload;

    //«апретить пользоватую смену пары
    procedure Filter(const aStockSymbol: string); overload;

    //ѕо закрытию мастер должен вернуть массив всех DataSource дл€ всех интервалов выбранной пары
    procedure OnOK(out aDataSources: TStockTimeIntervalDataSourceArray); overload;
    //“оже самое, только возвращает 1 указанный DataSource
    procedure OnOK(const aInterval: TStockTimeInterval; out aDataSource:IStockDataSource); overload;
  end;


implementation
   uses SystemService,BaseUtils,Application.Definitions, FC.Factory,FC.DataUtils,FC.Singletons;

{$R *.dfm}

type
  TInplaceFileEditor = class (TInplaceEditBtn)
  end;

const
  fnINT  = 'INT';
  fnPATH = 'PATH';

{ TStockDataConnectionWizard_B }

procedure TStockDataConnectionWizard_B.Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval);
begin
  Target.Filter(aStockSymbol,aInterval);
end;

procedure TStockDataConnectionWizard_B.OnOK(const aInterval: TStockTimeInterval; out aDataSource: IStockDataSource);
begin
  Target.OnOK(aInterval,aDataSource);
end;

procedure TStockDataConnectionWizard_B.Filter(const aStockSymbol: string);
begin
  Target.Filter(aStockSymbol);
end;

function TStockDataConnectionWizard_B.GetSelectedSymbol: string;
begin
  result:=Target.GetSelectedSymbol;
end;

procedure TStockDataConnectionWizard_B.SetSelectedSymbol(const aSymbol: string);
begin
  Target.SetSelectedSymbol(aSymbol);
end;

procedure TStockDataConnectionWizard_B.SetSingleMode;
begin
  Target.SetSingleMode;
end;

function TStockDataConnectionWizard_B.Control: TWinControl;
begin
  result:=Target;
end;

function TStockDataConnectionWizard_B.Target: TfrmStockDataConnectionWizardFiles_B;
begin
  result:=TfrmStockDataConnectionWizardFiles_B(inherited Target);
end;

function TStockDataConnectionWizard_B.Validate: boolean;
begin
  result:=Target.Validate;
end;

procedure TStockDataConnectionWizard_B.OnOK(out aDataSources: TStockTimeIntervalDataSourceArray);
begin
  Target.OnOK(aDataSources);
end;

{ TfrmStockDataConnectionWizardFiles_B }

constructor TfrmStockDataConnectionWizardFiles_B.Create(aOwner: TComponent);
var
  i : TStockTimeInterval;
begin
  inherited;
  rlRecentlyDirs.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyDirs';
  edInstrument.Text:=Workspace.Storage(self).ReadString(self,'Instrument',edInstrument.Text);

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FAllowedIntervals[i]:=true;

  Init;
end;

destructor TfrmStockDataConnectionWizardFiles_B.Destroy;
begin
  Workspace.Storage(self).WriteString(Self,'Instrument',edInstrument.Text);
  inherited;
end;

procedure TfrmStockDataConnectionWizardFiles_B.Init;
var
 i : TStockTimeInterval;
 j : integer;
 aSymbols: TStockSymbolInfoArray;
begin
  taFiles.Close;
  taFiles.CreateDataSet;
  taFiles.DisableControls;
  try
    for i:=Low(StockTimeIntervalValues) to High(StockTimeIntervalValues) do
    begin
      if FAllowedIntervals[i] then
      begin
        taFiles.Append;
        taFiles[fnINT]:=StockTimeIntervalValues[i];

        taFiles.Post;
      end;
    end;
    taFiles.First;
  finally
    taFiles.EnableControls;
  end;

  if FSingleMode then
    grSources.Options1:=grSources.Options1+[dgFitColumnsToSize]
  else
    grSources.Options1:=grSources.Options1-[dgFitColumnsToSize];
  
  grSources.Columns[0].Width:=100;
  grSources.ColumnSizes.FitLastColumn;
  grSources.ScrollBars:=ssNone;

  aSymbols:=StockDataStorage.GetSymbols;
  edInstrument.Items.Clear;
  for j := Low(aSymbols) to High(aSymbols) do
    edInstrument.Items.Add(aSymbols[j].Name);
  edInstrument.Sorted:=true;

  if FFilterSymbol<>'' then
  begin
    edInstrument.Text:=FFilterSymbol;
    edInstrument.Enabled:=false
  end
  else begin
    edInstrument.Enabled:=true;
  end;

  buBrowseForFolder.Visible:=not FSingleMode;
  laSymbol.Visible:=not FSingleMode;
  edInstrument.Visible:=not FSingleMode;

  laSpace.Visible:=not FSingleMode;

  taFilesINT.Visible:=not FSingleMode;
end;

procedure TfrmStockDataConnectionWizardFiles_B.Filter(const aStockSymbol: string);
begin
  SetSelectedSymbol(aStockSymbol);
  edInstrument.Enabled:=false;  
end;

procedure TfrmStockDataConnectionWizardFiles_B.Filter(const aStockSymbol: string; const aInterval: TStockTimeInterval);
var
  i             : TStockTimeInterval;
begin
  FFilterSymbol:=aStockSymbol;

  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    FAllowedIntervals[i]:=false;
  FAllowedIntervals[aInterval]:=true;

  Init;
end;

function TfrmStockDataConnectionWizardFiles_B.GetFiles: TStockTimeIntervalStringArray;
var
  aBookmark: TBookmark;
  i: TStockTimeInterval;
begin
  taFiles.CheckBrowseMode;

  aBookmark:=taFiles.Bookmark;

  //i:=low(TStockTimeIntervalStringArray);
  taFiles.DisableControls;
  try
    taFiles.First;
    while not taFiles.EOF do
    begin
      if not TStockDataUtils.GetTimeIntervalByValue(taFiles.FieldByName(fnINT).AsInteger,i) then
        raise EAlgoError.Create;

      result[i]:=taFiles.FieldByName(fnPATH).AsString;
      taFiles.Next;
    end;
    taFiles.Bookmark:=aBookmark;
  finally
    taFiles.EnableControls;
  end;
end;

function TfrmStockDataConnectionWizardFiles_B.Validate: boolean;
var
  aFiles: TStockTimeIntervalStringArray;
  i: TStockTimeInterval;
begin
  result:= (not grSources.EditorMode) and (GetSelectedSymbol<>'');

  if result then
  begin
    aFiles:=GetFiles;
    for i:=Low(aFiles) to High(aFiles) do
      if FAllowedIntervals[i] then
        result:=result and FileExists(aFiles[i]);
  end;
end;

procedure TfrmStockDataConnectionWizardFiles_B.WMSize(var Message: TWMSize);
begin
  inherited;
  if  not (dgFitColumnsToSize in grSources.Options1) then
    grSources.ColumnSizes.FitLastColumn;
end;

procedure TfrmStockDataConnectionWizardFiles_B.OnOK(out aDataSources: TStockTimeIntervalDataSourceArray);
var
  aFiles        : TStockTimeIntervalStringArray;
  i             : TStockTimeInterval;
  aConnection   : IStockDataSourceConnection;
begin
  taFiles.CheckBrowseMode;

  aFiles:=GetFiles;

  for i:=Low(aFiles) to High(aFiles) do
  begin
    aConnection:=CreateConnection(edInstrument.Text, i, aFiles[i]);
    aDataSources[i]:=aConnection.CreateDataSource;
  end;
end;

procedure TfrmStockDataConnectionWizardFiles_B.OnOK(const aInterval: TStockTimeInterval; out aDataSource: IStockDataSource);
var
  aConnection   : IStockDataSourceConnection;
begin
  if FSingleMode then //¬ режиме Single Mode нельз€ знать, что грузитс€
    aConnection:=CreateConnection('', aInterval, GetFiles[aInterval])
  else
    aConnection:=CreateConnection(edInstrument.Text, aInterval, GetFiles[aInterval]);
  aDataSource:=aConnection.CreateDataSource;
end;

procedure TfrmStockDataConnectionWizardFiles_B.LoadFromDir(const aDir: string);
var
  aFileList : TStringList;
  b        : boolean;
  i        : integer;
  j        : TStockTimeInterval;
  aIntName : string;
  aMask    : string;
begin
  TWaitCursor.SetUntilIdle;
  aFileList:= TStringList.Create;

  if (edInstrument.Text='') or (edInstrument.Text='*') then
    aMask:='*.'+FileExt
  else
    aMask:=edInstrument.Text+'*.'+FileExt;

  try
    FindFiles(aDir,aMask,aFileList, faAnyFile,true);

    if aFileList.Count=0 then
    begin
      MsgBox.MessageAttention(0,'There are no files in the folder',[]);
      exit;
    end;

    aFileList.Sort;
    rlRecentlyDirs.AddRecently(aDir);

    b:=false;
    for j:=High(TStockTimeInterval) downto Low(TStockTimeInterval) do
    begin
      aIntName:=IntToStr(StockTimeIntervalValues[j]);
      for i:=0 to aFileList.Count-1 do
      begin
        if StrIPos(aIntName+'.',ExtractFileBaseName(aFileList[i])+'.')<>0 then
        begin
          b:=true;
          if not (taFiles.Locate(fnINT,StockTimeIntervalValues[j],[])) then
            continue;

          taFiles.Edit;
          taFiles[fnPATH]:=aFileList[i];
          taFiles.Post;
        end;
      end;
    end;
  finally
    aFileList.Free;
  end;

  if not b then
    MsgBox.MessageAttention(0,'There are no files in the folder, that conform naming template',[]);
end;

procedure TfrmStockDataConnectionWizardFiles_B.acInsertFromDirExecute(Sender: TObject);
var
  sSelDir  : String;
begin
  sSelDir:=BrowseForFolder(Handle,'Get files from folder',rlRecentlyDirs.MostRecentlyDef(AppPath),'');
  if sSelDir<>'' then
    LoadFromDir(sSelDir);
end;

procedure TfrmStockDataConnectionWizardFiles_B.OnBtnEditClick(Sender: TObject; var Text: string);
begin
  if not (taFiles.State in [dsEdit,dsInsert]) then
    taFiles.Edit;

  dlgFileOpen.Options:=dlgFileOpen.Options-[ofAllowMultiSelect];
  dlgFileOpen.FileName:=Text;

  if dlgFileOpen.FileName='' then
    dlgFileOpen.FileName:='*'+GetSelectedSymbol+'*';

  dlgFileOpen.InitialDir:=rlRecentlyDirs.MostRecentlyDef(AppPath);
  if dlgFileOpen.Execute then
  begin
    Text:=dlgFileOpen.FileName;
    rlRecentlyDirs.AddRecently(ExtractFileDir(dlgFileOpen.FileName));
  end;
end;

procedure TfrmStockDataConnectionWizardFiles_B.grSourcesEditorCreating(Sender: TObject; var InplaceEditor: TInplaceEditEx);
begin
  InplaceEditor:=TInplaceFileEditor.Create(self);
  TInplaceEditBtn(InplaceEditor).OnButtonClick:=OnBtnEditClick;
end;

procedure TfrmStockDataConnectionWizardFiles_B.grSourcesCanEdit(Sender: TObject; var CanEdit: Boolean);
begin
  CanEdit:=AnsiSameText(grSources.SelectedField.FieldName,fnPATH);
end;

procedure TfrmStockDataConnectionWizardFiles_B.rlRecentlyDirsRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: String);
begin
  LoadFromDir(RecentlyValue);
end;

function TfrmStockDataConnectionWizardFiles_B.GetSelectedSymbol: string;
begin
  result:=edInstrument.Text;
end;

procedure TfrmStockDataConnectionWizardFiles_B.SetSelectedSymbol(const aSymbol: string);
begin
  edInstrument.Text:=aSymbol;
end;

procedure TfrmStockDataConnectionWizardFiles_B.SetSingleMode;
begin
  FSingleMode:=true;
  Filter('',sti1); //sti1 - это не важно, лишь бы один источник
end;

procedure TfrmStockDataConnectionWizardFiles_B.taFilesINTGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
var
  I: TStockTimeInterval;
begin
  inherited;
  if DisplayText then
  begin
    if TStockDataUtils.GetTimeIntervalByValue(Sender.AsInteger,I) then
      Text:=StockTimeIntervalNames[i]+' ['+Sender.AsString+']'
    else
      Text:=Sender.AsString;
  end;
end;

end.
