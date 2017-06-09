{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Вид, подчиняющийся документу "проект" и хранящий в себе фрейм чарта.
            Т.е. мост между моделью MVC и реальной визуализацией

 History:
-----------------------------------------------------------------------------}

unit FC.Documents.Project.fmChartView;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Application.Definitions, Documents.Definitions,Documents.Obj,Documents.Obj.View,
  StockChart.Definitions,
  FC.Definitions, FC.Factory, ufmForm_B, StdCtrls, ufrmFrame_B,
  FC.StockChart.frmStockChart, Menus, ToolWin, ActnMan, ActnList, ActnCtrls, ActnMenus, ExtCtrls;

type
  TfmChartView = class(TfmView)
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FChanged : boolean;
    FStockChart: IStockChart;
    FUpdateLocked: boolean;

    procedure WMStyleChanged(var Message: TMessage); message WM_STYLECHANGED;

    procedure OnRefreshChartTitle;
    function  GetStockSymbol: TStockSymbol;
  protected
    function  ComposeFileName: string;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure OnChanged;

    procedure SaveData;
    procedure LoadData;
    procedure OnSetDefaultPersistentPlacement; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(aDocument: TDocument; aStockChart:IStockChart); reintroduce;
    destructor Destroy; override;

    procedure OnDocumentNotify(const aNotifyType: string); override;
    //Обновить заголовок
    procedure ComposeCaption;

    property StockSymbol: TStockSymbol read GetStockSymbol;
    property Modified: boolean read FChanged;
    property StockChart: IStockChart read FStockChart;
  end;

implementation
  uses BaseUtils, IniFiles, Serialization, FC.fmUIDataStorage, FC.Documents.Project.Document;

resourcestring
  SCannotLoadData_s = 'Cannot load datafile %s';

{$R *.dfm}

{ TfmChartView }

constructor TfmChartView.Create(aDocument: TDocument; aStockChart:IStockChart);
begin
  inherited CreateDocumented(nil,aDocument);
  FStockChart:=aStockChart;

  FStockChart.Control.Parent:=self;
  FStockChart.Control.Align := alClient;
  FStockChart.Control.TabOrder := 0;
  FStockChart.Control.TabStop := True;

//  miLoadTheme.Action:=UIDataStorage.acChartLoadTheme;
//  miSaveTheme.Action:=UIDataStorage.acChartSaveTheme;


  try
    LoadData;
  except
    on E:Exception do
      Workspace.MainFrame.HandleException(E);
  end;

  ComposeCaption;
  OnRefreshChartTitle;
end;

procedure TfmChartView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (FormStyle<>fsMDIChild) and ([csLoading,csReading]*ComponentState=[])  then
    Params.ExStyle:=Params.ExStyle or WS_EX_APPWINDOW
end;

destructor TfmChartView.Destroy;
begin
  if FStockChart<>nil then
  begin
    if FStockChart.Control.Parent=self then
      FStockChart.Control.Parent:=nil;

    if FUpdateLocked then
    begin
      FUpdateLocked:=false;
      FStockChart.EndUpdate;
    end;
  end;
  inherited;
end;

procedure TfmChartView.DoClose(var Action: TCloseAction);
begin
  inherited;

  Assert(FUpdateLocked=false);
  FUpdateLocked:=true;
  FStockChart.BeginUpdate;

  Action:=caFree;
  SaveData;
end;

procedure TfmChartView.DoShow;
begin
  inherited;

  if FUpdateLocked then
  begin
    FUpdateLocked:=false;
    FStockChart.EndUpdate;
  end;
end;

procedure TfmChartView.ComposeCaption;
begin
  Caption:=StockSymbol.Name+ ' ['+StockTimeIntervalNames[StockSymbol.TimeInterval]+']';
end;

procedure TfmChartView.OnChanged;
begin
  FChanged:=true;
  ComposeCaption;
end;

procedure TfmChartView.OnDocumentNotify(const aNotifyType: string); 
begin
  inherited;
  if aNotifyType='AfterSave' then
    SaveData;
end;

function TfmChartView.ComposeFileName: string;
begin
  result:=TProjectDocument(Document).GetDSKPath;
end;

procedure TfmChartView.LoadData;
var
  s: string;
  aLeft,aTop,aWidth,aHeight: integer;
  aState  : integer;
  aFile: TIniFile;
  aSection : string;
begin
  s:=ComposeFileName;
  if s='' then
    exit;

  aSection:=StockSymbol.Name+' '+StockSymbol.GetTimeIntervalName;

  aFile:=TIniFile.Create(s);
  try
    try
      self.FormStyle:=TFormStyle(aFile.ReadInteger(aSection,'Style',integer(self.FormStyle)));

      aLeft:=aFile.ReadInteger(aSection,'Left',self.Left);
      aTop:=aFile.ReadInteger(aSection,'Top',self.Top);
      aWidth:=aFile.ReadInteger(aSection,'Width',self.Width);
      aHeight:=aFile.ReadInteger(aSection,'Height',self.Height);
      aState:=aFile.ReadInteger(aSection,'State',integer(wsNormal));

      if WindowState=wsNormal then //!!!! Временно, иначе сохраняется размер в
        self.SetBounds(aLeft,aTop,aWidth,aHeight);

      //окна в min режиме - это неправильно
      if aState=integer(wsMinimized) then
        aState:=integer(wsNormal);

      if TWindowState(aState) in [wsNormal,wsMinimized,wsMaximized] then
      begin
        if (FormStyle=fsMDIChild) then
        begin
          if IsMaximizedMDIChildren then
            aState:=integer(wsMaximized);
        end;
        WindowState:=TWindowState(aState);
      end;
    except
      on E:Exception do
        raise EBase.CreateFmt(SCannotLoadData_s,[s],E);
    end;
  finally
    aFile.Free;
  end;

  FChanged:=false;
  OnRefreshChartTitle;
end;

procedure TfmChartView.SaveData;
var
  s: string;
  aFile: TIniFile;
  aSection : string;
begin
  s:=ComposeFileName;
  if s='' then
    exit;

  aSection:=StockSymbol.Name+' '+StockSymbol.GetTimeIntervalName;

  aFile:=TIniFile.Create(s);
  try
    aFile.WriteInteger(aSection,'Left',self.Left);
    aFile.WriteInteger(aSection,'Top',self.Top);
    aFile.WriteInteger(aSection,'Width',self.Width);
    aFile.WriteInteger(aSection,'Height',self.Height);
    aFile.WriteInteger(aSection,'State',integer(self.WindowState));
    aFile.WriteInteger(aSection,'Style',integer(self.FormStyle));
  finally
    aFile.Free;
  end;

  FChanged:=false;
end;


procedure TfmChartView.FormActivate(Sender: TObject);
begin
  inherited;
  if (FStockChart.Control<>nil) and (FStockChart.Control.Parent=self) then
    (FStockChart.Control as TfrmStockChart).Activate;
end;

procedure TfmChartView.FormDeactivate(Sender: TObject);
begin
  inherited;
  if (FStockChart.Control<>nil) and (FStockChart.Control.Parent=self) then
    (FStockChart.Control as TfrmStockChart).Deactivate;
end;

procedure TfmChartView.OnSetDefaultPersistentPlacement;
begin
  PersistentPlacement:=fppNone;
end;

function TfmChartView.GetStockSymbol: TStockSymbol;
begin
  result:=FStockChart.StockSymbol;
end;


procedure TfmChartView.WMStyleChanged(var Message: TMessage);
begin
  inherited;
  OnRefreshChartTitle;
end;

procedure TfmChartView.OnRefreshChartTitle;
begin
  if FStockChart<>nil then
    FStockChart.ShowSymbolTitle((GetWindowLong(Handle,GWL_STYLE) and WS_BORDER)=0);
end;


procedure TfmChartView.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  aCE: IControlExtension;
begin
  if (FStockChart.Control.Owner<>self) and Supports(FStockChart.Control,IControlExtension,aCE) then
  begin
    Handled:=aCE.IsShortCut(Msg);
  end;
end;

end.



