unit FC.Trade.LiveDialog;
{$I Compiler.inc}

interface

uses
  BaseUtils, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, ExtendControls, ExtCtrls, ActnList,
  TeEngine, Series, TeeProcs, Chart, ComCtrls, Spin, ToolWin, JvComponentBase,
  JvCaptionButton, ImgList, DB, MemoryDS,
  Properties.Definitions,
  StockChart.Definitions,
  FC.fmUIDataStorage,
  FC.Definitions,
  FC.Singletons,
  FC.Trade.Statistics,
  FC.Trade.ResultPage, FC.Dialogs.DockedDialogCloseAndAppWindow_B, JvDockControlForm;


type
  TfmTradeLiveDialog = class(TfmDockedDialogCloseAndAppWindow_B,IStockBrokerEventHandler)
    pcPages: TPageControl;
    tsStart: TTabSheet;
    tsResults: TTabSheet;
    lvTraders: TExtendListView;
    Label1: TLabel;
    ActionList1: TActionList;
    acStartTrading: TAction;
    buStartTesting: TButton;
    paTrading: TPanel;
    buStop: TButton;
    acTraderProperties: TAction;
    Button1: TButton;
    acGetStatistic: TAction;
    acExportOrders: TAction;
    acCaptionButton: TAction;
    frmStockTradeResult: TfrmStockTradeResult;
    laProcess: TPanel;
    tmRefresh: TTimer;
    procedure buSeparateWindowClick(Sender: TObject);
    procedure acTraderPropertiesUpdate(Sender: TObject);
    procedure acTraderPropertiesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure buStopClick(Sender: TObject);
    procedure lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
    procedure buOKClick(Sender: TObject);
    procedure acStartTradingUpdate(Sender: TObject);
    procedure acStartTradingExecute(Sender: TObject);
  private
    FClosedOrderCount   : integer;
    FLastSortOrderColumn: integer;
    FTimeStep           : TTime;
    FCharts             : TInterfaceList; //of IStockChart
    FTraders            : TInterfaceList; //of IStockTrader

    function  CurrentTrader: IStockTrader;

    //from IStockBrokerEventHandler
    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewData(const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewOrder (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;

    function  GetChart(index: integer): IStockChart;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure RunTrading;
    procedure StopTrading;
    function  IsRunning: boolean;

    procedure AddTrader(aTrader: IStockTrader);

    procedure AddChart(aChart: IStockChart);
    function  ChartCount: integer;
    property  Charts[index:integer]: IStockChart read GetChart;

    constructor Create; reintroduce;
    destructor  Destroy; override;
  end;


implementation
  uses DateUtils, SystemService, Math,Application.Definitions,
  StockChart.Indicators.Properties, StockChart.Obj,
  ufmDialogOKCancel_B,FC.Trade.TesterTrainPropsDialog,FC.Trade.TesterTrainReportDialog;

{$R *.dfm}

{ TfmTradeLiveDialog }

procedure TfmTradeLiveDialog.acStartTradingExecute(Sender: TObject);
begin
  RunTrading;
end;

procedure TfmTradeLiveDialog.acStartTradingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=CurrentTrader<>nil;
end;

constructor TfmTradeLiveDialog.Create;
begin
  inherited Create(nil);
  TWaitCursor.SetUntilIdle;

  FTraders:=TInterfaceList.Create;
  FTimeStep:=EncodeTime(0,1,0,0); //1 min
  FCharts:=TInterfaceList.Create;
  frmStockTradeResult.Mode:=tmReal;

  pcPages.ActivePageIndex:=0;

  FLastSortOrderColumn:=-1;

  buSeparateWindow.Down:=true;
  buSeparateWindowClick(nil);

  //ssDoNormalCursor;
end;

destructor TfmTradeLiveDialog.Destroy;
begin
  lvTraders.Items.Clear;
  FreeAndNil(FCharts);
  FreeAndNil(FTraders);
  inherited;
end;

function TfmTradeLiveDialog.CurrentTrader: IStockTrader;
begin
  result:=nil;
  if lvTraders.Selected<>nil then
    result:=FTraders[integer(lvTraders.Selected.Data)] as IStockTrader;
end;

procedure TfmTradeLiveDialog.buOKClick(Sender: TObject);
begin
  inherited;
  buStopClick(nil);
  Close;
end;

procedure TfmTradeLiveDialog.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  Forms.Application.ProcessMessages;
  frmStockTradeResult.OnModifyOrder(aOrder,aModifyEventArgs);
end;

procedure TfmTradeLiveDialog.OnNewData(const aSender: IStockBroker; const aSymbol: string);
begin
  if AnsiSameText(aSymbol,CurrentTrader.GetProject.GetStockSymbol) then
  begin
    frmStockTradeResult.OnNewData(aSender,aSymbol);
    CurrentTrader.Update(aSender.GetCurrentTime);
  end;
end;

procedure TfmTradeLiveDialog.OnNewMessage(const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
  frmStockTradeResult.OnNewMessage(aSender,aOrder,aMessage);
end;

procedure TfmTradeLiveDialog.OnNewMessage(const aSender: IStockBroker; const aMessage: IStockBrokerMessage);
begin
  frmStockTradeResult.OnNewMessage(aSender,aMessage);
end;

procedure TfmTradeLiveDialog.OnNewOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
begin
  frmStockTradeResult.OnNewOrder(aOrder);
end;

procedure TfmTradeLiveDialog.OnStart(const aSender: IStockBroker);
begin

end;

procedure TfmTradeLiveDialog.lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FLastSortOrderColumn = Column.Index then
  begin
    FLastSortOrderColumn := -1;
    //lvOrders.SortColumn(Column,Sorts[Column.Index],true);
  end
  else
  begin
    FLastSortOrderColumn := Column.Index;
    //lvOrders.SortColumn(Column,Sorts[Column.Index],false);
  end;
end;

procedure TfmTradeLiveDialog.buStopClick(Sender: TObject);
begin
  StopTrading;
end;

procedure TfmTradeLiveDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if CanClose then
    StopTrading;
end;

procedure TfmTradeLiveDialog.AddTrader(aTrader: IStockTrader);
begin
  FTraders.Add(aTrader);
  lvTraders.AddItem(aTrader.GetName,TObject(FTraders.Count-1));

  if lvTraders.Items.Count=1 then
    lvTraders.Items[0].Selected:=true;
end;

procedure TfmTradeLiveDialog.AddChart(aChart: IStockChart);
var
  i,j: integer;
  aCharts : array of IStockChart;
begin
  //Вставляем так, чтобы были по порядку, сначала самые большие
  j:=0;
  for i:=0 to ChartCount-1 do
  begin
    if integer(Charts[i].StockSymbol.TimeInterval)<integer(aChart.StockSymbol.TimeInterval) then
      break;
    inc(j);
  end;

  FCharts.Insert(j,aChart);

  SetLength(aCharts,FCharts.Count);
  for j:=0 to FCharts.Count-1 do
   aCharts[j]:=FCharts[j] as IStockChart;
  frmStockTradeResult.Init(aCharts);
end;

procedure TfmTradeLiveDialog.acTraderPropertiesExecute(Sender: TObject);
begin
  CurrentTrader.ShowPropertyWindow;
end;

procedure TfmTradeLiveDialog.acTraderPropertiesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvTraders.Selected<>nil;

end;

function TfmTradeLiveDialog.GetChart(index: integer): IStockChart;
begin
  result:=FCharts[index] as IStockChart;
end;

function TfmTradeLiveDialog.ChartCount: integer;
begin
  result:=FCharts.Count;
end;

procedure TfmTradeLiveDialog.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if PopupMode=Forms.pmNone then
    Params.ExStyle:=Params.ExStyle or WS_EX_APPWINDOW
end;

procedure TfmTradeLiveDialog.RunTrading;
var
  aTrader : IStockTrader;
  aBroker : IStockBroker;
  aCharts : array of IStockChart;
  j: integer;
begin
  if StockBrokerConnectionRegistry.CurrentConnection=nil then
  begin
    MsgBox.MessageFailure(0,'Connection to broker is not set. Setup broker connection in Tools\Stock Brokers');
    exit;
  end;

  aTrader:=CurrentTrader;

  //aInputData:=FStockChart.GetInputData;
  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);

  aBroker:=StockBrokerConnectionRegistry.CurrentConnection.GetBroker;
  aTrader.SetBroker(aBroker);

  aTrader.Invalidate();

  SetLength(aCharts,FCharts.Count);
  for j:=0 to FCharts.Count-1 do
   aCharts[j]:=FCharts[j] as IStockChart;

  frmStockTradeResult.OnStart(aBroker,aTrader);
  frmStockTradeResult.Statictics.Balance:=aBroker.GetBalance;
  frmStockTradeResult.Statictics.StartDate:=Now;

  FLastSortOrderColumn:=-1;
  FClosedOrderCount:=0;
  tsStart.Enabled:=false;

  pcPages.ActivePage:=tsResults;
  paTrading.Visible:=true;
end;

procedure TfmTradeLiveDialog.StopTrading;
begin
  frmStockTradeResult.Statictics.StopDate:=Now;

  StockBrokerConnectionRegistry.RemoveBrokerEventHandler(self);

  tmRefresh.Enabled:=false;
  paTrading.Visible:=false;
  tsStart.Enabled:=true;
end;

procedure TfmTradeLiveDialog.buSeparateWindowClick(Sender: TObject);
begin
  if buSeparateWindow.Down then
    PopupMode:=Forms.pmNone
  else
    PopupMode:=pmAuto;
end;

function TfmTradeLiveDialog.IsRunning: boolean;
begin
  result:=paTrading.Visible;
end;

{ TStockBrokerEventHandler }

end.
