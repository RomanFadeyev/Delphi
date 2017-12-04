unit FC.Trade.TrainerDialog;
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
  FC.Trade.Statistics,
  FC.Trade.Brokers.Stub.Broker,
  FC.Trade.ResultPage, XPMan, FC.Dialogs.DockedDialogCloseAndAppWindow_B, JvDockControlForm, Buttons, RecentlyList,
  Menus, ActnPopup, Mask, Grids, DBGrids, MultiSelectDBGrid, ColumnSortDBGrid,
  EditDBGrid, FC.Trade.TerminalFrame,FC.Trade.Trader.Manual, PlatformDefaultStyleActnCtrls;

type
  ITrainerTraderID = interface
  ['{D3FC3836-59B9-4B60-8A98-F28D63CB92A2}']
  end;

  TStockTraderTrainer = class (FC.Trade.Trader.Manual.TStockTraderManual);

  TfmTradeTrainerDialog = class(TfmDockedDialogCloseAndAppWindow_B,IStockBrokerEventHandler)
    Label4: TLabel;
    edStartFromDate: TExtendDateTimePicker;
    edStartFromTime: TExtendDateTimePicker;
    acActions: TActionList;
    acStartTesting: TAction;
    buStartTesting: TButton;
    Label5: TLabel;
    cbInitialDeposit: TExtendComboBox;
    acGetStatistic: TAction;
    acExportOrders: TAction;
    acCaptionButton: TAction;
    Label6: TLabel;
    edStopAtDate: TExtendDateTimePicker;
    edStopAtTime: TExtendDateTimePicker;
    pcPages: TPageControl;
    tsStart: TTabSheet;
    tsResults: TTabSheet;
    paWorkspace: TPanel;
    paStart: TScrollBox;
    ckEmulateTicks: TExtendCheckBox;
    buFindStartDate: TSpeedButton;
    buFindStopDate: TSpeedButton;
    buShowRecentlyStartDates: TSpeedButton;
    buShowRecentlyStopDates: TSpeedButton;
    rlStartDates: TRecentlyList;
    rlStopDates: TRecentlyList;
    pmStartDates: TPopupActionBar;
    pmStopDate: TPopupActionBar;
    buPause: TButton;
    buResumeTesting: TButton;
    Label8: TLabel;
    edStopLevel: TExtendSpinEdit;
    Label2: TLabel;
    edSpread: TExtendSpinEdit;
    Label7: TLabel;
    cbGettingPriceType: TExtendComboBox;
    Label1: TLabel;
    edSkipTo: TExtendMinutePicker;
    buDropSkipTo: TSpeedButton;
    pmSkipTo: TPopupActionBar;
    N14301: TMenuItem;
    N15001: TMenuItem;
    N16001: TMenuItem;
    N8001: TMenuItem;
    N10001: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N23001: TMenuItem;
    buSkipToApply: TSpeedButton;
    buSkip1Min: TButton;
    Bevel1: TBevel;
    buSkip5Min: TButton;
    buSkip15Min: TButton;
    buSkip60Min: TButton;
    acPause: TAction;
    acResume: TAction;
    tbSpeed: TTrackBar;
    Bevel2: TBevel;
    acBuy: TAction;
    acSell: TAction;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    frmTerminal: TfrmTerminalFrame;
    acIncSpeed: TAction;
    acDecSpeed: TAction;
    Button4: TButton;
    paTesting: TPanel;
    pbTesting: TProgressBar;
    buStop: TButton;
    laTime: TLabel;
    procedure frmTerminalacCloseOrderExecute(Sender: TObject);
    procedure frmTerminalacModifyOrderExecute(Sender: TObject);
    procedure acDecSpeedExecute(Sender: TObject);
    procedure acIncSpeedExecute(Sender: TObject);
    procedure acSellExecute(Sender: TObject);
    procedure acBuyExecute(Sender: TObject);
    procedure acBuyUpdate(Sender: TObject);
    procedure buSkipToApplyClick(Sender: TObject);
    procedure buSkip1MinClick(Sender: TObject);
    procedure acResumeUpdate(Sender: TObject);
    procedure OnSkipToClick(Sender: TObject);
    procedure acPauseUpdate(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure buDropSkipToClick(Sender: TObject);
    procedure buStopClick(Sender: TObject);
    procedure buShowRecentlyStopDatesClick(Sender: TObject);
    procedure rlStopDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
    procedure rlStartDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
    procedure buShowRecentlyStartDatesClick(Sender: TObject);
    procedure buFindStopDateClick(Sender: TObject);
    procedure buFindStartDateClick(Sender: TObject);
    procedure buResumeTestingClick(Sender: TObject);
    procedure lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
    procedure buOKClick(Sender: TObject);
    procedure acStartTestingUpdate(Sender: TObject);
    procedure acStartTestingExecute(Sender: TObject);
  private
    FClosedOrderCount   : integer;
    FLastSortOrderColumn: integer;
    FStopped            : boolean;
    FCharts             : TStockTimeIntervalChartArray;
    FAuxCharts          : TStockChartArray;
    FInputDatas         : TStockTimeIntervalInputDataCollectionArray;
    FBroker             : FC.Trade.Brokers.Stub.Broker.TStockBroker;
    FRunning            : boolean;
    FPaused             : boolean;
    FClosing            : boolean;
    FLastRepaintTime    : TDateTime;
    FPauseAt            : TDateTime;
    FTrader             : TStockTraderTrainer;

    procedure EnableProcessPage(aEnable: boolean);
    procedure EnableStartPage(aEnable: boolean);
    procedure MakeUpdateActions;

    function  InitialDeposit: integer;
    function  CurrentSpreadPoints: integer;
    function  CurrentStopLevelPoints: integer;

    //from IStockBrokerEventHandler
    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewOrder   (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewData    (const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;
  public
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); override;  
    function IsShortCut(var Message: TWMKey): Boolean; override;
    function CloseQuery: Boolean; override;

    //Запуск тестирования
    procedure RunTesting;

    property  IsRunning: boolean read FRunning;

    constructor Create(const aProject: IStockProject); reintroduce;
    destructor  Destroy; override;
  end;

implementation
  uses DateUtils, SystemService, Math,Application.Definitions, Serialization,
  StockChart.Indicators.Properties,
  StockChart.Obj,FC.DataUtils,FC.Singletons,
  StockChart.Definitions.Drawing,
  FC.StockData.InputDataCollectionLimitor,
  FC.Trade.Trainer.NewOrderDialog,
  FC.Trade.Trainer.ModifyOrderDialog,
  FC.Trade.Trainer.CloseOrderDialog, FC.Dialogs.DockedDialogAndAppWindow_B;

{$R *.dfm}

procedure ssDoNormalCursor;
begin
  Screen.Cursor := crDefault;
end;

procedure ssDoWaitCursor;
begin
  Screen.Cursor := crHourGlass;
end;

{ TfmTradeTrainerDialog }

constructor TfmTradeTrainerDialog.Create(const aProject: IStockProject);
var
  aGpt:TBrokerGettingPriceType;
  aInterval: TStockTimeInterval;
begin
  inherited Create(nil);
  TWaitCursor.SetUntilIdle;
  EnableProcessPage(false);


  FBroker := FC.Trade.Brokers.Stub.Broker.TStockBroker.Create;
  FBroker.RealTime:=true;

  FBroker.AddEventHandler(StockBrokerConnectionRegistry);
  IInterface(FBroker)._AddRef;

  FTrader:=TStockTraderTrainer.Create;
  FTrader.FID:=ITrainerTraderID;
  FTrader.SetProject(aProject);
  IInterface(FTrader)._AddRef;


  for aGpt:=Low(TraderGetPriceTypeNames) to High(TraderGetPriceTypeNames) do
  begin
    cbGettingPriceType.AddItem(TraderGetPriceTypeNames[aGpt],TObject(aGpt));
    if aGpt=gptPessimistic then
      cbGettingPriceType.ItemIndex:=cbGettingPriceType.Items.Count-1;
  end;

  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    FInputDatas[aInterval]:=aProject.GetStockChart(aInterval).GetInputData;
    FCharts[aInterval]:=aProject.GetStockChart(aInterval);
  end;

  FAuxCharts:=aProject.GetAuxStockCharts;

  pcPages.ActivePageIndex:=0;
  //pcPages.DoubleBuffered:=true; С включенной опцией Дельфи плохо рисует в темах

  FLastSortOrderColumn:=-1;

  cbInitialDeposit.Items.AddObject('1000 $',pointer(1000));
  cbInitialDeposit.Items.AddObject('10,000 $',pointer(10000));
  cbInitialDeposit.ItemIndex:=0;

  try
    cbGettingPriceType.ItemIndex:= Workspace.Storage(self).ReadInteger(self,'GettingPriceMethod',0);
  except
    on E:Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;

  try
    edSpread.Value:=Workspace.Storage(self).ReadInteger(self,'Spread',3);
    edStopLevel.Value:=Workspace.Storage(self).ReadInteger(self,'StopLevel',3);
//    edSlippage.Value:=Workspace.Storage(self).ReadInteger(self,'Slippage',2);
    tbSpeed.Position:=Workspace.Storage(self).ReadInteger(self,'Speed',tbSpeed.Max);
  except
    on E:Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;

  RegisterPersistValue(ckEmulateTicks,false);

  buFindStartDateClick(nil);
  buFindStopDateClick(nil);

  rlStartDates.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyStartDates';
  rlStopDates.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyStopDates';

  frmTerminal.Init(FCharts);
  frmTerminal.AutoScrollOrders:=true;

  //ssDoNormalCursor;
end;

destructor TfmTradeTrainerDialog.Destroy;
begin
  FStopped:=true;
  FRunning:=false;
  //if IsRunning then
  //  raise EStockError.Create('Trainer dialog is still running');

  Workspace.Storage(self).WriteInteger(self,'GettingPriceMethod',cbGettingPriceType.ItemIndex);
  Workspace.Storage(self).WriteInteger(self,'Spread',edSpread.Value);
  Workspace.Storage(self).WriteInteger(self,'StopLevel',edStopLevel.Value);
  Workspace.Storage(self).WriteInteger(self,'Speed',tbSpeed.Position);
//  Workspace.Storage(self).WriteInteger(self,'Slippage',edSlippage.Value);

  FBroker.RemoveEventHandler(StockBrokerConnectionRegistry);
  IInterface(FBroker)._Release;

  FTrader.SetBroker(nil);
  FTrader.SetProject(nil);
  FTrader.Dispose;
  IInterface(FTrader)._Release;
  FTrader := nil;

  FBroker := nil;

  inherited;
end;

procedure TfmTradeTrainerDialog.Dock(NewDockSite: TWinControl; ARect: TRect);
begin
  inherited;

  if not (csDestroying in ComponentState) then
  begin
    bvBottom.Visible:=not IsDocked;
    buOK.Visible:=not IsDocked;
  end;
end;

procedure TfmTradeTrainerDialog.acBuyExecute(Sender: TObject);
var
  aDlg:TfmTrainerNewOrderDialog;
begin
  if not IsDocked then
    Visible:=false;
  aDlg:=TfmTrainerNewOrderDialog.Create(FTrader,okBuy);
  try
    if aDlg.ShowModal=mrOk then
     ;
  finally
    aDlg.Free;
    if not IsDocked then
      Visible:=true;
  end;
end;

procedure TfmTradeTrainerDialog.acBuyUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=FRunning;
end;

procedure TfmTradeTrainerDialog.acDecSpeedExecute(Sender: TObject);
begin
  inherited;
  tbSpeed.Position:=max(tbSpeed.Position-1,tbSpeed.Min);
end;

procedure TfmTradeTrainerDialog.acIncSpeedExecute(Sender: TObject);
begin
  inherited;
  tbSpeed.Position:=min(tbSpeed.Position+1,tbSpeed.Max);
end;

procedure TfmTradeTrainerDialog.acPauseExecute(Sender: TObject);
begin
  FPaused:=true;
  ssDoNormalCursor;
  while FPaused and not FStopped and not FClosing do
  begin
    Forms.Application.ProcessMessages;
    Forms.Application.DoApplicationIdle;
  end;
end;

procedure TfmTradeTrainerDialog.acPauseUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=FRunning and not FPaused;
  buSkip1Min.Enabled:=FRunning and FPaused;
  buSkip5Min.Enabled:=FRunning and FPaused;
  buSkip15Min.Enabled:=FRunning and FPaused;
  buSkip60Min.Enabled:=FRunning and FPaused;
end;

procedure TfmTradeTrainerDialog.acResumeUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=FRunning and FPaused;
end;

procedure TfmTradeTrainerDialog.acSellExecute(Sender: TObject);
var
  aDlg:TfmTrainerNewOrderDialog;
begin
  if not IsDocked then
    Visible:=false;

  aDlg:=TfmTrainerNewOrderDialog.Create(FTrader,okSell);
  try
    if aDlg.ShowModal=mrOk then
     ;
  finally
    aDlg.Free;
    if not IsDocked then
      Visible:=true;
  end;
end;

procedure TfmTradeTrainerDialog.acStartTestingExecute(Sender: TObject);
begin
  rlStartDates.AddRecently(DateTimeToStr(Trunc(edStartFromDate.DateTime)+Frac(edStartFromTime.DateTime)));
  rlStopDates.AddRecently(DateTimeToStr(Trunc(edStopAtDate.DateTime)+Frac(edStopAtTime.DateTime)));

  RunTesting;
end;

procedure TfmTradeTrainerDialog.acStartTestingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=not FRunning;
end;

procedure TfmTradeTrainerDialog.EnableProcessPage(aEnable: boolean);
begin
//  tsResults.Enabled:=aEnable;
//  tbSpeed.Enabled:=aEnable;
  edSkipTo.Enabled:=aEnable;
end;

procedure TfmTradeTrainerDialog.EnableStartPage(aEnable: boolean);
var
  i: Integer;
begin
  tsStart.Enabled:=aEnable;
  for i := 0 to paStart.ControlCount - 1 do
    if paStart.Controls[i]<>buStartTesting then
      paStart.Controls[i].Enabled:=aEnable;
end;

procedure TfmTradeTrainerDialog.buFindStopDateClick(Sender: TObject);
var
  aStart,aStop: TDateTime;
  aTickSearcher: TStockTickSearcher;
begin
  TWaitCursor.SetUntilIdle;
  aTickSearcher:=TStockTickSearcher.Create(FInputDatas);
  try
    aTickSearcher.FindTerminalReliableDates(aStart,aStop);
  finally
    aTickSearcher.Free;
  end;
  edStopAtDate.Date:=Int(aStop);
  edStopAtTime.Time:=Frac(aStop);
end;

procedure TfmTradeTrainerDialog.buDropSkipToClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmSkipTo.Popup(P.x, P.y);
end;

procedure TfmTradeTrainerDialog.buFindStartDateClick(Sender: TObject);
var
  aStart,aStop: TDateTime;
  aTickSearcher: TStockTickSearcher;
begin
  TWaitCursor.SetUntilIdle;
  aTickSearcher:=TStockTickSearcher.Create(FInputDatas);
  try
    aTickSearcher.FindTerminalReliableDates(aStart,aStop);
  finally
    aTickSearcher.Free;
  end;
  edStartFromDate.Date:=Int(aStart);
  edStartFromTime.Time:=Frac(aStart);
end;

procedure TfmTradeTrainerDialog.buOKClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfmTradeTrainerDialog.rlStartDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
begin
  edStartFromDate.DateTime:=StrToDateTime(RecentlyValue);
  edStartFromTime.DateTime:=StrToDateTime(RecentlyValue)
end;

procedure TfmTradeTrainerDialog.rlStopDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
begin
  edStopAtDate.DateTime:=StrToDateTime(RecentlyValue);
  edStopAtTime.DateTime:=StrToDateTime(RecentlyValue)
end;

function TfmTradeTrainerDialog.InitialDeposit: integer;
begin
  result:=integer(cbInitialDeposit.Items.Objects[cbInitialDeposit.ItemIndex]);
end;

function TfmTradeTrainerDialog.IsShortCut(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
begin
  result:=inherited IsShortCut(Message);

  if not result then
  begin
    ShiftState := KeyDataToShiftState(Message.KeyData);
    case Message.CharCode of
      VK_ADD:
        if ssCtrl in ShiftState then
          acIncSpeed.Execute;
      VK_SUBTRACT:
        if ssCtrl in ShiftState then
          acDecSpeed.Execute;
      VK_MULTIPLY:
        if ssCtrl in ShiftState then
          if FPaused then
            acResume.Execute
          else
            acPause.Execute;
      VK_RETURN:
        if ssCtrl in ShiftState then
          if FPaused then
            acResume.Execute
          else
            acPause.Execute;
    end;
  end;
end;

function TfmTradeTrainerDialog.CurrentSpreadPoints: integer;
begin
  result:=edSpread.Value;
end;

function TfmTradeTrainerDialog.CurrentStopLevelPoints: integer;
begin
  result:=edStopLevel.Value;
end;

procedure TfmTradeTrainerDialog.lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
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

procedure TfmTradeTrainerDialog.MakeUpdateActions;
var
  I: Integer;
  aForm: TCustomForm;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    aForm:=Screen.CustomForms[I];
    if aForm.HandleAllocated and IsWindowVisible(aForm.Handle) and IsWindowEnabled(aForm.Handle) then
      TfmTradeTrainerDialog(aForm).UpdateActions;
  end;
end;

procedure TfmTradeTrainerDialog.OnSkipToClick(Sender: TObject);
var
  aMinutes: Integer;
begin
  aMinutes:=TMenuItem(Sender).Tag;
  edSkipTo.TotalMinutes:=aMinutes;
  buSkipToApply.Click;
end;

procedure TfmTradeTrainerDialog.buShowRecentlyStartDatesClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmStartDates.Popup(P.x, P.y);
end;

procedure TfmTradeTrainerDialog.buShowRecentlyStopDatesClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmStopDate.Popup(P.x, P.y);
end;

procedure TfmTradeTrainerDialog.buSkip1MinClick(Sender: TObject);
var
  aLeft,aRight: int64;
begin
  FPauseAt:=FBroker.GetCurrentTime+TComponent(Sender).Tag/1440;
  TStockDataUtils.AlignTime(FPauseAt,TComponent(Sender).Tag,aLeft,aRight);
  FPauseAt:=TStockDataUtils.RefineTime(aLeft/MinsPerDay);
  acResume.Execute;
end;

procedure TfmTradeTrainerDialog.buSkipToApplyClick(Sender: TObject);
begin
  if CompareTime(edSkipTo.Time,FBroker.GetCurrentTime)<=0 then
    FPauseAt:=Trunc(FBroker.GetCurrentTime)+Frac(edSkipTo.Time)+1
  else
    FPauseAt:=Trunc(FBroker.GetCurrentTime)+Frac(edSkipTo.Time);

  //Суббота
  if DayOfTheWeek(FPauseAt)=6 then
  begin
    FPauseAt:=FPauseAt+2; //Переносим на понедельник
  end;

  ssDoWaitCursor;
  acResume.Execute;
end;

procedure TfmTradeTrainerDialog.buStopClick(Sender: TObject);
begin
  if FRunning then
    if MsgBox.Confirm(Handle, 'Are you sure you want to stop the coach?',[]) then
    begin
      FRunning:=false;
    end;

  if not FRunning then
  begin
    FStopped:=true;
    FPaused:=false;
  end;
end;

procedure TfmTradeTrainerDialog.frmTerminalacCloseOrderExecute(Sender: TObject);
var
  aOrder: IStockOrder;
  aDlg:TfmTrainerCloseOrderDialog;
begin
  aOrder:=frmTerminal.SelectedOrder;
  if aOrder=nil then
    exit;

  if not (aOrder.GetState in [osOpened,osPending]) then
    exit;

  if aOrder.GetState=osPending then
  begin
    if MsgBox.Confirm(0,'Revoke pending order?',[]) then
      aOrder.RevokePending;
    exit;
  end;


  if not IsDocked then
    Visible:=false;
  aDlg:=TfmTrainerCloseOrderDialog.Create(FTrader,aOrder);
  try
    if aDlg.ShowModal=mrOk then
     ;
  finally
    aDlg.Free;
    if not IsDocked then
      Visible:=true;
  end;
end;

procedure TfmTradeTrainerDialog.frmTerminalacModifyOrderExecute(Sender: TObject);
var
  aDlg:TfmTrainerModifyOrderDialog;
  aOrder: IStockOrder;
begin
  aOrder:=frmTerminal.SelectedOrder;
  if aOrder=nil then
    exit;

  if not (aOrder.GetState in [osOpened,osPending]) then
    exit;

  if not IsDocked then
    Visible:=false;
  aDlg:=TfmTrainerModifyOrderDialog.Create(FTrader,aOrder);
  try
    if aDlg.ShowModal=mrOk then
     ;
  finally
    aDlg.Free;
    if not IsDocked then
      Visible:=true;
  end;
end;

procedure TfmTradeTrainerDialog.RunTesting;
var
  aPercent: integer;
  aOldPercent: integer;
  aStart,aStop: TDateTime;
  aEmulateTickInMinute: boolean;
  aPriorDate: TDateTime;

  aIData: ISCInputDataCollection;
  aNewInputDatas: array [TStockTimeInterval]  of TSCInputDataCollection;
  aNewAuxInputDatas: array of IStockInputDataCollectionLimitor;
  aAuxInputDatas: array of ISCInputDataCollection;
  aInterval: TStockTimeInterval;

  k,l: integer;

  aGenerators: array [TStockTimeInterval] of TStockBarBackwardGenerator;
  aGenerator : TStockBarBackwardGenerator;
  aBarData   : TStockBarData;
  aNeedUpdate: array [TStockTimeInterval] of boolean;
//  aFirstMinuteIndex: integer;
  aCurrentMinuteIndex: integer;
  aCurrentTimeAligned: TDateTime;
  aSkipping : boolean;
  i: Integer;
begin
  inherited;

  //aInputData:=FStockChart.GetInputData;
  FBroker.Reset;

  FBroker.GettingPriceType:=TBrokerGettingPriceType(cbGettingPriceType.Items.Objects[cbGettingPriceType.ItemIndex]);

  aStart:=Int(edStartFromDate.Date)+Frac(edStartFromTime.Time);
  aStop:=Int(edStopAtDate.Date)+Frac(edStopAtTime.Time);

  if (aStop-aStart)>10*366 then
  begin
    MsgBox.MessageFailure(0,'Time range is too large. Reduce it to 10 years');
    exit;
  end;

  if (aStop<aStart) then
  begin
    MsgBox.MessageFailure(0,'Start time is larger than stop time. Correct time.');
    exit;
  end;

  if (aStop-aStart)<1 then
  begin
    MsgBox.MessageFailure(0,'Time range is too small. Enhance it to 1 day');
    exit;
  end;

  aEmulateTickInMinute:=ckEmulateTicks.Checked;

  //Подменяем InputData
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    Assert(FCharts[aInterval].GetInputData=FInputDatas[aInterval]);
    aNewInputDatas[aInterval]:=TSCInputDataCollection.Create(nil,StockTimeIntervalValues[aInterval]/MinsPerDay);
    //if ckUpdateOverOldBars.Checked then
    //begin
    //  aNewInputDatas[aInterval].Assign(FInputDatas[aInterval]);
    //end
    //else
    begin
      l:=TStockDataUtils.FindBar(FInputDatas[aInterval],aStart,aInterval);
      if l<>-1 then
        aNewInputDatas[aInterval].Assign(FInputDatas[aInterval],0,l-1);
    end;
    FCharts[aInterval].SetInputData(aNewInputDatas[aInterval]);
    if aInterval<>sti1 then
    begin
      if aEmulateTickInMinute then
        aGenerators[aInterval]:=TStockBarBackwardGenerator.Create(aNewInputDatas[sti1],aInterval)
      else //Оставлено от греха, но по идее, любой генератор должен работать по NewData
        aGenerators[aInterval]:=TStockBarBackwardGenerator.Create(FInputDatas[sti1],aInterval)
    end
    else
      aGenerators[aInterval]:=nil;
  end;

  SetLength(aAuxInputDatas,Length(FAuxCharts));
  SetLength(aNewAuxInputDatas,Length(FAuxCharts));
  for i := 0 to High(FAuxCharts) do
  begin
    aAuxInputDatas[i]:=FAuxCharts[i].GetInputData;
    aNewAuxInputDatas[i]:=TStockInputDataCollectionLimitor.Create(FAuxCharts[i].GetInputData,nil);
    FAuxCharts[i].SetInputData(aNewAuxInputDatas[i] as ISCInputDataCollection);
    aNewAuxInputDatas[i].Limit(0);
  end;


  FLastSortOrderColumn:=-1;
  FClosedOrderCount:=0;
  FPauseAt:=0;
  pbTesting.Position:=0;
  pbTesting.Max:=100;
  paTesting.Visible:=true;
  aOldPercent:=0;

  FRunning:=true;
  FPaused:=false;
  FStopped:=false;


  //for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  //  FCharts[aInterval].OnTraderInvalidate(FTrader);

  FTrader.Invalidate;
  FTrader.SetBroker(FBroker);
  frmTerminal.OnStart(FBroker, FTrader);

  EnableProcessPage(FRunning);
  EnableStartPage(false);
  UpdateActions;
  pcPages.ActivePage:=tsResults;
  UpdateActions;

//  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);
  FBroker.SetParams(StockDataStorage.GetSymbolInfo(FCharts[sti1].StockSymbol.Name),FInputDatas[sti1],aStart,aStop,aEmulateTickInMinute);
  FBroker.SetSpread(CurrentSpreadPoints);
  FBroker.SetStopLevel(CurrentStopLevelPoints);
  FBroker.SetDeposit(InitialDeposit);

  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);
  //aFirstMinuteIndex:=aNewInputDatas[sti1].Count;
  try
    FBroker.StartTicking;
    aCurrentMinuteIndex:=aNewInputDatas[sti1].Count-1;

    while FBroker.Tick do
    begin
      if FStopped then
        break;

      //В режиме пропуска?
      aSkipping:=(FPauseAt<>0) and (CompareDateTime(FBroker.GetCurrentTime,FPauseAt)<0);

      aCurrentTimeAligned:=TStockDataUtils.AlignTimeToLeft(FBroker.CurrentTime,sti1);

      //Если не в режиме тиков внутри минутки, то просто добавляем минутку
      if not aEmulateTickInMinute then
      begin
        inc(aCurrentMinuteIndex);
        //Минутку добавляем
        aNewInputDatas[sti1].AddItem(
           FInputDatas[sti1].DirectGetItem_DataDateTime(aCurrentMinuteIndex),
           FInputDatas[sti1].DirectGetItem_DataOpen(aCurrentMinuteIndex),
           FInputDatas[sti1].DirectGetItem_DataHigh(aCurrentMinuteIndex),
           FInputDatas[sti1].DirectGetItem_DataLow(aCurrentMinuteIndex),
           FInputDatas[sti1].DirectGetItem_DataClose(aCurrentMinuteIndex),
           FInputDatas[sti1].DirectGetItem_DataVolume(aCurrentMinuteIndex));
      end
      //Если эмуляция тиков внутри минутки
      else begin
        //Новый бар
        if (aCurrentMinuteIndex<0) or (aNewInputDatas[sti1].DirectGetItem_DataDateTime(aCurrentMinuteIndex)<>aCurrentTimeAligned) then
        begin
          inc(aCurrentMinuteIndex);
          //Минутку добавляем
          aNewInputDatas[sti1].AddItem(
             aCurrentTimeAligned,
             FBroker.CurrentBidPrice,
             FBroker.CurrentBidPrice,
             FBroker.CurrentBidPrice,
             FBroker.CurrentBidPrice,
             FInputDatas[sti1].DirectGetItem_DataVolume(aCurrentMinuteIndex));//!!!!!!
        end
        //Старый бар, надо модифицировать
        else begin
            aNewInputDatas[sti1].ModifyItem(aCurrentMinuteIndex,
               aNewInputDatas[sti1].DirectGetItem_DataOpen(aCurrentMinuteIndex),
               max(aNewInputDatas[sti1].DirectGetItem_DataHigh(aCurrentMinuteIndex),FBroker.CurrentBidPrice),
               min(aNewInputDatas[sti1].DirectGetItem_DataLow(aCurrentMinuteIndex),FBroker.CurrentBidPrice),
               FBroker.CurrentBidPrice,
               FInputDatas[sti1].DirectGetItem_DataVolume(aCurrentMinuteIndex));
        end;
      end;

      //Поиск по дате должен точно выводить нас на CurrentMinuteIndex
      Assert(aNewInputDatas[sti1].FindExactMatched(FBroker.CurrentTime)=aCurrentMinuteIndex);

      //Эмулируем тики на разных таймфреймах
      for aInterval:=Succ(sti1) to high(TStockTimeInterval) do
      begin
        aNeedUpdate[aInterval]:=false;
        aIData:=FInputDatas[aInterval];
        aGenerator:=aGenerators[aInterval];

        aGenerator.GenerateBar(aCurrentMinuteIndex);

        aBarData:=aGenerator.BarData;
        aNeedUpdate[aInterval]:=true;

        aPriorDate:=0;
        k:=aNewInputDatas[aInterval].Count;

        if (k>0) then
          aPriorDate:=aNewInputDatas[aInterval].DirectGetItem_DataDateTime(k-1);

        //Модифицируем старый бар
        if aPriorDate=aGenerator.DestLeftBound then
        begin
          Assert(k>0);
          aNewInputDatas[aInterval].ModifyItem(k-1,
                  aBarData.Open,
                  aBarData.High,
                  aBarData.Low,
                  aBarData.Close,
                  aBarData.Volume)
        end
        //Новый бар - добавляем
        else begin
          Assert(aPriorDate<aGenerator.DestLeftBound);
          aNewInputDatas[aInterval].AddItem(
                  aIData.DirectGetItem_DataDateTime(k),
                  aBarData.Open,
                  aBarData.High,
                  aBarData.Low,
                  aBarData.Close,
                  aBarData.Volume)

        end;
      end;

      //Не останавливаемся до тех пор, пока не дойдем до указанной отметки
      if aSkipping then
        continue;

      for i := 0 to High(aNewAuxInputDatas) do
      begin
        l:=aNewAuxInputDatas[i].GetDataSource.FindExactMatched(FBroker.GetCurrentTime);
        if l<>-1 then
          aNewAuxInputDatas[i].Limit(l);
      end;

      FBroker.AfterTick; //не генерируем AfterTick, если aSkipping, так быстрее

      if FClosing then
        break;

      laTime.Caption:=DefaultFormatter.DateTimeToStr(FBroker.CurrentTime,false,true,false);

      //Обновляем прогресс
      aPercent:=Round((FBroker.CurrentTime-aStart)*100/(aStop-aStart));
      if aPercent>aOldPercent then
      begin
        pbTesting.Position:=aPercent;
        aOldPercent:=aPercent;
      end;

      if FPauseAt<>0 then
      begin
        FPauseAt:=0;
        acPause.Execute;
      end;

      if FClosing then
        break;
      
      //пропускаем сообщения
      if (DateTimeToTimeStamp(Now-FLastRepaintTime).Time>100) then
      begin
        Forms.Application.ProcessMessages;
        MakeUpdateActions;
        ssDoNormalCursor; //Вроде как Idle прошло
        FLastRepaintTime:=Now;
      end;

      if FClosing then
        break;

      //Принудительный тормоз
      l:=tbSpeed.Max-tbSpeed.Position;
      if l>0 then
        Sleep(l*l);
    end;
    
  finally
    FRunning:=false;
    EnableProcessPage(FRunning);
    EnableStartPage(true);
    paTesting.Visible:=false;
    StockBrokerConnectionRegistry.RemoveBrokerEventHandler(self);
    FTrader.SetBroker(nil);
    frmTerminal.OnEnd;

    for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      //Возвращаем InputData
      FCharts[aInterval].SetInputData(FInputDatas[aInterval]);
      aGenerators[aInterval].Free;
    end;

    for i := 0 to High(FAuxCharts) do
    begin
      FAuxCharts[i].SetInputData(aAuxInputDatas[i]);
      aNewAuxInputDatas[i].Limit(-1);
      aNewAuxInputDatas[i]:=nil;
    end;
  end;

  if FClosing then
    PostMessage(Handle,WM_CLOSE,0,0);
end;


procedure TfmTradeTrainerDialog.buResumeTestingClick(Sender: TObject);
begin
  inherited;
  FPaused:=false;
//  UpdateActions;
end;

function TfmTradeTrainerDialog.CloseQuery: Boolean;
begin
  result:=inherited CloseQuery;

  if result and FRunning then
  begin
    buStopClick(nil);
    result:=false; //Всгда отменяем!

    //Если buStopClick() остановил выполнение, то ставим флаг "закрыться"
    if not FRunning then
      FClosing:=true;
  end;
end;

procedure TfmTradeTrainerDialog.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder;  const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  frmTerminal.OnModifyOrder(aOrder,aModifyEventArgs);
end;

procedure TfmTradeTrainerDialog.OnNewData(const aSender: IStockBroker; const aSymbol: string);
begin
  frmTerminal.OnNewData(aSender,aSymbol);
end;

procedure TfmTradeTrainerDialog.OnNewMessage(const aSender: IStockBroker;
  const aMessage: IStockBrokerMessage);
begin
  frmTerminal.OnNewMessage(aSender,aMessage);
end;

procedure TfmTradeTrainerDialog.OnNewMessage(const aSender: IStockBroker;
  const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
  frmTerminal.OnNewMessage(aSender,aOrder,aMessage);
end;

procedure TfmTradeTrainerDialog.OnNewOrder(const aSender: IStockBroker;
  const aOrder: IStockOrder);
begin
  frmTerminal.OnNewOrder(aOrder);
end;

procedure TfmTradeTrainerDialog.OnStart(const aSender: IStockBroker);
begin
  frmTerminal.OnStart(aSender,nil);
end;

end.
