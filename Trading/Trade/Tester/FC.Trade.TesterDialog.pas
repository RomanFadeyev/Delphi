unit FC.Trade.TesterDialog;
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
  Menus, ActnPopup, PlatformDefaultStyleActnCtrls;

type

  TfmTradeTesterDialog = class(TfmDockedDialogCloseAndAppWindow_B,IStockBrokerEventHandler)
    lvTraders: TExtendListView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edSpread: TExtendSpinEdit;
    edSlippage: TExtendSpinEdit;
    edStartFromDate: TExtendDateTimePicker;
    edStartFromTime: TExtendDateTimePicker;
    ActionList1: TActionList;
    acStartTesting: TAction;
    buStartTesting: TButton;
    pbTesting: TProgressBar;
    Label5: TLabel;
    cbInitialDeposit: TExtendComboBox;
    paTesting: TPanel;
    buStop: TButton;
    acTraderProperties: TAction;
    Button1: TButton;
    acGetStatistic: TAction;
    acExportOrders: TAction;
    acCaptionButton: TAction;
    buStartTraining: TButton;
    acStartTraining: TAction;
    Label6: TLabel;
    edStopAtDate: TExtendDateTimePicker;
    edStopAtTime: TExtendDateTimePicker;
    frmStockTradeResult: TfrmStockTradeResult;
    cbGettingPriceType: TExtendComboBox;
    Label7: TLabel;
    ckUpdateCharts: TExtendCheckBox;
    ckPauseAfterOpening: TExtendCheckBox;
    ckPauseAfterClosing: TExtendCheckBox;
    buResumeTesting: TButton;
    pcPages: TPageControl;
    tsStart: TTabSheet;
    tsResults: TTabSheet;
    paWorkspace: TPanel;
    ckUpdateOverOldBars: TExtendCheckBox;
    ckShiftBars: TExtendCheckBox;
    ScrollBox1: TScrollBox;
    ckGenerateFullBarsOnly: TExtendCheckBox;
    edStopLevel: TExtendSpinEdit;
    Label8: TLabel;
    Bevel1: TBevel;
    ckEmulateTicks: TExtendCheckBox;
    Bevel2: TBevel;
    buFindStartDate: TSpeedButton;
    buFindStopDate: TSpeedButton;
    buShowRecentlyStartDates: TSpeedButton;
    buShowRecentlyStopDates: TSpeedButton;
    rlStartDates: TRecentlyList;
    rlStopDates: TRecentlyList;
    pmStartDates: TPopupActionBar;
    pmStopDate: TPopupActionBar;
    procedure buShowRecentlyStopDatesClick(Sender: TObject);
    procedure rlStopDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
    procedure rlStartDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
    procedure buShowRecentlyStartDatesClick(Sender: TObject);
    procedure buFindStopDateClick(Sender: TObject);
    procedure buFindStartDateClick(Sender: TObject);
    procedure ckGenerateFullBarsOnlyClick(Sender: TObject);
    procedure ckEmulateTicksClick(Sender: TObject);
    procedure buResumeTestingClick(Sender: TObject);
    procedure acStartTrainingExecute(Sender: TObject);
    procedure acTraderPropertiesUpdate(Sender: TObject);
    procedure acTraderPropertiesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure buStopClick(Sender: TObject);
    procedure lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
    procedure buOKClick(Sender: TObject);
    procedure acStartTestingUpdate(Sender: TObject);
    procedure acStartTestingExecute(Sender: TObject);
  private
    FClosedOrderCount   : integer;
    FLastSortOrderColumn: integer;
    FStopped            : boolean;
    FTraders            : TInterfaceList; //of IStockTrader
    FCharts             : TStockTimeIntervalChartArray;
    FInputDatas         : TStockTimeIntervalInputDataCollectionArray;
    FBroker             : FC.Trade.Brokers.Stub.Broker.TStockBroker;
    FRunning            : boolean;
    FPaused             : boolean;
    FClosing            : boolean;
    FLastRepaintTime    : TDateTime;

    function  CurrentTrader: IStockTrader;
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
  protected
    procedure RunTraining(aTrainingProperties : TInterfaceList);

    procedure AddTrader(aTrader: IStockTrader);
  public
    function CloseQuery: Boolean; override;

    //Запуск тестирования
    procedure RunTesting;

    property  IsRunning: boolean read FRunning;

    procedure SaveData(aStream: TStream);
    procedure LoadData(aStream: TStream);

    constructor Create(const aProject: IStockProject); reintroduce;
    destructor  Destroy; override;
  end;

implementation
  uses DateUtils, SystemService, Math,Application.Definitions, Serialization,
  StockChart.Indicators.Properties, StockChart.Obj,FC.DataUtils,FC.Singletons,
  ufmDialogOKCancel_B,FC.Trade.TesterTrainPropsDialog,FC.Trade.TesterTrainReportDialog,
  ufmForm_B;

{$R *.dfm}

type
  TTesterDialogSerialize = class (TSerialize)
  protected
    function HeaderSignature: string; override;
    function HeaderVersion: string; override;
  end;

  //Прокси для сериализации документа
  TTesterDialogSerializationProxy = class (TNameValuePersistentObject)
  private
    FDialog: TfmTradeTesterDialog;
  public
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;

    procedure Save(aDialog: TfmTradeTesterDialog; aStream: TStream);
    procedure Load(aDialog: TfmTradeTesterDialog; aStream: TStream);
  end;

{ TTesterDialogSerialize }

function TTesterDialogSerialize.HeaderVersion: string;
begin
  result:='1.0.0';
end;

function TTesterDialogSerialize.HeaderSignature: string;
begin
  result:='TesterDialog';
end;


{ TfmTradeTesterDialog }

procedure TfmTradeTesterDialog.acStartTestingExecute(Sender: TObject);
begin
  rlStartDates.AddRecently(DateTimeToStr(Trunc(edStartFromDate.DateTime)+Frac(edStartFromTime.DateTime)));
  rlStopDates.AddRecently(DateTimeToStr(Trunc(edStopAtDate.DateTime)+Frac(edStopAtTime.DateTime)));

  RunTesting;
end;

procedure TfmTradeTesterDialog.acStartTestingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=CurrentTrader<>nil;
  if (ckEmulateTicks.Checked and ckGenerateFullBarsOnly.Checked) then
    ckGenerateFullBarsOnly.Checked:=false;
  cbGettingPriceType.Enabled:=not ckEmulateTicks.Checked;
end;

constructor TfmTradeTesterDialog.Create(const aProject: IStockProject);
var
  aGpt:TBrokerGettingPriceType;
  aInterval: TStockTimeInterval;
  i: integer;
  aListItem: TListItem;
  aSymbols: TStockSymbolInfoArray;
begin
  inherited Create(nil);
  TWaitCursor.SetUntilIdle;

  FTraders:=TInterfaceList.Create;

  FBroker := FC.Trade.Brokers.Stub.Broker.TStockBroker.Create;
  FBroker.AddEventHandler(StockBrokerConnectionRegistry);

  IInterface(FBroker)._AddRef;

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

  frmStockTradeResult.Init(FCharts);

  for i:=0 to aProject.TraderCount-1 do
    AddTrader(aProject.GetTrader(i));

  pcPages.ActivePageIndex:=0;
  //pcPages.DoubleBuffered:=true; С включенной опцией Дельфи плохо рисует в темах

  FLastSortOrderColumn:=-1;

  cbInitialDeposit.Items.AddObject('1000 $',pointer(1000));
  cbInitialDeposit.Items.AddObject('3000 $',pointer(3000));
  cbInitialDeposit.Items.AddObject('10,000 $',pointer(10000));
  cbInitialDeposit.Items.AddObject('100,000 $',pointer(100000));
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
    edSlippage.Value:=Workspace.Storage(self).ReadInteger(self,'Slippage',2);
    frmStockTradeResult.AutoScrollOrders:=Workspace.Storage(self).ReadBoolean(self,'AutoScrollOrders',true);
    cbInitialDeposit.ItemIndex:=Workspace.Storage(self).ReadInteger(self,'InitialDeposit',0);
  except
    on E:Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;

  aSymbols:=StockDataStorage.GetSymbols;
  for i := 0 to High(aSymbols) do
  begin
    if aSymbols[i].Name=aProject.GetStockSymbol then
    begin
      edSpread.Value:=aSymbols[i].Spread;
      edStopLevel.Value:=aSymbols[i].Spread;
      break;
    end;
  end;


  try
    aListItem:=lvTraders.FindCaption(0,Workspace.Storage(self).ReadString(self,'SelectedTrader',''),false,true,false);
    if aListItem<>nil then
    begin
      aListItem.Selected:=true;
      aListItem.Focused:=true;
    end;
  except
    on E:Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;

  RegisterPersistValue(ckPauseAfterOpening,false);
  RegisterPersistValue(ckPauseAfterClosing,false);
  RegisterPersistValue(ckUpdateCharts,false);
  RegisterPersistValue(ckUpdateOverOldBars,false);
  RegisterPersistValue(ckGenerateFullBarsOnly,false);
  RegisterPersistValue(ckEmulateTicks,false);

  buFindStartDateClick(nil);
  buFindStopDateClick(nil);

  rlStartDates.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyStartDates';
  rlStopDates.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyStopDates';  

  //ssDoNormalCursor;
end;

destructor TfmTradeTesterDialog.Destroy;
var
  aInterval:TStockTimeInterval;
begin
  //if IsRunning then
  //  raise EStockError.Create('Tester dialog is still running');

  Workspace.Storage(self).WriteInteger(self,'GettingPriceMethod',cbGettingPriceType.ItemIndex);
  Workspace.Storage(self).WriteInteger(self,'Spread',edSpread.Value);
  Workspace.Storage(self).WriteInteger(self,'StopLevel',edStopLevel.Value);
  Workspace.Storage(self).WriteInteger(self,'Slippage',edSlippage.Value);
  Workspace.Storage(self).WriteBoolean(self,'AutoScrollOrders',frmStockTradeResult.AutoScrollOrders);
  Workspace.Storage(self).WriteInteger(self,'InitialDeposit',cbInitialDeposit.ItemIndex);
  if lvTraders.Selected<>nil then
     Workspace.Storage(self).WriteString(self,'SelectedTrader',lvTraders.Selected.Caption)
  else
     Workspace.Storage(self).WriteString(self,'SelectedTrader','');

  lvTraders.Items.Clear;
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    FInputDatas[aInterval]:=nil;
    FCharts[aInterval]:=nil;
  end;

  FreeAndNil(FTraders);

  FBroker.RemoveEventHandler(StockBrokerConnectionRegistry);
  IInterface(FBroker)._Release;
  FBroker := nil;

  inherited;
end;

procedure TfmTradeTesterDialog.LoadData(aStream: TStream);
var
  aProxy: TTesterDialogSerializationProxy;
begin
  aProxy:=TTesterDialogSerializationProxy.Create;
  try
    aProxy.Load(self,aStream);
  finally
    aProxy.Free;
  end;
end;

procedure TfmTradeTesterDialog.SaveData(aStream: TStream);
var
  aProxy: TTesterDialogSerializationProxy;
begin
  aProxy:=TTesterDialogSerializationProxy.Create;
  try
    aProxy.Save(self,aStream);
  finally
    aProxy.Free;
  end;
end;

function TfmTradeTesterDialog.CurrentTrader: IStockTrader;
begin
  result:=nil;
  if lvTraders.Selected<>nil then
    result:=FTraders[integer(lvTraders.Selected.Data)] as IStockTrader;
end;

procedure TfmTradeTesterDialog.buFindStopDateClick(Sender: TObject);
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

procedure TfmTradeTesterDialog.buFindStartDateClick(Sender: TObject);
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

procedure TfmTradeTesterDialog.buOKClick(Sender: TObject);
begin
  buStopClick(nil);
  FClosing:=true;
  if not FRunning then
    PostMessage(Handle,WM_CLOSE,0,0);
end;

procedure TfmTradeTesterDialog.OnNewOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
begin
  frmStockTradeResult.OnNewOrder(aOrder);
end;

procedure TfmTradeTesterDialog.OnStart(const aSender: IStockBroker);
begin

end;

procedure TfmTradeTesterDialog.rlStartDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
begin
  edStartFromDate.DateTime:=StrToDateTime(RecentlyValue);
  edStartFromTime.DateTime:=StrToDateTime(RecentlyValue)
end;

procedure TfmTradeTesterDialog.rlStopDatesRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
begin
  edStopAtDate.DateTime:=StrToDateTime(RecentlyValue);
  edStopAtTime.DateTime:=StrToDateTime(RecentlyValue)
end;

procedure TfmTradeTesterDialog.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  frmStockTradeResult.OnModifyOrder(aOrder,aModifyEventArgs);

  if aModifyEventArgs.ModifyType=omtOpen then
  begin
    Forms.Application.ProcessMessages;

    if ckPauseAfterOpening.Checked then
    begin
      FPaused:=true;
      buResumeTesting.Visible:=true;
      try
        while FPaused and not FStopped do
          Forms.Application.ProcessMessages;
      finally
        buResumeTesting.Visible:=false;
      end;
    end
  end

  else if aModifyEventArgs.ModifyType=omtClose then
  begin
    Forms.Application.ProcessMessages;

    if ckPauseAfterClosing.Checked then
    begin
      FPaused:=true;
      buResumeTesting.Visible:=true;
      try
        while FPaused and not FStopped  do
          Forms.Application.ProcessMessages;
      finally
        buResumeTesting.Visible:=false;
      end;
    end;
  end;
end;

procedure TfmTradeTesterDialog.OnNewData(const aSender: IStockBroker; const aSymbol: string);
begin
  frmStockTradeResult.OnNewData(aSender,aSymbol);
end;

procedure TfmTradeTesterDialog.OnNewMessage(const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
  frmStockTradeResult.OnNewMessage(aSender,aOrder,aMessage);
end;

procedure TfmTradeTesterDialog.OnNewMessage(const aSender: IStockBroker; const aMessage: IStockBrokerMessage);
begin
  frmStockTradeResult.OnNewMessage(aSender,aMessage);
end;

function TfmTradeTesterDialog.InitialDeposit: integer;
begin
  result:=integer(cbInitialDeposit.Items.Objects[cbInitialDeposit.ItemIndex]);
end;

function TfmTradeTesterDialog.CurrentSpreadPoints: integer;
begin
  result:=edSpread.Value;
end;

function TfmTradeTesterDialog.CurrentStopLevelPoints: integer;
begin
  result:=edStopLevel.Value;
end;

procedure TfmTradeTesterDialog.lvOrdersColumnClick(Sender: TObject; Column: TListColumn);
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

procedure TfmTradeTesterDialog.buShowRecentlyStartDatesClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmStartDates.Popup(P.x, P.y);
end;

procedure TfmTradeTesterDialog.buShowRecentlyStopDatesClick(Sender: TObject);
var
  P: TPoint;
begin
  P:=TControl(Sender).ClientToScreen(Point(0,TControl(Sender).Height));
  pmStopDate.Popup(P.x, P.y);
end;

procedure TfmTradeTesterDialog.buStopClick(Sender: TObject);
begin
  FStopped:=true;
end;

procedure TfmTradeTesterDialog.ckEmulateTicksClick(Sender: TObject);
begin
  inherited;
  if ckEmulateTicks.Checked then
    ckGenerateFullBarsOnly.Checked:=false;
end;

procedure TfmTradeTesterDialog.ckGenerateFullBarsOnlyClick(Sender: TObject);
begin
  inherited;
  if ckGenerateFullBarsOnly.Checked then
    ckEmulateTicks.Checked:=false;
end;

procedure TfmTradeTesterDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if CanClose then
    buStopClick(nil);
end;

procedure TfmTradeTesterDialog.AddTrader(aTrader: IStockTrader);
begin
  FTraders.Add(aTrader);
  lvTraders.AddItem(aTrader.GetName,TObject(FTraders.Count-1));

  if lvTraders.Items.Count=1 then
    lvTraders.Items[0].Selected:=true;
end;

procedure TfmTradeTesterDialog.acTraderPropertiesExecute(Sender: TObject);
begin
  CurrentTrader.ShowPropertyWindow;
end;

procedure TfmTradeTesterDialog.acTraderPropertiesUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=lvTraders.Selected<>nil;
end;

procedure TfmTradeTesterDialog.RunTesting;
var
  aTrader : IStockTrader;
  aPercent: integer;
  aOldPercent: integer;
  aStart,aStop: TDateTime;
  aEmulateTickInMinute: boolean;
  aPriorDate: TDateTime;

  aNewInputDatas: array [TStockTimeInterval]  of TSCInputDataCollection;
  aInterval: TStockTimeInterval;

  k,l: integer;
  aRefPointMins,aShift: integer;

  aGenerators: array [TStockTimeInterval] of TStockBarBackwardGenerator;
  aGenerator : TStockBarBackwardGenerator;
  aBarData   : TStockBarData;
  aNeedUpdate: array [TStockTimeInterval] of boolean;
//  aFirstMinuteIndex: integer;
  aCurrentMinuteIndex: integer;
  aCurrentTimeAligned: TDateTime;
  i: Integer;
begin
  inherited;

  //aInputData:=FStockChart.GetInputData;
  aTrader:=CurrentTrader;
  aTrader.Invalidate;
  FBroker.SetParams(StockDataStorage.GetSymbolInfo(FCharts[sti1].StockSymbol.Name),nil,0,0,false);
  aTrader.SetBroker(FBroker);
  FBroker.Reset;
  FBroker.SetDeposit(InitialDeposit);

  FBroker.GettingPriceType:=TBrokerGettingPriceType(cbGettingPriceType.Items.Objects[cbGettingPriceType.ItemIndex]);

  aTrader.Invalidate();
  aStart:=Int(edStartFromDate.Date)+Frac(edStartFromTime.Time);
  aStop:=Int(edStopAtDate.Date)+Frac(edStopAtTime.Time);

  l:=FInputDatas[sti1].FindBestMatched(aStart,0,FInputDatas[sti1].Count-1);
  if l=-1 then
    raise Exception.Create('Cannot find M1 data near to '+DateTimeToStr(aStart));

  aStart:=FInputDatas[sti1].DirectGetItem_DataDateTime(l);

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

    //Для минуток копируем
    if aInterval=sti1 then
    begin
      l:=TStockDataUtils.FindBar(FInputDatas[aInterval],aStart,aInterval);
      Assert(l<>-1);
      aNewInputDatas[aInterval].Assign(FInputDatas[aInterval],0,l-1);
    end
    //Для всех остальных - сторим на основании скопированных минуток
    else begin
      TStockDataUtils.GenerateBars(aNewInputDatas[sti1],aNewInputDatas[aInterval],aInterval,0);
    end;

    FCharts[aInterval].SetInputData(aNewInputDatas[aInterval]);

    if aInterval=sti1 then
      aGenerators[aInterval]:=nil
    else
      aGenerators[aInterval]:=TStockBarBackwardGenerator.Create(aNewInputDatas[sti1],aInterval);
  end;

  frmStockTradeResult.OnStart(FBroker,aTrader);
  frmStockTradeResult.Statictics.Balance:=InitialDeposit;
  frmStockTradeResult.Statictics.StartDate:=aStart;
  frmStockTradeResult.Statictics.StopDate:=aStop;


  FLastSortOrderColumn:=-1;
  FClosedOrderCount:=0;

  FStopped:=false;

  pcPages.ActivePage:=tsResults;

  pbTesting.Position:=0;
  pbTesting.Max:=100;
  paTesting.Visible:=true;
  aOldPercent:=0;

  FRunning:=true;
  tsStart.Enabled:=false;
  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);

  FBroker.SetParams(StockDataStorage.GetSymbolInfo(FCharts[sti1].StockSymbol.Name),FInputDatas[sti1],aStart,aStop,aEmulateTickInMinute);
  FBroker.SetSpread(CurrentSpreadPoints);
  FBroker.SetStopLevel(CurrentStopLevelPoints);
  FBroker.SetDeposit(InitialDeposit);

  //aFirstMinuteIndex:=aNewInputDatas[sti1].Count;
  try
    FBroker.StartTicking;
    aCurrentMinuteIndex:=aNewInputDatas[sti1].Count-1;

    while FBroker.Tick do
    begin
      if FStopped then
        break;

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
        aGenerator:=aGenerators[aInterval];

        if ckShiftBars.Checked then
        begin
          aRefPointMins:=Round(Frac(aCurrentTimeAligned)*MinsPerDay);
          aShift:=aRefPointMins mod StockTimeIntervalValues[aInterval];
          TStockDataUtils.GenerateBars(FInputDatas[sti1],aNewInputDatas[aInterval],aInterval,aShift,0,aCurrentMinuteIndex);
        end
        else begin
          aGenerator.GenerateBar(aCurrentMinuteIndex);

          if not ckGenerateFullBarsOnly.Checked or aGenerator.IsNextNewBar then
          begin
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
                      aGenerator.DestLeftBound,
                      aBarData.Open,
                      aBarData.High,
                      aBarData.Low,
                      aBarData.Close,
                      aBarData.Volume)

            end;
          end;

        end;
      end;

      FBroker.AfterTick;
      aTrader.Update(FBroker.CurrentTime);

      //Обновляем содержимое экрана
      if ckUpdateCharts.Checked then
      begin
        for aInterval:=Succ(sti1) to high(TStockTimeInterval) do
          if aNeedUpdate[aInterval] then
            FCharts[aInterval].Repaint;
      end;

      //Обновляем прогресс
      aPercent:=Round((FBroker.CurrentTime-aStart)*100/(aStop-aStart));
      if aPercent>aOldPercent then
      begin
        pbTesting.Position:=aPercent;
        aOldPercent:=aPercent;
      end;

      //пропускаем сообщения
      if (DateTimeToTimeStamp(Now-FLastRepaintTime).Time>5000) then
      begin
        Forms.Application.ProcessMessages;
        FLastRepaintTime:=Now;
      end;

      if FClosing then
        break;
    end;

    for i := FBroker.GetOpenedOrders.Count-1 downto 0 do
      FBroker.GetOpenedOrders[i].Close('END SESSION');
  finally
    FRunning:=false;
    tsStart.Enabled:=true;
    frmStockTradeResult.OnEnd;
    paTesting.Visible:=false;
    StockBrokerConnectionRegistry.RemoveBrokerEventHandler(self);
    aTrader.SetBroker(nil);

    for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      //Возвращаем InputData
      FCharts[aInterval].SetInputData(FInputDatas[aInterval]);
      aGenerators[aInterval].Free;
    end;
  end;

  if FClosing then
    PostMessage(Handle,WM_CLOSE,0,0);
end;

procedure TfmTradeTesterDialog.RunTraining(aTrainingProperties : TInterfaceList); //of ISCIndicatorProperty);
begin
   TfmTesterTrainReportDialog.Run(self, aTrainingProperties);
end;

procedure TfmTradeTesterDialog.acStartTrainingExecute(Sender: TObject);
var
  fmProps: TfmTesterTrainPropsDialog;
  i,j: integer;
  aExpert: ISCExpert;
  aProperties : IPropertyCollection;
  aTrainingProperties : TInterfaceList;
begin
  fmProps:=TfmTesterTrainPropsDialog.Create(nil);
  aTrainingProperties := TInterfaceList.Create;
  try
    for i:=0 to CurrentTrader.ExpertCount-1 do
    begin
      aExpert:=CurrentTrader.GetExpert(i);
      aProperties:=aExpert.GetProperties;
      for j:=0 to aProperties.Count-1 do
        if Supports(aProperties.Items[j],ISCIndicatorPropertyTrainee) then
          fmProps.AddProperty(aExpert,aProperties.Items[j]);
    end;

    if fmProps.ShowModal=mrOk then
    begin
      for i:=0 to CurrentTrader.ExpertCount-1 do
      begin
        aExpert:=CurrentTrader.GetExpert(i);
        aProperties:=aExpert.GetProperties;
        for j:=0 to aProperties.Count-1 do
          if fmProps.Checked[aProperties.Items[j]] then
            aTrainingProperties.Add(aProperties.Items[j]);
      end;

      RunTraining(aTrainingProperties);
    end;
  finally
    FreeAndNil(fmProps);
    FreeAndNil(aTrainingProperties);
  end;
end;

procedure TfmTradeTesterDialog.buResumeTestingClick(Sender: TObject);
begin
  inherited;
  FPaused:=false;
end;

function TfmTradeTesterDialog.CloseQuery: Boolean;
begin
  result:=inherited CloseQuery;

  if FRunning then
  begin
    result:=false;
    FClosing:=true;
  end
end;


{ TTesterDialogSerializationProxy }

procedure TTesterDialogSerializationProxy.OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean);
begin
  inherited;
  if aName='OrderHistory' then
  begin
    FDialog.frmStockTradeResult.LoadData(aReader);
    aHandled:=true;
  end;
end;

procedure TTesterDialogSerializationProxy.OnWriteValues(const aWriter: INameValueDataWriter);
begin
  inherited;
  aWriter.DataWriter.WriteString('OrderHistory');
  FDialog.frmStockTradeResult.SaveData(aWriter.DataWriter);
end;

procedure TTesterDialogSerializationProxy.Load(aDialog: TfmTradeTesterDialog; aStream: TStream);
var
  aSerializer: TTesterDialogSerialize;
  aObject    : IPersistentObject;
begin
  FDialog:=aDialog;

  aObject:=self;
  aSerializer:=TTesterDialogSerialize.Create;
  try
    aSerializer.LoadObject(aStream,aObject);
  finally
    aObject:=nil;
    aSerializer.Free;
  end;
end;

procedure TTesterDialogSerializationProxy.Save(aDialog: TfmTradeTesterDialog; aStream: TStream);
var
  aSerializer: TTesterDialogSerialize;
begin
  FDialog:=aDialog;

  aSerializer:=TTesterDialogSerialize.Create;
  try
    aSerializer.SaveObject(self,aStream);
  finally
    aSerializer.Free;
  end;
end;

end.
