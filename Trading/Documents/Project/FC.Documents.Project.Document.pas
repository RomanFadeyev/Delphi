{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Документ "проект"

 History:
-----------------------------------------------------------------------------}

unit FC.Documents.Project.Document;
{$I Compiler.inc}

interface
  uses Classes, SysUtils, Controls, ActnList, Contnrs,Windows, Forms,
       Serialization,
       Collections.Map,
       Documents.Definitions, Documents.Obj, ufmForm_B,
       StockChart.Definitions,
       StockChart.Definitions.Units,
       FC.StockData.DataSourceToInputDataCollectionMediator,
       FC.fmUIDataStorage,
       FC.Definitions,
       FC.Factory,
       FC.Documents.Project.fmChartView,
       FC.Trade.TesterDialog,
       FC.Trade.LiveDialog,
       FC.Trade.TrainerDialog;

type
  TChartViewArray = array of TfmChartView;

  //Собственно проект
  TProjectDocument = class(Documents.Obj.TDocument,IStockProject,IStockBrokerEventHandler)
  private type
    TViewMap = class (TMap<string,TfmChartView>)
    public
      constructor Create;
    end;
    TViewMapIterator = TMapIterator<string,TfmChartView>;
  private
    FCharts           : array [TStockTimeInterval] of IStockChart;
    FAuxCharts        : TInterfaceList; //of IStockChart
    FStockChartViewMap: TViewMap;
    FStockSymbolName  : string;
    FTraders          : TInterfaceList; //of IStockTrader
    FAlerters         : TInterfaceList; //of IStockAlerter
    FTradeTesterDialog: TfmTradeTesterDialog;
    FTradeTesterData  : TStream;
    FTradeLiveDialog  : TfmTradeLiveDialog;
    FTradeTrainerDialog: TfmTradeTrainerDialog;
    FChartSyncPosition: boolean;
    FChartStickToEnd  : boolean;
    FSyncLock         : integer;
    FEventHandlers: TInterfaceList;
    FViewChartActions : TObjectList;
    FID               : TGUID;
    function GetEventHandler(index: integer): IStockProjectEventHandler; //of IStockProjectEventHandler
    function GetEventHandlerListCopy: IInterfaceList;

    procedure OnViewStockChartExecute(aAction:TObject);
    procedure OnViewStockChartUpdate(aAction:TObject);

    procedure OnFileSaveProject(aAction:TCustomAction);
    procedure OnFileCloseProject(aAction:TCustomAction);
    procedure OnTradingTradeTester(aAction:TCustomAction);
    procedure OnTradingTradeTesterUpdate(aAction:TCustomAction);
    procedure OnTradingTradeLive(aAction:TCustomAction);
    procedure OnTradingTradeLiveUpdate(aAction:TCustomAction);
    procedure OnTradingTradeTrainer(aAction:TCustomAction);
    procedure OnTradingTradeTrainerUpdate(aAction:TCustomAction);


    procedure OnChartSyncPosition(aAction:TCustomAction);
    procedure OnChartSyncPositionUpdate(aAction:TCustomAction);

    procedure OnChartGotoEnd(aAction:TCustomAction);
    procedure OnChartGotoEndUpdate(aAction:TCustomAction);

    procedure OnChartStickToEnd(aAction:TCustomAction);
    procedure OnChartStickToEndUpdate(aAction:TCustomAction);

    procedure OnChartLockUpdating(aAction:TCustomAction);

    procedure OnTradingTraderList(aAction:TCustomAction);
    procedure OnTradingTraderListUpdate(aAction:TCustomAction);

    procedure OnTradingAlerterList(aAction:TCustomAction);
    procedure OnTradingAlerterListUpdate(aAction:TCustomAction);

    procedure OnTradingAddOrderInfoToCharts(aAction:TCustomAction);
    procedure OnTradingImportOrdersToCharts(aAction:TCustomAction);

    procedure OnProjectRefreshDataSource(aAction:TCustomAction);
    procedure OnProjectChangeDataSource(aAction:TCustomAction);
    procedure OnProjectAuxSymbols(aAction:TCustomAction);

    procedure OnMergeIndicatorsExecute(aAction:TCustomAction);

    procedure OnCloseTradeTesterDialog(Sender: TObject; var Action: TCloseAction);
    procedure OnCloseTradeLiveDialog(Sender: TObject; var Action: TCloseAction);
    procedure OnCloseTradeTrainerDialog(Sender: TObject; var Action: TCloseAction);

    procedure SaveDSK;
    procedure LoadDSK;

    procedure SetDataSourceToChart(aDS: IStockDataSource; const aChart: IStockChart);
    procedure SetInputDataToChart(const aInputData: IStockDataSourceToInputDataCollectionMediator; const aChart: IStockChart);

    procedure OnAddTrader(aTrader: IStockTrader);
    procedure OnRemoveTrader(aTrader: IStockTrader);

    procedure OnAddAlerter(aAlerter: IStockAlerter);
    procedure OnRemoveAlerter(aAlerter: IStockAlerter);

    procedure RaiseCloseStockChart(const aStockChart: IStockChart);
    procedure RaiseCloseSelf;
    procedure CloseAllDialogs;
    procedure CreateMenuActions;

    function  GetStockChartID(const aChart: IStockChart): string;
  protected
    procedure OpenStockChartView(const aID: string);
    function  FindStockChartViewOpened(const aID: string):TfmChartView;

    procedure OnNew; override;
    procedure OnOpen(aStream:TStream); override;
    procedure OnSave(aStream: TStream); override;
    procedure OnClose; override;
    function  OnCloseQuery: boolean; override;

    procedure OnRemoveView(aView: TDocView); override;
    procedure OnAddView(aView: TDocView); override;
    procedure OnChartPositionChange(const aChart: IStockChart);

    //from IStockBrokerEventHandler
    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewOrder (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewData   (const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;

    procedure OnOpenOrder (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnCloseOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnChangeOrderStopLoss(const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnChangeOrderTakeProfit(const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnChangeOrderTrailingStop(const aSender: IStockBroker; const aOrder: IStockOrder);


    function  CreateEmptyStockChart: IStockChart;
  public
    function  AddAuxSymbol: boolean;
    function  GetAuxStockChartCount: integer;
    function  GetAuxStockChart(index: integer):IStockChart;
    function  IndexOfAuxStockChart(const aChart: IStockChart): integer;
    procedure DeleteAuxStockChart(index: integer);

    function GetGenuineStockChartInputData(const aChart: IStockChart): IStockDataSourceToInputDataCollectionMediator;

    //from IStockProject
    function  GetStockChart(aInterval:TStockTimeInterval) : IStockChart;
    //Только вспомогательные
    function  GetAuxStockCharts:TStockChartArray;
    //Все загруженные (главные и вспомогательные)
    function  GetAllStockCharts:TStockChartArray;
    //Найти среди всех загруженных (главных и вспомогательных)
    function  FindStockChart(const aSymbol: string; aInterval: TStockTimeInterval): IStockChart;

    procedure HilightOnCharts(const aD1,aD2: TDateTime; aCenterStart: boolean); overload;
    procedure HilightOnCharts(const aD1,aD2: TDateTime; aCenterStart: boolean; aExceptFor: IStockChart);overload;

    function  GetTrader(index: integer) : IStockTrader;
    function  IndexOfTrader(const aTrader:IStockTrader): integer;
    procedure AddTrader(const aTrader:IStockTrader);
    function  RemoveTrader(const aTrader:IStockTrader): integer;
    function  TraderCount: integer;

    function  GetAlerter(index: integer) : IStockAlerter;
    procedure AddAlerter(const aAlerter:IStockAlerter);
    function  IndexOfAlerter(const aAlerter:IStockAlerter): integer;
    function  RemoveAlerter(const aAlerter:IStockAlerter): integer;
    function  AlerterCount: integer;

    function  GetStockSymbol: string;

    procedure ReplayTicks(const aFrom,aTo: TDateTime; aClearOldValues: boolean);
    procedure ShiftTicks(const aReferencePoint:TDateTime);

    procedure AddEventHandler(const aHandler: IStockProjectEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockProjectEventHandler);
    //end of IStockProject

    constructor Create(aDocTemplate: TDocTemplate); override;
    destructor Destroy; override;
    procedure  Dispose;

    property  Traders[index:integer]: IStockTrader read GetTrader;
    property  Alerters[index:integer]: IStockAlerter read GetAlerter;

    function  EventHandlerCount: integer;
    property  EventHandlers[index:integer]: IStockProjectEventHandler read GetEventHandler;

    function GetOpenedViews: TChartViewArray;

    function GetModified: boolean; override;
    function GetIID: TGUID; override;

    function GetDSKPath: string;
  end;

  //Прокси для сериализации документа
  TProjectDocumentSerializationProxy = class (TNameValuePersistentObject)
  private
    FDocument: TProjectDocument;
  protected
    procedure OnDefineValues; override;
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Save(aDocument: TProjectDocument; aStream: TStream);
    procedure Load(aDocument: TProjectDocument; aStream: TStream);
  end;

  //Обработчик событий от трейдера
  TTraderEventHandler = class (TInterfacedObject,IStockTraderEventHandler)
  private
    FOwner: TProjectDocument;
  public
    constructor Create(aOwner: TProjectDocument);

    //from IStockOrderEventHandler
    procedure OnInvalidate(const aSender: IStockTrader);
    procedure OnPropertiesChanged(const aSender: IStockTrader);
  end;

  //Обработчик событий от чарта
  TStockChartEventHandler = class (TInterfacedObject,IStockChartEventHandler)
  private
    FOwner: TProjectDocument;
  public
    constructor Create(aOwner: TProjectDocument);

    //from IStockOrderEventHandler
    procedure OnChanged(const sender: IStockChart;aChangeKind: TStockChartEventKind);
    procedure OnHilighted(const sender: IStockChart; const aX1,aX2:TDateTime);
  end;

  TViewChartAction = class (TAction)
  public
    //Symbol: string;
    //Interval: TStockTimeInterval;
    ID: string;
  end;

  ITraderLineEditor = interface
  ['{4FE0F6DE-D341-4123-A33D-9B54BF1F9693}']
    function GetOwner:TProjectDocument;
  end;

  TTraderLineEditor = class (TInterfacedObject, IStockUnitTask,ITraderLineEditor)
  private
    FOwner: TProjectDocument;
  public
    function GetOwner:TProjectDocument;

    function CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);

    constructor Create(aOwner: TProjectDocument);
  end;

  //Специальный класс, который помнит, какой inputdata поставили МЫ
  IInputDataLink = interface (ISCAttribute)
  ['{DE791698-C187-454F-9F32-F58CE344C2A5}']
    procedure SetInputData(const aData: IStockDataSourceToInputDataCollectionMediator);
    function GetInputData:IStockDataSourceToInputDataCollectionMediator;
  end;

implementation
  uses Math, BaseUtils, DateUtils, IniFiles, Menus, SystemService, FC.Documents.Project.Serialization,
  StockChart.Obj, FC.Singletons, Application.Definitions,
  FC.DataUtils,
  FC.Trade.ViewTradersDialog,
  FC.Trade.ViewAlertersDialog,
  FC.Documents.Project.Tools.ReplayTicksDialog,
  FC.Documents.Project.Tools.AuxSymbolsDialog,
  FC.Documents.Project.Tools.MergeIndicatorsDialog,
  FC.Documents.Project.Tools.ShiftTicksDialog,
  FC.Documents.Project.Tools.AddOrderInfoDialog,
  FC.Documents.Project.Tools.ImportOrdersDialog,
  ufmDialog_B;

type
  TInputDataLink = class (TNameValuePersistentObjectRefCounted,IInputDataLink,ISCAttribute)
  private
    FInpuData:IStockDataSourceToInputDataCollectionMediator;
  public
    procedure SetInputData(const aData: IStockDataSourceToInputDataCollectionMediator);
    function GetInputData:IStockDataSourceToInputDataCollectionMediator;
  end;

{ TInputDataLink }

function TInputDataLink.GetInputData: IStockDataSourceToInputDataCollectionMediator;
begin
  result:=FInpuData;
end;

procedure TInputDataLink.SetInputData(const aData: IStockDataSourceToInputDataCollectionMediator);
begin
  FInpuData:=aData;
end;

{ TTraderEventHandler }

constructor TTraderEventHandler.Create(aOwner: TProjectDocument);
begin
  inherited Create;
  FOwner:=aOwner;
end;

procedure TTraderEventHandler.OnInvalidate(const aSender: IStockTrader);
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FOwner.FCharts[aInterval].OnTraderInvalidate(aSender);
end;

procedure TTraderEventHandler.OnPropertiesChanged;
begin
  FOwner.SetModified(true);
end;

{ TStockChartEventHandler }

constructor TStockChartEventHandler.Create(aOwner: TProjectDocument);
begin
  inherited Create;
  FOwner:=aOwner;
end;

procedure TStockChartEventHandler.OnChanged(const sender: IStockChart; aChangeKind: TStockChartEventKind);
begin
  if aChangeKind<>ekCrosshairChanged then
    FOwner.SetModified(true);
  if (aChangeKind in [ekCrosshairChanged,ekVisibleRangeChanged]) and (FOwner.FChartSyncPosition) then
    FOwner.OnChartPositionChange(sender);
end;

procedure TStockChartEventHandler.OnHilighted(const sender: IStockChart; const aX1, aX2: TDateTime);
var
  aCharts: TStockChartArray;
  i: integer;
begin
  inc(FOwner.FSyncLock);

  try
    aCharts:=FOwner.GetAllStockCharts;
    for i := 0 to High(aCharts) do
      if (sender.GetHashCode<>aCharts[i].GetHashCode) and //Не мы сами
         (FOwner.FindStockChartViewOpened(FOwner.GetStockChartID(aCharts[i]))<>nil) //Этот чарт открыт
      then begin
        aCharts[i].Hilight(aX1,aX2);
        aCharts[i].LocateTo(aX1,lmCenter);
      end;
  finally
    dec(FOwner.FSyncLock);
  end;
end;


{ TProjectDocumentSerializationProxy }

constructor TProjectDocumentSerializationProxy.Create;
begin
  inherited;
end;

destructor TProjectDocumentSerializationProxy.Destroy;
begin
  inherited;
end;

procedure TProjectDocumentSerializationProxy.Save(aDocument: TProjectDocument; aStream: TStream);
var
  aSerializer: TProjectDocSerialize;
begin
  FDocument:=aDocument;

  aSerializer:=TProjectDocSerialize.Create;
  try
    aSerializer.SaveObject(self,aStream);
  finally
    aSerializer.Free;
  end;
end;

procedure TProjectDocumentSerializationProxy.Load(aDocument: TProjectDocument; aStream: TStream);
var
  aSerializer: TProjectDocSerialize;
  aObject    : IPersistentObject;
begin
  FDocument:=aDocument;

  aObject:=self;
  aSerializer:=TProjectDocSerialize.Create;
  try
    try
      aSerializer.LoadObject(aStream,aObject);
    except
      on E:Serialization.EInvalidStreamFormat do
        raise EDocumentInvalidFormat.Create(FDocument,E)
      else
        raise;
    end;
  finally
    aObject:=nil;
    aSerializer.Free;
  end;
end;

procedure TProjectDocumentSerializationProxy.OnDefineValues;
begin
  with FDocument do
  begin
    DefValGUID('ID',FID);
    DefValString('StockSymbolName',FStockSymbolName);
    DefValBoolean('ChartSyncPosition',FChartSyncPosition);
    DefValBoolean('ChartStickToEnd ',FChartStickToEnd);
  end;
end;

procedure TProjectDocumentSerializationProxy.OnWriteValues(const aWriter: INameValueDataWriter);
var
  i: TStockTimeInterval;
  j: integer;
  aInputdata: IStockDataSourceToInputDataCollectionMediator;
begin
  inherited;

  aWriter.DataWriter.WriteString('Connections');
  //соединения
  for i:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    //Берем СВОЙ Inputdata!!!
    aInputdata:=FDocument.GetGenuineStockChartInputData(FDocument.GetStockChart(i));
    aWriter.DataWriter.WriteObject((aInputdata as IStockDataSourceToInputDataCollectionMediator).GetDataSource.Connection);
  end;

  aWriter.DataWriter.WriteString('StockCharts');
  for i:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    aWriter.DataWriter.WriteObject(FDocument.FCharts[i] as IPersistentObject);

  aWriter.DataWriter.WriteString('AuxStockCharts');
  aWriter.DataWriter.WriteListBegin;
  for j := 0 to FDocument.FAuxCharts.Count - 1 do
  begin
    aWriter.DataWriter.WriteObject(FDocument.FAuxCharts[j] as IPersistentObject);
    //Берем СВОЙ Inputdata!!!
    aInputdata:=FDocument.GetGenuineStockChartInputData(FDocument.GetAuxStockChart(j));
    aWriter.DataWriter.WriteObject((aInputData as IStockDataSourceToInputDataCollectionMediator).GetDataSource.Connection);
  end;
  aWriter.DataWriter.WriteListEnd;

  aWriter.DataWriter.WriteString('Alerters');
  aWriter.DataWriter.WriteListBegin;
  for j:=0 to FDocument.AlerterCount-1 do
    aWriter.DataWriter.WriteObject(FDocument.Alerters[j] as IPersistentObject);
  aWriter.DataWriter.WriteListEnd;

  aWriter.DataWriter.WriteString('Traders');
  aWriter.DataWriter.WriteListBegin;
  for j:=0 to FDocument.TraderCount-1 do
    aWriter.DataWriter.WriteObject(FDocument.Traders[j] as IPersistentObject);
  aWriter.DataWriter.WriteListEnd;

  //Если диалог живой, нужно сохранить его данные
  if FDocument.FTradeTesterDialog<>nil then
  begin
    FreeAndNil(FDocument.FTradeTesterData);
    FDocument.FTradeTesterData:=TMemoryStream.Create;
    FDocument.FTradeTesterDialog.SaveData(FDocument.FTradeTesterData);
  end;

  //Содержимое тестера
  if FDocument.FTradeTesterData<>nil then
  begin
    aWriter.DataWriter.WriteString('TradeTesterData');
    FDocument.FTradeTesterData.Seek(0,soBeginning);
    aWriter.DataWriter.WriteInteger(int64(FDocument.FTradeTesterData.Size));
    aWriter.DataWriter.Write(FDocument.FTradeTesterData,FDocument.FTradeTesterData.Size);
  end;
end;

procedure TProjectDocumentSerializationProxy.OnReadValue(const aReader: INameValueDataReader;const aName: string; var aHandled: boolean);
var
  i: TStockTimeInterval;
  aConnection : IStockDataSourceConnection;
  aI64 : Int64;
  aStockChart: IStockChart;
begin
  inherited OnReadValue(aReader,aName,aHandled);
  if aHandled then
    exit;

  //соединения
  if aName = 'Connections' then
  begin
    aHandled:=true;

    for i:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      aConnection:=aReader.ReadInterface as IStockDataSourceConnection;
      try
        FDocument.SetDataSourceToChart(aConnection.CreateDataSource,FDocument.FCharts[i]);
      except
        on E:Exception do
          Forms.Application.ShowException(E);
      end;
    end;
  end

  //StockCharts
  else if aName = 'StockCharts' then
  begin
    aHandled:=true;

    for i:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      aReader.ReadObjectExisting(FDocument.FCharts[i] as IPersistentObject);
  end

  //Aux StockCharts
  else if aName = 'AuxStockCharts' then
  begin
    aHandled:=true;
    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      aStockChart:=FDocument.CreateEmptyStockChart;
      aReader.ReadObjectExisting(aStockChart as IPersistentObject);
      aConnection:=aReader.ReadInterface as IStockDataSourceConnection;
      try
        FDocument.SetDataSourceToChart(aConnection.CreateDataSource,aStockChart);
        FDocument.FAuxCharts.Add(aStockChart);
      except
        on E:Exception do
          Forms.Application.ShowException(E);
      end;
    end;
    aReader.ReadListEnd;
  end

  //Traders
  else if aName = 'Traders' then
  begin
    aHandled:=true;

    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      FDocument.FTraders.Add(aReader.ReadInterface);
      FDocument.OnAddTrader(FDocument.Traders[FDocument.TraderCount-1]);
    end;
    aReader.ReadListEnd;
  end
  //Alerters
  else if aName = 'Alerters' then
  begin
    aHandled:=true;

    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      FDocument.FAlerters.Add(aReader.ReadInterface);
      FDocument.OnAddAlerter(FDocument.Alerters[FDocument.AlerterCount-1]);
    end;
    aReader.ReadListEnd;
  end
  //Содержимое тестера
  else if aName = 'TradeTesterData' then
  begin
    aHandled:=true;

    FreeAndNil(FDocument.FTradeTesterData);
    FDocument.FTradeTesterData:=TMemoryStream.Create;
    aReader.ReadInteger(aI64);
    aReader.Read(FDocument.FTradeTesterData,aI64);
  end;
end;

{ TProjecTProjectDocument }

function TProjectDocument.AddAuxSymbol: boolean;
var
  x: IStockDataConnectionWizardContainer;
  i: TStockTimeInterval;
  aDSs: TStockTimeIntervalDataSourceArray;
  aSymbolName: string;
  aStockChart: IStockChart;
begin
  result:=false;

  x:=Factory.CreateObject(IStockDataConnectionWizardContainer) as IStockDataConnectionWizardContainer;

  aSymbolName:='';
  //x.SetDefaultSymbol(FStockSymbolName);
  if (not x.Run(aSymbolName, aDSs)) then
    exit;

  //Инициализируем соединения
  for i:=Low(aDSs) to High(aDSs) do
  begin
    aStockChart:=CreateEmptyStockChart;
    aStockChart.SetStockSymbol(aSymbolName,i);
    SetInputDataToChart(TStockDataSourceToInputDataCollectionMediator.Create(aDSs[i],nil),aStockChart);
    //aStockChart.AddEventHandler(TStockChartEventHandler.Create(self));
    //aStockChart.SetProject(self);
    FAuxCharts.Add(aStockChart);
  end;

  SetModified(true);
  result:=true;

  //Если валюта поменялась, то нужно закрыть все диалоги трейдеров
  //CloseAllDialogs;

  CreateMenuActions;
end;

procedure TProjectDocument.AddEventHandler(const aHandler: IStockProjectEventHandler);
begin
  FEventHandlers.Add(aHandler);
end;

procedure TProjectDocument.AddTrader(const aTrader: IStockTrader);
begin
  FTraders.Add(aTrader);
  OnAddTrader(aTrader);
end;

procedure TProjectDocument.AddAlerter(const aAlerter: IStockAlerter);
begin
  FAlerters.Add(aAlerter);
  OnAddAlerter(aAlerter);
end;

procedure TProjectDocument.CloseAllDialogs;
begin
  FreeAndNil(FTradeTesterData);
  FreeAndNil(FTradeTesterDialog);
  FreeAndNil(FTradeTrainerDialog);
  FreeAndNil(FTradeLiveDialog);    
end;

constructor TProjectDocument.Create(aDocTemplate: TDocTemplate);
var
  aInterval: TStockTimeInterval;
begin
  inherited Create(aDocTemplate);
  CreateGUID(FID); 

  CreateBackupFile:=true;
  FStockChartViewMap:=TViewMap.Create;

  FTraders:=TInterfaceList.Create;
  FAlerters:=TInterfaceList.Create;
  FEventHandlers:=TInterfaceList.Create;

  FViewChartActions:=TObjectList.Create;
  FAuxCharts:=TInterfaceList.Create;

  //Динамически создадим Actions для TStockTimeInterval
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    FCharts[aInterval]:=CreateEmptyStockChart;
    //FCharts[aInterval].AddEventHandler(TStockChartEventHandler.Create(self));
    //FCharts[aInterval].SetProject(self);
    FCharts[aInterval].SetStockSymbol(FStockSymbolName,aInterval);
    SetInputDataToChart(TStockDataSourceToInputDataCollectionMediator.Create(nil,nil),FCharts[aInterval]);
  end;

  StockBrokerConnectionRegistry.AddBrokerEventHandler(self);

  ActionTarget.RegisterAction(UIDataStorage.acFileCloseProject,nil, OnFileCloseProject);
  ActionTarget.RegisterAction(UIDataStorage.acFileSaveProject,nil, OnFileSaveProject);
  ActionTarget.RegisterAction(UIDataStorage.acTradingTradeTester,OnTradingTradeTesterUpdate, OnTradingTradeTester);
  ActionTarget.RegisterAction(UIDataStorage.acTradingTradeLive,OnTradingTradeLiveUpdate, OnTradingTradeLive);
  ActionTarget.RegisterAction(UIDataStorage.acTradingTradeTrainer,OnTradingTradeTrainerUpdate, OnTradingTradeTrainer);
  ActionTarget.RegisterAction(UIDataStorage.acTradingTraderList,OnTradingTraderListUpdate, OnTradingTraderList);
  ActionTarget.RegisterAction(UIDataStorage.acTradingAlerterList,OnTradingAlerterListUpdate, OnTradingAlerterList);  
  ActionTarget.RegisterAction(UIDataStorage.acTradingAddOrderInfoToCharts,nil, OnTradingAddOrderInfoToCharts);
  ActionTarget.RegisterAction(UIDataStorage.acTradingImportOrdersToCharts,nil, OnTradingImportOrdersToCharts);
  ActionTarget.RegisterAction(UIDataStorage.acChartLockUpdating,nil, OnChartLockUpdating);
  ActionTarget.RegisterAction(UIDataStorage.acChartSyncPosition,OnChartSyncPositionUpdate,OnChartSyncPosition);
  ActionTarget.RegisterAction(UIDataStorage.acChartGotoEnd,OnChartGotoEndUpdate,OnChartGotoEnd);
  ActionTarget.RegisterAction(UIDataStorage.acChartStickToEnd,OnChartStickToEndUpdate,OnChartStickToEnd);
  ActionTarget.RegisterAction(UIDataStorage.acProjectChangeDataSource,nil, OnProjectChangeDataSource);
  ActionTarget.RegisterAction(UIDataStorage.acProjectRefreshDatasource,nil, OnProjectRefreshDataSource);
  ActionTarget.RegisterAction(UIDataStorage.acProjectAuxSymbols,nil, OnProjectAuxSymbols);
  ActionTarget.RegisterAction(UIDataStorage.acChartMergeIndicators,nil,OnMergeIndicatorsExecute);

  StockUnitTaskRegistry.AddUnitTask(TTraderLineEditor.Create(self));
end;

function TProjectDocument.CreateEmptyStockChart: IStockChart;
begin
  result:=Factory.CreateObject(IStockChart) as IStockChart;
  result.AddEventHandler(TStockChartEventHandler.Create(self));
  result.SetProject(self);
end;

procedure TProjectDocument.Dispose;
var
  i: integer;
  aInterval: TStockTimeInterval;
  aStockChart: IStockChart;
  aTLE: ITraderLineEditor;
begin
  StockBrokerConnectionRegistry.RemoveBrokerEventHandler(self);

  for i := 0 to StockUnitTaskRegistry.UnitTaskCount - 1 do
    if Supports(StockUnitTaskRegistry.UnitTasks[i],ITraderLineEditor,aTLE) then
    begin
      if aTLE.GetOwner=Self then
      begin
        StockUnitTaskRegistry.DeleteUnitTask(i);
        break;
      end;
    end;


  CloseAllDialogs;
  FViewChartActions.Clear;

  for i:=0 to TraderCount-1 do
  begin
    Traders[i].SetProject(nil);
    Traders[i].Dispose;
  end;

  for i:=0 to AlerterCount-1 do
  begin
    Alerters[i].SetProject(nil);
    Alerters[i].Dispose;
  end;

  while ViewCount>0 do
    RemoveView(Views[0]);

  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    if FCharts[aInterval]<>nil then
    begin
      SetInputDataToChart(nil,FCharts[aInterval]);
      FCharts[aInterval].SetProject(nil);
      FCharts[aInterval].Dispose;
      FCharts[aInterval]:=nil;
    end;
  end;

  //custom charts
  for i := 0 to FAuxCharts.Count - 1 do
  begin
    aStockChart:=FAuxCharts[i] as IStockChart;

    SetInputDataToChart(nil,aStockChart);
    aStockChart.SetProject(nil);
    aStockChart.Dispose;
  end;
  FAuxCharts.Clear;


  FEventHandlers.Clear;
end;

function TProjectDocument.EventHandlerCount: integer;
begin
  result:=FEventHandlers.Count;
end;

function TProjectDocument.GetEventHandlerListCopy: IInterfaceList;
var
  i: integer;
begin
  result:=TInterfaceList.Create;
  for i := 0 to EventHandlerCount - 1 do
    result.Add(EventHandlers[i]);
end;

procedure TProjectDocument.DeleteAuxStockChart(index: integer);
var
  aView:TfmChartView;
begin
  aView:=FindStockChartViewOpened(GetStockChartID(GetAuxStockChart(index)));
  if aView<>nil then
    RemoveView(aView);
  FAuxCharts.Delete(index);

  SetModified(true);
  CreateMenuActions;
end;

destructor TProjectDocument.Destroy;
begin
  inherited;

  //Обязательно после inherited так как может быть обращение к карте
  FreeAndNil(FStockChartViewMap);
  FreeAndNil(FTraders);
  FreeAndNil(FAlerters);  
  FreeAndNil(FEventHandlers);
  FreeAndNil(FViewChartActions);
  FreeAndNil(FAuxCharts);  
end;

function TProjectDocument.GetAllStockCharts: TStockChartArray;
var
  aInterval: TStockTimeInterval;
  i,j: integer;
begin
  Setlength(result,Length(FCharts)+FAuxCharts.Count);

  j:=0;
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    result[j]:=FCharts[aInterval];
    inc(j);
  end;

  for i := 0 to FAuxCharts.Count-1 do
  begin
    result[j]:=FAuxCharts[i] as IStockChart;
    inc(j);
  end;
end;

function TProjectDocument.GetAuxStockChart(index: integer): IStockChart;
begin
  result:=FAuxCharts[index] as IStockChart;
end;

function TProjectDocument.GetAuxStockChartCount: integer;
begin
  result:=FAuxCharts.Count;
end;

function TProjectDocument.GetAuxStockCharts: TStockChartArray;
var
  i: integer;
begin
  Setlength(result,FAuxCharts.Count);

  for i := 0 to FAuxCharts.Count-1 do
    result[i]:=FAuxCharts[i] as IStockChart;
end;

function TProjectDocument.GetDSKPath: string;
begin
  if Self.GetPath='' then  //Такое может быть для нового документа
    exit;

  result:=ChangeFileExt(Self.GetPath,'.dsk');
end;

function TProjectDocument.GetStockChartID(const aChart: IStockChart): string;
var
  aDS : IStockDataSourceToInputDataCollectionMediator;
begin
  aDS:=GetGenuineStockChartInputData(aChart);
  if (aDS=nil) or (aDS.GetDataSource=nil) then
  begin
    if aChart.StockSymbol=nil then
      result:=''
    else
      result:=aChart.StockSymbol.GetCode
  end
  else
    result:=aDS.GetDataSource.Connection.ConnectionString+'@'+
            aDS.GetDataSource.StockSymbol.GetCode;
end;

function TProjectDocument.GetEventHandler(index: integer): IStockProjectEventHandler;
begin
  result:=FEventHandlers[index] as IStockProjectEventHandler;
end;

function TProjectDocument.GetGenuineStockChartInputData(const aChart: IStockChart): IStockDataSourceToInputDataCollectionMediator;
var
  aLink: IInputDataLink;
  i: integer;
begin
  aLink:=nil;
  i:=aChart.GetAttributes.IndexOf(IInputDataLink);
  if i<>-1 then
    aLink:=aChart.GetAttributes.GetItem(i) as IInputDataLink;

  if aLink<>nil then
    result:=aLink.GetInputData;
end;

function TProjectDocument.GetIID: TGUID;
begin
  result:=IStockProject;
end;

function TProjectDocument.GetModified: boolean;
var
  i: integer;
begin
  result:=inherited GetModified;
  if not result then
  begin
    for i:=0 to ViewCount-1 do
      if (Views[i] is TfmChartView) then
      begin
        result:=TfmChartView(Views[i]).Modified;
        if result then
          exit;
      end;
  end;
end;

procedure TProjectDocument.OnNew;
var
  x: IStockDataConnectionWizardContainer;
  i: TStockTimeInterval;
  aDSs: TStockTimeIntervalDataSourceArray;
begin
  x:=Factory.CreateObject(IStockDataConnectionWizardContainer) as IStockDataConnectionWizardContainer;

  if (not x.Run(FStockSymbolName, aDSs)) then
  begin
    Dispose;
    abort;
  end;

  //Инициализируем соединения
  for i:=Low(aDSs) to High(aDSs) do
    SetDataSourceToChart(aDSs[i],FCharts[i]);

  CreateMenuActions;
  SetModified(true);
end;

procedure TProjectDocument.OnOpenOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FCharts[aInterval].OnOpenOrder(aOrder);

  //TODO. Хорошо бы еще понимать, что он открылся автоматом 
  if aSender.IsRealTime then
    Workspace.MainFrame.ShowTrayBalloonHint('Order opened',
      'Type: '+OrderKindNames[aOrder.GetKind]+#13#10+
      'Opened '+ DateTimeToStr(aOrder.GetOpenTime)+' at '+PriceToStr(aOrder,aOrder.GetOpenPrice),btInfo);
end;

procedure TProjectDocument.OnProjectAuxSymbols(aAction: TCustomAction);
begin
  with TfmAuxSymbolsDialog.Create(self) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TProjectDocument.OnProjectChangeDataSource(aAction: TCustomAction);
var
  x: IStockDataConnectionWizardContainer;
  i: TStockTimeInterval;
  aDSs: TStockTimeIntervalDataSourceArray;
  aOldSymbolName: string;
begin
  x:=Factory.CreateObject(IStockDataConnectionWizardContainer) as IStockDataConnectionWizardContainer;

  aOldSymbolName:=FStockSymbolName;
  x.SetDefaultSymbol(FStockSymbolName);
  if (not x.Run(FStockSymbolName, aDSs)) then
    exit;

  //Инициализируем соединения
  for i:=Low(aDSs) to High(aDSs) do
    SetDataSourceToChart(aDSs[i],FCharts[i]);

  SetModified(true);

  //Если валюта поменялась, то нужно закрыть все диалоги трейдеров
  CloseAllDialogs;
  CreateMenuActions;

  //if not AnsiSameText(aOldSymbolName,FStockSymbolName) then
  //  MsgBox.MessageInfo(0,'You have changed the stock symbol. If you have any traders active, it is recomended to restart them.',[]);
end;

procedure TProjectDocument.OnProjectRefreshDataSource(aAction: TCustomAction);
var
  i: TStockTimeInterval;
  aDS : IStockDataSource;
  aDSC : IStockDataSourceConnection;
begin
  for i:=Low(FCharts) to High(FCharts) do
  begin
    aDSC:=(FCharts[i].GetInputData as IStockDataSourceToInputDataCollectionMediator).GetDataSource.Connection;
    aDS:=aDSC.CreateDataSource(false);
    SetDataSourceToChart(aDS,FCharts[i]);
    aDSC:=nil;
    aDS:=nil;
  end;
end;

procedure TProjectDocument.OnCloseOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FCharts[aInterval].OnCloseOrder(aOrder);

  //TODO. Хорошо бы еще понимать, что он закрылся по TP или SL (но не руками)
  if aSender.IsRealTime then
    Workspace.MainFrame.ShowTrayBalloonHint('Order closed',
      'Type: '+OrderKindNames[aOrder.GetKind]+#13#10+
      'Opened '+ DateTimeToStr(aOrder.GetOpenTime)+' at '+PriceToStr(aOrder,aOrder.GetOpenPrice)+#13#10+
      'Closed '+ DateTimeToStr(aOrder.GetCloseTime)+' at '+PriceToStr(aOrder,aOrder.GetClosePrice)+#13#10+
      'Profit (pt): '+ IntToStr(aSender.PriceToPoint(aOrder.GetSymbol, aOrder.GetCurrentProfit)),
      btInfo);
end;

procedure TProjectDocument.OnNewData(const aSender: IStockBroker; const aSymbol: string);
var
  aView: TfmChartView;
  aOpenedViews: TChartViewArray;
  aData: ISCInputDataCollection;
  aAllCharts:TStockChartArray;
  i: Integer;
begin
  aOpenedViews:=nil;

  if FChartStickToEnd then
  begin
    aOpenedViews:=GetOpenedViews;
    for i := 0 to High(aOpenedViews) do
    begin
      aView:=aOpenedViews[i];
      if (aView.Visible) and (aView.StockChart.GetInputData.Count>0) then
      begin
        aData:=aView.StockChart.GetInputData;
        aView.StockChart.LocateTo(aData.DirectGetItem_DataDateTime(aData.Count-1),lmRight);
      end;
    end;
  end;


  if (aSender.IsRealTime) then
  begin
    aAllCharts:=GetAllStockCharts;
    for i := 0 to High(aAllCharts) do
      if SameText(aAllCharts[i].StockSymbol.Name,aSymbol) then
        aAllCharts[i].OnSetQuotation(
          aSender.GetCurrentPrice(aSymbol,bpkBid),
          aSender.GetCurrentPrice(aSymbol,bpkAsk));
  end;
end;

procedure TProjectDocument.OnNewMessage(const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
end;

procedure TProjectDocument.OnNewMessage(const aSender: IStockBroker; const aMessage: IStockBrokerMessage);
begin
end;

procedure TProjectDocument.OnNewOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
begin

end;

procedure TProjectDocument.OnOpen(aStream: TStream);
var
  aProxy: TProjectDocumentSerializationProxy;
begin
  aProxy:=TProjectDocumentSerializationProxy.Create;
  try
    aProxy.Load(self,aStream);
  finally
    aProxy.Free;
  end;

  LoadDSK;

  CreateMenuActions;

  //В конце устанавливаем, что документ не модицирован 
  SetModified(false);
end;

procedure TProjectDocument.OnSave(aStream: TStream);
var
  aProxy: TProjectDocumentSerializationProxy;
begin
  aProxy:=TProjectDocumentSerializationProxy.Create;
  try
    aProxy.Save(self,aStream);
  finally
    aProxy.Free;
  end;

  SaveDSK;
end;

procedure TProjectDocument.OnStart(const aSender: IStockBroker);
begin
end;

procedure TProjectDocument.OnClose;
begin
  RaiseCloseSelf;

  SaveDSK;
  inherited;
  Dispose;
end;

function TProjectDocument.FindStockChart(const aSymbol: string; aInterval: TStockTimeInterval): IStockChart;
var
  aCharts: TStockChartArray;
  i: integer;
begin
  result:=nil;

  aCharts:=GetAllStockCharts;
  for i := 0 to High(aCharts) do
  begin
    if (aCharts[i].StockSymbol.Name=aSymbol) and (aCharts[i].StockSymbol.TimeInterval=aInterval) then
    begin
      result:=aCharts[i];
      exit;
    end;
  end;
end;

function TProjectDocument.FindStockChartViewOpened(const aID: string): TfmChartView;
begin
  result:=nil;
  FStockChartViewMap.Lookup(aID,result);
end;

procedure TProjectDocument.CreateMenuActions;
var
  aAction: TAction;

procedure AddMenuItem(const aChart: IStockChart; aShortCut: boolean);
begin
  //Добавляем Action в меню
  aAction:=(Workspace.MainFrame as IStockMainFrame).AddMenuItem(aAction,true,TViewChartAction);
  aAction.Caption:=fmUIDataStorage.GetViewChartCaption(aChart.StockSymbol.Name,aChart.StockSymbol.TimeInterval);
  //TViewChartAction(aAction).Symbol:=aStockSymbol.Name;
  //TViewChartAction(aAction).Interval:=aStockSymbol.TimeInterval;
  TViewChartAction(aAction).ID:=GetStockChartID(aChart);
  aAction.OnExecute:=OnViewStockChartExecute;
  aAction.OnUpdate:=OnViewStockChartUpdate;
  if aShortCut then
    aAction.ShortCut:=Menus.ShortCut(49+(integer(aChart.StockSymbol.TimeInterval)),[ssCtrl]);
  FViewChartActions.Add(aAction);
end;

var
  aInterval: TStockTimeInterval;
  i: Integer;

begin
  FViewChartActions.Clear;

  aAction:=UIDataStorage.acViewStockChart_;
  for aInterval := Low(FCharts) to High(FCharts) do
    AddMenuItem(FCharts[aInterval],true);

  if FAuxCharts.Count>0 then
  begin
    aAction:=UIDataStorage.acViewStockChart2_;
    for i := 0 to FAuxCharts.Count - 1 do
      AddMenuItem((FAuxCharts[i] as IStockChart),false);
  end;
end;

procedure TProjectDocument.OpenStockChartView(const aID: string);
var
  aView: TfmChartView;
  aCharts: TStockChartArray;
  i: integer;
begin
  aView:=FindStockChartViewOpened(aID);

  if aView<>nil then
  begin
    aView.BringToFront;
    exit;
  end;


  aCharts:=GetAllStockCharts;
  for i := 0 to High(aCharts) do
  begin
    if GetStockChartID(aCharts[i])=aID then
    begin
      TfmChartView.Create(self,aCharts[i]);
      break;
    end;
  end;
end;

procedure TProjectDocument.OnAddView(aView: TDocView);
var
  aView_:TfmChartView;
begin
  inherited;
  if aView is TfmChartView then
  begin
    aView_:=TfmChartView(aView);
    FStockChartViewMap.Add(GetStockChartID(aView_.StockChart),aView_);
  end;
end;

procedure TProjectDocument.OnRemoveView(aView: TDocView);
var
  i: TViewMapIterator;
begin
  inherited OnRemoveView(aView);

  //Удаляем информацию о виде, если он ушел
  FStockChartViewMap.GetFirst(i);
  while i.Valid do
  begin
    if aView=i.Value then
    begin
      FStockChartViewMap.Delete(i.Key);
      break;
    end;
    FStockChartViewMap.GetNext(i);
  end;

  //Эта проверка на всяктй случай
  if (aView is TfmChartView) then
  begin
    RaiseCloseStockChart(TfmChartView(aView).StockChart);
  end;
end;

function TProjectDocument.GetOpenedViews: TChartViewArray;
var
  it      : TViewMapIterator;
  b       : boolean;
  i       : integer;
begin
  SetLength(result,FStockChartViewMap.Count);
  i:=0;

  b:=FStockChartViewMap.GetFirst(it);
  while b do
  begin
    result[i]:=TfmChartView(it.Value);
    inc(i);
    b:=FStockChartViewMap.GetNext(it);
  end;

end;

procedure TProjectDocument.OnViewStockChartExecute(aAction: TObject);
var
  aView:TfmChartView;
  aAction_: TViewChartAction;
begin
  aAction_:=aAction as TViewChartAction;
  aView:=FindStockChartViewOpened(aAction_.ID);
  if aView<>nil then
    aView.Close
  else
    OpenStockChartView(aAction_.ID);
end;

procedure TProjectDocument.OnViewStockChartUpdate(aAction: TObject);
var
  aAction_: TViewChartAction;
begin
  aAction_:=aAction as TViewChartAction;
  aAction_.Checked:=FindStockChartViewOpened(aAction_.ID)<>nil;
//  aAction_.Caption:=UIDataStorage.GetViewChartCaption(aAction_.Symbol,aAction_.Interval);
end;

procedure TProjectDocument.OnFileSaveProject(aAction: TCustomAction);
begin
  self.Save;
end;

procedure TProjectDocument.OnMergeIndicatorsExecute(aAction: TCustomAction);
begin
  with TfmMergeIndicatorsDialog.Create(self) do
    Show();
end;

procedure TProjectDocument.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder;const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  if aModifyEventArgs.ModifyType=omtOpen then
    OnOpenOrder(aSender,aOrder)
  else if aModifyEventArgs.ModifyType=omtClose then
    OnCloseOrder(aSender,aOrder)
  else if aModifyEventArgs.ModifyType=omtChangeTakeProfit then
    OnChangeOrderTakeProfit(aSender,aOrder)
  else if aModifyEventArgs.ModifyType=omtChangeStopLoss then
    OnChangeOrderStopLoss(aSender,aOrder)
  else if aModifyEventArgs.ModifyType=omtChangeTrailingStop then
    OnChangeOrderTrailingStop(aSender,aOrder);
end;

procedure TProjectDocument.OnFileCloseProject(aAction: TCustomAction);
begin
  self.Close;
end;

procedure TProjectDocument.LoadDSK;
var
  aFile: TIniFile;
  aFileName: string;
  aCharts: TStockChartArray;
  i: integer;
begin
  aFileName:=GetDSKPath;
  if aFileName='' then //Такое может быть для нового документа
    exit;

  aFile:=TIniFile.Create(aFileName);
  try
    //Загружаем открытые виды
    aCharts:=GetAllStockCharts;
    for i := 0 to High(aCharts) do
      if aFile.ReadBool('Views',aCharts[i].StockSymbol.Name+' '+aCharts[i].StockSymbol.GetTimeIntervalName,false) then
        OpenStockChartView(GetStockChartID(aCharts[i]));
  finally
    aFile.Free;
  end;
end;

procedure TProjectDocument.SaveDSK;
var
  aFile: TIniFile;
  aFileName: string;
  aOpenedViews: TChartViewArray;
  i: Integer;
begin
  aFileName:=GetDSKPath;
  if aFileName='' then //Такое может быть для нового документа
    exit;

  SysUtils.DeleteFile(aFileName);
  aFile:=TIniFile.Create(aFileName);

  try
    //Сохраняем список открытых видов
    aOpenedViews:=GetOpenedViews;
    for i := 0 to High(aOpenedViews) do
      aFile.WriteBool('Views',aOpenedViews[i].StockSymbol.Name+' '+aOpenedViews[i].StockSymbol.GetTimeIntervalName,true);
  finally
    aFile.Free;
  end;
end;

procedure TProjectDocument.SetDataSourceToChart(aDS: IStockDataSource; const aChart: IStockChart);
var
  aOldID: string;
  aNewID: string;
begin
  aOldID:=GetStockChartID(aChart);
  SetInputDataToChart(TStockDataSourceToInputDataCollectionMediator.Create(aDS,nil),aChart);
  aChart.SetStockSymbol(aDS.StockSymbol.Name,aDS.StockSymbol.TimeInterval);

  aNewID:=GetStockChartID(aChart);
  if (aOldID<>aNewID) and
     (FindStockChartViewOpened(aOldID)<>nil) then
  begin
    FindStockChartViewOpened(aOldID).Close;
    OpenStockChartView(aNewID);
  end;
end;

procedure TProjectDocument.SetInputDataToChart(const aInputData: IStockDataSourceToInputDataCollectionMediator; const aChart: IStockChart);
var
  aDataLink: TInputDataLink;
  i: integer;
begin
  aChart.SetInputData(aInputData as ISCInputDataCollection);

  //Сохраняем дополнительно ссылку на InputData в дополнительном атрибуте
  //Мы его будем использовать, чтобы точно пролучить СВОЙ inputdata,
  //так как в процессе работы назначенный нами Inputdata могут менять различные
  //компоненты программы
  i:=aChart.GetAttributes.IndexOf(IInputDataLink);
  if i<>-1 then
    aChart.GetAttributes.Delete(i);
    
  aDataLink:=TInputDataLink.Create;
  aDataLink.SetInputData(aInputData);
  aChart.GetAttributes.Add(aDataLink);
end;

procedure TProjectDocument.OnAddTrader(aTrader: IStockTrader);
begin
  aTrader.SetProject(Self);
  aTrader.AddEventHandler(TTraderEventHandler.Create(self));
end;

procedure TProjectDocument.OnRemoveTrader(aTrader: IStockTrader);
begin
  aTrader.SetProject(nil);
  aTrader.Dispose;
  //?????
  //aTrader.AddEventHandler(TTraderEventHandler.Create(self));
end;

procedure TProjectDocument.OnAddAlerter(aAlerter: IStockAlerter);
begin
  aAlerter.SetProject(Self);
//  aAlerter.AddEventHandler(TAlerterEventHandler.Create(self));
end;

procedure TProjectDocument.OnRemoveAlerter(aAlerter: IStockAlerter);
begin
  aAlerter.SetProject(nil);
  aAlerter.Dispose;
end;

function TProjectDocument.GetTrader(index: integer): IStockTrader;
begin
  result:=FTraders[index] as IStockTrader;
end;

function TProjectDocument.GetAlerter(index: integer): IStockAlerter;
begin
  result:=FAlerters[index] as IStockAlerter;
end;

procedure TProjectDocument.HilightOnCharts(const aD1, aD2: TDateTime; aCenterStart: boolean; aExceptFor: IStockChart);
var
  aCharts: TStockChartArray;
  i: integer;
begin
  aCharts:=GetAllStockCharts;
  for i := 0 to High(aCharts) do
  begin
    if (aExceptFor<>nil) and (IsEqualGUID(aExceptFor.GetID,aCharts[i].GetID)) then
      continue;

    if aCenterStart then
       aCharts[i].LocateTo(aD1,lmCenter);

    aCharts[i].EnsureVisible(aD1,aD2);
    aCharts[i].Hilight(aD1,aD2);
  end;
end;

function TProjectDocument.TraderCount: integer;
begin
  result:=FTraders.Count;
end;

function TProjectDocument.AlerterCount: integer;
begin
  result:=FAlerters.Count;
end;

procedure TProjectDocument.OnTradingAddOrderInfoToCharts(aAction: TCustomAction);
var
  aTraderLine: ISCIndicatorTradeLine;
  aInterval: TStockTimeInterval;
  aDlg:TfmAddOrderInfoDialog;
  aID: TGUID;
begin
  aDlg:=TfmAddOrderInfoDialog.Create(self);
  try
    if aDlg.ShowModal=mrOk then
    begin
      CreateGUID(aID);
      for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      begin
        aTraderLine:=FCharts[aInterval].GetTraderLine(self.FID,true);
        Assert(aTraderLine<>nil);
        with aTraderLine.AddItem do
        begin
          SetOpenTime(aDlg.OpenDateTime);
          SetOpenPrice(aDlg.OpenPrice);
          SetCloseTime(aDlg.CloseDateTime);
          SetClosePrice(aDlg.ClosePrice);
          SetOrderKind(aDlg.OrderKind);
          SetText(aDlg.Notes);
          Hilight;
          SetID(aID);
        end;

        if FindStockChartViewOpened(GetStockChartID(FCharts[aInterval]))<>nil then
          FCharts[aInterval].LocateTo(aDlg.OpenDateTime,lmCenter);
      end;
    end;
  finally
    aDlg.Free;
  end;
end;

procedure TProjectDocument.OnTradingTradeTester(aAction: TCustomAction);
var
  aDialog:TfmTradeTesterDialog;
begin
  if FTradeTesterDialog<>nil then
  begin
    FTradeTesterDialog.Close;
    exit;
  end;

  aDialog:=TfmTradeTesterDialog.Create(self);
  aDialog.OnClose:=OnCloseTradeTesterDialog;
  if FTradeTesterData<>nil then
  begin
    FTradeTesterData.Seek(0,soFromBeginning);
    try
      Forms.Application.ProcessMessages;
      TWaitCursor.SetUntilIdle;
      aDialog.LoadData(FTradeTesterData);
    except
      on E:Exception do
        Workspace.ExceptionManager.Publish(E,self);
    end;
  end;
  aDialog.Show;

  FTradeTesterDialog:=aDialog;
end;

procedure TProjectDocument.OnTradingTradeLive(aAction: TCustomAction);
var
  aDialog:TfmTradeLiveDialog;
  i: integer;
  aInterval: TStockTimeInterval;
begin
  if FTradeLiveDialog<>nil then
  begin
    FTradeLiveDialog.Close;
    exit;
  end;

  aDialog:=TfmTradeLiveDialog.Create;
  for i:=0 to FTraders.Count-1 do
    aDialog.AddTrader(FTraders[i] as IStockTrader);

  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    aDialog.AddChart(FCharts[aInterval]);

  aDialog.OnClose:=OnCloseTradeLiveDialog;
  aDialog.Show;

  FTradeLiveDialog:=aDialog;
end;

procedure TProjectDocument.OnTradingTradeTrainer(aAction: TCustomAction);
var
  aDialog:TfmTradeTrainerDialog;
begin
  if FTradeTrainerDialog<>nil then
  begin
    FTradeTrainerDialog.Close;
    exit;
  end;

  aDialog:=TfmTradeTrainerDialog.Create(self);
  aDialog.OnClose:=OnCloseTradeTrainerDialog;
  aDialog.Show;

  FTradeTrainerDialog:=aDialog;
end;

procedure TProjectDocument.OnCloseTradeTesterDialog(Sender: TObject; var Action: TCloseAction);
begin
  //Сохраняем состояние
  FreeAndNil(FTradeTesterData);
  FTradeTesterData:=TMemoryStream.Create;
  FTradeTesterDialog.SaveData(FTradeTesterData);

  Action:=caFree;
  FTradeTesterDialog:=nil;
end;

procedure TProjectDocument.OnCloseTradeLiveDialog(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  FTradeLiveDialog:=nil;
end;

procedure TProjectDocument.OnCloseTradeTrainerDialog(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  FTradeTrainerDialog:=nil;
end;

procedure TProjectDocument.OnTradingTradeTesterUpdate(aAction: TCustomAction);
begin
  aAction.Enabled:=FTraders.Count>0;
  aAction.Checked:=(FTradeTesterDialog<>nil);
end;

procedure TProjectDocument.OnTradingTraderListUpdate(aAction: TCustomAction);
begin
  aAction.Enabled:=true; //FTraders.Count>0;
end;

procedure TProjectDocument.OnTradingTraderList(aAction: TCustomAction);
begin
  with TfmViewTradersDialog.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TProjectDocument.OnTradingAlerterListUpdate(aAction: TCustomAction);
begin
  aAction.Enabled:=true; //FAlerters.Count>0;
end;

procedure TProjectDocument.OnTradingImportOrdersToCharts(aAction: TCustomAction);
var
  aDlg:TfmImportOrdersDialog;
  aID: TGUID;
  aInterval: TStockTimeInterval;
  aTraderLine: ISCIndicatorTradeLine;
begin
  aDlg:=TfmImportOrdersDialog.Create(self);
  try
    if aDlg.ShowModal=mrOk then
    begin
      aDlg.taOrders.DisableControls;
      aDlg.taOrders.First;
      while not aDlg.taOrders.Eof do
      begin
        CreateGUID(aID);
        for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
        begin
          aTraderLine:=FCharts[aInterval].GetTraderLine(self.FID,true);
          Assert(aTraderLine<>nil);
          with aTraderLine.AddItem do
          begin
            SetOpenTime(aDlg.taOrdersOpenTime.Value);
            SetOpenPrice(aDlg.taOrdersOpenPrice.Value);
            SetCloseTime(aDlg.taOrdersCloseTime.Value);
            SetClosePrice(aDlg.taOrdersClosePrice.Value);
            SetOrderKind(TStockOrderKind(aDlg.taOrdersType.Value));
            SetText(aDlg.taOrdersNotes.Value);
            Hilight;
            SetID(aID);
          end;
        end;

        aDlg.taOrders.Next;
      end;

      aDlg.taOrders.First;
      if not aDlg.taOrders.Eof then
        for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
        begin
          if FindStockChartViewOpened(GetStockChartID(FCharts[aInterval]))<>nil then
            FCharts[aInterval].LocateTo(aDlg.taOrdersOpenTime.Value,lmCenter);
        end;

    end;
  finally
    aDlg.Free;
  end;
end;

procedure TProjectDocument.OnTradingAlerterList(aAction: TCustomAction);
begin
  with TfmViewAlertersDialog.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TProjectDocument.OnTradingTradeLiveUpdate(aAction: TCustomAction);
begin
  aAction.Enabled:=FTraders.Count>0;
  aAction.Checked:=(FTradeLiveDialog<>nil);
end;

procedure TProjectDocument.OnTradingTradeTrainerUpdate(aAction: TCustomAction);
begin
  aAction.Checked:=(FTradeTrainerDialog<>nil);
end;

procedure TProjectDocument.OnChartStickToEnd(aAction: TCustomAction);
begin
  FChartStickToEnd:=not FChartStickToEnd;
end;

procedure TProjectDocument.OnChartStickToEndUpdate(aAction: TCustomAction);
begin
  aAction.Checked:=FChartStickToEnd;
end;

procedure TProjectDocument.OnChartSyncPosition(aAction: TCustomAction);
var
  aView: TDocView;
begin
  FChartSyncPosition:=not FChartSyncPosition;
  if FChartSyncPosition then
  begin
    aView:=GetActiveView;
    if (aView<>nil) and (aView is TfmChartView)  then
      OnChartPositionChange(TfmChartView(aView).StockChart);
  end;

end;

procedure TProjectDocument.OnChartSyncPositionUpdate(aAction: TCustomAction);
begin
  aAction.Checked:=FChartSyncPosition;
end;

procedure TProjectDocument.OnChartGotoEnd(aAction: TCustomAction);
var
  aView: TfmChartView;
  aOpenedViews: TChartViewArray;
  it: TStockTimeInterval;
  i: Integer;
  aDT: TDateTime;
begin
  aDT:=0;
  //Ищем самый младший открытый чарт
  //TODO Здесь не учитываются AuxCharts
  for it := low(TStockTimeInterval) to high(TStockTimeInterval) do
  begin
    if FindStockChartViewOpened(GetStockChartID(GetStockChart(it)))<>nil then
      if (GetStockChart(it).GetInputData.Count>0) then
      begin
        aDT:=GetStockChart(it).GetInputData.DirectGetItem_DataDateTime(GetStockChart(it).GetInputData.Count-1);
        break;
      end;
  end;

  if aDT<>0 then
  begin
    inc(FSyncLock);
    try
      aOpenedViews:=GetOpenedViews;
      for i := 0 to High(aOpenedViews) do
      begin
        aView:=aOpenedViews[i];
        if aView.Visible then
        begin
          aView.StockChart.LocateTo(aDT,lmCenter);
          aView.StockChart.Mark(aDT);
        end;
      end;
    finally
      dec(FSyncLock);
    end;
  end;
end;

procedure TProjectDocument.OnChartGotoEndUpdate(aAction: TCustomAction);
begin
  aAction.Enabled:=FStockChartViewMap.Count>0;
end;

function TProjectDocument.OnCloseQuery: boolean;
begin
  if (FTradeTesterDialog<>nil) and (FTradeTesterDialog.IsRunning) then
  begin
    MsgBox.MessageAttention(0,'Cannot close project while testing dialog is running. Stop tester before closing',[]);
    result:=false;
  end
  else if (FTradeTrainerDialog<>nil) and (FTradeTrainerDialog.IsRunning) then
  begin
    MsgBox.MessageAttention(0,'Cannot close project while coach dialog is running. Stop coach before closing',[]);
    result:=false;
  end
  else begin
    result:=inherited OnCloseQuery;
  end;
end;

procedure TProjectDocument.OnChangeOrderStopLoss(const aSender: IStockBroker; const aOrder: IStockOrder);
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FCharts[aInterval].OnChangeOrderStopLoss(aOrder);
end;

procedure TProjectDocument.OnChangeOrderTakeProfit(const aSender: IStockBroker; const aOrder: IStockOrder);
var
  aInterval: TStockTimeInterval;
begin
  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FCharts[aInterval].OnChangeOrderTakeProfit(aOrder);
end;

procedure TProjectDocument.OnChangeOrderTrailingStop(const aSender: IStockBroker; const aOrder: IStockOrder);
begin
end;

procedure TProjectDocument.OnChartLockUpdating(aAction: TCustomAction);
var
 i: TStockTimeInterval;
begin
  aAction.Checked:=not aAction.Checked;
  if aAction.Checked then
  begin
    for i:=Low(FCharts) to High(FCharts) do
      FCharts[i].BeginUpdate
  end
  else begin
    for i:=Low(FCharts) to High(FCharts) do
      FCharts[i].EndUpdate;
  end
end;

function TProjectDocument.GetStockChart(aInterval: TStockTimeInterval): IStockChart;
begin
  result:=FCharts[aInterval];
end;

function TProjectDocument.GetStockSymbol: string;
begin
  result:=FStockSymbolName;
end;

procedure TProjectDocument.RaiseCloseStockChart(const aStockChart: IStockChart);
var
  i: Integer;
  aEventHandlers : IInterfaceList;
begin
  aEventHandlers:=GetEventHandlerListCopy;

  for i := 0 to aEventHandlers.Count - 1 do
    (aEventHandlers[i] as IStockProjectEventHandler).OnCloseStockChart(self,aStockChart);
end;

procedure TProjectDocument.RaiseCloseSelf;
var
  i: Integer;
  aEventHandlers : IInterfaceList;
begin
  aEventHandlers:=GetEventHandlerListCopy;

  for i := 0 to aEventHandlers.Count - 1 do
    (aEventHandlers[i] as IStockProjectEventHandler).OnCloseProject(self);
end;

procedure TProjectDocument.RemoveEventHandler(const aHandler: IStockProjectEventHandler);
begin
  FEventHandlers.Remove(aHandler);
end;

function TProjectDocument.RemoveTrader(const aTrader: IStockTrader): integer;
begin
  result:=IndexOfTrader(aTrader);
  if result<>-1 then
  begin
    OnRemoveTrader(aTrader);
    FTraders.Delete(result);
  end;
end;

function TProjectDocument.RemoveAlerter(const aAlerter: IStockAlerter): integer;
begin
  result:=IndexOfAlerter(aAlerter);
  if result<>-1 then
  begin
    OnRemoveAlerter(aAlerter);
    FAlerters.Delete(result);
  end;
end;

function TProjectDocument.IndexOfAuxStockChart(const aChart: IStockChart): integer;
var
  I: Integer;
begin
  result:=-1;
  
  for I := 0 to GetAuxStockChartCount - 1 do
    if GetAuxStockChart(i)=aChart then
    begin
      result:=i;
      break;
    end;
end;

function TProjectDocument.IndexOfTrader(const aTrader: IStockTrader): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to TraderCount-1 do
    if GetTrader(i).IsThis(aTrader) then
    begin
      result:=i;
      break;
    end;
end;

function TProjectDocument.IndexOfAlerter(const aAlerter: IStockAlerter): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to AlerterCount-1 do
    if GetAlerter(i).IsThis(aAlerter) then
    begin
      result:=i;
      break;
    end;
end;

procedure TProjectDocument.HilightOnCharts(const aD1, aD2: TDateTime; aCenterStart: boolean);
begin
  HilightOnCharts(aD1,aD2,aCenterStart,nil);
end;

procedure TProjectDocument.ReplayTicks(const aFrom, aTo: TDateTime; aClearOldValues: boolean);
begin
  //Сначала закрываем старые диалоги, если есть
  TfmReplayTicksDialog.CloseAll(TfmReplayTicksDialog);


  with TfmReplayTicksDialog.Create(self,aFrom,aTo,aClearOldValues) do
    Show;
end;

procedure TProjectDocument.ShiftTicks(const aReferencePoint:TDateTime);
begin
  //Сначала закрываем старые диалоги, если есть
  TfmShiftTicksDialog.CloseAll(TfmShiftTicksDialog);

  with TfmShiftTicksDialog.Create(self,aReferencePoint) do
    Show;
end;

procedure TProjectDocument.OnChartPositionChange(const aChart: IStockChart);
var
  aView: TfmChartView;
  aFV: TDateTime;
  aCP: TDateTime;
  aOpenedViews: TChartViewArray;
  i: Integer;
begin
  if not FChartSyncPosition then
    exit;

  if FSyncLock>0 then
    exit;

  aFV:=aChart.GetFirstVisible;
  aCP:=aChart.GetCrosshairPos;

  if aFV<=0 then
    exit;

  inc(FSyncLock);
  try
    aOpenedViews:=GetOpenedViews;
    for i := 0 to High(aOpenedViews) do
    begin
      aView:=aOpenedViews[i];

      if aView.Visible and (aView.StockChart.GetHashCode<>aChart.GetHashCode) then
      begin
        aView.StockChart.LocateTo(aCP,lmCenter);
        aView.StockChart.Mark(aCP);
      end;
    end;
  finally
    dec(FSyncLock);
  end;
end;


{ TTraderLineEditor }

function TTraderLineEditor.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
var
  aLine: ISCIndicatorTradeLine;
begin
  result:=false;

  if Supports(aIndicator,ISCIndicatorTradeLine,aLine) and
     IsEqualGUID(aLine.GetID,FOwner.FID) then
  begin
    result:=aLine.IndexOfHilightedItem<>-1;
    if result then
      aOperationName:='Edit Order Info';
  end;
end;

constructor TTraderLineEditor.Create(aOwner: TProjectDocument);
begin
  FOwner:=aOwner;
end;

function TTraderLineEditor.GetOwner: TProjectDocument;
begin
  result:=FOwner;
end;

procedure TTraderLineEditor.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  aTraderLine: ISCIndicatorTradeLine;
  i: integer;
  aInterval: TStockTimeInterval;
  aDlg:TfmAddOrderInfoDialog;
  aID : TGUID;
begin
  if not Supports(aIndicator,ISCIndicatorTradeLine,aTraderLine) or
     not IsEqualGUID(aTraderLine.GetID,FOwner.FID) then
     exit;

  i:=aTraderLine.IndexOfHilightedItem;
  if i=-1 then
    exit;

  aID:=aTraderLine.GetItem(i).GetID;

  aDlg:=TfmAddOrderInfoDialog.Create(FOwner);
  try
    aDlg.Caption:='Edit Order Info';
    aDlg.OpenDateTime:=aTraderLine.GetItem(i).GetOpenTime;
    aDlg.OpenPrice:=aTraderLine.GetItem(i).GetOpenPrice;
    aDlg.CloseDateTime:=aTraderLine.GetItem(i).GetCloseTime;
    aDlg.ClosePrice:=aTraderLine.GetItem(i).GetClosePrice;
    aDlg.Notes:=aTraderLine.GetItem(i).GetText;
    aDlg.OrderKind:=aTraderLine.GetItem(i).GetOrderKind;

    if aDlg.ShowModal=mrOk then
    begin
      for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      begin
        aTraderLine:=FOwner.GetStockChart(aInterval).GetTraderLine(FOwner.FID,false);
        if aTraderLine<>nil then
        begin
          i:=aTraderLine.IndexOfItem(aID);
          if i<>-1 then
            with aTraderLine.GetItem(i) do
            begin
              SetOpenTime(aDlg.OpenDateTime);
              SetOpenPrice(aDlg.OpenPrice);
              SetCloseTime(aDlg.CloseDateTime);
              SetClosePrice(aDlg.ClosePrice);
              SetOrderKind(aDlg.OrderKind);
              SetText(aDlg.Notes);
            end;
        end;
      end;
    end;
  finally
    aDlg.Free;
  end;
end;


{ TProjectDocument.TViewMap }

constructor TProjectDocument.TViewMap.Create;
begin
  inherited Create;
  AllowAppendRewrite:=false;
end;

end.





