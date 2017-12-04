unit FC.Trade.ResultPage;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, TeEngine, Series, ExtCtrls, TeeProcs, Chart, ExtendControls,
  DB, MemoryDS, ActnList,
  Serialization,
  FC.fmUIDataStorage,
  FC.Definitions,
  StockChart.Definitions,
  FC.Trade.Statistics, Grids, DBGrids, MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, Menus, ActnPopup, JvExComCtrls,
  JvComCtrls, StdCtrls,Application.Definitions, PlatformDefaultStyleActnCtrls;

type
  TTradeMode = (tmTest,tmReal);

  TfrmStockTradeResult = class(TFrame,IActionTarget)
    acActions: TActionList;
    acGetStatistic: TAction;
    acExportOrders: TAction;
    taOrders: TMemoryDataSet;
    taOrdersNo: TIntegerField;
    taOrdersType: TStringField;
    taOrdersOpenTime: TDateTimeField;
    taOrdersOpenBarNo: TStringField;
    taOrdersOpenPrice: TCurrencyField;
    taOrdersCloseTime: TDateTimeField;
    taOrdersCloseBarNo: TStringField;
    taOrdersClosePrice: TCurrencyField;
    taOrdersProfitPt: TIntegerField;
    taOrdersProfit: TCurrencyField;
    taOrdersWorstPt: TIntegerField;
    taOrdersBestPt: TIntegerField;
    taOrdersBalance: TCurrencyField;
    taOrdersCloseComment: TStringField;
    dsOrder: TDataSource;
    taOrdersOrderID: TStringField;
    taOrdersQuality: TIntegerField;
    acChooseColumns: TAction;
    pmGrid: TPopupActionBar;
    ChooseColumns1: TMenuItem;
    acFitColumns: TAction;
    FitColumns1: TMenuItem;
    N1: TMenuItem;
    Hilightoncharts1: TMenuItem;
    acAutoScroll: TAction;
    miAutoScroll: TMenuItem;
    pcPages: TJvPageControl;
    tsOrderHistory: TTabSheet;
    tsGraph: TTabSheet;
    se: TChart;
    chartBalance: TFastLineSeries;
    Splitter1: TSplitter;
    tcTabs: TFlatTabControl;
    grOrders: TEditDBGrid;
    grOrderDetails: TEditDBGrid;
    taOrderDetails: TMemoryDataSet;
    taOrderDetailsNoInGroup: TIntegerField;
    taOrderDetailsOrderID: TStringField;
    taOrderDetailsText: TStringField;
    dsOrderDetails: TDataSource;
    taOrderDetailsWhen: TDateTimeField;
    taOrdersStopLoss: TCurrencyField;
    taOrdersTakeProfit: TCurrencyField;
    taOrdersTrailingStop: TCurrencyField;
    taOrdersDetailNo: TIntegerField;
    taOrdersPendingOpenPrice: TCurrencyField;
    taOrderDetailsPriceAsk: TCurrencyField;
    taOrderDetailsPriceBid: TCurrencyField;
    taOrdersOpenComment: TStringField;
    tsJournal: TTabSheet;
    grJournal: TEditDBGrid;
    taOrderDetailsOrderNo: TIntegerField;
    taOrdersSubsidencePt: TIntegerField;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    buExportOrders: TToolButton;
    buGetStatistic: TToolButton;
    ToolButton2: TToolButton;
    buAutoScroll: TToolButton;
    laBalance: TLabel;
    taOrderDetailsNo: TIntegerField;
    tmFilterDetails: TTimer;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    acFilter: TAction;
    laElapsedTime: TLabel;
    Gotothebeginning1: TMenuItem;
    Gotoend1: TMenuItem;
    N3: TMenuItem;
    taOrdersNotes: TStringField;
    taOrdersColor: TIntegerField;
    taOrdersCreateTime: TDateTimeField;
    taOrderDetailsColor: TIntegerField;
    acHilightOnCharts: TAction;
    taOrdersState: TIntegerField;
    tsEquity: TTabSheet;
    chEquity: TChart;
    seEquity: TFastLineSeries;
    taOrdersLots: TFloatField;
    taOperationHistory: TMemoryDataSet;
    dsOperationHistory: TDataSource;
    tsOperationHistory: TTabSheet;
    grOperationHistory: TEditDBGrid;
    taOperationHistoryNo: TIntegerField;
    taOperationHistoryTime: TDateTimeField;
    taOperationHistoryOrderNo: TIntegerField;
    taOperationHistoryType: TStringField;
    taOperationHistoryLots: TFloatField;
    taOperationHistoryPrice: TCurrencyField;
    taOperationHistoryStopLoss: TCurrencyField;
    taOperationHistoryTakeProfit: TCurrencyField;
    taOperationHistoryProfit: TCurrencyField;
    taOperationHistoryBalance: TCurrencyField;
    procedure Gotoend1Click(Sender: TObject);
    procedure Gotothebeginning1Click(Sender: TObject);
    procedure grJournalDblClick(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure taOrdersOpenTimeGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    procedure tmFilterDetailsTimer(Sender: TObject);
    procedure grOrderDetailsBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure pcPagesChange(Sender: TObject);
    procedure taOrderDetailsBeforeClose(DataSet: TDataSet);
    procedure taOrderDetailsAfterOpen(DataSet: TDataSet);
    procedure grOrdersChangeRecord(Sender: TObject);
    procedure taOrderDetailsFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure tcTabsChange(Sender: TObject);
    procedure acAutoScrollExecute(Sender: TObject);
    procedure acExportOrdersUpdate(Sender: TObject);
    procedure acFitColumnsExecute(Sender: TObject);
    procedure acChooseColumnsUpdate(Sender: TObject);
    procedure acChooseColumnsExecute(Sender: TObject);
    procedure grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure taOrdersBeforeClose(DataSet: TDataSet);
    procedure taOrdersAfterOpen(DataSet: TDataSet);
    procedure grOrdersDblClick(Sender: TObject);
    procedure acGetStatisticUpdate(Sender: TObject);
    procedure acGetStatisticExecute(Sender: TObject);
    procedure acExportOrdersExecute(Sender: TObject);
    procedure taOperationHistoryBeforeClose(DataSet: TDataSet);
    procedure taOperationHistoryAfterOpen(DataSet: TDataSet);
    procedure grOperationHistoryDblClick(Sender: TObject);
  private
    FStatictic          : TStockTradingStatistics;
    FLastRepaintTime    : TDateTime;
    FOrderCount   : integer;
    FCharts             : array of IStockChart;
    FProject            : IStockProject;
    FCurrentTrader      : IStockTrader;
    FOrderDetailNo      : integer;
    FStartTime          : TDateTime;
    FMode               : TTradeMode;
    FLogOrderDetails: boolean;
    FLastEquityTime     : TDateTime;
    FLastEquity         : TStockRealNumber;

    function GetAutoScroll: boolean;
    procedure SetAutoScroll(const Value: boolean);

  protected
    procedure OnOpenOrderInternal(aOrder: IStockOrder); virtual;
    procedure OnCloseOrderInternal(aOrder: IStockOrder); virtual;
    procedure AddDetails(const aOrder: IStockOrder; const aText: string; const aColor: TColor); overload;
    procedure AddDetails(const aBroker: IStockBroker; const aText: string; const aColor: TColor); overload;

    //Получить номер записи в таблице ордеров по ID ордера
    function  OrderRecordByID(const aID: TGUID): integer;
    procedure Loaded; override;
  public
    procedure Init(const aCharts : array of IStockChart);
    procedure OnStart(const aBroker: IStockBroker; const aTrader:IStockTrader); virtual;
    procedure OnEnd; virtual;

    property  LogOrderDetails: boolean read FLogOrderDetails write FLogOrderDetails;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveData(const aDataWriter: IDataWriter);
    procedure LoadData(const aDataReader: IDataReader);

    procedure OnNewData(const aSender: IStockBroker; const aSymbol: string); virtual;
    procedure OnNewOrder(const aOrder: IStockOrder); virtual;
    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewMessage (const aBroker: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;

    property  Statictics : TStockTradingStatistics read FStatictic;
    property  AutoScrollOrders: boolean read GetAutoScroll write SetAutoScroll;

    //Обработка событий
    procedure OnActionUpdate(aAction: TActionParams);
    procedure OnActionExecute(aAction: TActionParams);

    property  Mode : TTradeMode read FMode write FMode;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
  uses Math,BaseUtils, DateUtils, SystemService, DBUtils, Export.Definitions,FC.DataUtils,
  StockChart.Definitions.Drawing, FC.Trade.StatisticsDialog,FC.Trade.FilterDialog, ufmDialog_B,
  JvEditor, ufmForm_B;

{$R *.dfm}

type
  //Прокси для сериализации документа
  TResultPageSerializationProxy = class (TNameValuePersistentObjectRefCounted)
  private
    FOwner: TfrmStockTradeResult;
  public
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;

    constructor Create(aOwner: TfrmStockTradeResult);
  end;


{ TResultPageSerializationProxy }

constructor TResultPageSerializationProxy.Create(aOwner: TfrmStockTradeResult);
begin
  inherited Create;
  FOwner:=aOwner;
end;

procedure TResultPageSerializationProxy.OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean);
var
  aD: double;
  S: string;
begin
  inherited;


  if aName='DataSet' then
  begin
    //История ордеров
    FOwner.taOrders.EmptyTable;
    FOwner.taOrders.Open;
    aReader.ReadString(s);
    LoadDataFromString(FOwner.taOrders,s);
    aHandled:=true;
  end
  else if aName='Details' then
  begin
    FOwner.taOrderDetails.EmptyTable;
    FOwner.taOrderDetails.Open;
    aReader.ReadString(s);
    LoadDataFromString(FOwner.taOrderDetails,s);
    aHandled:=true;
  end
  else if aName='OperationHistory' then
  begin
    //История операций
    FOwner.taOperationHistory.EmptyTable;
    FOwner.taOperationHistory.Open;
    aReader.ReadString(s);
    LoadDataFromString(FOwner.taOperationHistory,s);
    aHandled:=true;
  end
  else if aName='Graphic' then
  begin
    //График баланса
    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      aReader.ReadDouble(aD);
      FOwner.chartBalance.AddY(aD);
    end;
    aReader.ReadListEnd;

    aHandled:=true;
  end

  else if aName='Selected Order' then
  begin
    aHandled:=true;  
    aReader.ReadString(S);
    if FOwner.taOrders.Active then
      FOwner.taOrders.Locate(FOwner.taOrdersOrderID.FieldName,S,[]);
  end;
end;

procedure TResultPageSerializationProxy.OnWriteValues(const aWriter: INameValueDataWriter);
var
  i: integer;
  aFiltered: boolean;
begin
  inherited;

  //График баланса
  aWriter.DataWriter.WriteString('Graphic');
  aWriter.DataWriter.WriteListBegin;
  for i := 0 to FOwner.chartBalance.YValues.Count - 1 do
    aWriter.DataWriter.WriteDouble(FOwner.chartBalance.YValues[i]);
  aWriter.DataWriter.WriteListEnd;

  //История ордеров
  if FOwner.taOrders.Active then
  begin
    aWriter.DataWriter.WriteString('DataSet');

    aFiltered:=FOwner.taOrders.Filtered;
    FOwner.taOrders.DisableControls;
    FOwner.taOrders.Filtered:=false;
    try
      aWriter.DataWriter.WriteString(SaveDataSetToString(FOwner.taOrders));
    finally
      FOwner.taOrders.Filtered:=aFiltered;
      FOwner.taOrders.EnableControls;
    end;
  end;

  if FOwner.taOrderDetails.Active then
  begin
    //Детализация ордеров
    //FOwner.dsOrderDetails.DataSet:=nil;
    aFiltered:=FOwner.taOrderDetails.Filtered;
    FOwner.taOrderDetails.Filtered:=false;
    try
      aWriter.DataWriter.WriteString('Details');
      aWriter.DataWriter.WriteString(SaveDataSetToString(FOwner.taOrderDetails));
    finally
      FOwner.taOrderDetails.Filtered:=aFiltered;
    end;
  end;

  if FOwner.taOperationHistory.Active then
  begin
    aFiltered:=FOwner.taOperationHistory.Filtered;
    FOwner.taOperationHistory.Filtered:=false;
    try
      aWriter.DataWriter.WriteString('OperationHistory');
      aWriter.DataWriter.WriteString(SaveDataSetToString(FOwner.taOperationHistory));
    finally
      FOwner.taOperationHistory.Filtered:=aFiltered;
    end;
  end;

  if FOwner.taOrders.Active then
  begin
    aWriter.WriteString('Selected Order',FOwner.taOrdersOrderID.AsString);
  end;

end;

{ TfrmStockTradeResult }

constructor TfrmStockTradeResult.Create(aOwner: TComponent);
begin
  inherited;
  FLogOrderDetails:=true;
  FStatictic:=TStockTradingStatistics.Create;
  tcTabs.Flat:=true;
  tcTabs.DrawTopLine:=true;
  pcPages.ActivePageIndex:=0;
  tcTabs.TabIndex:=0;
  Workspace.MainFrame.AddActionTarget(self);
  pcPagesChange(nil);
end;

destructor TfrmStockTradeResult.Destroy;
begin
  Workspace.MainFrame.RemoveActionTarget(self);
  if taOrders.Active then
  begin
    taOrdersBeforeClose(nil); //Не срабатывает автоматом
    taOrders.Close;
  end;

  if taOrderDetails.Active then
  begin
    taOrderDetailsBeforeClose(nil); //Не срабатывает автоматом
    taOrderDetails.Close;
  end;

  if taOperationHistory.Active then
  begin
    taOperationHistoryBeforeClose(nil); //Не срабатывает автоматом
    taOperationHistory.Close;
  end;

  Workspace.Storage(self).WriteString(grOrders,'Columns',grOrders.ColumnStates.StrDump);
  Workspace.Storage(self).WriteString(grOrderDetails,'Columns',grOrderDetails.ColumnStates.StrDump);

  inherited;
  FreeAndNil(FStatictic);
end;

procedure TfrmStockTradeResult.Init(const aCharts : array of IStockChart);
var
  i: integer;
  s: string;
begin
  SetLength(FCharts,Length(aCharts));
  for i:=0 to High(aCharts) do
    FCharts[i]:=aCharts[i];

  s:='#Bar(';
  for i:=0 to High(FCharts) do
    s:=s+FCharts[i].StockSymbol.GetTimeIntervalName+',';
  s:=StrTrimRight(s,[','])+')';

  taOrdersOpenBarNo.DisplayLabel:=s;
  taOrdersCloseBarNo.DisplayLabel:=s;

  FProject:=aCharts[0].GetProject;

  pcPages.ActivePageIndex:=0;
  pcPagesChange(pcPages);
end;

procedure TfrmStockTradeResult.OnStart(const aBroker: IStockBroker; const aTrader:IStockTrader);
begin
  FStartTime:=Now;
  FCurrentTrader:=aTrader;

  FreeAndNil(FStatictic);
  FStatictic:=TStockTradingStatistics.Create;

  taOrders.EmptyTable;
  taOrders.Open;
  chartBalance.Clear;
  seEquity.Clear;

  taOrderDetails.EmptyTable;
  taOrderDetails.Open;

  taOperationHistory.EmptyTable;
  taOperationHistory.Open;

  FOrderCount:=0;
  FLastEquityTime:=0;
  FLastEquity:=aBroker.GetEquity;
  laBalance.Caption:='Current balance: '+FormatCurr('0,.00',aBroker.GetBalance);

  if FMode=tmTest then
    laElapsedTime.Caption:='Elapsed Time: '+TimeToStr(0)
  else
    laElapsedTime.Caption:='Last tick: ';
end;

function TfrmStockTradeResult.OrderRecordByID(const aID: TGUID): integer;
var
  i: integer;
  s: string;
begin
  s:=GUIDToString(aID);

  for i := taOrders.RecordCount - 1 downto 0 do
    if V2S(taOrders.DirectGetFieldData(i,taOrdersOrderID))=s then
    begin
      result:=i;
      exit;
    end;

  raise EAlgoError.Create;
end;

procedure TfrmStockTradeResult.pcPagesChange(Sender: TObject);
begin
  tcTabs.TabIndex:=pcPages.ActivePageIndex;
  if pcPages.ActivePage=tsJournal then
  begin
    taOrderDetails.Filtered:=false;
    taOrderDetailsOrderNo.Visible:=true;
    taOrderDetailsNoInGroup.Visible:=false;
  end
  else if pcPages.ActivePage=tsOrderHistory then
  begin
    taOrderDetails.Filtered:=true;
    taOrderDetailsOrderNo.Visible:=false;
    taOrderDetailsNoInGroup.Visible:=true;
  end;
end;

procedure TfrmStockTradeResult.SaveData(const aDataWriter: IDataWriter);
var
  aProxy: IPersistentObject;
begin
  aProxy:=TResultPageSerializationProxy.Create(self);
  aDataWriter.WriteObject(aProxy);
end;

procedure TfrmStockTradeResult.LoadData(const aDataReader: IDataReader);
var
  aProxy: IPersistentObject;
begin
  aProxy:=TResultPageSerializationProxy.Create(self);
  aDataReader.ReadObjectExisting(aProxy);
end;

procedure TfrmStockTradeResult.Loaded;
begin
  inherited;
  grOrders.ColumnStates.StrDump:=Workspace.Storage(self).ReadString(grOrders,'Columns','');
  grOrderDetails.ColumnStates.StrDump:=Workspace.Storage(self).ReadString(grOrderDetails,'Columns','');
end;

procedure TfrmStockTradeResult.SetAutoScroll(const Value: boolean);
begin
  acAutoScroll.Checked:=Value;
end;

procedure TfrmStockTradeResult.OnEnd;
begin
  chartBalance.Repaint;
  chEquity.Repaint;
  if FMode=tmTest then
    laElapsedTime.Caption:='Elapsed Time: '+TimeToStr(Now-FStartTime);
end;

procedure TfrmStockTradeResult.OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
var
  aRecNo: integer;
  s: string;
begin
  if FMode=tmTest then
    laElapsedTime.Caption:='Elapsed Time: '+TimeToStr(Now-FStartTime);

  //Состояние ордеров и детализации может быть рассинхронизировано
  if (taOrderDetailsOrderID.Value<>taOrdersOrderID.Value) and (taOrderDetails.Filtered) then
    tmFilterDetailsTimer(nil);

  BeginUpdate;
  try
    //Open
    if aModifyEventArgs.ModifyType=omtOpen then
    begin
      OnOpenOrderInternal(aOrder);
    end
    //Close
    else if aModifyEventArgs.ModifyType=omtClose then
    begin
      OnCloseOrderInternal(aOrder);
    end
    //Modify
    else begin
      aRecNo:=OrderRecordByID(aOrder.GetID);

      if aModifyEventArgs.ModifyType=omtChangeStopLoss then
      begin
        if taOrders.DirectGetFieldData(aRecNo,taOrdersStopLoss)<>0 then
        begin
          if aModifyEventArgs.StopLossFromTrailingStop then
            s:='Stop Loss automatically changed from '
          else
            s:='Stop Loss changed from ';
          s:=s+PriceToStr(aOrder,taOrders.DirectGetFieldData(aRecNo,taOrdersStopLoss))+' to '+
               PriceToStr(aOrder,aOrder.GetStopLoss);
        end
        else begin
          if aModifyEventArgs.StopLossFromTrailingStop then
            s:='Stop Loss automatically set to '
          else
            s:='Stop Loss set to ';
          s:=s+PriceToStr(aOrder,aOrder.GetStopLoss);
        end;
        AddDetails(aOrder,s,clDefault);
      end

      else if aModifyEventArgs.ModifyType=omtChangeTakeProfit then
      begin
        if taOrders.DirectGetFieldData(aRecNo,taOrdersTakeProfit)<>0 then
          s:='Take Profit changed from '+
            PriceToStr(aOrder,taOrders.DirectGetFieldData(aRecNo,taOrdersTakeProfit))+' to '+
            PriceToStr(aOrder,aOrder.GetTakeProfit)
        else
          s:='Take Profit set to '+PriceToStr(aOrder,aOrder.GetTakeProfit);
        AddDetails(aOrder,s,clDefault);
      end

      else if aModifyEventArgs.ModifyType=omtChangeTrailingStop then
      begin
        if taOrders.DirectGetFieldData(aRecNo,taOrdersTrailingStop)<>0 then
          s:='Trailing Stop changed from '+IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,taOrders.DirectGetFieldData(aRecNo,taOrdersTrailingStop)))+' pts to '+
                                           IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetTrailingStop))+
             ' pts (Current profit = '+IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetCurrentProfit))+' pts)'
        else
          s:='Trailing Stop set to '+IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetTrailingStop))+
             ' pts (Current profit = '+IntToStr(aOrder.GetBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetCurrentProfit))+' pts)';

        AddDetails(aOrder,s,clDefault);
      end

      else if aModifyEventArgs.ModifyType=omtChangeOpenPrice then
      begin
        //Для открытия отложенного ордера нужно сразу указать и комментарий

        taOrders.DirectSetFieldData(aRecNo,taOrdersType,OrderKindNames[aOrder.GetKind]);
        taOrders.DirectSetFieldData(aRecNo,taOrdersStopLoss,aOrder.GetStopLoss);
        taOrders.DirectSetFieldData(aRecNo,taOrdersTakeProfit,aOrder.GetTakeProfit);
        taOrders.DirectSetFieldData(aRecNo,taOrdersTrailingStop,aOrder.GetTrailingStop);
        taOrders.DirectSetFieldData(aRecNo,taOrdersOpenComment,aOrder.GetOpenComment);
        taOrders.DirectSetFieldData(aRecNo,taOrdersLots,aOrder.GetLots);
        taOrders.DirectSetFieldData(aRecNo,taOrdersType,OrderKindNames[aOrder.GetKind]);
        taOrders.DirectSetFieldData(aRecNo,taOrdersState,aOrder.GetState);

        if taOrders.DirectGetFieldData(aRecNo,taOrdersPendingOpenPrice)<>0 then
        begin
          s:=Format('Pending Open Price changed from %s  to %s (%s)',
                    [PriceToStr(aOrder,taOrders.DirectGetFieldData(aRecNo,taOrdersPendingOpenPrice)),
                     PriceToStr(aOrder,aOrder.GetPendingOpenPrice),
                     OrderKindNames[aOrder.GetKind]]);
        end
        else begin
          s:=Format('Pending Open Price set to %s (%s)',
                    [PriceToStr(aOrder,aOrder.GetPendingOpenPrice),
                     OrderKindNames[aOrder.GetKind]]);
        end;

        AddDetails(aOrder,s,clDefault);

        taOrders.RecNo:=taOrders.RecNo;
      end

      else if aModifyEventArgs.ModifyType=omtPendingRevoke then
      begin
        s:='Order revoked';
        taOrders.DirectSetFieldData(aRecNo,taOrdersState,aOrder.GetState);
        AddDetails(aOrder,s,clDefault);

        taOrders.RecNo:=taOrders.RecNo;
      end

      else if aModifyEventArgs.ModifyType=omtChangeNotes then
      begin
        taOrders.DirectSetFieldData(aRecNo,taOrdersNotes,aOrder.GetNotes);
      end

      else if aModifyEventArgs.ModifyType=omtChangeColor then
      begin
        taOrders.DirectSetFieldData(aRecNo,taOrdersColor,aOrder.GetColor);
      end


      else if aModifyEventArgs.ModifyType=omtPendingSuspend then
      begin
        if aOrder.IsPendingSuspended then
          s:='Pending Order Suspended'
        else
          s:='Pending Order Resumed';
        AddDetails(aOrder,s,clDefault);
      end
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmStockTradeResult.OnNewMessage(const aBroker: IStockBroker; const aMessage: IStockBrokerMessage);
begin
  BeginUpdate;
  try
    if aMessage.Color=clDefault then
      AddDetails(aBroker,'Message: '+aMessage.Text,clWebCornSilk)
    else
      AddDetails(aBroker,'Message: '+aMessage.Text,aMessage.Color);
  finally
    EndUpdate;
  end;
end;

procedure TfrmStockTradeResult.OnNewMessage(const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
  BeginUpdate;
  try
    if aMessage.Color=clDefault then
      AddDetails(aOrder,'Message: '+aMessage.Text,clWebCornSilk)
    else
      AddDetails(aOrder,'Message: '+aMessage.Text,aMessage.Color);

    if AutoScrollOrders then
      taOrders.Locate(taOrdersOrderID.FieldName,GUIDToString(aOrder.GetID),[]);
  finally
    EndUpdate;
  end;
end;

procedure TfrmStockTradeResult.OnNewData(const aSender: IStockBroker; const aSymbol: string);
var
  i: integer;
  aOrder: IStockOrder;
  aOrders: IStockOrderCollection;
  aRecNo: integer;
  aCurrTime : TDateTime;
begin
  aCurrTime:=aSender.GetCurrentTime;
  if (Trunc(FLastEquityTime)<> Trunc(aCurrTime)) and (FLastEquityTime<>0) then
    seEquity.AddXY(Trunc(FLastEquityTime),FLastEquity);


  if FMode=tmReal then
    laElapsedTime.Caption:='Last tick: '+TimeToStr(aCurrTime);

  if (aSender.IsRealTime) and (Mode=tmReal) then
  begin
    aOrders:=aSender.GetOpenedOrders;
    for I := 0 to aOrders.Count-1 do
    begin
      aOrder:=aOrders[i];
      aRecNo:=OrderRecordByID(aOrder.GetID);
      taOrders.DirectSetFieldData(aRecNo,taOrdersProfit,aOrder.GetCurrentProfit);
      taOrders.DirectSetFieldData(aRecNo,taOrdersProfitPt,aSender.PriceToPoint(aOrder.GetSymbol,aOrder.GetCurrentProfit));
      taOrders.RecNo:=taOrders.RecNo;
    end;

    FLastEquityTime:=aCurrTime;
    FLastEquity:=aSender.GetEquity;
  end
  //Оптимизация. GetEquity считается на ходу, поэтому в тестах лучше не злоупотреблять
  else if (Trunc(FLastEquityTime)<> Trunc(aCurrTime)) or (MinutesBetween(aCurrTime,FLastEquityTime)>10) then
  begin
    FLastEquityTime:=aCurrTime;
    FLastEquity:=aSender.GetEquity;
  end;
end;

procedure TfrmStockTradeResult.OnNewOrder(const aOrder: IStockOrder);
var
  aRecNo: integer;
begin
  inc(FOrderCount);

  BeginUpdate;
  try
    aRecNo:=taOrders.DirectInsertRecord;
    taOrders.DirectSetFieldData(aRecNo,taOrdersNo,FOrderCount);
    taOrders.DirectSetFieldData(aRecNo,taOrdersOrderID,GUIDToString(aOrder.GetID));
    taOrders.DirectSetFieldData(aRecNo,taOrdersDetailNo,1);
    taOrders.DirectSetFieldData(aRecNo,taOrdersCreateTime,aOrder.GetCreateTime);

    if aOrder.GetState<>osNothing then
    begin
      taOrders.DirectSetFieldData(aRecNo,taOrdersType,OrderKindNames[aOrder.GetKind]);
      taOrders.DirectSetFieldData(aRecNo,taOrdersStopLoss,aOrder.GetStopLoss);
      taOrders.DirectSetFieldData(aRecNo,taOrdersTakeProfit,aOrder.GetTakeProfit);
      taOrders.DirectSetFieldData(aRecNo,taOrdersTrailingStop,aOrder.GetTrailingStop);
      taOrders.DirectSetFieldData(aRecNo,taOrdersNotes,aOrder.GetNotes);
    end;

    if aOrder.GetState=osPending then
      taOrders.DirectSetFieldData(aRecNo,taOrdersPendingOpenPrice,aOrder.GetPendingOpenPrice)
    else
      taOrders.DirectSetFieldData(aRecNo,taOrdersPendingOpenPrice,0);

    if AutoScrollOrders then
      taOrders.RecNo:=aRecNo+1
    else
      taOrders.RecNo:=taOrders.RecNo; //Обязательно нужно сделать, чтобы прошло Resync. Но просто Resync использовать нельзя!
  finally
    EndUpdate;
    //if taOrders.RecordCount=2 then
    //grOrders.Repaint; //Какая-та неразбериха с миганием на втором ордере
  end;
end;

procedure TfrmStockTradeResult.OnOpenOrderInternal(aOrder: IStockOrder);
var
  j,aRecNo: integer;
  s: string;
  aOrderNo : integer;
begin
  BeginUpdate;
  try
    aRecNo:=OrderRecordByID(aOrder.GetID);
    aOrderNo:=taOrders.DirectGetFieldData(aRecNo,taOrdersNo);

    s:='';
    for j:=0 to High(FCharts) do
      s:=s+IntToStr(FCharts[j].FindBar(aOrder.GetOpenTime))+',';

    //taOrders.DirectSetFieldData(aRecNo,taOrdersOrderID.Value:=GUIDToString(aOrder.GetID);
    taOrders.DirectSetFieldData(aRecNo,taOrdersType,OrderKindNames[aOrder.GetKind]);
    taOrders.DirectSetFieldData(aRecNo,taOrdersLots,aOrder.GetLots);
    taOrders.DirectSetFieldData(aRecNo,taOrdersOpenTime,aOrder.GetOpenTime);
    taOrders.DirectSetFieldData(aRecNo,taOrdersOpenBarNo,StrTrimRight(s,[',']));
    taOrders.DirectSetFieldData(aRecNo,taOrdersOpenPrice,aOrder.GetOpenPrice);
    taOrders.DirectSetFieldData(aRecNo,taOrdersOpenComment,aOrder.GetOpenComment);
    taOrders.DirectSetFieldData(aRecNo,taOrdersState,aOrder.GetState);

    AddDetails(aOrder,'Open at '+PriceToStr(aOrder,aOrder.GetOpenPrice)+'. Comment: '+aOrder.GetOpenComment,$D8FADC);

    if AutoScrollOrders then
      taOrders.RecNo:=aRecNo+1
    else
      taOrders.RecNo:=taOrders.RecNo;

    aRecNo:=taOperationHistory.DirectInsertRecord;
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryNo,taOperationHistory.RecordCount);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryTime,aOrder.GetOpenTime);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryType,OrderKindNames[aOrder.GetKind]);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryOrderNo,aOrderNo);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryLots,aOrder.GetLots);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryPrice,aOrder.GetOpenPrice);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryStopLoss,aOrder.GetStopLoss);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryTakeProfit,aOrder.GetTakeProfit);
    taOperationHistory.RecNo:=taOperationHistory.RecNo; //Обязательно нужно сделать, чтобы прошло Resync. Но просто Resync использовать нельзя!

  finally
    EndUpdate;
  end;
end;

procedure TfrmStockTradeResult.OnActionExecute(aAction: TActionParams);
begin
  aAction.Handled:=(aAction.Action=UIDataStorage.acTradingMoveNextOrder) or (aAction.Action=UIDataStorage.acTradingMovePrevOrder);
  if (aAction.Handled) and (not aAction.TestOnly)  then
  begin
    Assert(grOrders.DataLink.DataSet=taOrders);

    if aAction.Action=UIDataStorage.acTradingMoveNextOrder then
    begin
      taOrders.Next;
      while (taOrdersOpenTime.IsNull) and not taOrders.EOF do
        taOrders.Next;

      if not taOrders.Eof then
        grOrdersDblClick(nil);
    end
    else if aAction.Action=UIDataStorage.acTradingMovePrevOrder then
    begin
      grOrders.DataLink.DataSet.Prior;
      grOrdersDblClick(nil);
    end
  end;
end;

procedure TfrmStockTradeResult.OnActionUpdate(aAction: TActionParams);
begin
  aAction.Handled:=(aAction.Action=UIDataStorage.acTradingMoveNextOrder) or (aAction.Action=UIDataStorage.acTradingMovePrevOrder);
  if aAction.Handled then
  begin
    if not grOrders.IsDBActive then
      aAction.Action.Enabled:=false
    else if aAction.Action=UIDataStorage.acTradingMoveNextOrder then
      aAction.Action.Enabled:=not grOrders.DataLink.DataSet.Eof
    else if aAction.Action=UIDataStorage.acTradingMovePrevOrder then
      aAction.Action.Enabled:=not grOrders.DataLink.DataSet.Bof;
  end;
end;

procedure TfrmStockTradeResult.OnCloseOrderInternal(aOrder: IStockOrder);
var
  aProfit,aBestProfit,aWorstProfit: TStockRealNumber;
  aProfitPts,aBestProfitPts,aWorstProfitPts: integer;
  aDelta : TStockRealNumber;
  aBroker : IStockBroker;
  j: integer;
  s: string;
  aRecNo: integer;
  aOrderNo : integer;
begin
  aBroker:=aOrder.GetBroker;

  aProfitPts:=aBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetCurrentProfit);//-CurrentSpreadPoints;
  aProfit:=aBroker.PriceToMoney(aOrder.GetSymbol,aOrder.GetCurrentProfit,aOrder.GetLots);

  aBestProfitPts:=aBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetBestProfit);//-CurrentSpreadPoints;
  aBestProfit:=aBroker.PriceToMoney(aOrder.GetSymbol,aOrder.GetBestProfit,aOrder.GetLots);

  aWorstProfitPts:=aBroker.PriceToPoint(aOrder.GetSymbol,aOrder.GetWorstProfit);//-CurrentSpreadPoints;
  aWorstProfit:=aBroker.PriceToMoney(aOrder.GetSymbol,aOrder.GetWorstProfit,aOrder.GetLots);

  aDelta:=FStatictic.Balance;
  FStatictic.AddValue(aProfit,aBestProfit,aWorstProfit,aProfitPts,aBestProfitPts,aWorstProfitPts);
  aDelta:=FStatictic.Balance-aDelta;

  chartBalance.AddY(FStatictic.Balance);

  s:='Current balance: '+FormatCurr('0,.00',aBroker.GetBalance)+ ' (';
  if aDelta>0 then
    s:=s+'+';
  s:=s+FormatCurr('0,.00',aDelta)+')';
  laBalance.Caption:=s;

  BeginUpdate;
  try
    aRecNo:=OrderRecordByID(aOrder.GetID);
    aOrderNo:=taOrders.DirectGetFieldData(aRecNo,taOrdersNo);

    s:='';
    for j:=0 to High(FCharts) do
      s:=s+IntToStr(FCharts[j].FindBar(aOrder.GetCloseTime))+',';

    taOrders.DirectSetFieldData(aRecNo,taOrdersCloseTime,aOrder.GetCloseTime);

    taOrders.DirectSetFieldData(aRecNo,taOrdersCloseBarNo,s);
    taOrders.DirectSetFieldData(aRecNo,taOrdersClosePrice,aOrder.GetClosePrice);

    taOrders.DirectSetFieldData(aRecNo,taOrdersProfitPt,aProfitPts);
    taOrders.DirectSetFieldData(aRecNo,taOrdersLots,aOrder.GetLots);
    taOrders.DirectSetFieldData(aRecNo,taOrdersProfit,aProfit);
    taOrders.DirectSetFieldData(aRecNo,taOrdersWorstPt,Round(aWorstProfitPts));
    taOrders.DirectSetFieldData(aRecNo,taOrdersBestPt,Round(aBestProfitPts));
    if aProfitPts>0 then
      taOrders.DirectSetFieldData(aRecNo,taOrdersQuality,Round(aProfit/aBestProfit*100))
    else
      taOrders.DirectSetFieldData(aRecNo,taOrdersQuality,-1);
    taOrders.DirectSetFieldData(aRecNo,taOrdersSubsidencePt,aBroker.PriceToPoint(aOrder.GetSymbol,(aOrder.GetWorstPrice-aOrder.GetOpenPrice)));
    taOrders.DirectSetFieldData(aRecNo,taOrdersBalance, aBroker.GetBalance {FStatictic.Balance});
    taOrders.DirectSetFieldData(aRecNo,taOrdersCloseComment,aOrder.GetCloseComment);
    taOrders.DirectSetFieldData(aRecNo,taOrdersState,aOrder.GetState);

    AddDetails(aOrder,'Close at '+PriceToStr(aOrder,aOrder.GetClosePrice)+'. Comment: '+aOrder.GetCloseComment,$BBF7C2);

    if AutoScrollOrders then
      taOrders.RecNo:=aRecNo+1
    else
      taOrders.RecNo:=taOrders.RecNo;

    aRecNo:=taOperationHistory.DirectInsertRecord;
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryNo,taOperationHistory.RecordCount);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryTime,aOrder.GetCloseTime);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryType,'close');
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryOrderNo,aOrderNo);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryLots,aOrder.GetLots);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryPrice,aOrder.GetClosePrice);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryStopLoss,aOrder.GetStopLoss);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryTakeProfit,aOrder.GetTakeProfit);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryProfit,aProfit);
    taOperationHistory.DirectSetFieldData(aRecNo,taOperationHistoryBalance,aBroker.GetBalance);
    taOperationHistory.RecNo:=taOperationHistory.RecNo; //Обязательно нужно сделать, чтобы прошло Resync. Но просто Resync использовать нельзя!

  finally
    EndUpdate;
  end;

  if MilliSecondsBetween(Now,FLastRepaintTime)>200 then
  begin
    chartBalance.Repaint;
    chEquity.Repaint;

    for j:=0 to High(FCharts) do
      FCharts[j].Repaint;

    //Forms.Application.ProcessMessages;
    FLastRepaintTime:=Now;
  end;
end;

procedure TfrmStockTradeResult.acExportOrdersExecute(Sender: TObject);
var
  aExportInfo: TExportInfo;
  aPropCollection: IStockTraderPropertyCollection;
  aExportString  : TExportString;
  i: integer;
  aStatistic : TStringList;
begin
  aExportInfo:=TExportInfo.Create;

  aPropCollection:=FCurrentTrader.GetProperties;
  aStatistic:=TStringList.Create;
  try
    //Default format
    aExportInfo.DefaultFormat.FontName:='Tahoma';
    aExportInfo.DefaultFormat.FontSize:=7;
    aExportInfo.DefaultFormat.IgnoreFormat:=false;

    //Title
    aExportString:=TExportString.Create(FCurrentTrader.GetCategory+'\'+FCurrentTrader.GetName);
    aExportString.Format.IgnoreFormat:=false;
    aExportString.Format.FontSize:=12;
    aExportString.Format.FontStyle:=[efsBold,efsItalic];
    aExportInfo.HeadStrings.Add(aExportString);

    //Properties
    aExportString:=TExportString.Create(#13#10'Properties');
    aExportString.Format.IgnoreFormat:=false;
    aExportString.Format.FontSize:=10;
    aExportString.Format.FontStyle:=[efsBold];
    aExportInfo.HeadStrings.Add(aExportString);
    for i:=0 to aPropCollection.Count-1 do
    begin
      aExportString:=TExportString.Create(aPropCollection.Items[i].GetCategory+'\'+
                                          aPropCollection.Items[i].GetName+'='+
                                          aPropCollection.Items[i].ValueAsText);
      aExportString.Format.IgnoreFormat:=false;
      aExportString.Format.FontSize:=10;
      aExportInfo.HeadStrings.Add(aExportString);
    end;

    //Statistic
    aExportString:=TExportString.Create(#13#10'Statistic');
    aExportString.Format.IgnoreFormat:=false;
    aExportString.Format.FontSize:=10;
    aExportString.Format.FontStyle:=[efsBold];
    aExportInfo.HeadStrings.Add(aExportString);

    FStatictic.ToStrings(aStatistic);
    for i:=0 to aStatistic.Count-1 do
    begin
      aExportString:=TExportString.Create(aStatistic[i]);
      aExportString.Format.IgnoreFormat:=false;
      aExportString.Format.FontSize:=10;
      aExportInfo.HeadStrings.Add(aExportString);
    end;

    //Orders
    aExportString:=TExportString.Create(#13#10'Order History');
    aExportString.Format.IgnoreFormat:=false;
    aExportString.Format.FontSize:=10;
    aExportString.Format.FontStyle:=[efsBold];
    aExportInfo.HeadStrings.Add(aExportString);
    ExporterManager.ExportOperator.DoExport(grOrders,aExportInfo);
  finally
    aExportInfo.Free;
    aStatistic.Free;
  end;
end;

procedure TfrmStockTradeResult.acExportOrdersUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=FCurrentTrader<>nil;
end;

procedure TfrmStockTradeResult.acGetStatisticExecute(Sender: TObject);
var
  aList: TStringList;
  aDlg: TfmTradingStatisticDialog;
  i: integer;
begin
  aList:=TStringList.Create;
  aDlg:=TfmTradingStatisticDialog.Create(nil);
  try
    FStatictic.ToStrings(aList);
    for i:=0 to aList.Count-1 do
    if aList[i]='' then
      aDlg.AddProperty('','')
    else
      aDlg.AddProperty(aList.Names[i], aList.ValueFromIndex[i]);

    aDlg.Trader:=FCurrentTrader;
    aDlg.ShowModal;
  finally
    aList.Free;
    aDlg.Free;
  end;
end;

procedure TfrmStockTradeResult.acGetStatisticUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=FCurrentTrader<>nil;
end;

procedure TfrmStockTradeResult.AddDetails(const aBroker: IStockBroker; const aText: string; const aColor: TColor);
var
  aGUID: TGUID;
  aDetailRecNo: integer;
begin
  if not LogOrderDetails then
    exit;

  taOrderDetails.DisableControls;
  try
    CreateGUID(aGUID);
    inc(FOrderDetailNo);

    aDetailRecNo:=taOrderDetails.DirectInsertRecord();
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsNo,FOrderDetailNo);
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsText,aText);
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsWhen,aBroker.GetCurrentTime);
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsOrderID,'');
    if aColor<>clDefault then
      taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsColor,aColor);
    taOrderDetails.RecNo:=taOrderDetails.RecNo; //Обязательно нужно сделать, чтобы прошло Resync. Но просто Resync использовать нельзя!
  finally
    taOrderDetails.EnableControls;
  end;
end;

procedure TfrmStockTradeResult.AddDetails(const aOrder: IStockOrder; const aText: string; const aColor: TCOlor);
var
  aGUID: TGUID;
  aRecNo: integer;
  aDetailRecNo: integer;
begin
  if not LogOrderDetails then
    exit;

  aRecNo:=OrderRecordByID(aOrder.GetID);

  BeginUpdate;
  try
    CreateGUID(aGUID);
    inc(FOrderDetailNo);

    //Order Details
    aDetailRecNo:=taOrderDetails.DirectInsertRecord();
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsNo,FOrderDetailNo);
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsText,aText);
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsWhen,aOrder.GetBroker.GetCurrentTime);
    if aOrder.GetSymbol<>'' then
    begin
      taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsPriceAsk,aOrder.GetBroker.GetCurrentPrice(aOrder.GetSymbol, bpkAsk));
      taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsPriceBid,aOrder.GetBroker.GetCurrentPrice(aOrder.GetSymbol, bpkBid));
    end;
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsOrderID,GUIDToString(aOrder.GetID));
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsNoInGroup,taOrders.DirectGetFieldData(aRecNo,taOrdersDetailNo));
    taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsOrderNo,taOrders.DirectGetFieldData(aRecNo,taOrdersNo));
    taOrderDetails.RecNo:=taOrderDetails.RecNo; //Обязательно нужно сделать, чтобы прошло Resync. Но просто Resync использовать нельзя!
    if aColor<>clDefault then
      taOrderDetails.DirectSetFieldData(aDetailRecNo,taOrderDetailsColor,aColor);

    //Orders
    taOrders.DirectSetFieldData(aRecNo,taOrdersTrailingStop,aOrder.GetTrailingStop);
    taOrders.DirectSetFieldData(aRecNo,taOrdersTakeProfit,aOrder.GetTakeProfit);
    taOrders.DirectSetFieldData(aRecNo,taOrdersStopLoss,aOrder.GetStopLoss);
    if aOrder.GetState=osPending then
      taOrders.DirectSetFieldData(aRecNo,taOrdersPendingOpenPrice,aOrder.GetPendingOpenPrice)
    else
      taOrders.DirectSetFieldData(aRecNo,taOrdersPendingOpenPrice,0);
    taOrders.DirectSetFieldData(aRecNo,taOrdersDetailNo,integer(taOrders.DirectGetFieldData(aRecNo,taOrdersDetailNo))+1);
  finally
    EndUpdate;
  end;
end;

procedure TfrmStockTradeResult.grOrdersDblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taOrdersOpenTime.Value;
  aCloseTime:=taOrdersCloseTime.Value;
  if aCloseTime<=0 then
    aCloseTime:=aOpenTime+1/MinsPerDay;

  aShiftState:=KeyboardStateToShiftState;

  FProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfrmStockTradeResult.grOrdersChangeRecord(Sender: TObject);
begin
  if (taOrderDetailsOrderID.Value<>taOrdersOrderID.Value) and (taOrderDetails.Filtered) then
  begin
    tmFilterDetails.Enabled:=false;
    tmFilterDetails.Enabled:=true;
  end;
end;

procedure TfrmStockTradeResult.EndUpdate;
begin
  taOrders.EnableControls;
  taOrderDetails.EnableControls;
  taOperationHistory.EnableControls;
end;

function TfrmStockTradeResult.GetAutoScroll: boolean;
begin
  result:=acAutoScroll.Checked;
end;

procedure TfrmStockTradeResult.Gotoend1Click(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taOrdersCloseTime.Value;
  aCloseTime:=aOpenTime+MinuteAsDateTime;
  aShiftState:=KeyboardStateToShiftState;
  FProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfrmStockTradeResult.Gotothebeginning1Click(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taOrdersOpenTime.Value;
  aCloseTime:=aOpenTime+MinuteAsDateTime;
  aShiftState:=KeyboardStateToShiftState;
  FProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfrmStockTradeResult.BeginUpdate;
begin
  taOrders.DisableControls;
  taOrderDetails.DisableControls;
  taOperationHistory.DisableControls;
end;

procedure TfrmStockTradeResult.taOperationHistoryAfterOpen(DataSet: TDataSet);
begin
  grOperationHistory.ColumnStates.LoadColumnStates;
end;

procedure TfrmStockTradeResult.taOperationHistoryBeforeClose(DataSet: TDataSet);
begin
  grOperationHistory.ColumnStates.SaveColumnStates;
end;

procedure TfrmStockTradeResult.taOrderDetailsAfterOpen(DataSet: TDataSet);
begin
  grOrderDetails.ColumnStates.LoadColumnStates;
end;

procedure TfrmStockTradeResult.taOrderDetailsBeforeClose(DataSet: TDataSet);
begin
  grOrderDetails.ColumnStates.SaveColumnStates;
end;

procedure TfrmStockTradeResult.taOrderDetailsFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept:=taOrderDetailsOrderID.Value = taOrdersOrderID.Value;
end;

procedure TfrmStockTradeResult.taOrdersAfterOpen(DataSet: TDataSet);
begin
  grOrders.ColumnStates.LoadColumnStates;
end;

procedure TfrmStockTradeResult.taOrdersBeforeClose(DataSet: TDataSet);
begin
  grOrders.ColumnStates.SaveColumnStates;
end;

procedure TfrmStockTradeResult.taOrdersOpenTimeGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  if not Sender.IsNull then
    Text:=DefaultFormatter.DateTimeToStr(Sender.AsDateTime,false,true,true);
end;

procedure TfrmStockTradeResult.tcTabsChange(Sender: TObject);
begin
  pcPages.ActivePageIndex:=tcTabs.TabIndex;
  pcPagesChange(nil); //Автоматически не срабатываеть
end;

procedure TfrmStockTradeResult.tmFilterDetailsTimer(Sender: TObject);
begin
  tmFilterDetails.Enabled:=false;
  if (taOrderDetails.Active) and (taOrderDetailsOrderID.Value<>taOrdersOrderID.Value) and (taOrderDetails.Filtered) then
    taOrderDetails.First;
end;

procedure TfrmStockTradeResult.grJournalDblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taOrderDetailsWhen.Value;
  aCloseTime:=aOpenTime+1/MinsPerDay;

  aShiftState:=KeyboardStateToShiftState;
  FProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfrmStockTradeResult.grOperationHistoryDblClick(Sender: TObject);
var
  aOpenTime,aCloseTime: TDateTime;
  aShiftState: TShiftState;
begin
  aOpenTime:=taOperationHistoryTime.Value;
  aCloseTime:=aOpenTime+1/MinsPerDay;
  aShiftState:=KeyboardStateToShiftState;
  FProject.HilightOnCharts(aOpenTime,aCloseTime,not (ssShift in aShiftState));
end;

procedure TfrmStockTradeResult.grOrderDetailsBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  if not taOrderDetailsColor.IsNull then
    TEditDBGrid(Sender).Canvas.Brush.Color:=taOrderDetailsColor.Value;
end;

procedure TfrmStockTradeResult.grOrdersBeforeDrawColumnCell(Sender: TObject; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  if Column.Field=taOrdersProfitPt then
    TEditDBGrid(Sender).Canvas.Brush.Color:=clWebGhostWhite
  else if not taOrdersColor.IsNull then
  begin
    TEditDBGrid(Sender).Canvas.Brush.Color:=taOrdersColor.AsInteger;
  end;

  if not taOrdersNo.IsNull  then
  begin
    //Это ничего
    if taOrdersState.Value=integer(osNothing) then
    begin
  //    TEditDBGrid(Sender).Canvas.Font.Color:=clGrayText;
      if Mode=tmReal then
      begin
        TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightgrey;
      end;
    end
    //Это Pending Order
    else if taOrdersState.Value=integer(osPending) then
    begin
      TEditDBGrid(Sender).Canvas.Font.Color:=clGrayText;
      if Mode=tmReal then
      begin
        TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLightCyan;
      end;
    end
    //Это Opened Order
    else if taOrdersState.Value=integer(osOpened) then
    begin
      TEditDBGrid(Sender).Canvas.Font.Color:=clWebBrown;
      if Mode=tmReal then
        TEditDBGrid(Sender).Canvas.Brush.Color:=clWebLemonChiffon;
    end;
  end;
end;

procedure TfrmStockTradeResult.acAutoScrollExecute(Sender: TObject);
begin
  TAction(Sender).Checked:= not TAction(Sender).Checked;
end;

procedure TfrmStockTradeResult.acChooseColumnsExecute(Sender: TObject);
begin
  if grOrders.ShowColumnsDlg then
  begin
    grOrders.ColumnStates.SaveColumnStates;
  end;
end;

procedure TfrmStockTradeResult.acChooseColumnsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=grOrders.IsDBActive;
end;

procedure TfrmStockTradeResult.acFilterExecute(Sender: TObject);
var
  i: integer;
  aFilter: string;
  aFiltered: boolean;
  aFields: TStringList;

begin
  aFiltered:= not acFilter.Checked;

  if aFiltered then
  begin
    with TfmFilterDialog.Create(nil) do
    try
      aFields:=TStringList.Create;
      try
        for i := 0 to taOrders.Fields.Count - 1 do
          aFields.Add(taOrders.Fields[i].FieldName);
        SetFields(aFields);
      finally
        aFields.Free;
      end;

      mmFilter.Lines.Text:=taOrders.Filter;
      if ShowModal<>mrOk then
        aFiltered:=false
      else
        aFilter:=mmFilter.Lines.Text;
    finally
      Free;
    end;
  end;

  try
    if aFiltered then
      taOrders.Filter:=aFilter;
    taOrders.Filtered:=aFiltered;
  except
    on E:Exception do
    begin
      Workspace.MainFrame.HandleException(E);
      taOrders.Filtered:=false;
    end;
  end;

  acFilter.Checked:=taOrders.Filtered;
//wewerer
end;

procedure TfrmStockTradeResult.acFitColumnsExecute(Sender: TObject);
begin
  grOrders.ColumnSizes.FitColumnsByContents;
end;

end.


