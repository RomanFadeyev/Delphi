{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Источники данных, использующее для историю котировок
            (IStockDataStorage)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.HC.StockDataSource;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, AppEvnts, Controls, Serialization, FC.Definitions,
       FC.StockData.StockDataSource,StockChart.Definitions;

type
  //---------------------------------------------------------------------------
  //Реализация DataSource для History Center

  TStockDataSource_HistoryCenter=class(TStockDataSourceMemory_B,IStockDataSourceWriteable,IStockDataSource)
  private
    FDS      : IStockDataQuery;
    FDSEventHandler: IStockDataStorageEventHandler;
    FAppEvents     : TApplicationEvents;
    FLastDateTime  : TDateTime;
    FStockDataStorage: IStockDataStorage;
    FStartFrom   : TDateTime;

    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure LogMessage(const aMessage: string);
  protected
    procedure OnLoadPortion(index:integer); override;
    procedure CheckFreeDS;

    procedure LoadDataFromStorage;
  public
    function  RecordCount: integer; override;

    procedure OnAddRecord(const aItem: ISCInputData);
    procedure OnEditRecord(const aItem: ISCInputData);
    procedure OnDeleteRecord(const aItem: ISCInputData);

    function GetDataDateTime(index: integer): TDateTime;     override;

    //from IStockDataSourceWriteable
    procedure UpdateData(const aItem: ISCInputData);
    function Tag: string; override;

    property StartLoadingFrom: TDateTime read FStartFrom;

    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; const aStorage: IStockDataStorage; const aStartLoadingFrom: TDateTime);
    destructor Destroy; override;
  end;

  TStockDataSource_HistoryCenter_StorageEventHadler = class (TInterfacedObject,IStockDataStorageEventHandler)
  private
    FOwner: TStockDataSource_HistoryCenter;
  public
    procedure OnChangeData(const aSymbolID: string; const aInterval: TStockTimeInterval; const aItem: ISCInputData; aType: TStockDataModificationType);
    procedure OnChangeCalendarData;

    procedure OnMergeData(const aSymbolID: string; const aInterval: TStockTimeInterval; const aItems: ISCInputDataCollection);

    constructor Create(aOwner: TStockDataSource_HistoryCenter);
  end;

implementation
  uses Math,BaseUtils, SystemService, DateUtils, FC.DataUtils, FC.Singletons;

{ TStockDataSource_HistoryCenter }

constructor TStockDataSource_HistoryCenter.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; const aStorage: IStockDataStorage; const aStartLoadingFrom: TDateTime);
begin
  inherited Create(aConnection, aSymbol,aInterval);

  FStockDataStorage:=aStorage;
  FDSEventHandler:=TStockDataSource_HistoryCenter_StorageEventHadler.Create(self);
  FStockDataStorage.AddEventHandler(FDSEventHandler);
  FStartFrom:=aStartLoadingFrom;

  LoadDataFromStorage;

  if RecordCount>0 then
  begin
    FAppEvents:=TApplicationEvents.Create(nil);
    FAppEvents.OnIdle:=OnIdle;
  end;

  LogMessage('------------------------------------------------------------------------------------------------');
end;

destructor TStockDataSource_HistoryCenter.Destroy;
begin
  if FDSEventHandler<>nil then
    FStockDataStorage.RemoveEventHandler(FDSEventHandler);
  FDSEventHandler:=nil;
  FreeAndNil(FAppEvents);
  FStockDataStorage:=nil;

  inherited;
end;

function TStockDataSource_HistoryCenter.GetDataDateTime(index: integer): TDateTime;
begin
  //Оптмизация
  if (Index=RecordCount-1) and (FLastDateTime<>-1) then
    result:=FLastDateTime
  else
    result:=inherited GetDataDateTime(index);
end;

function TStockDataSource_HistoryCenter.RecordCount: integer;
begin
  if FDS=nil then
    result:=FRecordList.Count
  else
    result:=FDS.RecordCount;
end;

function TStockDataSource_HistoryCenter.Tag: string;
begin
  result:=FloatToStr(FStartFrom);
end;

procedure TStockDataSource_HistoryCenter.UpdateData(const aItem: ISCInputData);
begin
  FStockDataStorage.UpdateBar(StockSymbol.Name,StockSymbol.TimeInterval,aItem);
  FLastDateTime:=-1;
end;

procedure TStockDataSource_HistoryCenter.OnLoadPortion(index: integer);
var
  i: integer;
  aCursor: TWaitCursor;
begin
  aCursor:=nil;
  if (index-FRecordList.Count)>1000 then
    aCursor:=TWaitCursor.Create;

  try
    for i:=FRecordList.Count to index do
    begin
      FRecordList.Add(TStockDataRecord.Create(
                          FDS.GetDataDateTime,
                          FDS.GetDataOpen,
                          FDS.GetDataHigh,
                          FDS.GetDataLow,
                          FDS.GetDataClose,
                          FDS.GetDataVolume));

      FDS.Next;
    end;
  finally
    FreeAndNil(aCursor);
  end;

  CheckFreeDS;
end;

procedure TStockDataSource_HistoryCenter.CheckFreeDS;
begin
  if FDS<>nil then
  begin
    if FDS.RecordCount=0 then
      FDS:=nil
    else if FDS.RecordCount=FRecordList.Count then
      FDS:=nil;
  end;
end;

procedure TStockDataSource_HistoryCenter.OnAddRecord(const aItem: ISCInputData);
var
  i: integer;
begin
 FLastDateTime:=-1;

 //Если вставка внутрь списка
 if (RecordCount>0) and (GetDataDateTime(RecordCount-1)>=aItem.DataDateTime) then
 begin
   i:=FindBestMatched(aItem.DataDateTime);

   //Если есть точное совадение - это ошибка, нельзя вставить две одинаковые даты
   if (i<>-1) and (GetDataDateTime(i)=aItem.DataDateTime) then
     raise EStockError.Create('Date unique violation');

   if i=-1 then //В самое начало
     i:=0
   //Проверим чтобы не вставить справа
   else if GetDataDateTime(i)>aItem.DataDateTime then
     i:=max(0,i-1);

   CheckFreeDS; Assert(FDS=nil);//DS уже быть не должно, иначе как вставлять в список, если он недокачан

   FRecordList.Insert(i,TStockDataRecord.Create(
                        aItem.DataDateTime,
                        aItem.DataOpen,
                        aItem.DataHigh,
                        aItem.DataLow,
                        aItem.DataClose,
                        aItem.DataVolume));

   LogMessage('Insert item with time '+DateTimeToStr(aItem.DataDateTime)+' into '+IntToStr(i));
   RaiseChangeDataEvent(-1,idcctChangeAll);
 end
 //Если вставка в конец списка
 else begin
   CheckFreeDS; Assert(FDS=nil);//DS уже быть не должно, иначе как вставлять в список, если он недокачан

   FRecordList.Add(TStockDataRecord.Create(
                      aItem.DataDateTime,
                      aItem.DataOpen,
                      aItem.DataHigh,
                      aItem.DataLow,
                      aItem.DataClose,
                      aItem.DataVolume));
   LogMessage('Add item with time '+DateTimeToStr(aItem.DataDateTime)+' to the end');
   RaiseChangeDataEvent(RecordCount-1,idcctAdd);
 end;
end;

procedure TStockDataSource_HistoryCenter.OnDeleteRecord(const aItem: ISCInputData);
var
  i: integer;
begin
  FLastDateTime:=-1;

  //Здесь вытаскивается все в кеш и DS отрывается
  GetRecord(RecordCount);
  i:=IndexOf(aItem.DataDateTime);
  if i=-1 then
    raise EStockError.Create('Bad stock data');

  FRecordList.Delete(i);
  RaiseChangeDataEvent(i,idcctDelete);
end;

procedure TStockDataSource_HistoryCenter.OnEditRecord(const aItem: ISCInputData);
var
  i: integer;
  aRecord: TStockDataRecord;
begin
  FLastDateTime:=-1;

  i:=IndexOf(aItem.DataDateTime);
  if i=-1 then
  begin
    LogMessage('!!!! Cannot find item with time '+DateTimeToStr(aItem.DataDateTime));
    raise EStockError.CreateFmt('Data with time %s not found',[DateTimeToStr(aItem.DataDateTime)]);
  end;

  aRecord:=GetRecord(i);

  //Если даты разные, это какой-то косяк в хранении данных, в самой программе
  if (not SameDateTime(aRecord.DataDateTime,aItem.DataDateTime)) then
    raise EAssertionFailed.Create('Different dates: '+ DateTimeToStr(aRecord.DataDateTime)+','+DateTimeToStr(aItem.DataDateTime));


  if(aRecord.DataOpen<>aItem.DataOpen) or
    (aRecord.DataHigh<>aItem.DataHigh) or
    (aRecord.DataLow<>aItem.DataLow) or
    (aRecord.DataClose<>aItem.DataClose) or
    (aRecord.DataVolume<>aItem.DataVolume) then
  begin
    aRecord.DataOpen:=aItem.DataOpen;
    aRecord.DataHigh:=aItem.DataHigh;
    aRecord.DataLow:=aItem.DataLow;
    aRecord.DataClose:=aItem.DataClose;
    aRecord.DataVolume:=aItem.DataVolume;
    RaiseChangeDataEvent(i,idcctEdit);
  end;

  LogMessage('Edit item with time '+DateTimeToStr(aItem.DataDateTime));
end;

procedure TStockDataSource_HistoryCenter.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if FRecordList.Count<Self.RecordCount then
  begin
     //Провоцируем загрузку данных
     self.GetRecord(Min(FRecordList.Count+10,Self.RecordCount-1));
     Done:=true;
  end;

  if FRecordList.Count>=Self.RecordCount then
    FreeAndNil(FAppEvents);
end;

procedure TStockDataSource_HistoryCenter.LoadDataFromStorage;
var
  s: string;
  aInfo : TStockSymbolInfo;
begin
  FRecordList.Clear;


  aInfo:=FStockDataStorage.GetSymbolInfo(StockSymbol.Name);
  FPricePrecision:=aInfo.Digits;
  FPricesInPoint:=Round(1/aInfo.Point);

  s:='';
  if FStartFrom>0 then
    s:=FStockDataStorage.CreateDataFilterAdapter.DateGE(FStartFrom);

  FDS:=FStockDataStorage.QueryStockData(StockSymbol.Name,StockSymbol.TimeInterval,s);
  FLastDateTime:=FStockDataStorage.GetLastDateTime(StockSymbol.Name,StockSymbol.TimeInterval);

  FRecordList.Capacity:=Round(FDS.RecordCount*1.2);
  CheckFreeDS;
end;

procedure TStockDataSource_HistoryCenter.LogMessage(const aMessage: string);
begin
  //ssLogMessage('C:\'+StockSymbol.Name+StockSymbol.GetTimeIntervalName+'.log',aMessage,[]);
end;

{ TStockDataSource_HistoryCenter_StorageEventHadler }

constructor TStockDataSource_HistoryCenter_StorageEventHadler.Create(aOwner: TStockDataSource_HistoryCenter);
begin
  inherited Create;
  FOwner:=aOwner;
end;

procedure TStockDataSource_HistoryCenter_StorageEventHadler.OnChangeCalendarData;
begin

end;

procedure TStockDataSource_HistoryCenter_StorageEventHadler.OnChangeData(const aSymbolID: string;
  const aInterval: TStockTimeInterval; const aItem: ISCInputData; aType: TStockDataModificationType);
begin
  if FOwner.StockSymbol.IsEqual(aSymbolID,aInterval) then
  begin
    case aType of
     idcctAdd:
       FOwner.OnAddRecord(aItem);
     idcctEdit:
       FOwner.OnEditRecord(aItem);
     idcctDelete:
       FOwner.OnDeleteRecord(aItem);
     idcctChangeAll:
     begin
       FOwner.LoadDataFromStorage;
       FOwner.RaiseChangeDataEvent(-1,aType);
     end;
    end;
  end;
end;

procedure TStockDataSource_HistoryCenter_StorageEventHadler.OnMergeData(const aSymbolID: string; const aInterval: TStockTimeInterval; const aItems: ISCInputDataCollection);
var
  i: integer;
  aItem : ISCInputData;
begin
  if FOwner.StockSymbol.IsEqual(aSymbolID,aInterval) then
  begin
    for i := 0 to aItems.Count - 1 do
    begin
      aItem:=aItems[i];
      if FOwner.IndexOf(aItem.DataDateTime)=-1 then
        FOwner.OnAddRecord(aItem)
      else
        FOwner.OnEditRecord(aItem);
    end;
  end;
end;

end.


