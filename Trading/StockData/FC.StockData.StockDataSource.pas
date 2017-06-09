{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Базовый класс реаизации источника данных (IStockDataSource)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.StockDataSource;
{$I Compiler.inc}

interface
  uses Windows, Controls, SysUtils,Classes,Contnrs, DB,
  StockChart.Definitions,
  FC.Definitions,FC.Singletons,
  FC.StockData.DataSourceToInputDataCollectionMediator;

type
  //Базовый класс источников данных
  TStockDataSource_B = class (TStockInterfacedObject,IStockDataSource)
  private
    FConnection: IStockDataSourceConnection;
    FEventHandlers : TInterfaceList;
    FStockSymbol   : TStockSymbol;
    FFinder        : TStockDataSourceToInputDataCollectionMediator;

    function GetEventHandler(index: integer): IStockDataSourceEventHandler;
  protected
    FPricePrecision    : integer;
    FPricesInPoint     : integer;

    function EventHandlerCount: integer;
    property EventHandlers[index: integer]:IStockDataSourceEventHandler read GetEventHandler;

    procedure RaiseChangeDataEvent(index: integer; aType: TStockDataModificationType);
  public
    //------ from IStockDataSource
    function  RecordCount: integer;     virtual; abstract;
    function IndexOf(const aDateTime:TDateTime): integer;
    function FindBestMatched(const aDateTime:TDateTime): integer;
    procedure ResetSearchCache;

    procedure FetchAll; virtual;

    //Непосредственно данные
    function GetDataDateTime(index: integer): TDateTime;           virtual; abstract;
    function GetDataOpen(index: integer)  : TStockRealNumber;      virtual; abstract;
    function GetDataHigh(index: integer)  : TStockRealNumber;      virtual; abstract;
    function GetDataLow(index: integer)   : TStockRealNumber;      virtual; abstract;
    function GetDataClose(index: integer) : TStockRealNumber;      virtual; abstract;
    function GetDataVolume(index: integer): integer;               virtual; abstract;
    //------------------------------

    function Connection: IStockDataSourceConnection;
    function StockSymbol:TStockSymbol;
    function Tag: string; virtual;

    //Кол-во точек после запятой
    function GetPricePrecision: integer;
    //Коэффициент трансформации из точки в цену, для EURUSD=10000, для USDJPY=100
    function GetPricesInPoint: integer;

    //Округление цены до PricePrecision
    function RoundPrice(const aPrice: TSCRealNumber) : TSCRealNumber;
    //Перевод цены в пункты (умножение на PricesInPoint)
    function PriceToPoint(const aPrice: TSCRealNumber) : integer;
    //Перевод пунктов в цену (деление на PricesInPoint)
    function PointToPrice(const aPoint: integer) : TSCRealNumber;

    procedure AddEventHandler(const aHandler: IStockDataSourceEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockDataSourceEventHandler);

    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval);
    destructor Destroy; override;

    procedure AfterConstruction; override;
  end;

  //Класс для чтения данных из потока
  TStockDataRecord = class
  public
    DataDateTime: TDateTime;
    DataOpen    : TStockRealNumber;
    DataHigh    : TStockRealNumber;
    DataLow     : TStockRealNumber;
    DataClose   : TStockRealNumber;
    DataVolume  : integer;

    constructor Create(aDataDateTime: TDateTime;
                       aDataOpen    : TStockRealNumber;
                       aDataHigh    : TStockRealNumber;
                       aDataLow     : TStockRealNumber;
                       aDataClose   : TStockRealNumber;
                       aDataVolume  : integer);
  end;

  { TStockDataSourceMemory_B }

  TStockDataSourceMemory_B = class (TStockDataSource_B)
  protected
    FRecordList: TObjectList; //of TStockDataRecord

    function GetRecord(index: integer):TStockDataRecord; inline;
    procedure OnLoadPortion(index:integer); virtual;
  public
    procedure FetchAll; override;

    function Add(const aDataDateTime: TDateTime;
                 const aDataOpen, aDataHigh, aDataLow, aDataClose: TStockRealNumber;
                 aDataVolume: integer):TStockDataRecord;

    //------ from IStockDataSource
    function  RecordCount: integer;                          override;
    function GetDataDateTime(index: integer): TDateTime;     override;
    function GetDataOpen(index: integer)  : TStockRealNumber;override;
    function GetDataHigh(index: integer)  : TStockRealNumber;override;
    function GetDataLow(index: integer)   : TStockRealNumber;override;
    function GetDataClose(index: integer) : TStockRealNumber;override;
    function GetDataVolume(index: integer): integer;         override;

    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval);
    destructor Destroy; override;
  end;

  { TStockDataSource_StreamToMemory }

  TStockDataSource_StreamToMemory = class (TStockDataSourceMemory_B)
  end;

  EDataSourceException = class (EStockError);

function OnSortRecords(Item1, Item2: Pointer): Integer;

implementation
  uses Math,BaseUtils,DateUtils,SystemService,FC.DataUtils, StockChart.Obj,
       FC.StockData.StockDataSourceRegistry;

function OnSortRecords(Item1, Item2: Pointer): Integer;
begin
  result:=sign(TStockDataRecord(Item1).DataDateTime-TStockDataRecord(Item2).DataDateTime);
end;


{ TStockDataSource_B }

procedure TStockDataSource_B.AddEventHandler(const aHandler: IStockDataSourceEventHandler);
begin
  FEventHandlers.Add(aHandler);
end;

procedure TStockDataSource_B.RemoveEventHandler(const aHandler: IStockDataSourceEventHandler);
begin
  FEventHandlers.Remove(aHandler);
end;

procedure TStockDataSource_B.ResetSearchCache;
begin
  FFinder.ResetSearchCache;
end;

procedure TStockDataSource_B.AfterConstruction;
begin
  inherited;
  StockLoadedDataSourceRegistry.AddDataSource(self);
end;

function TStockDataSource_B.Connection: IStockDataSourceConnection;
begin
  result:=FConnection;
end;

function TStockDataSourceMemory_B.Add(const aDataDateTime: TDateTime; const aDataOpen, aDataHigh, aDataLow,
  aDataClose: TStockRealNumber; aDataVolume: integer): TStockDataRecord;
begin
  result:=
  TStockDataRecord.Create(
                    aDataDateTime,
                    aDataOpen,
                    aDataHigh,
                    aDataLow,
                    aDataClose,
                    aDataVolume);

  FRecordList.Add(result);                    
end;

constructor TStockDataSourceMemory_B.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval);
begin
  inherited Create(aConnection, aSymbol,aInterval);
  FRecordList:=TObjectList.Create;
end;

destructor TStockDataSourceMemory_B.Destroy;
begin
  FreeAndNil(FRecordList);
  inherited;
end;

procedure TStockDataSourceMemory_B.FetchAll;
begin
  inherited;
  if RecordCount>0 then
    GetRecord(RecordCount-1);
end;

function TStockDataSource_B.GetEventHandler(index: integer): IStockDataSourceEventHandler;
begin
  result:=FEventHandlers[index] as IStockDataSourceEventHandler;
end;

function TStockDataSource_B.GetPricePrecision: integer;
begin
  result:=FPricePrecision;
end;

function TStockDataSource_B.GetPricesInPoint: integer;
begin
  result:=FPricesInPoint;
end;

function TStockDataSource_B.EventHandlerCount: integer;
begin
  result:=FEventHandlers.Count;
end;


procedure TStockDataSource_B.RaiseChangeDataEvent(index: integer; aType: TStockDataModificationType);
var
  i: integer;
begin
  for i:=0 to EventHandlerCount-1 do
    EventHandlers[i].OnChangeData(self,index,aType);
end;

constructor TStockDataSource_B.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval);
begin
  FStockSymbol:=TStockSymbol.Create(aSymbol,aInterval);

  FEventHandlers:=TInterfaceList.Create;

  FConnection:=aConnection;

  //TODO

  FPricesInPoint := TKnownSymbolRegistry.GetPricesInPoint(aSymbol); //Коэффициент
  FPricePrecision := TKnownSymbolRegistry.GetPricePrecision(aSymbol);    //Кол-во знаков после запятой

  FFinder:=TStockDataSourceToInputDataCollectionMediator.Create(self,nil);
  FFinder.BeginUpdate;
  Assert(RefCount=2);
  _Release; //Это потому что медиатор подвесился
end;

destructor TStockDataSource_B.Destroy;
begin
  StockLoadedDataSourceRegistry.RemoveDataSource(self);

  FreeAndNil(FEventHandlers);
  FreeAndNil(FStockSymbol);

  SetInvalidRefCount; //Из-за принудильного _Release в конструкторе
  FreeAndNil(FFinder);
  inherited;
end;

function TStockDataSource_B.IndexOf(const aDateTime: TDateTime): integer;
var
  aStart,aStop: TDateTime;
begin
  TStockDataUtils.AlignTime(aDateTime,FStockSymbol.TimeInterval,aStart,aStop);
  Assert(SameDateTime(FStockSymbol.GetTimeIntervalValue/MinsPerDay,FFinder.GetGradation));
  result:=FFinder.FindExactMatched(aStart);
end;

procedure TStockDataSource_B.FetchAll;
begin

end;

function TStockDataSource_B.FindBestMatched(const aDateTime: TDateTime): integer;
begin
  result:=FFinder.FindBestMatched(aDateTime,0,RecordCount-1);
end;

function TStockDataSource_B.StockSymbol: TStockSymbol;
begin
  result:=FStockSymbol;
end;

function TStockDataSource_B.Tag: string;
begin
  result:='';
end;

//Округление цены до PricePrecision
function TStockDataSource_B.RoundPrice(const aPrice: TSCRealNumber) : TSCRealNumber;
begin
  result:=RoundTo(aPrice,-FPricePrecision);
end;

//Перевод цены в пункты (умножение на PricesInPoint)
function TStockDataSource_B.PriceToPoint(const aPrice: TSCRealNumber) : integer;
begin
  result:=Round(aPrice*FPricesInPoint);
end;

//Перевод пунктов в цену (деление на PricesInPoint)
function TStockDataSource_B.PointToPrice(const aPoint: integer) : TSCRealNumber;
begin
  result:=aPoint/FPricesInPoint;
end;

{ TStockDataRecord }

constructor TStockDataRecord.Create(aDataDateTime: TDateTime; aDataOpen,
  aDataHigh, aDataLow, aDataClose: TStockRealNumber; aDataVolume: integer);
begin
  try
    ASSERT(aDataOpen<=aDataHigh);
    ASSERT(aDataClose<=aDataHigh);
    ASSERT(aDataOpen>=aDataLow);
    ASSERT(aDataClose>=aDataLow);
  except
    raise;

  end;

  DataDateTime:=TStockDataUtils.RefineTime(aDataDateTime);
  DataOpen    :=aDataOpen;
  DataHigh    :=aDataHigh;
  DataLow     :=aDataLow;
  DataClose   :=aDataClose;
  DataVolume  :=aDataVolume;
end;

{ TStockDataSourceMemory_B }

function TStockDataSourceMemory_B.GetDataClose(index: integer): TStockRealNumber;
begin
  result:=GetRecord(index).DataClose;
end;

function TStockDataSourceMemory_B.GetDataDateTime(index: integer): TDateTime;
begin
  result:=GetRecord(index).DataDateTime;
end;

function TStockDataSourceMemory_B.GetDataHigh(index: integer): TStockRealNumber;
begin
  result:=GetRecord(index).DataHigh;
end;

function TStockDataSourceMemory_B.GetDataLow(index: integer): TStockRealNumber;
begin
  result:=GetRecord(index).DataLow;
end;

function TStockDataSourceMemory_B.GetDataOpen(index: integer): TStockRealNumber;
begin
  result:=GetRecord(index).DataOpen;
end;

function TStockDataSourceMemory_B.GetDataVolume(index: integer): integer;
begin
  result:=GetRecord(index).DataVolume;
end;

function TStockDataSourceMemory_B.RecordCount: integer;
begin
  result:=FRecordList.Count;
end;

function TStockDataSourceMemory_B.GetRecord(index: integer): TStockDataRecord;
begin
  if index>=RecordCount then
    raise EDataSourceException.Create('Index out of range');

  if index<0 then
    raise EDataSourceException.Create('Index out of range');

  if index>=FRecordList.Count then
    OnLoadPortion(index);

  if index>=FRecordList.Count then
    raise EAlgoError.Create;

  result:=TStockDataRecord(FRecordList.List[index]);
end;

procedure TStockDataSourceMemory_B.OnLoadPortion(index: integer);
begin
  raise EAlgoError.Create;
end;

end.


