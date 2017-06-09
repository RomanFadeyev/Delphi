{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный клас-переходник между библиотечным интерфейсом
            предоставления данных для чарта (ISCInputDataCollection) и
            выборкой данных (IStockDataQuery)
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.DataQueryToInputDataCollectionMediator;
{$I Compiler.inc}

interface
  uses BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj;

type
  //----------------------------------------------------------------------------
  TStockDataQueryToInputDataCollectionMediator = class (TSCInputDataCollection)
  private
    FDS                : IStockDataQuery;

    procedure CheckDataLoadedFor(index: integer);
  public
    constructor Create(aDS:IStockDataQuery; AOwner: ISCChangeNotifier);
    destructor Destroy; override;

    function  ISCInputDataCollection_GetItem(Index: Integer): ISCInputData; override;

    function  DirectGetItem_DataDateTime(index: integer):TDateTime; override;
    function  DirectGetItem_DataClose(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataHigh(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataLow(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataOpen(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataVolume(index: integer): integer; override;

    function  Count: integer; override;

    procedure SetDataQuery(aDS: IStockDataQuery);
    function  GetDataQuery: IStockDataQuery;
  end;

implementation
  uses Math;

{ TStockDataQueryToInputDataCollectionMediator }

constructor TStockDataQueryToInputDataCollectionMediator.Create(aDS: IStockDataQuery; AOwner: ISCChangeNotifier);
begin
  inherited Create(AOwner,aDS.StockSymbol.GetTimeIntervalValue/MinsPerDay);
  SetDataQuery(aDS);
end;

destructor TStockDataQueryToInputDataCollectionMediator.Destroy;
begin
  inherited;
end;

procedure TStockDataQueryToInputDataCollectionMediator.CheckDataLoadedFor(index: integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise ESCListIndexError.Create(Index);

  if Index>=FList.Count then
    while Index<>FList.Count-1 do
    begin
      AddItemInternal(FDS.GetDataDateTime,FDS.GetDataOpen,FDS.GetDataHigh,FDS.GetDataLow,FDS.GetDataClose,FDS.GetDataVolume);
      FDS.Next;
    end;
end;

function TStockDataQueryToInputDataCollectionMediator.Count: integer;
begin
  if FDS=nil then
    result:=0
  else
    result:=FDS.RecordCount;
end;

function TStockDataQueryToInputDataCollectionMediator.ISCInputDataCollection_GetItem(Index: Integer): ISCInputData;
begin
  CheckDataLoadedFor(index);
  result:=inherited ISCInputDataCollection_GetItem(index);
end;

procedure TStockDataQueryToInputDataCollectionMediator.SetDataQuery(aDS: IStockDataQuery);
begin
  if FDS<>aDS then
  begin
    FDS:=aDS;
    if aDS.StockSymbol.IsInfoAvailable then
    begin
      SetPricePrecision(aDS.StockSymbol.GetInfo.Digits);
      SetPricesInPoint(Round(1/aDS.StockSymbol.GetInfo.Point));
    end;
    OnItemsChanged;
  end;
end;

function TStockDataQueryToInputDataCollectionMediator.GetDataQuery: IStockDataQuery;
begin
  result:=FDS;
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataOpen(index: integer): TStockRealNumber;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataOpen(index)
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataHigh(index: integer): TStockRealNumber;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataHigh(index)
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataDateTime(index: integer): TDateTime;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataDateTime(index)
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataVolume(index: integer): integer;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataVolume(index)
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataClose(index: integer): TStockRealNumber;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataClose(index)
end;

function TStockDataQueryToInputDataCollectionMediator.DirectGetItem_DataLow(index: integer): TStockRealNumber;
begin
  CheckDataLoadedFor(index);
  result:=inherited DirectGetItem_DataLow(index)
end;


end.
