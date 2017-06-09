{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный клас-переходник между библиотечным интерфейсом
            предоставления данных для чарта (ISCInputDataCollection) и
            источником данных (IStockDataSource)
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.DataSourceToInputDataCollectionMediator;
{$I Compiler.inc}

interface
  uses BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj;

type
  //----------------------------------------------------------------------------
  TEventMediator = class;

  IStockDataSourceToInputDataCollectionMediator = interface
  ['{F10C6A71-9FFF-4137-B45A-E3B277466A90}']
    procedure SetDataSource(aDS: IStockDataSource);
    function  GetDataSource: IStockDataSource;
  end;

  TStockDataSourceToInputDataCollectionMediator = class (TSCInputDataCollection_B,IStockDataSourceToInputDataCollectionMediator)
  private
    FDS                : IStockDataSource;
    FEventMediator     : TEventMediator;
  public
    constructor Create(aDS:IStockDataSource; AOwner: ISCChangeNotifier);
    destructor Destroy; override;

    function  ISCInputDataCollection_GetItem(Index: Integer): ISCInputData; override;

    function  DirectGetItem_DataDateTime(index: integer):TDateTime; override;
    function  DirectGetItem_DataClose(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataHigh(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataLow(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataOpen(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataVolume(index: integer): integer; override;

    function  Count: integer; override;

    procedure SetDataSource(aDS: IStockDataSource);
    function  GetDataSource: IStockDataSource;
  end;

  TEventMediator = class (TInterfacedObject,IStockDataSourceEventHandler)
  private
    FEventHandlers:TInterfaceList; //ISCInputDataCollectionEventHandler
    FOwner : TStockDataSourceToInputDataCollectionMediator;
  public
    procedure OnChangeData(const aSender: IStockDataSource; index: integer; aType: TStockDataModificationType);

    constructor Create(aOwner: TStockDataSourceToInputDataCollectionMediator);
    destructor Destroy; override;
  end;

implementation
  uses Math;

type
  //----------------------------------------------------------------------------
  //Класс для описания одного элемента входных данных

  TDataMediator = class (TInterfacedObjectEx,ISCInputData)
  private
    FDS : IStockDataSource;
    FIndex : integer;
  protected
    function _Release: Integer; override; stdcall;
  public
    //from ISCInputData
    function GetDataClose: TStockRealNumber;
    function GetDataHigh: TStockRealNumber;
    function GetDataLow: TStockRealNumber;
    function GetDataOpen: TStockRealNumber;
    function GetDataVolume: integer;
    function GetDataDateTime: TDateTime;

    function IsBullish: boolean;
    function IsBearish: boolean;

    constructor Create(aDS: IStockDataSource; index: integer);
    destructor Destroy; override;
  end;

const
  gReserveInputDataObjectsSize = 1024;
var
  gReserveInputDataObjects : array [0..gReserveInputDataObjectsSize-1] of TDataMediator;
  gReserveInputDataObjectsCounter: integer;

{ TSCInputDataMediator }

constructor TDataMediator.Create(aDS: IStockDataSource; index: integer);
begin
  inherited Create;

  Assert(index>=0);
  Assert(index<=aDS.RecordCount);

  FDS:=aDS;
  FIndex:=index;
end;

function TDataMediator.GetDataOpen: TStockRealNumber;
begin
  result:=FDS.GetDataOpen(FIndex);
end;

function TDataMediator.GetDataHigh: TStockRealNumber;
begin
  result:=FDS.GetDataHigh(FIndex);
end;

function TDataMediator.GetDataDateTime: TDateTime;
begin
  result:=FDS.GetDataDateTime(FIndex);
end;

function TDataMediator.GetDataVolume: integer;
begin
  result:=FDS.GetDataVolume(FIndex);
end;

function TDataMediator.GetDataClose: TStockRealNumber;
begin
  result:=FDS.GetDataClose(FIndex);
end;

function TDataMediator.GetDataLow: TStockRealNumber;
begin
  result:=FDS.GetDataLow(FIndex);
end;

destructor TDataMediator.Destroy;
begin
  FDS:=nil;
  inherited;
end;

function TDataMediator._Release: Integer;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  ASSERT(Result >= 0);
  if (Result=0) then
  begin
    if gReserveInputDataObjectsCounter<gReserveInputDataObjectsSize then
    begin
      gReserveInputDataObjects[gReserveInputDataObjectsCounter]:=self;
      inc(gReserveInputDataObjectsCounter);
      self.FDS:=nil;
    end
    else
      self.free;
  end;
end;

function TDataMediator.IsBullish: boolean;
begin
  result:=GetDataOpen<GetDataClose;
end;

function TDataMediator.IsBearish: boolean;
begin
  result:=GetDataOpen>GetDataClose;
end;

{ TStockDataSourceToInputDataCollectionMediator }

constructor TStockDataSourceToInputDataCollectionMediator.Create(aDS: IStockDataSource; AOwner: ISCChangeNotifier);
begin
  inherited Create(AOwner,0.0);
  FEventMediator:=TEventMediator.Create(self);
  IInterface(FEventMediator)._AddRef;

  SetDataSource(aDS);
end;

destructor TStockDataSourceToInputDataCollectionMediator.Destroy;
begin
  inherited;
  FEventMediator.FOwner:=nil;
  IInterface(FEventMediator)._Release;
  FEventMediator:=nil;

end;

function TStockDataSourceToInputDataCollectionMediator.Count: integer;
begin
  if FDS=nil then
    result:=0
  else
    result:=FDS.RecordCount;
end;

function TStockDataSourceToInputDataCollectionMediator.ISCInputDataCollection_GetItem(Index: Integer): ISCInputData;
begin
  if gReserveInputDataObjectsCounter>0 then
  begin
    dec(gReserveInputDataObjectsCounter);
    gReserveInputDataObjects[gReserveInputDataObjectsCounter].FDS:=FDS;
    gReserveInputDataObjects[gReserveInputDataObjectsCounter].FIndex:=Index;
    result:=gReserveInputDataObjects[gReserveInputDataObjectsCounter];
    gReserveInputDataObjects[gReserveInputDataObjectsCounter+1]:=nil;
  end
  else begin
    result:=TDataMediator.Create(FDS,index);
  end;
end;

procedure TStockDataSourceToInputDataCollectionMediator.SetDataSource(aDS: IStockDataSource);
begin
  if FDS<>aDS then
  begin
    if FDS<>nil then
      FDS.RemoveEventHandler(FEventMediator);

    FDS:=aDS;

    if FDS<>nil then
    begin
      self.SetGradation(FDS.StockSymbol.GetTimeIntervalValue/MinsPerDay);    
      FDS.AddEventHandler(FEventMediator);

      SetPricePrecision(FDS.GetPricePrecision);
      SetPricesInPoint(FDS.GetPricesInPoint);

    end;

    OnItemsChanged;
  end;
end;

function TStockDataSourceToInputDataCollectionMediator.GetDataSource: IStockDataSource;
begin
  result:=FDS;
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataOpen(index: integer): TStockRealNumber;
begin
  result:=FDS.GetDataOpen(index);
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataHigh(index: integer): TStockRealNumber;
begin
  result:=FDS.GetDataHigh(index);
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataDateTime(index: integer): TDateTime;
begin
  result:=FDS.GetDataDateTime(index);
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataVolume(index: integer): integer;
begin
  result:=FDS.GetDataVolume(index);
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataClose(index: integer): TStockRealNumber;
begin
  result:=FDS.GetDataClose(index);
end;

function TStockDataSourceToInputDataCollectionMediator.DirectGetItem_DataLow(index: integer): TStockRealNumber;
begin
  result:=FDS.GetDataLow(index);
end;


procedure CleanupInputDataObjects;
var
  i: integer;
begin
  for i:=0 to gReserveInputDataObjectsSize-1 do
    FreeAndNil(gReserveInputDataObjects[i]);
end;

{ TEventMediator }

constructor TEventMediator.Create(aOwner: TStockDataSourceToInputDataCollectionMediator);
begin
  FEventHandlers := TInterfaceList.Create;
  FOwner:=aOwner;
end;

destructor TEventMediator.Destroy;
begin
  FreeAndNil(FEventHandlers);
  FOwner:=nil;
  inherited;
end;

procedure TEventMediator.OnChangeData(const aSender: IStockDataSource; index: integer; aType: TStockDataModificationType);
begin
  if FOwner<>nil then
    FOwner.OnItemChanged(index,aType);
end;

initialization

finalization
   CleanupInputDataObjects;

end.
