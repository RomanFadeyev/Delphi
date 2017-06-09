unit FC.StockData.InputDataCollectionLimitor;

{$I Compiler.inc}

interface
  uses BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj;

type
  //----------------------------------------------------------------------------
  IStockInputDataCollectionLimitor = interface
  ['{543055E5-5432-4D12-A6D0-173D9B863118}']
    procedure SetDataSource(aDS: ISCInputDataCollection);
    function  GetDataSource: ISCInputDataCollection;

    //Ограничивает правый край указанным индексом, как будто дальше данных нет
    procedure Limit(const aEnd: integer);
  end;

  TStockInputDataCollectionLimitor = class (TSCInputDataCollection_B,IStockInputDataCollectionLimitor)
  private
    FDS                : ISCInputDataCollection;
    FLimit: integer;
  public
    constructor Create(aDS:ISCInputDataCollection; AOwner: ISCChangeNotifier);
    destructor Destroy; override;

    function  ISCInputDataCollection_GetItem(Index: Integer): ISCInputData; override;

    function  DirectGetItem_DataDateTime(index: integer):TDateTime; override;
    function  DirectGetItem_DataClose(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataHigh(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataLow(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataOpen(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataVolume(index: integer): integer; override;

    function  Count: integer; override;

    //Ограничивает правый край указанным индексом, как будто дальше данных нет
    procedure Limit(const aEnd: integer);

    procedure SetDataSource(aDS: ISCInputDataCollection);
    function  GetDataSource: ISCInputDataCollection;
  end;

implementation
  uses Math;

{ TStockInputDataCollectionLimitor }

constructor TStockInputDataCollectionLimitor.Create(aDS: ISCInputDataCollection; AOwner: ISCChangeNotifier);
begin
  FLimit:=-1;
  inherited Create(AOwner,0.0);
  SetDataSource(aDS);
end;

destructor TStockInputDataCollectionLimitor.Destroy;
begin
  inherited;
end;

function TStockInputDataCollectionLimitor.Count: integer;
begin
  if FDS=nil then
    result:=0
  else
    result:=FDS.Count;

  if (FLimit<>-1) and (result>FLimit) then
    result:=FLimit;
end;

function TStockInputDataCollectionLimitor.ISCInputDataCollection_GetItem(Index: Integer): ISCInputData;
begin
  result:=FDS[index];
end;

procedure TStockInputDataCollectionLimitor.Limit(
  const aEnd: integer);
begin
  if FLimit=aEnd then
    exit;
  FLimit:=aEnd;
  OnItemsChanged;
end;

procedure TStockInputDataCollectionLimitor.SetDataSource(aDS: ISCInputDataCollection);
begin
  if FDS<>aDS then
  begin
    FDS:=aDS;
    if FDS<>nil then
    begin
      SetGradation(aDS.GetGradation);
      SetPricePrecision(aDS.GetPricePrecision);
      SetPricesInPoint(aDS.GetPricesInPoint);      
    end;
    OnItemsChanged;
  end;
end;

function TStockInputDataCollectionLimitor.GetDataSource: ISCInputDataCollection;
begin
  result:=FDS;
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataOpen(index: integer): TStockRealNumber;
begin
  result:=FDS.DirectGetItem_DataOpen(index);
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataHigh(index: integer): TStockRealNumber;
begin
  result:=FDS.DirectGetItem_DataHigh(index);
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataDateTime(index: integer): TDateTime;
begin
  result:=FDS.DirectGetItem_DataDateTime(index);
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataVolume(index: integer): integer;
begin
  result:=FDS.DirectGetItem_DataVolume(index);
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataClose(index: integer): TStockRealNumber;
begin
  result:=FDS.DirectGetItem_DataClose(index);
end;

function TStockInputDataCollectionLimitor.DirectGetItem_DataLow(index: integer): TStockRealNumber;
begin
  result:=FDS.DirectGetItem_DataLow(index);
end;


end.
