{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Коллекция тиков

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.StockTickCollection;
{$I Compiler.inc}

interface
  uses Classes, Windows, SysUtils, Controls, Contnrs,
       StockChart.Definitions,
       FC.Definitions;

type
  TStockTickCollection = class(TInterfacedObject,IStockTickCollectionWriteable, IStockTickCollection)
  private
    FList: TObjectList;
    FStockSymbol: string;
    FOrderValidation : boolean;
    procedure InsertInternal(aIndex: integer; const aDateTime: TDateTime; const aValue: TStockRealNumber);
  protected
    procedure Delete(index: integer);
    procedure ItemAdded(index: integer); virtual;
  public
    //from IStockTickCollection
    function StockSymbol: string;

    function Count: integer;

    function GetDateTime(index: integer): TDateTime;
    function GetValue(index: integer): TStockRealNumber;
    procedure SetValue(index: integer; aValue: TStockRealNumber);

    function GetFirstDateTime: TDateTime;
    function GetFirstValue: TStockRealNumber;

    function GetLastDateTime: TDateTime;
    function GetLastValue: TStockRealNumber;

    //end of IStockTickCollection

    procedure SetStockSymbol(const aSymbol: string);
    procedure SetOrderValidation(const aEnableOrderValidation: boolean);

    procedure Add(const aDateTime: TDateTime; const aValue: TStockRealNumber); virtual;
    //Упорядоченно вставить элемент в коллекцию. Если элемент с такой-же датой есть,
    //то возвращается индекс старого элемента, а новый не вставляется
    function  Merge(const aDateTime: TDateTime; const aValue: TStockRealNumber): integer; overload;
    //Тоже самое, только для коллекции
    procedure Merge(aCollection: TStockTickCollection); overload; virtual;

    procedure Load(const aReader: TReader);
    procedure Save(const aWriter: TWriter);

    procedure Clear;
    procedure Sort;

    constructor Create; overload;
    constructor Create(aQuery: IStockDataTickQuery); overload;

    destructor Destroy; override;
  end;

type
  EStockTickCollectionEmpty = class (EStockError)
    constructor Create;
  end;

  EStockTickCollectionOrderInvalid = class (EStockError)
    constructor Create;
  end;

implementation
  uses SystemService,DateUtils, Application.Definitions,BaseUtils,Collections.Algorithm, Generics.Defaults;

type
  TStockTickData=class
    DateTime: TDateTime;
    Value: TStockRealNumber;

    constructor Create(const aDateTime: TDateTime; const aValue: TStockRealNumber);
  end;

constructor TStockTickData.Create(const aDateTime: TDateTime;const aValue: TStockRealNumber);
begin
  DateTime:=aDateTime;
  Value:=aValue;
end;

{ TStockTickCollection }

constructor TStockTickCollection.Create;
begin
  FList:=TObjectList.Create;
  SetOrderValidation(true);
end;

procedure TStockTickCollection.Delete(index: integer);
begin
  FList.Delete(index);
end;

destructor TStockTickCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TStockTickCollection.Add(const aDateTime: TDateTime; const aValue: TStockRealNumber);
begin
  InsertInternal(FList.Count,aDateTime,aValue);
end;

procedure TStockTickCollection.Clear;
begin
  FList.Clear;
end;

function TStockTickCollection.Count: integer;
begin
  result:=FList.Count;
end;

constructor TStockTickCollection.Create(aQuery: IStockDataTickQuery);
begin
  Create;

  while not aQuery.Eof do
  begin
    Add(aQuery.GetDataDateTime,aQuery.GetDataValue);
    aQuery.Next;
  end;
end;

function TStockTickCollection.GetDateTime(index: integer): TDateTime;
begin
  result:=TStockTickData(FList[index]).DateTime;
end;

function TStockTickCollection.GetFirstDateTime: TDateTime;
begin
  if Count=0 then
    raise EStockTickCollectionEmpty.Create;
  result:=GetDateTime(0);
end;

function TStockTickCollection.GetFirstValue: TStockRealNumber;
begin
  if Count=0 then
    raise EStockTickCollectionEmpty.Create;
  result:=GetValue(0);
end;

function TStockTickCollection.GetLastDateTime: TDateTime;
begin
  if Count=0 then
    raise EStockTickCollectionEmpty.Create;
  result:=GetDateTime(Count-1);
end;

function TStockTickCollection.GetLastValue: TStockRealNumber;
begin
  if Count=0 then
    raise EStockTickCollectionEmpty.Create;
  result:=GetValue(Count-1);
end;

function TStockTickCollection.GetValue(index: integer): TStockRealNumber;
begin
  result:=TStockTickData(FList[index]).Value;
end;

procedure TStockTickCollection.InsertInternal(aIndex: integer;const aDateTime: TDateTime; const aValue: TStockRealNumber);
var
  aData:TStockTickData;
begin
  if FOrderValidation then
  begin
    //Проверяем, чтобы слева дата была меньшн или равна
    if aIndex>0 then
      if CompareDateTime(GetDateTime(aIndex-1), aDateTime)>0 then
        raise EStockTickCollectionOrderInvalid.Create;

    //Проверяем, чтобы справа дата была больше или равна
    if aIndex<FList.Count then
      if CompareDateTime(GetDateTime(aIndex), aDateTime)<0 then
        raise EStockTickCollectionOrderInvalid.Create;
  end;

  aData:=TStockTickData.Create(aDateTime,aValue);
  FList.Insert(aIndex,aData);
  ItemAdded(aIndex);
end;

procedure TStockTickCollection.ItemAdded(index: integer);
begin

end;

procedure TStockTickCollection.Load(const aReader: TReader);
var
  aCnt,i: integer;
  aDateTime: TDateTime;
  aValue: TStockRealNumber;
begin
  Clear;
  aCnt:=aReader.ReadInteger;
  for I := 0 to aCnt-1 do
  begin
    aDateTime:=aReader.ReadDate;
    aValue:=aReader.ReadDouble;
    Add(aDateTime,aValue);
  end;
end;

procedure TStockTickCollection.Merge(aCollection: TStockTickCollection);
var
  i,j,aInsert,aCmpr,aCnt: integer;
  aDateTime: TDateTime;
  aValue: TStockRealNumber;
begin
  if aCollection.Count=0 then
    exit;

  if Count=0 then
  begin
    for i := 0 to aCollection.Count - 1 do
      Add(aCollection.GetDateTime(i),aCollection.GetValue(i));
    exit;
  end;

  //Если две упорядоченные коллекции
  if (self.FOrderValidation) and (aCollection.FOrderValidation) then
  begin
    aDateTime:=aCollection.GetDateTime(0);
    aValue:=aCollection.GetValue(0);
    aInsert:=Merge(aDateTime,aValue);
    Assert(aInsert>=0);
    Assert(GetDateTime(aInsert)=aDateTime);

    j:=1; i:=aInsert+1;  aCnt:=aCollection.Count;
    while j<aCnt do
    begin
      aDateTime:=aCollection.GetDateTime(j);
      aValue:=aCollection.GetValue(j);

      if i=FList.Count then
      begin
        Add(aDateTime,aValue);
      end
      else begin
        aCmpr:=CompareDateTime(aDateTime,GetDateTime(i));
        if (aCmpr=0) then
          //Пропускаем
        else if aCmpr<0 then
          //Добавляем
          InsertInternal(i,aDateTime,aValue)
        else
          //Притормаживаем цикл по j, пропуская вперед i
          dec(j);
      end;

      inc(i);
      inc(j);
    end;
  end

  //Если хотя бы одна коллекция не упорядочена
  else begin
    for i := 0 to aCollection.Count - 1 do
      Merge(aCollection.GetDateTime(i),aCollection.GetValue(i));
  end;
end;

function TStockTickCollection.Merge(const aDateTime: TDateTime; const aValue: TStockRealNumber): integer;
var
  i,aInsert: integer;
begin
  if Count=0 then
  begin
    Add(aDateTime,aValue);
    result:=Count-1;
    exit;
  end;

  i:=TAlgorithm<TDateTime>.BinarySearch(GetDateTime,Count,aDateTime,TComparer<TDateTime>.Default,aInsert);
  //Если такого элемента нет, то добавляем
  if i=-1 then
  begin
    Assert(aInsert>=0);
    if aInsert>0 then
      Assert(CompareDateTime(GetDateTime(aInsert-1),aDateTime)<=0);
    if aInsert<Count then
      Assert(CompareDateTime(GetDateTime(aInsert),aDateTime)>=0);

    InsertInternal(aInsert,aDateTime,aValue);
    result:=aInsert;
  end
  //Такой элемент уже есть
  else
    result:=i;

end;

procedure TStockTickCollection.Save(const aWriter: TWriter);
var
  i: integer;
begin
  aWriter.WriteInteger(self.Count);
  for I := 0 to self.Count-1 do
  begin
    aWriter.WriteDate(GetDateTime(i));
    aWriter.WriteDouble(GetValue(i));
  end;
end;

procedure TStockTickCollection.SetOrderValidation(const aEnableOrderValidation: boolean);
begin
  FOrderValidation:=aEnableOrderValidation;
end;

procedure TStockTickCollection.SetStockSymbol(const aSymbol: string);
begin
  FStockSymbol:=aSymbol;
end;

procedure TStockTickCollection.SetValue(index: integer;aValue: TStockRealNumber);
begin
  TStockTickData(FList[index]).Value:=aValue;
end;

procedure TStockTickCollection.Sort;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
  result:=CompareDateTime(TStockTickData(Item1).DateTime,TStockTickData(Item2).DateTime);
end;

begin
  if Count>1 then
  begin
    FList.Sort(@SortFunc);
    Assert(GetDateTime(0)<GetDateTime(Count-1));
  end;
end;

function TStockTickCollection.StockSymbol: string;
begin
  result:=FStockSymbol;
end;


{ TStockTickData }


{ EStockTickCollectionEmpty }

constructor EStockTickCollectionEmpty.Create;
begin
  inherited Create('Collection is empty');
end;

{ EStockTickCollectionOrderInvalid }

constructor EStockTickCollectionOrderInvalid.Create;
begin
  inherited Create('Invalid order (new datetime is less than last one)');
end;

end.
