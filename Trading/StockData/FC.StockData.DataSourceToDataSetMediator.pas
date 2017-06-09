{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   —пециальный клас-переходник между библиотечным интерфейсом
            предоставлени€ данных дл€ чарта (IStockDataSource) и
            DataSet
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.DataSourceToDataSetMediator;
{$I Compiler.inc}

interface
  uses BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj,MemoryDS,DB;

type
  TStockInputDataCollectionToDataSetMediator = class (TCustomMemoryDataSet)
  private
    FMemoryRecord: TCustomMemoryRecord;

    FDataSource: IStockDataSource;
    procedure SetDataSource(const Value: IStockDataSource);
  protected
    function  AddMemoryRecord: TCustomMemoryRecord; override;
    function  InsertMemoryRecord(Index: Integer): TCustomMemoryRecord; override;
    function  GetMemoryRecord(Index: Integer): TCustomMemoryRecord; override;

    function  OnMemoryRecordAdded(aRecord:TCustomMemoryRecord): integer; override;
    function  OnMemoryRecordRemoved(aRecord:TCustomMemoryRecord): integer; override;
    procedure ChangeMemoryRecordIndex(aOld,aNew: integer); override;
    procedure ClearMemoryRecords; override;

    function  FindMemoryRecordByID(aID: integer): integer;  override;

    function  GetRecordCount: Integer;  override;
    function  GetCanModify: Boolean; override;

    procedure DoAfterOpen; override;
  public

    function GetDataDateTime: TDateTime;
    function GetDataOpen: TStockRealNumber;
    function GetDataHigh: TStockRealNumber;
    function GetDataLow: TStockRealNumber;
    function GetDataClose: TStockRealNumber;
    function GetDataVolume: integer;


    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: IStockDataSource read FDataSource write SetDataSource;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

type
  TBuffer = packed record
     a_0: byte;
     a_Index: integer;
     a_1: byte;
     a_Time: TDateTimeRec;
     a_2: byte;
     a_Open: Double;
     a_3: byte;
     a_High: Double;
     a_4: byte;
     a_Low: Double;
     a_5: byte;
     a_Close: Double;
     a_6: byte;
     a_Volume: integer;
  end;
  PBuffer = ^TBuffer;

  TMemoryRecordEx = class (TCustomMemoryRecord)
  protected
    procedure CleanupMemory; override;
    procedure AllocateMemory; override;

    function  GetBlobData(index: integer): TMemBlobData; override;
    procedure SetBlobData(index: integer; const Value: TMemBlobData); override;
  public
    procedure GetData(Buffer: TRecordBuffer); override;
    procedure SetData(Buffer: TRecordBuffer); override;
  end;

{ TStockInputDataCollectionToDataSetMediator }

constructor TStockInputDataCollectionToDataSetMediator.Create(aOwner: TComponent);
begin
  inherited;
  FieldDefs.Add('No', ftInteger);
  FieldDefs.Add('DateTime', ftDateTime);
  FieldDefs.Add('Open', ftCurrency);
  FieldDefs.Add('High', ftCurrency);
  FieldDefs.Add('Low', ftCurrency);
  FieldDefs.Add('Close', ftCurrency);
  FieldDefs.Add('Volume', ftInteger);

  FMemoryRecord:=TMemoryRecordEx.Create(self);
end;

destructor TStockInputDataCollectionToDataSetMediator.Destroy;
begin
  FreeAndNil(FMemoryRecord);
  inherited;
end;

procedure TStockInputDataCollectionToDataSetMediator.DoAfterOpen;
begin
  inherited;
  FieldByName('No').DisplayWidth:=7;
//  FieldByName('No').ReadOnly:=true;

  (FieldByName('Open') as TNumericField).DisplayFormat:='0.0000';
  FieldByName('Open').DisplayWidth:=7;
//  FieldByName('Open').ReadOnly:=true;

  (FieldByName('High') as TNumericField).DisplayFormat:='0.0000';
  FieldByName('High').DisplayWidth:=7;
//  FieldByName('High').ReadOnly:=true;

  (FieldByName('Low') as TNumericField).DisplayFormat:='0.0000';
  FieldByName('Low').DisplayWidth:=7;
//  FieldByName('Low').ReadOnly:=true;

  (FieldByName('Close') as TNumericField).DisplayFormat:='0.0000';
  FieldByName('Close').DisplayWidth:=7;
//  FieldByName('Close').ReadOnly:=true;
end;

function TStockInputDataCollectionToDataSetMediator.FindMemoryRecordByID(aID: integer): integer;
begin
  result:=aID; //»ндекс и ID должны совпадать
end;

function TStockInputDataCollectionToDataSetMediator.AddMemoryRecord: TCustomMemoryRecord;
begin
  result:=FMemoryRecord;
end;

procedure TStockInputDataCollectionToDataSetMediator.ChangeMemoryRecordIndex(aOld, aNew: integer);
begin
end;

procedure TStockInputDataCollectionToDataSetMediator.ClearMemoryRecords;
begin
end;

function TStockInputDataCollectionToDataSetMediator.GetCanModify: Boolean;
begin
  result:=false;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataClose: TStockRealNumber;
begin
  result:=FieldByName('Close').AsFloat;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataDateTime: TDateTime;
begin
  result:=FieldByName('DateTime').AsDateTime;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataHigh: TStockRealNumber;
begin
  result:=FieldByName('High').AsFloat;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataLow: TStockRealNumber;
begin
  result:=FieldByName('Low').AsFloat;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataOpen: TStockRealNumber;
begin
  result:=FieldByName('Open').AsFloat;
end;

function TStockInputDataCollectionToDataSetMediator.GetDataVolume: integer;
begin
  result:=FieldByName('Volume').AsInteger;
end;

function TStockInputDataCollectionToDataSetMediator.GetMemoryRecord(Index: Integer): TCustomMemoryRecord;
begin
  //ћожно ли в качестве ID использовать индекс???
  FMemoryRecord.ID:=index;

  result:=FMemoryRecord;
end;

function TStockInputDataCollectionToDataSetMediator.GetRecordCount: Integer;
begin
  if FDataSource=nil then
    result:=0
  else
    result:=FDataSource.RecordCount;
end;

function TStockInputDataCollectionToDataSetMediator.InsertMemoryRecord(Index: Integer): TCustomMemoryRecord;
begin
  result:=AddMemoryRecord;
end;

function TStockInputDataCollectionToDataSetMediator.OnMemoryRecordAdded(aRecord: TCustomMemoryRecord): integer;
begin
  result:=RecordCount-1;
end;

function TStockInputDataCollectionToDataSetMediator.OnMemoryRecordRemoved(aRecord: TCustomMemoryRecord): integer;
begin
  result:=RecordCount-1;
end;

procedure TStockInputDataCollectionToDataSetMediator.SetDataSource(const Value: IStockDataSource);
begin
  FDataSource := Value;
  Close;
  if FDataSource<>nil then
  begin
    Open;
  end;
end;

{ TMemoryRecordEx }

procedure TMemoryRecordEx.AllocateMemory;
begin
  inherited;

end;

procedure TMemoryRecordEx.CleanupMemory;
begin
  inherited;

end;

function TMemoryRecordEx.GetBlobData(index: integer): TMemBlobData;
begin
  result:='';
end;

procedure TMemoryRecordEx.GetData(Buffer: TRecordBuffer);
var
  aCollection: IStockDataSource;
begin
  aCollection:=TStockInputDataCollectionToDataSetMediator(Owner).DataSource;
  if aCollection<>nil then
  begin
    PBuffer(Buffer).a_0:=1;
    PBuffer(Buffer).a_1:=1;
    PBuffer(Buffer).a_2:=1;
    PBuffer(Buffer).a_3:=1;
    PBuffer(Buffer).a_4:=1;
    PBuffer(Buffer).a_5:=1;
    PBuffer(Buffer).a_6:=1;

    PBuffer(Buffer).a_Index:=ID; //»ндекс и ID у нас совпадают
    PBuffer(Buffer).a_Time.DateTime:=TimeStampToMSecs(DateTimeToTimeStamp(aCollection.GetDataDateTime(ID)));
    PBuffer(Buffer).a_Open:=aCollection.GetDataOpen(ID);
    PBuffer(Buffer).a_High:=aCollection.GetDataHigh(ID);
    PBuffer(Buffer).a_Low:=aCollection.GetDataLow(ID);
    PBuffer(Buffer).a_Close:=aCollection.GetDataClose(ID);
    PBuffer(Buffer).a_Volume:=aCollection.GetDataVolume(ID);
  end
  else
    FillChar(Buffer^,Owner.RecordSize,0);
end;

procedure TMemoryRecordEx.SetBlobData(index: integer; const Value: TMemBlobData);
begin

end;

procedure TMemoryRecordEx.SetData(Buffer: TRecordBuffer);
//var
//  aCollection: IStockDataSource;
begin
(*  aCollection:=TStockInputDataCollectionToDataSetMediator(Owner).InputDataCollection;
  if aCollection<>nil then
  begin
    if PBuffer(Buffer).a_1=1 then
    if PBuffer(Buffer).a_2=1 then
      aCollection.
    if PBuffer(Buffer).a_3=1 then
    if PBuffer(Buffer).a_4=1 then
    if PBuffer(Buffer).a_5=1 then
    if PBuffer(Buffer).a_6=1 then

    PBuffer(Buffer).a_Time.DateTime:=TimeStampToMSecs(DateTimeToTimeStamp(aCollection.DirectGetItem_DataDateTime(Index)));
    PBuffer(Buffer).a_Open:=aCollection.DirectGetItem_DataOpen(Index);
    PBuffer(Buffer).a_High:=aCollection.DirectGetItem_DataHigh(Index);
    PBuffer(Buffer).a_Low:=aCollection.DirectGetItem_DataLow(Index);
    PBuffer(Buffer).a_Close:=aCollection.DirectGetItem_DataClose(Index);
    PBuffer(Buffer).a_Volume:=aCollection.DirectGetItem_DataVolume(Index);
  end
*)
end;

end.

