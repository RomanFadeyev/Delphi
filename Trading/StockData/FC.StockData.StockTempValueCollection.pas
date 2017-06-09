unit FC.StockData.StockTempValueCollection;

interface
  uses SysUtils,Classes, Contnrs, FC.Definitions,Collections.List;

type
  TStockTempValue = class
  public
    DateTime : TDateTime;
    Value : TStockRealNumber;
    BarNo : integer;
    Comment: string;

    constructor Create(const aDateTime : TDateTime; const aValue : TStockRealNumber; const aBarNo : integer; const aComment: string);
  end;


  TStockTempValueCollection = class(TInterfacedObject,IStockTempValueCollection)
  private
    FData : TOwnedOjectList<TStockTempValue>;
  public
    procedure Add(const aDateTime : TDateTime; const aValue : TStockRealNumber; const aBarNo : integer; const aComment: string);

    //from IStockTempValueCollection
    function Count: integer;

    function GetDateTime(index: integer): TDateTime;
    function GetValue(index: integer): TStockRealNumber;
    function GetBarNo(index: integer): integer;
    function GetComment(index: integer): string;
    //

    function GetItem(index: integer) : TStockTempValue; inline;

    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TStockTempValue }

constructor TStockTempValue.Create(const aDateTime : TDateTime; const aValue : TStockRealNumber; const aBarNo : integer; const aComment: string);
begin
  DateTime:=aDateTime;
  Value:=aValue;
  BarNo:=aBarNo;
  Comment:=aComment;
end;


{ TStockTempValueCollection }

procedure TStockTempValueCollection.Add(const aDateTime : TDateTime; const aValue : TStockRealNumber; const aBarNo : integer; const aComment: string);
var
  aItem : TStockTempValue;
begin
  aItem:=TStockTempValue.Create(aDateTime,aValue,aBarNo,aComment);
  FData.Add(aItem);
end;

function TStockTempValueCollection.Count: integer;
begin
  result:=FData.Count;
end;

constructor TStockTempValueCollection.Create;
begin
  FData:=TOwnedOjectList<TStockTempValue>.Create;
end;

destructor TStockTempValueCollection.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TStockTempValueCollection.GetBarNo(index: integer): integer;
begin
  result:=GetItem(index).BarNo;
end;

function TStockTempValueCollection.GetComment(index: integer): string;
begin
  result:=GetItem(index).Comment;
end;

function TStockTempValueCollection.GetDateTime(index: integer): TDateTime;
begin
  result:=GetItem(index).DateTime;
end;

function TStockTempValueCollection.GetValue(index: integer): TStockRealNumber;
begin
  result:=GetItem(index).Value;
end;

function TStockTempValueCollection.GetItem(index: integer): TStockTempValue;
begin
  result:=FData[index];
end;


end.
