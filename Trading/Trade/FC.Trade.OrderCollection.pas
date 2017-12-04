unit FC.Trade.OrderCollection;

interface
uses
  SysUtils, BaseUtils, Classes, Serialization, FC.Definitions;

type
  { TSCOrderCollection }

  //Self Destruct | Not Self Destruct
  TStockOrderCollection = class (TInterfaceProvider,IStockOrderCollection)
  private
    FList: TInterfaceList;
  public
    // IStockOrderCollection
    function  GetItem(Index: Integer): IStockOrder;

    function  Remove(const aOrder: IStockOrder): integer;    
    procedure Delete(index: integer);
    procedure Clear;
    function  Add(const aOrder: IStockOrder): integer;
    function  Count: integer;

    property  Items[Index: Integer]: IStockOrder read GetItem; default;

    constructor Create(aSelfDestruct: boolean=true);
    destructor Destroy; override;
  end;

implementation

{ TStockOrderCollection }

function TStockOrderCollection.Add(const aOrder: IStockOrder): integer;
begin
  result:=FList.Add(aOrder);
end;

function TStockOrderCollection.Count: integer;
begin
  result:=FList.Count;
end;

function TStockOrderCollection.GetItem(Index: Integer): IStockOrder;
begin
  result:=FList[index] as IStockOrder;
end;

function TStockOrderCollection.Remove(const aOrder: IStockOrder): integer;
begin
  result:=FList.IndexOf(aOrder);
  if result<>-1 then
    Delete(result);
end;

procedure TStockOrderCollection.Delete(index: integer);
begin
  FList.Delete(index);
end;

procedure TStockOrderCollection.Clear;
begin
  FList.Clear;
end;

constructor TStockOrderCollection.Create(aSelfDestruct: boolean);
begin
  inherited CReate;
  FSelfDestruct:=aSelfDestruct;
  FList:=TInterfaceList.Create;
end;

destructor TStockOrderCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

end.
