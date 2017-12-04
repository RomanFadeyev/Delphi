unit FC.Trade.TraderCollection;

interface
uses
  SysUtils, BaseUtils, Classes, StockChart,Serialization, FC.Definitions;

type
  { TSCOrderCollection }

  //Self Destruct | Not Self Destruct
  TStockOrderCollection = class (TInterfaceProvider,IStockTraderCollection)
  private
    FList: TInterfaceList;
  public
    // IStockTraderCollection
    function  GetItem(Index: Integer): IStockTrader;

    procedure Delete(index: integer);
    procedure Clear;
    function  Add(const aTrader: IStockTrader): integer;
    function  Count: integer;

    property  Items[Index: Integer]: IStockTrader read GetItem; default;

    constructor Create(aSelfDestruct: boolean=true);
    destructor Destroy; override;
  end;

implementation

{ TStockTraderCollection }

function TStockTraderCollection.Add(const aTrader: IStockTrader): integer;
begin
  result:=FList.Add(aTrader);
end;

function TStockTraderCollection.Count: integer;
begin
  result:=FList.Count;
end;

function TStockTraderCollection.GetItem(Index: Integer): IStockTrader;
begin
  result:=FList[index] as IStockTrader;
end;

procedure TStockTraderCollection.Delete(index: integer);
begin
  FList.Delete(index);
end;

procedure TStockTraderCollection.Clear;
begin
  FList.Clear;
end;

constructor TStockTraderCollection.Create(aSelfDestruct: boolean);
begin
  inherited CReate;
  FSelfDestruct:=aSelfDestruct;
  FList:=TInterfaceList.Create;
end;

destructor TStockTraderCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

end.
