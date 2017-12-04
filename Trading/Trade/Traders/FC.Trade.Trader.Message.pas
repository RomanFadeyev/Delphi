unit FC.Trade.Trader.Message;

interface

uses Classes, BaseUtils, SysUtils, FC.Definitions;

type
  TStockTraderMessage = class (TInterfacedObject,IStockTraderMessage)
  private
    FDateTime: TDateTime;
    FText    : string;
    FID      : TGUID;
  public
    function GetDateTime: TDateTime;
    function GetID: TGUID;
    function GetText: string;

    constructor Create(const aDateTime: TDateTime; const aText:string);
  end;

  { TStockTraderMessageCollection }

  //Self Destruct | Not Self Destruct
  TStockTraderMessageCollection = class (TInterfaceProvider,IStockTraderMessageCollection)
  private
    FList: TInterfaceList;
  public
    // ISCTraderMessageCollection
    function  GetItem(Index: Integer): IStockTraderMessage;
    function  IndexOf(aTraderMessage: IStockTraderMessage): integer; overload;
    function  IndexOf(const aDateTime: TDateTime): integer; overload;
    function  IndexOf(aID: TGUID): integer; overload;

    procedure Delete(index: integer); virtual;
    function  Remove(aTraderMessage:IStockTraderMessage): integer;
    procedure Clear; virtual;
    function  Add(const aTraderMessage: IStockTraderMessage): integer; virtual;
    function  Count: integer;

    property  Items[Index: Integer]: IStockTraderMessage read GetItem; default;

    constructor Create(aSelfDestruct: boolean=true);
    destructor Destroy; override;
  end;


implementation

{ TStockTraderMessageCollection }

function TStockTraderMessageCollection.Add(const aTraderMessage: IStockTraderMessage): integer;
begin
  result:=FList.Add(aTraderMessage);
end;

function TStockTraderMessageCollection.Count: integer;
begin
  result:=FList.Count;
end;

function TStockTraderMessageCollection.GetItem(Index: Integer): IStockTraderMessage;
begin
  result:=FList[index] as IStockTraderMessage;
end;

procedure TStockTraderMessageCollection.Delete(index: integer);
begin
  FList.Delete(index);
end;

procedure TStockTraderMessageCollection.Clear;
begin
  FList.Clear;
end;

constructor TStockTraderMessageCollection.Create(aSelfDestruct: boolean);
begin
  inherited CReate;
  FSelfDestruct:=aSelfDestruct;
  FList:=TInterfaceList.Create;
end;

destructor TStockTraderMessageCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TStockTraderMessageCollection.IndexOf(aTraderMessage: IStockTraderMessage): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to Count-1 do
   if IsEqualGUID(GetItem(i).GetID,aTraderMessage.GetID) then
   begin
     result:=i;
     break;
   end;
end;

function TStockTraderMessageCollection.Remove(aTraderMessage: IStockTraderMessage): integer;
begin
  result:=IndexOf(aTraderMessage);
  if result<>-1 then
    Delete(result);
end;

function TStockTraderMessageCollection.IndexOf(const aDateTime: TDateTime): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to Count-1 do
   if (GetItem(i).GetDateTime=aDateTime) then
   begin
     result:=i;
     break;
   end;
end;

function TStockTraderMessageCollection.IndexOf(aID: TGUID): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to Count-1 do
   if IsEqualGUID(GetItem(i).GetID,aID) then
   begin
     result:=i;
     break;
   end;
end;


{ TStockTraderMessage }

constructor TStockTraderMessage.Create(const aDateTime: TDateTime; const aText: string);
begin
  inherited Create;
  FDateTime:=aDateTime;
  FText:=aText;
  CreateGUID(FID);
end;

function TStockTraderMessage.GetID: TGUID;
begin
  result:=FID;
end;

function TStockTraderMessage.GetDateTime: TDateTime;
begin
  result:=FDateTime;
end;

function TStockTraderMessage.GetText: string;
begin
  result:=FText;
end;

end.
