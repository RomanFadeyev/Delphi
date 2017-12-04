unit FC.Trade.Trader.Factory;
{$I Compiler.inc}

interface
  uses Windows,Classes,SysUtils, BaseUtils,Serialization, Generics.Collections, Collections.List, Collections.Map,
  FC.Definitions,FC.Trade.Trader.Base;

type
  TStockTraderInfo = class (TInterfacedObject,IStockTraderInfo)
  private
    FName: string;
    FIID: TGUID;
    FCategory: string;
    FClass   : TStockTraderClass;
  public
    property Class_: TStockTraderClass read FClass;

    function IID  : TGUID;
    function Category: string;
    function Name    : string;

    constructor Create(const aCategory, aName: string; aClass: TStockTraderClass;aIID  : TGUID);
    destructor Destroy; override;
  end;

  IStockTraderFactoryEx = interface (IStockTraderFactory)
    ['{3DB61007-BD94-4ADC-A3C3-97593FBF5C99}']
    //Регистрация класса трейдера. Указывается категория, имя, класс трейдера, идентифкатор интерфейса
    procedure RegisterTrader(const aCategory, aName: string; aClass: TStockTraderClass; aIID: TGUID; aHidden: boolean=false);

    function FindTrader(const aCategory, aName: string; aClass: TStockTraderClass): IStockTraderInfo;
  end;

  TStockTraderFactory = class (TInterfacedObject,IStockTraderFactory, IStockTraderFactoryEx, Serialization.IClassFactory)
  private type
    TStockTraderInfoContainer = TInterfacedObjectContainer<TStockTraderInfo>;
  private
    FTraderList: TOwnedOjectList<TStockTraderInfoContainer>;
    FTraderMap : TOwnedObjectValueMap<TGUID,TStockTraderInfoContainer>;

    function  GetTraderInfoImpl(index: integer) : TStockTraderInfo;
  protected
    //from Serialization.IClassFactory
    function  CreateInstance(const aClassName: string): TObject;

    constructor Create;
  public
    destructor Destroy; override;

    //from IStockTraderFactoryEx
    function GetImplObject:TStockTraderFactory;

    //From IStockTraderFactory
    function  TraderCount: integer;
    function  GetTraderInfo(index: integer) : IStockTraderInfo;

    procedure GetAllTraderCategories(aList: TStrings);
    procedure GetAlTradersForCategory(const aCategory: string; aList: TList<integer>);

    function  CreateTrader(index: integer): IStockTrader; overload;
    function  CreateTrader(const aIID: TGUID): IStockTrader;    overload;

    procedure RegisterTrader(const aCategory, aName: string; aClass: TStockTraderClass; aIID: TGUID; aHidden: boolean=false);
    function  FindTrader(const aCategory, aName: string; aClass: TStockTraderClass): IStockTraderInfo;

    property Traders[index: integer]: TStockTraderInfo read GetTraderInfoImpl;
  end;


  function TraderFactory: IStockTraderFactoryEx;

implementation
  uses FC.Singletons;

function TraderFactory: IStockTraderFactoryEx;
begin
  result:=(FC.Singletons.TraderFactory as IStockTraderFactoryEx);
end;


{ TStockTraderInfo }

constructor TStockTraderInfo.Create(const aCategory, aName: string;aClass: TStockTraderClass; aIID: TGUID);
begin
  inherited Create;
  FCategory:=aCategory;
  FName:=aName;
  FClass:=aClass;
  FIID:=aIID;
end;

function TStockTraderInfo.Category: string;
begin
  result:=FCategory;
end;

function TStockTraderInfo.IID: TGUID;
begin
  result:=FIID;
end;

function TStockTraderInfo.Name: string;
begin
  result:=FName;
end;


destructor TStockTraderInfo.Destroy;
begin

  inherited;
end;


{ TStockTraderFactory }

constructor TStockTraderFactory.Create;
begin
  FTraderList:=TOwnedOjectList<TStockTraderInfoContainer>.Create;
  FTraderMap:=TOwnedObjectValueMap<TGUID,TStockTraderInfoContainer>.Create;
  FTraderMap.AllowAppendRewrite:=false;
  Serialization.TClassFactory.RegisterClassFactory(self);
end;

destructor TStockTraderFactory.Destroy;
begin
  FTraderList.Free;
  FTraderMap.Free;
  inherited;
end;

function TStockTraderFactory.FindTrader(const aCategory, aName: string; aClass: TStockTraderClass): IStockTraderInfo;
var
  i: integer;
begin
  result:=nil;
  for I := 0 to TraderCount-1 do
  begin
    if (Traders[i].Category=aCategory) and (Traders[i].Name=aName) and (Traders[i].Class_=aClass) then
    begin
      result:=Traders[i];
      break;
    end;
  end;
end;

function TStockTraderFactory.TraderCount: integer;
begin
  result:=FTraderList.Count;
end;

function TStockTraderFactory.CreateTrader(index: integer): IStockTrader;
var
  aInfo : TStockTraderInfo;
  aRes  : TStockTraderBase;
begin
  aInfo:=Traders[index];

  aRes:=aInfo.Class_.CreateNaked;
  aRes.IID:=aInfo.IID;
  aRes.SetCategory(aInfo.Category);
  aRes.SetName(aInfo.Name);
  result:=aRes;
end;

function TStockTraderFactory.CreateTrader(const aIID: TGUID): IStockTrader;
var
  aInfoContainer : TStockTraderInfoContainer;
  aInfo: TStockTraderInfo;
  aRes : TStockTraderBase;
begin
  if not FTraderMap.Lookup(aIID,aInfoContainer) then
    raise EStockError.Create(Format('Trader %s not found',[GUIDToString(aIID)]));

  aInfo:=aInfoContainer.Value;
  aRes:=aInfo.Class_.CreateNaked;
  aRes.IID:=aInfo.IID;
  aRes.SetCategory(aInfo.Category);
  aRes.SetName(aInfo.Name);

  result:=aRes;
end;

procedure TStockTraderFactory.RegisterTrader(const aCategory, aName: string;aClass: TStockTraderClass; aIID: TGUID; aHidden: boolean=false);
var
  aInfo: TStockTraderInfo;
  aInfoContainer : TStockTraderInfoContainer;
begin
  if FTraderMap.Lookup(aIID,aInfoContainer) then
    raise EStockError.CreateFmt('Duplicate IID. Same value was set to %s',[aInfoContainer.Value.FName]);

  aInfo:=TStockTraderInfo.Create(aCategory,aName, aClass,aIID);

  if not aHidden then
    FTraderList.Add(TStockTraderInfoContainer.Create(aInfo));

  FTraderMap.Add(aIID,TStockTraderInfoContainer.Create(aInfo));
end;

function TStockTraderFactory.GetTraderInfo(index: integer): IStockTraderInfo;
begin
  result:=GetTraderInfoImpl(index);
end;

procedure TStockTraderFactory.GetAllTraderCategories(aList: TStrings);
var
  i: integer;
begin
  for i:=0 to TraderCount-1 do
  begin
    if aList.IndexOf(Traders[i].Category)=-1 then
      aList.Add(Traders[i].Category)
  end;
end;

procedure TStockTraderFactory.GetAlTradersForCategory(const aCategory: string; aList: TList<integer>);
var
  i: integer;
begin
  for i:=0 to TraderCount-1 do
  begin
    if AnsiSameText(aCategory,Traders[i].Category) then
      aList.Add(i);
  end;
end;

function TStockTraderFactory.GetTraderInfoImpl(index: integer): TStockTraderInfo;
begin
  result:=FTraderList[index].Value;
end;

function TStockTraderFactory.GetImplObject: TStockTraderFactory;
begin
  result:=self;
end;

function TStockTraderFactory.CreateInstance(const aClassName: string): TObject;
var
  aInfo: TStockTraderInfo;
  it : TMapIterator<TGUID,TStockTraderInfoContainer>;
begin
  result:=nil;

  FTraderMap.GetFirst(it);
  while it.Valid do
  begin
    aInfo:=it.Value.Value;
    if (aInfo.Class_.ClassName=aClassName) then
    begin
      result:=aInfo.Class_.CreateNaked;
      break;
    end;

    FTraderMap.GetNext(it);
  end;
end;

initialization
  FC.Singletons.SetTraderFactory(TStockTraderFactory.Create);

end.
