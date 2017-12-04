unit FC.Trade.Alerter.Factory;
{$I Compiler.inc}

interface
  uses Windows,Classes,SysUtils, Baseutils, Serialization, Generics.Collections, Collections.List,Collections.Map,
  FC.Definitions,FC.Trade.Alerter.Base;

type
  TStockAlerterInfo = class (TInterfacedObject,IStockAlerterInfo)
  private
    FName: string;
    FIID: TGUID;
    FCategory: string;
    FClass   : TStockAlerterClass;
  public
    property Class_: TStockAlerterClass read FClass;

    function IID  : TGUID;
    function Category: string;
    function Name    : string;

    constructor Create(const aCategory, aName: string; aClass: TStockAlerterClass;aIID  : TGUID);
    destructor Destroy; override;
  end;

  IStockAlerterFactoryEx = interface (IStockAlerterFactory)
    ['{3DB61007-BD94-4ADC-A3C3-97593FBF5C99}']
    //Регистрация класса трейдера. Указывается категория, имя, класс трейдера, идентифкатор интерфейса
    procedure RegisterAlerter(const aCategory, aName: string; aClass: TStockAlerterClass; aIID: TGUID; aHidden: boolean=false);

    function FindAlerter(const aCategory, aName: string; aClass: TStockAlerterClass): IStockAlerterInfo;
  end;

  TStockAlerterFactory = class (TInterfacedObject,IStockAlerterFactory, IStockAlerterFactoryEx, Serialization.IClassFactory)
  private type
    TStockAlerterInfoContainer = TInterfacedObjectContainer<TStockAlerterInfo>;
  private
    FAlerterList: TOwnedOjectList<TStockAlerterInfoContainer>;
    FAlerterMap : TOwnedObjectValueMap<TGUID,TStockAlerterInfoContainer>;

    function  GetAlerterInfoImpl(index: integer) : TStockAlerterInfo;
  protected
    //from Serialization.IClassFactory
    function  CreateInstance(const aClassName: string): TObject;

    constructor Create;
  public
    destructor Destroy; override;

    //from IStockAlerterFactoryEx
    function GetImplObject:TStockAlerterFactory;

    //From IStockAlerterFactory
    function  AlerterCount: integer;
    function  GetAlerterInfo(index: integer) : IStockAlerterInfo;

    procedure GetAllAlerterCategories(aList: TStrings);
    procedure GetAlAlertersForCategory(const aCategory: string; aList: TList<integer>);

    function  CreateAlerter(index: integer): IStockAlerter; overload;
    function  CreateAlerter(const aIID: TGUID): IStockAlerter;    overload;

    procedure RegisterAlerter(const aCategory, aName: string; aClass: TStockAlerterClass; aIID: TGUID; aHidden: boolean=false);
    function  FindAlerter(const aCategory, aName: string; aClass: TStockAlerterClass): IStockAlerterInfo;

    property Alerters[index: integer]: TStockAlerterInfo read GetAlerterInfoImpl;
  end;


  function AlerterFactory: IStockAlerterFactoryEx;

implementation
  uses FC.Singletons;

function AlerterFactory: IStockAlerterFactoryEx;
begin
  result:=(FC.Singletons.AlerterFactory as IStockAlerterFactoryEx);
end;


{ TStockAlerterInfo }

constructor TStockAlerterInfo.Create(const aCategory, aName: string;aClass: TStockAlerterClass; aIID: TGUID);
begin
  inherited Create;
  FCategory:=aCategory;
  FName:=aName;
  FClass:=aClass;
  FIID:=aIID;
end;

function TStockAlerterInfo.Category: string;
begin
  result:=FCategory;
end;

function TStockAlerterInfo.IID: TGUID;
begin
  result:=FIID;
end;

function TStockAlerterInfo.Name: string;
begin
  result:=FName;
end;


destructor TStockAlerterInfo.Destroy;
begin

  inherited;
end;


{ TStockAlerterFactory }

constructor TStockAlerterFactory.Create;
begin
  FAlerterList:=TOwnedOjectList<TStockAlerterInfoContainer>.Create;
  FAlerterMap:=TOwnedObjectValueMap<TGUID,TStockAlerterInfoContainer>.Create;
  FAlerterMap.AllowAppendRewrite:=false;
  Serialization.TClassFactory.RegisterClassFactory(self);
end;

destructor TStockAlerterFactory.Destroy;
begin
  FAlerterList.Free;
  FAlerterMap.Free;
  inherited;
end;

function TStockAlerterFactory.FindAlerter(const aCategory, aName: string; aClass: TStockAlerterClass): IStockAlerterInfo;
var
  i: integer;
begin
  result:=nil;
  for I := 0 to AlerterCount-1 do
  begin
    if (Alerters[i].Category=aCategory) and (Alerters[i].Name=aName) and (Alerters[i].Class_=aClass) then
    begin
      result:=Alerters[i];
      break;
    end;
  end;
end;

function TStockAlerterFactory.AlerterCount: integer;
begin
  result:=FAlerterList.Count;
end;

function TStockAlerterFactory.CreateAlerter(index: integer): IStockAlerter;
var
  aInfo : TStockAlerterInfo;
  aRes  : TStockAlerterBase;
begin
  aInfo:=Alerters[index];

  aRes:=aInfo.Class_.Create;
  aRes.IID:=aInfo.IID;
  aRes.SetCategory(aInfo.Category);
  aRes.SetName(aInfo.Name);
  result:=aRes;
end;

function TStockAlerterFactory.CreateAlerter(const aIID: TGUID): IStockAlerter;
var
  aInfoContainer: TStockAlerterInfoContainer;
  aInfo: TStockAlerterInfo;
  aRes : TStockAlerterBase;
begin
  if not FAlerterMap.Lookup(aIID,aInfoContainer) then
    raise EStockError.Create(Format('Alerter %s not found',[GUIDToString(aIID)]));

  aInfo:=aInfoContainer.Value;
  aRes:=aInfo.Class_.CreateNaked; //Naked!
  aRes.IID:=aInfo.IID;
  aRes.SetCategory(aInfo.Category);
  aRes.SetName(aInfo.Name);

  result:=aRes;
end;

procedure TStockAlerterFactory.RegisterAlerter(const aCategory, aName: string;aClass: TStockAlerterClass; aIID: TGUID; aHidden: boolean=false);
var
  aInfoContainer: TStockAlerterInfoContainer;
  aInfo: TStockAlerterInfo;
begin
  if FAlerterMap.Lookup(aIID,aInfoContainer) then
    raise EStockError.CreateFmt('Duplicate IID. Same value was set to %s',[aInfoContainer.Value.FName]);

  aInfo:=TStockAlerterInfo.Create(aCategory,aName, aClass,aIID);

  if not aHidden then
    FAlerterList.Add(TStockAlerterInfoContainer.Create(aInfo));

  FAlerterMap.Add(aIID,TStockAlerterInfoContainer.Create(aInfo));
end;

function TStockAlerterFactory.GetAlerterInfo(index: integer): IStockAlerterInfo;
begin
  result:=GetAlerterInfoImpl(index);
end;

procedure TStockAlerterFactory.GetAllAlerterCategories(aList: TStrings);
var
  i: integer;
begin
  for i:=0 to AlerterCount-1 do
  begin
    if aList.IndexOf(Alerters[i].Category)=-1 then
      aList.Add(Alerters[i].Category)
  end;
end;

procedure TStockAlerterFactory.GetAlAlertersForCategory(const aCategory: string; aList: TList<integer>);
var
  i: integer;
begin
  for i:=0 to AlerterCount-1 do
  begin
    if AnsiSameText(aCategory,Alerters[i].Category) then
      aList.Add(i);
  end;
end;

function TStockAlerterFactory.GetAlerterInfoImpl(index: integer): TStockAlerterInfo;
begin
  result:=FAlerterList[index].Value;
end;

function TStockAlerterFactory.GetImplObject: TStockAlerterFactory;
begin
  result:=self;
end;

function TStockAlerterFactory.CreateInstance(const aClassName: string): TObject;
var
  aInfo: TStockAlerterInfo;
  it : TMapIterator<TGUID,TStockAlerterInfoContainer>;
begin
  result:=nil;

  FAlerterMap.GetFirst(it);
  while it.Valid do
  begin
    aInfo:=it.Value.Value;
    if (aInfo.Class_.ClassName=aClassName) then
    begin
      result:=aInfo.Class_.CreateNaked;
      break;
    end;

    FAlerterMap.GetNext(it);
  end;
end;

initialization
  FC.Singletons.SetAlerterFactory(TStockAlerterFactory.Create);

end.
