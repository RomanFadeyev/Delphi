unit FC.Trade.Properties;
{$I Compiler.inc}

interface
  uses Classes, SysUtils, Controls, BaseUtils, Variants, Serialization,Generics.Collections,Properties.Obj,
  StockChart.Definitions,
  StockChart.Indicators.Properties,
  FC.Definitions;

type
  TTraderExpertWeighingType = (ewtAllEqual,ewtTimeFrame,ewtSeniority);

const

  TraderExpertWeighingTypeNames : array [TTraderExpertWeighingType] of string =  ('All Equal','Weighed by Time Frame','Weighed by Seniority');

type
  //----------------------------------------------------------------------------
  { TSCTraderPropertyExpertWeighingType }

  TSCTraderPropertyExpertWeighingType = class (TPropertyCombo)
  private
    procedure SetValue_(const Value: TTraderExpertWeighingType);
    function  GetValue_: TTraderExpertWeighingType;
  public
    constructor CreateNaked;                   override;
    constructor Create (aNotifier: IPropertyChangeHandler);

    property Value: TTraderExpertWeighingType read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TSCTraderPropertyLeadExpert }

  TSCTraderPropertyLeadExpert = class (TPropertyCombo)
  private
    procedure SetValue_(const Value: ISCIndicator);
    function  GetValue_: ISCIndicator;
  protected
    function  StoreItems: boolean; override;
  public
    constructor Create (aNotifier: IPropertyChangeHandler);
    destructor Destroy; override;

    procedure AddItem(const aValue: ISCIndicator);
    function ValueAsText: string; override;

    property Value: ISCIndicator read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TSCTraderPropertyExpertPriority }

  TExpertPriority = integer;

  TSCTraderPropertyExpertPriority =  class (TPropertyCombo)
  private
    FOwnerList: TList;
    FExpert : ISCIndicator;
    FCopy   : boolean;
    procedure SetValue_(const aValue: TExpertPriority);
    function  GetValue_: TExpertPriority;
    function GetNeighbour(index: integer): TSCTraderPropertyExpertPriority;
  protected
    function  StoreItems: boolean; override;

    procedure SetValue(const aValue: variant); override;
    procedure UpdateItems;

    function  FindNeighbourByValue(aValue: integer): TSCTraderPropertyExpertPriority;
    property  Neighbours[index: integer]: TSCTraderPropertyExpertPriority read GetNeighbour;
  public
    constructor Create (aNotifier: IPropertyChangeHandler; aExpert: ISCIndicator; aOwnerList: Tlist);
    procedure Init;

    procedure ReadData(const aReader: IDataReader);                 override;
    procedure WriteData(const aWriter: IDataWriter);                override;

    procedure OnAssignedFrom(aSource: TProperty); override;
    procedure OnAfterEdit; override;

    property OwnerList: TList read FOwnerList write FOwnerList;
    property Value: TExpertPriority read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TPropertyExperts }

  TPropertyExperts = class (TProperty)
  private
    FOwner : IStockTrader;
    procedure OnClick(sender: TObject);
    procedure SetExperts(const aExperts: ISCIndicatorCollection);
    function  GetExperts:ISCIndicatorCollection;
  protected
  public
    function CreateControl: TWinControl;  override;
    function ValueAsText: string; override;

    //from ISCPersistentObject
    procedure ReadData(const aReader: IDataReader);                 override;
    procedure WriteData(const aWriter: IDataWriter);                override;

    property Value:ISCIndicatorCollection read GetExperts write SetExperts;

    constructor Create(const aCategory, aName: string; aOwner: IStockTrader);
  end;

  //----------------------------------------------------------------------------
  { TPropertyMinimizationLossType }

  TMinimizationLossType =(mltNone,mltFixAtZeroPoint,mltMinimizeAll);

  TPropertyMinimizationLossType = class (TPropertyCombo)
  private
    procedure SetValue_(const Value: TMinimizationLossType);
    function  GetValue_: TMinimizationLossType;
  public
    constructor CreateNaked;                   override;
    constructor Create (aNotifier: IPropertyChangeHandler);

    property Value: TMinimizationLossType read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TPropertyStockTimeInterval }

  TPropertyStockTimeInterval = class (TPropertyCombo)
  private
    procedure SetValue_(const Value: TStockTimeInterval);
    function  GetValue_: TStockTimeInterval;
  public
    constructor CreateNaked;                   override;
    constructor Create (aNotifier: IPropertyChangeHandler);

    property Value: TStockTimeInterval read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TPropertyPriceRelation }

  TPriceRelation = (rlHigher,rlLower);
  TPropertyPriceRelation = class (TPropertyCombo)
  private
    procedure SetValue_(const aValue: TPriceRelation);
    function  GetValue_: TPriceRelation;
  public
    constructor CreateNaked;                   override;

    property Value: TPriceRelation read GetValue_ write SetValue_;
  end;

  //----------------------------------------------------------------------------
  { TPropertyPriceKind }

  TPropertyPriceKind = class (TPropertyCombo)
  private
    procedure SetValue_(const aValue: TStockBrokerPriceKind);
    function  GetValue_: TStockBrokerPriceKind;
  public
    constructor CreateNaked;                   override;

    property Value: TStockBrokerPriceKind read GetValue_ write SetValue_;
  end;

const
  IndicatorRelationNames: array [TPriceRelation ] of string =('>','<');

implementation
  uses Properties.Controls, FC.Trade.ChooseExpertsDialog,StockChart.Obj;

{ TSCTraderPropertyExpertWeighingType }

constructor TSCTraderPropertyExpertWeighingType.Create(aNotifier: IPropertyChangeHandler);
begin
  inherited Create('Method','Expert Weight',aNotifier);
end;

constructor TSCTraderPropertyExpertWeighingType.CreateNaked;
var
  i:TTraderExpertWeighingType;
begin
  inherited;
  for i:=Low(TraderExpertWeighingTypeNames) to High(TraderExpertWeighingTypeNames) do
    AddItem(TraderExpertWeighingTypeNames[i],integer(i));
  Value:=Low(TTraderExpertWeighingType);
end;

function TSCTraderPropertyExpertWeighingType.GetValue_: TTraderExpertWeighingType;
begin
  result:=TTraderExpertWeighingType(inherited GetValue);
end;

procedure TSCTraderPropertyExpertWeighingType.SetValue_(const Value: TTraderExpertWeighingType);
begin
  inherited SetValue(integer(Value));
end;

{ TSCTraderPropertyLeadExpert }

function TSCTraderPropertyLeadExpert.GetValue_: ISCIndicator;
begin
  result:=IInterface(inherited GetValue) as ISCIndicator;
end;

constructor TSCTraderPropertyLeadExpert.Create(aNotifier: IPropertyChangeHandler);
begin
  inherited Create('Method','Lead Expert',aNotifier);
end;

destructor TSCTraderPropertyLeadExpert.Destroy;
begin
  inherited;
end;

procedure TSCTraderPropertyLeadExpert.SetValue_(const Value: ISCIndicator);
begin
  inherited SetValue(Value);
end;

procedure TSCTraderPropertyLeadExpert.AddItem(const aValue: ISCIndicator);
begin
  inherited AddItem(aValue.GetName+'['+GetParentStockChart(aValue).StockSymbol.GetTimeIntervalName+']' ,aValue);
end;


function TSCTraderPropertyLeadExpert.StoreItems: boolean;
begin
  result:=true;
end;

function TSCTraderPropertyLeadExpert.ValueAsText: string;
begin
  if Value=nil then
    result:=''
  else
    result:=inherited ValueAsText;
end;

{ TSCTraderPropertyExpertPriority }

constructor TSCTraderPropertyExpertPriority.Create(
  aNotifier: IPropertyChangeHandler; aExpert: ISCIndicator;
  aOwnerList: Tlist);
begin
  inherited Create('Priority',
    Format('%s-%d [%s]',[ aExpert.GetName,
                          integer(GetParentStockChart(aExpert).StockSymbol.TimeInterval)+1,
                          GetParentStockChart(aExpert).StockSymbol.GetTimeIntervalName]),
    aNotifier);

  FOwnerList:=aOwnerList;
  FExpert:=aExpert;
end;

function TSCTraderPropertyExpertPriority.GetValue_: TExpertPriority;
begin
  result:= inherited GetValue;
end;

procedure TSCTraderPropertyExpertPriority.Init;
var
  i: integer;
begin
  UpdateItems;

  for i:=0 to ItemCount-1 do
    if FindNeighbourByValue(ItemValues[i])=nil then
    begin
      SetValue(ItemValues[i]);
      exit;
    end;

  raise EAlgoError.Create;
end;

procedure TSCTraderPropertyExpertPriority.SetValue(const aValue: variant);
var
  i: integer;
begin
  UpdateItems;

  inherited;

  if (FOwnerList<>nil) then
    for i:=0 to FOwnerList.Count-1 do
      if not IsEqualGUID(Neighbours[i].FExpert.GetID,self.FExpert.GetID) then
        Neighbours[i].UpdateItems;
end;

procedure TSCTraderPropertyExpertPriority.SetValue_( const aValue: TExpertPriority);
begin
  inherited SetValue(aValue);
end;

procedure TSCTraderPropertyExpertPriority.UpdateItems;
var
  aList: TList<integer>;
  i: integer;
begin
  Clear;

  if FOwnerList=nil then
   exit;

  aList:=TList<integer>.Create;
  try
    for i:=0 to FOwnerList.Count-1 do
      aList.Add(i);

//    if not FCopy then
//      for i:=0 to FOwnerList.Count-1 do
//        if not IsEqualGUID(Neighbours[i].FExpert.GetID,self.FExpert.GetID) then
//          aList.Remove(Neighbours[i].Value);

    for i:=0 to aList.Count-1 do
      AddItem(IntToStr(aList[i]),aList[i]);
  finally
    aList.Free;
  end;

  Assert(ItemCount>0);
end;

procedure TSCTraderPropertyExpertPriority.ReadData(const aReader: IDataReader);
begin
  FExpert:=aReader.ReadInterface as  ISCIndicator;
  inherited;
end;

procedure TSCTraderPropertyExpertPriority.WriteData(const aWriter: IDataWriter);
begin
  aWriter.WriteObject(FExpert as IPersistentObject);
  inherited;
end;

procedure TSCTraderPropertyExpertPriority.OnAssignedFrom( aSource: TProperty);
begin
  inherited;
  FCopy:=true;
  FOwnerList:=TSCTraderPropertyExpertPriority(aSource).FOwnerList;
  UpdateItems;
end;

function TSCTraderPropertyExpertPriority.GetNeighbour(index: integer): TSCTraderPropertyExpertPriority;
begin
  result:=TSCTraderPropertyExpertPriority(FOwnerList[index]);
end;

procedure TSCTraderPropertyExpertPriority.OnAfterEdit;
var
  aN: TSCTraderPropertyExpertPriority;
begin
  inherited;
  aN:=FindNeighbourByValue(self.Value);
  if aN<>nil then
    raise EPropertyError.Create(aN,'Duplicate priority');
end;

function TSCTraderPropertyExpertPriority.FindNeighbourByValue(aValue: integer): TSCTraderPropertyExpertPriority;
var
  j: integer;
begin
  result:=nil;
  for j:=0 to FOwnerList.Count-1 do
    if not IsEqualGUID(Neighbours[j].FExpert.GetID,self.FExpert.GetID) then
      if Neighbours[j].Value=aValue then
      begin
        result:=Neighbours[j];
        break;
      end;
end;

function TSCTraderPropertyExpertPriority.StoreItems: boolean;
begin
  result:=true;
end;

{ TPropertyExperts }

constructor TPropertyExperts.Create(const aCategory, aName: string; aOwner: IStockTrader);
var
  i: integer;
  aExperts: ISCIndicatorCollection;
begin
  inherited Create(aCategory,aName,aOwner as IPropertyChangeHandler);
  FOwner:=aOwner;

  aExperts:=TSCIndicatorCollection.Create();
  for i:=0 to FOwner.ExpertCount-1 do
    aExperts.Add(FOwner.GetExpert(i));
  Value:=aExperts;
end;

function TPropertyExperts.ValueAsText: string;
begin
  result:=IntToStr(VarArrayHighBound(inherited Value,1)-VarArrayLowBound(inherited Value,1)+1)+' experts' ;
end;

procedure TPropertyExperts.OnClick(sender: TObject);
var
  aDialog: TfmSelectExpertsDialog;
  it: TStockTimeInterval;
  i: integer;
  aChart: IStockChart;
  aExpert: ISCExpert;
  aMyExperts:ISCIndicatorCollection;
  aExperts  :ISCIndicatorCollection;
begin
  aDialog:=TfmSelectExpertsDialog.Create(nil);
  aMyExperts:=GetExperts;
  try
    for it:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    begin
      aChart:=FOwner.GetProject.GetStockChart(it);
      aExperts:=aChart.FindIndicators(ISCExpert);
      for i:=0 to aExperts.Count-1  do
      begin
        aExpert:=aExperts[i] as ISCExpert;
        aDialog.AddExpert(aExpert);
        aDialog.Checked[aExpert]:=aMyExperts.IndexOf(aExpert)<>-1;
      end;
    end;

    if aDialog.ShowModal=mrOk then
    begin
      aMyExperts.Clear;

      for it:=low(TStockTimeInterval) to high(TStockTimeInterval) do
      begin
        aChart:=FOwner.GetProject.GetStockChart(it);
        aExperts:=aChart.FindIndicators(ISCExpert);
        for i:=0 to aExperts.Count-1  do
        begin
          aExpert:=aExperts[i] as ISCExpert;
          if aDialog.Checked[aExpert] then
            aMyExperts.Add(aExpert);
        end;
      end;

      Value:=aMyExperts;
    end;
  finally
    aDialog.Free;
  end;
end;

function TPropertyExperts.CreateControl: TWinControl;
begin
  result:=TButtonControl.Create(nil);
  TButtonControl(result).OnClick:=OnClick;
end;

procedure TPropertyExperts.ReadData(const aReader: IDataReader);
begin
  inherited;
  if FOwner<>nil then
    aReader.ReadObjectExisting(FOwner as IPersistentObject)
  else
    FOwner:=aReader.ReadInterface as IStockTrader;
end;

procedure TPropertyExperts.WriteData(const aWriter: IDataWriter);
begin
  inherited;
  aWriter.WriteObject(FOwner as IPersistentObject);
end;

function TPropertyExperts.GetExperts: ISCIndicatorCollection;
var
  aExperts: ISCIndicatorCollection;
  i: integer;
  v: variant;
begin
  aExperts:=TSCIndicatorCollection.Create();
  v:=inherited Value;

  for i:=VarArrayLowBound(v,1) to VarArrayHighBound(v,1) do
    aExperts.Add(IInterface(v[i]) as ISCIndicator);

  result:=aExperts;
end;

procedure TPropertyExperts.SetExperts(const aExperts: ISCIndicatorCollection);
var
  v: variant;
  i: integer;
begin
  v:=VarArrayCreate([0,aExperts.Count-1],varUnknown);
  for i:=0 to aExperts.Count-1 do
    v[i]:=aExperts[i];

  inherited Value:=v;
end;

{ TPropertyMinimizationLossType }

constructor TPropertyMinimizationLossType.Create(aNotifier: IPropertyChangeHandler);
begin
  inherited Create('Method','Risks Minimization',aNotifier);
  AddObsoleteName('Minimize Any Possible Loss');
end;

constructor TPropertyMinimizationLossType.CreateNaked;
var
  i:TMinimizationLossType;
const
  aNames: array [TMinimizationLossType] of string =
  ('No minimization',
   'Move stop loss to open price when order becomes profitable',
   'Always move stop loss to open price to minimize any possible losses');
begin
  inherited;
  for i:=Low(aNames) to High(aNames) do
    AddItem(aNames[i],integer(i));
  Value:=Low(TMinimizationLossType);
end;

function TPropertyMinimizationLossType.GetValue_: TMinimizationLossType;
begin
  result:=TMinimizationLossType(inherited GetValue);
end;

procedure TPropertyMinimizationLossType.SetValue_(const Value: TMinimizationLossType);
begin
  inherited SetValue(integer(Value));
end;

{ TPropertyStockTimeInterval }

constructor TPropertyStockTimeInterval.Create(aNotifier: IPropertyChangeHandler);
begin
  inherited Create('Method','Interval',aNotifier);
end;

constructor TPropertyStockTimeInterval.CreateNaked;
var
  i:TStockTimeInterval;
begin
  inherited;
  for i:=Low(TStockTimeInterval) to High(TStockTimeInterval) do
    AddItem(StockTimeIntervalNames[i],integer(i));
  Value:=low(TStockTimeInterval);
end;

function TPropertyStockTimeInterval.GetValue_: TStockTimeInterval;
begin
  result:=TStockTimeInterval(inherited GetValue);
end;

procedure TPropertyStockTimeInterval.SetValue_(const Value: TStockTimeInterval);
begin
  inherited SetValue(integer(Value));
end;

{ TPropertyPriceRelation }

constructor TPropertyPriceRelation.CreateNaked;
var
  i:TPriceRelation;
begin
  inherited CreateNaked;
  for i:=Low(IndicatorRelationNames) to High(IndicatorRelationNames) do
    AddItem(IndicatorRelationNames[i],integer(i));
  Value:=Low(TPriceRelation);
end;

function TPropertyPriceRelation.GetValue_: TPriceRelation;
begin
  result:=TPriceRelation(inherited GetValue);
end;

procedure TPropertyPriceRelation.SetValue_(const aValue: TPriceRelation);
begin
  inherited SetValue(integer(aValue));
end;

{ TPropertyPriceKind }

constructor TPropertyPriceKind.CreateNaked;
var
  i:TStockBrokerPriceKind;
begin
  inherited CreateNaked;
  for i:=Low(StockBrokerPriceKindNames) to High(StockBrokerPriceKindNames) do
    AddItem(StockBrokerPriceKindNames[i],integer(i));
  Value:=Low(StockBrokerPriceKindNames);
end;

function TPropertyPriceKind.GetValue_: TStockBrokerPriceKind;
begin
  result:=TStockBrokerPriceKind(inherited GetValue);
end;

procedure TPropertyPriceKind.SetValue_(const aValue: TStockBrokerPriceKind);
begin
  inherited SetValue(integer(aValue));
end;

end.
