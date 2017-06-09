{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   —пециальный клас-переходник, позвол€ющий представить значени€ индикатора как
            коллецию данных (ISCInputDataCollection)
 History:
-----------------------------------------------------------------------------}

unit FC.StockData.IndicatorToInputDataCollectionMediator;
{$I Compiler.inc}

interface
  uses BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, StockChart.Obj;

type
  //----------------------------------------------------------------------------
  TEventMediator = class;

  IStockIndicatorToInputDataCollectionMediator = interface
  ['{70CD9A6D-3682-4552-BD9B-F5A3FA30AE90}']
    function  GetDataSource: ISCIndicator;
  end;

  TStockIndicatorToInputDataCollectionMediator = class (TSCInputDataCollection_B,IStockIndicatorToInputDataCollectionMediator,ISCInputDataCollectionCustomDataSupport)
  private
    FIndicator_     : ISCIndicator;
    FIndicator      : ISCIndicatorValueSupport;
    FEventMediator  : TEventMediator;

    procedure CheckMutualdependence(const aIndicator: ISCIndicator);
  public
    constructor Create(const aDS:ISCIndicator; AOwner: ISCChangeNotifier);
    destructor Destroy; override;

    function  ISCInputDataCollection_GetItem(Index: Integer): ISCInputData; override;

    function  DirectGetItem_DataDateTime(index: integer):TDateTime; override;
    function  DirectGetItem_DataClose(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataHigh(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataLow(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataOpen(index: integer): TStockRealNumber; override;
    function  DirectGetItem_DataVolume(index: integer): integer; override;

    function  Count: integer; override;

    procedure SetDataSource(const aDS: ISCIndicator);

    //from IStockIndicatorToInputDataCollectionMediator
    function  GetDataSource: ISCIndicator;

    //from ISCInputDataCollectionCustomDataSupport
    function Caption: string;
  end;

  TEventMediator = class (TInterfacedObject,ISCInputDataCollectionEventHandler)
  private
    FEventHandlers:TInterfaceList; //ISCInputDataCollectionEventHandler
    FOwner : TStockIndicatorToInputDataCollectionMediator;
  public
    procedure OnChangeItem(const aSender: ISCInputDataCollection; aIndex: integer; aType: TSCInputDataCollectionChangeType);

    constructor Create(aOwner: TStockIndicatorToInputDataCollectionMediator);
    destructor Destroy; override;
  end;

implementation
  uses Math;

type
  //----------------------------------------------------------------------------
  // ласс дл€ описани€ одного элемента входных данных

  TDataMediator = class (TInterfacedObjectEx,ISCInputData)
  private
    FValue   : TStockRealNumber;
    FDateTime: TDateTime;
    FVolume  : integer;
  protected
    function _Release: Integer; override; stdcall;
  public
    //from ISCInputData
    function GetDataClose: TStockRealNumber;
    function GetDataHigh: TStockRealNumber;
    function GetDataLow: TStockRealNumber;
    function GetDataOpen: TStockRealNumber;
    function GetDataVolume: integer;
    function GetDataDateTime: TDateTime;

    function IsBullish: boolean;
    function IsBearish: boolean;
  end;

const
  gReserveInputDataObjectsSize = 1024;
var
  gReserveInputDataObjects : array [0..gReserveInputDataObjectsSize-1] of TDataMediator;
  gReserveInputDataObjectsCounter: integer;

{ TSCInputDataMediator }

function TDataMediator.GetDataOpen: TStockRealNumber;
begin
  result:=FValue;
end;

function TDataMediator.GetDataHigh: TStockRealNumber;
begin
  result:=FValue;
end;

function TDataMediator.GetDataDateTime: TDateTime;
begin
  result:=FDateTime;
end;

function TDataMediator.GetDataVolume: integer;
begin
  result:=FVolume;
end;

function TDataMediator.GetDataClose: TStockRealNumber;
begin
  result:=FValue;
end;

function TDataMediator.GetDataLow: TStockRealNumber;
begin
  result:=FValue;
end;

function TDataMediator._Release: Integer;
begin
  Dec(FRefCount);
  Result:=FRefCount;
  ASSERT(Result >= 0);
  if (Result=0) then
  begin
    if gReserveInputDataObjectsCounter<gReserveInputDataObjectsSize then
    begin
      gReserveInputDataObjects[gReserveInputDataObjectsCounter]:=self;
      inc(gReserveInputDataObjectsCounter);
    end
    else
      self.free;
  end;
end;

function TDataMediator.IsBullish: boolean;
begin
  result:=GetDataOpen<GetDataClose;
end;

function TDataMediator.IsBearish: boolean;
begin
  result:=GetDataOpen>GetDataClose;
end;

{ TStockIndicatorToInputDataCollectionMediator }

constructor TStockIndicatorToInputDataCollectionMediator.Create(const aDS:ISCIndicator; AOwner: ISCChangeNotifier);
begin
  inherited Create(AOwner,0.0);
  FEventMediator:=TEventMediator.Create(self);
  IInterface(FEventMediator)._AddRef;

  SetDataSource(aDS);
end;

destructor TStockIndicatorToInputDataCollectionMediator.Destroy;
begin
  inherited;
  FEventMediator.FOwner:=nil;
  IInterface(FEventMediator)._Release;
  FEventMediator:=nil;

end;

function TStockIndicatorToInputDataCollectionMediator.Caption: string;
begin
  if FIndicator_=nil then
    result:=''
  else
    result:=FIndicator_.Caption;
 
end;

procedure TStockIndicatorToInputDataCollectionMediator.CheckMutualdependence(const aIndicator: ISCIndicator);
var
  aMediator: IStockIndicatorToInputDataCollectionMediator;
begin
  if aIndicator.GetInputData=nil then
    exit;

  if not Supports(aIndicator.GetInputData,IStockIndicatorToInputDataCollectionMediator,aMediator) then
    exit;

  if aMediator=IStockIndicatorToInputDataCollectionMediator(self) then
    raise EStockError.Create('Mutual dependence detected');

  CheckMutualdependence(aMediator.GetDataSource);
end;

function TStockIndicatorToInputDataCollectionMediator.Count: integer;
begin
  if (FIndicator=nil) or (FIndicator_.GetInputData=nil) then
    result:=0
  else
    result:=FIndicator_.GetInputData.Count;
end;

function TStockIndicatorToInputDataCollectionMediator.ISCInputDataCollection_GetItem(Index: Integer): ISCInputData;
var
  aDataItem: TDataMediator;
begin
  if gReserveInputDataObjectsCounter>0 then
  begin
    dec(gReserveInputDataObjectsCounter);
    aDataItem:=gReserveInputDataObjects[gReserveInputDataObjectsCounter];
    gReserveInputDataObjects[gReserveInputDataObjectsCounter+1]:=nil;
  end
  else begin
    aDataItem:=TDataMediator.Create;
  end;

  aDataItem.FValue:=FIndicator.GetValue(index);
  with FIndicator_.GetInputData do
  begin
    aDataItem.FDateTime:=DirectGetItem_DataDateTime(index);
    aDataItem.FVolume:=DirectGetItem_DataVolume(index);
  end;

  result:=aDataItem;
end;

procedure TStockIndicatorToInputDataCollectionMediator.SetDataSource(const aDS: ISCIndicator);
begin
  if FIndicator_<>aDS then
  begin
    CheckMutualdependence(aDS);

    if FIndicator_<>nil then
      FIndicator_.GetInputData.RemoveEventHandler(FEventMediator);

    FIndicator_:=aDS;
    FIndicator:=aDS as ISCIndicatorValueSupport;

    if FIndicator_<>nil then
    begin
      FIndicator_.GetInputData.AddEventHandler(FEventMediator);
      SetGradation(FIndicator_.GetInputData.GetGradation);
    end;

    OnItemsChanged;
  end;
end;

function TStockIndicatorToInputDataCollectionMediator.GetDataSource: ISCIndicator;
begin
  result:=FIndicator_;
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataOpen(index: integer): TStockRealNumber;
begin
  result:=FIndicator.GetValue(index);
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataHigh(index: integer): TStockRealNumber;
begin
  result:=FIndicator.GetValue(index);
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataDateTime(index: integer): TDateTime;
begin
  result:=FIndicator_.GetInputData.DirectGetItem_DataDateTime(index)
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataVolume(index: integer): integer;
begin
  result:=FIndicator_.GetInputData.DirectGetItem_DataVolume(index)
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataClose(index: integer): TStockRealNumber;
begin
  result:=FIndicator.GetValue(index);
end;

function TStockIndicatorToInputDataCollectionMediator.DirectGetItem_DataLow(index: integer): TStockRealNumber;
begin
  result:=FIndicator.GetValue(index);
end;


procedure CleanupInputDataObjects;
var
  i: integer;
begin
  for i:=0 to gReserveInputDataObjectsSize-1 do
    FreeAndNil(gReserveInputDataObjects[i]);
end;

{ TEventMediator }

constructor TEventMediator.Create(aOwner: TStockIndicatorToInputDataCollectionMediator);
begin
  FEventHandlers := TInterfaceList.Create;
  FOwner:=aOwner;
end;

destructor TEventMediator.Destroy;
begin
  FreeAndNil(FEventHandlers);
  FOwner:=nil;
  inherited;
end;

procedure TEventMediator.OnChangeItem(const aSender: ISCInputDataCollection; aIndex: integer; aType: TSCInputDataCollectionChangeType);
begin
  if FOwner<>nil then
    FOwner.OnItemChanged(aIndex,aType);
end;

initialization

finalization
   CleanupInputDataObjects;

end.
