{-----------------------------------------------------------------------------
 Unit Name:
 Author:    Roman
 Purpose:   Базовый класс для всех трейдеров. Реализует интерфейс IStockTrader

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Trader.Base;
{$I Compiler.inc}

{$DEFINE IGNORE_UNKNOWN_PROPERTIES}
{$DEFINE USE_NONCACHED_RECALCULATION} //Для оптимизации. Во включенном режиме удаляет пре-расчет и кеширование у индикаторов

interface

uses
  Classes, Math,Contnrs, Controls, SysUtils, BaseUtils, ActnList, Forms, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Indicators, StockChart.Definitions.Drawing,
  Serialization, FC.Definitions, FC.Trade.Properties;

type
  { TStockTrader }

  TStockTraderBase = class;
  TStockTraderClass = class of TStockTraderBase;

  TLastOrderType = (lotNone,lotBuy,lotSell);

  TLogRecord = class
  public
    DateTime: TDateTime;
    Text    : string;
  end;

  //Псевдо-интерфейс, необходимый для генерации уникального GUID
  //Будем использовать его в ExternalData экспертов, чтобы отличить
  //где наш эксперта, а где не наш
  IStockTraderExternalIdentifier = interface (ISCAttribute)
  ['{40DE82A4-840E-4AA3-A89C-78BBE85691D0}']
    function  GetIdentification: string;
    function  GetTraderIID: TGUID;
  end;

  //... и собственно класс, его реализующий
  TStockTraderExternalIdentifier = class (TNameValuePersistentObjectRefCounted,IStockTraderExternalIdentifier,ISCAttribute)
  private
    FTraderIID: TGUID;
    FIdentification: string;
    procedure SetIdentification(const Value: string);
    function  GetIdentification: string;
    function  GetTraderIID: TGUID;
  protected
    procedure OnDefineValues; override;

    property Identification: string read FIdentification write SetIdentification;
    property TraderIID: TGUID read FTraderIID write FTraderIID;

    constructor Create; virtual;
  end;

  TStockTraderExternalIdentifierClass = class of TStockTraderExternalIdentifier;
  TStockBrokerEventHandler = class;

  //Базовый
  TStockTraderBase = class (TNameValuePersistentObjectRefCounted,IStockTrader,IPropertyChangeHandler)
  private
    FExperts   : ISCIndicatorCollection;
    FExternalIdentifiers: TInterfaceList; //of IStockTraderExternalIdentifier
    FExternalIdentifierClass: TStockTraderExternalIdentifierClass;
    FIID       : TGUID;
    FCategory  : string;
    FName      : string;
    FOrders    : IStockOrderCollection;
    FHandlers  : TInterfaceList;
    FLastTime  : TDateTime;
    FBroker    : IStockBroker;
    //FMessages  : IStockTraderMessageCollection;
    FProject   : IStockProject;
    FState    : TSCIndicatorStates;
    FBrokerEventHandler : TStockBrokerEventHandler;

    //
    FTestBenchDialog: TForm;
    FTestBenchDialogAction: TAction;

    //Свойства (настраиваемые)
    FPropMaxSubsidence  : TPropertyInt;
    FPropTrailingStop       : TPropertyInt;
    FPropTrailingStopDescend: TPropertyReal;
    FPropEnoughProfit       : TPropertyInt;
    //Способ минимизации рисков
    FMinimizationRiskType  : TPropertyMinimizationLossType;


    FPropLotDefaultRateSize : TPropertyReal;
    FPropLotDynamicRate     : TPropertyYesNo;
    FPropLotDynamicRateSize : TPropertyReal;


    FCachedPropMaxSubsidence: TStockRealNumber;
    FCachedPropTrailingStop : TStockRealNumber;
    FCachedPropEnoughProfit : TStockRealNumber;
    FCachedPropTrailingStopDescend: TStockRealNumber;

    FVisibleProperties: TPropertyList;

    function  GetExternalIdentifier(index: integer): IStockTraderExternalIdentifier;
    function  GetEventHandler(index: integer): IStockTraderEventHandler;
    procedure CachePropertyValues;

    procedure OnTestBenchActionUpdate(Sender: TObject);
    procedure OnTestBenchActionExecute(Sender: TObject);
  protected
    FID        : TGUID;

    //Сериализация (TNameValuePersistentObjectRefCounted)
    procedure OnDefineValues; override;
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;

    //Вспомогательная функция для находждения "наших" индикаторов на указанном чарте
    //Если таковой не находится - мы его создаем. Параметр aIndicatorIdentification должен быть
    //уникален для каждого создаваемого индикатора
    //Параметр aRegister указывает, регистрировать ли созданный индикатор в трейдере (назначить ему атрибут
    //ExternalId трейдера). По регистрации см. также OnAddObject
    function  CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean=true): ISCIndicator; overload;
    function  CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean; out aCreated: boolean): ISCIndicator; overload;

    //Установка Trailing Stop на основании свойств трейдера
    procedure SetTrailingStopAccordingProperty(aOrder: IStockOrder); virtual;

    //Установка Stop Stop на основании свойств трейдера
    procedure SetStopLossAccordingProperty(aOrder: IStockOrder); virtual;

    //Считает, на какой цене сработает Stop Loss или опередивший его Trailing Stop
    //Если Trailing Stop не задан, то фукнция просто вернет значение StopLoss
    function  GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;

    //Считает, какой убыток будет, если закроется по StopLoss или Trailing Stop (что быстрее)
    //Возвращает знак наоборот: плюс, если действительно есть лосс, и минус, если стоп стоит уже на
    //профитной стороне
    function  GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;

    //проверяет, можно ли установить ордеру указанный стоп. Невозможность установки может возникнуть
    //из-за близости маркет-цены, установокй не с той стороны и т.д.
    function CanSetStopLoss(const aOrder: IStockOrder; aStopLevel:TStockRealNumber): boolean;

    //подсчитывает, какойстоп лосс можно поставить для ордера, чтобы он был как можно
    //ближе к маркет цене, стандартное использование: подтянуть стоп как можно ближе, чтобы
    //он сразу же сработал, если цена двинется в отрицаетельном направлении
    function GetNearestStopLossToMarketPrice(const aOrder: IStockOrder): TStockRealNumber;

    function IsLevelTooCloseToCurrentPrice(aPriceKind: TStockBrokerPriceKind;aLevel: TSCRealNumber): boolean;

    //двигает StopLoss в сторону безубыточности ордера на заданное значение
    //При этом проверяется, чтобы
    //1) Указанный SL не был хуже текущего
    //2) Указанный SL не противоречил текущей цене брокера
    function MoveStopLossCloser(const aOrder: IStockOrder; const aSL: TStockRealNumber):boolean;

    //возвращает рекомедуемое кол-во лотов
    function GetRecommendedLots: TStockOrderLots; virtual;

    //Создает пустой экзмепдяр ордера, никуда не открытого.
    //Просто создается экзмепляр и регистрирурется в внутр. списке
    function CreateEmptyOrder: IStockOrder; virtual;

    //Специальные методы-обработчики для наследников, определяющие начало и конец работы треедра
    //Вызываются при инициализации трейдера брокером (см. SetBroker)
    procedure OnBeginWorkSession; virtual;
    procedure OnEndWorkSession; virtual;

    procedure OnAddObject(const aIndicator:ISCIndicator; aIndicatorIdentification: string); virtual;
    procedure OnRemoveObject(const aIndicator:ISCIndicator); virtual;
    procedure OnPropertyChanged(aNotifier:TProperty); virtual;
    procedure OnPropertyCreated(aNotifier:TProperty); virtual;

    //Оставлено для потомков. Методы, в которых нужно создавать и удалять свои объекты
    procedure OnCreateObjects; virtual;
    procedure OnReleaseObjects; virtual;
    //Событие на изменение "нашего" ордера (который был создан функциями  OpenOrder.... или CreateEmptyOrder)
    procedure OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs); virtual;

    procedure GetProperties(aList: TPropertyList); overload; virtual;
    function  GetProperties: IPropertyCollection; overload;

    procedure RegisterProperties(aProperties: array of TProperty);
    procedure UnRegisterProperties(aProperties: array of TProperty);

    property  State : TSCIndicatorStates read FState;
    property  ExternalIdentifierClass: TStockTraderExternalIdentifierClass read FExternalIdentifierClass write FExternalIdentifierClass;

    function  ExternalIdentifierCount: integer;
    property  ExternalIdentifiers[index: integer]: IStockTraderExternalIdentifier read GetExternalIdentifier;

    //Найти наш идентификатор в указанном индикаторе. Если нет, возврашает nil;
    function  FindExternalIdentifier(const aIndicator:ISCIndicator; aIndicatorIdentification: string): IStockTraderExternalIdentifier;

    //Добавить на все чарты значок
    procedure AddMarkToCharts(const aTime: TDateTime; const aPrice: TStockRealNumber;
                              const aMarkType: TSCChartMarkKind;
                              const aMessage: string);

    //Добавить на указанный чарт значок
    procedure AddMarkToChart( const aTimeInterval: TStockTimeInterval;
                              const aTime: TDateTime; const aPrice: TStockRealNumber;
                              const aMarkType: TSCChartMarkKind;
                              const aMessage: string);

    //Выдает НЕмодальный диалог с кнопкой "ПРодолжить". Останавливает процесс, пока виден этот диалог
    procedure Pause(const aMessage: string);

    //форма для тестирования
    function TestBenchDialogClass: TClass; virtual; //TClass = TfmTestBenchDialogClass

    property PropMaxSubsidence  : TPropertyInt read FPropMaxSubsidence;
    property CachedPropMaxSubsidence: TStockRealNumber read FCachedPropMaxSubsidence;

    property PropTrailingStop       : TPropertyInt read FPropTrailingStop;
    property PropTrailingStopDescend: TPropertyReal read FPropTrailingStopDescend;
    property PropEnoughProfit       : TPropertyInt read FPropEnoughProfit;

    property PropLotDefaultRateSize : TPropertyReal read FPropLotDefaultRateSize;
    property PropLotDynamicRate     : TPropertyYesNo read FPropLotDynamicRate;
    property PropLotDynamicRateSize : TPropertyReal read FPropLotDynamicRateSize;

    property PropMinimizationRiskType  : TPropertyMinimizationLossType read FMinimizationRiskType;

    constructor Create; virtual;
  public
    procedure OnCloseTestBenchDialog(aDialog: TForm); virtual;

    //from IPersistentObject
    procedure ReadData(const aReader: IDataReader);  override;

    //from IStockTrader
    function  GetCategory: string;
    function  GetName: string;
    procedure SetCategory(const aValue: string);
    procedure SetName(const aValue : string);

    function  GetSymbol: string;
    function  GetID: TGUID;

    function  GetBroker: IStockBroker;
    procedure SetBroker(const aBroker: IStockBroker); virtual;

    function  GetProject: IStockProject;
    procedure SetProject(const aValue : IStockProject); virtual;

    //Посчитать
    procedure IStockTrader.Update = UpdateStep1;
    procedure UpdateStep1(const aTime: TDateTime); virtual;
    procedure UpdateStep2(const aTime: TDateTime); virtual; abstract;
    procedure Invalidate;

    procedure AddEventHandler(const aHandler: IStockTraderEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockTraderEventHandler);

    procedure AddExpert(const aExpert: ISCExpert);
    function  RemoveExpert(const aExpert: ISCExpert): integer;
    procedure DeleteExpert(index: integer);
    function  ExpertCount: integer;
    function  GetExpert(aIndex: integer): ISCExpert;
    function  IndexOfExpert(const aExpert:ISCExpert): integer;

    function  GetOrders: IStockOrderCollection;

    function  ShowPropertyWindow: boolean;

    function  IsThis(const aTrader: IStockTrader): boolean;
    //end of IStockTrader

    property IID: TGUID read FIID write FIID;

    //Немедленное открытие ордера по текущей цене брокера
    //Значения Rate, Take Profit, Stop Loss, Trailing Stop устанавливаются автоматически, в соответствии
    //с настройками трейдера
    function  OpenOrder(aKind: TStockOrderKind; const aComment: string=''): IStockOrder; overload;

    //Немедленное открытие ордера по текущей цене брокера
    //Значения Rate, Take Profit, Stop Loss, Trailing Stop указываются вручную
    function  OpenOrder(aKind: TStockOrderKind;
                        aLots:TStockOrderLots;
                        aStopLoss, aTakeProfit,aTrailingStop: TSCRealNumber;
                        const aComment: string=''): IStockOrder; overload;

    //открытие Limit  или Stop ордера по запрошенной цене
    //Значения Rate, Take Profit, Stop Loss, Trailing Stop устанавливаются автоматически, в соответствии
    //с настройками трейдера
    function  OpenOrderAt(aKind: TStockOrderKind;aPrice: TStockRealNumber; const aComment: string=''): IStockOrder; overload;

    //открытие Limit  или Stop ордера по запрошенной цене
    //Значения Rate, Take Profit, Stop Loss, Trailing Stop указываются вручную
    function  OpenOrderAt(aKind: TStockOrderKind;aPrice: TStockRealNumber;
                          aLots:TStockOrderLots;
                          aStopLoss, aTakeProfit,aTrailingStop: TSCRealNumber;
                          const aComment: string=''): IStockOrder; overload;

    //Немедленное закрытие ордера
    procedure CloseOrder(const aOrder: IStockOrder;const aComment: string);

    //Немедленное закрытие последнего ордера в списке ордера по текущей цене брокера
    procedure CloseLastOrder(const aComment: string);
    procedure CloseAllSellOrders(const aComment: string);
    procedure CloseAllBuyOrders(const aComment: string);


    //Брокер может закрывать ордера самостоятельно по TakeProfit или StopLoss
    //Чтобы они не болтались в нашем списке, их можно удалить с помощтью данной функции
    procedure RemoveClosedOrders;

    //Если в списке есть ордера, возвращает тип последнего. Если ордеров нет, возвращает lotNone
    function  LastOrderType: TLastOrderType;
    //Если в списке есть ордера, возвращает статус последнего. Если ордеров нет, возвращает osNothing
    function  LastOrderState: TStockOrderState;
    //Ищет первый ордер, совпадающий по состоянию с указанным. Поиск можно вести как
    //с начала в конец, так и с конца в начало (aForward);
    function  FindOrderByState(aState:TStockOrderState; aForward: boolean):IStockOrder;


    function EventHandlerCount: integer;
    property EventHandler[index:integer]:IStockTraderEventHandler read GetEventHandler;

    procedure Dispose; virtual;

    constructor CreateNaked; override;
    destructor Destroy; override;
  end;

  TStockBrokerEventHandler = class (TInterfacedObject,IStockBrokerEventHandler)
  private
    FOwner: TStockTraderBase;
  public
    constructor Create (aOwner: TStockTraderBase);
    destructor Destroy; override;

    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewOrder   (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewData    (const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;
  end;

implementation
  uses FC.Trade.OrderCollection, FC.Singletons, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.DataUtils, StockChart.Obj,
  FC.fmUIDataStorage,Application.Definitions,FC.Trade.Trader.TestBenchDialog_B,
  ufmResume;

{ TStockTraderBase }

constructor TStockTraderBase.CreateNaked;
begin
  FState:=[isReading];
  inherited;
  Create;
  Exclude(FState,isReading);
end;

constructor TStockTraderBase.Create;
begin
  inherited Create;
  FBrokerEventHandler:=TStockBrokerEventHandler.Create(self);
  IInterface(FBrokerEventHandler)._AddRef;

  FExternalIdentifiers:=TInterfaceList.Create;
  FOrders:=TStockOrderCollection.Create;
  FHandlers:=TInterfaceList.Create;
  FExperts:=TSCIndicatorCollection.Create;

  //FMessages:=TStockTraderMessageCollection.Create;

  FExternalIdentifierClass:=TStockTraderExternalIdentifier;

  CreateGUID(FID);
  FLastTime:=-1;

  FPropMaxSubsidence:=TPropertyInt.Create('Method','Max Subsidence',self);
  FPropMaxSubsidence.Value:=30;

  FPropTrailingStop:=TPropertyInt.Create('Method','Trailing stop',self);
  FPropTrailingStop.Value:=30;

  FPropTrailingStopDescend:=TPropertyReal.Create('Method','Trailing Stop Descend Koeff',self);
  FPropTrailingStopDescend.HelpString:='Decreases trailing stop while profit grows. '#13#10+'RealTS = TS-k*(Profit-TS)'+#13#10+'where TS-trailing stop, k-descend';
  FPropTrailingStopDescend.Value:=0;

  FPropEnoughProfit:=TPropertyInt.Create('Method','Enough Profit',self);
  FPropEnoughProfit.Value:=300;

  FPropLotDefaultRateSize:= TPropertyReal.Create('Lots','Static Value',self);
  FPropLotDefaultRateSize.Value:=0.1;

  FPropLotDynamicRate   := TPropertyYesNo.Create('Lots\Dynamic','Enable',self);
  FPropLotDynamicRate.Value:=False;

  FPropLotDynamicRateSize:= TPropertyReal.Create('Lots\Dynamic','Percent of Balace',self);
  FPropLotDynamicRateSize.Value:=5;

  FMinimizationRiskType:=TPropertyMinimizationLossType.Create(self);

  RegisterProperties([FPropMaxSubsidence,FPropTrailingStop,FPropTrailingStopDescend,FPropEnoughProfit,
                             FPropLotDefaultRateSize,FPropLotDynamicRate,FPropLotDynamicRateSize,FMinimizationRiskType]);

  CachePropertyValues;

  StockBrokerConnectionRegistry.AddBrokerEventHandler(FBrokerEventHandler);

  if TestBenchDialogClass<>nil then
  begin
    FTestBenchDialogAction:=(Workspace.MainFrame as IStockMainFrame).AddMenuItem(UIDataStorage.acTradingTail,false);
    FTestBenchDialogAction.Caption:=self.GetName+' - Test Bench';
    FTestBenchDialogAction.OnExecute:=OnTestBenchActionExecute;
    FTestBenchDialogAction.OnUpdate:=OnTestBenchActionUpdate;
  end;
end;

destructor TStockTraderBase.Destroy;
begin
  FBrokerEventHandler.FOwner:=nil;
  StockBrokerConnectionRegistry.RemoveBrokerEventHandler(FBrokerEventHandler);
  IInterface(FBrokerEventHandler)._Release;
  FBrokerEventHandler:=nil;

  FOrders:=nil;
  FExperts:=nil;
  //FMessages:=nil;
  FreeAndNil(FHandlers);
  FreeAndNil(FVisibleProperties);
  FreeAndNil(FExternalIdentifiers);
  inherited;
end;

procedure TStockTraderBase.Dispose;
var
  i: integer;
begin
  FreeAndNil(FTestBenchDialog);
  if FTestBenchDialogAction<>nil then
    (Workspace.MainFrame as IStockMainFrame).RemoveMenuItem(FTestBenchDialogAction);
  FreeAndNil(FTestBenchDialogAction);

  if FOrders<>nil then
    FOrders.Clear;

  if FExperts<>nil then
  begin
    for i:=0 to ExpertCount-1 do
      OnRemoveObject(GetExpert(i));
    FExperts.Clear;
  end;

  FreeAndNil(FPropMaxSubsidence);
  FreeAndNil(FPropTrailingStop);
  FreeAndNil(FPropTrailingStopDescend);
  FreeAndNil(FPropEnoughProfit);
  FreeAndNil(FPropLotDefaultRateSize);
  FreeAndNil(FPropLotDynamicRate);
  FreeAndNil(FPropLotDynamicRateSize);
  FreeAndNil(FMinimizationRiskType);

  if FVisibleProperties<>nil then
    FVisibleProperties.Clear;

  FBroker:=nil;
end;

function TStockTraderBase.GetID: TGUID;
begin
  result:=FID;
end;

procedure TStockTraderBase.Invalidate;
var
  i: integer;
begin
  FLastTime:=-1;
  FOrders.Clear;

  for i:=0 to EventHandlerCount-1 do
    EventHandler[i].OnInvalidate(self);
end;

function TStockTraderBase.GetCategory: string;
begin
  result:=FCategory;
end;

function TStockTraderBase.GetName: string;
begin
  result:=FName;
end;

function TStockTraderBase.GetNearestStopLossToMarketPrice(const aOrder: IStockOrder): TStockRealNumber;
var
  aMarketPrice: TStockRealNumber;
  aStopDelta: TStockRealNumber;
begin
  //Берем +1, чтобы наверняка
  aStopDelta:=GetBroker.PointToPrice(aOrder.GetSymbol,GetBroker.GetMarketInfo(aOrder.GetSymbol).StopLevel+1);

  aStopDelta:=GetBroker.RoundPrice(aOrder.GetSymbol,aStopDelta);

  if aOrder.GetKind= okBuy then
  begin
    aMarketPrice:=GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkBid);
    result:=aMarketPrice+aStopDelta;//Для ордера вверх добавляем (а не отнимаем) к цене спред, чтобы стремится к меньшим потерям
  end
  else begin
    aMarketPrice:=GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkAsk);
    result:=aMarketPrice-aStopDelta; //Для ордера вниз отнимаем от цены спред, чтобы стремится к меньшим потерям
  end;

  Assert(CanSetStopLoss(aOrder,result));
end;

procedure TStockTraderBase.UpdateStep1(const aTime: TDateTime);
var
  i: integer;
begin
  if FLastTime>=aTime then
    exit;

  //Нужно минимизировать риски
  if FMinimizationRiskType.Value<>mltNone then
    for i:=0 to GetOrders.Count-1 do
      SetStopLossAccordingProperty(GetOrders[i]);

  //Если установлен динамический Trailing Stop, то нужно обежать все ордера
  //и модифицировать его
  for i:=0 to GetOrders.Count-1 do
    SetTrailingStopAccordingProperty(GetOrders[i]);


  {$IFDEF USE_NONCACHED_RECALCULATION}
  for i:=0 to ExpertCount-1 do
    GetExpert(i).SetCalculationMode(icmMinPrecache);
  {$ENDIF}

  try
    try
      UpdateStep2(aTime);
    except
      on E:EAbort do;
    end;
  finally
    {$IFDEF USE_NONCACHED_RECALCULATION}
    for i:=0 to ExpertCount-1 do
      GetExpert(i).SetCalculationMode(icmMaxPrecache);
    {$ENDIF}
  end;


  FLastTime:=aTime;
end;

function TStockTraderBase.GetOrders: IStockOrderCollection;
begin
  result:=FOrders;
end;

procedure TStockTraderBase.OnDefineValues;
begin
  inherited;
  DefValGUID('ID',FID);
  DefValString('Category',FCategory);
  DefValString('Name',FName);
end;

procedure TStockTraderBase.OnModifyOrder(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin

end;

procedure TStockTraderBase.OnWriteValues(const aWriter: INameValueDataWriter);
var
  aPropList: TPropertyList;
  i: integer;
  aList: TInterfaceList;
begin
  inherited;

  aList:=TInterfaceList.Create;
  try
    for i := 0 to FExperts.Count-1 do
      aList.Add(FExperts.Items[i]);
    aWriter.WriteObjects('Experts',aList);
  finally
    aList.Free;
  end;


  aPropList:=TPropertyList.Create;
  try
    GetProperties(aPropList);
    for i:=0 to aPropList.Count-1 do
      aWriter.WriteVariant('Property:'+aPropList[i].Category+'\'+aPropList[i].Name,aPropList[i].Value);
  finally
    aPropList.Free;
  end;
end;

procedure TStockTraderBase.OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean);
var
  aPropList: TPropertyList;
  i,j: integer;
  s: string;
  v: variant;
  aList: TInterfaceList;
begin
  inherited;

  if (aName =  'Experts') then
  begin
    aHandled:=true;
    aList:=TInterfaceList.Create;
    try
      aReader.ReadObjects(aList);
      for i := 0 to aList.Count - 1 do
        AddExpert(aList[i] as ISCExpert);
    finally
      aList.Free;
    end;
  end

  else if StrIsInLeft('Property:',pchar(aName)) then
  begin
    aPropList:=TPropertyList.Create;
    try
      GetProperties(aPropList);
      for i:=0 to aPropList.Count-1 do
      begin
        s:='Property:'+aPropList[i].Category+'\'+aPropList[i].Name;
        if s=aName then
          aHandled:=true;

        //Устаревшие названия
        if not aHandled and (aPropList[i].ObsoleteNames<>nil) then
          for j:=0 to aPropList[i].ObsoleteNames.Count-1 do
          begin
            s:='Property:'+aPropList[i].Category+'\'+aPropList[i].ObsoleteNames[j];
            if s=aName then
            begin
              aHandled:=true;
            end
          end;

        //Если имя подходит - считываем
        if (aHandled) then
        begin
          aReader.ReadVariant(v);
          aPropList[i].Value:=v;
          break;
        end;
      end;
    finally
      aPropList.Free;
    end;

    CachePropertyValues;

    {$IFDEF IGNORE_UNKNOWN_PROPERTIES}
    if not aHandled then
    begin
      aHandled:=true;
      aReader.ReadVariant(v);
    end;
    {$ENDIF}
  end
end;

procedure TStockTraderBase.OnCloseTestBenchDialog(aDialog: TForm);
begin
  FTestBenchDialog:=nil;
end;

procedure TStockTraderBase.OnCreateObjects;
begin

end;

procedure TStockTraderBase.OnReleaseObjects;
begin

end;

procedure TStockTraderBase.AddEventHandler(const aHandler: IStockTraderEventHandler);
begin
  if FHandlers.IndexOf(aHandler)<>-1 then
    raise ESCItemAlreadyExists.Create;

  FHandlers.Add(aHandler);
end;

procedure TStockTraderBase.RemoveClosedOrders;
var
  i: integer;
begin
  for i:=FOrders.Count-1 downto 0 do
    if GetOrders[i].GetState in [osClosed,osNothing] then
      FOrders.Delete(i);
end;

procedure TStockTraderBase.RemoveEventHandler(const aHandler: IStockTraderEventHandler);
begin
  FHandlers.Remove(aHandler);
end;

procedure TStockTraderBase.CloseAllBuyOrders(const aComment: string);
var
  i: integer;
  aOrder: IStockOrder;
begin
  for i:=FOrders.Count-1 downto 0 do
  begin
    aOrder:=FOrders[FOrders.Count-1];

    if (aOrder.GetState<>osClosed) and (aOrder.GetKind=okBuy) then
    begin
      aOrder.Close(aComment);
      aOrder:=nil;
      FOrders.Delete(i);
    end;
  end;
end;

procedure TStockTraderBase.CloseAllSellOrders(const aComment: string);
var
  i: integer;
  aOrder: IStockOrder;
begin
  for i:=FOrders.Count-1 downto 0 do
  begin
    aOrder:=FOrders[FOrders.Count-1];

    if (aOrder.GetState<>osClosed) and (aOrder.GetKind=okSell) then
    begin
      aOrder.Close(aComment);
      aOrder:=nil;
      FOrders.Delete(i);
    end;
  end;
end;

procedure TStockTraderBase.CloseLastOrder(const aComment: string);
var
  aOrder: IStockOrder;
begin
  if FOrders.Count=0 then
    raise ESCError.Create('Нет открытых ордеров');

  aOrder:=FOrders[FOrders.Count-1];

  if aOrder.GetState<>osClosed then
    aOrder.Close(aComment);

  aOrder:=nil;
  FOrders.Delete(FOrders.Count-1);
end;

procedure TStockTraderBase.CloseOrder(const aOrder: IStockOrder; const aComment: string);
begin
  aOrder.Close(aComment);
  FOrders.Remove(aOrder);
end;

function TStockTraderBase.LastOrderState: TStockOrderState;
begin
  if FOrders.Count=0 then
    result:=osNothing
  else
    result:=FOrders[FOrders.Count-1].GetState;
end;

function TStockTraderBase.LastOrderType: TLastOrderType;
begin
  if FOrders.Count=0 then
    result:=lotNone
  else if FOrders[FOrders.Count-1].GetKind=okBuy then
    result:=lotBuy
  else
    result:=lotSell;
end;

function TStockTraderBase.MoveStopLossCloser(const aOrder: IStockOrder; const aSL: TStockRealNumber):boolean;
var
  aRes: TStockRealNumber;
begin
  result:=false;

  //Сравниваем с текущим SL
  if aOrder.GetKind=okBuy then
  begin
    aRes:=max(aOrder.GetStopLoss,aSL);

    //Смотрим, чтобы не слишком близко к Market-цене
    aRes:=min(aRes,GetNearestStopLossToMarketPrice(aOrder));

    //Смотрим, что получилось, и если SL стал лучше, ставим
    if aRes>aOrder.GetStopLoss then
    begin
      aOrder.SetStopLoss(aRes);
      result:=true;
    end;
  end
  else begin
    aRes:=min(aOrder.GetStopLoss,aSL);
    //Смотрим, чтобы не слишком близко к Market-цене
    aRes:=max(aRes,GetNearestStopLossToMarketPrice(aOrder));
    //Смотрим, что получилось, и если SL стал лучше, ставим
    if aRes<aOrder.GetStopLoss then
    begin
      aOrder.SetStopLoss(aRes);
      result:=true;
    end;
  end;
end;

function TStockTraderBase.CreateEmptyOrder: IStockOrder;
begin
  if FBroker=nil then
    raise ESCError.Create('Broker is not defined for trader');

  if FLastTime>FBroker.GetCurrentTime then
    raise ESCError.Create('The indicated time is less than in previous order');

  FLastTime:=FBroker.GetCurrentTime;

  result:=FBroker.CreateOrder(self);

  //добавляем себя в список
  FOrders.Add(result);
end;

function TStockTraderBase.OpenOrder(aKind: TStockOrderKind;const aComment: string=''): IStockOrder;
var
  aLots : TStockOrderLots;
begin
  result:=nil;

  aLots:=GetRecommendedLots;
  if aLots=0 then
    raise EStockError.CreateFmt('Deposit too small to open order (Type=%s)', [OrderKindNames[aKind]]);

  result:=CreateEmptyOrder;
  //открываемся
  result.Open(GetSymbol, aKind,aLots,aComment);

  //Выставляем TakeProfit и StopLoss
  case aKind of
    okBuy: begin
      if FCachedPropMaxSubsidence>0 then
        result.SetStopLoss(result.GetOpenPrice-FCachedPropMaxSubsidence);

      if FCachedPropEnoughProfit>0 then
        result.SetTakeProfit(result.GetOpenPrice+FCachedPropEnoughProfit);
    end;
    okSell: begin
       if FCachedPropMaxSubsidence>0 then
         result.SetStopLoss(result.GetOpenPrice+FCachedPropMaxSubsidence);
       if FCachedPropEnoughProfit>0 then
         result.SetTakeProfit(result.GetOpenPrice-FCachedPropEnoughProfit);
    end;
    else
      raise EAlgoError.Create;
  end;

  //Выставляем trailing stop
  SetTrailingStopAccordingProperty(result);
  //result.AddMessage('Order opened');
end;

function TStockTraderBase.OpenOrder(aKind: TStockOrderKind; aLots:TStockOrderLots; aStopLoss, aTakeProfit,
  aTrailingStop: TSCRealNumber; const aComment: string): IStockOrder;
begin
  result:=nil;

  result:=CreateEmptyOrder;
  try
    //открываемся
    result.Open(GetSymbol, aKind,aLots,aComment);

    result.SetStopLoss(aStopLoss);
    result.SetTakeProfit(aTakeProfit);

    //Выставляем trailing stop
    result.SetTrailingStop(aTrailingStop);
  except
    FOrders.Remove(result);
    raise;
  end;
end;

function TStockTraderBase.OpenOrderAt(aKind: TStockOrderKind; aPrice: TStockRealNumber;
  aLots:TStockOrderLots; aStopLoss, aTakeProfit, aTrailingStop: TSCRealNumber;
  const aComment: string): IStockOrder;
begin
  result:=nil;

  result:=CreateEmptyOrder;
  //открываемся
  result.OpenAt(GetSymbol, aKind,aPrice,aLots,aComment);

  result.SetStopLoss(aStopLoss);
  result.SetTakeProfit(aTakeProfit);

  //Выставляем trailing stop
  result.SetTrailingStop(aTrailingStop);
end;

procedure TStockTraderBase.Pause(const aMessage: string);
begin
  with TfmResume.Create(nil) do
  begin
    laText.Caption:=aMessage;
    Show;
    while Visible do
      Forms.Application.ProcessMessages;
    Free;
  end;
end;

function TStockTraderBase.OpenOrderAt(aKind: TStockOrderKind; aPrice: TStockRealNumber; const aComment: string): IStockOrder;
var
  aLots : TStockOrderLots;
begin
  result:=nil;

  aLots:=GetRecommendedLots;
  if aLots=0 then
    raise EStockError.CreateFmt('Deposit too small to open order (Type=%s)', [OrderKindNames[aKind]]);

  result:=CreateEmptyOrder;
  //открываемся
  result.OpenAt(GetSymbol, aKind,aPrice,aLots,aComment);

  //Выставляем TakeProfit и StopLoss
  case aKind of
    okBuy: begin
      result.SetStopLoss(result.GetPendingOpenPrice-FCachedPropMaxSubsidence);
      result.SetTakeProfit(result.GetPendingOpenPrice+FCachedPropEnoughProfit);
    end;
    okSell: begin
       result.SetStopLoss(result.GetPendingOpenPrice+FCachedPropMaxSubsidence);
       result.SetTakeProfit(result.GetPendingOpenPrice-FCachedPropEnoughProfit);
    end;
    else
      raise EAlgoError.Create;
  end;

  //Выставляем trailing stop
  SetTrailingStopAccordingProperty(result);
end;

function TStockTraderBase.GetEventHandler(index: integer): IStockTraderEventHandler;
begin
  result:=IStockTraderEventHandler(FHandlers[index]);
end;

function TStockTraderBase.EventHandlerCount: integer;
begin
  result:=FHandlers.Count;
end;

procedure TStockTraderBase.SetName(const aValue: string);
begin
  FName:=aValue;
  OnPropertyChanged(nil);
end;

procedure TStockTraderBase.SetCategory(const aValue: string);
begin
  FCategory:=aValue;
  OnPropertyChanged(nil);
end;

procedure TStockTraderBase.AddExpert(const aExpert: ISCExpert);
begin
  FExperts.Add(aExpert);
end;

procedure TStockTraderBase.AddMarkToChart(const aTimeInterval: TStockTimeInterval; const aTime: TDateTime;
  const aPrice: TStockRealNumber; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  if aMarkType=mkNone then
    raise EStockError.Create('mkNone is prohibited to use');

  FProject.GetStockChart(aTimeInterval).OnTraderSetMark(self,aTime,aPrice,aMarkType,aMessage);
end;

procedure TStockTraderBase.AddMarkToCharts(const aTime: TDateTime; const aPrice: TStockRealNumber;
  const aMarkType: TSCChartMarkKind; const aMessage: string);
var
  aInterval: TStockTimeInterval;
begin
  if aMarkType=mkNone then
    raise EStockError.Create('mkNone is prohibited to use');

  for aInterval:=low(TStockTimeInterval) to high(TStockTimeInterval) do
    FProject.GetStockChart(aInterval).OnTraderSetMark(self,aTime,aPrice,aMarkType,aMessage);
end;

function TStockTraderBase.ExpertCount: integer;
begin
  result:=FExperts.Count;
end;

function TStockTraderBase.ExternalIdentifierCount: integer;
begin
  result:=FExternalIdentifiers.Count;
end;

function TStockTraderBase.FindExternalIdentifier(const aIndicator: ISCIndicator; aIndicatorIdentification: string): IStockTraderExternalIdentifier;
var
  i: integer;
begin
  //Ищем, нет ли такого атриубута у индикатора уже
  for i := 0 to aIndicator.GetAttributes.Count-1 do
    if (Supports (aIndicator.GetAttributes.Items[i],IStockTraderExternalIdentifier,result)) and
       (IsEqualGUID(result.GetTraderIID,FIID)) then
    begin
      if result.GetIdentification=aIndicatorIdentification then
        exit; //Уже идентифицирован
    end;

  result:=nil;
end;

function TStockTraderBase.FindOrderByState(aState: TStockOrderState; aForward: boolean): IStockOrder;
var
  i: integer;
begin
  result:=nil;
  if aForward then
  begin
    for i := 0 to FOrders.Count - 1 do
      if FOrders[i].GetState=aState then
      begin
        result:=FOrders[i];
        break;
      end;
  end
  else begin
    for i := FOrders.Count - 1 downto 0 do
      if FOrders[i].GetState=aState then
      begin
        result:=FOrders[i];
        break;
      end;
  end;
end;

function TStockTraderBase.GetExternalIdentifier(index: integer): IStockTraderExternalIdentifier;
begin
  result:=FExternalIdentifiers[index] as IStockTraderExternalIdentifier;
end;

function TStockTraderBase.GetExpert(aIndex: integer): ISCExpert;
begin
  result:=FExperts[aIndex] as ISCExpert;
end;

procedure TStockTraderBase.ReadData(const aReader: IDataReader);
begin
  Include(FState,isReading);
  inherited;
  Exclude(FState,isReading);
end;

procedure TStockTraderBase.OnAddObject(const aIndicator:ISCIndicator; aIndicatorIdentification: string);
var
  aId    : IStockTraderExternalIdentifier;
  aIdImpl: TStockTraderExternalIdentifier;
  i: integer;
begin
  aIndicator.AddUsing('Trader "'+GetName+'"');

  //Ищем, не создавали ли мы раньше такой идентификатор
  aId:=FindExternalIdentifier(aIndicator,aIndicatorIdentification);

  //Если не нашли
  if aId=nil then
  begin
    //Проверка. Идентификатор не должен повторяться. Поэтому если у нас такой идентификатор
    //зарегистрирован, то скорее, это ошибка в наследниках - пытаются использовать один и тот же Id
    for i := 0 to  ExternalIdentifierCount - 1 do
    begin
       if ExternalIdentifiers[i].GetIdentification=aIndicatorIdentification then
         raise EStockError.Create('Duplicate identification string');
    end;

    Assert(FExternalIdentifierClass<>nil);
    aIdImpl:=FExternalIdentifierClass.Create;
    aIdImpl.Identification:=aIndicatorIdentification;
    aIdImpl.FTraderIID:=FIID;

    aId:=aIdImpl;

    //Добавляем в коллекцию
    FExternalIdentifiers.Add(aId);

    //Нужно дописать свои идентификационные данные к индикатору
    Assert(aId<>nil);
    Assert(aId.GetIdentification=aIndicatorIdentification);
    aIndicator.GetAttributes.Add(aId);
  end;
end;

procedure TStockTraderBase.OnBeginWorkSession;
begin
  CachePropertyValues;
end;

procedure TStockTraderBase.OnEndWorkSession;
begin

end;

procedure TStockTraderBase.OnRemoveObject(const aIndicator:ISCIndicator);
var
  i: integer;
  aId    : IStockTraderExternalIdentifier;
  aOurs  : boolean;
begin
  aIndicator.RemoveUsing('Trader "'+GetName+'"');

  aOurs:=false;
  //Удаляем свои идентификационные данные
  for i := aIndicator.GetAttributes.Count-1 downto 0 do
    if (Supports (aIndicator.GetAttributes.Items[i],IStockTraderExternalIdentifier,aId)) and
       (IsEqualGUID(aId.GetTraderIID,FIID)) then
    begin
      aIndicator.GetAttributes.Remove(aId);
      aOurs:=true;
    end;

  if aOurs then
    if GetParentStockChart(aIndicator)<>nil then
      GetParentStockChart(aIndicator).DeleteIndicator(aIndicator);
end;

procedure TStockTraderBase.OnTestBenchActionExecute(Sender: TObject);
begin
  if FTestBenchDialog=nil then
  begin
    FTestBenchDialog:=TfmTestBenchDialogClass(TestBenchDialogClass).Create(self);
    FTestBenchDialog.Show;
  end
end;

procedure TStockTraderBase.OnTestBenchActionUpdate(Sender: TObject);
begin
  TAction(Sender).Caption:=self.GetName+' - Test Bench';
  TAction(Sender).Checked:=FTestBenchDialog<>nil;
end;

function TStockTraderBase.ShowPropertyWindow: boolean;
var
  aDialog: TSCIndicatorPropertiesDialog;
  aPropList: TPropertyList;
begin
  result:=false;
  aPropList:=TPropertyList.Create;
  try
    GetProperties(aPropList);
    aDialog:=TSCIndicatorPropertiesDialog.Create(aPropList,GetName);
    try
      if aDialog.ShowModal=mrOk then
        result:=true;
    finally
      aDialog.Free;
    end;
  finally
    aPropList.Free;
  end;
end;

procedure TStockTraderBase.GetProperties(aList: TPropertyList);
var
  i: integer;
begin
  if FVisibleProperties<>nil then
    for i:=0 to FVisibleProperties.Count-1 do
      aList.Add(FVisibleProperties[i]);
end;

procedure TStockTraderBase.OnPropertyChanged(aNotifier: TProperty);
var
  i: integer;
begin
  for i:=0 to EventHandlerCount-1 do
    EventHandler[i].OnPropertiesChanged(self);

  CachePropertyValues;
end;

procedure TStockTraderBase.OnPropertyCreated(aNotifier: TProperty);
begin

end;

function TStockTraderBase.GetProperties: IPropertyCollection;
var
  aCollection: TPropertyCollection;
begin
  aCollection:=TPropertyCollection.Create;
  GetProperties(aCollection.List);
  result:=aCollection;
end;

function TStockTraderBase.GetRecommendedLots: TStockOrderLots;
begin
  if FPropLotDynamicRate.Value then
    result:=(FBroker.GetEquity/FBroker.GetMargin*(FPropLotDynamicRateSize.Value/100))/10
  else
    result:=FPropLotDefaultRateSize.Value;
end;

procedure TStockTraderBase.RegisterProperties(aProperties: array of TProperty);
var
  i: integer;
begin
  if Length(aProperties)=0 then
    exit;

  if FVisibleProperties=nil then
    FVisibleProperties:=TPropertyList.Create;

  for i:=0 to High(aProperties) do
  begin
    if FVisibleProperties.IndexOf(aProperties[i])=-1 then
      FVisibleProperties.Add(aProperties[i]);
  end;
end;

procedure TStockTraderBase.UnRegisterProperties(aProperties: array of TProperty);
var
  i: integer;
begin
  if Length(aProperties)=0 then
    exit;

  if FVisibleProperties=nil then
    exit;

  for i:=0 to High(aProperties) do
    FVisibleProperties.Remove(aProperties[i]);
end;

procedure TStockTraderBase.CachePropertyValues;
begin
  if FBroker=nil then
    exit;

  if FPropMaxSubsidence<>nil then
    FCachedPropMaxSubsidence:=GetBroker.PointToPrice(GetSymbol,FPropMaxSubsidence.Value);

  if FPropTrailingStop<>nil then
    FCachedPropTrailingStop:=GetBroker.PointToPrice(GetSymbol,FPropTrailingStop.Value);

  if FPropTrailingStopDescend<>nil then
    FCachedPropTrailingStopDescend:=FPropTrailingStopDescend.Value;

  if FPropEnoughProfit<>nil then
    FCachedPropEnoughProfit:=GetBroker.PointToPrice(GetSymbol,FPropEnoughProfit.Value);
end;

function TStockTraderBase.CanSetStopLoss(const aOrder: IStockOrder; aStopLevel: TStockRealNumber): boolean;
var
  aMarketPrice: TStockRealNumber;
begin
  result:=true;
  if aOrder.GetKind= okBuy then
    aMarketPrice:=GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkBid)
  else
    aMarketPrice:=GetBroker.GetCurrentPrice(aOrder.GetSymbol,bpkAsk);

  //Проверим, чтобы SL не был слшиком близко к маркет-цене
  if Abs(GetBroker.PriceToPoint(aOrder.GetSymbol,aStopLevel-aMarketPrice))<GetBroker.GetMarketInfo(aOrder.GetSymbol).StopLevel then
    result:=false;
end;

procedure TStockTraderBase.SetBroker(const aBroker: IStockBroker);
begin
  if aBroker=FBroker then
    exit;

  //Если брокера сняли, считаем, что конец текущий работы
  if (aBroker=nil) then
    OnEndWorkSession;

  FBroker:=aBroker;

  if (FBroker<>nil) then
    OnBeginWorkSession;
end;

function TStockTraderBase.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockTraderBase.GetSymbol: string;
begin
  result:=FProject.GetStockSymbol;
end;

function TStockTraderBase.TestBenchDialogClass: TClass;
begin
  result:=nil;
end;

procedure TStockTraderBase.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  if FProject<>nil then
    OnReleaseObjects;

  FProject:=aValue;

  if FProject<>nil then
    OnCreateObjects;
end;

procedure TStockTraderBase.SetStopLossAccordingProperty(aOrder: IStockOrder);
var
  aMinimizationRisksType : TMinimizationLossType;
  aStopLoss: TStockRealNumber;
begin
  if aOrder.GetState in [osClosed,osNothing] then
    exit;

  aMinimizationRisksType:=FMinimizationRiskType.Value;

  //Нужно перевести в безубыточное состояние ордер, поэтому
  //как только появится возможность поставить стоп с безубыточной стороны - делаем это
  if (aMinimizationRisksType<>mltNone) and (GetExpectedLoss(aOrder)>0) then
  begin
    if (aMinimizationRisksType=mltFixAtZeroPoint) and (CanSetStopLoss(aOrder,aOrder.GetOpenPrice)) then
    begin
      aOrder.SetStopLoss(aOrder.GetOpenPrice);
      GetBroker.AddMessage(aOrder,'Zero stop loss ('+PriceToStr(aOrder,aOrder.GetOpenPrice)+') was set');
    end
    //безубыточного стоп лоса нет, но хотя бы подвинем его как можно ближе
    else if aMinimizationRisksType=mltMinimizeAll then
    begin
      aStopLoss:=max(aOrder.GetStopLoss,GetNearestStopLossToMarketPrice(aOrder));
      if (CanSetStopLoss(aOrder,aStopLoss)) then
        aOrder.SetStopLoss(aStopLoss);
    end;
  end;
end;

procedure TStockTraderBase.SetTrailingStopAccordingProperty(aOrder: IStockOrder);
var
  aK : TStockRealNumber;
begin
  if aOrder.GetState in [osClosed,osNothing] then
    exit;

  if FCachedPropTrailingStop>0 then
  begin
    //Динамический Trailing Stop
    aK:=max(0,FCachedPropTrailingStop-max(0,FCachedPropTrailingStopDescend*(aOrder.GetBestProfit-FCachedPropTrailingStop)));
    aK:=max(aK,GetBroker.PointToPrice(aOrder.GetSymbol,GetBroker.GetMarketInfo(GetSymbol).StopLevel));
    aK:=GetBroker.RoundPrice(aOrder.GetSymbol,aK);
    aOrder.SetTrailingStop(aK);
  end;
end;

function TStockTraderBase.GetExpectedLoss(const aOrder: IStockOrder): TStockRealNumber;
begin
  if aOrder.GetKind=okBuy then
    result:=aOrder.GetOpenPrice-GetExpectedStopLossPrice(aOrder)
  else
    result:=GetExpectedStopLossPrice(aOrder) - aOrder.GetOpenPrice;
end;

function TStockTraderBase.GetExpectedStopLossPrice(aOrder: IStockOrder): TStockRealNumber;
begin
  result:=aOrder.GetStopLoss;
  if (aOrder.GetState=osOpened) and (aOrder.GetTrailingStop>0) then
    if aOrder.GetKind=okBuy then
      result:=max(result,aOrder.GetBestPrice-aOrder.GetTrailingStop)
    else
      result:=min(result,aOrder.GetBestPrice+aOrder.GetTrailingStop);
end;


function TStockTraderBase.GetProject: IStockProject;
begin
  result:=FProject;
end;

function TStockTraderBase.IndexOfExpert(const aExpert: ISCExpert): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to ExpertCount-1 do
    if GetExpert(i).IsThis(aExpert) then
    begin
      result:=i;
      break;
    end;
end;

function TStockTraderBase.RemoveExpert(const aExpert: ISCExpert):integer;
begin
  result:=IndexOfExpert(aExpert);
  if result<>-1 then
    DeleteExpert(result);
end;

procedure TStockTraderBase.DeleteExpert(index: integer);
begin
  OnRemoveObject(GetExpert(index));
  FExperts.Delete(index);
end;

function TStockTraderBase.IsLevelTooCloseToCurrentPrice(aPriceKind: TStockBrokerPriceKind; aLevel: TSCRealNumber): boolean;
var
  aPrice: TSCRealNumber;
  aStopDelta: TStockRealNumber;
begin
  //Берем +1, чтобы наверняка
  aStopDelta:=GetBroker.PointToPrice(GetSymbol,GetBroker.GetMarketInfo(GetSymbol).StopLevel+1);
  aPrice:=GetBroker.GetCurrentPrice(GetSymbol,aPriceKind);

  result:=Abs(aPrice-aLevel)>=aStopDelta;
end;

function TStockTraderBase.IsThis(const aTrader: IStockTrader): boolean;
begin
  result:=IsEqualGUID(aTrader.GetID,self.GetID);
end;

function TStockTraderBase.CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean=true): ISCIndicator;
var
  aRes: boolean;
begin
  result:=CreateOrFindIndicator(aChart,aIID,aIndicatorIdentification,aRegister, aRes);
end;

function TStockTraderBase.CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean; out aCreated: boolean): ISCIndicator;
var
  aIndicators: ISCIndicatorCollection;
  aId: IStockTraderExternalIdentifier;
  i: integer;
begin
  //Ищем среди уже существующих
  aIndicators:=aChart.FindIndicators(aIID);
  aId:=nil;
  result:=nil;

  for i := 0 to aIndicators.Count- 1 do
  begin
    aId:=FindExternalIdentifier(aIndicators.Items[i],aIndicatorIdentification);
    if aId<>nil then
    begin
      result:=aIndicators.Items[i];
      break;
    end;
  end;

  //Итак, нашли среди существующих
  if result<>nil then
  begin
    aCreated:=false;
  end
  //Если нет - тогда создаем
  else begin
    result:=aChart.CreateIndicator(IndicatorFactory.GetIndicatorInfo(aIID),false);
    aCreated:=true;
  end;

  //Регистрируем индикатор
  if aRegister then
    OnAddObject(result,aIndicatorIdentification);
end;

{ TStockTraderExternalIdentifier }

constructor TStockTraderExternalIdentifier.Create;
begin
  inherited Create;
end;

function TStockTraderExternalIdentifier.GetIdentification: string;
begin
  result:=FIdentification;
end;

function TStockTraderExternalIdentifier.GetTraderIID: TGUID;
begin
  result:=FTraderIID;
end;

procedure TStockTraderExternalIdentifier.OnDefineValues;
begin
  inherited;
  DefValString('Identification',FIdentification);
end;

procedure TStockTraderExternalIdentifier.SetIdentification(const Value: string);
begin
  FIdentification := Value;
end;

{ TStockBrokerEventHandler }

constructor TStockBrokerEventHandler.Create(aOwner: TStockTraderBase);
begin
  inherited Create;
  FOwner:=aOwner;
end;

destructor TStockBrokerEventHandler.Destroy;
begin
  Assert(FOwner=nil);
  inherited;
end;

procedure TStockBrokerEventHandler.OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
begin
  if (FOwner<>nil) and (IStockTrader(FOwner)=aOrder.GetTrader) then
    FOwner.OnModifyOrder(aOrder,aModifyEventArgs);
end;

procedure TStockBrokerEventHandler.OnNewData(const aSender: IStockBroker; const aSymbol: string);
begin
end;

procedure TStockBrokerEventHandler.OnNewMessage(const aSender: IStockBroker; const aMessage: IStockBrokerMessage);
begin
end;

procedure TStockBrokerEventHandler.OnNewMessage(const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage);
begin
end;

procedure TStockBrokerEventHandler.OnNewOrder(const aSender: IStockBroker; const aOrder: IStockOrder);
begin

end;

procedure TStockBrokerEventHandler.OnStart(const aSender: IStockBroker);
begin

end;

initialization
  Serialization.TClassFactory.RegisterClass(TStockTraderExternalIdentifier);
  Serialization.TClassFactory.RegisterClass(TStockTraderBase);
end.





