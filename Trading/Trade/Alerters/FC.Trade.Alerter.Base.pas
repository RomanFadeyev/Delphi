{-----------------------------------------------------------------------------
 Unit Name:
 Author:    Roman
 Purpose:   Базовый класс для всех трейдеров. Реализует интерфейс IStockAlerter

 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Alerter.Base;
{$I Compiler.inc}

{$DEFINE IGNORE_UNKNOWN_PROPERTIES}

interface

uses
  Classes, Windows, Math,Contnrs, Controls, SysUtils, BaseUtils, ActnList, Forms, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Drawing,
  Serialization, FC.Definitions;

type
  { TStockAlerter }

  TSCAlerterState = (isReading);
  TSCAlerterStates = set of TSCAlerterState;

  TStockAlerterBase = class;
  TStockAlerterClass = class of TStockAlerterBase;

  TLastOrderType = (lotNone,lotBuy,lotSell);

  TLogRecord = class
  public
    DateTime: TDateTime;
    Text    : string;
  end;

  //Псевдо-интерфейс, необходимый для генерации уникального GUID
  //Будем использовать его в ExternalData экспертов, чтобы отличить
  //где наш эксперта, а где не наш
  IStockAlerterExternalIdentifier = interface (ISCAttribute)
  ['{40DE82A4-840E-4AA3-A89C-78BBE85691D0}']
    function  GetIdentification: string;
    function  GetAlerterIID: TGUID;
  end;

  //... и собственно класс, его реализующий
  TStockAlerterExternalIdentifier = class (TNameValuePersistentObjectRefCounted,IStockAlerterExternalIdentifier,ISCAttribute)
  private
    FAlerterIID: TGUID;
    FIdentification: string;
    procedure SetIdentification(const Value: string);
    function  GetIdentification: string;
    function  GetAlerterIID: TGUID;
  protected
    procedure OnDefineValues; override;

    property Identification: string read FIdentification write SetIdentification;
    property AlerterIID: TGUID read FAlerterIID write FAlerterIID;

    constructor Create; virtual;
  end;

  TStockAlerterExternalIdentifierClass = class of TStockAlerterExternalIdentifier;
  TStockBrokerEventHandler = class;

  //Базовый
  TStockAlerterBase = class (TNameValuePersistentObjectRefCounted,IStockAlerter,IPropertyChangeHandler)
  private
    FExternalIdentifiers: TInterfaceList; //of IStockAlerterExternalIdentifier
    FExternalIdentifierClass: TStockAlerterExternalIdentifierClass;
    FIID       : TGUID;
    FCategory  : string;
    FName      : string;
    FHandlers  : TInterfaceList;
    FLastTime  : TDateTime;
    FBroker    : IStockBroker;
    FProject   : IStockProject;
    FState     : TSCAlerterStates;
    FEnabled   : boolean;
    FBrokerEventHandler : TStockBrokerEventHandler;

    FPropStartFrom  : TPropertyTime;
    FPropStopAt     : TPropertyTime;
    FPropBreak      : TPropertySmallUint;
    FPropShowMessage: TPropertyYesNo;
    FPropSendMail   : TPropertyYesNo;
    FPropPlaySound  : TPropertyYesNo;

    FVisibleProperties: TPropertyList;

    function  GetExternalIdentifier(index: integer): IStockAlerterExternalIdentifier;
    function  GetEventHandler(index: integer): IStockAlerterEventHandler;
    procedure CachePropertyValues;
  protected
    FID        : TGUID;

    //Сериализация (TNameValuePersistentObjectRefCounted)
    procedure OnDefineValues; override;
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;

    procedure AddMessage(const a_Message: string);
    procedure FormatMessage(const aMessage: string; out aCaption,aText: string); virtual;

    //Вспомогательная функция для находждения "наших" индикаторов на указанном чарте
    //Если таковой не находится - мы его создаем. Параметр aIndicatorIdentification должен быть
    //уникален для каждого создаваемого индикатора
    //Параметр aRegister указывает, регистрировать ли созданный индикатор в трейдере (назначить ему атрибут
    //ExternalId трейдера). По регистрации см. также OnAddObject
    function  CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean=true): ISCIndicator; overload;
    function  CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean; out aCreated: boolean): ISCIndicator; overload;

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

    procedure GetProperties(aList: TPropertyList); overload; virtual;
    function  GetProperties: IPropertyCollection; overload;

    procedure RegisterProperties(aProperties: array of TProperty);
    procedure UnRegisterProperties(aProperties: array of TProperty);

    property  ExternalIdentifierClass: TStockAlerterExternalIdentifierClass read FExternalIdentifierClass write FExternalIdentifierClass;

    function  ExternalIdentifierCount: integer;
    property  ExternalIdentifiers[index: integer]: IStockAlerterExternalIdentifier read GetExternalIdentifier;

    //Найти наш идентификатор в указанном индикаторе. Если нет, возврашает nil;
    function  FindExternalIdentifier(const aIndicator:ISCIndicator; aIndicatorIdentification: string): IStockAlerterExternalIdentifier;

    property State     : TSCAlerterStates read FState;

    property PropStartFrom  : TPropertyTime read FPropStartFrom;
    property PropStopAt     : TPropertyTime read FPropStopAt;
    property PropBreak      : TPropertySmallUint read FPropBreak;
    property PropShowMessage: TPropertyYesNo read FPropShowMessage;
    property PropSendMail   : TPropertyYesNo read FPropSendMail;
    property PropPlaySound  : TPropertyYesNo read  FPropPlaySound;

    constructor Create; virtual;
  public
    //from IPersistentObject
    procedure ReadData(const aReader: IDataReader);  override;

    //from IStockAlerter
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

    function  GetEnabled: boolean;
    procedure SetEnabled(const aValue : boolean); virtual;

    //Посчитать
    procedure IStockAlerter.Update = UpdateStep1;
    procedure UpdateStep1(const aTime: TDateTime); virtual;
    procedure UpdateStep2(const aTime: TDateTime); virtual; abstract;
    procedure Invalidate;

    procedure AddEventHandler(const aHandler: IStockAlerterEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockAlerterEventHandler);

    function  ShowPropertyWindow: boolean;

    function  IsThis(const aAlerter: IStockAlerter): boolean;
    //end of IStockAlerter

    property IID: TGUID read FIID write FIID;

    function EventHandlerCount: integer;
    property EventHandler[index:integer]:IStockAlerterEventHandler read GetEventHandler;

    procedure Dispose; virtual;

    constructor CreateNaked; override;
    destructor Destroy; override;
  end;

  TStockBrokerEventHandler = class (TInterfacedObject,IStockBrokerEventHandler)
  private
    FOwner: TStockAlerterBase;
  public
    constructor Create (aOwner: TStockAlerterBase);
    destructor Destroy; override;

    procedure OnStart (const aSender: IStockBroker);
    procedure OnNewOrder   (const aSender: IStockBroker; const aOrder: IStockOrder);
    procedure OnModifyOrder(const aSender: IStockBroker; const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
    procedure OnNewData    (const aSender: IStockBroker; const aSymbol: string);
    procedure OnNewMessage (const aSender: IStockBroker; const aMessage: IStockBrokerMessage); overload;
    procedure OnNewMessage (const aSender: IStockBroker; const aOrder: IStockOrder; const aMessage: IStockBrokerMessage); overload;
  end;

implementation
  uses SystemService, Log, FC.Trade.OrderCollection, FC.Singletons,
  StockChart.Indicators.Properties.Dialog, FC.DataUtils, FC.General.Mailer, StockChart.Obj,
  FC.fmUIDataStorage,Application.Definitions;


{ TStockAlerterBase }

constructor TStockAlerterBase.CreateNaked;
begin
  FState:=[isReading];
  inherited;
  Create;
  Exclude(FState,isReading);
end;

constructor TStockAlerterBase.Create;
begin
  inherited Create;
  FBrokerEventHandler:=TStockBrokerEventHandler.Create(self);
  IInterface(FBrokerEventHandler)._AddRef;

  FExternalIdentifiers:=TInterfaceList.Create;
  FHandlers:=TInterfaceList.Create;

  FExternalIdentifierClass:=TStockAlerterExternalIdentifier;

  CreateGUID(FID);
  FLastTime:=-1;

  FPropStartFrom:=TPropertyTime.Create('Time','Start From',self);
  FPropStartFrom.Value:=0;
  FPropStartFrom.HelpString:='You can set start time of the alerter';

  FPropStopAt:=TPropertyTime.Create('Time','Stop At',self);
  FPropStopAt.Value:=0;
  FPropStartFrom.HelpString:='You can set stop time of the alerter';

  FPropBreak:=TPropertySmallUint.Create('Time','Break In Minutes',self);
  FPropBreak.Value:=5;
  FPropBreak.HelpString:='The break between alerter updates';

  FPropShowMessage:= TPropertyYesNo.Create('Notification','Show Baloon',self);
  FPropShowMessage.Value:=true;

  FPropSendMail   := TPropertyYesNo.Create('Notification','Send E-Mail',self);
  FPropPlaySound  := TPropertyYesNo.Create('Notification','Play Sound',self);

  RegisterProperties([FPropStartFrom,FPropStopAt,FPropBreak,FPropShowMessage,FPropSendMail,FPropPlaySound]);

  CachePropertyValues;

  StockBrokerConnectionRegistry.AddBrokerEventHandler(FBrokerEventHandler);
end;

destructor TStockAlerterBase.Destroy;
begin
  FBrokerEventHandler.FOwner:=nil;
  StockBrokerConnectionRegistry.RemoveBrokerEventHandler(FBrokerEventHandler);
  IInterface(FBrokerEventHandler)._Release;
  FBrokerEventHandler:=nil;

  //FMessages:=nil;
  FreeAndNil(FHandlers);
  FreeAndNil(FVisibleProperties);
  FreeAndNil(FExternalIdentifiers);
  inherited;
end;

procedure TStockAlerterBase.Dispose;
begin
  if FVisibleProperties<>nil then
    FVisibleProperties.Clear;

  FreeAndNil(FPropStartFrom);
  FreeAndNil(FPropStopAt);
  FreeAndNil(FPropBreak);
  FreeAndNil(FPropShowMessage);
  FreeAndNil(FPropSendMail);
  FreeAndNil(FPropPlaySound);      

  FBroker:=nil;
  FProject:=nil;
end;

function TStockAlerterBase.GetID: TGUID;
begin
  result:=FID;
end;

procedure TStockAlerterBase.Invalidate;
var
  i: integer;
begin
  FLastTime:=-1;
  for i:=0 to EventHandlerCount-1 do
    EventHandler[i].OnInvalidate(self);
end;

function TStockAlerterBase.GetCategory: string;
begin
  result:=FCategory;
end;

function TStockAlerterBase.GetName: string;
begin
  result:=FName;
end;

procedure TStockAlerterBase.UpdateStep1(const aTime: TDateTime);
begin
  if not FEnabled then
    exit;

  if FLastTime>=aTime then
    exit;

  //Не чаще указанного диапазона
  if TStockDataUtils.ToMinutes(aTime-FLastTime)<FPropBreak.Value then
    exit;

  if ((Frac(aTime)>=FPropStartFrom.Value) and (Frac(aTime)<FPropStopAt.Value)) or
     (FPropStartFrom.Value=FPropStopAt.Value) then
    try
      UpdateStep2(aTime);
    except
      on E:EAbort do;
    end;

  FLastTime:=aTime;
end;

procedure TStockAlerterBase.OnDefineValues;
begin
  inherited;
  DefValGUID('ID',FID);
  DefValString('Category',FCategory);
  DefValString('Name',FName);
  DefValBoolean('Enabled',FEnabled);
end;

procedure TStockAlerterBase.OnWriteValues(const aWriter: INameValueDataWriter);
var
  aPropList: TPropertyList;
  i: integer;
begin
  inherited;

  aPropList:=TPropertyList.Create;
  try
    GetProperties(aPropList);
    for i:=0 to aPropList.Count-1 do
      aWriter.WriteVariant('Property:'+aPropList[i].Category+'\'+aPropList[i].Name,aPropList[i].Value);
  finally
    aPropList.Free;
  end;
end;

procedure TStockAlerterBase.OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean);
var
  aPropList: TPropertyList;
  i,j: integer;
  s: string;
  v: variant;
begin
  inherited;

  if StrIsInLeft('Property:',pchar(aName)) then
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

procedure TStockAlerterBase.OnCreateObjects;
begin

end;

procedure TStockAlerterBase.OnReleaseObjects;
begin

end;

procedure TStockAlerterBase.AddEventHandler(const aHandler: IStockAlerterEventHandler);
begin
  if FHandlers.IndexOf(aHandler)<>-1 then
    raise ESCItemAlreadyExists.Create;

  FHandlers.Add(aHandler);
end;

procedure TStockAlerterBase.RemoveEventHandler(const aHandler: IStockAlerterEventHandler);
begin
  FHandlers.Remove(aHandler);
end;

function TStockAlerterBase.GetEnabled: boolean;
begin
  result:=FEnabled;
end;

function TStockAlerterBase.GetEventHandler(index: integer): IStockAlerterEventHandler;
begin
  result:=FHandlers[index] as IStockAlerterEventHandler;
end;

function TStockAlerterBase.EventHandlerCount: integer;
begin
  result:=FHandlers.Count;
end;

procedure TStockAlerterBase.SetName(const aValue: string);
begin
  FName:=aValue;
  OnPropertyChanged(nil);
end;

procedure TStockAlerterBase.SetCategory(const aValue: string);
begin
  FCategory:=aValue;
  OnPropertyChanged(nil);
end;

procedure TStockAlerterBase.SetEnabled(const aValue: boolean);
begin
  FEnabled:=aValue;
end;

procedure TStockAlerterBase.AddMessage(const a_Message: string);
var
  aCaption,aText: string;
begin
  aText:=DateTimeToStr(GetBroker.GetCurrentTime)+#9+GetSymbol+#9+self.GetName+': '+a_Message;
  Log.AppendLineToFile(AppPath+'alerts.log',aText);

  FormatMessage(a_Message,aCaption,aText);
  if PropShowMessage.Value then
     Workspace.MainFrame.ShowTrayBalloonHint(aCaption,aText,btInfo);

  if PropSendMail.Value then
    with TMailer.Create() do
    begin
      MailSafe(aCaption,aText);
      Free;
    end;
end;

function TStockAlerterBase.ExternalIdentifierCount: integer;
begin
  result:=FExternalIdentifiers.Count;
end;

function TStockAlerterBase.FindExternalIdentifier(const aIndicator: ISCIndicator; aIndicatorIdentification: string): IStockAlerterExternalIdentifier;
var
  i: integer;
begin
  //Ищем, нет ли такого атриубута у индикатора уже
  for i := 0 to aIndicator.GetAttributes.Count-1 do
    if (Supports (aIndicator.GetAttributes.Items[i],IStockAlerterExternalIdentifier,result)) and
       (IsEqualGUID(result.GetAlerterIID,FIID)) then
    begin
      if result.GetIdentification=aIndicatorIdentification then
        exit; //Уже идентифицирован
    end;

  result:=nil;
end;

procedure TStockAlerterBase.FormatMessage(const aMessage: string; out aCaption, aText: string);
begin
  aCaption:='Alerter "'+self.GetName+'"';
  aText:=DateTimeToStr(GetBroker.GetCurrentTime)+', '+GetSymbol+#13#10+aMessage;
end;

function TStockAlerterBase.GetExternalIdentifier(index: integer): IStockAlerterExternalIdentifier;
begin
  result:=FExternalIdentifiers[index] as IStockAlerterExternalIdentifier;
end;

procedure TStockAlerterBase.ReadData(const aReader: IDataReader);
begin
  Include(FState,isReading);
  inherited;
  Exclude(FState,isReading);
end;

procedure TStockAlerterBase.OnAddObject(const aIndicator:ISCIndicator; aIndicatorIdentification: string);
var
  aId    : IStockAlerterExternalIdentifier;
  aIdImpl: TStockAlerterExternalIdentifier;
  i: integer;
begin
  aIndicator.AddUsing('Alerter "'+GetName+'"');

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
    aIdImpl.FAlerterIID:=FIID;

    aId:=aIdImpl;

    //Добавляем в коллекцию
    FExternalIdentifiers.Add(aId);

    //Нужно дописать свои идентификационные данные к индикатору
    Assert(aId<>nil);
    Assert(aId.GetIdentification=aIndicatorIdentification);
    aIndicator.GetAttributes.Add(aId);
  end;
end;

procedure TStockAlerterBase.OnBeginWorkSession;
begin

end;

procedure TStockAlerterBase.OnEndWorkSession;
begin

end;

procedure TStockAlerterBase.OnRemoveObject(const aIndicator:ISCIndicator);
var
  i: integer;
  aId    : IStockAlerterExternalIdentifier;
  aOurs  : boolean;
begin
  aIndicator.RemoveUsing('Alerter "'+GetName+'"');

  aOurs:=false;
  //Удаляем свои идентификационные данные
  for i := aIndicator.GetAttributes.Count-1 downto 0 do
    if (Supports (aIndicator.GetAttributes.Items[i],IStockAlerterExternalIdentifier,aId)) and
       (IsEqualGUID(aId.GetAlerterIID,FIID)) then
    begin
      aIndicator.GetAttributes.Remove(aId);
      aOurs:=true;
    end;

  if aOurs then
    if GetParentStockChart(aIndicator)<>nil then
      GetParentStockChart(aIndicator).DeleteIndicator(aIndicator);
end;

function TStockAlerterBase.ShowPropertyWindow: boolean;
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

procedure TStockAlerterBase.GetProperties(aList: TPropertyList);
var
  i: integer;
begin
  if FVisibleProperties<>nil then
    for i:=0 to FVisibleProperties.Count-1 do
      aList.Add(FVisibleProperties[i]);
end;

procedure TStockAlerterBase.OnPropertyChanged(aNotifier: TProperty);
var
  i: integer;
begin
  for i:=0 to EventHandlerCount-1 do
    EventHandler[i].OnPropertiesChanged(self);

  CachePropertyValues;
end;

procedure TStockAlerterBase.OnPropertyCreated(aNotifier: TProperty);
begin

end;

function TStockAlerterBase.GetProperties: IPropertyCollection;
var
  aCollection: TPropertyCollection;
begin
  aCollection:=TPropertyCollection.Create;
  GetProperties(aCollection.List);
  result:=aCollection;
end;

procedure TStockAlerterBase.RegisterProperties(aProperties: array of TProperty);
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

procedure TStockAlerterBase.UnRegisterProperties(aProperties: array of TProperty);
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

procedure TStockAlerterBase.CachePropertyValues;
begin
end;

procedure TStockAlerterBase.SetBroker(const aBroker: IStockBroker);
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

function TStockAlerterBase.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockAlerterBase.GetSymbol: string;
begin
  result:=FProject.GetStockSymbol;
end;

procedure TStockAlerterBase.SetProject(const aValue: IStockProject);
begin
  if GetProject=aValue then
    exit;

  if FProject<>nil then
    OnReleaseObjects;

  FProject:=aValue;

  if FProject<>nil then
    OnCreateObjects;
end;

function TStockAlerterBase.GetProject: IStockProject;
begin
  result:=FProject;
end;

function TStockAlerterBase.IsThis(const aAlerter: IStockAlerter): boolean;
begin
  result:=IsEqualGUID(aAlerter.GetID,self.GetID);
end;

function TStockAlerterBase.CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean=true): ISCIndicator;
var
  aRes: boolean;
begin
  result:=CreateOrFindIndicator(aChart,aIID,aIndicatorIdentification,aRegister, aRes);
end;

function TStockAlerterBase.CreateOrFindIndicator(const aChart: IStockChart; const aIID: TGUID; aIndicatorIdentification: string; aRegister: boolean; out aCreated: boolean): ISCIndicator;
var
  aIndicators: ISCIndicatorCollection;
  aId: IStockAlerterExternalIdentifier;
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

{ TStockAlerterExternalIdentifier }

constructor TStockAlerterExternalIdentifier.Create;
begin
  inherited Create;
end;

function TStockAlerterExternalIdentifier.GetIdentification: string;
begin
  result:=FIdentification;
end;

function TStockAlerterExternalIdentifier.GetAlerterIID: TGUID;
begin
  result:=FAlerterIID;
end;

procedure TStockAlerterExternalIdentifier.OnDefineValues;
begin
  inherited;
  DefValString('Identification',FIdentification);
end;

procedure TStockAlerterExternalIdentifier.SetIdentification(const Value: string);
begin
  FIdentification := Value;
end;

{ TStockBrokerEventHandler }

constructor TStockBrokerEventHandler.Create(aOwner: TStockAlerterBase);
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
end;

procedure TStockBrokerEventHandler.OnNewData(const aSender: IStockBroker; const aSymbol: string);
begin
  if FOwner.GetEnabled then
    if AnsiSameText(FOwner.GetSymbol,aSymbol) then
    begin
      FOwner.SetBroker(aSender);
      FOwner.UpdateStep1(aSender.GetCurrentTime);
    end;
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
  Serialization.TClassFactory.RegisterClass(TStockAlerterExternalIdentifier);
  Serialization.TClassFactory.RegisterClass(TStockAlerterBase);
end.





