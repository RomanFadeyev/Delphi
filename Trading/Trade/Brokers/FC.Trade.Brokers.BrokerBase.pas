unit FC.Trade.Brokers.BrokerBase;

interface
  uses Classes, BaseUtils, SysUtils, Graphics,
  FC.Definitions,StockChart.Definitions;

type
  TStockBrokerBase = class (TInterfacedObject)
  private
    FEventHandlers: array of IStockBrokerEventHandler;
    FMessages : TInterfaceList; //of IStockBrokerMessage

    function GetEventHandler(index: integer): IStockBrokerEventHandler;
    function GetMessage(index: integer): IStockBrokerMessage;
  protected
    procedure RaiseOnStartEvent;
    procedure RaiseOnNewDataEvent(const aSymbol: string);
    procedure RaiseOnNewOrderEvent(const aOrder: IStockOrder);
    procedure RaiseOnModifyOrderEvent(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);

    function EventHandlerCount: integer;
    property EventHandlers[index:integer]: IStockBrokerEventHandler read GetEventHandler;

    function MessageCount: integer;
    property Messages[index:integer]: IStockBrokerMessage read GetMessage;
  public
    //from IStockBroker
    procedure AddEventHandler(const aHandler: IStockBrokerEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockBrokerEventHandler);

    //Добавить произволный комментарий. Время установи комментария берется текущее
    function AddMessage(const aMessage: string; aColor: TColor=clDefault): IStockBrokerMessage; overload;

    //Добавить произволный комментарий. Время установи комментария берется текущее
    function AddMessage(const aOrder:IStockOrder; const aMessage: string; aColor: TColor=clDefault): IStockBrokerMessage;  overload;

     //Кол-во точек после запятой
    function GetPricePrecision(const aSymbol: string) : integer; virtual; abstract;
    //Коэффициент трансформации из точки в цену, для EURUSD=10000, для USDJPY=100
    function GetPricesInPoint(const aSymbol: string): integer; virtual; abstract;

    //Округление цены до PricePrecision
    function RoundPrice(const aSymbol: string; const aPrice: TSCRealNumber) : TSCRealNumber; virtual;
    //Перевод цены в пункты (умножение на PricesInPoint)
    function PriceToPoint(const aSymbol: string; const aPrice: TSCRealNumber) : integer; virtual;
    //Перевод пунктов в цену (деление на PricesInPoint)
    function PointToPrice(const aSymbol: string; const aPoint: integer) : TSCRealNumber; virtual;

    //Перевод цены в деньги с учетом указанного кол-ва лотов
    function PriceToMoney(const aSymbol: string; const aPrice: TSCRealNumber; const aLots: TStockOrderLots): TStockRealNumber; virtual;

    constructor Create;
    destructor Destroy; override;
  end;

  TStockBrokerMessage=class(TInterfacedObject,IStockBrokerMessage)
  private
    FText: string;
    FColor: TColor;
  public
    function Text: string;
    function Color: TColor;

    constructor Create(const aText: string; aColor:TColor=clDefault);
  end;

implementation
  uses Math;

{ TStockBrokerBase }

procedure TStockBrokerBase.AddEventHandler(const aHandler: IStockBrokerEventHandler);
begin
  SetLength(FEventHandlers,Length(FEventHandlers)+1);
  FEventHandlers[High(FEventHandlers)]:=aHandler;
end;

procedure TStockBrokerBase.RaiseOnNewDataEvent(const aSymbol: string);
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  for i:=0 to EventHandlerCount-1 do
    FEventHandlers[i].OnNewData(aStockBroker,aSymbol);
end;

procedure TStockBrokerBase.RaiseOnStartEvent;
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  for i:=0 to EventHandlerCount-1 do
    FEventHandlers[i].OnStart(aStockBroker);
end;

procedure TStockBrokerBase.RaiseOnNewOrderEvent(const aOrder: IStockOrder);
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  for i:=0 to EventHandlerCount-1 do
    FEventHandlers[i].OnNewOrder(aStockBroker,aOrder);
end;

procedure TStockBrokerBase.RaiseOnModifyOrderEvent(const aOrder: IStockOrder; const aModifyEventArgs: TStockOrderModifyEventArgs);
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  for i:=0 to EventHandlerCount-1 do
    FEventHandlers[i].OnModifyOrder(aStockBroker,aOrder,aModifyEventArgs);
end;

procedure TStockBrokerBase.RemoveEventHandler(const aHandler: IStockBrokerEventHandler);
var
  i,j: integer;
begin
  for i := 0 to High(FEventHandlers) do
  begin
    if FEventHandlers[i]=aHandler then
    begin
      //Сдвигаем то, что сзади на место образовавшегося проема
      for j:=i to High(FEventHandlers)-1 do
        FEventHandlers[j]:=FEventHandlers[j+1];
      SetLength(FEventHandlers,Length(FEventHandlers)-1);
      break;
    end;
  end;
end;

function TStockBrokerBase.AddMessage(const aMessage: string; aColor: TColor=clDefault):IStockBrokerMessage;
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  result:=TStockBrokerMessage.Create(aMessage,aColor);
  FMessages.Add(Result);

  for i:=0 to EventHandlerCount-1 do
    EventHandlers[i].OnNewMessage(aStockBroker,result);
end;

function TStockBrokerBase.AddMessage(const aOrder:IStockOrder; const aMessage: string; aColor: TColor=clDefault):IStockBrokerMessage;
var
  i: integer;
  aStockBroker: IStockBroker;
begin
  aStockBroker:=self as IStockBroker;
  result:=TStockBrokerMessage.Create(aMessage,aColor);
  FMessages.Add(Result);

  for i:=0 to EventHandlerCount-1 do
    EventHandlers[i].OnNewMessage(aStockBroker,aOrder,result);
end;

constructor TStockBrokerBase.Create;
begin
  FMessages:=TInterfaceList.Create;
end;

destructor TStockBrokerBase.Destroy;
begin
  Finalize(FEventHandlers);
  FreeAndNil(FMessages);
  inherited;
end;

function TStockBrokerBase.EventHandlerCount: integer;
begin
  result:=Length(FEventHandlers);
end;

function TStockBrokerBase.GetEventHandler(index: integer): IStockBrokerEventHandler;
begin
  result:= FEventHandlers[index];
end;


function TStockBrokerBase.GetMessage(index: integer): IStockBrokerMessage;
begin
  result:=FMessages[index] as IStockBrokerMessage;
end;

function TStockBrokerBase.MessageCount: integer;
begin
  result:=FMessages.Count;
end;


function TStockBrokerBase.RoundPrice(const aSymbol: string; const aPrice: TSCRealNumber): TSCRealNumber;
begin
  result:=RoundTo(aPrice,-GetPricePrecision(aSymbol))
end;

function TStockBrokerBase.PointToPrice(const aSymbol: string; const aPoint: integer): TSCRealNumber;
begin
   result:=aPoint/GetPricesInPoint(aSymbol);
end;

function TStockBrokerBase.PriceToMoney(const aSymbol: string; const aPrice: TSCRealNumber; const aLots: TStockOrderLots): TStockRealNumber;
begin
  //Не учитывает стоимость пункта в $. Всегда считает его 1:1
  result:=PriceToPoint(aSymbol,aPrice)*aLots*10;
end;

function TStockBrokerBase.PriceToPoint(const aSymbol: string; const aPrice: TSCRealNumber): integer;
begin
  result:=Round(aPrice*GetPricesInPoint(aSymbol));
end;

{ TStockBrokerMessage }

function TStockBrokerMessage.Color: TColor;
begin
  result:=FColor;
end;

constructor TStockBrokerMessage.Create(const aText: string; aColor: TColor);
begin
  inherited Create;

  FText:=aText;
  FColor:=aColor;
end;

function TStockBrokerMessage.Text: string;
begin
  result:=FText;
end;


end.
