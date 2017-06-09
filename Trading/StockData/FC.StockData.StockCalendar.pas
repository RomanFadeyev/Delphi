{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:    оллекци€ событий (календарь)

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.StockCalendar;
{$I Compiler.inc}

interface
  uses Classes, Windows, SysUtils, Controls, Contnrs,Collections.Map,
       StockChart.Definitions,
       FC.Definitions;

type
  TStockCalendarItem = class;

  TStockCalendar = class(TInterfacedObject,IStockCalendarWriteable, IStockCalendar)
  private
    FItems: TList;
    FEventHandlers: TInterfaceList;
    FLastFoundDate: TDateTime;
    FLastFoundIndex: integer;
  protected
    procedure Delete(index: integer);
    procedure RaiseChangeEvent;
    function  GetItemImpl(index: integer):TStockCalendarItem; inline;
    procedure DeleteInternal(index: integer);
    procedure ClearInternal;
  public
    //from IStockCalendar
    function Count: integer; inline;
    function GetItem(index: integer):ISCCalendarItem;

    //найти первый элемент в коллекции, который случилс€ на указанную дату
    //или первый случившийс€ позже, если точно на указанную дату ничего нет
    function FindFirstItemGE(const aDateTime: TDateTime): integer;

    //end of IStockCalendar

    //IStockCalendarWriteable
    procedure Add(const aID: integer;
                  const aDateTime: TDateTime;
                  const aCountry: string;
                  const aClass: integer;
                  const aIndicator: string;
                  const aPriority: string;
                  const aPeriod: string;
                  const aPreviousValue: string;
                  const aForecastValue: string;
                  const aFactValue: string;
                  const aPFChangeType: TSCCalendarChangeType;
                  const aFFChangeType: TSCCalendarChangeType;
                  const aSource: string;
                  const aNotes: string
                    );
    procedure AddCopy(const aItem: IStockCalendarItem);                    

    procedure Clear;

    procedure AddEventHandler(const aHandler:ISCCalendarEventHandler);
    procedure RemoveEventHandler(const aHandler:ISCCalendarEventHandler);
    //end of IStockCalendarWriteable

    constructor Create; overload;
    constructor Create(aQuery: IStockDataCalendarQuery); overload;

    procedure Load(aQuery: IStockDataCalendarQuery);

    destructor Destroy; override;
  end;

  TStockCalendarItem=class (TInterfacedObject,ISCCalendarItem)
  public
    DateTime: TDateTime;
    Country: string;
    Class_: integer;
    Indicator: string;
    Priority: string;
    Period: string;
    PreviousValue: string;
    ForecastValue: string;
    FactValue: string;
    PFChangeType: TSCCalendarChangeType;
    FFChangeType: TSCCalendarChangeType;
    Source: string;
    Notes: string;
    ID: integer;

    //from ISCCalendarItem
    function GetID: integer;

    function GetKey: string;

    function GetDateTime: TDateTime;
    function GetCountry: string;
    function GetClass: integer;
    function GetIndicator: string;
    function GetPriority: string;
    function GetPeriod: string;
    function GetPreviousValue: string;
    function GetForecastValue: string;
    function GetFactValue: string;

    //в какую сторону сменилс€ показатель, если сравнивать Previous и Fact
    function GetPFChangeType: TSCCalendarChangeType;
    //в какую сторону сменилс€ показатель, если сравнивать Forecast и Fact
    function GetFFChangeType: TSCCalendarChangeType;

    //ќткуда было вз€то
    function GetSource: string;

    function GetNotes: string;
  end;


implementation
  uses SystemService,DateUtils, Application.Definitions;

function TStockCalendarItem.GetClass: integer;
begin
  result:=Class_;
end;

function TStockCalendarItem.GetCountry: string;
begin
  result:=Country;
end;

function TStockCalendarItem.GetDateTime: TDateTime;
begin
  result:=DateTime;
end;

function TStockCalendarItem.GetFactValue: string;
begin
  result:=FactValue;
end;

function TStockCalendarItem.GetForecastValue: string;
begin
  result:=ForecastValue;
end;

function TStockCalendarItem.GetID: integer;
begin
  result:=ID;
end;

function TStockCalendarItem.GetIndicator: string;
begin
  result:=Indicator;
end;

function TStockCalendarItem.GetKey: string;
begin
  result:=DateTimeToStr(GetDateTime)+'|'+GetCountry+'|'+GetIndicator+'|'+GetPeriod;
end;

function TStockCalendarItem.GetNotes: string;
begin
  result:=Notes;
end;

function TStockCalendarItem.GetPeriod: string;
begin
  result:=Period;
end;

function TStockCalendarItem.GetPreviousValue: string;
begin
  result:=PreviousValue;
end;

function TStockCalendarItem.GetPriority: string;
begin
  result:=Priority;
end;

function TStockCalendarItem.GetSource: string;
begin
  result:=Source;
end;

function TStockCalendarItem.GetPFChangeType: TSCCalendarChangeType;
begin
  result:=PFChangeType;
end;

function TStockCalendarItem.GetFFChangeType: TSCCalendarChangeType;
begin
  result:=FFChangeType;
end;

{ TStockCalendar }

constructor TStockCalendar.Create;
begin
  FItems:=TList.Create;
  FEventHandlers:=TInterfaceList.Create;
end;

procedure TStockCalendar.Delete(index: integer);
begin
  RaiseChangeEvent;
end;

procedure TStockCalendar.DeleteInternal(index: integer);
begin
  IInterface(GetItemImpl(index))._Release;
  FItems.Delete(index);
end;

destructor TStockCalendar.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FEventHandlers);
  inherited;
end;

function TStockCalendar.FindFirstItemGE(const aDateTime: TDateTime): integer;
var
  i,b,e: integer;
begin
  if (FLastFoundDate>0) and (FLastFoundIndex>=0) then
  begin
    i:=CompareDateTime(aDateTime,FLastFoundDate);
    if i=0 then
    begin
      result:=FLastFoundIndex;
      exit;
    end
    else if i>0 then
    begin
      b:=FLastFoundIndex;
      e:=Count;
    end
    else begin
      b:=0;
      e:=FLastFoundIndex+1;
    end;
  end
  else begin
    b:=0;
    e:=Count;
  end;

  //»щем методом половинного делени€
  i:=-1;
  while (e-b>1) do
  begin
    i:=(b+e) div 2;
    if CompareDateTime(GetItemImpl(i).DateTime,aDateTime)>0 then
      b:=i
    else if CompareDateTime(GetItemImpl(i).DateTime,aDateTime)<0 then
      e:=i
    else
      break;
  end;

  //≈сли поиск зашел слишков влево, двигаем вправо
  while (i>=0) and (CompareDateTime(GetItemImpl(i).DateTime,aDateTime)<0) do
  begin
    inc(i);
    //Ѕольше двигать некуда
    if (i>=Count)  then
    begin
      i:=-1;
      break;
    end;
  end;

  //≈сли поиск зашел слишком вправо, двигаем влево
  while (i>=1) and (CompareDateTime(GetItemImpl(i-1).DateTime,aDateTime)>=0) do
    dec(i);

  result:=i;

  if result>=0 then //«апоминаем значение только в случае удачного поиска
  begin
    FLastFoundDate:=aDateTime;
    FLastFoundIndex:=result;
  end;
end;


function TStockCalendar.GetItem(index: integer): ISCCalendarItem;
begin
  result:=TStockCalendarItem(FItems[index]);
end;

function TStockCalendar.GetItemImpl(index: integer): TStockCalendarItem;
begin
  result:=TStockCalendarItem(FItems[index]);
end;

procedure TStockCalendar.Add(
                  const aID: integer;
                  const aDateTime: TDateTime;
                  const aCountry: string;
                  const aClass: integer;
                  const aIndicator: string;
                  const aPriority: string;
                  const aPeriod: string;
                  const aPreviousValue: string;
                  const aForecastValue: string;
                  const aFactValue: string;
                  const aPFChangeType: TSCCalendarChangeType;
                  const aFFChangeType: TSCCalendarChangeType;
                  const aSource: string;
                  const aNotes: string);
var
  aData:TStockCalendarItem;
begin
  aData:=TStockCalendarItem.Create;
  IInterface(aData)._AddRef;

  aData.ID:=aID;
  aData.DateTime:=aDateTime;
  aData.Country:=aCountry;
  aData.Class_:=aClass;
  aData.Indicator:=aIndicator;
  aData.Priority:=aPriority;
  aData.Period:=aPeriod;
  aData.PreviousValue:=aPreviousValue;
  aData.ForecastValue:=aForecastValue;
  aData.FactValue:=aFactValue;
  aData.PFChangeType:=aPFChangeType;
  aData.FFChangeType:=aFFChangeType;
  aData.Source:=aSource;
  aData.Notes:=aNotes;

  FItems.Add(aData);

  FLastFoundDate:=-1;
  FLastFoundIndex:=-1;
end;

procedure TStockCalendar.AddCopy(const aItem: IStockCalendarItem);
begin
  Add(aItem.GetID,aItem.GetDateTime,aItem.GetCountry,aItem.GetClass,aItem.GetIndicator,
      aItem.GetPriority,aItem.GetPeriod,aItem.GetPreviousValue,aItem.GetForecastValue,aItem.GetFactValue,aItem.GetPFChangeType,
      aItem.GetFFChangeType,aItem.GetSource, aItem.GetNotes);
end;

procedure TStockCalendar.AddEventHandler(
  const aHandler: ISCCalendarEventHandler);
begin
  FEventHandlers.Add(aHandler)
end;

procedure TStockCalendar.Clear;
begin
  ClearInternal;

  FLastFoundDate:=-1;
  FLastFoundIndex:=-1;

  RaiseChangeEvent;
end;

procedure TStockCalendar.ClearInternal;
begin
  while Count>0 do
    DeleteInternal(Count-1);
end;

function TStockCalendar.Count: integer;
begin
  result:=FItems.Count;
end;

constructor TStockCalendar.Create(aQuery: IStockDataCalendarQuery);
begin
  Create;
  Load(aQuery);
end;

procedure TStockCalendar.Load(aQuery: IStockDataCalendarQuery);
begin
  ClearInternal;
  while not aQuery.Eof do
  begin
    Add(aQuery.GetID,
        aQuery.GetDateTime,
        aQuery.GetCountry,
        aQuery.GetClass,
        aQuery.GetIndicator,
        aQuery.GetPriority,
        aQuery.GetPeriod,
        aQuery.GetPreviousValue,
        aQuery.GetForecastValue,
        aQuery.GetFactValue,
        aQuery.GetPFChangeType,
        aQuery.GetFFChangeType,
        aQuery.GetSource,
        aQuery.GetNotes);
    aQuery.Next;
  end;

  RaiseChangeEvent;
end;

procedure TStockCalendar.RaiseChangeEvent;
var
  i: integer;
begin
  for I := 0 to FEventHandlers.Count-1 do
    (FEventHandlers[i] as ISCCalendarEventHandler).OnChange;
end;

procedure TStockCalendar.RemoveEventHandler(
  const aHandler: ISCCalendarEventHandler);
begin
  FEventHandlers.Remove(aHandler);
end;

end.
