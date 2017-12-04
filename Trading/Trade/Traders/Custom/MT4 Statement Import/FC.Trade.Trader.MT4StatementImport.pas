unit FC.Trade.Trader.MT4StatementImport;

interface

uses
  Classes, Math,Graphics, Contnrs, Forms, Controls, SysUtils, BaseUtils, ActnList, Properties.Obj, Properties.Definitions,
  StockChart.Definitions, StockChart.Definitions.Units, Properties.Controls, StockChart.Indicators,
  Serialization, FC.Definitions, FC.Trade.Trader.Base,FC.Trade.Properties,FC.fmUIDataStorage;

type
  //Интерфейс нашего трейдера
  //Пока здесь объявлен. Потом как устоится, вынести в Definitions
  IStockTraderMT4StatementImport = interface
  ['{D80AB38D-90BC-4F46-BDBA-02417B946A95}']
  end;

  TImportedRecord = class
    Time : TDateTime;
    Type_: string;
    OrderNo : integer;
    Size : TSCRealNumber;
    Price: TSCRealNumber;
    SL   : TSCRealNumber;
    TP   : TSCRealNumber;
    Order : IStockOrder;
  end;

  //Собственно трейдер
  TStockTraderMT4StatementImport = class (TStockTraderBase,IStockTraderMT4StatementImport)
  private
    FPropPath: TPropertyString;
    FImportedData : TObjectList;

    procedure LoadFromFile;
    procedure CleanOrders;
  protected
    procedure OnBeginWorkSession; override;
    procedure OnEndWorkSession; override;
  public
    //Создание-удаление своих объектов
    procedure OnCreateObjects; override;
    procedure OnReleaseObjects; override;

    procedure OnPropertyChanged(aNotifier:TProperty); override;

    //Посчитать
    procedure UpdateStep2(const aTime: TDateTime); override;

    constructor Create; override;
    destructor Destroy; override;
    procedure Dispose; override;
  end;

implementation
  uses Variants,Application.Definitions, FC.Trade.OrderCollection, FC.Trade.Trader.Message,
  StockChart.Indicators.Properties.Dialog, FC.Trade.Trader.Factory,
  FC.DataUtils,DateUtils;



{ TStockTraderMT4StatementImport }

procedure TStockTraderMT4StatementImport.CleanOrders;
var
  i: Integer;
begin
  if FImportedData=nil then
    exit;

  for i := 0 to FImportedData.Count - 1 do
    TImportedRecord(FImportedData[i]).Order:=nil;
end;

constructor TStockTraderMT4StatementImport.Create;
begin
  inherited Create;
  RegisterProperties([FPropPath]);
end;

destructor TStockTraderMT4StatementImport.Destroy;
begin
  inherited;
end;

procedure TStockTraderMT4StatementImport.Dispose;
begin
  inherited;
end;

procedure TStockTraderMT4StatementImport.OnBeginWorkSession;
begin
  inherited;
  CleanOrders;
end;

procedure TStockTraderMT4StatementImport.OnCreateObjects;
begin
  inherited;
end;

procedure TStockTraderMT4StatementImport.OnEndWorkSession;
begin
  inherited;
  CleanOrders;
end;

procedure TStockTraderMT4StatementImport.OnPropertyChanged(
  aNotifier: TProperty);
begin
  inherited;
  if aNotifier=FPropPath then
  begin
    FreeAndNil(FImportedData);
    if FPropPath.Value<>'' then
      LoadFromFile;
  end;
end;

procedure TStockTraderMT4StatementImport.OnReleaseObjects;
begin
  inherited;
end;

procedure TStockTraderMT4StatementImport.LoadFromFile;
var
  aText: TStringList;
  i: integer;
  aRecord: TImportedRecord;
  aParts : TStringList;
begin
  if FImportedData<>nil then
    exit;

  aText:=TStringList.Create;
  aParts:=TStringList.Create;
  try
    aText.LoadFromFile(FPropPath.ValueAsText);
    for i := 1 to aText.Count-1 do
    begin
      SplitString(aText[i],aParts,[';']);

      aRecord:=TImportedRecord.Create;
      //aRecord.N:=StrToInt(aParts[0]);
      aRecord.Time:=StrToDateTime(aParts[1]);
      aRecord.Type_:=aParts[2];
      aRecord.OrderNo:=StrToInt(aParts[3]);
      aRecord.Size:=StrToFloat(aParts[4]);
      aRecord.TP:=StrToFloat(aParts[5]);
      aRecord.SL:=StrToFloat(aParts[5]);

      FImportedData.Add(aRecord);
    end;
  finally
    aText.Free;
  end;
end;

procedure TStockTraderMT4StatementImport.UpdateStep2(const aTime: TDateTime);
var
  i: integer;
  aRecord: TImportedRecord;
begin
  LoadFromFile;

  aRecord:=nil;
  for I := 0 to FImportedData.Count-1 do
  begin
    if SameDateTime(TImportedRecord(FImportedData[i]).Time,aTime) then
    begin
      aRecord:=TImportedRecord(FImportedData[i]);
      break;
    end;
  end;

  if aRecord=nil then
    exit;

  if SameText(aRecord.Type_,'buy') then
    aRecord.Order:=OpenOrder(okBuy,1,aRecord.SL,aRecord.TP,0)
  else if SameText(aRecord.Type_,'buy limit') then
    aRecord.Order:=OpenOrderAt(okBuy,aRecord.Price, 1,aRecord.SL,aRecord.TP,0)
  else if SameText(aRecord.Type_,'sell') then
    aRecord.Order:=OpenOrder(okSell,1,aRecord.SL,aRecord.TP,0)
  else if SameText(aRecord.Type_,'sell limit') then
    aRecord.Order:=OpenOrderAt(okSell,aRecord.Price,1,aRecord.SL,aRecord.TP,0)
end;

initialization
  FC.Trade.Trader.Factory.TraderFactory.RegisterTrader('Samples','MT4StatementImport',TStockTraderMT4StatementImport,IStockTraderMT4StatementImport);
end.




