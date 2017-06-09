{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:    оллекци€ тиков построенна€ по типу очереди на заданный интервала времени
            при добавлении нового элемента старый, который "вышел" за пределы интервала,
            удал€етс€

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.StockTickCollectionAsQueue;
{$I Compiler.inc}

interface

  uses Classes, Windows, SysUtils, Controls, Contnrs,
       StockChart.Definitions,
       FC.Definitions,FC.StockData.StockTickCollection;

type
  TStockTickCollectionAsQueue = class(TStockTickCollection)
  private
    FRangeMins: integer;
    FProcessingCollection : integer;
  protected
    procedure ItemAdded(index: integer); override;
  public
    //“оже самое, только дл€ коллекции
    procedure Merge(aCollection: TStockTickCollection); override;

    property RangeInMinutes: integer read FRangeMins;

    //aRangeMins задает интервал в минутах
    constructor Create(aRangeMins: integer);
  end;

implementation
  uses FC.DataUtils;

{ TStockTickCollectionAsQueue }

procedure TStockTickCollectionAsQueue.ItemAdded(index: integer);
var
  aDateTime: TDateTime;
begin
  inherited;
  if (FProcessingCollection=0) and (Count>0) then
  begin
    aDateTime:=GetDateTime(Count-1);
    while (Count>0) and (TStockDataUtils.MinutesBetween(GetDateTime(0),aDateTime)>FRangeMins) do
      self.Delete(0);
  end;
end;

procedure TStockTickCollectionAsQueue.Merge(aCollection: TStockTickCollection);
begin
  inc(FProcessingCollection);
  try
    inherited;
  finally
    dec(FProcessingCollection);
    if Count>0 then
      ItemAdded(Count-1);
  end;

end;

constructor TStockTickCollectionAsQueue.Create(aRangeMins: integer);
begin
  inherited Create();
  FRangeMins:=aRangeMins;
end;

end.
