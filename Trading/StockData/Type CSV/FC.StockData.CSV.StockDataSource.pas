{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSource для файлов формата CSV

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.CSV.StockDataSource;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile;

type
  TStockDataSource_CSV = class(TStockDataSource_StreamToMemory)
  public
    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream; aDate,aTime,aOpen,aHigh,aLow,aClose,aVolume: integer;const aFilter_StartFrom: TDateTime=0; const aFilter_GoTo: TDateTime=0);
  end;

implementation
  uses DateUtils,BaseUtils;


constructor TStockDataSource_CSV.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream; aDate,aTime, aOpen, aHigh, aLow, aClose, aVolume: integer;const aFilter_StartFrom: TDateTime=0; const aFilter_GoTo: TDateTime=0);

var
  aValues: array [0..6] of string;
  aFS          : TFormatSettings;

procedure CreateRecord;
var
  aDateTime   : TDateTime;
  aDataOpen   : TStockRealNumber;
  aDataHigh   : TStockRealNumber;
  aDataLow    : TStockRealNumber;
  aDataClose  : TStockRealNumber;
  aDataVolume : integer;
begin
  try
    try
      aDateTime  :=StrToDate(aValues[aDate],aFS)+StrToTime(aValues[aTime],aFS);
    except
      if aFS.ShortDateFormat='yyyy.mm.dd' then
        aFS.ShortDateFormat:='dd.mm.yyyy'
      else
        aFS.ShortDateFormat:='yyyy.mm.dd';
      aDateTime  :=StrToDate(aValues[aDate],aFS)+StrToTime(aValues[aTime],aFS);
    end;

    //Значение меньше стартового порога
    if (aFilter_StartFrom>0) and (CompareDateTime(aDateTime,aFilter_StartFrom)<0) then
      exit;

    //Значение больше конечного порога
    if (aFilter_GoTo>0) and (CompareDateTime(aDateTime,aFilter_GoTo)>0) then
      exit;

    aDataOpen  :=StrToMoney(aValues[aOpen]);
    aDataHigh  :=StrToMoney(aValues[aHigh]);
    aDataLow   :=StrToMoney(aValues[aLow]);
    aDataClose :=StrToMoney(aValues[aClose]);
    aDataVolume:=StrToInt(aValues[aVolume]);
  except
    on E:EConvertError do
      raise Exception.CreateFmt('Cannot understand expression "%s": %s',
         [aValues[0]+aValues[1]+aValues[2]+aValues[3]+
          aValues[4]+aValues[5],e.Message]);
  end;

  FRecordList.Add(TStockDataRecord.Create(
                    aDateTime,
                    aDataOpen,
                    aDataHigh,
                    aDataLow,
                    aDataClose,
                    aDataVolume));
end;

var
  aString: string;
  aTemp  : AnsiString;
  i,j    : integer;
  b,b2   : boolean;
begin
  inherited Create(aConnection, aSymbol,aInterval) ;

  aFS.DateSeparator:='.';
  aFS.ShortTimeFormat:='hh:mm';
  aFS.TimeSeparator:=':';
  aFS.ShortDateFormat:='yyyy.mm.dd';

  b:=true;
  //Алгоритм: вычитываем порционном из файла текст и складываем его в
  //aString. Тут же его разбираем на части и складываем в массив aValues
  //как только массив заполняется - формирует запись, очищаем массив и продолжаем далше
  aString:=''; i:=0;
  SetLength(aTemp,255);
  while b do
  begin
    j:=aStream.Read(PAnsiString(aTemp)^,255);
    b:=j=255;
    SetLength(aTemp,j);
    aString:=aString+aTemp;
    b2:=true;

    while b2 do
    begin
      while i<Length(aValues) do
      begin
        if not SplitString(aString,aValues[i],aString,[';',',',#13]) then
        begin
          aString:=aValues[i];
          b2:=false;
          break;
        end;
        aValues[i]:=Trim(aValues[i]);
        inc(i);
      end;

      //массив полный, пора создать запись
      if (i=Length(aValues)) then
      begin
        CreateRecord;
        i:=0;
      end;
    end;
  end;

  if (aString<>'') and (i=High(aValues)) then
  begin
    aValues[i]:=aString;
    CreateRecord;
  end;

end;

end.
