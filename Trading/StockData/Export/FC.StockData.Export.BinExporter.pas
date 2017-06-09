{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Специальный класс для экспорта котировок. Отличается от MT4-формата
            тем, что хранит информацию более компактно
 History:
-----------------------------------------------------------------------------}
unit FC.StockData.Export.BinExporter;

{$I Compiler.inc}

interface
  uses Windows,Forms,BaseUtils,SysUtils, Classes, Controls, Serialization,
  StockChart.Definitions,FC.Definitions, StockChart.Obj,
  FC.StockData.Bin.StockDataSource;

type
  TStockDataFileExporterBin = class(TStockInterfacedObjectVirtual,IStockDataFileExporter)
  private
    procedure Run1(const aDS: IStockDataSource; aStream: TStream);
    procedure Run2(const aDS: IStockDataSource; aStream: TStream);
  public
    function  Filter: string;
    function  DefaultExt: string;
    function  Description: string;

    procedure Run(const aDS: IStockDataSource; aStream: TStream);
  end;



const
  CurrentVersion: TStockDataSource_BinVersion=dsbv2;

implementation
  uses Math, DateUtils, SystemService,
       Application.Definitions,
       FC.Factory,
       FC.DataUtils,
       FC.StockData.DataSourceToInputDataCollectionMediator;

{ TStockDataFileExporterBin }

function TStockDataFileExporterBin.DefaultExt: string;
begin
  result:='.sbf';
end;

function TStockDataFileExporterBin.Description: string;
begin
  result:='Binary files (*.sbf)';
end;

function TStockDataFileExporterBin.Filter: string;
begin
  result:='*.sbf';
end;

procedure TStockDataFileExporterBin.Run(const aDS: IStockDataSource; aStream: TStream);
begin
  TWaitCursor.SetUntilIdle;

  Assert(sizeof(TBinHeader)=sizeof(TBinHeader2));
  case CurrentVersion of
   dsbv1:Run1(aDS,aStream);
   dsbv2:Run2(aDS,aStream);
   else
     raise EAlgoError.Create;
  end;
end;

procedure TStockDataFileExporterBin.Run1(const aDS: IStockDataSource; aStream: TStream);
var
  i: integer;
  s: string;
  aDate: integer;
  aV: TStockRealNumber;
  aVolume: integer;
  aMediator: TStockDataSourceToInputDataCollectionMediator;
  aMin,aMax: TStockRealNumber;
  aIndex16: word;
  aIndex32: cardinal;
  aSign   : word;

  aHeader: TBinHeader;
  aWriter: TWriter;
begin
  //Считаем Min/Max
  aMediator:=TStockDataSourceToInputDataCollectionMediator.Create(aDS,nil);
  aMediator.FindMinMaxValues(0,aDS.RecordCount-1,aMin,aMax);
  aMediator.Free;

  //Дальше заголовок
  ZeroMemory(@aHeader,sizeof(aHeader));
  aHeader.version:=1;
  s:='(C) '+Workspace.CompanyName;
  StrLCopy(@aHeader.copyright[0],pchar(s),sizeof(aHeader.copyright));
  StrLCopy(@aHeader.symbol[0],pchar(aDS.StockSymbol.Name),sizeof(aHeader.symbol));
  aHeader.digits:=aDS.GetPricePrecision;
  aHeader.minval:=aMin;
  aHeader.maxval:=aMax;
  if aDS.RecordCount>0 then
  begin
    aHeader.firsttime:=DateTimeToUnix(aDS.GetDataDateTime(0));
    aHeader.lasttime:=DateTimeToUnix(aDS.GetDataDateTime(aDS.RecordCount-1));
  end;

  aStream.WriteBuffer(aHeader,sizeof(aHeader));


  if aDS.PriceToPoint(aMax-aMin)>$FFFF then
    raise EStockError.Create('Too large prices diapason to store');

  aSign:=$FFFF;

  //данные
  aWriter:=TWriter.Create(aStream,1024*1024); //использование врайтера дает оптимизацию за счет использования буфера
  try
    for I := 0 to aDS.RecordCount - 1 do
    begin
      aDate:=DateTimeToUnix(aDS.GetDataDateTime(i));
      aWriter.Write(aDate,sizeof(aDate));

      //Open
      aV:=aDS.GetDataOpen(i);
      aIndex32:=aDS.PriceToPoint(aV-aMin);
      if aIndex32<$FFFF then
      begin
        aIndex16:=aIndex32;
        aWriter.Write(aIndex16,sizeof(aIndex16));
      end
      else begin
        aWriter.Write(aSign,2);
        aWriter.Write(aIndex32,sizeof(aIndex32));
      end;


      //High
      aV:=aDS.GetDataHigh(i);
      aIndex32:=aDS.PriceToPoint(aV-aMin);
      if aIndex32<$FFFF then
      begin
        aIndex16:=aIndex32;
        aWriter.Write(aIndex16,sizeof(aIndex16));
      end
      else begin
        aWriter.Write(aSign,2);
        aWriter.Write(aIndex32,sizeof(aIndex32));
      end;

      //Low
      aV:=aDS.GetDataLow(i);
      aIndex32:=aDS.PriceToPoint(aV-aMin);
      if aIndex32<$FFFF then
      begin
        aIndex16:=aIndex32;
        aWriter.Write(aIndex16,sizeof(aIndex16));
      end
      else begin
        aWriter.Write(aSign,2);
        aWriter.Write(aIndex32,sizeof(aIndex32));
      end;

      //Close
      aV:=aDS.GetDataClose(i);
      aIndex32:=aDS.PriceToPoint(aV-aMin);
      if aIndex32<$FFFF then
      begin
        aIndex16:=aIndex32;
        aWriter.Write(aIndex16,sizeof(aIndex16));
      end
      else begin
        aWriter.Write(aSign,2);
        aWriter.Write(aIndex32,sizeof(aIndex32));
      end;

      //Volume
      aVolume:=aDS.GetDataVolume(i);
      aWriter.Write(aVolume,sizeof(aVolume));
    end;

    aWriter.FlushBuffer;
  finally
    aWriter.Free;
  end;
end;

function GetValueType(const Value: LongInt): ShortInt; inline;
begin
  if (Value >= Low(ShortInt)) and (Value <= High(ShortInt)) then
    result:=1
  else if (Value >= Low(SmallInt)) and (Value <= High(SmallInt)) then
    result:=2
  else
    result:=3;
end;

procedure TStockDataFileExporterBin.Run2(const aDS: IStockDataSource; aStream: TStream);
var
  i: integer;
  s: string;
  aCurDate,aNewDate,aDDate: integer;

  aCurVolume,aNewVolume,aDVolume: integer;
  aCurOpen,aCurHigh,aCurLow,aCurClose: Double;
  aNewOpen,aNewHigh,aNewLow,aNewClose: Double;
  aDOpen,aDHigh,aDLow,aDClose: LongInt;

  aDateSize,aOCSize,aHLSize, aVolSize,aAllSize: byte;

  aHeader: TBinHeader2;
  aWriter: TWriter;

const
  aBytes: array [0..3] of integer = (0,1,2,4);
begin
  //Дальше заголовок
  ZeroMemory(@aHeader,sizeof(aHeader));
  aHeader.version:=2;
  s:='(C) '+Workspace.CompanyName;
  StrLCopy(@aHeader.copyright[0],pchar(s),sizeof(aHeader.copyright));
  StrLCopy(@aHeader.symbol[0],pchar(aDS.StockSymbol.Name),sizeof(aHeader.symbol));
  aHeader.digits:=aDS.GetPricePrecision;
  if aDS.RecordCount>0 then
  begin
    aCurDate:=DateTimeToUnix(aDS.GetDataDateTime(0));
    aCurOpen:=aDS.GetDataOpen(0);
    aCurHigh:=aDS.GetDataHigh(0);
    aCurLow:=aDS.GetDataLow(0);
    aCurClose:=aDS.GetDataClose(0);
    aCurVolume:=aDS.GetDataVolume(0);

    aHeader.firsttime:=aCurDate;
    aHeader.lasttime:=DateTimeToUnix(aDS.GetDataDateTime(aDS.RecordCount-1));

    aHeader.firstopen:=aCurOpen;
    aHeader.firsthigh:=aCurHigh;
    aHeader.firstlow:=aCurLow;
    aHeader.firstclose:=aCurClose;
    aHeader.firstvolume:=aCurVolume;
  end
  else begin
    //Набор пустой, записываем заголовок и уходим
    aStream.WriteBuffer(aHeader,sizeof(aHeader));
    exit;
  end;

  aStream.WriteBuffer(aHeader,sizeof(aHeader));

  //Спец. наворот, чтобы сразу загрузить все
  aDS.FetchAll;

  //данные
  aWriter:=TWriter.Create(aStream,1024*1024); //использование врайтера дает оптимизацию за счет использования буфера
  try
    for I := 0 to aDS.RecordCount - 1 do
    begin
      //Смысл алгоритма: записываем разницу между предыдущим и текущим
      //значением и храним ее в пунктах. Как правило, дельта полчается маленькая
      //и помещается в 1 байт

      aNewDate:=DateTimeToUnix(aDS.GetDataDateTime(i));
      aNewOpen:=aDS.GetDataOpen(i);
      aNewHigh:=aDS.GetDataHigh(i);
      aNewLow:=aDS.GetDataLow(i);
      aNewClose:=aDS.GetDataClose(i);
      aNewVolume:=aDS.GetDataVolume(i);

      aDDate:=aNewDate-aCurDate;
      aDOpen:=aDS.PriceToPoint(aNewOpen-aCurOpen);
      aDHigh:=aDS.PriceToPoint(aNewHigh-aCurHigh);
      aDLow :=aDS.PriceToPoint(aNewLow-aCurLow);
      aDClose:=aDS.PriceToPoint(aNewClose-aCurClose);
      aDVolume:=aNewVolume-aCurVolume;

      //Выбираем максимальный размер для всех значений
      aDateSize:=GetValueType(aDDate);
      //Выбираем максимальный размер для значений Open-Close
      aOCSize:=max(GetValueType(aDOpen),GetValueType(aDClose));
      //Выбираем максимальный размер для значений High-Low
      aHLSize:=max(GetValueType(aDHigh),GetValueType(aDLow));
      //Выбираем максимальный размер для Volume
      aVolSize:=GetValueType(aDVolume);

      Assert(aDateSize in [1..3]);
      Assert(aOCSize in [1..3]);
      Assert(aHLSize in [1..3]);
      Assert(aVolSize in [1..3]);

      //записываем размеры
      aAllSize:=(aDateSize shl 6) or (aOCSize shl 4) or (aHLSize shl 2) or (aVolSize);
      aWriter.Write(aAllSize, sizeof(aAllSize));

      //записываем дату
      aWriter.Write(aDDate, aBytes[aDateSize]);
      //Записываем данные
      aWriter.Write(aDOpen, aBytes[aOCSize]);
      aWriter.Write(aDHigh, aBytes[aHLSize]);
      aWriter.Write(aDLow,  aBytes[aHLSize]);
      aWriter.Write(aDClose,aBytes[aOCSize]);
      //Записываем volume
      aWriter.Write(aDVolume,aBytes[aVolSize]);

      aCurDate:=aNewDate;
      aCurOpen:=aNewOpen;
      aCurHigh:=aNewHigh;
      aCurLow:=aNewLow;
      aCurClose:=aNewClose;
      aCurVolume:=aNewVolume;

    end;
    aWriter.FlushBuffer;
  finally
    aWriter.Free;
  end;
end;

initialization
  Factory.RegisterCreator(TObjectCreator_Coexistent.Create(TStockDataFileExporterBin));

end.
