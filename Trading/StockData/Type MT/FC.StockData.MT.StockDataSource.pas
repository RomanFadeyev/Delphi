{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSource для файлов формата MetaTrader 4

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.MT.StockDataSource;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB,
  StockChart.Definitions,
  FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile,
  MetaTrader.HistoryFileStruct;

type
  TMTHeaderVersion = (mtV3,mtV4, mtV401);
  TMTHeader = packed record
    Copyright : AnsiString;
    Symbol    : AnsiString;
    Period    : integer;
    Digits    : integer;
    TimeSign  : TDateTime;
    LastSync  : TDateTime;
  end;

  TStockDataSource_MT = class(TStockDataSource_StreamToMemory)
  private
    FVersion  : TMTHeaderVersion;
    FHeader   : TMTHeader;

    procedure LoadHeader(aStream: TStream);
    procedure LoadData(aStream: TStream;const aStartFrom, aTo: TDateTime);
  public
    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream; const aStartFrom: TDateTime=0; const aTo: TDateTime=0); overload;
    constructor Create(const aFileName: string;const aStartFrom: TDateTime=0; const aTo: TDateTime=0); overload;

    property Version:TMTHeaderVersion read FVersion;
    property Header  : TMTHeader read FHeader;
  end;

implementation
  uses DateUtils,Math,FC.DataUtils;

constructor TStockDataSource_MT.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream; const aStartFrom: TDateTime=0; const aTo: TDateTime=0);
var
  aSymbol_ : string;
begin
  aSymbol_:=aSymbol;

  LoadHeader(aStream);

  if aSymbol_='' then //если при создании не указали, что за пара, берем из заголовка
    aSymbol_:=FHeader.Symbol;

  inherited Create(aConnection, aSymbol_,aInterval);

  LoadData(aStream,aStartFrom, aTo);
end;

constructor TStockDataSource_MT.Create(const aFileName: string; const aStartFrom, aTo: TDateTime);
var
  aStream: TFileStream;
  aInterval : TStockTimeInterval;
begin
  aStream:=TFileStreamEx.CreateForRead(aFileName);
  try
    LoadHeader(aStream);
    if not TStockDataUtils.GetTimeIntervalByValue(FHeader.Period,aInterval) then
      raise EStockError.Create('Unknown period');

    inherited Create(nil,FHeader.Symbol,aInterval);
    LoadData(aStream,aStartFrom, aTo);
  finally
    aStream.Free;
  end;
end;

procedure TStockDataSource_MT.LoadData(aStream: TStream;const aStartFrom, aTo: TDateTime);
var
  aRecord     : TStockDataRecord;
  i           : integer;
  aDateTime   : TDateTime;
  aDataOpen   : TStockRealNumber;
  aDataHigh   : TStockRealNumber;
  aDataLow    : TStockRealNumber;
  aDataClose  : TStockRealNumber;
  aDataVolume : integer;
  aDataVolume4 : TStockRealNumber;
  aMaxDate    : TDateTime;
  aNeedSort   : boolean;
  aDigits     : integer;
  aRecordSize : integer;
  aReader     : TReader;
  aCount,j    : integer;
  aInt64: int64;
begin
  //Дальше заголовок
  aDigits:=FHeader.Digits;

  aNeedSort:=false;
  aMaxDate:=-1;

  if FVersion=mtV401 then
    aRecordSize:=60
  else if FVersion=mtV4 then
    aRecordSize:=sizeof(time_t)+sizeof(TStockRealNumber)*5
  else
    aRecordSize:=SizeOf(time_t)+sizeof(TStockRealNumber)*4+sizeof(integer);

  aCount:=((aStream.Size-aStream.Position) div aRecordSize);
  FRecordList.Capacity:=aCount+1;

  aReader:=TReader.Create(aStream,100*1024);

  try
    //данные
    for j := 0 to aCount-1 do
    begin
      if FVersion=mtV401 then
      begin
{
.hst file format valid as of MT4 574 and later
the bars array (single-byte justification) . . . total 60 bytes

datetime	ctm;	// bar start time	8 bytes
double	open;	// open price	8 bytes
double	high;	// highest price	8 bytes
double	low;	// lowest price	8 bytes
double	close;	// close price	8 bytes
long	volume;	// tick count	8 bytes
int	spread;	// spread	4 bytes
long	real_volume;	// real volume	8 bytes
}
        //Time
        aReader.Read(aInt64,sizeof(aInt64));
        aDateTime:=UnixToDateTime(aInt64);

        //Open
        aReader.Read(aDataOpen,sizeof(aDataOpen));

        //High
        aReader.Read(aDataHigh,sizeof(aDataHigh));

        //Low
        aReader.Read(aDataLow,sizeof(aDataLow));

        //Close
        aReader.Read(aDataClose,sizeof(aDataClose));

        //volume
        aReader.Read(aInt64,sizeof(aInt64));
        aDataVolume:=aInt64;

        //spread
        aReader.Read(i,sizeof(i));

        //real_volume
        aReader.Read(aInt64,sizeof(aInt64));
      end
      else begin
        aReader.Read(i,sizeof(i));
        aDateTime:=UnixToDateTime(i);

        //Open
        aReader.Read(aDataOpen,sizeof(aDataOpen));

        //Low
        aReader.Read(aDataLow,sizeof(aDataLow));

        //High
        aReader.Read(aDataHigh,sizeof(aDataHigh));

        //Close
        aReader.Read(aDataClose,sizeof(aDataClose));

        //Volume
        if FVersion=mtV4 then
        begin
          aReader.Read(aDataVolume4,sizeof(aDataVolume4));
          aDataVolume:=Trunc(aDataVolume4);
        end
        else begin
           aReader.Read(aDataVolume,sizeof(aDataVolume));
        end;
      end;

      if (aStartFrom>0) and (CompareDateTime(aDateTime,aStartFrom)<0) then
        continue;

      if (aTo>0) and (CompareDateTime(aDateTime,aTo)>0) then
        continue;


      if aDateTime<aMaxDate then
        aNeedSort:=true
      else
        aMaxDate:=aDateTime;

      aDataOpen:=RoundTo(aDataOpen,-aDigits);
      aDataHigh:=RoundTo(aDataHigh,-aDigits);
      aDataLow:=RoundTo(aDataLow,-aDigits);
      aDataClose:=RoundTo(aDataClose,-aDigits);

      Assert(aDataOpen>=aDataLow);
      Assert(aDataClose>=aDataLow);
      Assert(aDataOpen<=aDataHigh);
      Assert(aDataClose<=aDataHigh);


      aRecord:=TStockDataRecord.Create(aDateTime,aDataOpen, aDataHigh, aDataLow, aDataClose, aDataVolume);
      FRecordList.Add(aRecord);
    end;
  finally
    aReader.Free;
  end;

  if aNeedSort then
    FRecordList.Sort(OnSortRecords);
end;

procedure TStockDataSource_MT.LoadHeader(aStream: TStream);
var
  aVersion    : integer;
  aMT4Header  : TMT4Header;
  aMT3Header  : TMT3Header;
begin
  //4 байта - версия
  aStream.ReadBuffer(aVersion,sizeof(aVersion));

  //Дальше заголовок
  if aVersion=401 then
  begin
    FVersion:=mtV401;
    aStream.ReadBuffer(aMT4Header,sizeof(aMT4Header));
    FHeader.Copyright:=aMT4Header.copyright;
    FHeader.Symbol:=aMT4Header.symbol;
    FHeader.Period:=aMT4Header.period;
    FHeader.Digits:=aMT4Header.digits;
    FHeader.TimeSign:=UnixToDateTime(aMT4Header.timesign);
    FHeader.LastSync:=UnixToDateTime(aMT4Header.last_sync);
  end
  else if aVersion=400 then
  begin
    FVersion:=mtV4;
    aStream.ReadBuffer(aMT4Header,sizeof(aMT4Header));
    FHeader.Copyright:=aMT4Header.copyright;
    FHeader.Symbol:=aMT4Header.symbol;
    FHeader.Period:=aMT4Header.period;
    FHeader.Digits:=aMT4Header.digits;
    FHeader.TimeSign:=UnixToDateTime(aMT4Header.timesign);
    FHeader.LastSync:=UnixToDateTime(aMT4Header.last_sync);
  end
  else if aVersion=100 then
  begin
    FVersion:=mtV3;
    aStream.ReadBuffer(aMT3Header,sizeof(aMT3Header));
    FHeader.Copyright:=aMT3Header.copyright;
    FHeader.Symbol:=aMT3Header.copyright;
    FHeader.Period:=aMT3Header.period;
    FHeader.Digits:=4; //??
    FHeader.TimeSign:=UnixToDateTime(aMT3Header.timesign);
    FHeader.LastSync:=UnixToDateTime(aMT3Header.last_sync);
  end
  else
    raise EStreamError.Create('Wrong version');
end;

end.
