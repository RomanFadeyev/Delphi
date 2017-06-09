{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Реализация IStockDataSource для файлов формата Bin

 History:
-----------------------------------------------------------------------------}

unit FC.StockData.Bin.StockDataSource;
{$I Compiler.inc}

interface
  uses SysUtils,Classes,DB, FC.Definitions,FC.StockData.StockDataSource, FC.StockData.StockDataConnectionFile;

type
  TBinHeader = packed record
    version   : integer;             // version of the base
    copyright : array [0..63] of char; // copyright information
    symbol    : array [0..11] of char; // security
    digits    : integer;            // the amount of digits after point shown for the symbol
    firsttime : integer;
    lasttime  : integer;
    minval    : Double;
    maxval    : Double;
    unused    : array[0..12] of integer;         // for future use
  end;

  TBinHeader2 = packed record
    version   : integer;             // version of the base
    copyright : array [0..63] of char; // copyright information
    symbol    : array [0..11] of char; // security
    digits    : integer;            // the amount of digits after point shown for the symbol
    firsttime : integer;
    lasttime  : integer;
    firstopen : Double;
    firsthigh : Double;
    firstlow  : Double;
    firstclose: Double;
    firstvolume: integer;
    unused    : array[0..7] of integer;         // for future use
  end;
  PBinHeader2 = ^TBinHeader2;

  TStockDataSource_BinVersion = (dsbv1, dsbv2);

  TStockDataSource_Bin = class(TStockDataSource_StreamToMemory)
  private
    procedure Load1(const aHeader:TBinHeader;aStream: TStream);
    procedure Load2(const aHeader:TBinHeader2;aStream: TStream);
  public
    constructor Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream);
  end;


implementation
  uses DateUtils,BaseUtils,Math;


constructor TStockDataSource_Bin.Create(const aConnection: IStockDataSourceConnection; const aSymbol: string; aInterval: TStockTimeInterval; aStream: TStream);
var
  aHeader     : TBinHeader;
begin
  inherited Create(aConnection, aSymbol,aInterval);

  aStream.ReadBuffer(aHeader,sizeof(aHeader));
  if aHeader.version=1 then
    Load1(aHeader,aStream)
  else if aHeader.version=2 then
    Load2(PBinHeader2(@aHeader)^,aStream)
       
  else
    raise EStockError.Create('Unknown version');

end;


procedure TStockDataSource_Bin.Load1(const aHeader:TBinHeader;aStream: TStream);
var
  aRecord     : TStockDataRecord;
  aDateTime   : TDateTime;
  aDataOpen   : TStockRealNumber;
  aDataHigh   : TStockRealNumber;
  aDataLow    : TStockRealNumber;
  aDataClose  : TStockRealNumber;
  aDataVolume : integer;
  aReader     : TReader;

  aK          : integer;
  aTime       : integer;
  aStreamSize : Int64;
  aStreamPos  : Int64;
  aIndex16    : word;
  aIndex32    : cardinal;
begin
  aK:=Trunc(power(10,aHeader.digits));

  aStreamSize:=aStream.Size;
  aStreamPos:=aStream.Position;

  aReader:=TReader.Create(aStream,1024*1024);
  try
    while aStreamPos<aStreamSize do
    begin
      aReader.Read(aTime,sizeof(aTime)); inc(aStreamPos,sizeof(aTime));
      aDateTime:=UnixToDateTime(aTime);

      //Open
      aReader.Read(aIndex16,sizeof(aIndex16)); inc(aStreamPos,sizeof(aIndex16));
      if aIndex16=$FFFF then
      begin
        aReader.Read(aIndex32,sizeof(aIndex32)); inc(aStreamPos,sizeof(aIndex32));
      end
      else
        aIndex32:=aIndex16;
      aDataOpen:=aHeader.minval+aIndex32/aK;

      //High
      aReader.Read(aIndex16,sizeof(aIndex16)); inc(aStreamPos,sizeof(aIndex16));
      if aIndex16=$FFFF then
      begin
        aReader.Read(aIndex32,sizeof(aIndex32)); inc(aStreamPos,sizeof(aIndex32));
      end
      else
        aIndex32:=aIndex16;
      aDataHigh:=aHeader.minval+aIndex32/aK;

      //Low
      aReader.Read(aIndex16,sizeof(aIndex16)); inc(aStreamPos,sizeof(aIndex16));
      if aIndex16=$FFFF then
      begin
        aReader.Read(aIndex32,sizeof(aIndex32)); inc(aStreamPos,sizeof(aIndex32));
      end
      else
        aIndex32:=aIndex16;
      aDataLow:=aHeader.minval+aIndex32/aK;

      //Close
      aReader.Read(aIndex16,sizeof(aIndex16)); inc(aStreamPos,sizeof(aIndex16));
      if aIndex16=$FFFF then
      begin
        aReader.Read(aIndex32,sizeof(aIndex32)); inc(aStreamPos,sizeof(aIndex32));
      end
      else
        aIndex32:=aIndex16;
      aDataClose:=aHeader.minval+aIndex32/aK;

      //Volume
      aReader.Read(aDataVolume,sizeof(aDataVolume)); inc(aStreamPos,sizeof(aDataVolume));

      aRecord:=TStockDataRecord.Create(aDateTime,aDataOpen, aDataHigh, aDataLow, aDataClose, aDataVolume);
      FRecordList.Add(aRecord);
    end;
  finally
    aReader.Free;
  end;
end;

procedure TStockDataSource_Bin.Load2(const aHeader: TBinHeader2;aStream: TStream);
var
  aRecord     : TStockDataRecord;
  aReader     : TReader;

  aCurDate,aNewDate,aDDate: integer;

  aCurVolume,aNewVolume,aDVolume: integer;
  aCurOpen,aCurHigh,aCurLow,aCurClose: double;
  aNewOpen,aNewHigh,aNewLow,aNewClose: double;
  aDOpen,aDHigh,aDLow,aDClose: LongInt;
  aDateTime   : TDateTime;

  aDateSize,aOCSize,aHLSize, aVolSize,aAllSize: byte;

  aStreamSize : Int64;
  aStreamPos  : Int64;
  aK          : integer;

procedure ReadValue(var aValue: LongInt; const aSize: byte);
var
  S: Shortint;
  I: Smallint;
begin
  case aSize of
    1:
      begin
        aReader.Read(S, SizeOf(S));
        inc(aStreamPos,SizeOf(S));
        aValue := S;
      end;
    2:
      begin
        aReader.Read(I, SizeOf(I));
        aValue:=I;
        inc(aStreamPos,SizeOf(I));
      end;
    3: begin
      aReader.Read(aValue, SizeOf(aValue));
      inc(aStreamPos,SizeOf(aValue));
    end
  else
    raise EAlgoError.Create;
  end;
end;

begin
  aK:=Trunc(power(10,aHeader.digits));

  aStreamSize:=aStream.Size;
  aStreamPos:=aStream.Position;

  aCurDate:=aHeader.firsttime;
  aCurOpen:=aHeader.firstopen;
  aCurHigh:=aHeader.firsthigh;
  aCurLow:=aHeader.firstlow;
  aCurClose:=aHeader.firstclose;
  aCurVolume:=aHeader.firstvolume;

  aReader:=TReader.Create(aStream,1024*1024);
  try
    while aStreamPos<aStreamSize do
    begin
      aReader.Read(aAllSize, sizeof(aAllSize)); inc(aStreamPos,sizeof(aAllSize));
      aVolSize:=aAllSize and 3; aAllSize:=aAllSize shr 2;
      aHLSize:=aAllSize and 3; aAllSize:=aAllSize shr 2;
      aOCSize:=aAllSize and 3; aAllSize:=aAllSize shr 2;
      aDateSize:=aAllSize and 3; aAllSize:=aAllSize shr 2;

      Assert(aDateSize in [1..3]);
      Assert(aOCSize in [1..3]);
      Assert(aHLSize in [1..3]);
      Assert(aVolSize in [1..3]);

      aDDate:=0; aDOpen:=0; aDHigh:=0; aDLow:=0; aDClose:=0; aDVolume:=0;

      //считываем дату
      ReadValue(aDDate, aDateSize);
      //считываем данные
      ReadValue(aDOpen, aOCSize);
      ReadValue(aDHigh, aHLSize);
      ReadValue(aDLow,  aHLSize);
      ReadValue(aDClose,aOCSize);  
      //считываем volume
      ReadValue(aDVolume,aVolSize);

      aNewDate:=aCurDate+aDDate;
      aDateTime:=UnixToDateTime(aNewDate);


      aNewOpen:=RoundTo(aCurOpen+aDOpen/aK,-aHeader.digits);
      aNewHigh:=RoundTo(aCurHigh+aDHigh/aK,-aHeader.digits);
      aNewLow:=RoundTo(aCurLow+aDLow/aK,-aHeader.digits);
      aNewClose:=RoundTo(aCurClose+aDClose/aK,-aHeader.digits);

      aNewVolume:=aCurVolume+aDVolume;


      aRecord:=TStockDataRecord.Create(aDateTime,aNewOpen, aNewHigh, aNewLow, aNewClose, aNewVolume);
      FRecordList.Add(aRecord);

      aCurDate:=aNewDate;
      aCurOpen:=aNewOpen;
      aCurHigh:=aNewHigh;
      aCurLow:=aNewLow;
      aCurClose:=aNewClose;
      aCurVolume:=aNewVolume;
    end;
  finally
    aReader.Free;
  end;
end;

end.
