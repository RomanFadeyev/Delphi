{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   »стори€ тиков

 History:
-----------------------------------------------------------------------------}
unit FC.StockData.StockTickCollectionSerializer;
{$I Compiler.inc}

interface
  uses Classes, Windows, SysUtils, Controls, Contnrs,
       StockChart.Definitions,
       FC.Definitions;

type
  TFileHeader = packed record
    version   : integer;             // version of the base
    copyright : array [0..63] of AnsiChar; // copyright information
    symbol    : array [0..11] of AnsiChar; // security
    digits    : integer;            // the amount of digits after point shown for the symbol
    firsttime : integer;
    lasttime  : integer;
    unused    : array[0..12] of integer;         // for future use
  end;

  TFileHeader2= packed record
    version   : integer;             // version of the base
    copyright : array [0..63] of AnsiChar; // copyright information
    symbol    : array [0..11] of AnsiChar; // security
    digits    : integer;            // the amount of digits after point shown for the symbol
    firsttime : integer;
    lasttime  : integer;
    firstvalue: double;
    unused    : array[0..10] of integer;         // for future use
  end;
  PFileHeader2 = ^TFileHeader2;

  TStockTickCollectionVersion = (tcv1,tcv2);

type
  TStockTickCollectionSerializer = class
  private
    procedure Load1(const aData: IStockTickCollectionWriteable; aStream: TStream; const aHeader: TFileHeader);
    procedure Load2(const aData: IStockTickCollectionWriteable; aStream: TStream; const aHeader: TFileHeader2);

    procedure Save1(const aData: IStockTickCollection; aStream: TStream);
    procedure Save2(const aData: IStockTickCollection; aStream: TStream);
  public
    procedure SaveToFile(const aData: IStockTickCollection; const aFileName:string);
    procedure SaveToStream(const aData: IStockTickCollection; aStream: TStream);

    procedure LoadFromStream(const aData: IStockTickCollectionWriteable; aStream: TStream);
    procedure LoadFromFile(const aData: IStockTickCollectionWriteable; const aFileName:string);
  end;


const
  CurrentVersion : TStockTickCollectionVersion = tcv2;

implementation
  uses Math,SystemService,DateUtils, BaseUtils, Application.Definitions;


{ TStockTickCollectionSerializer }

procedure TStockTickCollectionSerializer.Load1(
                    const aData: IStockTickCollectionWriteable;
                    aStream: TStream;
                    const aHeader: TFileHeader);
var
  aDateTime   : TDateTime;
  aValue      : TStockRealNumber;
  aReader     : TReader;
  aTime       : integer;
  aStreamSize : Int64;
  aStreamPos  : Int64;
begin
  aStreamSize:=aStream.Size;
  aStreamPos:=aStream.Position;

  aReader:=TReader.Create(aStream,1024*1024);
  try
    while aStreamPos<aStreamSize do
    begin
      aReader.Read(aTime,sizeof(aTime)); inc(aStreamPos,sizeof(aTime));
      aDateTime:=UnixToDateTime(aTime);
      aReader.Read(aValue,sizeof(aValue)); inc(aStreamPos,sizeof(aValue));

      aData.Add(aDateTime,aValue);
    end;
  finally
    aReader.Free;
  end;
end;

procedure TStockTickCollectionSerializer.Load2(
  const aData: IStockTickCollectionWriteable; aStream: TStream;
  const aHeader: TFileHeader2);
var
  aDateTime   : TDateTime;
  aReader     : TReader;
  aStreamSize : Int64;
  aStreamPos  : Int64;
  aK           : integer;

  aCurValue,aNewValue: double;
  aCurDate,aNewDate: integer;
  aDValue,aDDate: Longint;

  aDateSize,aValueSize,aAllSize: Shortint;

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
  aStreamSize:=aStream.Size;
  aStreamPos:=aStream.Position;
  aK:=Trunc(power(10,aHeader.digits));

  aCurDate:=aHeader.firsttime;
  aCurValue:=aHeader.firstvalue;

  aReader:=TReader.Create(aStream,1024*1024);
  try
    while aStreamPos<aStreamSize do
    begin
      aReader.Read(aAllSize, sizeof(aAllSize)); inc(aStreamPos,sizeof(aAllSize));
      aValueSize:=aAllSize and 3; aAllSize:=aAllSize shr 2;
      aDateSize:=aAllSize and 3;

      Assert(aDateSize in [1..3]);
      Assert(aValueSize in [1..3]);
      aDDate:=0; aDValue:=0;

      //считываем дату
      ReadValue(aDDate, aDateSize);
      //считываем данные
      ReadValue(aDValue, aValueSize);

      aNewDate:=aCurDate+aDDate;
      aDateTime:=UnixToDateTime(aNewDate);
      aNewValue:=RoundTo(aCurValue+aDValue/aK,-aHeader.digits);

      aData.Add(aDateTime,aNewValue);

      aCurValue:=aNewValue;
      aCurDate:=aNewDate;
      if aData.Count=138 then
      begin
        aCurValue:=aCurValue;
      end;
    end;
  finally
    aReader.Free;
  end;
end;

procedure TStockTickCollectionSerializer.LoadFromFile(const aData: IStockTickCollectionWriteable; const aFileName:string);
var
  aStream: TStream;
begin
  aStream:=TFileStream.Create(aFileName,fmOpenRead);
  try
    LoadFromStream(aData,aStream);
  finally
    aStream.Free;
  end;
end;

procedure TStockTickCollectionSerializer.LoadFromStream( const aData: IStockTickCollectionWriteable; aStream: TStream);
var
  aHeader     : TFileHeader;
begin
  Assert(sizeof(TFileHeader)=sizeof(TFileHeader2));
  
  aStream.ReadBuffer(aHeader,sizeof(aHeader));
  if aHeader.version=1 then
  begin
    aData.SetStockSymbol(aHeader.symbol);
    Load1(aData,aStream,aHeader);
  end
  else if aHeader.version=2 then
  begin
    aData.SetStockSymbol(aHeader.symbol);
    Load2(aData,aStream,PFileHeader2(@aHeader)^);
  end
  else
    raise EStockError.Create('Unknown version');
end;

procedure TStockTickCollectionSerializer.SaveToStream(const aData: IStockTickCollection; aStream: TStream);
begin
  TWaitCursor.SetUntilIdle;
  case CurrentVersion of
    tcv1: Save1(aData,aStream);
    tcv2: Save2(aData,aStream);
    else
      raise EAlgoError.Create;
  end;
end;

procedure TStockTickCollectionSerializer.Save1(const aData: IStockTickCollection; aStream: TStream);
var
  s: string;
  i: integer;
  aDate: integer;
  aV: TStockRealNumber;
  aWriter: TWriter;
  aHeader: TFileHeader;
begin
  //ƒальше заголовок
  ZeroMemory(@aHeader,sizeof(aHeader));
  aHeader.version:=1;
  s:='(C) '+Workspace.CompanyName;
  StrLCopy(@aHeader.copyright[0],pchar(s),sizeof(aHeader.copyright));
  StrLCopy(@aHeader.symbol[0],pchar(aData.StockSymbol),sizeof(aHeader.symbol));

  if aData.Count>0 then
  begin
    aHeader.firsttime:=DateTimeToUnix(aData.GetDateTime(0));
    aHeader.lasttime:=DateTimeToUnix(aData.GetDateTime(aData.Count-1));
  end;
  
  aHeader.digits:=4; //TODO

  //данные
  aWriter:=TWriter.Create(aStream,1024*1024); //использование врайтера дает оптимизацию за счет использовани€ буфера
  try
    aWriter.Write(aHeader,sizeof(aHeader));
    for I := 0 to aData.Count - 1 do
    begin
      aDate:=DateTimeToUnix(aData.GetDateTime(i));
      aWriter.Write(aDate,sizeof(aDate));
      //Volume
      aV:=aData.GetValue(i);
      aWriter.Write(aV,sizeof(aV));
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


procedure TStockTickCollectionSerializer.Save2(const aData: IStockTickCollection; aStream: TStream);
var
  s: string;
  i: integer;
  aWriter: TWriter;
  aHeader: TFileHeader2;

  aCurValue,aNewValue: double;
  aCurDate,aNewDate: integer;
  aDValue,aDDate: Longint;

  aDateSize,aValueSize,aAllSize: Shortint;
  aK: integer;
const
  aBytes: array [0..3] of integer = (0,1,2,4);
begin
  aCurValue:=0;
  aCurDate:=0;


  //ƒальше заголовок
  ZeroMemory(@aHeader,sizeof(aHeader));

  aHeader.version:=2;
  s:='(C) '+Workspace.CompanyName;
  StrLCopy(@aHeader.copyright[0],pchar(s),sizeof(aHeader.copyright));
  StrLCopy(@aHeader.symbol[0],pchar(aData.StockSymbol),sizeof(aHeader.symbol));

  aHeader.digits:=4; //TODO
  aK:=Trunc(power(10,aHeader.digits));

  if aData.Count>0 then
  begin
    aCurValue:=aData.GetValue(0);
    aCurDate:=DateTimeToUnix(aData.GetDateTime(0));

    aHeader.firsttime:=aCurDate;
    aHeader.lasttime:=DateTimeToUnix(aData.GetDateTime(aData.Count-1));
    aHeader.firstvalue:=aCurValue;
  end;

  //данные
  aWriter:=TWriter.Create(aStream,1024*1024); //использование врайтера дает оптимизацию за счет использовани€ буфера
  try
    aWriter.Write(aHeader,sizeof(aHeader));
    for I := 0 to aData.Count - 1 do
    begin
      aNewDate:=DateTimeToUnix(aData.GetDateTime(i));
      aDDate:=aNewDate-aCurDate;
      aDateSize:=GetValueType(aDDate);

      aNewValue:=aData.GetValue(i);
      aDValue:=Round((aNewValue-aCurValue)*aK);
      aValueSize:=GetValueType(aDValue);

      Assert(aDateSize in [1..3]);
      Assert(aValueSize in [1..3]);

      //записываем размеры
      aAllSize:=(aDateSize shl 2) or (aValueSize);

      aWriter.Write(aAllSize, sizeof(aAllSize));
      aWriter.Write(aDDate,aBytes[aDateSize]);
      aWriter.Write(aDValue,aBytes[aValueSize]);

      aCurDate:=aNewDate;
      aCurValue:=aNewValue;
    end;
    aWriter.FlushBuffer;
  finally
    aWriter.Free;
  end;
end;

procedure TStockTickCollectionSerializer.SaveToFile(const aData: IStockTickCollection; const aFileName: string);
var
  aStream: TStream;
begin
  aStream:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(aData,aStream);
  finally
    aStream.Free;
  end;
end;

end.
