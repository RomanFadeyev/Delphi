unit MediaProcessing.Global;

interface
  uses Windows, Classes,SyncObjs, SysUtils,MediaProcessing.Definitions,
        Generics.Collections,Collections.Map;

type
  IPropertiesReader = interface
  ['{CD76F24D-2988-4B35-8724-25E4B5213F10}']
    function ReadString(const Name, Default: string): string;
    function ReadInteger(const Name: string; Default: Longint): Longint;
    function ReadDouble(const Name: string; Default: double): TDateTime;
    function ReadDateTime(const Name: string; Default: TDateTime): TDateTime;
    function ReadBool(const Name: string; Default: Boolean): Boolean;
    function ReadBytes(const Name: string): TBytes;
  end;


  IPropertiesWriter = interface
  ['{7649996B-D4F8-443B-99C0-B65BAC73632F}']
    procedure WriteString(const Name, Value: String);
    procedure WriteInteger(const Name: string; Value: Longint);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    procedure WriteDouble(const Name: string; const Value: double);
    procedure WriteBytes(const Name: string;Value: TBytes);
  end;

  TMediaProcessor = class (TInterfacedObject,IMediaProcessor)
  private
    FLastError: string;
    FLastErrorDateTime: TDateTime;
    FLastErrorLock: TCriticalSection;
    FTraceIdentifer:string;
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); overload; virtual;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); overload; virtual;
    procedure Prepare; virtual;

    class function MetaInfo:TMediaProcessorInfo; virtual; abstract;

    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); virtual; abstract;


    //Набор проверок
    procedure CheckIfChannel0(const aFormat: TMediaStreamDataHeader);

    procedure SetLastError(const aError: string);
    procedure SetTraceIdentifier(const aIdentifier: string);

    procedure TraceLine(const aMessage: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    function Info:TMediaProcessorInfo; virtual;

    function  HasCustomProperties: boolean; virtual;
    procedure ShowCustomProperiesDialog;virtual;

    procedure ProcessData(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);

    function LastError: string;
    procedure GetLastErrorInfo(out aMessage: string; out aDateTime: TDateTime); virtual;

    procedure SaveCustomProperties(aStream: TStream); overload;
    procedure LoadCustomProperties(aStream: TStream); overload;

    procedure SaveCustomProperties(var aBytes: TBytes); overload;
    procedure LoadCustomProperties(aBytes: TBytes); overload;
  end;

  TMediaProcessorClass = class of TMediaProcessor;

  TMediaProceccorFactoryItem = class
    MediaProcessorClass: TMediaProcessorClass;
    MediaImplementorClass: TMediaProcessorClass;
  end;

  TMediaProceccorFactory = class
  private
    FRegistry:TObjectList<TMediaProceccorFactoryItem>;

    function GetRegistryIndexByTypeID(const aTypeID: TGUID): integer;
  public
    procedure RegisterMediaProcessor(aClass: TMediaProcessorClass);
    procedure RegisterMediaProcessorImplementation(aClass: TMediaProcessorClass);

    function GetMediaProcessorInfoByTypeID(const aTypeID: TGUID): TMediaProcessorInfo;
    function GetMediaProcessorInfoByIndex(const aIndex: integer): TMediaProcessorInfo;
    function MediaProcessorCount: integer;


    function CreateMediaProcessor(const aTypeID: TGUID; aImplementation: boolean): TMediaProcessor;
    function CreateMediaProcessorsFromStream(aStream:TStream; aImplementation: boolean):TIMediaProcessorArray;
    function CreateMediaProcessorsImplFromStream(aStream:TStream):TIMediaProcessorImplArray;

    function CreateMediaProcessorsFromBytes(const aBytes: TBytes; aImplementation: boolean):TIMediaProcessorArray;
    function CreateMediaProcessorsImplFromBytes(const aBytes: TBytes):TIMediaProcessorImplArray;

    constructor Create;
    destructor Destroy; override;
  end;

  EMediaProcessorInvalidInputData = class (Exception);

  function MediaProceccorFactory: TMediaProceccorFactory;

implementation
  uses Variants,RTLConsts, uTrace,
    //Подключаем все известные процессоры
  MediaProcessing.Convertor.HH.RGB,
  MediaProcessing.Convertor.H264.RGB,
  MediaProcessing.Convertor.HHH264.H264,
  MediaProcessing.Convertor.RGB.MJPEG,
  MediaProcessing.Convertor.RGB.H264,
  MediaProcessing.Convertor.RGB.VFW,
  MediaProcessing.Panorama.RGB,
  MediaProcessing.Stabilizer.RGB,
  MediaProcessing.Drawer.RGB,
  MediaProcessing.Transformer.RGB,
  MediaProcessing.Transformer.HH,
  MediaProcessing.Transformer.PCM,
  MediaProcessing.Convertor.HHAU.PCM,
  MediaProcessing.Processor.VA.Any;

type
  TPropertiesMap = TTextKeyMap<Variant>;
  TPropertiesMapIt = TMapIterator<string,Variant>;
  TPropertiesReader = class (TInterfacedObject,IPropertiesReader)
  private
    FMap: TPropertiesMap;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadString(const Name, Default: string): string;
    function ReadInteger(const Name: string; Default: Longint): Longint;
    function ReadDouble(const Name: string; Default: double): TDateTime;
    function ReadDateTime(const Name: string; Default: TDateTime): TDateTime;
    function ReadBool(const Name: string; Default: Boolean): Boolean;
    function ReadBytes(const Name: string): TBytes;
  end;

  TPropertiesWriter = class (TInterfacedObject,IPropertiesWriter)
  private
    FMap: TPropertiesMap;
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteString(const Name, Value: String);
    procedure WriteInteger(const Name: string; Value: Longint);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDateTime(const Name: string; const Value: TDateTime);
    procedure WriteDouble(const Name: string; const Value: double);
    procedure WriteBytes(const Name: string; value: TBytes);
  end;

  TReaderEx = class (TReader)
  private
  protected
    procedure ReadVariantInternal(aType: integer; var Value: Variant);
  public
    procedure Read(aStream: TStream; Count: Longint); overload;

    procedure ReadBoolean(var Value: Boolean);
    procedure ReadChar(var Value: Char);
    procedure ReadInteger(var Value: cardinal); overload;
    procedure ReadInteger(var Value: Longint); overload;
    procedure ReadInteger(var Value: Int64); overload;
    procedure ReadDouble(var Value: Double);
    procedure ReadDateTime(var Value: TDateTime);
    procedure ReadEnum(var Value);
    procedure ReadString(var Value: string);
    procedure ReadVariant(var Value: Variant);
    procedure ReadGUID(var Value: TGUID);

    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
  end;

  //Базовый класс писателя
  //AddRef/Release отключен
  TWriterEx = class (TWriter)
  protected
    procedure WriteVariantInternal(aType: integer; const aValue: Variant);
  public
    //from IDataWriter
    procedure Write(aStream: TStream; Count: Longint);  overload;

    procedure WriteDateTime(const Value: TDateTime);
    procedure WriteEnum(const Value);
    procedure WriteVariant(const aValue: Variant);
    procedure WriteGUID(const aValue: TGUID);

    //Всегда сохраняет 4 байта вне зависимости от того, что реально хранит число
    procedure WriteDWORD(aValue: integer);

    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
  end;

var
  gMediaProceccorFactory:TMediaProceccorFactory;

const
  vaGUID = 127; //Расширение стандартных хранимых типов

function MediaProceccorFactory: TMediaProceccorFactory;
begin
  if gMediaProceccorFactory=nil then
    gMediaProceccorFactory:=TMediaProceccorFactory.Create;

  result:=gMediaProceccorFactory;
end;

{ TReaderEx }

procedure TReaderEx.ReadVariantInternal(aType: integer; var Value: Variant);
var
  aDouble: double;
begin
  ASSERT((aType and varArray)=0);
  if not (aType in [varEmpty, varNull]) then
  begin
    case aType of
      varSmallint, varInteger, varByte: Value:=ReadInteger;
      varSingle:   Value:=ReadSingle;
      varDouble:   begin ReadDouble(aDouble); Value:=aDouble; end;
      varCurrency: Value:=ReadCurrency;
      varDate    : Value:=ReadDate;
      else
        Value:=inherited ReadString;
    end;
  end
end;

procedure TReaderEx.ReadVariant(var Value: Variant);
var
  aType : integer;
  aLowBound: integer;
  aHighBound: integer;
  i         : integer;
  aItem     : variant;
begin
  aType:=ReadInteger;

  if (aType and varArray)=0 then
    ReadVariantInternal(aType,Value)
  else begin
    aType:=(aType and not varArray);

    ReadInteger(aLowBound);
    ReadInteger(aHighBound);
    Value:=VarArrayCreate([aLowBound,aHighBound],aType);

    for i:=aLowBound to aHighBound do
    begin
      ReadVariantInternal(aType,aItem);
      Value[i]:=aItem;
    end;
  end;
end;

procedure TReaderEx.ReadGUID(var Value: TGUID);
begin
  if ReadValue<>TValueType(vaGUID) then
    raise EReadError.Create(SInvalidPropertyValue);

  inherited Read(Value,sizeof(TGUID));
end;

procedure TReaderEx.Read(aStream: TStream; Count: Integer);
var
  a: array of byte;
begin
  while Count>1024 do
  begin
    Read(aStream,1024);
    Dec(count,1024);
  end;

  if Count>0 then
  begin
    SetLength(a,Count);
    Read(a[0],Count);
    aStream.Write(a[0],Count)
  end;
end;

procedure TReaderEx.ReadBoolean(var Value: Boolean);
begin
  Value := inherited ReadBoolean;
end;

procedure TReaderEx.ReadChar(var Value: Char);
begin
  Value := inherited ReadChar;
end;

procedure TReaderEx.ReadInteger(var Value: Integer);
begin
  Value := inherited ReadInteger;
end;

procedure TReaderEx.ReadInteger(var Value: Int64);
begin
  Value := inherited ReadInt64;
end;

procedure TReaderEx.ReadInteger(var Value: cardinal);
begin
  Value := inherited ReadInteger;
end;

procedure TReaderEx.ReadString(var Value: string);
begin
  Value := inherited ReadString;
end;

procedure TReaderEx.ReadEnum(var Value);
begin
  integer(Value):=inherited ReadInteger;
end;

constructor TReaderEx.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream,BufSize);
end;

procedure TReaderEx.ReadDouble(var Value: Double);
begin
  Value:=inherited ReadDouble;
end;

procedure TReaderEx.ReadDateTime(var Value: TDateTime);
begin
  Value:=inherited ReadDate;
end;

destructor TReaderEx.Destroy;
begin
  inherited;
end;

{ TWriterEx }

procedure TWriterEx.WriteEnum(const Value);
begin
  WriteInteger(integer(Value));
end;

procedure TWriterEx.WriteVariantInternal(aType: integer; const aValue: Variant);
begin
  ASSERT(not VarIsArray(aValue));

  if not (aType in [varEmpty, varNull]) then
  begin
    case aType of
      varSmallint, varInteger, varByte: WriteInteger(aValue);
      varSingle  : WriteSingle(aValue);
      varDouble  : WriteDouble(aValue);
      varCurrency: WriteCurrency(aValue);
      varDate    : WriteDate(aValue);
      else
        WriteString(aValue);
    end;
  end;
end;

procedure TWriterEx.WriteVariant(const aValue: Variant);
var
  aType    : integer;
  i        : integer;
begin
  aType:=VarType(aValue);
  if not VarIsArray(aValue) then
    if aType in [varSingle,varDouble,varCurrency] then
      if aValue=0.0 then
        aType:=varInteger;

  WriteInteger(aType);

  if not VarIsArray(aValue) then
  begin
    WriteVariantInternal(aType,aValue)
  end
  else begin
    ASSERT(VarArrayDimCount(aValue)=1);
    WriteInteger(VarArrayLowBound(aValue,1));
    WriteInteger(VarArrayHighBound(aValue,1));

    aType:=aType and not varArray;
    for i:=VarArrayLowBound(aValue,1) to VarArrayHighBound(aValue,1) do
      WriteVariantInternal(aType,aValue[i]);
  end;
end;

procedure TWriterEx.WriteGUID(const aValue: TGUID);
begin
  WriteValue(TValueType(vaGUID));
  inherited Write(aValue,sizeof(TGUID));
end;

procedure TWriterEx.WriteDWORD(aValue: integer);
begin
  WriteValue(vaInt32);
  Write(aValue, SizeOf(Integer));
end;

destructor TWriterEx.Destroy;
begin
  inherited;
end;

constructor TWriterEx.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream,BufSize);
end;

procedure TWriterEx.Write(aStream: TStream; Count: Integer);
var
  a: array of byte;
begin
  while Count>1024 do
  begin
     Write(aStream,1024);
     Dec(Count,1024);
  end;

  if Count>0 then
  begin
    SetLength(a,Count);
    aStream.Read(a[0],Count);
    Write(a[0],Count);
  end;
end;

procedure TWriterEx.WriteDateTime(const Value: TDateTime);
begin
  WriteDouble(Value);
end;

{ TPropertiesReader }

constructor TPropertiesReader.Create;
begin
  FMap:=TPropertiesMap.Create;
end;

destructor TPropertiesReader.Destroy;
begin
  FreeAndNil(FMap);
  inherited;
end;

function TPropertiesReader.ReadBool(const Name: string;Default: Boolean): Boolean;
var
  x: variant;
begin
  result:=default;
  try
    if FMap.Lookup(Name,x) then
      result:=x;
  except
  end;
end;

function TPropertiesReader.ReadBytes(const Name: string): TBytes;
var
  x: variant;
begin
  result:=nil;
  try
    if FMap.Lookup(Name,x) then
    begin
      if VarArrayHighBound(x,1)>=0 then
        result:=x;
    end;
  except
  end;
end;

function TPropertiesReader.ReadDateTime(const Name: string;Default: TDateTime): TDateTime;
var
  x: variant;
begin
  result:=default;
  try
    if FMap.Lookup(Name,x) then
      result:=x;
  except
  end;
end;

function TPropertiesReader.ReadDouble(const Name: string;Default: double): TDateTime;
var
  x: variant;
begin
  result:=default;
  try
    if FMap.Lookup(Name,x) then
      result:=x;
  except
  end;
end;

function TPropertiesReader.ReadInteger(const Name: string;Default: Integer): Longint;
var
  x: variant;
begin
  result:=default;
  try
    if FMap.Lookup(Name,x) then
      result:=x;
  except
  end;
end;

function TPropertiesReader.ReadString(const Name, Default: string): string;
var
  x: variant;
begin
  result:=default;
  try
    if FMap.Lookup(Name,x) then
      result:=x;
  except
  end;
end;


{ TPropertiesWriter }

constructor TPropertiesWriter.Create;
begin
  FMap:=TPropertiesMap.Create;
end;

destructor TPropertiesWriter.Destroy;
begin
  FreeAndNil(FMap);
  inherited;
end;

procedure TPropertiesWriter.WriteBool(const Name: string; Value: Boolean);
begin
  FMap.Add(Name,Value);
end;

procedure TPropertiesWriter.WriteBytes(const Name: string; value: TBytes);
begin
  FMap.Add(Name,Value);
end;

procedure TPropertiesWriter.WriteDateTime(const Name: string; const Value: TDateTime);
begin
  FMap.Add(Name,Value);
end;

procedure TPropertiesWriter.WriteDouble(const Name: string;const Value: double);
begin
  FMap.Add(Name,Value);
end;

procedure TPropertiesWriter.WriteInteger(const Name: string; Value: Integer);
begin
  FMap.Add(Name,Value);
end;

procedure TPropertiesWriter.WriteString(const Name, Value: String);
begin
  FMap.Add(Name,Value);
end;

{ TMediaProcessor }

procedure TMediaProcessor.AfterConstruction;
var
  aDataReader : TPropertiesReader;
begin
  inherited;
  aDataReader:=TPropertiesReader.Create;
  try
    LoadCustomProperties(aDataReader);
  finally
    aDataReader.Free;
  end;
end;

constructor TMediaProcessor.Create;
begin
  FLastErrorLock:=TCriticalSection.Create;
end;

destructor TMediaProcessor.Destroy;
begin
  inherited;
  FreeAndNil(FLastErrorLock);
end;

procedure TMediaProcessor.GetLastErrorInfo(out aMessage: string;
  out aDateTime: TDateTime);
begin
  FLastErrorLock.Enter;
  try
    aMessage:=FLastError;
    aDateTime:=FLastErrorDateTime;
  finally
    FLastErrorLock.Leave;
  end;
end;

function TMediaProcessor.HasCustomProperties: boolean;
begin
  result:=false;
end;

function TMediaProcessor.Info: TMediaProcessorInfo;
begin
  result:=MetaInfo;
end;

function TMediaProcessor.LastError: string;
begin
  result:=FLastError;
end;

procedure TMediaProcessor.LoadCustomProperties(aStream: TStream);
var
  aPropReader : TPropertiesReader;
var
  aCount: integer;
  i: Integer;
  aKey: string;
  aValue: Variant;
  aReader : TReaderEx;
begin
  aPropReader:=TPropertiesReader.Create;
  try
    aReader:=TReaderEx.Create(aStream,1024);
    try
      aReader.ReadInteger(aCount);
      for i := 0 to aCount-1 do
      begin
        aReader.ReadString(aKey);
        aReader.ReadVariant(aValue);
        aPropReader.FMap.Add(aKey,aValue);
      end;
    finally
      aReader.Free;
    end;
    LoadCustomProperties(aPropReader);
  finally
    aPropReader.Free;
  end;
end;

procedure TMediaProcessor.Prepare;
begin

end;

procedure TMediaProcessor.ProcessData(aInData: pointer; aInDataSize: cardinal;
  const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
  out aOutData: pointer; out aOutDataSize: cardinal;
  out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer;
  out aOutInfoSize: cardinal);
var
  aMsg: string;
begin
  if not Info.CanAcceptInputStreamType(aInFormat.biStreamType) then
  begin
    aMsg:=Format('Несовместимые типы потока: ожидался тип: %s, получен тип: %s',[StreamTypesToFourccString(Info.InputStreamTypes),GetStreamTypeName(aInFormat.biStreamType)]);
    SetLastError(aMsg);
    TraceLine(aMsg);
    aOutData:=nil;
    aOutDataSize:=0;
    aOutFormat.Clear;
    aOutInfo:=nil;
    aOutInfoSize:=0;
    exit;
  end;

  try
    Process(aInData,aInDataSize,aInFormat,aInfo,aInfoSize,aOutData,aOutDataSize,aOutFormat,aOutInfo,aOutInfoSize);
  except
    on E:Exception do
    begin
      SetLastError(E.Message);
      TraceLine('Возникло исключение: '+E.Message);
      aOutData:=nil;
      aOutDataSize:=0;
      aOutFormat.Clear;
      aOutInfo:=nil;
      aOutInfoSize:=0;
    end;
  end;
end;

procedure TMediaProcessor.SaveCustomProperties(aStream: TStream);
var
  aDataWriter : TPropertiesWriter;
  aPersister : TWriterEx;
  it: TPropertiesMapIt;
begin
  aDataWriter:=TPropertiesWriter.Create;
  try
    SaveCustomProperties(aDataWriter);
    aPersister:=TWriterEx.Create(aStream,1024);
    aPersister.WriteInteger(aDataWriter.FMap.Count);

    aDataWriter.FMap.GetFirst(it);
    while it.Valid do
    begin
      aPersister.WriteString(it.Key);
      aPersister.WriteVariant(it.Value);
      aDataWriter.FMap.GetNext(it);
    end;

    aPersister.Free;

  finally
    aDataWriter.Free;
  end;
end;

procedure TMediaProcessor.SetLastError(const aError: string);
begin
  Assert(FLastErrorLock<>nil);
  FLastErrorLock.Enter;
  try
    FLastError:=aError;
    FLastErrorDateTime:=Now;
  finally
    FLastErrorLock.Leave;
  end;
end;

procedure TMediaProcessor.SetTraceIdentifier(const aIdentifier: string);
begin
  Assert(aIdentifier<>'');
  FTraceIdentifer:=aIdentifier;
  RegisterCustomTrace(ClassName,'','.proc.'+aIdentifier);
end;

procedure TMediaProcessor.TraceLine(const aMessage: string);
begin
  if FTraceIdentifer<>'' then
    uTrace.TraceLine(ClassName,aMessage);
end;

procedure TMediaProcessor.LoadCustomProperties(const aReader: IPropertiesReader);
begin

end;

procedure TMediaProcessor.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin

end;

procedure TMediaProcessor.ShowCustomProperiesDialog;
begin

end;

procedure TMediaProcessor.CheckIfChannel0(const aFormat: TMediaStreamDataHeader);
begin
  if aFormat.Channel<>0 then
    raise EMediaProcessorInvalidInputData.CreateFmt('Поддерживается обработка только одного канала: №0. Запрос на обработку канала №%d отклонен',[aFormat.Channel]);
end;

procedure TMediaProcessor.LoadCustomProperties(aBytes: TBytes);
var
  aByteStream: TBytesStream;
begin
  aByteStream:=TBytesStream.Create(aBytes);
  try
    LoadCustomProperties(aByteStream);
  finally
    aByteStream.Free;
  end;
end;

procedure TMediaProcessor.SaveCustomProperties(var aBytes: TBytes);
var
  aByteStream: TBytesStream;
begin
  aByteStream:=TBytesStream.Create();
  try
    SaveCustomProperties(aByteStream);
    aBytes:=Copy(aByteStream.Bytes,0,aByteStream.Position);
  finally
    aByteStream.Free;
  end;
end;

{ TMediaProceccorFactory }

constructor TMediaProceccorFactory.Create;
begin
  FRegistry:=TObjectList<TMediaProceccorFactoryItem>.Create;
end;

function TMediaProceccorFactory.CreateMediaProcessor(const aTypeID: TGUID; aImplementation: boolean): TMediaProcessor;
var
  aItem: TMediaProceccorFactoryItem;
begin
  aItem:=FRegistry[GetRegistryIndexByTypeID(aTypeID)];
  if (not aImplementation) then
    result:=aItem.MediaProcessorClass.Create
  else begin
    if aItem.MediaImplementorClass=nil then
      raise Exception.Create('Реализация медиа-процессора не зарегистрирована');

    result:=aItem.MediaImplementorClass.Create;
  end;
end;

function TMediaProceccorFactory.CreateMediaProcessorsFromBytes(const aBytes: TBytes; aImplementation: boolean): TIMediaProcessorArray;
var
  aByteStream: TBytesStream;
begin
  result:=nil;
  if Length(aBytes)>0 then
  begin
    aByteStream:=TBytesStream.Create(aBytes);
    try
      result:=CreateMediaProcessorsFromStream(aByteStream,aImplementation);
    finally
      aByteStream.Free;
    end;
  end;
end;

function TMediaProceccorFactory.CreateMediaProcessorsFromStream(aStream: TStream; aImplementation: boolean): TIMediaProcessorArray;
var
  aIID : TGUID;
  i: integer;
  aMP : IMediaProcessor;
begin
  //Медиа процессоры
  aStream.ReadBuffer(i,sizeof(i));
  Assert(i<1000);
  SetLength(result,i);
  for i := 0 to i-1 do
  begin
    aStream.Read(aIID,sizeof(aIID));
    aMP:=MediaProcessing.Global.MediaProceccorFactory.CreateMediaProcessor(aIID,aImplementation);
    aMP.LoadCustomProperties(aStream);
    result[i]:=aMP;
  end;
end;

function TMediaProceccorFactory.CreateMediaProcessorsImplFromBytes(const aBytes: TBytes): TIMediaProcessorImplArray;
var
  aRes: TIMediaProcessorArray;
  i: Integer;
begin
  aRes:=CreateMediaProcessorsFromBytes(aBytes,true);
  SetLength(result,Length(aRes));

  for i := 0 to High(aRes) do
    result[i]:=aRes[i] as IMediaProcessorImpl;
end;

function TMediaProceccorFactory.CreateMediaProcessorsImplFromStream(aStream: TStream): TIMediaProcessorImplArray;
var
  aRes: TIMediaProcessorArray;
  i: Integer;
begin
  aRes:=CreateMediaProcessorsFromStream(aStream,true);
  SetLength(result,Length(aRes));

  for i := 0 to High(aRes) do
    result[i]:=aRes[i] as IMediaProcessorImpl;
end;

destructor TMediaProceccorFactory.Destroy;
begin
  inherited;
  FreeAndNil(FRegistry);
end;

function TMediaProceccorFactory.GetMediaProcessorInfoByIndex(
  const aIndex: integer): TMediaProcessorInfo;
begin
  result:=FRegistry[aIndex].MediaProcessorClass.MetaInfo;
end;

function TMediaProceccorFactory.GetMediaProcessorInfoByTypeID(const aTypeID: TGUID): TMediaProcessorInfo;
begin
  result:=FRegistry[GetRegistryIndexByTypeID(aTypeID)].MediaProcessorClass.MetaInfo;
end;

function TMediaProceccorFactory.GetRegistryIndexByTypeID(const aTypeID: TGUID): integer;
var
  i: integer;
begin
  for i := 0 to FRegistry.Count - 1 do
  begin
    if IsEqualGUID(aTypeID,FRegistry[i].MediaProcessorClass.MetaInfo.TypeID) then
    begin
      result:=i;
      exit;
    end;
  end;

  raise Exception.Create('Медиа процессор не найден');
end;

function TMediaProceccorFactory.MediaProcessorCount: integer;
begin
  result:=FRegistry.Count;
end;

procedure TMediaProceccorFactory.RegisterMediaProcessor(aClass: TMediaProcessorClass);
var
  aItem: TMediaProceccorFactoryItem;
begin
  aItem:=TMediaProceccorFactoryItem.Create;
  aItem.MediaProcessorClass:=aClass;
  FRegistry.Add(aItem);
end;

procedure TMediaProceccorFactory.RegisterMediaProcessorImplementation(
  aClass: TMediaProcessorClass);
begin
  FRegistry[GetRegistryIndexByTypeID(aClass.MetaInfo.TypeID)].MediaImplementorClass:=aClass;
end;

initialization

finalization
  FreeAndNil(gMediaProceccorFactory);

end.
