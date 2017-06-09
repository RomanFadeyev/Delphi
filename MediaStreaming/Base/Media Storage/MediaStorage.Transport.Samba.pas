unit MediaStorage.Transport.Samba;

interface
  uses SysUtils,Classes, Windows, dInterfacesObjectFileStorage,MediaStorage.Transport;

type
  //Транспорт средствами Microsoft Network (samba)
  TRecordObjectTransport_NetworkPath =class (TRecordObjectTransportBase,IRecordObjectTransport)
  private
    FFileName: string;
  public
    function TypeCode: byte;

    //Название транспорта
    function Name: string;

    //Доставляет файл и дает на него ссылку
    function GetReader: IRecordObjectReader;
    //Создает экземпляр писателя файла
    function GetWriter: IRecordObjectWriter;

    function FileExists: boolean;
    function FileSize: int64;

    function ConnectionString: string;
    function NeedCopying: boolean;

    constructor Create(const aConnectionString: string);
  end;

function  IsNetworkPath(const aFileName: string): boolean;

implementation
  uses uTrace,uBaseUtils;

const
  SearchResultTitles: array [boolean] of string = ('Не найден','Найден');

function ExtractHostFromNetAddress(const aNetAddress: string): string;
var
  i: integer;
begin
  if (not (Length(aNetAddress)>2)) or
     (not (aNetAddress[1]='\')) or
     (not (aNetAddress[2]='\')) then
     raise Exception.Create('Указанный адрес не является сетевым');

  i:=3;
  while i<=Length(aNetAddress) do
  begin
    if i<Length(aNetAddress) then
      if aNetAddress[i+1]='\' then
        break;

    inc(i);
  end;

  result:=Copy(aNetAddress,3,i-3+1);
end;

function IsNetworkPath(const aFileName: string): boolean;
begin
  result:=(Length(aFileName)>2) and (aFileName[1]='\') and (aFileName[2]='\');
end;

type
  TRecordObjectReader_NetworkPath = class (TRecordObjectFileBase,IRecordObjectReader)
  private
    FFileName: string;
    FStream  : TFileStream;
  public
    function GetStream: TStream;
    function GetWholeFile: string;

    procedure PrepareFromBegin;
    procedure PrepareFromEnd;

    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  TRecordObjectWriter_NetworkPath = class (TRecordObjectFileBase,IRecordObjectWriter)
  private
    FStream : TFileStream;
  public
    procedure WriteAndCommit(const aData; aSize: integer); overload;
    //Записывает данные из указанного файла в себя и сразу же закрывает сессию. Можно вызывать только 1 раз
    procedure WriteAndCommit(const aFileName: string); overload;

    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  TSambaStream = class (TFileStream)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;


{ TRecordObjectTransport_NetworkPath }

function TRecordObjectTransport_NetworkPath.ConnectionString: string;
begin
  result:=FFileName;
end;

constructor TRecordObjectTransport_NetworkPath.Create(const aConnectionString: string);
begin
  inherited Create;
  FFileName:=aConnectionString;
end;

function TRecordObjectTransport_NetworkPath.FileExists: boolean;
const
  aMethodName = 'TRecordObjectTransport_NetworkPath.FileExists';
var
  aTraceID: cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    result:=SysUtils.FileExists(FFileName);
    TraceLine('Поиск файла '+FFileName+':'+SearchResultTitles[result]);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordObjectTransport_NetworkPath.FileSize: int64;
const
  aMethodName = 'TRecordObjectTransport_NetworkPath.FileExists';
var
  aTraceID: cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    result:=fsiBaseUtils.GetFileSize(FFileName);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordObjectTransport_NetworkPath.GetReader: IRecordObjectReader;
begin
  result:=TRecordObjectReader_NetworkPath.Create(FFileName);
end;

function TRecordObjectTransport_NetworkPath.TypeCode: byte;
begin
  result:=0;
end;

function TRecordObjectTransport_NetworkPath.GetWriter: IRecordObjectWriter;
begin
  result:=TRecordObjectWriter_NetworkPath.Create(FFileName);
end;

function TRecordObjectTransport_NetworkPath.NeedCopying: boolean;
begin
  result:=false;
end;

function TRecordObjectTransport_NetworkPath.Name: string;
begin
  if IsNetworkPath(FFileName) then
    result:='MS Network (Samba)'
  else
    result:='Local File';
end;

{ TRecordObjectReader_NetworkPath }

constructor TRecordObjectReader_NetworkPath.Create(const aFileName: string);
begin
  FFileName:=aFileName;
  FStream:=TSambaStream.Create(aFileName,fmOpenRead or  fmShareDenyNone);
end;

destructor TRecordObjectReader_NetworkPath.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TRecordObjectReader_NetworkPath.GetStream : TStream;
begin
  result:=FStream;
end;

function TRecordObjectReader_NetworkPath.GetWholeFile: string;
begin
  result:=FStream.FileName;
end;

procedure TRecordObjectReader_NetworkPath.PrepareFromBegin;
begin

end;

procedure TRecordObjectReader_NetworkPath.PrepareFromEnd;
begin

end;

{ TRecordObjectWriter_NetworkPath }

constructor TRecordObjectWriter_NetworkPath.Create( const aFileName: string);
begin
  FStream:=TFileStream.Create(aFileName,fmCreate);
end;

destructor TRecordObjectWriter_NetworkPath.Destroy;
begin
  inherited;
  FreeAndNil(FStream);
end;

procedure TRecordObjectWriter_NetworkPath.WriteAndCommit(const aFileName: string);
var
  aSourceStream : TFileStream;
begin
  if FStream=nil then
    raise Exception.Create('Сессия записи закрыта');

  aSourceStream:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  try
    FStream.CopyFrom(aSourceStream,0);
  finally
    aSourceStream.Free;
  end;

  FreeAndNil(FStream);
end;

procedure TRecordObjectWriter_NetworkPath.WriteAndCommit(const aData; aSize: integer);
begin
  if FStream=nil then
    raise Exception.Create('Сессия записи закрыта');
  FStream.WriteBuffer(aData,aSize);
  FreeAndNil(FStream);
end;

{ TSambaStream }

function TSambaStream.Read(var Buffer; Count: Integer): Longint;
begin
//  if Count<=sizeof(HV_FRAME_HEAD) then
//  uTrace.TraceLine(Format('12 байт от позиции %d',[Position]));
  result:=inherited Read(Buffer,Count);
end;

end.

