unit MediaStorage.Transport.FileServer;

interface
  uses SysUtils,Classes, Windows, dInterfacesFileAgent,dInterfacesObjectFileStorage,MediaStorage.Transport;

type
  //Транспорт через собственный FileServer
  TRecordObjectTransport_FileServer =class (TRecordObjectTransportBase,IRecordObjectTransport)
  private
    FConnectionString: string;
    FPrebuffer_OperationalKB: cardinal;
    FPrebuffer_FileKB: cardinal;
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

    constructor Create(const aConnectionString: string;aPrebuffer_OperationalKB: cardinal=100; aPrebuffer_FileKB: cardinal=100);
  end;

implementation
  uses HHCommon, uFileAgent,uTrace
  {$IFDEF STATIC_LINK_FILEAGENT}
  ,uFileAgent_impl
  {$ENDIF}
  ;

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

function GetFileAgent(аPrebuffer_OperationalKB: cardinal): IFileAgent;
var
  aTempPath: string;
begin
{$IFDEF STATIC_LINK_FILEAGENT}
  result:=uFileAgent_impl.GetXClassIface as IFileAgent;
{$ELSE}
  result:=fsiFileAgent;
{$ENDIF}
  //result.ConnectCount:=1;
  //result.ConnectDelay:=0;
  Assert(аPrebuffer_OperationalKB>0);
  result.BlockSize:=аPrebuffer_OperationalKB*1024;

  SetLength(aTempPath,MAX_PATH);
  GetTempPath(length(aTempPath), pchar(aTempPath));
  aTempPath:=IncludeTrailingPathDelimiter(pchar(aTempPath))+'FileAgentCache';
  result.CacheDirectory:=aTempPath;
end;

type
  TRecordObjectReader_FileServer = class (TRecordObjectFileBase,IRecordObjectReader)
  private
    FFileAgentStreamMediator : TStream;
    FPrebuffer_OperationalKB: cardinal;
    FPrebuffer_FileKB: cardinal;
  public
    function GetStream: TStream;
    function GetWholeFile: string;

    procedure PrepareFromBegin;
    procedure PrepareFromEnd;

    constructor Create(const aConnectionString: string;aPrebuffer_OperationalKB: cardinal; aPrebuffer_FileKB: cardinal);
    destructor Destroy; override;
  end;

  TRecordObjectWriter_FileServer = class (TRecordObjectFileBase,IRecordObjectWriter)
  private
    FFileName : string;
  public
    procedure WriteAndCommit(const aData; aSize: integer); overload;
    //Записывает данные из указанного файла в себя и сразу же закрывает сессию. Можно вызывать только 1 раз
    procedure WriteAndCommit(const aFileName: string); overload;

    constructor Create(const aFileName: string);
    destructor Destroy; override;
  end;

  TFileAgentStreamMediator = class (TStream)
  private
    FPrebuffer_OperationalKB: cardinal;
    FFileAgentReader: IFileReader;
  public
    constructor Create(const aFileAgentReader: IFileReader; aPrebuffer_OperationalKB: cardinal);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TRecordObjectTransport_FileServer }

function TRecordObjectTransport_FileServer.ConnectionString: string;
begin
  result:=FConnectionString;
end;

constructor TRecordObjectTransport_FileServer.Create(const aConnectionString: string;aPrebuffer_OperationalKB: cardinal; aPrebuffer_FileKB: cardinal);
begin
  inherited Create;
  Assert(aPrebuffer_OperationalKB>0);
  FPrebuffer_OperationalKB:=aPrebuffer_OperationalKB;
  FPrebuffer_FileKB:=aPrebuffer_FileKB;

  FConnectionString:=aConnectionString;
  ExtractHostFromNetAddress(FConnectionString); //Проверим, а вообще это сетевой путь
end;

function TRecordObjectTransport_FileServer.FileExists: boolean;
const
  aMethodName = 'TRecordObjectTransport_FileServer.FileExists';
var
  aFileAgent : IFileAgent;
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    aFileAgent:=GetFileAgent(FPrebuffer_OperationalKB);
    result:=aFileAgent.FileExists(FConnectionString);
    TraceLine('Поиск файла '+FConnectionString+':'+SearchResultTitles[result]);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordObjectTransport_FileServer.FileSize: int64;
const
  aMethodName = 'TRecordObjectTransport_FileServer.FileSize';
var
  aFileAgent : IFileAgent;
  aTraceID : cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    aFileAgent:=GetFileAgent(FPrebuffer_OperationalKB);
    result:=aFileAgent.FileSize(FConnectionString);
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TRecordObjectTransport_FileServer.GetReader: IRecordObjectReader;
begin
  result:=TRecordObjectReader_FileServer.Create(ConnectionString,FPrebuffer_OperationalKB,FPrebuffer_FileKB);
end;

function TRecordObjectTransport_FileServer.TypeCode: byte;
begin
  result:=1;
end;

function TRecordObjectTransport_FileServer.GetWriter: IRecordObjectWriter;
begin
  result:=TRecordObjectWriter_FileServer.Create(ConnectionString);
end;

function TRecordObjectTransport_FileServer.NeedCopying: boolean;
begin
  result:=true;
end;

function TRecordObjectTransport_FileServer.Name: string;
begin
  result:='File Server';
end;

{ TRecordObjectReader_FileServer }

constructor TRecordObjectReader_FileServer.Create(const aConnectionString: string;aPrebuffer_OperationalKB: cardinal; aPrebuffer_FileKB: cardinal);
var
  aFileAgent : IFileAgent;
  aFileAgentReader : IFileReader;
begin
  Assert(aPrebuffer_OperationalKB>0);
  FPrebuffer_OperationalKB:=aPrebuffer_OperationalKB;
  FPrebuffer_FileKB:=aPrebuffer_FileKB;

  aFileAgent:=GetFileAgent(FPrebuffer_OperationalKB);

  aFileAgentReader:=aFileAgent.GetFileStreamEx(aConnectionString);
  FFileAgentStreamMediator:=TFileAgentStreamMediator.Create(aFileAgentReader,FPrebuffer_OperationalKB);
end;

destructor TRecordObjectReader_FileServer.Destroy;
begin
  FreeAndNil(FFileAgentStreamMediator);
  inherited;
end;

function TRecordObjectReader_FileServer.GetStream: TStream;
begin
  result:=FFileAgentStreamMediator;
end;

function TRecordObjectReader_FileServer.GetWholeFile: string;
var
  aLocalFile : TFileStream;
  aSize: int64;
begin
  with TFileAgentStreamMediator(FFileAgentStreamMediator) do
  begin
    aSize:=FFileAgentReader.Size;
    FFileAgentReader.PrepareSync(0,aSize);
    result:=FFileAgentReader.GetLocalFileName;
  end;
  FreeAndNil(FFileAgentStreamMediator);

  //Проверка
  aLocalFile:=TFileStream.Create(PChar(result),fmOpenRead or fmShareDenyNone);
  try
    Assert(aSize=aLocalFile.Size);
  finally
    aLocalFile.Free;
  end;
end;

procedure TRecordObjectReader_FileServer.PrepareFromBegin;
begin
  if FFileAgentStreamMediator<>nil then //Такое может быть в случае GetWholeFile
    //X КБ с начала
    TFileAgentStreamMediator(FFileAgentStreamMediator).FFileAgentReader.Prepare(0,FPrebuffer_FileKB*1024);
end;

procedure TRecordObjectReader_FileServer.PrepareFromEnd;
begin
  if FFileAgentStreamMediator<>nil then //Такое может быть в случае GetWholeFile
    with TFileAgentStreamMediator(FFileAgentStreamMediator) do
    begin
      //Оперативный буфер с начала
      FFileAgentReader.Prepare(0,FPrebuffer_OperationalKB*1024);
      //Файловый буфер с конца
      FFileAgentReader.Prepare(FFileAgentReader.Size-integer(FPrebuffer_FileKB*1024),FPrebuffer_FileKB*1024);
    end;
end;

{ TRecordObjectWriter_FileServer }

constructor TRecordObjectWriter_FileServer.Create(const aFileName: string);
begin
  inherited Create;
  FFileName:=aFileName;
end;

destructor TRecordObjectWriter_FileServer.Destroy;
begin
  inherited;
end;

procedure TRecordObjectWriter_FileServer.WriteAndCommit(const aFileName: string);
var
  aFileAgent : IFileAgent;
begin
  aFileAgent:=GetFileAgent(100*1024);
  aFileAgent.CopyFile(aFileName,FFileName);
  aFileAgent:=nil;
end;

procedure TRecordObjectWriter_FileServer.WriteAndCommit(const aData; aSize: integer);
begin
  raise Exception.Create('Не реализовано');
end;

{ TFileAgentStreamMediator }

constructor TFileAgentStreamMediator.Create(const aFileAgentReader: IFileReader;aPrebuffer_OperationalKB: cardinal);
begin
  Assert(aPrebuffer_OperationalKB>0);
  FPrebuffer_OperationalKB:=aPrebuffer_OperationalKB;
  FFileAgentReader:=aFileAgentReader;
  inherited Create;
end;

destructor TFileAgentStreamMediator.Destroy;
begin
  FFileAgentReader:=nil;
  inherited;
end;

function TFileAgentStreamMediator.Read(var Buffer; Count: Integer): Longint;
begin
  //FIX Считывание заголовков из MP6
  //TODO продумать, как сделать настройку
  if Count<=sizeof(HV_FRAME_HEAD) then
  begin
    result:=FFileAgentReader.ReadNoCache(Buffer,Count);
  end
  else begin
    result:=FFileAgentReader.Read(Buffer,Count);
    //Ставим на докачку следующие X КБ оперативного буфера
    FFileAgentReader.Prepare(FFileAgentReader.Position,FPrebuffer_OperationalKB*1024);
  end;
end;

function TFileAgentStreamMediator.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result:=FFileAgentReader.Seek(Offset,Word(Origin));
end;

function TFileAgentStreamMediator.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('Операция не поддерживается');
end;

end.

