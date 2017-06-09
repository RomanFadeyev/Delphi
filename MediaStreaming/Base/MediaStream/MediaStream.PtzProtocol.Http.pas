unit MediaStream.PtzProtocol.Http;

interface
  uses SysUtils, Classes, MediaStream.PtzProtocol.Base,Http.Cgi, Generics.Collections;

type
  TPtzProtocol_Http = class (TPtzProtocol)
  private
    FAddress: string;
    FPort: Word;
    FUser, FPassword: string;

    procedure Init(const aAddress: string; aPort: Word; const aUser,aPassword: string);
  protected
    procedure ExecutePost(const aCommand: string);
    function  ExecuteGet(const aCommand: string): string;

    procedure TraceLine(const aMessage: string);
  public
    constructor Create(const aAddress: string; aPort: Word; const aUser,aPassword: string); virtual;
    destructor Destroy; override;

    class function Name: string; virtual; abstract;
  end;

  TPtzProtocol_HttpClass = class of TPtzProtocol_Http;

  TPtzProtocol_HttpRegistry = class
  private
    FProtocols : TList<TPtzProtocol_HttpClass>;
    function GetProtocol(index: integer): TPtzProtocol_HttpClass;
  public
    procedure AddProtocol(aProtocolClass: TPtzProtocol_HttpClass);

    property Protocols[index: integer]: TPtzProtocol_HttpClass read GetProtocol;
    function Count: integer;

    constructor Create;
    destructor Destroy; override;
  end;

  function ProtocolRegistry: TPtzProtocol_HttpRegistry;

implementation
  uses
    uTrace,
    {implementations}
    MediaStream.PtzProtocol.Http.Everfocus;

var
  gProtocolRegistry: TPtzProtocol_HttpRegistry;

function ProtocolRegistry: TPtzProtocol_HttpRegistry;
begin
  if gProtocolRegistry=nil then
    gProtocolRegistry:=TPtzProtocol_HttpRegistry.Create;

  result:=gProtocolRegistry;
end;

constructor TPtzProtocol_Http.Create(const aAddress: string; aPort: Word;
  const aUser, aPassword: string);
var
  s: string;
begin
  Init(aAddress,aPort,aUser,aPassword);

  s:=StringReplace(Copy(ClassName,2,High(Word)),'_','.',[rfReplaceAll]);
  RegisterCustomTrace(ClassName,'','.'+s);
end;

destructor TPtzProtocol_Http.Destroy;
begin
  inherited;
end;

function TPtzProtocol_Http.ExecuteGet(const aCommand: string): string;
var
  aCgi: TCgi;
begin
  inherited;

  TraceLine('REQUEST GET: '+aCommand);
  try
    aCgi:=TCgi.Create(FAddress, FPort, FUser, FPassword);
    try
      result:=aCgi.Get(aCommand);
    finally
      aCgi.Free;
    end;

    TraceLine('ANSWER: '+result);
  except
    on E:Exception do
    begin
      TraceLine('ERROR: '+E.Message);
      raise;
    end;
  end;
end;

procedure TPtzProtocol_Http.ExecutePost(const aCommand: string);
var
  aCgi: TCgi;
begin
  inherited;

  TraceLine('REQUEST POST: '+aCommand);
  try
    aCgi:=TCgi.Create(FAddress, FPort, FUser, FPassword);
    try
      //TraceLine();
      aCgi.Post(aCommand);
    finally
      aCgi.Free;
    end;
    TraceLine('ANSWER: OK');
  except
    on E:Exception do
    begin
      TraceLine('ERROR: '+E.Message);
      raise;
    end;
  end;
end;

procedure TPtzProtocol_Http.Init(const aAddress: string; aPort: Word; const aUser,aPassword: string);
begin
  FAddress:=aAddress;
  FPort:=aPort;
  FUser:=aUser;
  FPassword:=aPassword;
end;



procedure TPtzProtocol_Http.TraceLine(const aMessage: string);
begin
  uTrace.TraceLine(ClassName,aMessage);
end;

{ TPtzProtocol_HttpRegistry }

procedure TPtzProtocol_HttpRegistry.AddProtocol(
  aProtocolClass: TPtzProtocol_HttpClass);
begin
  FProtocols.Add(aProtocolClass);
end;

function TPtzProtocol_HttpRegistry.Count: integer;
begin
  result:=FProtocols.Count;
end;

constructor TPtzProtocol_HttpRegistry.Create;
begin
  FProtocols:=TList<TPtzProtocol_HttpClass>.Create;
end;

destructor TPtzProtocol_HttpRegistry.Destroy;
begin
  FreeAndNil(FProtocols);
  inherited;
end;

function TPtzProtocol_HttpRegistry.GetProtocol(index: integer): TPtzProtocol_HttpClass;
begin
  result:=FProtocols[index];
end;

initialization

finalization
  FreeAndNil(gProtocolRegistry);

end.
