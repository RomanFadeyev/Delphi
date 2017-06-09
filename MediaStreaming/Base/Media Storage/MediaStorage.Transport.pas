unit MediaStorage.Transport;

interface
  uses SysUtils,Classes, Windows;

//{$DEFINE STATIC_LINK_FILEAGENT}
type
  IRecordObjectReader = interface
  ['{26EFF231-E972-4197-95A9-F88191C2A421}']
    function GetStream: TStream;
    function GetWholeFile: string;

    //Подготовка к загрузке сначала файла
    procedure PrepareFromBegin;
    //Подготовка к загрузке с конца файла
    procedure PrepareFromEnd;
  end;

  IRecordObjectWriter = interface
  ['{39EFFB2A-856A-4D2A-8D66-349C4DD4B21B}']
    //Записывает данные и сразу же закрывает сессию. Можно вызывать только 1 раз
    procedure WriteAndCommit(const aData; aSize: integer); overload;
    //Записывает данные из указанного файла в себя и сразу же закрывает сессию. Можно вызывать только 1 раз
    procedure WriteAndCommit(const aFileName: string); overload;
  end;

  IRecordObjectTransport = interface
  ['{A47E3851-820A-4EF3-9E26-C715417E4FEE}']
    function TypeCode: byte;

    //Название транспорта
    function Name: string;

    //Доставляет файл и дает на него ссылку для чтения
    function GetReader: IRecordObjectReader;

    //Создает экземпляр писателя файла
    function GetWriter: IRecordObjectWriter;

    function FileExists: boolean;
    function FileSize: int64;

    function ConnectionString: string;
    function NeedCopying: boolean;

    //Указывает на то, что транспорту надо будет создать временную локальную копию файла
    //function NeedFileLocalCopyCreation:boolean;
  end;

  IRecordObjectTransportFactory = interface
    function CreateTransport(const aFileName: string):IRecordObjectTransport;
  end;

  TRecordObjectTransportBase = class (TInterfacedObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRecordObjectFileBase = class (TInterfacedObject)
  end;

implementation


var
  gTransportInstanceCount: integer;


{ TRecordObjectTransportBase }

constructor TRecordObjectTransportBase.Create;
begin
  inc(gTransportInstanceCount);
end;

destructor TRecordObjectTransportBase.Destroy;
begin
  inherited;
  dec(gTransportInstanceCount);
end;



initialization

finalization
  try
  except
    Assert(gTransportInstanceCount=0);
  end;

end.

