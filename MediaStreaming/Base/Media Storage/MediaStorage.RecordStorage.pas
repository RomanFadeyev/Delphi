{***********************************<_INFO>************************************}
{  <������>      �����������                                                   }
{                                                                              }
{  <�������>     16:�����-��������                                             }
{                                                                              }
{  <������>      ������� ��� ������� � ��������� ���������                     }
{                                                                              }
{  <�����>       ������ �.�.                                                   }
{                                                                              }
{  <����>        16.12.2008                                                    }
{                                                                              }
{  <��������>    ��� "�������"                                                 }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaStorage.RecordStorage;

interface

uses
  Windows, SysUtils, Classes,
  MediaStorage.Transport, MediaStorage.Consts, uBaseClasses;

type
  TrsRecordSourceId = record
    IsGuidCompartible: boolean;
    Guid: TGUID;
    Name: string[255];

    procedure Init(const aGuid: TGUID); overload;
    procedure Init(const aName: string); overload;

    function Equals(const aId: TrsRecordSourceId): boolean;
  end;

  //���������� �� ���������, ������� �������� � ��
  TrsRecordSourceInfo = record
    Name: string;
    Url  : string;
    Id  : TrsRecordSourceId;
    //ContentType: cardinal;

    ReserveConnectionString: string;
    Available : boolean;
  end;
  TrsRecordSourceInfoArray = array of TrsRecordSourceInfo;

  // ���������� � ������
  TrsRecordObjectInfo = record
    Id: string;
    StartDateTime : TDateTime;
    EndDateTime   : TDateTime;
    Transport     : IRecordObjectTransport;
    Owner       : string;
  end;
  TrsRecordObjectInfoArray = array of TrsRecordObjectInfo;


  TrsEventInfo = record
    EventType : TRsEventType;
    DateTime: TDateTime;
    EndDateTime   : TDateTime;
  end;
  TrsEventInfoArray = array of TrsEventInfo;


  TrsFileLink = class
  private
    FFileName: string;
    FTemporary: boolean;
  public
    constructor Create(const aFileName: string; aTemporary: boolean);
    destructor Destroy; override;

    property FileName: string read FFileName;
  end;


type
  TRecordStorage = class;
  TRecordStorageReadProgress = procedure (aSender: TRecordStorage; aPosition, aSize: int64; var aContinue: boolean) of object;

  IRecordWriteableAccess = interface
    //������� ������ �� �� Id
    procedure DeleteRecord(const aId: string);

    //���������� ������ �� ������ ��������� � ������
    procedure MoveRecords(const aRecordSourceId, aDestRecordSourceId: TrsRecordSourceId);
  end;

  IRecordSourceWriteableAccess = interface
    //������� �������� �� ��� Id
    procedure DeleteRecordSource(const aId: TrsRecordSourceId);
  end;

  TRecordStorage = class (TInterfaceProvider)
  public
    function GetConnectionString: string; virtual; abstract;

    procedure CheckAvailable; virtual; abstract;
    function  IsAvailable:boolean; virtual; abstract;

    //���� ����� ������������ � ����� ����������� �� ���� ��������� ��� ������
    procedure GetMinMaxDate(out aMin,aMax: TDateTime); virtual; abstract;

    //�������� ���������� ��� ���� ����������, �������� � ��
    function  GetAllRecordSources:TrsRecordSourceInfoArray; virtual; abstract;
    function  GetRecordSourceByName(const aRecordSourceName: string): TrsRecordSourceInfo;

    //�������� ���������� ��� ���� ������ � ��������� ���������
    function  GetRecords(const aSourceId: TrsRecordSourceId; const aFromTime:TDateTime=0; const aToTime: TDateTime=0; aHandler:TRecordStorageReadProgress=nil): TrsRecordObjectInfoArray; overload; virtual; abstract;

    //�������� ���������� � �������� ������� ��������
    function  GetEvents(const aSourceId: TrsRecordSourceId; const aFromTime,aToTime: TDateTime): TrsEventInfoArray; virtual; abstract;

    procedure GetRecordSourceDateRanges(const aSourceId: TrsRecordSourceId; out aStart,aEnd: TDateTime); virtual; abstract;

    //������� �� ��������� ��������� ���� � ���������� ���� � ����. ���� ���� �� ������, ���������� nil
    function  GetFile(const aFileName: string): TrsFileLink; virtual; abstract;

    function  RecordWriteableAccess: IRecordWriteableAccess; virtual;
    function  RecordSourceWriteableAccess: IRecordSourceWriteableAccess; virtual;
  end;


implementation
  uses ActiveX;

{ TrsFileLink }

constructor TrsFileLink.Create(const aFileName: string; aTemporary: boolean);
begin
  FFileName:=aFileName;
  FTemporary:=aTemporary;
end;

destructor TrsFileLink.Destroy;
begin
  if FTemporary then
    DeleteFile(FFileName);
  inherited;
end;

{ TrsRecordSourceId }

procedure TrsRecordSourceId.Init(const aGuid: TGUID);
begin
  self.IsGuidCompartible:=true;
  self.Guid:=aGuid;
  self.Name:=GUIDToString(self.Guid);
end;

function TrsRecordSourceId.Equals(const aId: TrsRecordSourceId): boolean;
begin
  result:=IsEqualGUID(aId.Guid,self.Guid) and (self.Name=aId.Name);
end;

procedure TrsRecordSourceId.Init(const aName: string);
begin
  self.Name:=aName;
  self.IsGuidCompartible:=Succeeded(CLSIDFromString(PWideChar(WideString(aName)), self.Guid));
end;

{ TRecordStorage }


function TRecordStorage.GetRecordSourceByName(const aRecordSourceName: string): TrsRecordSourceInfo;
var
  aRecordSources: TrsRecordSourceInfoArray;
  i: Integer;
begin
  aRecordSources:=GetAllRecordSources;
  for i := 0 to High(aRecordSources) do
    if AnsiSameText(aRecordSources[i].Name,aRecordSourceName) then
      exit(aRecordSources[i]);

  raise Exception.CreateFmt('�������� ������� "%s" � �������� ��������� �� ����������',[aRecordSourceName]);
end;

function TRecordStorage.RecordSourceWriteableAccess: IRecordSourceWriteableAccess;
begin
  result:=nil;
end;

function TRecordStorage.RecordWriteableAccess: IRecordWriteableAccess;
begin
  result:=nil;
end;

end.




