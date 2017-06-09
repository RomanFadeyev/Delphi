{***********************************<_INFO>************************************}
{  <Проект>      Библиотека медиа-обработки                                    }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Инкапсуляция медиа-фрейма. Класс, обеспечивающий выделение и  }
{                управление блоком памяти                                      }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        14.01.2011                                                    }
{                                                                              }
{  <Примечание>  Нет примечаний.                                               }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
{                                                                              }
{***********************************</_INFO>***********************************}
unit MediaStream.Frame;

interface
uses
  SysUtils, Windows, MediaProcessing.Definitions;

type
  TMediaStreamFrame =class
  private
    FDataPtr:pointer;
    FDataSize: cardinal;
    FDataAllocatedSize: cardinal;

    FInfoPtr:pointer;
    FInfoSize: cardinal;
    FInfoAllocatedSize: cardinal;

    FFormat: TMediaStreamDataHeader;

    procedure DisposeMemory;
  public
    constructor Create(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize:cardinal; aInfo: pointer; aInfoSize: cardinal);
    destructor Destroy; override;

    procedure Assign(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize:cardinal; aInfo: pointer; aInfoSize: cardinal; aReuseMemory: boolean=true);

    property Format: TMediaStreamDataHeader read FFormat;

    property DataPtr: pointer read FDataPtr;
    property DataSize: cardinal read FDataSize;
    property DataAllocatedBlockSize: cardinal read FDataAllocatedSize;

    property InfoPtr: pointer read FInfoPtr;
    property InfoSize: cardinal read FInfoSize;
    property InfoAllocatedBlockSize: cardinal read FInfoAllocatedSize;
  end;

implementation

{ TMediaStreamFrame }

destructor TMediaStreamFrame.Destroy;
begin
  inherited;
  DisposeMemory;
end;

procedure TMediaStreamFrame.DisposeMemory;
begin
  FreeMem(FDataPtr);
  FDataPtr:=nil;
  FDataAllocatedSize:=0;
  FDataSize:=0;

  FreeMem(FInfoPtr);
  FInfoPtr:=nil;
  FInfoAllocatedSize:=0;
  FInfoSize:=0;
end;

procedure TMediaStreamFrame.Assign(const aFormat: TMediaStreamDataHeader;
  aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal;
  aReuseMemory: boolean);
begin
  if not aReuseMemory then
    DisposeMemory;

  FFormat:=aFormat;

  if aDataSize>0 then
  begin
    //Если размер уже выделенного блока памяти мал, то нужно выделить блок памяти заново
    if FDataAllocatedSize<aDataSize then
    begin
      FreeMem(FDataPtr);
      FDataAllocatedSize:=0;
      FDataSize:=0;

      GetMem(FDataPtr,aDataSize);
      FDataAllocatedSize:=aDataSize;
    end;

    FDataSize:=aDataSize;
    CopyMemory(FDataPtr,aData,aDataSize);

  end;

  if aInfoSize>0 then
  begin
    //Если размер уже выделенного блока памяти мал, то нужно выделить блок памяти заново
    if FInfoAllocatedSize<aInfoSize then
    begin
      FreeMem(FInfoPtr);
      FInfoSize:=0;
      FInfoAllocatedSize:=0;

      GetMem(FInfoPtr,aInfoSize);
      FInfoAllocatedSize:=aInfoSize;
    end;
    FInfoSize:=aInfoSize;
    CopyMemory(FInfoPtr,aInfo,aInfoSize);
  end;

end;

constructor TMediaStreamFrame.Create(const aFormat: TMediaStreamDataHeader;  aData: pointer; aDataSize:cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  Assign(aFormat,aData,aDataSize,aInfo,aInfoSize);
end;

end.
