unit MediaStream.FramePool;

interface
uses
  Windows,SysUtils, Classes, SyncObjs, Generics.Collections, Collections.Lists,
  MediaProcessing.Definitions,MediaStream.Frame;

type
  TMediaStreamFramePool = class
  private
    FFreeItemsLists: array [TMediaType] of TLinkedList<TMediaStreamFrame>;
    FItems: TList<TMediaStreamFrame>;
    FLock : TCriticalSection;
    FMaxCount: integer;
    procedure SetMaxCount(const Value: integer);
    function GetItems(index: integer): TMediaStreamFrame;

    function FindAppropriateFreeItemToUse(aMT: TMediaType; aDataSize:cardinal; aInfoSize: cardinal): TMediaStreamFrame;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function  Add(aData: pointer; aDataSize:cardinal; const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal):boolean;
    procedure AddAsQueue(aData: pointer; aDataSize:cardinal; const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal);

    function  ExtractFirstOrDefault: TMediaStreamFrame;

    //Удаляет с начала очереди фреймы, пока не доберется до I-фрейма
    function  DeleteUpToIFrame: integer;

    //Ищет I-frame с конца
    function  FindIFrameBackward: integer;


    procedure Delete(index: integer);
    procedure DeleteRange(aStartIndex,aCount: integer);
    procedure Clear;
    procedure FreeItem(aItem: TMediaStreamFrame);

    function  Count: integer;
    function  Length: cardinal; //длительность в мс

    property  Items[index: integer]: TMediaStreamFrame read GetItems;

    property  MaxCount: integer read FMaxCount write SetMaxCount;
  end;

implementation
  uses Math;
{ TMediaStreamFramePool }
{
procedure TMediaStreamFramePool.CheckTimeStampOrder;
var
  aTs: int64;
  i: Integer;
begin
  FLock.Enter;
  try
    if FItems.Count=0 then
      exit;

    aTs:=FItems[0].Format.TimeStamp;
    for i := 1 to FItems.Count-1 do
    begin
      if aTs>FItems[i].Format.TimeStamp then
        raise Exception.Create('Timestamp sequence order error');

      aTs:=FItems[i].Format.TimeStamp;
    end;

  finally
    FLock.Leave;
  end;
end;
}
procedure TMediaStreamFramePool.AddAsQueue(aData: pointer; aDataSize: cardinal;
  const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal);
begin
  self.Lock;
  try
    while Count>=MaxCount do
      Delete(0);
    Add(aData,aDataSize,aFormat,aInfo,aInfoSize)
  finally
    self.Unlock;
  end;
end;

procedure TMediaStreamFramePool.Clear;
var
  i: Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FItems.Count-1 do
    begin
      FreeItem(FItems[i]);
      FItems[i]:=nil;
    end;

    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TMediaStreamFramePool.Count: integer;
begin
  Lock;
  try
    result:=FItems.Count;
  finally
    Unlock;
  end;
end;

constructor TMediaStreamFramePool.Create;
var
  aMT: TMediaType;
begin
  FLock:=TCriticalSection.Create;
  FItems:=TList<TMediaStreamFrame>.Create;
  for aMT := Low(TMediaType) to High(TMediaType) do
    FFreeItemsLists[aMT]:=TLinkedList<TMediaStreamFrame>.Create;
end;

procedure TMediaStreamFramePool.Delete(index: integer);
var
  aItem: TMediaStreamFrame;
begin
  self.Lock;
  try
    aItem:=FItems[index];
    FItems.Delete(index);
    FreeItem(aItem);
  finally
    self.Unlock;
  end;
end;

procedure TMediaStreamFramePool.DeleteRange(aStartIndex,aCount: integer);
var
  i: Integer;
begin
  self.Lock;
  try
    for I := Min(aStartIndex+aCount-1,Count-1) downto aStartIndex do
      Delete(i);
  finally
    self.Unlock;
  end;
end;

function TMediaStreamFramePool.DeleteUpToIFrame: integer;
begin
  result:=0;
  Lock;
  try
    //Удаляем все не опорные кадры, потому что они больше не имеют смысла - будут кубики
    while Count>0 do
    begin
      if not (ffKeyFrame in  Items[0].Format.biFrameFlags) then
      begin
        Delete(0);
        inc(result);
      end
      else
        break;
    end;
  finally
    Unlock;
  end;
end;

destructor TMediaStreamFramePool.Destroy;
var
  aMT: TMediaType;
  aItem: TLinkedListItem<TMediaStreamFrame>;
begin
  Clear;

  //Очищаем кэш
  for aMT := Low(TMediaType) to High(TMediaType) do
  begin
    aItem:=FFreeItemsLists[aMT].First;
    while aItem<>nil do
    begin
      FreeAndNil(aItem.Value);
      aItem:=FFreeItemsLists[aMT].Next(aItem);
    end;
    FreeAndNil(FFreeItemsLists[aMT]);
  end;

  FreeAndNil(FItems);

  FreeAndNil(FLock);

  inherited;
end;

function TMediaStreamFramePool.ExtractFirstOrDefault: TMediaStreamFrame;
begin
  self.Lock;
  try
    result:=nil;
    if Count>0 then
    begin
      result:=FItems[0];
      FItems.Delete(0);
    end;
  finally
    self.Unlock;
  end;
end;

function TMediaStreamFramePool.FindAppropriateFreeItemToUse(aMT: TMediaType; aDataSize, aInfoSize: cardinal): TMediaStreamFrame;
var
  aItem: TLinkedListItem<TMediaStreamFrame>;
begin
  result:=nil;

  aItem:=FFreeItemsLists[aMT].First;
  while aItem<>nil do
  begin
    if (aItem.Value.DataAllocatedBlockSize>=aDataSize) and (aItem.Value.InfoAllocatedBlockSize>=aInfoSize) then
    begin
      result:=aItem.Value;
      FFreeItemsLists[aMT].Delete(aItem);
      exit;
    end;

    aItem:=FFreeItemsLists[aMT].Next(aItem);
  end;

  //Если ничего не нашли, то применим первую попавшуюся
  if FFreeItemsLists[aMT].First<>nil then
  begin
    result:=FFreeItemsLists[aMT].First.Value;
    FFreeItemsLists[aMT].Delete(FFreeItemsLists[aMT].First);
  end;
end;

function TMediaStreamFramePool.FindIFrameBackward: integer;
var
  i: Integer;
begin
  result:=-1;
  Lock;
  try
    for i := Count-1 downto 0 do
    begin
      if (ffKeyFrame in  Items[i].Format.biFrameFlags) then
      begin
        result:=i;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TMediaStreamFramePool.FreeItem(aItem: TMediaStreamFrame);
begin
  self.Lock;
  try
    if FFreeItemsLists[aItem.Format.biMediaType].Size<FMaxCount div 10 then //10%
    begin
      FFreeItemsLists[aItem.Format.biMediaType].Add(aItem);
    end
    else begin
      FreeAndNil(aItem);
    end;
  finally
    self.Unlock;
  end;
end;

function TMediaStreamFramePool.GetItems(index: integer): TMediaStreamFrame;
begin
  result:=FItems[index];
end;

function TMediaStreamFramePool.Length: cardinal;
var
  aStart,aStop: cardinal;
begin
  result:=0;

  Lock;
  try
    if Count>1 then
    begin
      aStart:=Items[0].Format.TimeStampMs;
      aStop:=Items[Count-1].Format.TimeStampMs;
      if aStart<aStop then
        result:=aStop-aStart;
    end;
  finally
    Unlock;
  end;
end;

procedure TMediaStreamFramePool.Lock;
begin
  FLock.Enter;
end;

function TMediaStreamFramePool.Add(aData: pointer; aDataSize:cardinal; const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal):boolean;
var
  aFrame: TMediaStreamFrame;
begin
  FLock.Enter;
  try
    result:=false;
    if FItems.Count<FMaxCount then
    begin
      aFrame:=FindAppropriateFreeItemToUse(aFormat.biMediaType, aDataSize,aInfoSize);
      if aFrame=nil then
        aFrame:=TMediaStreamFrame.Create(aFormat, aData,aDataSize,aInfo,aInfoSize)
      else
        aFrame.Assign(aFormat,aData,aDataSize,aInfo,aInfoSize);

      FItems.Add(aFrame);
      result:=true;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaStreamFramePool.SetMaxCount(const Value: integer);
begin
  FLock.Enter;
  try
    FMaxCount := Value;
    FItems.Capacity:=FMaxCount;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaStreamFramePool.Unlock;
begin
  FLock.Leave;
end;

end.
