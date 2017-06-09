unit MediaStream.FrameQueue;

interface
uses
  Windows,SysUtils, Classes, SyncObjs, Generics.Collections, Collections.Lists,
  MediaProcessing.Definitions,MediaStream.Frame;

type
  TMediaStreamFrameQueue = class
  private
    FFreeItemsLists: array [TMediaType] of TLinkedList<TMediaStreamFrame>;
    FItems: TLinkedList<TMediaStreamFrame>;
    FLock : TCriticalSection;
    FMaxCount: integer;
    procedure SetMaxCount(const Value: integer);
    function FindAppropriateFreeItemToUse(aMT: TMediaType; aDataSize:cardinal; aInfoSize: cardinal): TMediaStreamFrame;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    procedure Add(aData: pointer; aDataSize:cardinal; const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal);
    function  ExtractFirstOrDefault: TMediaStreamFrame;

    procedure Clear;
    procedure FreeItem(aItem: TMediaStreamFrame);

    function  Count: integer;
    function  Length: cardinal; //длительность в мс

    property  MaxCount: integer read FMaxCount write SetMaxCount;
  end;

implementation

{ TMediaStreamFrameQueue }

procedure TMediaStreamFrameQueue.Add(aData: pointer; aDataSize: cardinal;
  const aFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal);
var
  aFrame: TMediaStreamFrame;
begin
  Lock;
  try
    while Count>=MaxCount do
      FreeItem(ExtractFirstOrDefault);

    aFrame:=FindAppropriateFreeItemToUse(aFormat.biMediaType, aDataSize,aInfoSize);
    if aFrame=nil then
      aFrame:=TMediaStreamFrame.Create(aFormat, aData,aDataSize,aInfo,aInfoSize)
    else
      aFrame.Assign(aFormat,aData,aDataSize,aInfo,aInfoSize);

    FItems.Add(aFrame);
  finally
    Unlock;
  end;
end;

procedure TMediaStreamFrameQueue.Clear;
var
  aItem: TLinkedListItem<TMediaStreamFrame>;
begin
  FLock.Enter;
  try
    aItem:=FItems.First;
    while aItem<>nil do
    begin
      FreeItem(aItem.Value);
      aItem:=FItems.Next(aItem);
    end;

    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TMediaStreamFrameQueue.Count: integer;
begin
  Lock;
  try
    result:=FItems.Size;
  finally
    Unlock;
  end;
end;

constructor TMediaStreamFrameQueue.Create;
var
  aMT: TMediaType;
begin
  FLock:=TCriticalSection.Create;
  FItems:=TLinkedList<TMediaStreamFrame>.Create;
  for aMT := Low(TMediaType) to High(TMediaType) do
    FFreeItemsLists[aMT]:=TLinkedList<TMediaStreamFrame>.Create;

  SetMaxCount(100);
end;

destructor TMediaStreamFrameQueue.Destroy;
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

function TMediaStreamFrameQueue.ExtractFirstOrDefault: TMediaStreamFrame;
begin
  self.Lock;
  try
    result:=nil;
    if Count>0 then
    begin
      result:=FItems.First.Value;
      FItems.Delete(FItems.First);
    end;
  finally
    self.Unlock;
  end;
end;

function TMediaStreamFrameQueue.FindAppropriateFreeItemToUse(aMT: TMediaType; aDataSize, aInfoSize: cardinal): TMediaStreamFrame;
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

procedure TMediaStreamFrameQueue.FreeItem(aItem: TMediaStreamFrame);
begin
  Assert(aItem<>nil);
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

function TMediaStreamFrameQueue.Length: cardinal;
var
  aStart,aStop: cardinal;
begin
  result:=0;

  Lock;
  try
    if Count>1 then
    begin
      aStart:=FItems.First.Value.Format.TimeStampMs;
      aStop:=FItems.Last.Value.Format.TimeStampMs;
      if aStart<aStop then
        result:=aStop-aStart;
    end;
  finally
    Unlock;
  end;
end;

procedure TMediaStreamFrameQueue.Lock;
begin
  FLock.Enter;
end;

procedure TMediaStreamFrameQueue.SetMaxCount(const Value: integer);
begin
  Assert(value>0);
  FLock.Enter;
  try
    FMaxCount := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TMediaStreamFrameQueue.Unlock;
begin
  FLock.Leave;
end;

end.
