{-------------------------------------------------------------------------------

   Сохранение видео/аудио потока на диск

 -------------------------------------------------------------------------------}
unit MediaStream.Writer.Tsm;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

interface
  uses Windows, SysUtils, Classes, SyncObjs, Graphics, Messages,Controls,MediaProcessing.Definitions,TsmFile;

type
  TMediaStreamWriter_Tsm = class
  private
    FStream: TStream;
    FOwnStream : boolean;
    FFileName: string;

    FHeader  : TTsmHeader;

    FIndexTable : array of TTsmIndexTableItem;
    FIndexTableLength : integer;

    procedure GrowIndexTable;

    procedure CompleteWriting;
    procedure UpdateHeader;

    procedure CheckOpened;

    procedure InternalWriteData(const aFormat: TMediaStreamDataHeader;aData: pointer; aDataSize: cardinal);
  public
    //Открыть файл для записи
    procedure Open(const aFileName: string);
    function  Opened: boolean;

    //Ассоциировать себя с потоком
    procedure AssignToStream(aStream: TStream);

    procedure EnableVideo(aStreamType: TStreamType; aStreamSubType: DWORD; aWidth: DWORD; aHeight: DWORD; aBitCount: Word);
    procedure EnableAudio(aStreamType: TStreamType; aStreamSubType: DWORD; aChannels: Byte; aBitsPerSample: Word; aSamplesPerSec: DWORD);

    //Закрыть файл
    procedure Close;

    procedure WriteData(const aFormat: TMediaStreamDataHeader;aData: pointer; aDataSize: cardinal);
    procedure CheckDataCompartibility(const aFormat: TMediaStreamDataHeader;aData: pointer; aDataSize: cardinal);

    constructor Create; overload;
    constructor Create (const aFileName: string); overload;
    constructor Create (const aStream: TStream); overload;

    destructor Destroy; override;

    property Stream: TStream read FStream;

    function VideoFramesWrittenCount :int64;
    function AudioFramesWrittenCount :int64;
  end;

implementation
  uses Math,Forms;


function TMediaStreamWriter_Tsm.Opened: boolean;
begin
  result:=FStream<>nil;
end;

procedure TMediaStreamWriter_Tsm.UpdateHeader;
var
  aPos: int64;
begin
  aPos:=FStream.Position;
  FStream.Seek(0,soFromBeginning);
  FStream.WriteBuffer(FHeader,sizeof(FHeader));
  FStream.Seek(aPos,soFromBeginning);
end;

function TMediaStreamWriter_Tsm.VideoFramesWrittenCount: int64;
begin
  result:=FHeader.VideoFrameCount;
end;

{ TMediaStreamWriter_Tsm }

constructor TMediaStreamWriter_Tsm.Create;
begin
  inherited;
end;

constructor TMediaStreamWriter_Tsm.Create(const aFileName: string);
begin
  Create;
  Open(aFileName);
end;

constructor TMediaStreamWriter_Tsm.Create(const aStream: TStream);
begin
  Create;
  AssignToStream(aStream);
end;

destructor TMediaStreamWriter_Tsm.Destroy;
begin
  Close;
  inherited;
end;

procedure TMediaStreamWriter_Tsm.EnableAudio(aStreamType: TStreamType;
  aStreamSubType: DWORD; aChannels: Byte; aBitsPerSample: Word;
  aSamplesPerSec: DWORD);
begin
  if (FHeader.AudioStreamType=aStreamType) and
     (FHeader.AudioStreamSubType=aStreamSubType) and
     (FHeader.AudioChannels=aChannels) and
     (FHeader.AudioBitsPerSample=aBitsPerSample) and
     (FHeader.AudioSamplesPerSec=aSamplesPerSec) then
  exit;

  if (FHeader.AudioFrameCount<>0) then
    raise Exception.Create('Не допускается смена формата после начала записи');

  FHeader.AudioStreamType:=aStreamType;
  FHeader.AudioStreamSubType:=aStreamSubType;
  FHeader.AudioChannels:=aChannels;
  FHeader.AudioBitsPerSample:=aBitsPerSample;
  FHeader.AudioSamplesPerSec:=aSamplesPerSec;
end;

procedure TMediaStreamWriter_Tsm.EnableVideo(aStreamType: TStreamType;
  aStreamSubType, aWidth, aHeight: DWORD; aBitCount: Word);
begin
  if (FHeader.VideoStreamType=aStreamType) and
     (FHeader.VideoStreamSubType=aStreamSubType) and
     (FHeader.VideoWidth=aWidth) and
     (FHeader.VideoHeight=aHeight) and
     (FHeader.VideoBitCount=aBitCount) then
  exit;

  if (FHeader.VideoFrameCount<>0) then
    raise Exception.Create('Не допускается смена формата после начала записи');

  FHeader.VideoStreamType:=aStreamType;
  FHeader.VideoStreamSubType:=aStreamSubType;
  FHeader.VideoWidth:=aWidth;
  FHeader.VideoHeight:=aHeight;
  FHeader.VideoBitCount:=aBitCount;
end;

procedure TMediaStreamWriter_Tsm.GrowIndexTable;
begin
  if Length(FIndexTable)=FIndexTableLength then
    SetLength(FIndexTable,Length(FIndexTable)+25*60*20); //20 Минут

  Assert(Length(FIndexTable)>FIndexTableLength);
end;

procedure TMediaStreamWriter_Tsm.InternalWriteData(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal);
var
  aTimeStamp: int64;
  aFrameHeader: TTsmFrameHeader;
  aExtension: TTsmFrameHeaderExtension;
begin
  CheckDataCompartibility(aFormat,aData,aDataSize);

  GrowIndexTable;
  aTimeStamp:=aFormat.TimeStamp*aFormat.TimeKoeff;

  aFrameHeader.Marker:=0;
  aExtension.StreamType:=aFormat.biStreamType;
  aExtension.StreamSubType:=aFormat.biStreamSubType;
  aExtension.Channel:=aFormat.Channel;
  aExtension.TimestampHigh:=(aTimeStamp shr 32);
  aExtension.Tag:=aFormat.Tag;

  if aFormat.biMediaType=mtVideo then
  begin
    if (FHeader.VideoFrameCount>0) and (aTimeStamp<FHeader.VideoFirstTimeStamp) then
      raise Exception.CreateFmt('Нарушен порядок следования временных отсчетов Video. Первый отсчет в файле=%d; Переданный отсчет=%d',[FHeader.VideoFirstTimeStamp,aTimeStamp]);

    FIndexTable[FIndexTableLength].SetDataTypeAndKey(TsmDataTypeVideo,ffKeyFrame in aFormat.biFrameFlags);
    FIndexTable[FIndexTableLength].TimestampOffs:=aTimeStamp-FHeader.VideoFirstTimeStamp;
    FIndexTable[FIndexTableLength].StreamPosition:=FStream.Position;
    inc(FHeader.VideoFrameCount);
  end
  else if aFormat.biMediaType=mtAudio then
  begin
    if (FHeader.AudioFrameCount>0) and (aTimeStamp<FHeader.AudioFirstTimeStamp) then
      raise Exception.CreateFmt('Нарушен порядок следования временных отсчетов Audio. Первый отсчет в файле=%d; Переданный отсчет=%d',[FHeader.AudioFirstTimeStamp,aTimeStamp]);

    FIndexTable[FIndexTableLength].SetDataTypeAndKey(TsmDataTypeAudio,ffKeyFrame in aFormat.biFrameFlags);
    FIndexTable[FIndexTableLength].TimestampOffs:=aTimeStamp-FHeader.AudioFirstTimeStamp;
    FIndexTable[FIndexTableLength].StreamPosition:=FStream.Position;
    inc(FHeader.AudioFrameCount);
  end
  else if aFormat.biMediaType=mtSysData then
  begin
    //if (FHeader.AudioFrameCount>0) and (aTimeStamp<FHeader.AudioFirstTimeStamp) then
    //   raise Exception.CreateFmt('Нарушен порядок следования временных отсчетов Audio. Первый отсчет в файле=%d; Переданный отсчет=%d',[FHeader.AudioFirstTimeStamp,aTimeStamp]);

    FIndexTable[FIndexTableLength].SetDataTypeAndKey(TsmDataTypeSysData,ffKeyFrame in aFormat.biFrameFlags);
    FIndexTable[FIndexTableLength].TimestampOffs:=aTimeStamp;//-FHeader.AudioFirstTimeStamp;
    FIndexTable[FIndexTableLength].StreamPosition:=FStream.Position;
    inc(FHeader.SysDataFrameCount);

    aFrameHeader.Marker:=aFrameHeader.Marker or $80;
  end

  else
    raise Exception.Create('Неизвестный тип медиа');

  aFrameHeader.DataTypeAndKey:=FIndexTable[FIndexTableLength].DataTypeAndKey;
  aFrameHeader.TimestampOffs:=FIndexTable[FIndexTableLength].TimestampOffs;
  aFrameHeader.DataSize:=aDataSize;
  aFrameHeader.PrevFrameOffset:=0;
  aFrameHeader.IndexTablePos:=FIndexTableLength;
  if FIndexTableLength>0 then
    aFrameHeader.PrevFrameOffset:=FIndexTable[FIndexTableLength].StreamPosition-FIndexTable[FIndexTableLength-1].StreamPosition;

  inc(FIndexTableLength);

  FStream.WriteBuffer(aFrameHeader,sizeof(aFrameHeader));

  if aFrameHeader.Marker and $80 <>0 then
    FStream.WriteBuffer(aExtension,sizeof(aExtension));

  FStream.WriteBuffer(aData^,aDataSize);
  inc(FHeader.TotalFrameCount);
end;

procedure TMediaStreamWriter_Tsm.AssignToStream(aStream: TStream);
begin
  Close;

  FStream:=aStream;
  FOwnStream:=false;
  //Заголовок
  FStream.WriteBuffer(FHeader,sizeof(FHeader));
end;

procedure TMediaStreamWriter_Tsm.Close;
begin
  if Opened then
    CompleteWriting;

  if FOwnStream then
    FreeAndNil(FStream)
  else
    FStream:=nil;

  FFileName:='';

  SetLength(FIndexTable,0);
  FIndexTableLength:=0;
  FHeader.VideoFrameCount:=0;
  FHeader.AudioFrameCount:=0;
  FHeader.SysDataFrameCount:=0;

  ZeroMemory(@FHeader,sizeof(FHeader));
  FHeader.Signature:=TsmSignature;
  FHeader.Version:=1;
end;

procedure TMediaStreamWriter_Tsm.Open(const aFileName: string);
begin
  Close;

  FFileName:=aFileName;

  FStream:=TFileStream.Create(aFileName,fmCreate);
  FOwnStream:=true;

  //Заголовок
  FStream.WriteBuffer(FHeader,sizeof(FHeader));
end;

procedure TMediaStreamWriter_Tsm.WriteData(const aFormat: TMediaStreamDataHeader;aData: pointer; aDataSize: cardinal);
var
  aStreamType: TStreamType;
  aTimeStamp: int64;
begin
  CheckOpened;

  aTimeStamp:=aFormat.TimeStamp*aFormat.TimeKoeff;

  if aFormat.biMediaType=mtVideo then
  begin
    Assert(aFormat.biStreamType<>0);
    aStreamType:=FHeader.VideoStreamType;
    EnableVideo(aFormat.biStreamType,aFormat.biStreamSubType,aFormat.VideoWidth,aFormat.VideoHeight,aFormat.VideoBitCount);

    //Первым должен быть опорный видео-кадр
    if (FHeader.VideoFrameCount>0) or (ffKeyFrame in aFormat.biFrameFlags) then
    begin
      FHeader.VideoLastTimeStamp:=aTimeStamp;

      //В случае аварийного заверешения записи эта информация поможет нам легче восстановить файл
      if (aStreamType=0) or (FHeader.VideoFrameCount=0) then
      begin
        Assert(FHeader.VideoStreamType<>0);
        if FHeader.VideoFrameCount=0 then
          FHeader.VideoFirstTimeStamp:=aTimeStamp;
        UpdateHeader;
      end;

      InternalWriteData(aFormat,aData,aDataSize);
    end;
  end

  else if aFormat.biMediaType=mtAudio then
  begin
    Assert(aFormat.biStreamType<>0);
    aStreamType:=FHeader.AudioStreamType;
    EnableAudio(aFormat.biStreamType,aFormat.biStreamSubType,aFormat.AudioChannels,aFormat.AudioBitsPerSample,aFormat.AudioSamplesPerSec);
    FHeader.AudioLastTimeStamp:=aTimeStamp;
    //Первым должен быть опорный кадр
    //if (FHeader.AudioFrameCount>0) or (ffKeyFrame in aFormat.biFrameFlags) then
    begin

      //В случае аварийного заверешения записи эта информация поможет нам легче восстановить файл
      if (aStreamType=0) or (FHeader.AudioFrameCount=0) then
      begin
        Assert(FHeader.AudioStreamType<>0);
        if FHeader.AudioFrameCount=0 then
          FHeader.AudioFirstTimeStamp:=aTimeStamp;
        UpdateHeader;
      end;

      InternalWriteData(aFormat,aData,aDataSize);
    end;
  end

  else if aFormat.biMediaType=mtSysData then
  begin
    InternalWriteData(aFormat,aData,aDataSize);
    
  end;
end;

procedure TMediaStreamWriter_Tsm.CompleteWriting;
var
  aFrameHead : TTsmFrameHeader;
  aIndexTableSize : cardinal;
  aIndexTablePosition: int64;
begin
  CheckOpened;

  //Записываем индексную таблицу
  aIndexTableSize:=FIndexTableLength*sizeof(TTsmIndexTableItem);

  ZeroMemory(@aFrameHead,sizeof(aFrameHead));
  aFrameHead.Marker:=0;
  aFrameHead.DataSize:=aIndexTableSize;
  aFrameHead.SetDataTypeAndKey(TsmDataTypeIndexTable,false);

  aIndexTablePosition:=FStream.Position;
  //Заголовок
  FStream.WriteBuffer(aFrameHead,sizeof(aFrameHead));
  //Данные
  FStream.WriteBuffer(FIndexTable[0],aIndexTableSize);

  //Корректируем заголовок
  FHeader.Valid:=true;
  FHeader.IndexTableStreamPosition:=aIndexTablePosition;
  FStream.Seek(0,soFromBeginning);

  //Обновляем StreamInfo
  UpdateHeader;
end;

function TMediaStreamWriter_Tsm.AudioFramesWrittenCount: int64;
begin
  result:=FHeader.AudioFrameCount;
end;

procedure TMediaStreamWriter_Tsm.CheckDataCompartibility(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal);
var
  aTimeStamp: int64;
begin
  aTimeStamp:=aFormat.TimeStamp*aFormat.TimeKoeff;

  if (aFormat.biMediaType=mtVideo) and (FHeader.VideoFrameCount>0) then
  begin
    if aTimeStamp<FHeader.VideoFirstTimeStamp then
      raise Exception.CreateFmt('Нарушен порядок следования временных отсчетов Video. Первый отсчет в файле=%d; Переданный отсчет=%d',[FHeader.VideoFirstTimeStamp,aTimeStamp]);
  end
  else if (aFormat.biMediaType=mtAudio) and (FHeader.AudioFrameCount>0) then
  begin
    if aTimeStamp<FHeader.AudioFirstTimeStamp then
      raise Exception.CreateFmt('Нарушен порядок следования временных отсчетов Video. Первый отсчет в файле=%d; Переданный отсчет=%d',[FHeader.AudioFirstTimeStamp,aTimeStamp]);
  end
end;

procedure TMediaStreamWriter_Tsm.CheckOpened;
begin
  if not Opened then
    raise Exception.Create('Записыватель еще не инициализирован');
end;


end.

