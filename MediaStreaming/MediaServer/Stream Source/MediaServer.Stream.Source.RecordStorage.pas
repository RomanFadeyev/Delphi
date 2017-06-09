{***********************************<_INFO>************************************}
{  <Проект>      Медиа-сервер                                                  }
{                                                                              }
{  <Область>     16:Медиа-контроль                                             }
{                                                                              }
{  <Задача>      Медиа-источник, предоставляющий данные из медиа-архива        }
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

unit MediaServer.Stream.Source.RecordStorage;

interface
  uses Windows, SysUtils, Classes, SyncObjs,uBaseClasses,
  MediaServer.Stream.Source, MediaStream.Framer,
  MediaProcessing.Definitions,MediaStorage.RecordStorage, MediaServer.RecordStorage,
  MediaStorage.Transport,MediaServer.Configuration;

type
  //Класс, выполняющий непосредственно получение данных (видеопотока) из камеры
  TMediaServerSourceRecordStorage = class (TMediaServerSourceFinite)
  private
    FUDLFileName: string;
    FName: string;
    FPeriodStart,FPeriodEnd: TDateTime;
    FTransport: TRecordStorageTransportType;
    FReadThread : TThreadObjectVar<TThread>;
    FTransmitAudio : boolean;
    FFramerClass: TStreamFramerClass;
    FConnect_ThreadHandle: THandle;
    FLoop: boolean;

    procedure OnConnectionOKSync(aParams: pointer);
    procedure OnConnectionFailedSync(aParams: pointer);

    procedure OnFrameReceived(aMediaType: TMediaType;
                              aData: pointer; aDataSize:cardinal;
                              const aFormat: TMediaStreamDataHeader;
                              aInfo: pointer; aInfoSize: cardinal);
  protected
    function GetStreamType(aMediaType: TMediaType): TStreamType; override;
  public
    constructor Create(const aUDLFileName: string;
                       const aName: string;
                       const aPeriodStart,aPeriodEnd: TDateTime;
                       aTransport: TRecordStorageTransportType;
                       aTransmitAudio: boolean; //Записывать ли аудио
                       aLoop: boolean //Закольцевать проигрывание
                      ); overload;
    destructor Destroy; override;

    procedure DoOpen(aSync: boolean); override;
    procedure DoClose; override;

    procedure WaitWhileConnecting(aTimeout: integer); override;
    function  Opened: Boolean; override;

    //TMediaServerSourceFinite
    function  Duration: integer; override;
    function  Position: integer; override;
    function  Finished: boolean; override;
    //

    function RecordCount: integer;


    function Name: string; override;
    function DeviceType: string; override;
    function ConnectionString: string; override;
    function StreamInfo: TBytes; override;

    function PtzSupported: boolean; override;

    property Loop: boolean read FLoop write FLoop;
  end;

implementation
  uses Math,Forms,ThreadNames, MediaServer.Workspace,uTrace,uSync,MediaStream.Framer.Mp6;

type
  TOpenConnectionOkParams = record
    Records: TrsRecordObjectInfoArray;
  end;
  POpenConnectionOkParams = ^TOpenConnectionOkParams;

  TOpenConnectionFailedParams = record
    E: Exception;
  end;
  POpenConnectionFailedParams = ^TOpenConnectionFailedParams;

type
  TReadThread = class (TThread)
  private
    FRecords: TrsRecordObjectInfoArray;
    FOwner: TMediaServerSourceRecordStorage;
    FOpenLock: TCriticalSection;
    FPosition: integer;

    FStreamInfo: TBytes;
    function StreamInfo: TBytes;
  protected
    procedure ProcessFile(aReader: IRecordObjectReader; aDuration: integer);
    procedure Execute; override;
  public
    constructor Create(aOwner: TMediaServerSourceRecordStorage; aRecords: TrsRecordObjectInfoArray);
    destructor Destroy; override;
  end;

function GetReadThread(aThread : TThreadObjectVar<TThread>): TReadThread;
begin
  result:=aThread.Value as TReadThread;
  Assert(result<>nil);
end;

{ TReadThread }

constructor TReadThread.Create(aOwner: TMediaServerSourceRecordStorage;aRecords: TrsRecordObjectInfoArray);
begin
  FOwner:=aOwner;
  FOpenLock:=TCriticalSection.Create;
  FRecords:=aRecords;

  inherited Create(false);
end;

destructor TReadThread.Destroy;
begin
  inherited;
  FreeAndNil(FOpenLock);
end;

procedure TReadThread.ProcessFile(aReader: IRecordObjectReader; aDuration: integer);
const
  aMethodName = 'TReadThread.ProcessFile';
var
  aTicks: Cardinal;
  aDelay : int64;

  aData: pointer;
  aDataSize: cardinal;
  aInfo: pointer;
  aInfoSize: cardinal;
  aFormat: TMediaStreamDataHeader;

  aFramer: TStreamFramer; //Записывать ли аудио
  aStream : TStream;
  aFrameDuration: integer;
  i: integer;
  aTraceID: integer;
  aVideoFramesReaded: cardinal;
begin
  aTraceID:=TraceProcBegin(aMethodName);
  try
    aStream:=aReader.GetStream;
    aFramer:=FOwner.FFramerClass.Create;
    try
      aFramer.OpenStream(aStream);

      FOpenLock.Enter;
      try
        FStreamInfo:=aFramer.StreamInfo;
      finally
        FOpenLock.Leave;
      end;


      aFrameDuration:=40; //40 - это кол-во мсек на кадр
      aVideoFramesReaded:=0;

      if (aFramer is TStreamFramerMp6) then
      begin
        TStreamFramerMp6(aFramer).Reader.ReadIndexTable;
        i:=TStreamFramerMp6(aFramer).Reader.IndexTable.GetVideoFrameCount;
        if i>0 then
          aFrameDuration:=aDuration div i;
      end;

      while not Terminated do
      begin
        aTicks := GetTickCount;

        //----------- Читаем фрейм
        if not aFramer.GetNextFrame(aFormat,aData,aDataSize, aInfo,aInfoSize) then
          break;

        inc(FPosition,aFrameDuration);
        if FOwner<>nil then
          FOwner.OnFrameReceived(aFormat.biMediaType,aData,aDataSize,aFormat,aInfo,aInfoSize);

        aTicks := GetTickCount - aTicks;

        if aFormat.biMediaType=mtVideo then
        begin
          aDelay:=int64(aFrameDuration)-int64(aTicks);
          if aDelay<0 then
            aDelay:=0;
          Sleep(aDelay);
          inc(aVideoFramesReaded);
        end;
      end;

      TraceLine('Video Frames Readed: '+IntToStr(aVideoFramesReaded));
    finally
      aFramer.Free;
    end;
  finally
    TraceProcEnd(aMethodName,aTraceID);
  end;
end;

function TReadThread.StreamInfo: TBytes;
begin
  FOpenLock.Enter;
  try
    result:=FStreamInfo;
  finally
    FOpenLock.Leave;
  end;
end;

procedure TReadThread.Execute;
const
  aMethodName = 'TReadThread.Execute';
var
  i: Integer;
  aTraceID: integer;
  aReader : IRecordObjectReader;
begin
  SetCurrentThreadName('Source: MediaServer.Stream.Source.RecordStorage.'+ClassName);

  aTraceID:=TraceProcBegin(aMethodName);
  try
    while not Terminated do
    begin
      try
        FPosition:=0;
        for i := 0 to High(FRecords) do
        begin
          if Terminated then
            break;

          try
            aReader:=FRecords[i].Transport.GetReader;
            ProcessFile(aReader,
                        Round((FRecords[i].EndDateTime-FRecords[i].StartDateTime)*MSecsPerDay));
            aReader:=nil; //Удобно для отладки
          except
            on E:Exception do
            begin
              TraceLine('ERROR while reading '+FRecords[i].Id+': '+E.Message);
              WorkSpace.HandleException(self,E,aMethodName+'.1');
            end;
          end;
        end;

        if not FOwner.FLoop then
          break;

        sleep(50);
      except
        on E:Exception do
        begin
          WorkSpace.HandleException(self,E,aMethodName+'.2');
        end;
      end;
    end;
  finally
    TraceProcEnd(aMethodName, aTraceID);
  end;
end;

{ TMediaServerSourceRecordStorage }

constructor TMediaServerSourceRecordStorage.Create(const aUDLFileName: string;
                       const aName: string;
                       const aPeriodStart,aPeriodEnd: TDateTime;
                       aTransport: TRecordStorageTransportType;
                       aTransmitAudio: boolean;
                       aLoop: boolean);
begin
  Create(-1);
  FName:=aName;
  FUdlFileName:=aUDLFileName;
  FPeriodStart:=aPeriodStart;
  FPeriodEnd:=aPeriodEnd;
  FTransport:=aTransport;
  FTransmitAudio:=aTransmitAudio;
  FLoop:=aLoop;
  FReadThread:=TThreadObjectVar<TThread>.Create;

  FFramerClass:=TStreamFramerMp6; //TODO
end;

destructor TMediaServerSourceRecordStorage.Destroy;
begin
  inherited;
  FreeAndNil(FReadThread);
end;

function TMediaServerSourceRecordStorage.DeviceType: string;
begin
  result:='ФХ';
end;

function TMediaServerSourceRecordStorage.Name: string;
begin
  result:=FName;
end;

procedure TMediaServerSourceRecordStorage.OnConnectionFailedSync(aParams: pointer);
begin
  //Добавим себя в список очередников на повторное соединение
  StartReconnect;

  if Assigned(OnConnectionFailed) then
    OnConnectionFailed(self,POpenConnectionFailedParams(aParams).E);
end;

procedure TMediaServerSourceRecordStorage.OnConnectionOKSync(aParams: pointer);
begin
  Assert(FReadThread.Value=nil);
  FReadThread.Value:=TReadThread.Create(self,POpenConnectionOkParams(aParams).Records);

  if Assigned(OnConnectionOk) then
    OnConnectionOk(self);
end;

procedure TMediaServerSourceRecordStorage.OnFrameReceived(aMediaType: TMediaType;
                                  aData: pointer; aDataSize:cardinal;
                                  const aFormat: TMediaStreamDataHeader;
                                  aInfo: pointer; aInfoSize: cardinal);
begin
  //Если не нужно записывать аудио данные, то выходим
  if not FTransmitAudio and (aMediaType=mtAudio) then
    exit;

  DoDataReceived(aFormat, aData,aDataSize, aInfo,aInfoSize);
end;

function ThreadConnectionProc(aRecordSource: TMediaServerSourceRecordStorage): Integer;
var
  aOpenOk : TOpenConnectionOkParams;
  aOpenFailed: TOpenConnectionFailedParams;

  aRecordStorage: TRecordStorage;
begin
  result:=-1;

  if not (aRecordSource.Destroying or aRecordSource.Closing) then
    if (gLastCommand=rslcStart) then
      if not Application.Terminated then
      begin
        result:=0;
      end;

  if result<>0 then
    exit;

  ZeroMemory(@aOpenOk,sizeof(aOpenOk));

  //открываемся
  try
    // открываем канал
    aRecordStorage:=TRecordStorageProvider.CreateStorage(aRecordSource.FUDLFileName,aRecordSource.FTransport);
    try
      aOpenOk.Records:=aRecordStorage.GetRecords(
        aRecordStorage.GetRecordSourceByName(aRecordSource.FName).Id,
        aRecordSource.FPeriodStart,
        aRecordSource.FPeriodEnd);
    finally
      aRecordStorage.Free;
    end;
  except
    on E:Exception do
    begin
      aOpenFailed.E:=E;
      Sync.Synchronize(aRecordSource.OnConnectionFailedSync,@aOpenFailed);
      exit;
    end;
  end;

  Sync.Synchronize(aRecordSource.OnConnectionOkSync,@aOpenOk);
  aRecordSource.FConnect_ThreadHandle:=0;
end;

procedure TMediaServerSourceRecordStorage.DoOpen(aSync: boolean);
var
  aThreadID: cardinal;
begin
  if Opened then
    exit;

  Close;

  if aSync then
    ThreadConnectionProc(self)
  else
    FConnect_ThreadHandle:=BeginThread(nil, 0, @ThreadConnectionProc, self, 0, aThreadID);
end;

procedure TMediaServerSourceRecordStorage.DoClose;
begin
  //Вычистим очередь
  if FConnect_ThreadHandle<>0 then
  begin
    Sync.PeekSynchronizationMessages;
    Sync.WaitWhileSynchronizationMessagesProcessed(FConnect_ThreadHandle);
    FConnect_ThreadHandle:=0;
  end;

  FReadThread.FreeValue;
end;

function TMediaServerSourceRecordStorage.Duration: integer;
var
  aRecords: TrsRecordObjectInfoArray;
  i: integer;
begin
  if FLoop then
    result:=-1
  else if FReadThread.Value=nil then
    result:=-1
  else begin
    aRecords:=GetReadThread(FReadThread).FRecords;
    result:=0;
    for i := 0 to High(aRecords) do
      inc(result,Round((aRecords[i].EndDateTime-aRecords[i].StartDateTime)*MSecsPerDay));
  end;
end;

function TMediaServerSourceRecordStorage.Finished: boolean;
begin
  result:=(FReadThread.Value<>nil) and (FReadThread.Value.Finished);
end;

function TMediaServerSourceRecordStorage.ConnectionString: string;
begin
  result:=ExtractFileName(FUDLFileName)+': '+FName;
end;

function TMediaServerSourceRecordStorage.Opened: Boolean;
begin
  result:=FReadThread.Value<>nil;
end;

function TMediaServerSourceRecordStorage.StreamInfo: TBytes;
begin
  FReadThread.Lock;
  try
    CheckConnected;
    result:=GetReadThread(FReadThread).StreamInfo;
  finally
    FReadThread.Unlock;
  end;
end;

function TMediaServerSourceRecordStorage.GetStreamType(aMediaType: TMediaType): TStreamType;
begin
  case aMediaType of
    mtVideo: result:=stHHVI;
    mtAudio: result:=stHHAU;
    else
      raise Exception.Create('Неизвестный тип медиа');
  end;
end;

function TMediaServerSourceRecordStorage.Position: integer;
begin
  result:=0;
  if FReadThread.Value<>nil then
    result:=GetReadThread(FReadThread).FPosition;
end;

function TMediaServerSourceRecordStorage.PtzSupported: boolean;
begin
  result:=false;
end;

function TMediaServerSourceRecordStorage.RecordCount: integer;
begin
  result:=0;
  if FReadThread<>nil then
    result:=Length(GetReadThread(FReadThread).FRecords);
end;

procedure TMediaServerSourceRecordStorage.WaitWhileConnecting(aTimeout: integer);
begin
  inherited;
  WaitForSingleObject(FConnect_ThreadHandle,aTimeout);
end;

end.



