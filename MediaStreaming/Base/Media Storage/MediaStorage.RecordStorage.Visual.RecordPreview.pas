unit MediaStorage.RecordStorage.Visual.RecordPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmForm_B,Player.Control,Player.VideoOutput.Base,VideoPane.Monitor,
  ExtCtrls,MediaProcessing.Definitions,SyncObjs,MediaStream.Framer,
  StdCtrls, ExtendControls, uTimeBar,MediaStorage.RecordStorage;

type
  TfmMediaStorageRecordPreview = class(TfmForm_B)
    paScreen: TPanel;
    paMonitor: TPanel;
    paTime: TTimeScalePanel;
    Panel1: TPanel;
    edProperties: TExtendMemo;
    ckTimeBar: TExtendCheckBox;
    procedure ckTimeBarClick(Sender: TObject);
  private
    FPlayerMonitor: TfrmVideoPaneMonitor;
    FPlayer: TWindowStreamPlayer;
    FReadThread : TThread;

    procedure OnFrameReceived(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);

    procedure FillWithRecords(const aRecords: TrsRecordObjectInfoArray);

    constructor Create; reintroduce; overload;
  public
    constructor Create(aRecordStorage: TRecordStorage; const aSourceName: string; const aStart,aEnd: TDateTime); reintroduce; overload;
    constructor Create(const aRecords: TrsRecordObjectInfoArray); reintroduce; overload;

    destructor Destroy; override;

    class procedure Run(aRecordStorage: TRecordStorage; const aSourceName: string; const aStart,aEnd: TDateTime); overload;
    class procedure Run(const aRecords: TrsRecordObjectInfoArray); overload;
  end;


implementation
  uses VclUtils,
       Player.VideoOutput.AllTypes,
       Player.AudioOutput.AllTypes,
       MediaStorage.Transport,
       MediaStream.Framer.Mp6,
       MediaStream.FramerFactory;
{$R *.dfm}

type
  TReadThread = class (TThread)
  private
    FRecords: TrsRecordObjectInfoArray;
    FOwner: TfmMediaStorageRecordPreview;
    FPosition: integer;
  protected
    procedure ProcessFile(aReader: IRecordObjectReader; aFramerClass: TStreamFramerClass; aDuration: integer);
    procedure Execute; override;
  public
    constructor Create(aOwner: TfmMediaStorageRecordPreview; aRecords: TrsRecordObjectInfoArray);
    destructor Destroy; override;
  end;

{ TReadThread }

constructor TReadThread.Create(aOwner: TfmMediaStorageRecordPreview;aRecords: TrsRecordObjectInfoArray);
begin
  FOwner:=aOwner;
  FRecords:=aRecords;

  inherited Create(false);
end;

destructor TReadThread.Destroy;
begin
  inherited;
end;

procedure TReadThread.ProcessFile(aReader: IRecordObjectReader; aFramerClass: TStreamFramerClass; aDuration: integer);
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
begin
  aStream:=aReader.GetStream;

  aFramer:=aFramerClass.Create;
  try
    aFramer.OpenStream(aStream);


    aFrameDuration:=40; //40 - это кол-во мсек на кадр

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
        FOwner.OnFrameReceived(aFormat,aData,aDataSize,aInfo,aInfoSize);

      aTicks := GetTickCount - aTicks;

      if aFormat.biMediaType=mtVideo then
      begin
        aDelay:=int64(aFrameDuration)-int64(aTicks);
        if aDelay<0 then
          aDelay:=0;
        Sleep(aDelay);
      end;
    end;
  finally
    aFramer.Free;
  end;
end;

procedure TReadThread.Execute;
const
  aMethodName = 'TReadThread.Execute';
var
  i: Integer;
  aReader : IRecordObjectReader;
  aFramerClass: TStreamFramerClass;
begin
  //SetCurrentThreadName('Source: MediaServer.Stream.Source.RecordStorage.'+ClassName);

  FPosition:=0;
  for i := 0 to High(FRecords) do
  begin
    if Terminated then
      break;

    try
      aReader:=FRecords[i].Transport.GetReader;

      aFramerClass:=GetFramerClassFromFileName(FRecords[i].Transport.ConnectionString);

      FOwner.edProperties.Lines.Add(
        Format('Проигрывается файл N%d, %s, %s - %s ',
           [i+1,ExtractFileName(FRecords[i].Transport.ConnectionString),DateTimeToStr(FRecords[i].StartDateTime),DateTimeToStr(FRecords[i].EndDateTime)]));

      ProcessFile(aReader,
                  aFramerClass,
                  Round((FRecords[i].EndDateTime-FRecords[i].StartDateTime)*MSecsPerDay)
                  );
      aReader:=nil; //Удобно для отладки
    except
      on E:Exception do
      begin
        Application.ShowException(E);
      end;
    end;
  end;
end;

{ TfmMediaStorageRecordPreview }

procedure TfmMediaStorageRecordPreview.ckTimeBarClick(Sender: TObject);
begin
  inherited;
  paTime.Visible:=ckTimeBar.Checked;
end;

constructor TfmMediaStorageRecordPreview.Create(aRecordStorage: TRecordStorage; const aSourceName: string; const aStart,aEnd: TDateTime);
var
  //aRecordStorage: TRecordStorage;
  aRecords: TrsRecordObjectInfoArray;
begin
  Create;

  Caption:='Просмотр: '+aRecordStorage.GetConnectionString+', '+aSourceName;
  edProperties.Lines.Clear;
  edProperties.Lines.Add('Свойства соединения:');

  TWaitCursor.SetUntilIdle;
  //aRecordStorage:=TRecordStorageProvider.CreateStorage(aUDLPath,aTransport);
  try
    aRecords:=aRecordStorage.GetRecords(
      aRecordStorage.GetRecordSourceByName(aSourceName).Id,
      aStart,aEnd);
    edProperties.Lines.Add('  Строка подключения = '+aRecordStorage.GetConnectionString);
    edProperties.Lines.Add(Format('  Период = %s - %s',[DateToStr(aStart),DateToStr(aEnd)]));

    edProperties.Lines.Add(Format('Найдено %d записей:',[Length(aRecords)]));
  finally
    //aRecordStorage.Free;
  end;

  FillWithRecords(aRecords);
end;

constructor TfmMediaStorageRecordPreview.Create;
begin
  inherited Create(Application);
  FPlayer:=TWindowStreamPlayer.Create(TPlayerVideoOutput_AllTypes,TPlayerAudioOutput_AllTypes);
  FPlayer.Control.Parent:=paScreen;
  FPlayer.Control.Align:=alClient;
  FPlayer.KeepAspectRatio:=true;

  FPlayerMonitor:=TfrmVideoPaneMonitor.Create(self);
  FPlayerMonitor.Parent:=paMonitor;
  FPlayerMonitor.Align:=alTop;
  FPlayerMonitor.Player:=FPlayer;
  paMonitor.AutoSize:=true;
end;

constructor TfmMediaStorageRecordPreview.Create(
  const aRecords: TrsRecordObjectInfoArray);
begin
  Create;
  FillWithRecords(aRecords);
end;

destructor TfmMediaStorageRecordPreview.Destroy;
begin
  FreeAndNil(FReadThread);
  inherited;
end;

procedure TfmMediaStorageRecordPreview.FillWithRecords(const aRecords: TrsRecordObjectInfoArray);
var
  aLayer: TTimeScaleLayer;
  i: integer;
begin
  if Length(aRecords)=0 then
    exit;

  paTime.StartDateTime:=aRecords[0].StartDateTime;
  paTime.EndDateTime:=aRecords[High(aRecords)].EndDateTime;
  aLayer:=paTime.Layers.Add;
  aLayer.Color:=$8404992;
  for i := 0 to High(aRecords) do
  begin
    edProperties.Lines.Add(Format(' N%d: %s - %s',[i+1,DateTimeToStr(aRecords[i].StartDateTime),DateTimeToStr(aRecords[i].EndDateTime)]));
    aLayer.AddRange(aRecords[i].StartDateTime,aRecords[i].EndDateTime);
  end;

  FReadThread:=TReadThread.Create(self,aRecords);
end;

procedure TfmMediaStorageRecordPreview.OnFrameReceived(const aFormat: TMediaStreamDataHeader; aData: pointer; aDataSize: cardinal; aInfo: pointer; aInfoSize: cardinal);
begin
  if FPlayerMonitor.ckDisableDisplay.Checked then
  begin
    exit;
  end;

  FPlayerMonitor.OnDataSourceData(nil,aFormat,aData,aDataSize,aInfo,aInfoSize,FPlayer.VideoOutput);
  FPlayer.ProcessFrame(aFormat,aData,aDataSize,aInfo,aInfoSize);
end;

class procedure TfmMediaStorageRecordPreview.Run(
  const aRecords: TrsRecordObjectInfoArray);
begin
  TfmMediaStorageRecordPreview.Create(aRecords).Show;
end;

class procedure TfmMediaStorageRecordPreview.Run(aRecordStorage: TRecordStorage; const aSourceName: string; const aStart,aEnd: TDateTime);
begin
  TfmMediaStorageRecordPreview.Create(aRecordStorage,aSourceName,aStart,aEnd).Show;
end;

end.

