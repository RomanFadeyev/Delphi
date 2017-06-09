unit MediaProcessing.Definitions;

interface
  uses Windows,SysUtils,Classes,SyncObjs,MMSystem, Generics.Collections;

type
  TGUIDArray = array of TGUID;
  TMediaType = (mtVideo,mtAudio,mtSysData);
  TMediaTypeSet = set of TMediaType;
const
  AllMediaTypes = [mtVideo,mtAudio,mtSysData];

type
  TStreamType = DWORD;
  TAllMediaStreamTypes = array [TMediaType] of TStreamType;
  TAllMediaStreamBytes = array [TMediaType] of TBytes;
  TAllMediaStreamGuids = array [TMediaType] of TGUIDArray;
  TAllMediaStreamBools =  array [TMediaType] of boolean;

const
  MediaTypeNames: array [TMediaType] of string = ('Video','Audio','SysData');

  stHHVI  = TStreamType($49564848); //HHVI;
  stHHAU  = TStreamType($55414848); //HHAU;
  stYUV420 = TStreamType($34565559); //YUV, 4+2+0;
  stRGB = TStreamType($20424752); //RGB
  stMJPEG = TStreamType($47504A4D); //MJPG
  stH264 = TStreamType($34363248); //H264
  stMpeg4 = TStreamType($34504D46); //FMP4

  //AUDIO
  stPCM  =TStreamType($204D4350); //PCM
  stPCMU  =TStreamType($554D4350); //PCMU

  stUNIV = TStreamType($554E4956); //UNIV. Universal container. Type of stream is unknown until first frame came


  stCOPY = TStreamType($59504F43); //COPY
  stBinary = TStreamType($53545354); //TSTS

  //SYS
  stPtzPosition = TStreamType($505A5450); //PTZP
  stVideoAnalytics = TStreamType($53524156); //VARS
  stPING = TStreamType($474E4950); //PING
  stNotification = TStreamType ($4346544E); //NTFC
//  stAcceptanceFilter = TStreamType($52464341); //ACFR

var
  stKnownTypes: array [TMediaType] of TArray<TStreamType>;

type
  TFrameFlag = (ffKeyFrame,ffInitParamsFrame,ffPrebuffer);
  TFrameFlags = set of TFrameFlag;

  TMediaStreamDataHeader = packed record
    biMediaType: TMediaType;
    biStreamType: TStreamType;
    biStreamSubType: DWORD;
    biFrameFlags: TFrameFlags;

    Channel: integer;
    TimeStamp: int64; //ms
    TimeKoeff: word;  //TimeStamp*TimeKoeff = ms

    Tag : integer;

    case TMediaType of
     mtVideo: (
       VideoWidth: integer;
       VideoHeight: integer;
        //biPlanes: Word;
       VideoBitCount: Word;
       VideoReversedVertical: boolean;
       );
     mtAudio: (
       AudioChannels: DWORD;
       AudioBitsPerSample: DWORD;
       AudioSamplesPerSec: DWORD;
     );
  end;

  TMediaStreamDataHeaderHelper = record helper for TMediaStreamDataHeader
    procedure Assign(const aSource: TBitmapInfoHeader); overload;
    procedure Assign(const aSource: TPCMWaveFormat); overload;
    procedure Assign(const aSource: TWaveFormatEx); overload;
    procedure AssignBitmap(aVideoWidth,aVideoHeight,aVideoBits: integer);

    function ToBitmapInfoHeader(aImageSize: cardinal): TBitmapInfoHeader;
    function ToPCMWaveFormat: TPCMWaveFormat;
    function ToWaveFormatEx: TWaveFormatEx;

    function TimeStampMs: int64;

    procedure Clear;
  end;

  TAllMediaStreamDataHeaders = array [TMediaType] of TMediaStreamDataHeader;

  //MediaType = SYSDATA, StreamType = stPing
  TPingArgs = record
    Reserved:byte;
  end;
  PPingArgs = ^TPtzPositionArgs;

  //MediaType = SYSDATA, StreamType = stPtzPosition;
  TPtzPositionArgs = record
    Pan: double;
    Tilt: double;
  end;
  PPtzPositionArgs = ^TPtzPositionArgs;

  //MediaType = SYSDATA, StreamType = stNotification;
  TNotificationArgs = record
    Notification:cardinal; //TMediaStreamDataSourceNotification;
    SourceId: cardinal;
  end;
  PNotificationArgs = ^TNotificationArgs;


  //MediaType =SYSDATA, StreamType = stVideoAnalytics
  TVaData = pointer;
(*
  TVaEvent = record
    {//// Event type. Can be SVA_EVENT_TYPE_*. }
    type_:     integer;
    {//// Event level. Can be SVA_EVENT_LEVEL_*. }
    level:     integer;
    {//// Object ID or SVA_UNDEFINED_ID. }
    object_id: integer;
    {//// ID of rule which generates this event or SVA_UNDEFINED_ID. }
    rule_id:   integer;
    {//// Event description. It can be NULL. }
    description: string;
  end;
  TVaEventArray = array of TVaEvent;

  TVaObject = record
    {//// Unique object ID }
    id:    integer;
    {//// Object type. Can be SVA_OBJECT_TYPE_*. }
    type_: integer;
    {//// Object position at current time. }
    position: TVaPosition;
    {//// Object bounding box. }
    rect:  TRect;
    {//// Start object position. }
    start_position: TVaPosition;
    {//// Object trajectory. }
    trajectory: TVaPositionArray;
    {//// Index of object region in mask }
    mask_index: integer;
    {//// Bounding box of object region in mask. }
    mask_rect: TRect;

    all_events: array of integer;   //SVA_EVENT_TYPE_
  end;
  *)


//Получить человеко-читабельное имя типа потока
function GetStreamTypeName(aStreamType: TStreamType): string;

//Получить тип потока в представлении FourCC (четырехсимвольная строка)
function StreamTypeToFourccString(aStreamType: TStreamType): AnsiString;

//Получить тип потока в представлении FourCC (четырехсимвольная строка), через запятую
function StreamTypesToFourccString(const aStreamTypes: TArray<TStreamType>): AnsiString;

//Преобразовать массив типов потоков в структуру TArray<TStreamType>
function StreamTypesToArray(const aStreamTypes: array of TStreamType): TArray<TStreamType>;

//Преобразовать четырехсимвольную строку в тип потока
function FourccStringToStreamType(aFourcc: AnsiString): TStreamType;

function GetStreamTypeNames(aStreamTypes: TAllMediaStreamTypes; aLongFormat: boolean = true): string;

function FrameFlagsToString(aFlags: TFrameFlags): string;

function IsAppropriateToMediaType(aStreamType: TStreamType; aMediaType: TMediaType): boolean;

type
  TConsumingLevel = 0..9;

  TMediaProcessorInfo = record
    TypeID: TGUID;
    Name: string;
    Description: string;

    InputStreamTypes: TArray<TStreamType>;
    InputStreamSubType: string;
    OutputStreamType: TStreamType;
    OutputStreamSubType: string;
    ConsumingLevel:TConsumingLevel;
  end;

  TMediaProcessorInfoHelper =  record helper for TMediaProcessorInfo
    procedure Clear;

    procedure SetInputStreamType(aStreamType: TStreamType);
    procedure SetInputStreamTypes(const aStreamTypes: array of TStreamType);

    function CanAcceptInputStreamType(aStreamType: TStreamType): boolean;
    function IndexOfInputStreamType(aStreamType: TStreamType): integer;
  end;

  IMediaProcessor = interface
  ['{526EF959-157A-4767-96B7-E66012783175}']
    function  Info: TMediaProcessorInfo;
    procedure Prepare;

    function  HasCustomProperties: boolean;
    procedure ShowCustomProperiesDialog;

    procedure SaveCustomProperties(aStream: TStream);
    procedure LoadCustomProperties(aStream: TStream);
  end;
  TIMediaProcessorArray = array of IMediaProcessor;

  IMediaProcessorImpl = interface (IMediaProcessor)
  ['{5543C840-A2CB-4624-8C18-83634ACB4A59}']
    procedure ProcessData(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);

    procedure GetLastErrorInfo(out aMessage: string; out aDateTime: TDateTime);
  end;
  TIMediaProcessorImplArray = array of IMediaProcessorImpl;

  TMediaProcessorResult = record
    Data: pointer;
    DataSize:cardinal;
    Info: pointer;
    InfoSize: cardinal;
    Format: TMediaStreamDataHeader;
  end;
  TMediaProcessorResultList = TList<TMediaProcessorResult>;

  IMediaProcessorImpl2 = interface (IMediaProcessor)
  ['{D8F15C33-6158-4052-8E66-63D460752418}']
    procedure ProcessData2(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal;
                      aAuxResults: TMediaProcessorResultList);
  end;


  IMediaProcessor_Convertor_AvcAnyVideo_Rgb = interface
  ['{10A51974-972B-4768-86ED-54182222D8D3}']
  end;

  IMediaProcessor_Convertor_Hh_Rgb = interface
  ['{7738F4A5-3357-45BE-8EF5-7BE4B6B808E9}']
  end;

  IMediaProcessor_Convertor_H264_Rgb = interface
  ['{75F3FEEB-24AF-4DBC-89C6-13D986A47FA3}']
  end;

  IMediaProcessor_Convertor_Rgb_MJpeg = interface
  ['{74819984-9B95-4C88-9172-47DCBF35F28E}']
  end;

  IMediaProcessor_Convertor_Rgb_H264 = interface
  ['{7CDB426E-FC52-486E-801A-E8FBD1229F94}']
  end;

  IMediaProcessor_Convertor_Rgb_Vfw = interface
  ['{5182446E-2307-4998-8413-6BDFA8E99820}']
  end;

  IMediaProcessor_Transformer_Hh = interface
  ['{7DA18990-DE91-4377-8D95-00E1583821C7}']
  end;

  IMediaProcessor_Convertor_HhH264_H264 = interface
  ['{D350E82D-45F0-48AD-80C9-F6DBD04CC759}']
  end;

  IMediaProcessor_Drawer_Rgb = interface
  ['{BF4298D5-A15D-4AB3-8CD4-B9ECC383CD96}']
  end;

  IMediaProcessor_Transformer_Rgb = interface
  ['{69A016FC-2AAE-468E-8CA4-5CA47B8B5017}']
  end;

  IMediaProcessor_Panorama_Rgb = interface
  ['{CFA07663-940D-488D-A658-A30177E39C04}']
  end;

  IMediaProcessor_Stabilizer_Rgb = interface
  ['{4ACDA439-C84A-42B4-95B0-5C14FC4FCC97}']
  end;

  IMediaProcessor_Convertor_HhAu_Pcm = interface
  ['{B0BC9F25-D4C0-45D6-87CF-4D2314E5CD05}']
  end;

  IMediaProcessor_Transformer_Pcm = interface
  ['{1C11C7F1-4973-4C43-BFE0-FFA1814C46DA}']
  end;


  IMediaProcessor_Processor_Va_Any = interface
  ['{8234F7FE-1C23-4F20-8ED2-CAB64D3E7E3C}']
  end;


const
  MediaFilesOpenDialogFilter ='Файлы медиа (*.tsm;*.mp6;*.h264;*.hv;*.avi;*.mkv;*.ps;*.ts;*.mpg;*.ogg;*.asf;*.mov;*.flv;*.webm;*.wav;*.bmp;*.jpg;*.jpeg)|*.tsm;*.mp6;*.h264;*.hv;*.avi;*.mkv;*.ps;*.ts;*.mpg;*.ogg;*.asf;*.mov;*.flv;*.webm;*.wav;*.bmp;*.jpg;*.jpeg|Все файлы|*.*';

implementation

//Продублирована из VFW, чтобы не подключать целый модуль из-за одного макроса
function FOURCCTOSTR(value: cardinal): AnsiString;
var
  s: AnsiString;
begin
  if value=0 then
    exit('NONE');

  s:=AnsiChar(Value)+
  AnsiChar(Value shr 8)+
  AnsiChar(Value shr 16)+
  AnsiChar(Value shr 24);

  result:=s;
end;


{ TMediaProcessorInfoHelper }

procedure TMediaProcessorInfoHelper.Clear;
begin
  FillChar(TypeID,0,sizeof(TypeID));
  Name:='';
  Description:='';
  InputStreamTypes:=nil;
  InputStreamSubType:='';

  OutputStreamType:=Low(TStreamType);
  OutputStreamSubType:='';
  ConsumingLevel:=0;
end;

function TMediaProcessorInfoHelper.IndexOfInputStreamType(aStreamType: TStreamType): integer;
var
  i: Integer;
begin
  result:=-1;
  for i := 0 to High(InputStreamTypes) do
  begin
    if (InputStreamTypes[i]=aStreamType) then
    begin
      result:=i;
      break;
    end;
  end;
end;

function GetStreamTypeName(aStreamType: TStreamType): string;
begin
  case aStreamType of
    0: result:='None';
    stHHVI: result:='Beward Video';
    stHHAU: result:='Beward Audio';
    stRGB: result:='RGB';
    stMJPEG: result:='MJPG';
    stYUV420: result:='YUV 4-2-0';
    stH264: result:='H.264';
    stUNIV: result:='Container (Universal)';
    stMpeg4: result:='MPEG-4';
    else
      result:=string(FOURCCTOSTR(aStreamType));
  end;
end;

function StreamTypeToFourccString(aStreamType: TStreamType): AnsiString;
var
  i: Integer;
begin
  if aStreamType<10 then
  begin
    result:='000'+IntToStr(aStreamType);
  end
  else begin
    result:=FOURCCTOSTR(aStreamType);

    for i := 1 to Length(result) do
      if result[i]<=#32 then
        result[i]:='_';
  end;
end;

function StreamTypesToFourccString(const aStreamTypes: TArray<TStreamType>): AnsiString;
var
  i: Integer;
begin
  result:='';

  for i := 0 to High(aStreamTypes) do
  begin
    if result<>'' then
      result:=result+', ';
    result:=result+StreamTypeToFourccString(aStreamTypes[i]);
  end;

end;

function FourccStringToStreamType(aFourcc: AnsiString): TStreamType;
begin
  if Length(aFourcc)<>4 then
    exit(0);

  if aFourcc='NONE' then
    exit(0);


  Result := (DWord(Ord(aFourcc[1]))) or
            (DWord(Ord(aFourcc[2])) shl 8) or
            (DWord(Ord(aFourcc[3])) shl 16) or
            (DWord(Ord(aFourcc[4])) shl 24);
end;

function GetStreamTypeNames(aStreamTypes: TAllMediaStreamTypes; aLongFormat: boolean = true): string;
begin
  if aLongFormat then
    result:=Format('Video: %s; Audio: %s',[GetStreamTypeName(aStreamTypes[mtVideo]),GetStreamTypeName(aStreamTypes[mtAudio])])
  else
    result:=Format('V: %s; A: %s',[GetStreamTypeName(aStreamTypes[mtVideo]),GetStreamTypeName(aStreamTypes[mtAudio])]);
end;

function IsAppropriateToMediaType(aStreamType: TStreamType; aMediaType: TMediaType): boolean;
begin
  if aMediaType=mtVideo then
    result:=(aStreamType<>stPCM) and (aStreamType<>stPCMU) and (aStreamType<>stHHAU)
  else if aMediaType=mtAudio then
    result:=(aStreamType=stPCM) or (aStreamType=stHHAU) or (aStreamType=stPCMU) or (aStreamType=stUNIV)
  else
    result:=true;
end;

function FrameFlagsToString(aFlags: TFrameFlags): string;
begin
  result:='';
  if ffKeyFrame in aFlags then
    result:=result+',key';
  if ffInitParamsFrame in aFlags then
    result:=result+',init params';
  if ffPrebuffer in aFlags then
    result:=result+',prebuffer';

  if result<>'' then
    Delete(result,1,1);

  result:='['+result+']';
end;

function StreamTypesToArray(const aStreamTypes: array of TStreamType): TArray<TStreamType>;
var
  i: Integer;
begin
  SetLength(result,Length(aStreamTypes));
  for i := 0 to High(aStreamTypes) do
    result[i]:=aStreamTypes[i];
end;


function TMediaProcessorInfoHelper.CanAcceptInputStreamType(aStreamType: TStreamType): boolean;
var
  i: Integer;
begin
  result:=false;
  for i := 0 to High(InputStreamTypes) do
  begin
    if (InputStreamTypes[i]=aStreamType) or (InputStreamTypes[i]=stCOPY) then
    begin
      result:=true;
      break;
    end;
  end;
end;

procedure TMediaProcessorInfoHelper.SetInputStreamType(aStreamType: TStreamType);
begin
  SetLength(InputStreamTypes,1);
  InputStreamTypes[0]:=aStreamType;
end;

procedure TMediaProcessorInfoHelper.SetInputStreamTypes(const aStreamTypes: array of TStreamType);
begin
  InputStreamTypes:=StreamTypesToArray(aStreamTypes)
end;

{ TMediaStreamDataHeaderHelper }

procedure TMediaStreamDataHeaderHelper.Assign(const aSource: TBitmapInfoHeader);
begin
  self.Clear;
  self.biMediaType:=mtVideo;
  self.biStreamType:=aSource.biCompression;
  if aSource.biCompression=0 then
    Self.biStreamType:=stRGB;
  self.VideoWidth:=aSource.biWidth;
  self.VideoHeight:=aSource.biHeight;
  self.VideoBitCount:=aSource.biBitCount;
  //self.DataSize:=aSource.biSizeImage;
  self.biFrameFlags:=[ffKeyFrame];
end;

procedure TMediaStreamDataHeaderHelper.Assign(const aSource: TPCMWaveFormat);
begin
  self.Clear;
  self.biMediaType:=mtAudio;

  //0 (0x0000) 	Unknown
  //1 (0x0001) 	PCM/uncompressed
  //2 (0x0002) 	Microsoft ADPCM
  //6 (0x0006) 	ITU G.711 a-law
  //7 (0x0007) 	ITU G.711 Aч-law
  //17 (0x0011) 	IMA ADPCM
  //20 (0x0016) 	ITU G.723 ADPCM (Yamaha)
  //49 (0x0031) 	GSM 6.10
  //64 (0x0040) 	ITU G.721 ADPCM
  //80 (0x0050) 	MPEG
  //65,536 (0xFFFF) 	Experimental

  case aSource.wf.wFormatTag of
    WAVE_FORMAT_PCM: self.biStreamType:=stPCM;
    7:self.biStreamType:=stPCMU;
    else
      self.biStreamType:=aSource.wf.wFormatTag; //??
  end;

  self.AudioChannels:=aSource.wf.nChannels;
  self.AudioBitsPerSample:=aSource.wBitsPerSample;
  self.AudioSamplesPerSec:=aSource.wf.nSamplesPerSec;
  self.biFrameFlags:=[ffKeyFrame];
end;

procedure TMediaStreamDataHeaderHelper.Assign(const aSource: TWaveFormatEx);
var
  aPCMFormat: TPCMWaveFormat;
begin
  aPCMFormat.wf.wFormatTag:=aSource.wFormatTag;
  aPCMFormat.wf.nChannels:=aSource.nChannels;
  aPCMFormat.wf.nSamplesPerSec:=aSource.nSamplesPerSec;
  aPCMFormat.wf.nAvgBytesPerSec:=aSource.nAvgBytesPerSec;
  aPCMFormat.wf.nBlockAlign:=aSource.nBlockAlign;
  aPCMFormat.wBitsPerSample:=aSource.wBitsPerSample;
  Assign(aPCMFormat);
end;

procedure TMediaStreamDataHeaderHelper.AssignBitmap(aVideoWidth, aVideoHeight,aVideoBits: integer);
begin
  self.Clear;
  self.biMediaType:=mtVideo;
  Self.biStreamType:=stRGB;
  self.VideoWidth:=aVideoWidth;
  self.VideoHeight:=aVideoHeight;
  self.VideoBitCount:=aVideoBits;
  self.biFrameFlags:=[ffKeyFrame];
end;

procedure TMediaStreamDataHeaderHelper.Clear;
begin
  ZeroMemory(@self,sizeof(TMediaStreamDataHeader));
  Self.TimeKoeff:=1;
end;

function TMediaStreamDataHeaderHelper.TimeStampMs: int64;
begin
  result:=TimeStamp*TimeKoeff;
end;

function TMediaStreamDataHeaderHelper.ToBitmapInfoHeader(aImageSize: cardinal): TBitmapInfoHeader;
begin
  if self.biMediaType<>mtVideo then
    raise Exception.Create('Данные не являются видео');

  ZeroMemory(@result,sizeof(result));
  result.biSize:=SizeOf(result);
  result.biWidth:=self.VideoWidth;
  result.biHeight:=self.VideoHeight;
  result.biBitCount:=self.VideoBitCount;
  result.biPlanes:=1;
  result.biCompression:=self.biStreamType;
  result.biSizeImage:=aImageSize;// self.DataSize;

  if result.biCompression=stRGB then
    result.biCompression:=BI_RGB;
end;

function TMediaStreamDataHeaderHelper.ToPCMWaveFormat: TPCMWaveFormat;
begin
  ZeroMemory(@result,sizeof(Result));
  Result.wBitsPerSample:=self.AudioBitsPerSample;
  Result.wf.nChannels:=self.AudioChannels;
  Result.wf.nSamplesPerSec:=self.AudioSamplesPerSec;
  Result.wf.nBlockAlign:=(Result.wBitsPerSample div 8) * Result.wf.nChannels;
  Result.wf.nAvgBytesPerSec:=Result.wf.nBlockAlign*Result.wf.nSamplesPerSec;

  if self.biStreamType=stPCM then
    Result.wf.wFormatTag:=WAVE_FORMAT_PCM
  else if self.biStreamType=stPCMU then
    result.wf.wFormatTag:=7
  else
    result.wf.wFormatTag:=0;


  //0 (0x0000) 	Unknown
  //1 (0x0001) 	PCM/uncompressed
  //2 (0x0002) 	Microsoft ADPCM
  //6 (0x0006) 	ITU G.711 a-law
  //7 (0x0007) 	ITU G.711 Aч-law
  //17 (0x0011) 	IMA ADPCM
  //20 (0x0016) 	ITU G.723 ADPCM (Yamaha)
  //49 (0x0031) 	GSM 6.10
  //64 (0x0040) 	ITU G.721 ADPCM
  //80 (0x0050) 	MPEG
  //65,536 (0xFFFF) 	Experimental
end;

function TMediaStreamDataHeaderHelper.ToWaveFormatEx: TWaveFormatEx;
var
  aPCMFormat: TPCMWaveFormat;
begin
  aPCMFormat:=self.ToPCMWaveFormat;
  ZeroMemory(@result,sizeof(result));
  Result.cbSize:=sizeof(result);
  Result.wFormatTag:=aPCMFormat.wf.wFormatTag;
  Result.nChannels:=aPCMFormat.wf.nChannels;
  Result.nSamplesPerSec:=aPCMFormat.wf.nSamplesPerSec;
  Result.nAvgBytesPerSec:=aPCMFormat.wf.nAvgBytesPerSec;
  Result.nBlockAlign:=aPCMFormat.wf.nBlockAlign;
  Result.wBitsPerSample:=aPCMFormat.wBitsPerSample;

end;

initialization

  stKnownTypes[mtVideo]:=StreamTypesToArray(
  [stHHVI,
   stMJPEG,
   stH264,
   stMpeg4,
   stRGB,
   stYUV420,
   stUNIV
  ]);

  stKnownTypes[mtAudio]:=StreamTypesToArray(
  [
   stHHAU,
   stPCM,
   stPCMU,
   stUNIV
   ]);

  stKnownTypes[mtSysData]:=StreamTypesToArray(
  [
    stPtzPosition,
    stVideoAnalytics,
    stPING,
    stNotification
  ]);


end.



