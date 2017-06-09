unit MediaStream.UrlFormats;

interface
  uses SysUtils,Windows;

//Медиа-сервер
function MakeMs3sUrl(const aIp: string; aPort: word; const aSourceName: string): string; overload;
function MakeMs3sUrl(const aIp: string; aPort: word; const aSourceName: string; const aUser,aPassword: string): string; overload;

//MSCP
function MakeMscpUrl(const aIp: string; aPort: word; const aSourceName: string): string; overload;
function MakeMscpUrl(const aIp: string; aPort: word; const aSourceName: string; const aUser,aPassword: string): string; overload;


//RTSP
function MakeRtspUrl(const aIp: string; aPort: word; const aSourceName: string): string; overload;
function MakeRtspUrl(const aIp: string; aPort: word; const aSourceName,aUserName,aUserPassword: string): string; overload;

//Http
function MakeHttpUrl(const aIp: string; aPort: word; const aSourceName: string): string; overload;
function MakeHttpUrl(const aIp: string; aPort: word; const aSourceName,aUserName,aUserPassword: string): string; overload;

//Файл
function MakeFileUrl(const aFilePath: string): string;

//Ip-камеры Beward
function MakeBewardUrl(const aIp: string; aPort: word; aChannel: integer; aChannelProfile: integer): string;overload;
function MakeBewardUrl(const aIp: string; aPort: word; aChannel: integer; aChannelProfile: integer; const aUserName,aUserPassword: string): string;overload;

//Файловое хранилище
function MakeRecordStorageUrl(const aDataBaseConnectionString: string; const aSourceName: string): string;

//компресионные карты HikVision
function MakeHikVisionCompCardUrl(const aChannelNo: integer; const aChannelProfile: integer): string;


function ParseRtspUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
function ParseHttpUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
function ParseMs3sUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
function ParseMscpUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
function ParseBewardUrl(const url:string; var aAddress: string; var aPort: Word; var aChannelNo: integer; var aChannelProfile: integer; var aUserName: string; var aUserPassword: string):boolean;
function ParseFileUrl(const url:string; var aFileName: string):boolean;
function ParseHikVisionCompCardUrl(const url:string; var aChannelNo: integer; var aChannelProfile: integer):boolean;

implementation
  uses StrUtils, MediaServer.Net.Definitions;

function MakeMs3sUrl(const aIp: string; aPort: word; const aSourceName: string): string;
begin
  result:=Format('ms3s://%s:%d/%s',[aIp,aPort,aSourceName]);
end;

function MakeMs3sUrl(const aIp: string; aPort: word; const aSourceName: string; const aUser,aPassword: string): string; overload;
begin
  result:=Format('ms3s://%s:%s@%s:%d/%s',[aUser,aPassword,aIp,aPort,aSourceName]);
end;

function MakeMscpUrl(const aIp: string; aPort: word; const aSourceName: string): string;
begin
  result:=Format('mscp://%s:%d/%s',[aIp,aPort,aSourceName]);
end;

function MakeMscpUrl(const aIp: string; aPort: word; const aSourceName: string; const aUser,aPassword: string): string; overload;
begin
  result:=Format('mscp://%s:%s@%s:%d/%s',[aUser,aPassword,aIp,aPort,aSourceName]);
end;

function MakeRtspUrl(const aIp: string; aPort: word; const aSourceName: string): string;
begin
  if aPort=0 then
   aPort:=554;

  result:=Format('rtsp://%s:%d/%s',[aIp,aPort,aSourceName]);
end;

function MakeRtspUrl(const aIp: string; aPort: word; const aSourceName,aUserName,aUserPassword: string): string;
begin
  if (aUserName='') and (aUserPassword='') then
    result:=MakeRtspUrl(aIp,aPort,aSourceName)
  else
    result:=Format('rtsp://%s:%s@%s:%d/%s',[aUsername,aUserPassword,aIp,aPort,aSourceName]);
end;

function MakeHttpUrl(const aIp: string; aPort: word; const aSourceName: string): string;
begin
  if aPort=0 then
   aPort:=80;

  result:=Format('http://%s:%d/%s',[aIp,aPort,aSourceName]);
end;

function MakeHttpUrl(const aIp: string; aPort: word; const aSourceName,aUserName,aUserPassword: string): string;
begin
  if (aUserName='') and (aUserPassword='') then
    result:=MakeHttpUrl(aIp,aPort,aSourceName)
  else
    result:=Format('http://%s:%s@%s:%d/%s',[aUsername,aUserPassword,aIp,aPort,aSourceName]);
end;

function MakeFileUrl(const aFilePath: string): string;
begin
  result:='file://'+StringReplace(aFilePath,'\','/',[rfReplaceAll]);
end;

function MakeBewardUrl(const aIp: string; aPort: word; aChannel: integer; aChannelProfile: integer): string;
begin
  result:=Format('bwrd://%s:%d/%d/%d',[aIp,aPort,aChannel+1,aChannelProfile+1]);
end;

function MakeBewardUrl(const aIp: string; aPort: word; aChannel: integer; aChannelProfile: integer; const aUserName,aUserPassword: string): string;overload;
begin
  result:=Format('bwrd://%s:%s@%s:%d/%d/%d',[aUserName,aUserPassword, aIp,aPort,aChannel+1,aChannelProfile+1]);
end;

function MakeRecordStorageUrl(const aDataBaseConnectionString: string; const aSourceName: string): string;
begin
  result:=Format('rstg://%s/%s',[aDataBaseConnectionString,aSourceName]);
  result:=StringReplace(result,'\','/',[rfReplaceAll]);
end;

function MakeHikVisionCompCardUrl(const aChannelNo: integer; const aChannelProfile: integer): string;
begin
  //HikVision Compression Card
  result:=Format('hvcc://%d/%d',[aChannelNo+1,aChannelProfile+1]);
end;

// =================================== PARSING =================================

procedure SkipSpaces(var aPtr: PChar);
begin
  //пропускаем пробела
  while aPtr^<>#0 do
  begin
    if not CharInSet(aPtr^,[' ',#9]) then
      break;
    Inc(aPtr);
  end;
end;

function SplitString(const S: string; var SLeft,SRight: string; const Separator: string):boolean;
var
  i : integer;
  aTmp: string;
begin
  i:=Pos(Separator,S);
  if (i=0) then
  begin
    SLeft :=S;
    SRight:='';
    result:=false;
  end
  else begin
    aTmp:=s;
    SLeft:=copy(aTmp,1,i-1);
    SRight:=copy(aTmp,i+Length(Separator),$7FFFFFFF);
    result:=true;
  end;
end;

function ParseAnyUrl(const aPrefix: string; const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
var
  aPtr: PChar;
  i: integer;
  aTmp: string;
begin
  aUserName:='';
  aUserPassword:='';
  // Parse the URL as "rtsp://<address>:<port>/<etc>"
  // (with ":<port>" and "/<etc>" optional)
  // Also, skip over any "<username>[:<password>]@" preceding <address>

  aPtr:=PChar(url);
  //Адрес
  SkipSpaces(aPtr);
  if AnsiStrLIComp(aPtr,PChar(aPrefix+'://'),Length(aPrefix)+3)=0 then
    inc(aPtr,7)
  else if AnsiStrLIComp(aPtr,PChar(aPrefix+':/'),Length(aPrefix)+2)=0 then
   inc(aPtr,6);

  aAddress:='';

  aURLSuffix:=aPtr;

  //Адрес должен быть до ближайшего /
  i:=Pos('/',aURLSuffix);
  if i=0 then
  begin
    aAddress:=aURLSuffix;
    aURLSuffix:='';
  end
  else begin
    aAddress:=Copy(aURLSuffix,1,i-1);
    aURLSuffix:=Copy(aURLSuffix,i+1,High(integer));
  end;

  result:=aAddress<>'';

  if SplitString(aAddress,aTmp,aAddress,'@') then
  begin
    SplitString(aTmp,aUserName,aUserPassword,':');
  end
  else
   aAddress:=aTmp;


  if result then
  begin
    try
      if SplitString(aAddress,aAddress,aTmp,':') then
      begin
        if TryStrToInt(aTmp,i) then
          aPort:=i;
      end;
    except
      result:=false;
    end;
  end;
end;

function ParseRtspUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
begin
  aPort:=554;
  result:=ParseAnyUrl('rtsp',url,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword);
end;

function ParseHttpUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
begin
  aPort:=80;
  result:=ParseAnyUrl('Http',url,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword);
end;

function ParseMs3sUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
begin
  aPort:=MediaServer.Net.Definitions.icCommandServerPort;
  result:=ParseAnyUrl('ms3s',url,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword);
end;

function ParseMscpUrl(const url:string; var aAddress: string; var aPort: Word; var aUrlSuffix: string; var aUserName: string; var aUserPassword: string):boolean;
begin
  aPort:=MediaServer.Net.Definitions.icMscpServerPort;
  result:=ParseAnyUrl('mscp',url,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword);
end;

function ParseBewardUrl(const url:string; var aAddress: string; var aPort: Word; var aChannelNo: integer; var aChannelProfile: integer; var aUserName: string; var aUserPassword: string):boolean;
var
  aURLSuffix: string;
  aChannelNoStr: string;
  aChannelProfileStr: string;
begin
  aPort:=5000;
  result:=ParseAnyUrl('bwrd',url,aAddress,aPort,aUrlSuffix,aUserName,aUserPassword);
  SplitString(aURLSuffix,aChannelNoStr,aChannelProfileStr,'/');
  aChannelNo:=StrToIntDef(aChannelNoStr,1)-1;
  aChannelProfile:=StrToIntDef(aChannelProfileStr,1)-1;
end;


function ParseFileUrl(const url:string; var aFileName: string):boolean;
begin
  if StartsText('file://',url) then
  begin
    aFileName:=StringReplace(Copy(url,8,High(Word)),'/','\',[rfReplaceAll]);
  end
  else if StartsText('file:/',url) then
  begin
    aFileName:=StringReplace(Copy(url,7,High(Word)),'/','\',[rfReplaceAll]);
  end
  else
    aFileName:=url;

  result:=true;
end;

function ParseHikVisionCompCardUrl(const url:string; var aChannelNo: integer; var aChannelProfile: integer):boolean;
var
  aChannelNoStr: string;
  aChannelProfileStr: string;
  p: Word;
  aUN,aUP: string;
begin
  result:=ParseAnyUrl('hvcc',url,aChannelNoStr,p,aChannelProfileStr,aUP,aUN);
  aChannelNo:=StrToIntDef(aChannelNoStr,0);
  aChannelProfile:=StrToIntDef(aChannelProfileStr,0);
end;

end.
