unit MediaStream.Framer.Bmp;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer,MediaStream.Framer.Picture,BitPlane;

type
  TStreamFramerBmp = class (TStreamFramerPicture)
  private
    FBmpFileHeader: BITMAPFILEHEADER;
    FBmpInfoHeader: BITMAPINFOHEADER;
    FBmpDib: TBytes;
    FFrameNo: int64;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenStream(aStream: TStream); override;
    function  GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal):boolean; override;

    function VideoStreamType: TStreamType; override;
    function AudioStreamType: TStreamType; override;
    function StreamInfo: TBytes; override;

  end;

implementation

{ TStreamFramerBmp }

constructor TStreamFramerBmp.Create;
begin
  inherited;
end;

destructor TStreamFramerBmp.Destroy;
begin
  inherited;
end;

function TStreamFramerBmp.GetNextFrame(out aOutFormat: TMediaStreamDataHeader; out aOutData: pointer; out aOutDataSize: cardinal; out aOutInfo: pointer; out aOutInfoSize: cardinal): boolean;
begin
  result:=true;


  aOutData:=@FBmpDib[0];
  aOutDataSize:=Length(FBmpDib);

  aOutInfo:=nil;
  aOutInfoSize:=0;

  aOutFormat.Clear;
  aOutFormat.biMediaType:=mtVideo;
  aOutFormat.Assign(FBmpInfoHeader);
  aOutFormat.TimeStamp:=FFrameNo*FrameInterval;
  inc(FFrameNo);
  Assert(aOutFormat.biStreamType=VideoStreamType);
end;

procedure TStreamFramerBmp.OpenStream(aStream: TStream);
var
  aPos: int64;
begin
  aPos:=aStream.Position;
  aStream.ReadBuffer(FBmpFileHeader,sizeof(FBmpFileHeader));
  aStream.ReadBuffer(FBmpInfoHeader,sizeof(FBmpInfoHeader));


  if FBmpFileHeader.bfType<>$4d42 then //"BM"
    raise Exception.Create('Неверный формат');

  aPos:=aStream.Position-aPos;
  if FBmpFileHeader.bfOffBits<aPos then
    raise Exception.Create('Неверный формат');

  aStream.Seek(FBmpFileHeader.bfOffBits-aPos,soFromCurrent);

  aPos:=GetRGBLineSize(FBmpInfoHeader.biWidth,FBmpInfoHeader.biBitCount)*FBmpInfoHeader.biHeight;
  if (aPos<>FBmpInfoHeader.biSizeImage) then
    raise Exception.Create('Размер изображения некорректный');


  SetLength(FBmpDib,FBmpInfoHeader.biSizeImage);
  aStream.ReadBuffer(FBmpDib[0],FBmpInfoHeader.biSizeImage);
  FFrameNo:=0;
end;

function TStreamFramerBmp.StreamInfo: TBytes;
begin
  result:=nil;
end;

function TStreamFramerBmp.VideoStreamType: TStreamType;
begin
  result:=stRGB;
end;

function TStreamFramerBmp.AudioStreamType: TStreamType;
begin
  result:=0;
end;


end.


