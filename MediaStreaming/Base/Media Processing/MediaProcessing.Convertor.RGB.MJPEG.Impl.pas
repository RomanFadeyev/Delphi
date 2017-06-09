unit MediaProcessing.Convertor.RGB.MJPEG.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics, BitmapStreamMediator,
       MediaProcessing.Definitions,MediaProcessing.Global,
       MediaProcessing.Convertor.RGB.MJPEG,JPeg,VFW;

type
  TJPegImageEx = class (TJPegImage);

  TMediaProcessor_Convertor_Rgb_MJpeg_Impl= class (TMediaProcessor_Convertor_Rgb_MJpeg,IMediaProcessorImpl)
  private
    FBuffer : TMemoryStream;
    FBitmapStream : TBimapStreamMediator;
    FJpg    : TJpegImageEx;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;

  end;



implementation
  uses uBaseClasses;


{ TMediaProcessor_Convertor_Rgb_MJpeg_Impl }

constructor TMediaProcessor_Convertor_Rgb_MJpeg_Impl.Create;
begin
  inherited;
  FBuffer:=TMemoryStream.Create;
  FBitmapStream:=TBimapStreamMediator.Create;
  FJpg:=TJPegImageEx.Create;
end;

destructor TMediaProcessor_Convertor_Rgb_MJpeg_Impl.Destroy;
begin
  FreeAndNil(FBuffer);
  FreeAndNil(FBitmapStream);
  FreeAndNil(FJpg);
  inherited;
end;

procedure TMediaProcessor_Convertor_Rgb_MJpeg_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aFileHeader: BITMAPFILEHEADER;
  aBitmapHeader : BITMAPINFOHEADER;
begin
  TArgumentValidation.NotNil(aInData);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;


  FJpg.Performance:=TJPegPerformance(FPerformance);
  FJpg.CompressionQuality:=FCompressionQuality;

  FJpg.NewImage;
  FJpg.NewBitmap;

  ZeroMemory(@aFileHeader,sizeof(aFileHeader));
  aFileHeader.bfType := $4d42; //"BM"
  aFileHeader.bfReserved1 := 0;
  aFileHeader.bfReserved2 := 0;
  aFileHeader.bfOffBits   := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
  aFileHeader.bfSize :=   aInDataSize + sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);

  //Записываем в MJpeg наш bmp
  aBitmapHeader:=aInFormat.ToBitMapInfoHeader(aInDataSize);
  Assert(aBitmapHeader.biCompression=BI_RGB);
  FBitmapStream.Init(@aFileHeader,@aBitmapHeader,aInData,aInDataSize);
  //Записываем вверх ногами. Потому что MJpeg тоже записывает вверх ногами, в результате получится правильно
  FJpg.Bitmap.LoadFromStream(FBitmapStream);

  //Сохраняем MJpeg в память
  FBuffer.Position:=0;
  FJpg.SaveToStream(FBuffer);

  aOutData:=FBuffer.Memory;
  aOutDataSize:=FBuffer.Position;
  aOutFormat:=aInFormat;
  aOutFormat.biStreamType:=stMJPEG;
  //aOutFormat.biSizeImage:=FBuffer.Position; //???
  Include(aOutFormat.biFrameFlags,ffKeyFrame);
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Convertor_Rgb_MJpeg_Impl);

end.
