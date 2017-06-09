unit MediaProcessing.Drawer.RGB.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics, BitmapStreamMediator,
       MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Drawer.RGB;

type
  TMediaProcessor_Drawer_Rgb_Impl =class (TMediaProcessor_Drawer_Rgb,IMediaProcessorImpl)
  private
    FBimap: TBitmap;
    FBitmapStream : TBimapStreamMediator;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;

constructor TMediaProcessor_Drawer_Rgb_Impl.Create;
begin
  inherited;
  FBimap:=TBitmap.Create;
  FBitmapStream:=TBimapStreamMediator.Create;
end;

destructor TMediaProcessor_Drawer_Rgb_Impl.Destroy;
begin
  FreeAndNil(FBitmapStream);
  FreeAndNil(FBimap);
  inherited;
end;

procedure TMediaProcessor_Drawer_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
//var
  //aFileHeader: BITMAPFILEHEADER;
  //aBitmapHeader : BITMAPINFOHEADER;
begin
  TArgumentValidation.NotNil(aInData);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutFormat:=aInFormat;

  (* TODO
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
  FBimap.LoadFromStream(FBitmapStream);

  aOutData:=FBimap.ScanLine[0];
  Assert(FBimap.PixelFormat=pf24bit);
  aOutDataSize:=FBimap.Width*FBimap.Height*3;
  aOutFormat:=aInFormat;
  *)
end;

//initialization
  //MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Drawer_Rgb_Impl);

end.
