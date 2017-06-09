unit MediaProcessing.Common.Processor.FPS.ImageSize;

interface
  uses SysUtils,Windows,Classes,MediaProcessing.Definitions,MediaProcessing.Global,
  MediaProcessing.Common.SettingsDialog.FPS.ImageSize,MediaProcessing.Common.Processor.FPS;

type
  TChangeImageSizeMode = (cismNone,cismScale,cismCustomSize);

  TMediaProcessor_FpsImageSize<T: TfmMediaProcessingSettingsFpsImageSize,constructor>=class (TMediaProcessor_Fps<T>)
  private
    procedure Process_ScaleBmp(aDIB: pointer; aWidth,aHeight: integer; aScaleX, aScaleY: integer; out aNewWidth,aNewHeight: integer);
  protected
    FImageSizeMode   : TChangeImageSizeMode;
    FImageSizeScale  : integer;
    FImageSizeWidth  : integer;
    FImageSizeHeight : integer;
  protected
    procedure SaveCustomProperties(const aWriter: IPropertiesWriter); override;
    procedure LoadCustomProperties(const aReader: IPropertiesReader); override;

    procedure OnLoadPropertiesToDialog(aDialog: T); override;
    procedure OnSavePropertiesFromDialog(aDialog: T); override;

    procedure Process_ImageSizeRGB(aDIB: pointer; var aWidth, aHeight: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Controls,uBaseClasses,MediaProcessing.Convertor.H264.RGB.SettingsDialog;


{ TMediaProcessor_FpsImageSize }

constructor TMediaProcessor_FpsImageSize<T>.Create;
begin
  inherited;
  FImageSizeScale:=2;
  FImageSizeMode:=cismNone;
  FImageSizeWidth:=640;
  FImageSizeHeight:=480;
end;

destructor TMediaProcessor_FpsImageSize<T>.Destroy;
begin
  inherited;
end;

procedure TMediaProcessor_FpsImageSize<T>.LoadCustomProperties(const aReader: IPropertiesReader);
begin
  inherited;
  FImageSizeMode:=TChangeImageSizeMode(aReader.ReadInteger('ImageSize.ChangeMode',integer(FImageSizeMode)));
  FImageSizeScale:=aReader.ReadInteger('ImageSize.Scale',FImageSizeScale);
  FImageSizeWidth:=aReader.ReadInteger('ImageSize.Width',FImageSizeWidth);
  FImageSizeHeight:=aReader.ReadInteger('ImageSize.Height',FImageSizeHeight);
end;

procedure TMediaProcessor_FpsImageSize<T>.SaveCustomProperties(const aWriter: IPropertiesWriter);
begin
  inherited;
  aWriter.WriteInteger('ImageSize.ChangeMode',integer(FImageSizeMode));
  aWriter.WriteInteger('ImageSize.Scale',FImageSizeScale);
  aWriter.WriteInteger('ImageSize.Width',FImageSizeWidth);
  aWriter.WriteInteger('ImageSize.Height',FImageSizeHeight);
end;

procedure TMediaProcessor_FpsImageSize<T>.OnLoadPropertiesToDialog(aDialog: T);
begin
  inherited;

  aDialog.ckChangeImageSize.Checked:=FImageSizeMode<>cismNone;
  aDialog.cbImageSizeScale.ItemIndex:=aDialog.cbImageSizeScale.Items.IndexOfData(FImageSizeScale);
  if aDialog.cbImageSizeScale.ItemIndex=-1 then
    aDialog.cbImageSizeScale.ItemIndex:=0;

  aDialog.buImageSizeScale.Checked:=FImageSizeMode=cismScale;
  aDialog.buImageSizeCustomSize.Checked:=FImageSizeMode=cismCustomSize;
  if FImageSizeMode=cismNone then
    aDialog.buImageSizeScale.Checked:=true;


  aDialog.edImageSizeWidth.Value:=FImageSizeWidth;
  aDialog.edimageSizeHeight.Value:=FImageSizeHeight;
end;

procedure TMediaProcessor_FpsImageSize<T>.OnSavePropertiesFromDialog(
  aDialog: T);
begin
  inherited;

  if not aDialog.ckChangeImageSize.Checked then
    FImageSizeMode:=cismNone
  else if aDialog.buImageSizeScale.Checked then
    FImageSizeMode:=cismScale
  else if aDialog.buImageSizeCustomSize.Checked then
    FImageSizeMode:=cismCustomSize
  else
    Assert(false);

  FImageSizeScale:=aDialog.cbImageSizeScale.CurrentItemData;
  FImageSizeWidth:=aDialog.edImageSizeWidth.Value;
  FImageSizeHeight:=aDialog.edimageSizeHeight.Value;
end;


procedure TMediaProcessor_FpsImageSize<T>.Process_ImageSizeRGB(aDIB: pointer; var aWidth, aHeight: integer);
var
  aKX,aKY: integer;
  aNewWidth, aNewHeight: integer;
begin
  aNewWidth:=aWidth;
  aNewHeight:=aHeight;

  if FImageSizeMode=cismScale then
  begin
    if FImageSizeScale<>1 then
      Process_ScaleBmp(aDIB,aWidth,aHeight,FImageSizeScale, FImageSizeScale,aNewWidth,aNewHeight);
  end
  else if FImageSizeMode=cismCustomSize then
  begin
    aKX:=1;
    if FImageSizeWidth>0 then
    begin
      aKX:=Round(aWidth/FImageSizeWidth);
      if aKX<1 then
        aKX:=1;
    end;

    aKY:=1;
    if FImageSizeWidth>0 then
    begin
      aKY:=Round(aHeight/FImageSizeHeight);
      if aKY<1 then
        aKY:=1;
    end;

    Process_ScaleBmp(aDIB,aWidth,aHeight,aKX,aKY,aNewWidth,aNewHeight);
  end;

  aWidth:=aNewWidth;
  aHeight:=aNewHeight;
end;

procedure TMediaProcessor_FpsImageSize<T>.Process_ScaleBmp(aDIB: pointer;
  aWidth, aHeight, aScaleX, aScaleY: integer; out aNewWidth,
  aNewHeight: integer);
var
  aBmpDIBPtr: PAnsiChar;
  aStrideOld,aStrideNew: integer;
  aLOld,aLNew: PAnsiChar;
  yOld,xOld,yNew: integer;
const
  aBPP=3;
begin
  Assert(aScaleX>0);
  Assert(aScaleY>0);
  if (aScaleX=1) and (aScaleY=1) then
    exit;

  aNewHeight:=aHeight div aScaleY;
  aNewWidth:=aWidth div aScaleX;

  aStrideOld :=  aBPP*aWidth;
  aStrideNew := aBPP*aNewWidth;

  aBmpDIBPtr := aDIB;
  yNew:=0;
  for yOld :=0 to aHeight-1 do
  begin
    if (yOld mod aScaleY)<>0 then
      continue;

    aLOld:=aBmpDIBPtr+yOld*aStrideOld;
    aLNew:=aBmpDIBPtr+yNew*aStrideNew;
    for xOld := 0 to aWidth-1 do
    begin
      if (xOld mod aScaleX)=0 then
      begin
        case aBPP of
          1: aLNew^:=aLOld^;
          2: PWord(aLNew)^:=PWord(aLOld)^;
          3: begin
            PWord(aLNew)^:=PWord(aLOld)^;
            (aLNew+2)^:=(aLOld+2)^;
          end;
          4: PDWord(aLNew)^:=PDWord(aLOld)^;
          else
            Assert(false);
        end;
        inc(aLNew,aBPP);
      end;
      inc(aLOld,aBPP);
    end;
    inc(yNew);
  end;
end;

(*
{
  Here is the routine I use in my thumbnail component and I belive it is quite
  fast.
  A tip to gain faster loading of jpegs is to use the TJpegScale.Scale
  property. You can gain a lot by using this correct.

  This routine can only downscale images no upscaling is supported and you
  must correctly set the dest image size. The src.image will be scaled to fit
  in dest bitmap.
}


 const
  FThumbSize = 150;

 //Speed up by Renate Schaaf, Armido, Gary Williams...
procedure MakeThumbNail(src, dest: tBitmap);
 type
   PRGB24 = ^TRGB24;
   TRGB24 = packed record
     B: Byte;
     G: Byte;
     R: Byte;
   end;
 var
   x, y, ix, iy: integer;
   x1, x2, x3: integer;

   xscale, yscale: single;
   iRed, iGrn, iBlu, iRatio: Longword;
   p, c1, c2, c3, c4, c5: tRGB24;
   pt, pt1: pRGB24;
   iSrc, iDst, s1: integer;
   i, j, r, g, b, tmpY: integer;

   RowDest, RowSource, RowSourceStart: integer;
   w, h: integer;
   dxmin, dymin: integer;
   ny1, ny2, ny3: integer;
   dx, dy: integer;
   lutX, lutY: array of integer;

 begin
   if src.PixelFormat <> pf24bit then src.PixelFormat := pf24bit;
   if dest.PixelFormat <> pf24bit then dest.PixelFormat := pf24bit;
   w := Dest.Width;
   h := Dest.Height;

   if (src.Width <= FThumbSize) and (src.Height <= FThumbSize) then
   begin
     dest.Assign(src);
     exit;
   end;

   iDst := (w * 24 + 31) and not 31;
   iDst := iDst div 8; //BytesPerScanline
  iSrc := (Src.Width * 24 + 31) and not 31;
   iSrc := iSrc div 8;

   xscale := 1 / (w / src.Width);
   yscale := 1 / (h / src.Height);

   // X lookup table
  SetLength(lutX, w);
   x1 := 0;
   x2 := trunc(xscale);
   for x := 0 to w - 1 do
   begin
     lutX[x] := x2 - x1;
     x1 := x2;
     x2 := trunc((x + 2) * xscale);
   end;

   // Y lookup table
  SetLength(lutY, h);
   x1 := 0;
   x2 := trunc(yscale);
   for x := 0 to h - 1 do
   begin
     lutY[x] := x2 - x1;
     x1 := x2;
     x2 := trunc((x + 2) * yscale);
   end;

   dec(w);
   dec(h);
   RowDest := integer(Dest.Scanline[0]);
   RowSourceStart := integer(Src.Scanline[0]);
   RowSource := RowSourceStart;
   for y := 0 to h do
   begin
     dy := lutY[y];
     x1 := 0;
     x3 := 0;
     for x := 0 to w do
     begin
       dx:= lutX[x];
       iRed:= 0;
       iGrn:= 0;
       iBlu:= 0;
       RowSource := RowSourceStart;
       for iy := 1 to dy do
       begin
         pt := PRGB24(RowSource + x1);
         for ix := 1 to dx do
         begin
           iRed := iRed + pt.R;
           iGrn := iGrn + pt.G;
           iBlu := iBlu + pt.B;
           inc(pt);
         end;
         RowSource := RowSource - iSrc;
       end;
       iRatio := 65535 div (dx * dy);
       pt1 := PRGB24(RowDest + x3);
       pt1.R := (iRed * iRatio) shr 16;
       pt1.G := (iGrn * iRatio) shr 16;
       pt1.B := (iBlu * iRatio) shr 16;
       x1 := x1 + 3 * dx;
       inc(x3,3);
     end;
     RowDest := RowDest - iDst;
     RowSourceStart := RowSource;
   end;

   if dest.Height < 3 then exit;

   // Sharpening...
  s1 := integer(dest.ScanLine[0]);
   iDst := integer(dest.ScanLine[1]) - s1;
   ny1 := Integer(s1);
   ny2 := ny1 + iDst;
   ny3 := ny2 + iDst;
   for y := 1 to dest.Height - 2 do
   begin
     for x := 0 to dest.Width - 3 do
     begin
       x1 := x * 3;
       x2 := x1 + 3;
       x3 := x1 + 6;

       c1 := pRGB24(ny1 + x1)^;
       c2 := pRGB24(ny1 + x3)^;
       c3 := pRGB24(ny2 + x2)^;
       c4 := pRGB24(ny3 + x1)^;
       c5 := pRGB24(ny3 + x3)^;

       r := (c1.R + c2.R + (c3.R * -12) + c4.R + c5.R) div -8;
       g := (c1.G + c2.G + (c3.G * -12) + c4.G + c5.G) div -8;
       b := (c1.B + c2.B + (c3.B * -12) + c4.B + c5.B) div -8;

       if r < 0 then r := 0 else if r > 255 then r := 255;
       if g < 0 then g := 0 else if g > 255 then g := 255;
       if b < 0 then b := 0 else if b > 255 then b := 255;

       pt1 := pRGB24(ny2 + x2);
       pt1.R := r;
       pt1.G := g;
       pt1.B := b;
     end;
     inc(ny1, iDst);
     inc(ny2, iDst);
     inc(ny3, iDst);
   end;
 end;
*)

end.




