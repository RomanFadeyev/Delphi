{***********************************<_INFO>************************************}
{  <������>      ���������� �����-��������������                               }
{                                                                              }
{  <�������>     �����������                                                   }
{                                                                              }
{  <������>      ��������������� �����-������ � ������� BMP. ��������� �����   }
{                �� ������ ������� � ����, ����������                          }
{                ����������.                                                   }
{                                                                              }
{  <�����>       ������ �.�.                                                   }
{                                                                              }
{  <����>        21.01.2011                                                    }
{                                                                              }
{  <����������>  �����������                                                   }
{                                                                              }
{  <��������>    ��� ��� "���������-�����", ��� "�������"                      }
{                                                                              }
{***********************************</_INFO>***********************************}

unit MediaProcessing.Panorama.RGB.Impl;

interface
  uses SysUtils,Windows,Classes, Generics.Collections, SyncObjs,
       MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Panorama.RGB;

//{$DEFINE CHECK_WITH_OLD_VER}

const
   MaxPieces = 8; //������������ ���-�� �������������� ������� ��� ����������

type
  //����� ��� �������� ����� ��������. ������ ����� ������������ �� �������������� ������� �������
  TPanoramaPieceInfo = class
  private
    FLock: TCriticalSection;
    FDib: TBytes;
    FDibSize: cardinal;
    FIndex: integer;
    FXOffest: integer;
    FYOffset: integer;
    FBitmapInfoHeader: TBitmapInfoHeader;
    FReversedVertical: boolean;
    FLastDibDateTime  : TDateTime;
  public
    Valid: boolean;

    procedure SetHeader(const aHeader:TBitmapInfoHeader; aReversedVertical: boolean);
    procedure SetOffsets(aXOffest,aYOffset: integer);

    procedure StoreDib(aDib: pointer; aDibSize: integer);

    function  StitchedWidth: integer;
    function  StitchedHeght: integer;

    constructor Create(aIndex: integer);
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;
  end;

  //���������� ���������� �����-����������
  TMediaProcessor_Panorama_Rgb_Impl =class (TMediaProcessor_Panorama_Rgb,IMediaProcessorImpl)
  private
    FResultBitmapDIB: TBytes;
    FResultBitmapReversedVertical: boolean;
    FResultBitmapHeader: TBitmapInfoHeader;
    FPieceInfos: array [0..MaxPieces-1] of TPanoramaPieceInfo;
    FPieceCount: integer;
    FLastOutFrameTicks: cardinal;
    FAutoImageStitchingThread: TThread;
  protected
    function  CopyPieceToImage(const aBitmapInfoHeader: TBitmapInfoHeader; aDib: PByte; aReversedVertical: boolean; aPieceIndex: integer):boolean;
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses Math,DateUtils, BitPlane,{$IFDEF CHECK_WITH_OLD_VER}CMSA, {$ENDIF} Cmsa.Correlator, Cmsa.Image, uBaseClasses, ThreadNames;

type
  //����� ��� ���������� ��������� ������ ���������� ������� ���������� ��������
  //��������� ��������� � ������� ������, ��� ����������� �� �������� ������ ������������ ��������
  TImageStitchingThread = class (TThread)
  private
    FOwner: TMediaProcessor_Panorama_Rgb_Impl;

    procedure Stitch(aLeftImage,aRightImage: TPanoramaPieceInfo; var aPositionChanged: boolean);
    function Iteration: integer;
  protected
    constructor Create(aOwner: TMediaProcessor_Panorama_Rgb_Impl);

    procedure Execute; override;
  end;

{ TImageStitchingThread }

constructor TImageStitchingThread.Create(aOwner: TMediaProcessor_Panorama_Rgb_Impl);
begin
  FOwner:=aOwner;
  inherited Create(false);
end;

procedure TImageStitchingThread.Execute;
var
  aStart: cardinal;
  aEnd: cardinal;
  aDuration: int64;
  aFirstSuccess: boolean;
  x: integer;
  i: Integer;
  aIsNewImage: boolean;
  aLastIterationDateTime: TDateTime;
  aPiece: TPanoramaPieceInfo;
begin
  SetCurrentThreadName(ClassName);
  aFirstSuccess:=false;
  aLastIterationDateTime:=Now;

  while not Terminated do
  begin
    aStart:=GetTickCount;

    try
      x:=Iteration;
      aLastIterationDateTime:=Now;

      //�������� ������� ���� ������-�� ����������. ������ ������� ������ ������ ��������, ����� ��� ��������� �� ��������� ����� (FAutoImageStitchingIntervalSecs)
      if not aFirstSuccess then
      begin
        if x=0 then
        begin
          Pause(1000);
          continue;
        end;

        aFirstSuccess:=true;
      end;
    except
      on E:Exception do
        FOwner.SetLastError('����������: '+E.Message);
    end;

    aEnd:=GetTickCount;
    aDuration:=aEnd-aStart;
    if (aDuration<0) then
      aDuration:=0;

    aDuration:=FOwner.FAutoImageStitchingIntervalSecs*1000-aDuration;
    if aDuration>0 then
      Pause(aDuration);

    //���� ������ ������
    while not Terminated do
    begin
      aIsNewImage:=false;
      for i := 0 to High(FOwner.FPieceInfos)-1 do
      begin
        aPiece:=FOwner.FPieceInfos[i];
        aPiece.Lock;
        try
          if aPiece.Valid then
            //���� ���� ������ �����������
            if aPiece.FLastDibDateTime>aLastIterationDateTime then
            begin
              aIsNewImage:=true;
              break;
            end;
        finally
          aPiece.Unlock;
        end;
      end;

      if aIsNewImage then //���������� ���� �� ���� ��������
        break;

      Pause(100);
    end;
  end;
end;


function TImageStitchingThread.Iteration: integer;
var
  aPositionChanged: boolean;
  aFormat: TMediaStreamDataHeader;
  i: Integer;
begin
  aPositionChanged:=false;
  result:=0;

  for i := 0 to High(FOwner.FPieceInfos)-1 do
  begin
    if Terminated then
      break;

    if not (FOwner.FPieceInfos[i]).Valid then
      continue;

    if not (FOwner.FPieceInfos[i+1]).Valid then
      continue;

    FOwner.FPieceInfos[i].Lock;
    try
      FOwner.FPieceInfos[i+1].Lock;
      try
        Stitch(FOwner.FPieceInfos[i],FOwner.FPieceInfos[i+1],aPositionChanged);
        inc(result);
      finally
        FOwner.FPieceInfos[i+1].Unlock;
      end;
    finally
      FOwner.FPieceInfos[i].Unlock;
    end;
  end;

  if (aPositionChanged) then
  begin
    for i := 1 to High(FOwner.FPieceInfos) do
    begin
      if FOwner.FPieceInfos[i].Valid then
      begin
        aFormat.Assign(FOwner.FPieceInfos[i].FBitmapInfoHeader);

        (*
        FOwner.Process(
           @FOwner.FPieceInfos[i].FDib[0],
           FOwner.FPieceInfos[i].FDibSize,
           aFormat,
           nil,0);
           *)
      end;
    end;
  end;
end;

function DoSuperPos (
      const aLeftBitmapHeader: TBitmapInfoHeader;  //��������� ������ �������
      aLeftDIB: PByte; //����� ������ �������. ������ ������� ������ ���������� � BITMAPINFOHEADER::ImageSize
      const aRightBitmapHeader: TBitmapInfoHeader; //��������� ������� �������
      aRightDIB:PByte; //����� ������� �������.
      pcntMaxWinWidth, //��������� ��������� �� ����������� �� ���� ������ ����������� ����� ������������.
      pcntMaxYDisp: integer; //��������� ��������� �� ��������� ����������� ����� ����������
      out aClmnIS: integer; //���������� ��������. ����������� ������������� ��������, � ������� ������ �������� �������� �� �����
      out aVerticalOffset: integer; //�������� �� ��������� ������ �������� ������������ �����. >0 - ������ ���� �����. <0 - ������ ���� �����
      out aCorrelation: double //����������� ����������
    ):integer;
var
	image_l: TCmsaImage;
	image_r: TCmsaImage;
  aCorrelator: TCorrelator ;
  Params: TPicturesMergingParams;
begin
	image_l:=TCmsaImage.Create;
	image_r:=TCmsaImage.Create;

  try
    image_l.copyFrom (@aLeftBitmapHeader, aLeftDIB);
    image_r.copyFrom (@aRightBitmapHeader, aRightDIB);


    aCorrelator:=TCorrelator.Create;
    try
      Params:=aCorrelator.CorrelateWith(50, 25, image_l, image_r);

      aClmnIS := image_l.Width- Params.disp.x;
      aVerticalOffset := -Params.disp.y;
      aCorrelation := Params.correlation;
    finally
      aCorrelator.Free;
    end;
  finally
    image_l.Free;
    image_r.Free;
  end;


	result:=0;
end;

procedure TImageStitchingThread.Stitch(aLeftImage,aRightImage: TPanoramaPieceInfo; var aPositionChanged: boolean);
var
{$IFDEF CHECK_WITH_OLD_VER}
  aCmsaResult1: CMSA_RESULT;
  aHorizontalOffs1: integer;
  aVerticalOffs1: integer;
  aCorrelation1: double;
{$ENDIF}

  aCmsaResult2: integer;
  aHorizontalOffs2: integer;
  aVerticalOffs2: integer;
  aCorrelation2: double;
begin
  if not aLeftImage.Valid then
    exit;
  if not aRightImage.Valid then
    exit;
  if aLeftImage.FDibSize=0 then
    exit;
  if aRightImage.FDibSize=0 then
    exit;

  if (aLeftImage.FBitmapInfoHeader.biSizeImage<>aLeftImage.FDibSize) then
    raise Exception.CreateFmt('������ ���������: ����������� (%d ����) � ����������� (%d ����) ������� ����������� �%d �� ���������',[aLeftImage.FBitmapInfoHeader.biSizeImage,aLeftImage.FDibSize,aLeftImage.FIndex]);
  if (aRightImage.FBitmapInfoHeader.biSizeImage<>aRightImage.FDibSize) then
    raise Exception.CreateFmt('������ ���������: ����������� (%d ����) � ����������� (%d ����) ������� ����������� �%d �� ���������',[aRightImage.FBitmapInfoHeader.biSizeImage,aRightImage.FDibSize,aRightImage.FIndex]);

  //Test
  //x: TBitPlaneDesc;
  //x.Init(aLeftImage.FDib,aLeftImage.FDibSize,aLeftImage.FBitmapInfoHeader.biWidth,aLeftImage.FBitmapInfoHeader.biHeight,aLeftImage.FBitmapInfoHeader.biBitCount);
  //x.CopyToFileAsBitmap('C:\left.bmp',false);
  //x.Init(aRightImage.FDib,aRightImage.FDibSize,aRightImage.FBitmapInfoHeader.biWidth,aRightImage.FBitmapInfoHeader.biHeight,aRightImage.FBitmapInfoHeader.biBitCount);
  //x.CopyToFileAsBitmap('C:\Right.bmp',false);



  //aLeftBitplane.Init(aLeftImage.FDib,aLeftImage.FDibSize,aLeftImage.FBitmapInfoHeader.biWidth,aLeftImage.FBitmapInfoHeader.biHeight,aLeftImage.FBitmapInfoHeader.biBitCount);
  //aLeftGreyDib:=aLeftBitplane.GetGrayscaleDIB(cgLuminosity);

{$IFDEF CHECK_WITH_OLD_VER}
  aHorizontalOffs1:=0;
  aVerticalOffs1:=0;
  aCorrelation1:=1;
  aCmsaResult1:=CMSA.superpos(
    aLeftImage.FBitmapInfoHeader,
    @aLeftImage.FDib[0],
    aRightImage.FBitmapInfoHeader,
    @aRightImage.FDib[0],
    aHorizontalOffs1,
    aVerticalOffs1,
    aCorrelation1);
{$ENDIF}
  aHorizontalOffs2:=0;
  aVerticalOffs2:=0;
  aCorrelation2:=1;
  aCmsaResult2:=DoSuperPos(
    aLeftImage.FBitmapInfoHeader,
    @aLeftImage.FDib[0],
    aRightImage.FBitmapInfoHeader,
    @aRightImage.FDib[0],
    FOwner.FAutoImageStitchingMaxWinWidthPercent,
    FOwner.FAutoImageStitchingMaxYDispPercent,
    aHorizontalOffs2,
    aVerticalOffs2,
    aCorrelation2);

{$IFDEF CHECK_WITH_OLD_VER}
  Assert(aCmsaResult1=aCmsaResult2);
{$ENDIF}

  if aCmsaResult2=0 then
  begin
{$IFDEF CHECK_WITH_OLD_VER}
    Assert(aVerticalOffs1=aVerticalOffs2);
    Assert(aHorizontalOffs1=aHorizontalOffs2);
    Assert(Round(aCorrelation1*1000)=Round(aCorrelation2*1000));
{$ENDIF}
    if (aHorizontalOffs2<>aRightImage.FXOffest) or (aVerticalOffs2<>aRightImage.FYOffset) then
    begin
      aRightImage.SetOffsets(aHorizontalOffs2,aVerticalOffs2);
      aPositionChanged:=true;
    end;
  end
  else
    raise Exception.CreateFmt('Error code =%d',[aCmsaResult2]);
end;


{ TMediaProcessor_Panorama_Rgb_Impl }

function TMediaProcessor_Panorama_Rgb_Impl.CopyPieceToImage(const aBitmapInfoHeader: TBitmapInfoHeader; aDib: PByte; aReversedVertical: boolean; aPieceIndex: integer):boolean;
var
  aPanoramaWidth: integer;
  aPanoramaHeight: integer;
  i: integer;
  x,y,ys: Integer;
  aDestStride,aSourceStride: integer;
  aPtrDst,aPtrSrc: PByte;
  aPieceWidth: integer;
begin
  result:=false;
  //�������, �� ������� �������� ����� �������� ���� ��������, ��� ����� ��������� �������� �������
  FPieceCount:=max(FPieceCount,aPieceIndex+1);

  aPanoramaWidth:=0;
  aPanoramaHeight:=aBitmapInfoHeader.biHeight;
  //�������, ����� ������ ��������� ��� ������� ������������
  for i := 0 to FPieceCount-1 do
  begin
    if FPieceInfos[i].Valid then
    begin
      inc(aPanoramaWidth,FPieceInfos[i].StitchedWidth);
      aPanoramaHeight:=min(aPanoramaHeight,FPieceInfos[i].StitchedHeght);
    end
    else begin //���� �� ����� �������, �� �����������, ��� ��� ������ ����� ������
      inc(aPanoramaWidth,aBitmapInfoHeader.biWidth);
    end;
  end;

  if (aPanoramaWidth<=0) or (aPanoramaHeight<=0) then
    exit;

  //���� ������� �� �������� �
  if (aPanoramaWidth<>FResultBitmapHeader.biWidth) or (aPanoramaHeight<>FResultBitmapHeader.biHeight) then
  begin
    FResultBitmapHeader.biWidth:=aPanoramaWidth;
    FResultBitmapHeader.biHeight:=aPanoramaHeight;
    FResultBitmapHeader.biBitCount:=24;
    FResultBitmapHeader.biSizeImage:=GetRGBSize(aPanoramaWidth,aPanoramaHeight,24);

    FResultBitmapDIB:=nil;
    SetLength(FResultBitmapDIB,FResultBitmapHeader.biSizeImage);

    if FPieceInfos[0].Valid then
      FResultBitmapReversedVertical:=FPieceInfos[0].FReversedVertical
    else
      FResultBitmapReversedVertical:=aReversedVertical;
  end;


  x:=0;
  for i := 0 to aPieceIndex-1 do
    if FPieceInfos[i].Valid then
      inc(x,FPieceInfos[i].StitchedWidth)
    else
      inc(x,aBitmapInfoHeader.biWidth);

  if aPieceIndex>0 then
    dec(x,FPieceInfos[aPieceIndex].FXOffest);

  if (x<0) then //�������� ����� � ������������� ������� (������� "�����")
    exit;

  //��������� ��������
  aSourceStride:=GetRGBLineSize(aBitmapInfoHeader.biWidth,aBitmapInfoHeader.biBitCount);
  aDestStride:=GetRGBLineSize(FResultBitmapHeader.biWidth,FResultBitmapHeader.biBitCount);
  aPieceWidth:=aBitmapInfoHeader.biWidth;
  if (aPieceIndex<MaxPieces-1) then
    if (FPieceInfos[aPieceIndex+1].Valid) then
      if FPieceInfos[aPieceIndex+1].FXOffest>0 then
        dec(aPieceWidth,FPieceInfos[aPieceIndex+1].FXOffest);

  //TODO ������������ ������� ����� �� bitplane
  for y := 0 to aPanoramaHeight-1 do
  begin
    ys:=y+FPieceInfos[aPieceIndex].FYOffset;
    if (ys<0) then
      continue;

    if (ys>=aBitmapInfoHeader.biHeight) then
      break;

    aPtrSrc:=aDib+(ys*aSourceStride);
    aPtrDst:=@FResultBitmapDIB[y*aDestStride+x*3];
    CopyMemory(aPtrDst,aPtrSrc,aPieceWidth*3);
  end;

  result:=true;
end;

constructor TMediaProcessor_Panorama_Rgb_Impl.Create;
var
  i: Integer;
begin
  inherited;
  FResultBitmapHeader.biSize:=sizeof(FResultBitmapHeader);
  for i := 0 to High(FPieceInfos) do
    FPieceInfos[i]:=TPanoramaPieceInfo.Create(i);
end;

destructor TMediaProcessor_Panorama_Rgb_Impl.Destroy;
var
  i: Integer;
begin
  inherited;

  FResultBitmapDIB:=nil;
  FreeAndNil(FAutoImageStitchingThread);
  for i := 0 to High(FPieceInfos) do
   FreeAndNil(FPieceInfos[i]);
end;

procedure TMediaProcessor_Panorama_Rgb_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  aStartTicks: cardinal;
  aDT: TDateTime;
  aPiece: TPanoramaPieceInfo;
begin
  aStartTicks:=GetTickCount;
  TArgumentValidation.NotNil(aInData);

  Assert(aInFormat.biMediaType=mtVideo);
  Assert(aInFormat.biStreamType=stRGB);

  aOutData:=nil;
  aOutDataSize:=0;
  aOutInfo:=nil;
  aOutInfoSize:=0;
  aOutFormat:=aInFormat;

  if (aInFormat.VideoBitCount<>24) then
  begin
    SetLastError(Format('������ RGB ������ ���� ������� ����� 24 ����. ����������� ������ - %d ���',[aInFormat.VideoBitCount]));
    aOutFormat.Clear;
    exit;
  end;


  if aInFormat.Channel>=MaxPieces then
    exit; //������ MaxPieces ���������� �������� �� ���������


  if not CopyPieceToImage(aInFormat.ToBitmapInfoHeader(aInDataSize),aInData,aInFormat.VideoReversedVertical, aInFormat.Channel) then
   exit;

  aPiece:=FPieceInfos[aInFormat.Channel];
  aPiece.Lock;
  try
    aPiece.SetHeader(aInFormat.ToBitmapInfoHeader(aInDataSize),aInFormat.VideoReversedVertical);
    if (FAutoImageStitching) then
    begin
      aDT:=aPiece.FLastDibDateTime;
      if (aDT=0) or ((Now-aDT)*MSecsPerDay>FAutoImageStitchingIntervalSecs*1000-1000) then
        aPiece.StoreDib(aInData,aInDataSize)
      else begin
        //Assert(true); //��� �������
      end;
    end;
  finally
    aPiece.Unlock;
  end;


  if aStartTicks-FLastOutFrameTicks>cardinal(1000 div FFPSValue) then
  begin
    aOutData:=FResultBitmapDIB;
    aOutDataSize:=FResultBitmapHeader.biSizeImage;
    aOutFormat.Assign(FResultBitmapHeader);
    aOutFormat.TimeStamp:=GetTickCount;
    aOutFormat.TimeKoeff:=1;
    aOutFormat.VideoReversedVertical:=FResultBitmapReversedVertical;
    FLastOutFrameTicks:=aStartTicks;
  end;

  //���� ����� ���������� �������� ��� �� ������� - ���������
  if FAutoImageStitching then
   if FAutoImageStitchingThread=nil then
     FAutoImageStitchingThread:=TImageStitchingThread.Create(self);
end;


{ TPanoramaPieceInfo }

constructor TPanoramaPieceInfo.Create(aIndex: integer);
begin
  FLock:=TCriticalSection.Create;
  FIndex:=aIndex;
end;

destructor TPanoramaPieceInfo.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TPanoramaPieceInfo.Lock;
begin
  FLock.Enter;
end;

procedure TPanoramaPieceInfo.SetHeader(const aHeader: TBitmapInfoHeader; aReversedVertical: boolean);
begin
  Lock;
  try
    FBitmapInfoHeader:=aHeader;
    FReversedVertical:=aReversedVertical;
    Valid:=true;
  finally
    Unlock;
  end;
end;

procedure TPanoramaPieceInfo.SetOffsets(aXOffest,aYOffset: integer);
begin
  FXOffest:=aXOffest;
  FYOffset:=aYOffset;
end;

function TPanoramaPieceInfo.StitchedHeght: integer;
begin
  Lock;
  try
    result:=FBitmapInfoHeader.biHeight+Abs(FYOffset);
  finally
    Unlock;
  end;
end;

function TPanoramaPieceInfo.StitchedWidth: integer;
begin
  Lock;
  try
    result:=FBitmapInfoHeader.biWidth-FXOffest;
  finally
    Unlock;
  end;
end;

procedure TPanoramaPieceInfo.StoreDib(aDib: pointer; aDibSize: integer);
begin
  Lock;
  try
    FLastDibDateTime:=Now;
    if Length(FDib)<aDibSize then
    begin
      FDib:=nil;
      SetLength(FDib,aDibSize);
    end;

    CopyMemory(@FDib[0],aDib,aDibSize);
    FDibSize:=aDibSize;
  finally
    Unlock;
  end;
end;

procedure TPanoramaPieceInfo.Unlock;
begin
  FLock.Leave;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Panorama_Rgb_Impl);

end.
