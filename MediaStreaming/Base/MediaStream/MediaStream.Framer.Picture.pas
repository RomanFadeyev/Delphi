unit MediaStream.Framer.Picture;

interface
  uses Windows,SysUtils,Classes,MediaProcessing.Definitions,MediaStream.Framer;

type
  TStreamFramerPicture  = class (TStreamFramer)
  private
    FFrameInterval: cardinal;
  public
    constructor Create; override;
    //Интервал между кадрами
    property FrameInterval: cardinal read FFrameInterval write FFrameInterval;
  end;


implementation

const DefaultFrameInterval = 5000;

{ TStreamFramerPicture }

constructor TStreamFramerPicture.Create;
begin
  inherited;
  FFrameInterval:=DefaultFrameInterval;
end;

end.


