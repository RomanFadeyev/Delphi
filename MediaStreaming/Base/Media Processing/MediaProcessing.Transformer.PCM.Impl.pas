{***********************************<_INFO>************************************}
{  <������>      ���������� �����-��������������                               }
{                                                                              }
{  <�������>     �����������                                                   }
{                                                                              }
{  <������>      ��������������� PCM. ����������                               }
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
unit MediaProcessing.Transformer.Pcm.Impl;

interface
  uses SysUtils,Windows,Classes, Graphics, BitmapStreamMediator,
       MediaProcessing.Definitions,MediaProcessing.Global,MediaProcessing.Transformer.Pcm;

type
  TMediaProcessor_Transformer_Pcm_Impl =class (TMediaProcessor_Transformer_Pcm,IMediaProcessorImpl)
  private
    FPcmBuffer: TBytes;
  protected
    procedure Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
  uses uBaseClasses;

{ TMediaProcessor_Transformer_Pcm_Impl }

constructor TMediaProcessor_Transformer_Pcm_Impl.Create;
begin
  inherited;

end;
//------------------------------------------------------------------------------
destructor TMediaProcessor_Transformer_Pcm_Impl.Destroy;
begin
  FPcmBuffer:=nil;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TMediaProcessor_Transformer_Pcm_Impl.Process(aInData: pointer; aInDataSize:cardinal; const aInFormat: TMediaStreamDataHeader; aInfo: pointer; aInfoSize: cardinal;
                      out aOutData: pointer; out aOutDataSize: cardinal; out aOutFormat: TMediaStreamDataHeader; out aOutInfo: pointer; out aOutInfoSize: cardinal);
var
  i: Integer;
begin
  TArgumentValidation.NotNil(aInData);

  //����� �� ��������� �������� ������ ��������� � ��������
  aOutInfo:=aInfo;
  aOutInfoSize:=aInfoSize;
  aOutFormat:=aInFormat; //�������� ������ � ��� �� ��������. �������� ������ ������ ������

  if self.SomeSetting<>0 then
  begin
    //������ ������������� ��������
  end;

  //������ ��������������
  case Random(2) of
    0: begin
      aOutData:=nil;
      aOutDataSize:=0; //����� ������������
    end;
    1: begin
      //�������������� � �������������� ������
      if cardinal(Length(FPcmBuffer))<aInDataSize then
      begin
        FPcmBuffer:=nil;  //������� �������� ������ �����, ����� ��� ����� Allocate, � �� Relocate. ��� ������� ������������
        SetLength(FPcmBuffer,aInDataSize);

        for i := 0 to aInDataSize-1 do
          FPcmBuffer[i]:=(PByte(aInData)+i)^ div 2;

        aOutData:=FPcmBuffer;
        aOutDataSize:=aInDataSize; //������ �������� ������ �� ��������
      end;
    end;
    2: begin
      //������ �� ������. ��������� ������� ����� �������
      aOutData:=aInData;
      aOutDataSize:=aInDataSize;
    end
    else begin
      raise EAlgoError.Create;
    end;
  end;
end;

initialization
  MediaProceccorFactory.RegisterMediaProcessorImplementation(TMediaProcessor_Transformer_Pcm_Impl);

end.
