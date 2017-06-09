{***********************************<_INFO>************************************}
{  <Проект>      Компоненты медиа-преобразования                               }
{                                                                              }
{  <Область>     Мультимедиа                                                   }
{                                                                              }
{  <Задача>      Преобразователь PCM. Реализация                               }
{                                                                              }
{  <Автор>       Фадеев Р.В.                                                   }
{                                                                              }
{  <Дата>        21.01.2011                                                    }
{                                                                              }
{  <Примечание>  Отсутствует                                                   }
{                                                                              }
{  <Атрибуты>    ООО НПП "Спецстрой-Связь", ООО "Трисофт"                      }
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

  //Пусть по умолчанию выходные данные совпадают с входными
  aOutInfo:=aInfo;
  aOutInfoSize:=aInfoSize;
  aOutFormat:=aInFormat; //Выходной формат у нас не меняется. Меняется только состав данных

  if self.SomeSetting<>0 then
  begin
    //Пример использования настроек
  end;

  //Пример преобразования
  case Random(2) of
    0: begin
      aOutData:=nil;
      aOutDataSize:=0; //Фрейм выкидывается
    end;
    1: begin
      //Преобразование с использованием буфера
      if cardinal(Length(FPcmBuffer))<aInDataSize then
      begin
        FPcmBuffer:=nil;  //Сначала отпустим старый буфер, чтобы был новый Allocate, а не Relocate. Это снижает фрагментацию
        SetLength(FPcmBuffer,aInDataSize);

        for i := 0 to aInDataSize-1 do
          FPcmBuffer[i]:=(PByte(aInData)+i)^ div 2;

        aOutData:=FPcmBuffer;
        aOutDataSize:=aInDataSize; //Размер выходных данных не меняется
      end;
    end;
    2: begin
      //Ничего не делаем. Выходными данными будут входные
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
