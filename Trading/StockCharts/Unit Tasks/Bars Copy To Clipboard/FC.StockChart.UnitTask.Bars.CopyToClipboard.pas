unit FC.StockChart.UnitTask.Bars.CopyToClipboard;


interface
{$I Compiler.inc}

uses
  SysUtils,Classes, BaseUtils, Serialization, StockChart.Definitions.Units,StockChart.Definitions,
  FC.Definitions, FC.Singletons,
  FC.StockChart.UnitTask.Base;

implementation
  uses Clipbrd,StockChart.Definitions.Drawing;

type
  //Ñïåöèàëüíûé Unit Task äëÿ àâòîìàòè÷åñêîãî ïîäáîğà ğàçìåğîâ ãğàíè è äëèíû ïåğèîäà.
  //Ïîäşîğ îóñùåñòüâëÿåòñÿ ïğÿìûì ïåğåáîğîì.
  TStockUnitTaskBarsCopyToClipboard = class(TStockUnitTaskBase)
  public
    function  CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean; override;
    procedure Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition); override;
  end;

{ TStockUnitTaskBarsCopyToClipboard }

function TStockUnitTaskBarsCopyToClipboard.CanApply(const aIndicator: ISCIndicator; out aOperationName: string): boolean;
begin
  result:=Supports(aIndicator,ISCIndicatorBars);
  if result then
    aOperationName:='Bars: Copy Item To Clipboard';
end;

procedure TStockUnitTaskBarsCopyToClipboard.Perform(const aIndicator: ISCIndicator; const aStockChart: IStockChart; const aCurrentPosition: TStockPosition);
var
  s: string;
  i: integer;
begin
  if aCurrentPosition.X>0 then
  begin
    i:=Trunc(aCurrentPosition.X);
    if (i>0) and (i<aStockChart.GetInputData.Count) then
    begin
      s:='Time: '+DefaultFormatter.DateTimeToStr(aStockChart.GetInputData.DirectGetItem_DataDateTime(i),false,true);
      s:=s+#13#10'OHLCV: '+
        DefaultFormatter.RealToStr(aStockChart.GetInputData[i].DataOpen)+';'+
        DefaultFormatter.RealToStr(aStockChart.GetInputData[i].DataHigh)+';'+
        DefaultFormatter.RealToStr(aStockChart.GetInputData[i].DataLow)+';'+
        DefaultFormatter.RealToStr(aStockChart.GetInputData[i].DataClose)+';'+
        IntToStr(aStockChart.GetInputData[i].DataVolume);

      Clipboard.Open;
      Clipboard.AsText:=s;
      Clipboard.Close;
    end;
  end;
end;

initialization
  //Ğåãèñòğèğóåì Unit Task
  StockUnitTaskRegistry.AddUnitTask(TStockUnitTaskBarsCopyToClipboard.Create);


end.
