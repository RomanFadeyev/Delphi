unit FC.StockChart.CustomDialog_B;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FC.Dialogs.DockedDialogAndAppWindow_B, ImgList, JvCaptionButton, JvComponentBase, JvDockControlForm,
  FC.Definitions, FC.Dialogs.DockedDialogCloseAndAppWindow_B, StdCtrls, ExtendControls, ExtCtrls;

type
  TfmStockChartCustomDialog_B = class(TfmDockedDialogCloseAndAppWindow_B,IStockProjectEventHandler)
  private
    FStockChart: IStockChart;
  protected
    procedure DoClose(var Action: TCloseAction); override;  
  public
    constructor Create(const aStockChart: IStockChart); reintroduce; virtual;

    //from IStockProjectEventHandler
    procedure OnCloseProject(const aSender: IStockProject);    
    procedure OnCloseStockChart  (const aSender: IStockProject; const aChart: IStockChart);
    //end of IStockProjectEventHandler

    property StockChart: IStockChart read FStockChart;
  end;

implementation

{$R *.dfm}

{ TfmStockChartCustomDialog_B }

constructor TfmStockChartCustomDialog_B.Create(const aStockChart: IStockChart);
begin
  Assert(aStockChart<>nil);
  inherited Create(nil);
  FStockChart:=aStockChart;
  FStockChart.GetProject.AddEventHandler(self);
end;

procedure TfmStockChartCustomDialog_B.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action:=caFree;
  if FStockChart<>nil then
  begin
    FStockChart.GetProject.RemoveEventHandler(self);
    //FStockChart:=nil;
  end;
end;

procedure TfmStockChartCustomDialog_B.OnCloseProject(const aSender: IStockProject);
begin

end;

procedure TfmStockChartCustomDialog_B.OnCloseStockChart(const aSender: IStockProject; const aChart: IStockChart);
begin
  if (FStockChart<>nil) and IsEqualGUID(aChart.GetID, FStockChart.GetID) then
    self.Close;
end;

end.
