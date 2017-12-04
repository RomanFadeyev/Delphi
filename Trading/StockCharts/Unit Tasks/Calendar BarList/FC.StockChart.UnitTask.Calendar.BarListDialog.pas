unit FC.StockChart.UnitTask.Calendar.BarListDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FC.Dialogs.DockedDialogCloseAndAppWindow_B, JvDockControlForm, ImgList, JvComponentBase, JvCaptionButton, StdCtrls,
  ExtendControls, ExtCtrls,StockChart.Definitions,StockChart.Definitions.Units, FC.Definitions, Grids, DBGrids,
  MultiSelectDBGrid, ColumnSortDBGrid, EditDBGrid, DB,FC.StockData.InputDataCollectionToDataSetMediator,
  FC.StockChart.CustomDialog_B, MemoryDS;

type
  TfmCalendarBarListDialog = class(TfmStockChartCustomDialog_B)
    dsData: TDataSource;
    grData: TEditDBGrid;
    taData: TMemoryDataSet;
    taDataDate: TDateTimeField;
    taDataCountry: TStringField;
    taDataClass: TIntegerField;
    taDataIndicator: TStringField;
    taDataPeriod: TStringField;
    taDataPriority: TStringField;
    taDataPrevious: TStringField;
    taDataForecast: TStringField;
    taDataFact: TStringField;
    taDataPFTrend: TStringField;
    taDataFFTrend: TStringField;
  private
    FIndicator : ISCIndicatorCalendar;
  protected
  public
    constructor Create(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart; aTime: TDateTime); reintroduce;

    class procedure Run(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart; aTime: TDateTime);
  end;


implementation

uses ufmDialog_B,DateUtils, Application.Definitions;

{$R *.dfm}

{ TfmCalendarBarListDialog }

constructor TfmCalendarBarListDialog.Create(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart; aTime: TDateTime);
var
  aFrom,aTo: TDateTime;
  i: integer;
  aItem: ISCCalendarItem;
begin
  inherited Create(aStockChart);
  FIndicator:=aExpert;

  aFrom:=aTime;
  aTo:=aFrom+aStockChart.StockSymbol.GetTimeIntervalValue/1440;

  Caption:=IndicatorFactory.GetIndicatorInfo(FIndicator.GetIID).Name+': '+'Events from '+DateTimeToStr(aFrom)+' to '+DateTimeToStr(aTo);

  i:=FIndicator.GetData.FindFirstItemGE(aFrom);

  taData.Open;
  taData.EmptyTable;

  while i<>-1 do
  begin
    aItem:=FIndicator.GetData.GetItem(i);
    
    taData.Append;
    taDataDate.Value:=aItem.GetDateTime;
    taDataCountry.Value:=aItem.GetCountry;
    taDataClass.Value:=aItem.GetClass;
    taDataIndicator.Value:=aItem.GetIndicator;
    taDataPeriod.Value:=aItem.GetPeriod;
    taDataPriority.Value:=aItem.GetPeriod;
    taDataPrevious.Value:=aItem.GetPreviousValue;
    taDataForecast.Value:=aItem.GetForecastValue;
    taDataFact.Value:=aItem.GetFactValue;
    taDataPFTrend.Value:=CalendarChangeTypeNames[aItem.GetPFChangeType];
    taDataFFTrend.Value:=CalendarChangeTypeNames[aItem.GetFFChangeType];
    taData.Post;

    if i=FIndicator.GetData.Count-1 then
      break;

    inc(i);

    if (aItem.GetDateTime>=aTo) then
      break;
  end;
end;

class procedure TfmCalendarBarListDialog.Run(const aExpert: ISCIndicatorCalendar; const aStockChart: IStockChart; aTime: TDateTime);
begin
  with TfmCalendarBarListDialog.Create(aExpert,aStockChart,aTime) do
    Show;
end;

end.
