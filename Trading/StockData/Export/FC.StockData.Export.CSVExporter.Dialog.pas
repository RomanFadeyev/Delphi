unit FC.StockData.Export.CSVExporter.Dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  StockChart.Definitions,FC.Definitions, StockChart.Obj, ComCtrls;

type
  TfmExportDialog = class(TfmDialogOkCancel_B)
    Label1: TLabel;
    cbDateFormat: TExtendComboBox;
    Label2: TLabel;
    cbSeparator: TExtendComboBox;
    Label3: TLabel;
    mmPreview: TMemo;
    pbProgress: TProgressBar;
    Label4: TLabel;
    cbTimeFormat: TExtendComboBox;
    Bevel1: TBevel;
    ckExportFrom: TExtendCheckBox;
    dtDateFrom: TExtendDateTimePicker;
    dtTimeFrom: TExtendDateTimePicker;
    dtTimeTo: TExtendDateTimePicker;
    dtDateTo: TExtendDateTimePicker;
    ckExportTo: TExtendCheckBox;
    procedure acOKUpdate(Sender: TObject);
    procedure OnSettingsChange(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
  private
    FDS: IStockDataSource;
    FStream: TStream;
    FLocked: boolean;

    procedure ExportInternal(aStream: TStream; aCount: integer);
  public
    constructor Create(AOwner: TComponent); override;

    class procedure Run(const aDS: IStockDataSource; aStream: TStream);
  end;

implementation

uses Math,
     SystemService,
     FC.Factory,
     FC.DataUtils;

{$R *.dfm}

const
  DateFormat1 = 'YYYY.MM.DD';
  DateFormat2 = 'DD.MM.YYYY';
  DateFormat3 = 'DD.MM.YY';
  DateFormat4 = 'YYYYMMDD';

  TimeFormat1 = 'HH:MM';
  TimeFormat2 = 'HH:MM:SS';
  TimeFormat3 = 'HHMM';
  TimeFormat4 = 'HHMMSS';

{ TfmExportDialog }

procedure TfmExportDialog.OnSettingsChange(Sender: TObject);
var
  aStream: TMemoryStream;
begin
  if FLocked then
    exit;

  aStream:=TStringStream.Create('');
  try
    ExportInternal(aStream,min(10,FDS.RecordCount));
    mmPreview.Text:=PAnsiChar(aStream.Memory);
  finally
    aStream.Free;
  end;
end;

procedure TfmExportDialog.acOKUpdate(Sender: TObject);
begin
  inherited;
  dtDateFrom.Enabled:=ckExportFrom.Checked;
  dtTimeFrom.Enabled:=ckExportFrom.Checked;

  dtDateTo.Enabled:=ckExportTo.Checked;
  dtTimeTo.Enabled:=ckExportTo.Checked;
end;

constructor TfmExportDialog.Create(AOwner: TComponent);
begin
  inherited;
  FLocked:=true;

  cbDateFormat.Clear;
  cbDateFormat.Items.Add(DateFormat1);
  cbDateFormat.Items.Add(DateFormat2);
  cbDateFormat.Items.Add(DateFormat3);
  cbDateFormat.Items.Add(DateFormat4);
  cbDateFormat.ItemIndex:=0;

  cbTimeFormat.Clear;
  cbTimeFormat.Items.Add(TimeFormat1);
  cbTimeFormat.Items.Add(TimeFormat2);
  cbTimeFormat.Items.Add(TimeFormat3);
  cbTimeFormat.Items.Add(TimeFormat4);
  cbTimeFormat.ItemIndex:=0;

  dtTimeTo.DateTime:=Trunc(Now);
  dtTimeFrom.DateTime:=Trunc(Now);

  cbSeparator.Clear;
  cbSeparator.Items.Add(',');
  cbSeparator.Items.Add(';');
  cbSeparator.Items.Add('tab');
  cbSeparator.ItemIndex:=0;

  FLocked:=false;
end;

procedure TfmExportDialog.acOKExecute(Sender: TObject);
begin
  inherited;
  ModalResult:=mrNone;

  pbProgress.Position:=0;
  if FDS.RecordCount>100 then
    pbProgress.Visible:=true;
  try
    ExportInternal(FStream,FDS.RecordCount);
  finally
    pbProgress.Visible:=false;
  end;

  ModalResult:=mrOk;
end;

procedure TfmExportDialog.ExportInternal(aStream: TStream; aCount: integer);
var
  i: integer;
  s: AnsiString;
  FormatMath: TFormatSettings;
  aPercent: integer;
  aDateFormat: string;
  aTimeFormat: string;
  aSeparator: string;
  aFrom,aTo: TDateTime;
  b       : boolean;
begin
  GetLocaleFormatSettings(GetThreadLocale,FormatMath);
  FormatMath.DecimalSeparator:='.';

  TWaitCursor.SetUntilIdle;
  aPercent:=0;

  aDateFormat:=AnsiLowerCase(cbDateFormat.Text);
  aTimeFormat:=StringReplace(AnsiLowerCase(cbTimeFormat.Text),'mm','nn',[]);
  aSeparator:=cbSeparator.Text;
  if AnsiSameText(aSeparator,'tab') then
    aSeparator:=#9;

  aFrom:=-1;
  aTo:=-1;

  if ckExportFrom.Checked then
    aFrom:=Trunc(dtDateFrom.DateTime)+Frac(dtTimeFrom.DateTime);

  if ckExportTo.Checked then
    aTo:=Trunc(dtDateTo.DateTime)+Frac(dtTimeTo.DateTime);

  for i := 0 to aCount- 1 do
  begin
    b:=true;

    if aFrom>=0 then
      if FDS.GetDataDateTime(i)<aFrom then
        b:=false;

    if aTo>=0 then
      if FDS.GetDataDateTime(i)>aTo then
      begin
        pbProgress.Position:=pbProgress.Max;
        break;
      end;

    if b then
    begin
      s:= FormatDateTime(aDateFormat,FDS.GetDataDateTime(i))+aSeparator+
          FormatDateTime(aTimeFormat,FDS.GetDataDateTime(i))+aSeparator+
          PriceToStr(FDS, FDS.GetDataOpen(i))+aSeparator+
          PriceToStr(FDS,FDS.GetDataHigh(i))+aSeparator+
          PriceToStr(FDS,FDS.GetDataLow(i))+aSeparator+
          PriceToStr(FDS,FDS.GetDataClose(i))+aSeparator+
          IntToStr(FDS.GetDataVolume(i))+#13#10;
      aStream.Write(PAnsiChar(s)^,Length(s));
    end;

    if (aCount>100) and (Round(i/aCount*100)<>aPercent) then
    begin
      aPercent:=Round(i/FDS.RecordCount*100);
      pbProgress.Position:=aPercent;
      self.Repaint;
    end;
  end;


end;

class procedure TfmExportDialog.Run(const aDS: IStockDataSource; aStream: TStream);
begin
  with TfmExportDialog.Create(nil) do
  try
    FDS:=aDS;
    FStream:=aStream;
    OnSettingsChange(nil);
    ShowModal;
  finally
    Free;
  end;
end;

end.
