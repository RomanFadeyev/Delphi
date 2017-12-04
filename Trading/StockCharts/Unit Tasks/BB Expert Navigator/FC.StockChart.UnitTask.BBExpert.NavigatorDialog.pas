unit FC.StockChart.UnitTask.BBExpert.NavigatorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogClose_B, StdCtrls, CheckLst, ComCtrls, ExtCtrls, ExtendControls,
  StockChart.Definitions.Units,
  FC.Definitions;

type
  TfmNavigator = class(TfmDialogClose_B)
    Panel2: TPanel;
    Panel1: TPanel;
    pbProgress: TProgressBar;
    buSearchNext: TButton;
    buReset: TButton;
    buPrev: TButton;
    lbAttributes: TCheckListBox;
    Label1: TLabel;
    laFound: TLabel;
    procedure buPrevClick(Sender: TObject);
    procedure buSearchNextClick(Sender: TObject);
    procedure buResetClick(Sender: TObject);
    procedure buOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FStockChart: IStockChart;
    FExpert: ISCExpertBB;
    FIndex: integer;

    function IsRequiredAttribute(aAttribute: TExpertBBGuessAttribute): boolean;
  public
    constructor Create(const aExpert: ISCExpertBB; const aStockChart: IStockChart); reintroduce;

    class procedure Run(const aExpert: ISCExpertBB; const aStockChart: IStockChart);
  end;


implementation

uses ufmDialog_B,Application.Definitions;

{$R *.dfm}

{ TfmNavigator }

procedure TfmNavigator.buOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmNavigator.buPrevClick(Sender: TObject);
begin
  laFound.Caption:='';
  if FIndex>FStockChart.GetInputData.Count then
    FIndex:=FStockChart.GetInputData.Count;

  while true do
  begin
    Dec(FIndex);
    if FIndex<0 then
      break;

    if IsRequiredAttribute(FExpert.GetGuessAttribute(FIndex)) then
    begin
      FStockChart.LocateTo(FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex),lmCenter);
      FStockChart.Hilight(FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex),
                          FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex)+1/MinsPerDay);
      break;
    end;
  end;
end;

procedure TfmNavigator.buResetClick(Sender: TObject);
begin
  inherited;
  FIndex:=-1;
end;

procedure TfmNavigator.buSearchNextClick(Sender: TObject);
begin
  laFound.Caption:='';
  if FIndex<-1 then
    FIndex:=-1;

  while true do
  begin
    Inc(FIndex);
    if FIndex>=FStockChart.GetInputData.Count then
      break;

    if IsRequiredAttribute(FExpert.GetGuessAttribute(FIndex)) then
    begin
      FStockChart.LocateTo(FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex),lmCenter);
      FStockChart.Hilight(FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex),
                          FStockChart.GetInputData.DirectGetItem_DataDateTime(FIndex)+1/MinsPerDay);
      break;
    end;
  end;
end;

constructor TfmNavigator.Create(const aExpert: ISCExpertBB; const aStockChart: IStockChart);
var
  i:TExpertBBGuessAttribute;
  b:boolean;
begin
  inherited Create(nil);
  FStockChart:=aStockChart;
  FExpert:=aExpert;

  Caption:=aExpert.GetName+' Navigator';

  for i := low(TExpertBBGuessAttribute) to High(TExpertBBGuessAttribute) do
  begin
    lbAttributes.AddItem(ExpertBBGuessAttributeNames[i],TObject(i));
    b:=i<>ebbNone;
    lbAttributes.Checked[lbAttributes.Count-1]:=Workspace.Storage(self).ReadBoolean(lbAttributes,ExpertBBGuessAttributeNames[i],b);
  end;

  buResetClick(nil);
end;

procedure TfmNavigator.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i:TExpertBBGuessAttribute;
begin
  inherited;

  for i := low(TExpertBBGuessAttribute) to High(TExpertBBGuessAttribute) do
    Workspace.Storage(self).WriteBoolean(lbAttributes,ExpertBBGuessAttributeNames[i],lbAttributes.Checked[integer(i)]);

  Action:=caFree;
end;

function TfmNavigator.IsRequiredAttribute(aAttribute: TExpertBBGuessAttribute): boolean;
begin
  result:=lbAttributes.Checked[integer(aAttribute)];
  if result then
    laFound.Caption:=lbAttributes.Items[integer(aAttribute)];
end;

class procedure TfmNavigator.Run(const aExpert: ISCExpertBB; const aStockChart: IStockChart);
begin
  with TfmNavigator.Create(aExpert,aStockChart) do
    Show;
end;

end.
