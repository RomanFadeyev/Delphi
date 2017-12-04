{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   ƒиалог дл€ выбора индикатора дл€ вставки его в чарт

 History:
-----------------------------------------------------------------------------}

unit FC.StockChart.CreateIndicatorDialog;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  StockChart.Definitions,FC.Definitions, ComCtrls;

type
  TIndicatorNode = class(TTreeNode)
  private
    FIndicator : ISCIndicatorInfo;
  public
    property Indicator: ISCIndicatorInfo read FIndicator write FIndicator;
  end;

  TfmCreateIndicatorDialog = class(TfmDialogOkCancel_B)
    Panel1: TPanel;
    tvIndicators: TExtendTreeView;
    laHeader: TLabel;
    procedure tvIndicatorsAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure tvIndicatorsDblClick(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
  private
    FIndicators: ISCIndicatorInfoCollection;
    FKind      : TSCIndicatorKind;
    function  GetSelected: ISCIndicatorInfo;
    procedure SetSelected(const Value: ISCIndicatorInfo);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(aOwner:TComponent); override;

    procedure Init(const Value: ISCIndicatorInfoCollection; aKind:TSCIndicatorKind);

    property Indicators: ISCIndicatorInfoCollection read FIndicators;
    property SelectedIndicator: ISCIndicatorInfo read GetSelected write SetSelected;

    class function RunSingleSelection(aIndicators: ISCIndicatorInfoCollection; aKind:TSCIndicatorKind; var aSelected: ISCIndicatorINfo): boolean;

  end;


implementation

uses ufmDialog_B,FC.Factory, ufmForm_B, ufmDialogOK_B,Properties.Dialog, Types;

type
  TTreeViewFriend  = class (TExtendTreeView);

{$R *.dfm}

{ TfmCreateIndicatorDialog }

class function TfmCreateIndicatorDialog.RunSingleSelection(aIndicators: ISCIndicatorInfoCollection; aKind:TSCIndicatorKind; var aSelected: ISCIndicatorInfo): boolean;
begin
  with TfmCreateIndicatorDialog.Create(nil) do
  try
    Init(aIndicators,aKind);
    result:=ShowModal = mrOk;
     if result then
      aSelected:= SelectedIndicator;
  finally
    Free;
  end;
end;

constructor TfmCreateIndicatorDialog.Create(aOwner: TComponent);
begin
  inherited;
  TTreeViewFriend(tvIndicators).CreateWndRestores:=false;
end;

procedure TfmCreateIndicatorDialog.CreateWnd;
begin
  inherited;
  if FIndicators<>nil then
    Init(FIndicators,FKind);
end;

function TfmCreateIndicatorDialog.GetSelected: ISCIndicatorInfo;
var
  aNode: TIndicatorNode;
begin
  result:=nil;
  if (tvIndicators.Selected<>nil) and (tvIndicators.Selected is TIndicatorNode) then
  begin
    aNode:=TIndicatorNode(tvIndicators.Selected);
    result:=aNode.Indicator;
  end;
end;

procedure TfmCreateIndicatorDialog.SetSelected(const Value: ISCIndicatorInfo);
begin

end;

procedure TfmCreateIndicatorDialog.tvIndicatorsAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean);
var
  x,y: integer;
begin
  inherited;
  if  (Node is TIndicatorNode) and (TIndicatorNode(Node).Indicator=nil) then //and (Stage in [cdPrePaint, cdPreErase, cdPostErase])  then
  begin
    DefaultDraw:=false;

    Sender.Canvas.Brush.Color:=clGroupHeader;
    Sender.Canvas.FillRect(Node.DisplayRect(false));
    if cdsSelected in State then
      Sender.Canvas.Font.Color:=clHighlightText
    else
      Sender.Canvas.Font.Color:=clWindowText;

    with Node.DisplayRect(true) do
    begin
      x:=Left;
      y:=(Bottom+Top-Sender.Canvas.TextHeight(Node.Text)) div 2;
    end;

    Sender.Canvas.TextOut(x,y,Node.Text);
  end;
end;

procedure TfmCreateIndicatorDialog.tvIndicatorsDblClick(Sender: TObject);
begin
  inherited;
  acOK.Execute;
end;

procedure TfmCreateIndicatorDialog.Init(const Value: ISCIndicatorInfoCollection; aKind:TSCIndicatorKind);
var
  aCategories  : TStringList;
  i,j    : integer;
  aIndicator: ISCIndicatorInfo;
  aNode0 : TTreeNode;
  aNode1 : TTreeNode;
  aNode2 : TIndicatorNode;
begin
  FIndicators:=Value;
  FKind:=aKind;

  Caption:='Select '+IndicatorKindNamesSingle[FKind];
  laHeader.Caption:=Caption;

  tvIndicators.Items.Clear;
  tvIndicators.HandleNeeded;
  tvIndicators.Items.Clear;  

  if (FIndicators=nil) or (FIndicators.Count=0) then
    exit;

  //«аполн€ем список индикаторов
  aCategories:=TStringList.Create;
  tvIndicators.Items.BeginUpdate;
  try
    aCategories.Sorted:=true;
    aCategories.Duplicates:=dupIgnore;
    for i:=0 to FIndicators.Count-1 do
      aCategories.Add(FIndicators.Items[i].Category);

    aNode0:=nil;

    for i:=0 to aCategories.Count-1 do
    begin
      aNode1:=nil;
      for j:=0 to FIndicators.Count-1 do
      begin
        aIndicator:=FIndicators.Items[j];
        if (aIndicator.Category=aCategories[i]) and (aIndicator.Kind=FKind) then
        begin
          if aNode1=nil then
            aNode1:=tvIndicators.Items.AddChild(aNode0,aCategories[i]);

          aNode2:=TIndicatorNode.Create(tvIndicators.Items);
          //aNode2.Text:=aIndicator.Name;
          aNode2.Indicator:=aIndicator;
          tvIndicators.Items.AddNode(aNode2, aNode1, aIndicator.Name, nil, naAddChild);
        end;
      end;
    end;

    for i:=0 to tvIndicators.Items.Count-1 do
      tvIndicators.Items[i].Expanded:=true;
  finally
    tvIndicators.Items.EndUpdate;
    aCategories.Free;
  end;

  tvIndicators.AlphaSort(true);
  if tvIndicators.Items.Count>0 then
  begin
    tvIndicators.Items[0].MakeVisible;
    tvIndicators.Items[0].Focused:=true;
    tvIndicators.Items[0].Selected:=true;
  end;
end;

procedure TfmCreateIndicatorDialog.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=SelectedIndicator<>nil;
end;


end.
