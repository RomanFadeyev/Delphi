{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   ƒиалог дл€ выбора индикатора дл€ вставки его в чарт

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.CreateTraderDialog;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  StockChart.Definitions,FC.Definitions, ComCtrls;

type
  TTraderNode = class(TTreeNode)
  private
    FTrader : IStockTraderInfo;
  public
    property Trader: IStockTraderInfo read FTrader write FTrader;
  end;

  TfmCreateTraderDialog = class(TfmDialogOkCancel_B)
    Panel1: TPanel;
    tvTraders: TExtendTreeView;
    laHeader: TLabel;
    procedure tvTradersDblClick(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
  private
    function  GetSelected: IStockTraderInfo;
    procedure SetTraders;
  public
    constructor Create(aOwner:TComponent); override;
    property SelectedTrader: IStockTraderInfo read GetSelected;
  end;

implementation

uses ufmDialog_B,FC.Singletons, ufmDialogOK_B;

type
  TTreeViewFriend  = class (TExtendTreeView);

{$R *.dfm}

{ TfmSelectTrader }

constructor TfmCreateTraderDialog.Create(aOwner: TComponent);
begin
  inherited;
  TTreeViewFriend(tvTraders).CreateWndRestores:=false;
  HandleNeeded;
  SetTraders;
end;

function TfmCreateTraderDialog.GetSelected: IStockTraderInfo;
var
  aNode: TTraderNode;
begin
  result:=nil;
  if (tvTraders.Selected<>nil) and (tvTraders.Selected is TTraderNode) then
  begin
    aNode:=TTraderNode(tvTraders.Selected);
    result:=aNode.Trader;
  end;
end;

procedure TfmCreateTraderDialog.SetTraders;
var
  aCategories  : TStringList;
  i,j    : integer;
  aTrader: IStockTraderInfo;
  aNode1 : TTreeNode;
  aNode2 : TTraderNode;
begin
  tvTraders.Items.Clear;
  tvTraders.HandleNeeded;


  if (TraderFactory.TraderCount=0) then
    exit;

  //«аполн€ем список индикаторов
  aCategories:=TStringList.Create;
  tvTraders.Items.BeginUpdate;
  try
    aCategories.Sorted:=true;
    aCategories.Duplicates:=dupIgnore;
    for i:=0 to TraderFactory.TraderCount-1 do
      aCategories.Add(TraderFactory.GetTraderInfo(i).Category);

    for i:=0 to aCategories.Count-1 do
    begin
      aNode1:=nil;
      for j:=0 to TraderFactory.TraderCount-1 do
      begin
        aTrader:=TraderFactory.GetTraderInfo(j);
        if (aTrader.Category=aCategories[i])then
        begin
          if aNode1=nil then
            aNode1:=tvTraders.Items.AddChild(nil,aCategories[i]);

          aNode2:=TTraderNode.Create(tvTraders.Items);
          //aNode2.Text:=aTrader.Name;
          aNode2.Trader:=aTrader;
          tvTraders.Items.AddNode(aNode2, aNode1, aTrader.Name, nil, naAddChild);
        end;
      end
    end;

    for i:=0 to tvTraders.Items.Count-1 do
      tvTraders.Items[i].Expanded:=true;
  finally
    tvTraders.Items.EndUpdate;
    aCategories.Free;
  end;

  tvTraders.AlphaSort(true);
  if tvTraders.Items.Count>0 then
  begin
    tvTraders.Items[0].MakeVisible;
    tvTraders.Items[0].Focused:=true;
    tvTraders.Items[0].Selected:=true;
  end;
end;

procedure TfmCreateTraderDialog.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=SelectedTrader<>nil;
end;

procedure TfmCreateTraderDialog.tvTradersDblClick(Sender: TObject);
begin
  inherited;
  acOK.Execute;
end;

end.
