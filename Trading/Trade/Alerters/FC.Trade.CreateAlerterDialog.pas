{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   ƒиалог дл€ выбора индикатора дл€ вставки его в чарт

 History:
-----------------------------------------------------------------------------}

unit FC.Trade.CreateAlerterDialog;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  StockChart.Definitions,FC.Definitions, ComCtrls;

type
  TAlerterNode = class(TTreeNode)
  private
    FAlerter : IStockAlerterInfo;
  public
    property Alerter: IStockAlerterInfo read FAlerter write FAlerter;
  end;

  TfmCreateAlerterDialog = class(TfmDialogOkCancel_B)
    Panel1: TPanel;
    tvAlerters: TExtendTreeView;
    laHeader: TLabel;
    procedure tvAlertersDblClick(Sender: TObject);
    procedure acOKUpdate(Sender: TObject);
  private
    function  GetSelected: IStockAlerterInfo;
    procedure SetAlerters;
  public
    constructor Create(aOwner:TComponent); override;
    property SelectedAlerter: IStockAlerterInfo read GetSelected;
  end;

implementation

uses ufmDialog_B,FC.Singletons, ufmDialogOK_B;

type
  TTreeViewFriend  = class (TExtendTreeView);

{$R *.dfm}

{ TfmSelectAlerter }

constructor TfmCreateAlerterDialog.Create(aOwner: TComponent);
begin
  inherited;
  TTreeViewFriend(tvAlerters).CreateWndRestores:=false;
  HandleNeeded;
  SetAlerters;
end;

function TfmCreateAlerterDialog.GetSelected: IStockAlerterInfo;
var
  aNode: TAlerterNode;
begin
  result:=nil;
  if (tvAlerters.Selected<>nil) and (tvAlerters.Selected is TAlerterNode) then
  begin
    aNode:=TAlerterNode(tvAlerters.Selected);
    result:=aNode.Alerter;
  end;
end;

procedure TfmCreateAlerterDialog.SetAlerters;
var
  aCategories  : TStringList;
  i,j    : integer;
  aAlerter: IStockAlerterInfo;
  aNode1 : TTreeNode;
  aNode2 : TAlerterNode;
begin
  tvAlerters.Items.Clear;
  tvAlerters.HandleNeeded;


  if (AlerterFactory.AlerterCount=0) then
    exit;

  //«аполн€ем список индикаторов
  aCategories:=TStringList.Create;
  tvAlerters.Items.BeginUpdate;
  try
    aCategories.Sorted:=true;
    aCategories.Duplicates:=dupIgnore;
    for i:=0 to AlerterFactory.AlerterCount-1 do
      aCategories.Add(AlerterFactory.GetAlerterInfo(i).Category);

    for i:=0 to aCategories.Count-1 do
    begin
      aNode1:=nil;
      for j:=0 to AlerterFactory.AlerterCount-1 do
      begin
        aAlerter:=AlerterFactory.GetAlerterInfo(j);
        if (aAlerter.Category=aCategories[i])then
        begin
          if aNode1=nil then
            aNode1:=tvAlerters.Items.AddChild(nil,aCategories[i]);

          aNode2:=TAlerterNode.Create(tvAlerters.Items);
          //aNode2.Text:=aAlerter.Name;
          aNode2.Alerter:=aAlerter;
          tvAlerters.Items.AddNode(aNode2, aNode1, aAlerter.Name, nil, naAddChild);
        end;
      end
    end;

    for i:=0 to tvAlerters.Items.Count-1 do
      tvAlerters.Items[i].Expanded:=true;
  finally
    tvAlerters.Items.EndUpdate;
    aCategories.Free;
  end;

  tvAlerters.AlphaSort(true);
  if tvAlerters.Items.Count>0 then
  begin
    tvAlerters.Items[0].MakeVisible;
    tvAlerters.Items[0].Focused:=true;
    tvAlerters.Items[0].Selected:=true;
  end;
end;

procedure TfmCreateAlerterDialog.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=SelectedAlerter<>nil;
end;

procedure TfmCreateAlerterDialog.tvAlertersDblClick(Sender: TObject);
begin
  inherited;
  acOK.Execute;
end;

end.
