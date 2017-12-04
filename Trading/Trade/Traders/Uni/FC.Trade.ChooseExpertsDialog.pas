unit FC.Trade.ChooseExpertsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls,
  Buttons, ComCtrls,FC.Definitions, ImgList, ToolWin, VirtualTrees,
  StockChart.Definitions;

type
  TfmSelectExpertsDialog = class(TfmDialogOkCancel_B)
    ToolBar1: TToolBar;
    paWorkspace: TPanel;
    tvProperties: TVirtualStringTree;
    Label1: TLabel;
    procedure tvPropertiesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure tvPropertiesBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure acOKUpdate(Sender: TObject);
    procedure tvPropertiesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    procedure SetChecked(const aStockExpert: ISCExpert; aChecked: boolean);
    function GetChecked(const aStockExpert: ISCExpert): boolean;

    function NodeByExpert(const aStockExpert: ISCExpert): PVirtualNode;
  public
    procedure AddExpert(const aStockExpert: ISCExpert);

    property Checked[const aStockExpert: ISCExpert]: boolean read GetChecked write SetChecked;
    constructor Create(aOwner:TComponent); override;
  end;


implementation

{$R *.dfm}

type
  TItemKind = (ikChart,ikExp);
  TItemData = record
    ID   : TGUID;
    Kind : TItemKind;
    Text : string;
  end;
  PItemData = ^TItemData;

{ TfmCreateTraderDialog }

constructor TfmSelectExpertsDialog.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TfmSelectExpertsDialog.AddExpert(const aStockExpert: ISCExpert);
var
  aNodeData : PItemData;
  aNode,aTmp : PVirtualNode;
  aChart: IStockChart;
begin
  aNode:=nil;

  aChart:=GetParentStockChart(aStockExpert);
  aTmp:=tvProperties.GetFirst;
  while aTmp<>nil do
  begin
    aNodeData:= tvProperties.GetNodeData(aTmp);
    if (aNodeData.Kind=ikChart) and (IsEqualGUID(aNodeData.ID,aChart.GetID)) then
    begin
      aNode:=aTmp;
      break;
    end;

    aTmp:=tvProperties.GetNext(aTmp);
  end;

  if aNode=nil then
  begin
    aNode:=tvProperties.AddChild(nil);
    aNodeData:=tvProperties.GetNodeData(aNode);
    aNodeData.ID:=aChart.GetID;
    aNodeData.Kind:=ikChart;
    aNodeData.Text:=aChart.StockSymbol.GetTimeIntervalName;
  end;
  //tvProperties.Expanded[aNode]:=true;

  aNode:=tvProperties.AddChild(aNode);
  aNodeData:=tvProperties.GetNodeData(aNode);
  aNodeData.ID:=aStockExpert.GetID;
  aNodeData.Kind:=ikExp;
  aNodeData.Text:=aStockExpert.GetName;
  aNode.CheckType := ctCheckBox;

  tvProperties.CheckState[aNode]:=csCheckedNormal;

  tvProperties.FullExpand();
end;

procedure TfmSelectExpertsDialog.acOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=true;
end;

procedure TfmSelectExpertsDialog.tvPropertiesBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  Data: PItemData;
begin
  Data := Sender.GetNodeData(Node);
  if Data=nil then exit;

  EraseAction:=eaColor;
  if Data.Kind=ikChart then
    ItemColor:=clSkyBlue
  else
    ItemColor:=clWindow;
end;

procedure TfmSelectExpertsDialog.tvPropertiesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TItemData);
end;


procedure TfmSelectExpertsDialog.tvPropertiesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  aNodeData : PItemData;
begin
  aNodeData:=Sender.GetNodeData(Node);
  if aNodeData<>nil then
    CellText:=aNodeData.Text;
end;

procedure TfmSelectExpertsDialog.SetChecked(const aStockExpert: ISCExpert; aChecked: boolean);
var
  aNode: PVirtualNode;
begin
  aNode:=NodeByExpert(aStockExpert);
  if aChecked then
    tvProperties.CheckState[aNode]:=csCheckedNormal
  else
    tvProperties.CheckState[aNode]:=csUncheckedNormal;
end;

function TfmSelectExpertsDialog.GetChecked(const aStockExpert: ISCExpert): boolean;
var
  aNode: PVirtualNode;
begin
  aNode:=NodeByExpert(aStockExpert);
  result:=tvProperties.CheckState[aNode]=csCheckedNormal;
end;

function TfmSelectExpertsDialog.NodeByExpert(const aStockExpert: ISCExpert): PVirtualNode;
var
  aTmp : PVirtualNode;
  aNodeData : PItemData;
begin
  result:=nil;
  aTmp:=tvProperties.GetFirst;
  while aTmp<>nil do
  begin
    aNodeData:= tvProperties.GetNodeData(aTmp);
    if (aNodeData.Kind=ikExp) and (IsEqualGUID(aNodeData.ID,aStockExpert.GetID)) then
    begin
      result:=aTmp;
      break;
    end;
    aTmp:=tvProperties.GetNext(aTmp);
  end;

  if result = nil then
    EStockError.Create('There is no such expert');
end;

end.
