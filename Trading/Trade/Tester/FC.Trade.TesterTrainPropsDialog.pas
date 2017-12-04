unit FC.Trade.TesterTrainPropsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, StdCtrls, ComCtrls, ActnList, ExtendControls, ExtCtrls,
  Properties.Definitions,
  FC.Definitions, VirtualTrees,
  StockChart.Definitions;

type
  TfmTesterTrainPropsDialog = class(TfmDialogOkCancel_B)
    paWorkPlace: TPanel;
    tvProperties: TVirtualStringTree;
    Label1: TLabel;
    procedure acOKUpdate(Sender: TObject);
    procedure tvPropertiesBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvPropertiesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure tvPropertiesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
  private
    function GetItemChecked(aProperty: IProperty): boolean;
  public
    procedure AddProperty(const aParent: ISCIndicator; aProperty: IProperty);

    property Checked[aProperty: IProperty]: boolean read GetItemChecked;
  end;


implementation

{$R *.dfm}

type
  TItemKind = (ikExpert,ikProperty);
  TItemData = record
    ID   : TGUID;
    Kind : TItemKind;
    Text : string;
  end;
  PItemData = ^TItemData;


{ TfmTesterTrainPropsDialog }

procedure TfmTesterTrainPropsDialog.AddProperty(const aParent: ISCIndicator; aProperty: IProperty);
var
  aNodeData : PItemData;
  aNode,aTmp : PVirtualNode;
begin
  aNode:=nil;

  aTmp:=tvProperties.GetFirst;
  while aTmp<>nil do
  begin
    aNodeData:= tvProperties.GetNodeData(aTmp);
    if (aNodeData.Kind=ikExpert) and (IsEqualGUID(aNodeData.ID,aParent.GetID)) then
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
    aNodeData.ID:=aParent.GetID;
    aNodeData.Kind:=ikExpert;
    aNodeData.Text:=aParent.GetName;
  end;
  tvProperties.Expanded[aNode]:=true;

  aNode:=tvProperties.AddChild(aNode);
  aNodeData:=tvProperties.GetNodeData(aNode);
  aNodeData.ID:=aProperty.GetID;
  aNodeData.Kind:=ikProperty;
  aNodeData.Text:=aProperty.GetCategory+'\'+aProperty.GetName;
  aNode.CheckType := ctCheckBox;

  tvProperties.CheckState[aNode]:=csCheckedNormal;
end;


function TfmTesterTrainPropsDialog.GetItemChecked(aProperty: IProperty): boolean;
var
  aNode : PVirtualNode;
  aNodeData : PItemData;
begin
  result:=false;

  aNode:=tvProperties.GetFirst;
  while aNode<>nil do
  begin
    aNodeData:= tvProperties.GetNodeData(aNode);
    if (aNodeData.Kind=ikProperty) and (IsEqualGUID(aNodeData.ID,aProperty.GetID)) then
    begin
      result:=tvProperties.CheckState[aNode]=csCheckedNormal;
      break;
    end;
    aNode:=tvProperties.GetNext(aNode);
  end;
end;

procedure TfmTesterTrainPropsDialog.tvPropertiesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TItemData);
end;

procedure TfmTesterTrainPropsDialog.tvPropertiesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  CellText:=PItemData(Sender.GetNodeData(Node)).Text;
end;

procedure TfmTesterTrainPropsDialog.tvPropertiesBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  Data: PItemData;
begin
  Data := Sender.GetNodeData(Node);
  if Data=nil then exit;

  EraseAction:=eaColor;
  if Data.Kind=ikExpert then
    ItemColor:=clSkyBlue
  else
    ItemColor:=clWindow;
end;

procedure TfmTesterTrainPropsDialog.acOKUpdate(Sender: TObject);
var
  b: boolean;
  aNode : PVirtualNode;
begin
  inherited;
  b:=false;
  aNode:=tvProperties.GetFirst;
  while aNode<>nil do
  begin
    b:=tvProperties.CheckState[aNode]=csCheckedNormal;
    if b then
      break;
    aNode:=tvProperties.GetNext(aNode);
  end;

  TAction(Sender).Enabled:=b;
end;

end.
