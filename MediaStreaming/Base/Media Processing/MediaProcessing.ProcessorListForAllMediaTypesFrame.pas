unit MediaProcessing.ProcessorListForAllMediaTypesFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,MediaProcessing.Definitions, MediaProcessing.ProcessorListFrame;

type
  TfrmMediaProcessorListForAllMediaTypes = class(TFrame)
    pcMediaTypes: TPageControl;
  private
    FProcessorListFrames: array [TMediaType] of TfrmMediaProcessorList;
    FOnDataChanged: TNotifyEvent;

    procedure OnProcessorListDataChanged(aSender: TObject);
    function GetProcessorList(aIndex: TMediaType): TfrmMediaProcessorList;
    function GetOriginalStreamType(aIndex: TMediaType): TStreamType;
    function GetOutputStreamType(aIndex: TMediaType): TStreamType;
    procedure SetOriginalStreamType(aIndex: TMediaType;
      const Value: TStreamType);
    function GetPageVisible(aIndex: TMediaType): boolean;
    procedure SetPageVisible(aIndex: TMediaType; const Value: boolean);
  public
    constructor Create(aOwner: TComponent); override;

    property ProcessorList[aIndex: TMediaType]: TfrmMediaProcessorList read GetProcessorList;
    property OriginalStreamType[aIndex: TMediaType]: TStreamType read GetOriginalStreamType write SetOriginalStreamType;
    property OutputStreamType[aIndex: TMediaType]: TStreamType read GetOutputStreamType;
    property PageVisible[aIndex: TMediaType]: boolean read GetPageVisible write SetPageVisible;


    property  OnDataChanged: TNotifyEvent read  FOnDataChanged write FOnDataChanged;
  end;

implementation

{$R *.dfm}

{ TFrame1 }

constructor TfrmMediaProcessorListForAllMediaTypes.Create(aOwner: TComponent);
var
  aMediaType: TMediaType;
  aPage:TTabSheet;
  i,j: Integer;
begin
  inherited Create(aOwner);

  for aMediaType:=Low(TMediaType) to High(TMediaType) do
  begin
    aPage:=TTabSheet.Create(pcMediaTypes);
    aPage.PageControl:=pcMediaTypes;
    aPage.Caption:=MediaTypeNames[aMediaType];
    FProcessorListFrames[aMediaType]:=TfrmMediaProcessorList.Create(self);
    FProcessorListFrames[aMediaType].Name:='MediaType'+IntToStr(integer(aMediaType));
    FProcessorListFrames[aMediaType].Parent:=aPage;
    FProcessorListFrames[aMediaType].Align:=alClient;
    FProcessorListFrames[aMediaType].OnDataChanged:=OnProcessorListDataChanged;
    FProcessorListFrames[aMediaType].ParentBackground:=true;
    FProcessorListFrames[aMediaType].FillOriginalStreamTypeList(aMediaType);
    FProcessorListFrames[aMediaType].AcceptableMediaTypes:=[aMediaType];

    with FProcessorListFrames[aMediaType] do
    begin
      for j:= 0 to OriginalStreamTypeListCount-1 do
        for i := 0 to OriginalStreamTypeListCount-1 do
        begin
          if not IsAppropriateToMediaType(OriginalStreamTypeList[i],aMediaType) then
          begin
            FProcessorListFrames[aMediaType].RemoveFromOriginalStreamTypeList(OriginalStreamTypeList[i]);
            break;
          end;
        end;
    end;


    pcMediaTypes.ActivePageIndex:=0;
  end;
end;

function TfrmMediaProcessorListForAllMediaTypes.GetOriginalStreamType(aIndex: TMediaType): TStreamType;
begin
  result:=FProcessorListFrames[aIndex].OriginalStreamType;
end;

function TfrmMediaProcessorListForAllMediaTypes.GetOutputStreamType(aIndex: TMediaType): TStreamType;
begin
  result:=FProcessorListFrames[aIndex].OutputStreamType;
end;

function TfrmMediaProcessorListForAllMediaTypes.GetPageVisible(
  aIndex: TMediaType): boolean;
begin
  result:=pcMediaTypes.Pages[integer(aIndex)].TabVisible;
end;

function TfrmMediaProcessorListForAllMediaTypes.GetProcessorList(aIndex: TMediaType): TfrmMediaProcessorList;
begin
  result:=FProcessorListFrames[aIndex];
end;

procedure TfrmMediaProcessorListForAllMediaTypes.OnProcessorListDataChanged(aSender: TObject);
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(self);
end;

procedure TfrmMediaProcessorListForAllMediaTypes.SetOriginalStreamType(aIndex: TMediaType; const Value: TStreamType);
begin
  FProcessorListFrames[aIndex].OriginalStreamType:=Value;
end;

procedure TfrmMediaProcessorListForAllMediaTypes.SetPageVisible(
  aIndex: TMediaType; const Value: boolean);
begin
  pcMediaTypes.Pages[integer(aIndex)].TabVisible:=Value;
end;

end.
