unit FC.Trade.FilterDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ufmDialogOKCancel_B, ActnList, StdCtrls, ExtendControls, ExtCtrls, JvExControls,
  JvComponent, JvEditorCommon, JvEditor, JvHLEditor, RecentlyList, Menus, ActnPopup, Buttons,
  PlatformDefaultStyleActnCtrls;

type
  TFilterEditorHighlighter  = class;

  TfmFilterDialog = class(TfmDialogOkCancel_B)
    mmAvailableFields: TMemo;
    Label1: TLabel;
    mmFilter: TJvHLEditor;
    rlFilters: TRecentlyList;
    Panel1: TPanel;
    buRecentlyFilters: TSpeedButton;
    pmRecentlyFilters: TPopupActionBar;
    procedure rlFiltersRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure buRecentlyFiltersClick(Sender: TObject);
    procedure mmFilterCompletionIdentifier(Sender: TObject; var Cancel: Boolean);
    procedure mmFilterCompletionDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure mmFilterCompletionMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure FormShow(Sender: TObject);
  private
    FHilighter: TFilterEditorHighlighter;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFields(aStrings: TStrings);
  end;

  TFilterEditorHighlighter = class(TJvEditorHighlighter)
  private
    FFields: TStrings;
    FFunctions: TStrings;
  protected
    procedure GetAttr(Editor: TJvHLEditor; Lines: TStrings; Line, ColBeg, ColEnd: Integer;
      LongToken: TLongTokenType; var LineAttrs: TLineAttrs); override;
    procedure ScanLongTokens(Editor: TJvHLEditor; Lines: TStrings; Line: Integer;
      var FLong: TLongTokenType); override;
    function GetRescanLongKeys(Editor: TJvHLEditor; Action: TModifiedAction;
      ACaretX, ACaretY: Integer; const Text: string): Boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function IsFunction(const S: string):boolean;
    function IsField(const S: string):boolean;
    function IsKeyword(const S: string):boolean;

    property  Fields: TStrings read FFields;
    property  Functions: TStrings read FFunctions;
  end;

implementation
  uses BaseUtils,MemoryDS,JvJCLUtils,Math,Application.Definitions;
{$R *.dfm}

const
  FilterReservedWords: array [0..5] of string = ('and','or','xor','div','mod','like');
  FilterBuiltInFunctions: array [0..10] of string = ('round','trunc','int','frac','sin','cos','tan','atan','ln','exp','sign');

{ TFilterEditorHighlighter }

constructor TFilterEditorHighlighter.Create(aOwner: TComponent);
var
  i: integer;
begin
  inherited;
  FFields:=TStringList.Create;
  FFunctions:=TStringList.Create;

  for I := 0 to High(FilterBuiltInFunctions) - 1 do
    FFunctions.Add(FilterBuiltInFunctions[i]);
end;

destructor TFilterEditorHighlighter.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FFunctions);
  inherited;
end;

procedure TFilterEditorHighlighter.GetAttr(Editor: TJvHLEditor; Lines: TStrings; Line, ColBeg,
  ColEnd: Integer; LongToken: TLongTokenType; var LineAttrs: TLineAttrs);
var
  ps0,ps1, ps2 : pchar;
  PrevSymbol: char;

function IsCharBelongsID(aChar: char): boolean;
begin
  result:=IsCharAlphaNumeric(aChar) or (aChar='_');
end;

procedure OnToken(const aX1,aX2: pchar);
var
  s: string;
  j: Integer;
  aColor: TJvSymbolColor;
begin
  if aX2<=aX1 then
    exit;

  SetLength(s, aX2-aX1);
  StrLCopy(pchar(s), aX1,aX2-aX1);

  aColor:=nil;

  if (aColor=nil) and IsRealNumericString(s) then
    aColor:=Editor.Colors.Number
  else if (aColor=nil) and CharInSet(s[1],['''','"']) then
    aColor:=Editor.Colors.Strings;

  if aColor=nil then
    if IsKeyword(s) then
        aColor:=Editor.Colors.Reserved;

  if aColor=nil then
    if IsFunction(s) then
        aColor:=Editor.Colors.FunctionCall;

  if aColor=nil then
    if IsField(s) then
        aColor:=Editor.Colors.Identifier;

  if aColor<>nil then
  begin
    for j:= aX1-ps0 to aX2-ps0 do
    begin
      LineAttrs[j].FC:=aColor.ForeColor;
      LineAttrs[j].BC:=aColor.BackColor;
      LineAttrs[j].Style:=aColor.Style;
    end;
  end;
end;

begin
  ps0:=pchar(Lines[Line]);
  ps1:=ps0;
  ps2:=ps1;
  PrevSymbol:='a';

  while ps2^<>#0 do
  begin
    if not IsCharBelongsID(ps2^) or not IsCharBelongsID(PrevSymbol)then
    begin
      OnToken(ps1,ps2);
      ps1:=ps2;
    end;

    //Одинарная строка
    if ps2^='''' then
    begin
     inc(ps2);
     while (ps2^<>'''') and (ps2^<>#0) do
       inc(ps2);
     if (ps2^='''') then inc(ps2);
     OnToken(ps1,ps2);
     ps1:=ps2;
    end
    //Двойная строка
    else if ps2^='"' then
    begin
     inc(ps2);
     while (ps2^<>'"') and (ps2^<>#0) do
       inc(ps2);
     if (ps2^='"') then inc(ps2);
     OnToken(ps1,ps2);
     ps1:=ps2;
    end;

    if (ps2^=#0) then break;

    PrevSymbol:=ps2^;
    inc(ps2);
  end;

  OnToken(ps1,ps2);
end;

function TFilterEditorHighlighter.GetRescanLongKeys(Editor: TJvHLEditor; Action: TModifiedAction;
  ACaretX, ACaretY: Integer; const Text: string): Boolean;
begin
  result:=true;
end;

function TFilterEditorHighlighter.IsField(const S: string): boolean;
begin
  result:=FFields.IndexOf(s)<>-1;
end;

function TFilterEditorHighlighter.IsFunction(const S: string): boolean;
begin
  result:=FFunctions.IndexOf(s)<>-1;
end;

function TFilterEditorHighlighter.IsKeyword(const S: string): boolean;
var
  i: integer;
begin
  result:=false;
  for I := 0 to High(FilterReservedWords) - 1 do
    if SameText(s,FilterReservedWords[i]) then
    begin
      result:=true;
      break;
    end;
end;

procedure TFilterEditorHighlighter.ScanLongTokens(Editor: TJvHLEditor; Lines: TStrings;
  Line: Integer; var FLong: TLongTokenType);
begin
  inherited;
end;

{ TfmFilterDialog }

procedure TfmFilterDialog.buRecentlyFiltersClick(Sender: TObject);
var
  aP: TPoint;
begin
  inherited;
  aP:=buRecentlyFilters.ClientToScreen(Point(buRecentlyFilters.BoundsRect.Left,buRecentlyFilters.BoundsRect.Bottom));
  pmRecentlyFilters.Popup(aP.X,aP.Y);
end;

constructor TfmFilterDialog.Create(aOwner: TComponent);
begin
  inherited;
  FHilighter:=TFilterEditorHighlighter.Create(mmFilter);
  mmFilter.Highlighter:=hlSyntaxHighlighter;
  mmFilter.SyntaxHighlighter:=FHilighter;
  rlFilters.RegistryKey:=Workspace.MainRegistryKey+'\'+Self.ClassName+'\RecentlyFilters';
  buRecentlyFilters.Enabled:=not rlFilters.IsEmpty;
end;

destructor TfmFilterDialog.Destroy;
begin
  inherited;
end;

procedure TfmFilterDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if ModalResult=mrOk then
    rlFilters.AddRecently(Trim(mmFilter.Lines.Text));
end;

procedure TfmFilterDialog.FormShow(Sender: TObject);
begin
  inherited;
  mmFilter.SetFocus;
end;

procedure TfmFilterDialog.mmFilterCompletionDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

const
  FuncTitle = 'function';
  FieldTitle = 'field';
var
  aRect: TRect;
  aCanvas : TCanvas;
  s: string;
  aItem: string;
begin
  inherited;
  aItem:=TListBox(Control).Items[Index];

  aCanvas:=TListBox(Control).Canvas;
  aCanvas.FillRect(Rect);

  inc(Rect.Left,4);
  dec(Rect.Right,4);

  aRect:=Rect;
  aRect.Right:=max(aCanvas.TextWidth(FuncTitle),aCanvas.TextWidth(FieldTitle));

  s:='';
  if FHilighter.IsField(aItem) then
    s:=FieldTitle
  else if FHilighter.IsFunction(aItem) then
    s:=FuncTitle;

  if not (odSelected in State) then
  begin
    if FHilighter.IsField(aItem) then
      aCanvas.Font.Color:=clGreen
    else if FHilighter.IsFunction(aItem) then
      aCanvas.Font.Color:=clBlue
    else
      aCanvas.Font.Color:=clWindowText;
  end
  else begin
    aCanvas.Font.Color:=clHighlightText
  end;
  aCanvas.TextOut(aRect.Left,aRect.Top,s);

  //----------

  aRect.Left:=aRect.Right+8;
  aRect.Right:=Rect.Right;

  if not (odSelected in State) then
    aCanvas.Font.Color:=clWindowText
  else
    aCanvas.Font.Color:=clHighlightText;

  aCanvas.TextOut(aRect.Left,aRect.Top,aItem);
end;

procedure TfmFilterDialog.mmFilterCompletionIdentifier(Sender: TObject; var Cancel: Boolean);
var
  s: string;
  i: integer;
begin
  if mmFilter.Lines.Count=0 then
    Cancel:=true
  else begin
    S := Trim(GetWordOnPos(mmFilter.Lines[mmFilter.CaretY], mmFilter.CaretX));
    if s='' then
      Cancel:=true
    else begin
      mmFilter.Completion.Identifiers.Clear;
      for i := 0 to FHilighter.FFields.Count - 1 do
        if StrStartsWithI(FHilighter.Fields[i],s) then
          mmFilter.Completion.Identifiers.Add(FHilighter.Fields[i]);

      for i := 0 to FHilighter.Functions.Count - 1 do
        if StrStartsWithI(FHilighter.Functions[i],s) then
          mmFilter.Completion.Identifiers.Add(FHilighter.Functions[i]);
    end;
  end;
end;

procedure TfmFilterDialog.mmFilterCompletionMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  inherited;
//
end;

procedure TfmFilterDialog.rlFiltersRecentlyMenuClick(Sender: TRecentlyList; RecentlyValue: string);
begin
  inherited;
  mmFilter.Lines.Text:=RecentlyValue;
end;

procedure TfmFilterDialog.SetFields(aStrings: TStrings);
var
  i: integer;
begin
  FHilighter.FFields.Assign(aStrings);
  mmFilter.Completion.Identifiers.Assign(aStrings);

  for i := 0 to FHilighter.FFields.Count - 1 do
    mmAvailableFields.Text:=mmAvailableFields.Text+FHilighter.FFields[i]+' ';
end;

end.
