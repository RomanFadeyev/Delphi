{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   Фрейми, соединяющий библиотечный Chart с проектом

 History:
-----------------------------------------------------------------------------}

unit FC.StockChart.frmStockChart;
{$I Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus,
  ActnPopup, ActnList, ActnMan,  ActnMenus, StdCtrls,ComCtrls, ToolWin, ExtCtrlsSupport, Dialogs,BaseUtils, Serialization,
  Generics.Collections, Collections.Map, Properties.Obj,
  FC.Definitions, FC.Singletons, ufrmFrame_B, StockChart,
  StockChart.Definitions,StockChart.Definitions.Drawing,StockChart.Definitions.Units, ExtendControls, RecentlyList,
  FC.fmUIDataStorage, ExtCtrls, PlatformDefaultStyleActnCtrls;

const
  CM_POSTACTIVATE  = WM_USER + $200;
  CM_ENDGOTOBOX  = WM_USER + $201;

type
  TStockChartContainer = class;
  TCrosshairPosition = record
    X: integer;
    Y: TStockRealNumber;
  end;

  //Собственно фрейм
  TfrmStockChart = class(TfrmFrame_B,IPropertyChangeHandler)
    alMain: TActionList;
    acEditIndicator: TAction;
    acDeleteIndicator: TAction;
    pmChart: TPopupActionBar;
    miInsertIndicator: TMenuItem;
    miModifyIndicator: TMenuItem;
    pmIndicator: TPopupActionBar;
    miIndicatorEdit: TMenuItem;
    miIndicatorDelete: TMenuItem;
    miDeleteIndicator: TMenuItem;
    miInsertExpert: TMenuItem;
    miInsertShape: TMenuItem;
    N1: TMenuItem;
    miModifyExpert: TMenuItem;
    miDeleteExpert: TMenuItem;
    miModify: TMenuItem;
    miDelete: TMenuItem;
    N2: TMenuItem;
    miExpertTester: TMenuItem;
    acExpertTester: TAction;
    acInvalidateIndicator: TAction;
    N3: TMenuItem;
    Invalidate1: TMenuItem;
    N4: TMenuItem;
    InvalidateAll1: TMenuItem;
    acInvalidateAll: TAction;
    acSyncRegion: TAction;
    SyncHilightRegion1: TMenuItem;
    SyncHilightRegion2: TMenuItem;
    laSymbol: TLabel;
    acReplayTicksInRegion: TAction;
    N5: TMenuItem;
    ReplayTicksinRegion1: TMenuItem;
    SyncHilightRegion3: TMenuItem;
    Options1: TMenuItem;
    FitAll1: TMenuItem;
    miModifyShape: TMenuItem;
    miDeleteShape: TMenuItem;
    acReplayTicksInRegion2: TAction;
    ReplayTicksinRegionOver1: TMenuItem;
    miTasks: TMenuItem;
    acShiftTicks: TAction;
    Shiftticksfromhere1: TMenuItem;
    cbGoto: TExtendComboBox;
    laPosition: TLabel;
    ShowCurrentPosition1: TMenuItem;
    ShowCrosshair1: TMenuItem;
    N6: TMenuItem;
    rlGoto: TRecentlyList;
    SyncCursorPositiononAllCharts1: TMenuItem;
    N7: TMenuItem;
    FixVerticalScale1: TMenuItem;
    N8: TMenuItem;
    imScale: TImage;
    IndicatorWindow1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    acIndicatorWindowMoveUp: TAction;
    acIndicatorWindowMoveDown: TAction;
    acChangeIndicatorDataSource: TAction;
    miCharacteristics: TMenuItem;
    miIndicatorName: TMenuItem;
    acGetTicksInRegion: TAction;
    GetTicksInRegion1: TMenuItem;
    odTheme: TOpenDialog;
    sdTheme: TSaveDialog;
    acBoldIndicator: TAction;
    Bold1: TMenuItem;
    Copy1: TMenuItem;
    acCopyIndicator: TAction;
    N9: TMenuItem;
    acPasteIndicator: TAction;
    Paste1: TMenuItem;
    acIndicatorBringToFront: TAction;
    acIndicatorSendToBack: TAction;
    Position1: TMenuItem;
    BringToFront1: TMenuItem;
    SendToBack1: TMenuItem;
    pmWindow: TPopupActionBar;
    miClose: TMenuItem;
    acFormPopup: TAction;
    acFormPopup1: TMenuItem;
    acFormClose: TAction;
    acChartOptions: TAction;
    N10: TMenuItem;
    ChartOptions1: TMenuItem;
    acHelpIndicator: TAction;
    acHelpIndicator1: TMenuItem;
    acChangeIndicatorWindow: TAction;
    Bold2: TMenuItem;
    N11: TMenuItem;
    procedure acChartOptionsExecute(Sender: TObject);
    procedure acFormCloseUpdate(Sender: TObject);
    procedure acFormPopupExecute(Sender: TObject);
    procedure acFormCloseExecute(Sender: TObject);
    procedure acFormPopupUpdate(Sender: TObject);
    procedure acIndicatorSendToBackExecute(Sender: TObject);
    procedure acIndicatorBringToFrontExecute(Sender: TObject);
    procedure acPasteIndicatorExecute(Sender: TObject);
    procedure acPasteIndicatorUpdate(Sender: TObject);
    procedure acCopyIndicatorExecute(Sender: TObject);
    procedure acCopyIndicatorUpdate(Sender: TObject);
    procedure acBoldIndicatorExecute(Sender: TObject);
    procedure acGetTicksInRegionExecute(Sender: TObject);
    procedure pmIndicatorGetControlClass(Sender: TCustomActionBar; AnItem: TActionClient;
      var ControlClass: TCustomActionControlClass);
    procedure acChangeIndicatorDataSourceExecute(Sender: TObject);
    procedure acIndicatorWindowMoveDownExecute(Sender: TObject);
    procedure acIndicatorWindowMoveUpExecute(Sender: TObject);
    procedure acIndicatorWindowMoveDownUpdate(Sender: TObject);
    procedure acIndicatorWindowMoveUpUpdate(Sender: TObject);
    procedure imScaleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imScaleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cbGotoExit(Sender: TObject);
    procedure cbGotoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acShiftTicksExecute(Sender: TObject);
    procedure acReplayTicksInRegion2Execute(Sender: TObject);
    procedure acReplayTicksInRegionExecute(Sender: TObject);
    procedure acSyncRegionExecute(Sender: TObject);
    procedure acInvalidateAllExecute(Sender: TObject);
    procedure acInvalidateIndicatorExecute(Sender: TObject);
    procedure acDeleteIndicatorUpdate(Sender: TObject);
    procedure acDeleteIndicatorExecute(Sender: TObject);
    procedure acEditIndicatorUpdate(Sender: TObject);
    procedure acEditIndicatorExecute(Sender: TObject);
    procedure pmChartPopup(Sender: TObject);
    procedure acHelpIndicatorUpdate(Sender: TObject);
    procedure acHelpIndicatorExecute(Sender: TObject);
    procedure acChangeIndicatorWindowUpdate(Sender: TObject);
    procedure acChangeIndicatorWindowExecute(Sender: TObject);
  private type
    TTraderLineMap = TMap<TGUID,ISCIndicatorTradeLine>; //TraderId -> Line
    TTraderLineMapIterator = TMapIterator<TGUID,ISCIndicatorTradeLine>;
  private
    FID    : TGUID;
    FChart : TStockChart;
    FLockCount: integer;
    FStockSymbol: TStockSymbol;
    FSelectedIndicatorId : TGUID;
    FSelectedWindow: TSCIndicatorWindow;
    FThemeName  : string;
    FMeAsInterface: IStockChart;
    FTraderLineMap : TTraderLineMap;
    FCurrentTradeColorIndex: integer;
    FEventHandlers : array of IStockChartEventHandler; //используем массив для скорости. очень критично
    FSelectingRegion : boolean;
    FLoading : boolean;
    FProject : IStockProject;
    FActive: boolean;
    FActivatedEarly: boolean;
    FCrosshairPos: TCrosshairPosition;
    FDefaultPositionDate: TDateTime;
    FMovingSchartMouseYPos: integer;
    FMovingSchartMouseXPos: integer;
    FMovingSchartMinValue: TSCRealNumber;
    FMovingSchartMaxValue: TSCRealNumber;
    FAttributes      : IStockAttributeCollection;
    FPropGridX: TPropertyDrawingColor;
    FPropGridY: TPropertyDrawingColor;


    procedure CMPostActivate (var Message: TMessage); message CM_POSTACTIVATE;
    procedure CMEndGotoBox (var Message: TMessage); message CM_ENDGOTOBOX;

    procedure OnStockChartContextMenu(Sender: TObject; aPos: TPoint; var Handled: Boolean);
    procedure OnStockChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnStockChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnStockChartMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure OnStockChartMouseDblClick(Sender: TObject);
    procedure OnStockChartExit(Sender: TObject);
    procedure OnStockChartIndicatorChanged(sender: TStockChart; aObject: ISCIndicatorDrawSupport);
    procedure OnStockChartIndicatorAdded(sender: TStockChart; aObject: ISCIndicatorDrawSupport);
    procedure OnStockChartIndicatorDeleted(sender: TStockChart; aObject: ISCIndicatorDrawSupport);
    procedure OnStockChartPropertyChanged(sender: TStockChart);
    procedure OnStockChartVisibleRangeChanged(sender: TStockChart);
    procedure OnStockChartWindowsChanged(sender: TSCIndicatorWindowCollection);
    procedure OnStockChartKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnStockChartKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    //UI Actions
    procedure acChartShowCrosshairUpdate(aAction: TCustomAction);
    procedure acChartShowCrosshairExecute(aAction: TCustomAction);
    procedure acChartShowRulerUpdate(aAction: TCustomAction);
    procedure acChartShowRulerExecute(aAction: TCustomAction);
    procedure acChartShowPositionExecute(Sender: TCustomAction);
    procedure acChartShowPositionUpdate(Sender: TCustomAction);
    procedure acChartFitAllUpdate(Sender: TCustomAction);
    procedure acChartFitAllExecute(Sender: TCustomAction);
    procedure acChartFixVerticalScaleUpdate(Sender: TCustomAction);
    procedure acChartFixVerticalScaleExecute(Sender: TCustomAction);
    procedure acZoomInExecute(Sender: TCustomAction);
    procedure acZoomOutExecute(Sender: TCustomAction);

    procedure OnInsertIndicatorExecute(aAction:TCustomAction);
    procedure OnInsertShapeExecute(aAction:TCustomAction);
    procedure OnInsertExpertExecute(aAction:TCustomAction);

    procedure OnLoadThemeExecute(aAction:TCustomAction);
    procedure OnSaveThemeExecute(aAction:TCustomAction);

    procedure OnPerformTask(Sender: TObject);
    procedure OnShowCharacteristic(Sender: TObject);

    function  GetSelectedIndicator: ISCIndicatorDrawSupport;

    procedure miEditIndicatorClick(Sender: TObject);
    procedure miDeleteIndicatorClick(Sender: TObject);
    procedure miAddIndicatorClick(Sender: TObject);

    procedure miAddShapeClick(Sender: TObject);

    procedure miAddExpertClick(Sender: TObject);
    procedure miDeleteExpertClick(Sender: TObject);

    procedure LoadTheme; overload;
    procedure SaveTheme; overload;
    procedure SetStockSymbol(const aSymbolName: string; aInterval: TStockTimeInterval);

    procedure SetPositionLabelTitle;
    procedure SetDragCursor(shift: TShiftState);

    function TrimCaption(const aValue: string): string;
  protected
    // IChart methods -----------
    function Control: TWinControl;

    procedure SetTheme(const aThemeName: string);
    //---------------------------

    function  IsTraderLineExists(aIndicator: ISCIndicator):boolean; overload;
    function  IsTraderLineExists(aTrader: IStockTrader):boolean; overload;

    //Если линии не найдено, возвращается nil
    function  TradeLineByTraderID(const aTraderID: TGUID): ISCIndicatorTradeLine;
    //Если линии не найдено, она создается
    function  TradeLineByTraderSafe(aTrader: IStockTrader): ISCIndicatorTradeLine;
    //Если линии не найдено, она создается
    function  TradeLineByTraderIDSafe(const aTraderID: TGUID): ISCIndicatorTradeLine;

    //Если линии не найдено, возвращается nil
    function  PriceLineByID(const aID: TGUID): ISCIndicatorPriceLine;
    //Если линии не найдено, она создается
    function  PriceLineByIDSafe(const aID: TGUID): ISCIndicatorPriceLine;

    procedure FillIndicatorList;
    procedure FillShapeList;
    procedure FillExpertList;

    procedure RaiseChangedEvent(aChangeKind: TStockChartEventKind);
    procedure RaiseHilightedEvent(aX1,aX2: TDateTime);

    function  GetShowPosition: boolean;
    function  GetShowCrosshair: boolean;
    function  GetShowRuler: boolean;
    function  GetFixedVerticalScale: boolean;

    procedure SetShowCrosshair(aValue: boolean);
    procedure SetShowRuler(aValue: boolean);
    procedure SetShowPosition(aValue: boolean);
    procedure SetFixedVerticalScale(aValue: boolean);

    function  CheckOnDelete(const aIndicator: ISCIndicator): boolean;

    //from IPropertyChangeHandler
    procedure OnPropertyChanged(aNotifier:TProperty);
    procedure OnPropertyCreated(aNotifier:TProperty);
  public
    function  CreateIndicator(aIndicatorInfo: ISCIndicatorInfo; aShowPropDialog: boolean=true): ISCIndicator;
    function  CreateExpert(aExpertInfo: ISCExpertInfo; aShowPropDialog: boolean=true): ISCExpert;
    function  CreateShape(aShapeInfo: ISCIndicatorInfo; aWindow: TSCIndicatorWindow): ISCIndicatorShape;

    procedure Activate;
    procedure Deactivate;
//    procedure LoadTheme(aStream: TStream); overload;
//    procedure SaveTheme(aStream: TStream); overload;

    procedure SetInputData(aInputData: ISCInputDataCollection);
    property  ThemeName: string read  FThemeName write SetTheme;
    property  StockSymbol: TStockSymbol read FStockSymbol;
    function  SelectedWindow: TSCIndicatorWindow;

    procedure SaveData(aStream: TStream);
    procedure LoadData(aStream: TStream);

    procedure OnTraderOpenOrder(aOrder: IStockOrder);
    procedure OnTraderCloseOrder(aOrder: IStockOrder);
    procedure OnChangeOrderStopLoss(const aOrder: IStockOrder);
    procedure OnChangeOrderTakeProfit(const aOrder: IStockOrder);

    procedure OnTraderSetMark   (const aSender: IStockTrader;
                                 const aTime: TDateTime; const aPrice: TStockRealNumber;
                                 const aMarkType: TSCChartMarkKind;
                                 const aMessage: string);
    procedure OnInvalidateTraderLine(const aSender: IStockTrader);

    procedure OnTraderAdded(aTrader: IStockTrader);
    procedure OnTraderRemoved(aTrader: IStockTrader);

    function  FindIndicators(const aIID: TGUID; const aExternalData: ISCAttribute = nil): ISCIndicatorCollection;

    procedure Hilight(aX1,aX2: integer);
    procedure Mark(aX: integer);
    procedure LocateTo(const aX: integer; aLocateMode:TStockChartLocateMode);

    //создать или найти существующую линию цены
    function  GetPriceLine(const aID: TGUID; aCreate: boolean):ISCIndicatorPriceLine;
    procedure RemovePriceLine(const aID: TGUID);

    procedure AddEventHandler(const aHandler: IStockChartEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockChartEventHandler);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TfrmStockChartSerializationProxy }

  //Утилитный класс для выполнения сериализации фрейма. Нужен из-за того, что
  //требуется провести наследование от TNameValuePersistentObject
  //RefCounted = false
  TfrmStockChartSerializationProxy = class (TNameValuePersistentObject)
  private
    FOwner: TfrmStockChart;
  protected
    procedure OnDefineValues; override;
    procedure OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean); override;
    procedure OnWriteValues(const aWriter: INameValueDataWriter); override;
  public
    constructor Create(aOwner: TfrmStockChart);
  end;

  //Напрямую TFrame нельзя выдавать наружу в виде интерфейса, так как у него
  //заглушены AddRef/Release
  TStockChartContainer = class (TComponentContainer,IStockChart,IPersistentObject)
  private
    function  Target : TfrmStockChart;
  protected
    function GetID: TGUID;

    function  GetStockSymbol:TStockSymbol;
    procedure SetStockSymbol(const aSymbolName: string; aInterval: TStockTimeInterval);

    function  GetProject: IStockProject;
    procedure SetProject(const aValue : IStockProject);

    function  Control: TWinControl;
    procedure SetInputData(const aData: ISCInputDataCollection);
    function  GetInputData: ISCInputDataCollection;

    function  CreateIndicator(const aIndicatorInfo: ISCIndicatorInfo; aShowPropDialog: boolean=true): ISCIndicator;
    procedure DeleteIndicator(const aIndicator : ISCIndicator);
    function  GetIndicator(index: integer):ISCIndicator;
    function  IndicatorCount:integer;
    function  FindIndicators(const aIID: TGUID; const aExternalData: ISCAttribute = nil): ISCIndicatorCollection; overload;

    //создать или найти существующую линию ордеров. поиск осуществляется по ID
    //чтобы линия сразу и создалась (в случае отсутствия), нужно указать aCreate=true
    function  GetTraderLine(const aID: TGUID; aCreate: boolean):ISCIndicatorTradeLine;

    function  FindBar(const aDateTime: TDateTime): integer;

    function  GetFirstVisible: TDateTime;
    function  GetCrosshairPos: TDateTime;

    procedure LocateTo(const aDateTime: TDateTime; aLocateMode:TStockChartLocateMode);
    procedure EnsureVisible(const aDateTime1,aDateTime2: TDateTime);
    procedure Hilight(const aDateTime1,aDateTime2: TDateTime);
    procedure Mark(const aDateTime: TDateTime);
    procedure ShowSymbolTitle(aShow: boolean);

    //создать или найти существующую линию цены
    function  GetPriceLine(const aID: TGUID; aCreate: boolean):ISCIndicatorPriceLine;
    procedure RemovePriceLine(const aID: TGUID);

    procedure AddEventHandler(const aHandler: IStockChartEventHandler);
    procedure RemoveEventHandler(const aHandler: IStockChartEventHandler);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Repaint;
    procedure InvalidateAll;

    procedure OnOpenOrder(const aOrder: IStockOrder);
    procedure OnCloseOrder(const aOrder: IStockOrder);
    procedure OnChangeOrderStopLoss(const aOrder: IStockOrder);
    procedure OnChangeOrderTakeProfit(const aOrder: IStockOrder);
    procedure OnSetQuotation(const aBid,aAsk: TSCRealNumber);

    procedure OnTraderSetMark   (const aSender: IStockTrader;
                           const aTime: TDateTime; const aPrice: TStockRealNumber;
                           const aMarkType: TSCChartMarkKind;
                           const aMessage: string);
    procedure OnTraderInvalidate(const aSender: IStockTrader);

    //Для нужд прикладной разработки. К ордеру можно прицепить любые внешние данные
    function  GetAttributes:IStockAttributeCollection;

    procedure Dispose;

    //IPersistent
    procedure ReadData(const aReader: IDataReader);
    procedure WriteData(const aWriter: IDataWriter);
    function  GetPersistentObject: TObject;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TStockChartInterfaceMediator = class (TStockChartContainer)
  public
    destructor  Destroy;     override;
  end;

implementation
  uses Types,DateUtils,Clipbrd, SystemService, Math,ComObj,Registry, Collections.List,Application.Definitions,
  ufmChooseItem,
  Properties.Dialog,
  FC.Factory, StockChart.Theme, StockChart.Serialize,
  FC.StockData.IndicatorToInputDataCollectionMediator, StockChart.Obj, FC.DataUtils,
  FC.StockChart.GetTicksDialog,
  FC.StockChart.CreateIndicatorDialog,
  FC.StockData.StockCalendar;

{$R *.dfm}

resourcestring
  SCannotLoadTheme_s = 'Cannot load theme from file %s';
  SCannotSaveTheme_s = 'Cannot save theme to file %s';

type
  TStockChartFriend = class (TStockChart);

const
  TraderLineColors: array [0..15] of TColor = (
    clRed ,
    clMaroon ,
    clGreen ,
    clOlive ,
    clNavy ,
    clPurple ,
    clTeal ,
    clGray ,
    clSilver ,
    clLime ,
    clYellow ,
    clFuchsia ,
    clAqua ,
    clLtGray ,
    clDkGray ,
    clWhite
  );

type
  TIndicatorMenuItem = class (TMenuItem)
  public
    IndicatorInfo: ISCIndicatorInfo;
    IndicatorID: TGUID;
  end;

  TShapeMenuItem = class (TMenuItem)
  public
    ShapeInfo: ISCIndicatorInfo;
  end;

  TExpertMenuItem = class (TMenuItem)
  public
    ExpertInfo: ISCExpertInfo;
    ExpertID: TGUID;
  end;

  TUnitTaskMenuItem = class (TMenuItem)
  public
    UnitTask: IStockUnitTask;
  end;

  TCharacteristicMenuItem = class (TMenuItem)
  public
    Indicator: ISCIndicator;
    Characteristic: ISCIndicatorCharacteristic;
  end;

  TStockChartAttribute = class (TNameValuePersistentObjectRefCounted,IStockChartAttribute,ISCAttribute)
  private
    FChart: TfrmStockChart;
  public
    constructor Create(aChart: TfrmStockChart);

    function GetStockChart: IStockChart;
  end;

  TFriendControl = class (TControl);

  TCaptionMenuItem = class(TCustomMenuItem)
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawText(var Rect: TRect; var Flags: Cardinal; Text: String); override;
    procedure DrawShadowedText(Rect: TRect; Flags: Cardinal; Text: String; TextColor: TColor; ShadowColor: TColor); override;
    procedure CalcBounds; override;

    constructor Create(aOwner: TComponent); override;
  end;



{ TStockChartAttribute }

constructor TStockChartAttribute.Create(aChart: TfrmStockChart);
begin
  inherited Create;
  FChart:=aChart;
end;

function TStockChartAttribute.GetStockChart: IStockChart;
begin
  result:=FChart.FMeAsInterface;
end;

{ TStockChartContainer }

constructor TStockChartContainer.Create;
var
  aTarget: TfrmStockChart;
begin
  aTarget:=TfrmStockChart.Create(nil);
  aTarget.FMeAsInterface:=Self;
  inherited CreateTargeted(aTarget);
end;

destructor TStockChartContainer.Destroy;
begin
  inherited;
end;

function TStockChartContainer.Target: TfrmStockChart;
begin
  result:=TfrmStockChart(inherited Target);
end;

function TStockChartContainer.Control: TWinControl;
begin
  result:=Target.Control;
end;

function TStockChartContainer.GetStockSymbol: TStockSymbol;
begin
  result:=Target.StockSymbol;
end;

function TStockChartContainer.GetTraderLine(const aID: TGUID; aCreate: boolean): ISCIndicatorTradeLine;
begin
  result:=Target.TradeLineByTraderID(aID);
  if (result=nil) and (aCreate) then
    result:=Target.TradeLineByTraderIDSafe(aID);
end;

procedure TStockChartContainer.SetStockSymbol(const aSymbolName: string; aInterval: TStockTimeInterval);
begin
  Target.SetStockSymbol(aSymbolName,aInterval);
end;

function TStockChartContainer.GetInputData: ISCInputDataCollection;
begin
  result:=Target.FChart.InputData;
end;

procedure TStockChartContainer.SetInputData(const aData: ISCInputDataCollection);
begin
  Target.SetInputData(aData);
end;

procedure TStockChartContainer.LocateTo(const aDateTime: TDateTime; aLocateMode:TStockChartLocateMode);
var
  i: integer;
begin
  i:=Target.FChart.InputData.FindBestMatched(aDateTime);
  if i>0 then
  begin
    Target.LocateTo(i,aLocateMode);
  end;
end;

procedure TStockChartContainer.EnsureVisible(const aDateTime1, aDateTime2: TDateTime);
var
  i,j: integer;
  aDT1,aDT2: TDateTime;
begin
  TStockDataUtils.AlignTimeIntervalToLeft(aDateTime1,aDateTime2,GetStockSymbol.TimeInterval,aDT1,aDT2);
  i:=Target.FChart.InputData.FindBestMatched(aDT1);
  j:=Target.FChart.InputData.FindBestMatched(aDT2);

  if i>j then
    SwapInteger(i,j);


  if (i>0) and (j>0) then
  begin
    //if ((j-i)<=Target.FChart.VisibleValuesCount) then
    begin
      if (i<Target.FChart.FirstVisible) then
        Target.FChart.FirstVisible:=i;
      if (j>Target.FChart.FirstVisible+Target.FChart.VisibleValuesCount) then
        Target.FChart.FirstVisible:=(j-Target.FChart.VisibleValuesCount+1);
    end;
  end;
end;

type
  IHL = interface ['{0430D819-959E-4B0A-9D13-4020D0BDBB30}'] end;
  IMark = interface ['{F20677F9-9BB2-45E4-A4C6-8D925D875368}'] end;

procedure TStockChartContainer.Hilight(const aDateTime1, aDateTime2: TDateTime);
var
  x1,x2: integer;
  aDT1,aDT2: TDateTime;
begin
  TStockDataUtils.AlignTimeIntervalToLeft(aDateTime1,aDateTime2,GetStockSymbol.TimeInterval,aDT1,aDT2);

  x1:=Target.FChart.InputData.FindBestMatched(aDT1);
  x2:=Target.FChart.InputData.FindBestMatched(aDT2);

  if (x1<>-1) and (x2<>-1) then
  begin
    if x1=x2 then
    begin
      if aDateTime1<aDateTime2 then
        inc(x2)
      else if aDateTime1>aDateTime2 then
        dec(x1);
    end;

    Target.Hilight(x1,x2);
  end;
end;

procedure TStockChartContainer.Mark(const aDateTime: TDateTime);
var
  x: integer;
begin
  x:=Target.FChart.InputData.FindBestMatched(aDateTime);

  if (x<>-1) then
  begin
    Target.Mark(x);
  end;
end;

procedure TStockChartContainer.Dispose;
begin

  Target.Free;
end;

procedure TStockChartContainer.OnTraderInvalidate(const aSender: IStockTrader);
begin
  Target.OnInvalidateTraderLine(aSender);
end;

procedure TStockChartContainer.OnOpenOrder(const aOrder: IStockOrder);
begin
  Target.OnTraderOpenOrder(aOrder);
end;

procedure TStockChartContainer.OnSetQuotation(const aBid, aAsk: TSCRealNumber);
begin
  Target.FChart.Bid:=aBid;
  Target.FChart.Ask:=aAsk;
end;

procedure TStockChartContainer.OnTraderSetMark(const aSender: IStockTrader; const aTime: TDateTime;
  const aPrice: TStockRealNumber; const aMarkType: TSCChartMarkKind; const aMessage: string);
begin
  Target.OnTraderSetMark(aSender,aTime,aPrice,aMarkType,aMessage);
end;

procedure TStockChartContainer.OnChangeOrderStopLoss(const aOrder: IStockOrder);
begin
  Target.OnChangeOrderStopLoss(aOrder);
end;

procedure TStockChartContainer.OnChangeOrderTakeProfit(const aOrder: IStockOrder);
begin
 Target.OnChangeOrderTakeProfit(aOrder);
end;

procedure TStockChartContainer.OnCloseOrder(const aOrder: IStockOrder);
begin
  Target.OnTraderCloseOrder(aOrder);
end;

procedure TStockChartContainer.BeginUpdate;
begin
  Target.FChart.BeginUpdate;
  inc(Target.FLockCount);
end;

procedure TStockChartContainer.EndUpdate;
begin
  Target.FChart.EndUpdate;
  dec(Target.FLockCount);
  if Target.FLockCount=0 then
    Target.RaiseChangedEvent(ekEndUpdate);
end;

procedure TStockChartContainer.Repaint;
var
  aForm: TCustomForm;
begin
  aForm:=GetParentForm(Target);

  if (aForm<>nil) and (TForm(aForm).Visible) then
  begin
    Target.FChart.Repaint;
  end;
end;

function TStockChartContainer.GetPersistentObject: TObject;
begin
  result:=self;
end;

procedure TStockChartContainer.ReadData(const aReader: IDataReader);
begin
  Target.FLoading:=true;
  with TfrmStockChartSerializationProxy.Create(Target) do
  try
     ReadData(aReader);
  finally
    Target.FLoading:=false;
    Free;
  end
end;

procedure TStockChartContainer.WriteData(const aWriter: IDataWriter);
begin
  with TfrmStockChartSerializationProxy.Create(Target) do
  try
     WriteData(aWriter);
  finally
    Free;
  end
end;

procedure TStockChartContainer.AddEventHandler(const aHandler: IStockChartEventHandler);
begin
  Target.AddEventHandler(aHandler);
end;

procedure TStockChartContainer.RemoveEventHandler(const aHandler: IStockChartEventHandler);
begin
  Target.RemoveEventHandler(aHandler);
end;

procedure TStockChartContainer.RemovePriceLine(const aID: TGUID);
begin
  Target.RemovePriceLine(aID);
end;

function TStockChartContainer.IndicatorCount: integer;
begin
  result:=Target.FChart.Indicators.Count;
end;

function TStockChartContainer.GetIndicator(index: integer): ISCIndicator;
begin
  result:=Target.FChart.Indicators2[index];
end;

procedure TStockChartContainer.InvalidateAll;
begin
  Target.FChart.InvalidateAll;
end;

function TStockChartContainer.FindBar(const aDateTime: TDateTime): integer;
begin
  if GetInputData=nil then
    result:=-1
  else begin
    result:=TStockDataUtils.FindBar(GetInputData,aDateTime,GetStockSymbol.TimeInterval);
  end;

end;

function TStockChartContainer.FindIndicators(const aIID: TGUID;
  const aExternalData: ISCAttribute): ISCIndicatorCollection;
begin
  result:=Target.FindIndicators(aIID,aExternalData);
end;

function TStockChartContainer.GetID: TGUID;
begin
  result:=Target.FID;
end;

function TStockChartContainer.GetPriceLine(const aID: TGUID; aCreate: boolean):ISCIndicatorPriceLine;
begin
  result:=Target.GetPriceLine(aID,aCreate);
end;

procedure TStockChartContainer.ShowSymbolTitle(aShow: boolean);
begin
  if Target.StockSymbol<>nil then
  begin
    Target.laSymbol.Caption:=Target.StockSymbol.Name+ ' ['+StockTimeIntervalNames[Target.StockSymbol.TimeInterval]+']';
    Target.laSymbol.Visible:=aShow;
    if aShow then
      Target.laSymbol.BringToFront;
  end;
end;

function TStockChartContainer.CreateIndicator(const aIndicatorInfo: ISCIndicatorInfo;
  aShowPropDialog: boolean): ISCIndicator;
begin
  result:=Target.CreateIndicator(aIndicatorInfo,aShowPropDialog);
end;

procedure TStockChartContainer.DeleteIndicator(const aIndicator: ISCIndicator);
begin
  Target.FChart.Indicators.Remove(aIndicator as ISCIndicatorDrawSupport);
end;

procedure TStockChartContainer.SetProject(const aValue: IStockProject);
begin
  Target.FProject:=aValue;
end;

function TStockChartContainer.GetProject: IStockProject;
begin
  result:=Target.FProject;
end;

function TStockChartContainer.GetFirstVisible: TDateTime;
begin
  if Target.FChart.FirstVisible=-1 then
    result:=0
  else
    result:=Target.FChart.InputData.DirectGetItem_DataDateTime(Target.FChart.FirstVisible);
end;


function TStockChartContainer.GetAttributes: IStockAttributeCollection;
begin
  result:=Target.FAttributes;
end;

function TStockChartContainer.GetCrosshairPos: TDateTime;
var
  x: integer;
begin
  result:=0;

  x:=Target.FChart.GetCrosshairXPos;
  if x<>-1 then
    Result:=Target.FChart.InputData.DirectGetItem_DataDateTime(x);
end;

{ TfrmStockChartSerializationProxy }

constructor TfrmStockChartSerializationProxy.Create(aOwner: TfrmStockChart);
begin
  inherited Create;
  FOwner:=aOwner;
end;

procedure TfrmStockChartSerializationProxy.OnDefineValues;
begin
  inherited;
  with FOwner do
  begin
    DefValGUID('ID',FID);
    DefValObjectPtr('StockChart',FChart);
  end;
end;

procedure TfrmStockChartSerializationProxy.OnWriteValues(const aWriter: INameValueDataWriter);
var
  it : TfrmStockChart.TTraderLineMapIterator;
  i: Integer;
  aCustomData: IStockIndicatorToInputDataCollectionMediator;
begin
  inherited;

  //Линии экспертов.
  aWriter.DataWriter.WriteString('ExpertLines');
  aWriter.DataWriter.WriteListBegin;
  FOwner.FTraderLineMap.GetFirst(it);
  while it.Valid do
  begin
    if FOwner.FChart.Indicators2.IndexOf(it.Value as ISCIndicatorDrawSupport)<>-1 then
    begin
      aWriter.DataWriter.WriteGUID(it.Key);
      aWriter.DataWriter.WriteObject(it.Value as IPersistentObject);
    end
    else begin
      try
        raise EAlgoError.Create;
      except
        on E:Exception do
          Workspace.ExceptionManager.Publish(E);
      end;
    end;
    FOwner.FTraderLineMap.GetNext(it)
  end;
  aWriter.DataWriter.WriteListEnd;

  //Подмененные InputDataCollecation
  aWriter.DataWriter.WriteString('IndicatorToInputDataCollectionMediators');
  aWriter.DataWriter.WriteListBegin;
  for i := 0 to FOwner.FChart.Indicators2.Count - 1 do
  begin
    if Supports(FOwner.FChart.Indicators2[i].GetInputData,IStockIndicatorToInputDataCollectionMediator,aCustomData) then
    begin
      aWriter.DataWriter.WriteObject(FOwner.FChart.Indicators2[i]);
      aWriter.DataWriter.WriteObject(aCustomData.GetDataSource);
    end;
  end;
  aWriter.DataWriter.WriteListEnd;

  if FOwner.FChart.FirstVisible=-1 then
    aWriter.WriteDateTime('CurrentPos',0)
  else
    aWriter.WriteDateTime('CurrentPos',FOwner.FChart.InputData.DirectGetItem_DataDateTime(FOwner.FChart.FirstVisible));

  //aWriter.WriteString('CurrentPosIndex');
  //aWriter.WriteInteger(FOwner.FChart.FirstVisible);

  aWriter.WriteBoolean('Fit All Vertically',FOwner.FChart.Apppearance.FitAllVertically);
  aWriter.WriteBoolean('ShowCrosshair',FOwner.GetShowCrosshair);
  aWriter.WriteBoolean('ShowPosition',FOwner.GetShowPosition);
  aWriter.WriteBoolean('FixedVerticalScale',FOwner.GetFixedVerticalScale);
  aWriter.WriteDouble('VerticalMin',FOwner.FChart.MainIndicatorWindow.YScale.MinValue);
  aWriter.WriteDouble('VerticalMax',FOwner.FChart.MainIndicatorWindow.YScale.MaxValue);
end;

procedure TfrmStockChartSerializationProxy.OnReadValue(const aReader: INameValueDataReader; const aName: string; var aHandled: boolean);
var
  aID: TGUID;
  aIntf: IInterface;
  aBool: boolean;
  aInt : integer;
  aReal: TSCRealNumber;
  aInd1,aInd2: ISCIndicator;
begin
  inherited;
  if aHandled then
    exit;

  if aName = 'ExpertLines' then
  begin
    aHandled:=true;

    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      aReader.ReadGUID(aID);
      aIntf:=aReader.ReadInterface;
      FOwner.FTraderLineMap.Add(aID,aIntf as ISCIndicatorTradeLine);
    end;
    aReader.ReadListEnd;

    exit;
  end;

  //Подмененные InputDataCollecation
  if aName='IndicatorToInputDataCollectionMediators' then
  begin
    aReader.ReadListBegin;
    while not aReader.EndOfList do
    begin
      aInd1:=aReader.ReadInterface as ISCIndicator;
      aInd2:=aReader.ReadInterface as ISCIndicator;
      aInd1.SetInputData(TStockIndicatorToInputDataCollectionMediator.Create(aInd2,nil));
    end;
    aReader.ReadListEnd;

    aHandled:=true;
    exit;
  end;

  if aName ='CurrentPos' then
  begin
    aReader.ReadDateTime(FOwner.FDefaultPositionDate);
    aHandled:=true;
    exit;
  end;

  if aName ='CurrentPosIndex' then //Obsolete
  begin
    aReader.ReadInteger(aInt);
    aHandled:=true;
    exit;
  end;

  if aName ='ShowCrosshair' then
  begin
    aReader.ReadBoolean(aBool);
    FOwner.SetShowCrosshair(aBool);
    aHandled:=true;
    exit;
  end;

  if aName ='ShowPosition' then
  begin
    aReader.ReadBoolean(aBool);
    FOwner.SetShowPosition(aBool);
    aHandled:=true;
    exit;
  end;

  if aName='Fit All Vertically' then
  begin
    aReader.ReadBoolean(aBool);
    FOwner.FChart.Apppearance.FitAllVertically:=aBool;

    aHandled:=true;
    exit;
  end;

  if aName ='FixedVerticalScale' then
  begin
    aReader.ReadBoolean(aBool);
    FOwner.SetFixedVerticalScale(aBool);
    aHandled:=true;
    exit;
  end;

  if aName='VerticalMin' then
  begin
    aReader.ReadDouble(aReal);
    FOwner.FChart.MainIndicatorWindow.YScale.MinValue:=aReal;

    aHandled:=true;
    exit;
  end;

  if aName='VerticalMax' then
  begin
    aReader.ReadDouble(aReal);
    FOwner.FChart.MainIndicatorWindow.YScale.MaxValue:=aReal;

    aHandled:=true;
    exit;
  end;
end;

{ TfrmChart }

function TfrmStockChart.CheckOnDelete(const aIndicator: ISCIndicator): boolean;
var
  aList: TStrings;
  j: integer;
begin
  if (aIndicator.UsingCount>0) then
  begin
    aList:=TStringList.Create;
    for j:=0 to aIndicator.UsingCount-1 do
      aList.Add(aIndicator.Usings[j]);

    MsgBox.MessageAttention(0,'Cannot delete object that in use. Objects that uses expert are:'#13#10#13#10+aList.Text,[]);

    aList.Free;

    result:=false;
  end
  else begin
    result:=true;
  end;
end;

procedure TfrmStockChart.acFormCloseExecute(Sender: TObject);
begin
  GetParentForm(self).Close;
end;

procedure TfrmStockChart.acFormCloseUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=GetParentForm(self)<>nil;
end;

procedure TfrmStockChart.CMEndGotoBox(var Message: TMessage);
begin
  cbGoto.Visible:=false;
  FChart.SetFocus;

  FChart.FirstVisible:=Message.WParam;
  Mark(Message.WParam);
  LocateTo(Message.WParam,lmCenter);
end;

procedure TfrmStockChart.CMPostActivate(var Message: TMessage);
begin
end;

function TfrmStockChart.Control: TWinControl;
begin
  result:=self;
end;

constructor TfrmStockChart.Create(aOwner: TComponent);
begin
  inherited;
  FAttributes:=TSCAttributeCollection.Create;

  CreateGUID(FID);
  FMeAsInterface:=TStockChartInterfaceMediator.CreateTargeted(self);

  FEventHandlers:=nil;

  FTraderLineMap:=TTraderLineMap.Create;

  FChart:=TStockChart.Create(self);
  FChart.Parent:=self;
  FChart.Align:=alClient;
  FChart.OnContextPopup:=OnStockChartContextMenu;

  FChart.OnAddIndicator:=OnStockChartIndicatorAdded;
  FChart.OnDeleteIndicator:=OnStockChartIndicatorDeleted;
  FChart.OnIndicatorChanged:=OnStockChartIndicatorChanged;
  FChart.OnPropertyChanged:=OnStockChartPropertyChanged;
  FChart.OnVisibleRangeChanged:=OnStockChartVisibleRangeChanged;
  FChart.OnMouseDown:=OnStockChartMouseDown;
  FChart.OnMouseUp:=OnStockChartMouseUp;
  FChart.OnMouseMove:=OnStockChartMouseMove;
  FChart.OnDblClick:=OnStockChartMouseDblClick;
  FChart.OnExit:=OnStockChartExit;
  FChart.OnKeyDown:=OnStockChartKeyDown;
  FChart.OnKeyUp:=OnStockChartKeyUp;  
  TStockChartFriend(FChart).IndicatorWindowCollection.OnChanged:=OnStockChartWindowsChanged;

  FPropGridX:=TPropertyDrawingColor.Create(nil);
  FPropGridX.Category:='Grid';
  FPropGridX.Name:='X Axis';
  FPropGridX.Value:=FChart.Colors.GridX;

  FPropGridY:=TPropertyDrawingColor.Create(nil);
  FPropGridY.Category:='Grid';
  FPropGridY.Name:='Y Axis';
  FPropGridY.Value:=FChart.Colors.GridY;

  //SetTheme('Default');
  FillIndicatorList;
  FillShapeList;
  FillExpertList;

  laSymbol.Parent:=FChart;
  laPosition.Parent:=FChart;
  imScale.Parent:=FChart;
  cbGoto.Parent:=FChart;

  //по умолчанию курсор = true
  SetShowCrosshair(true);

  FChart.DoubleBuffered:=true;

  Workspace.MainFrame.AddActionTarget(self);
  ActionTarget.RegisterAction(UIDataStorage.acChartShowCrosshair,acChartShowCrosshairUpdate,acChartShowCrosshairExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartShowRuler,acChartShowRulerUpdate,acChartShowRulerExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartShowPosition,acChartShowPositionUpdate,acChartShowPositionExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartFitAll,acChartFitAllUpdate,acChartFitAllExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartFixVerticalScale,acChartFixVerticalScaleUpdate,acChartFixVerticalScaleExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartZoomIn,nil,acZoomInExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartZoomOut,nil,acZoomOutExecute);

  ActionTarget.RegisterAction(UIDataStorage.acChartInsertIndicator,nil,OnInsertIndicatorExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartInsertShape,nil,OnInsertShapeExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartInsertExpert,nil,OnInsertExpertExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartLoadTheme,nil,OnLoadThemeExecute);
  ActionTarget.RegisterAction(UIDataStorage.acChartSaveTheme,nil,OnSaveThemeExecute);
end;

destructor TfrmStockChart.Destroy;
begin
  if FChart<>nil then
  begin
    SaveTheme;
  end;

  FreeAndNil(FChart); //В такой последовательности
  FreeAndNil(FTraderLineMap);
  Finalize(FEventHandlers);

  FAttributes:=nil;
  inherited;
end;

procedure TfrmStockChart.FillIndicatorList;
var
  aList  : TStringList;
  aList2 : TList<integer>;
  aListObjects: TStringList;
  i,j    : integer;
  aItem  : TMenuItem;
  aItem2 : TIndicatorMenuItem;
  aSeparator: TMenuItem;
  aInfo  : ISCIndicatorInfo;
  aGroup : integer;
begin
  //Заполняем список индикаторов
  aList:=TStringList.Create;
  try
    IndicatorFactory.GetAllIndicatorCategories(aList);
    aList.Sort;
    for i:=0 to aList.Count-1 do
    begin
      aList2:=TList<integer>.Create;
      aListObjects:=TStringList.Create;
      try
        //Получаем список индикаторов (точнее список их индексов)
        //и упорядочиваем по имени
        IndicatorFactory.GetAlIndicatorsForCategory(aList[i],aList2);
        for j:=0 to aList2.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(aList2[j]);
          if aInfo.Kind=ikIndicator then
            aListObjects.AddObject(IntToStrEx(aInfo.Group,8)+aInfo.Name,TObject(aList2[j]));
        end;

        if (aListObjects.Count=0) then
          continue;

        aListObjects.Sort;

        aItem:=TMenuItem.Create(pmChart);
        aItem.Caption:=aList[i];
        miInsertIndicator.Add(aItem);

        //Начальная группа
        aGroup:=IndicatorFactory.GetIndicatorInfo(integer(aListObjects.Objects[0])).Group;

        //Добавляем
        for j:=0 to aListObjects.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(integer(aListObjects.Objects[j]));
          if aInfo.Group<>aGroup then
          begin
            aSeparator:=TMenuItem.Create(pmChart);
            aSeparator.Caption:= '-';
            aItem.Add(aSeparator);
          end;

          aGroup:=aInfo.Group;

          aItem2:=TIndicatorMenuItem.Create(pmChart);
          aItem2.Caption:= aInfo.Name;
          aItem2.IndicatorInfo:=aInfo;
          aItem2.OnClick:=miAddIndicatorClick;
          aItem.Add(aItem2);
        end;
      finally
        aList2.Free;
        aListObjects.Free;
      end;
    end;
  finally
    aList.Free;
  end;
end;

procedure TfrmStockChart.FillExpertList;
var
  aList  : TStringList;
  aList2 : TList<integer>;
  aListObjects: TStringList;
  i,j    : integer;
  aItem  : TMenuItem;
  aItem2 : TExpertMenuItem;
  aInfo  : ISCExpertInfo;
begin
  //Заполняем список экспертов
  aList:=TStringList.Create;
  try
    IndicatorFactory.GetAllIndicatorCategories(aList);
    aList.Sort;
    for i:=0 to aList.Count-1 do
    begin

      aList2:=TList<integer>.Create;
      aListObjects:=TStringList.Create;
      try
        //Получаем список индикаторов (точнее список их индексов)
        //и упорядочиваем по имени
        IndicatorFactory.GetAlIndicatorsForCategory(aList[i],aList2);
        for j:=0 to aList2.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(aList2[j]);
          if aInfo.Kind=ikExpert then
            aListObjects.AddObject(aInfo.Name,TObject(aList2[j]));
        end;

        if (aListObjects.Count=0) then
          continue;

        aListObjects.Sort;

        aItem:=TMenuItem.Create(pmChart);
        aItem.Caption:=aList[i];
        miInsertExpert.Add(aItem);

        //Добавляем
        for j:=0 to aListObjects.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(integer(aListObjects.Objects[j]));

          aItem2:=TExpertMenuItem.Create(pmChart);
          aItem2.Caption:= aInfo.Name;
          aItem2.ExpertInfo:=aInfo;
          aItem2.OnClick:=miAddExpertClick;
          aItem.Add(aItem2);
        end;
      finally
        aList2.Free;
        aListObjects.Free;
      end;
    end;
  finally
    aList.Free;
  end;
end;

procedure TfrmStockChart.FillShapeList;
var
  aList  : TStringList;
  aList2 : TList<integer>;
  aListObjects: TStringList;
  i,j    : integer;
  aItem  : TMenuItem;
  aItem2 : TShapeMenuItem;
  aInfo  : ISCIndicatorInfo;
begin
  //Заполняем список индикаторов
  aList:=TStringList.Create;
  try
    IndicatorFactory.GetAllIndicatorCategories(aList);
    aList.Sort;
    for i:=0 to aList.Count-1 do
    begin
      aList2:=TList<integer>.Create;
      aListObjects:=TStringList.Create;
      try
        //Получаем список индикаторов (точнее список их индексов)
        //и упорядочиваем по имени
        IndicatorFactory.GetAlIndicatorsForCategory(aList[i],aList2);
        for j:=0 to aList2.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(aList2[j]);
          if aInfo.Kind=ikShape then
            aListObjects.AddObject(aInfo.Name,TObject(aList2[j]));
        end;
        aListObjects.Sort;

        if (aListObjects.Count=0) then
          continue;

        aItem:=TMenuItem.Create(pmChart);
        aItem.Caption:=aList[i];
        miInsertShape.Add(aItem);

        //Добавляем
        for j:=0 to aListObjects.Count-1 do
        begin
          aInfo:=IndicatorFactory.GetIndicatorInfo(integer(aListObjects.Objects[j]));

          aItem2:=TShapeMenuItem.Create(pmChart);
          aItem2.Caption:= aInfo.Name;
          aItem2.ShapeInfo:=aInfo;
          aItem2.OnClick:=miAddShapeClick;
          aItem.Add(aItem2);
        end;
      finally
        aList2.Free;
        aListObjects.Free;
      end;
    end;
  finally
    aList.Free;
  end;
end;

function TfrmStockChart.FindIndicators(const aIID: TGUID;
  const aExternalData: ISCAttribute): ISCIndicatorCollection;
var
  aIndicatorCollection: ISCIndicatorCollection;
  i: integer;
  aIndicator: ISCIndicator;
begin
  aIndicatorCollection:=TSCIndicatorCollection.Create();
  for i := 0 to FChart.Indicators.Count - 1 do
  begin
    aIndicator:=FChart.Indicators2[i];
    if Supports(aIndicator,aIID) then
    begin
      if (aExternalData=nil) or (aIndicator.GetAttributes.IndexOf(aExternalData)<>-1) then
        aIndicatorCollection.Add(aIndicator);
    end;
  end;

  result:=aIndicatorCollection;
end;

function TfrmStockChart.SelectedWindow: TSCIndicatorWindow;
begin
  result:=FSelectedWindow;
  if result=nil then
  begin
    if FChart.IndicatorWindowCount=0 then
      exit;

    result:=FChart.IndicatorWindows[0];
  end;
end;

procedure TfrmStockChart.SetInputData(aInputData: ISCInputDataCollection);
begin
  FChart.InputData:=aInputData;
end;

procedure TfrmStockChart.SetPositionLabelTitle;
begin
  if FChart.InputData.Count=0 then
    laPosition.Caption:=''
  else
    laPosition.Caption:=IntToStr(min((FChart.FirstVisible+FChart.VisibleValuesCount)*100 div FChart.InputData.Count,100))+'%';
end;

function TfrmStockChart.GetFixedVerticalScale: boolean;
begin
  result:=FChart.MainIndicatorWindow.YScale.Fixed;
end;

function TfrmStockChart.GetSelectedIndicator: ISCIndicatorDrawSupport;
var
  aItem: TSCIndicatorCollectionItem;
begin
  aItem:=FChart.Indicators.FindByIndicator(FSelectedIndicatorId);
  if aItem<>nil then
    result:=aItem.Indicator;
end;


function TfrmStockChart.GetShowCrosshair: boolean;
begin
  result:=FChart.Crosshair.Visible;
end;

function TfrmStockChart.GetShowRuler: boolean;
begin
  result:=FChart.Ruler.Visible;
end;

function TfrmStockChart.GetShowPosition: boolean;
begin
  result:=laPosition.Visible;
end;

procedure TfrmStockChart.cbGotoExit(Sender: TObject);
begin
  inherited;
  cbGoto.Visible:=false;
end;

procedure TfrmStockChart.cbGotoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  aDT: TDateTime;
  i: integer;
  aFormatSettings: TFormatSettings;
begin
  inherited;

  if Key=13 then
  begin
    GetLocaleFormatSettings(GetUserDefaultLCID,aFormatSettings);
    try
      //В формате бара
      if IsDigitString(cbGoto.Text) then
        i:=StrToInt(cbGoto.Text)
      //В формате даты
      else begin
        try
          aDT:=StrToDateTime(cbGoto.Text);
        except
          aFormatSettings.DateSeparator:='.';
          aFormatSettings.ShortDateFormat:='YYYY.MM.DD';
          aDT:=StrToDateTime(cbGoto.Text,aFormatSettings);
        end;
        i:=FChart.InputData.FindBestMatched(aDT);
      end;
      if i<>-1 then
      begin
        rlGoto.AddRecently(cbGoto.Text);
        PostMessage(Handle,CM_ENDGOTOBOX,i,0);
        Key:=0;
      end
      else begin
        MessageBeep(cardinal(-1));
      end;
    except
      on E: Exception do
        MsgBox.MessageAttention(Handle,'Cannot understand %s',[cbGoto.Text]);
    end;
  end
  else if Key=27 then
  begin
    cbGoto.Visible:=false;
    FChart.SetFocus;
    Key:=0;
  end;
end;

procedure TfrmStockChart.OnStockChartKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  aRect: TRect;
begin
  inherited;
  if (Key=13) and (Shift=[]) then
  begin
    aRect:=FChart.ClientRect;
    FChart.AdjustClientRect(aRect);
    Key:=0;

    cbGoto.Left:=aRect.Right-cbGoto.Width-8;
    cbGoto.Items.Assign(rlGoto.RecenlyList);
    cbGoto.Visible:=true;
    cbGoto.BringToFront;
    cbGoto.SetFocus;
  end;

  SetDragCursor(Shift);
end;

procedure TfrmStockChart.OnStockChartKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  SetDragCursor(Shift);
end;

procedure TfrmStockChart.acZoomInExecute(Sender: TCustomAction);
begin
  FChart.ZoomIn;
end;

procedure TfrmStockChart.acZoomOutExecute(Sender: TCustomAction);
begin
  FChart.ZoomOut;
end;

procedure TfrmStockChart.AddEventHandler(const aHandler: IStockChartEventHandler);
begin
  SetLength(FEventHandlers,Length(FEventHandlers)+1);
  FEventHandlers[High(FEventHandlers)]:=aHandler;
end;

procedure TfrmStockChart.RemoveEventHandler(const aHandler: IStockChartEventHandler);
var
  i,j: integer;
begin
  for i := 0 to High(FEventHandlers) do
  begin
    if FEventHandlers[i]=aHandler then
    begin
      //Сдвигаем то, что сзади на место образовавшегося проема
      for j:=i to High(FEventHandlers)-1 do
        FEventHandlers[j]:=FEventHandlers[j+1];
      SetLength(FEventHandlers,Length(FEventHandlers)-1);
      break;
    end;
  end;
end;

procedure TfrmStockChart.RemovePriceLine(const aID: TGUID);
var
  aLine: ISCIndicatorPriceLine;
begin
  aLine:=PriceLineByID(aID);
  if aLine<>nil then
    FChart.Indicators.Remove(aLine as ISCIndicatorDrawSupport);
end;

procedure TfrmStockChart.miAddIndicatorClick(Sender: TObject);
begin
  CreateIndicator(TIndicatorMenuItem(Sender).IndicatorInfo);
end;

procedure TfrmStockChart.miAddShapeClick(Sender: TObject);
var
  aWindow: TSCIndicatorWindow;
begin
  aWindow:=FSelectedWindow;
  if aWindow=nil then
  begin
    if FChart.IndicatorWindowCount=0 then
      exit;

    aWindow:=FChart.IndicatorWindows[0];
  end;

  CreateShape(TShapeMenuItem(Sender).ShapeInfo,aWindow);
end;

procedure TfrmStockChart.miAddExpertClick(Sender: TObject);
begin
  CreateExpert(TExpertMenuItem(Sender).ExpertInfo);
end;

procedure TfrmStockChart.miDeleteExpertClick(Sender: TObject);
begin
  FSelectedIndicatorId:=(sender as TExpertMenuItem).ExpertID;
  acDeleteIndicatorExecute(nil);
end;

procedure TfrmStockChart.pmChartPopup(Sender: TObject);
var
  i: integer;
  aMenuItem:TMenuItem;
  aPt: TPoint;
begin
  inherited;

  FSelectedWindow:=FChart.GetIndicatorWindowFromPoint(FChart.ScreenToClient(Mouse.CursorPos),aPt);

  miModifyIndicator.Clear;
  miModifyExpert.Clear;
  miModifyShape.Clear;

  for i:=0 to FChart.Indicators.Count-1 do
  begin
    if not Supports(FChart.Indicators[i].Indicator,ISCExpert) then
    begin
      aMenuItem:=TIndicatorMenuItem.Create(self);
      aMenuItem.Caption:=TrimCaption(FChart.Indicators[i].Indicator.Caption);
      aMenuItem.Enabled:=FChart.Indicators[i].Indicator.HasPropertyWindow;
      TIndicatorMenuItem(aMenuItem).IndicatorID:=FChart.Indicators[i].Indicator.GetID;
      aMenuItem.OnClick:=miEditIndicatorClick;
      if not Supports(FChart.Indicators[i].Indicator,ISCIndicatorShape) then
        miModifyIndicator.Add(aMenuItem)
      else
        miModifyShape.Add(aMenuItem);
    end
    else begin
      aMenuItem:=TExpertMenuItem.Create(self);
      aMenuItem.Caption:=TrimCaption(FChart.Indicators[i].Indicator.Caption);
      aMenuItem.Enabled:=FChart.Indicators[i].Indicator.HasPropertyWindow;
      TExpertMenuItem(aMenuItem).ExpertID:=FChart.Indicators[i].Indicator.GetID;
      aMenuItem.OnClick:=miEditIndicatorClick;
      miModifyExpert.Add(aMenuItem);
    end;
  end;

  miDeleteIndicator.Clear;
  miDeleteExpert.Clear;
  for i:=0 to FChart.Indicators.Count-1 do
  begin
    if not Supports(FChart.Indicators[i].Indicator,ISCExpert) then
    begin
      aMenuItem:=TIndicatorMenuItem.Create(self);
      aMenuItem.Caption:=TrimCaption(FChart.Indicators[i].Indicator.Caption);
      //aMenuItem.Enabled:=not IsTraderLineExists(FChart.Indicators[i].Indicator);
      aMenuItem.OnClick:=miDeleteIndicatorClick;
      TIndicatorMenuItem(aMenuItem).IndicatorID:=FChart.Indicators[i].Indicator.GetID;
      if not Supports(FChart.Indicators[i].Indicator,ISCIndicatorShape) then
        miDeleteIndicator.Add(aMenuItem)
      else
        miDeleteShape.Add(aMenuItem);
    end
    else begin
      aMenuItem:=TExpertMenuItem.Create(self);
      aMenuItem.Caption:=TrimCaption(FChart.Indicators[i].Indicator.Caption);
      //aMenuItem.Enabled:=not FOrderManager.IsExpertLineExists(FChart.Experts[i].Expert);
      aMenuItem.OnClick:=miDeleteExpertClick;
      TExpertMenuItem(aMenuItem).ExpertID:=FChart.Indicators[i].Indicator.GetID;
      miDeleteExpert.Add(aMenuItem);
    end;
  end;

  miDeleteShape.Enabled:=miDeleteShape.Count>0;
  miDeleteIndicator.Enabled:=miDeleteIndicator.Count>0;
  miDeleteExpert.Enabled:=miDeleteExpert.Count>0;    
end;

procedure TfrmStockChart.pmIndicatorGetControlClass(Sender: TCustomActionBar; AnItem: TActionClient;
  var ControlClass: TCustomActionControlClass);
begin
  if (AnItem is  TActionClientItem) and (TActionClientItem(AnItem).Caption=miIndicatorName.Caption) then
  begin
    ControlClass:=TCaptionMenuItem;
  end;
end;

procedure TfrmStockChart.miEditIndicatorClick(Sender: TObject);
begin
  if sender is TExpertMenuItem then
    FSelectedIndicatorId:=(sender as TExpertMenuItem).ExpertID
  else
    FSelectedIndicatorId:=(sender as TIndicatorMenuItem).IndicatorID;
  acEditIndicatorExecute(nil);
end;

procedure TfrmStockChart.miDeleteIndicatorClick(Sender: TObject);
begin
  FSelectedIndicatorId:=(sender as TIndicatorMenuItem).IndicatorID;
  acDeleteIndicatorExecute(nil);
end;

procedure TfrmStockChart.acEditIndicatorExecute(Sender: TObject);
var
  aIndicator: ISCIndicator;
begin
  aIndicator:=GetSelectedIndicator;
  if (aIndicator<>nil) then
    aIndicator.ShowPropertyWindow;
end;

procedure TfrmStockChart.acEditIndicatorUpdate(Sender: TObject);
var
  aIndicator: ISCIndicator;
  aAction : TAction;
begin
  aIndicator:=GetSelectedIndicator;
  aAction:=(sender as TAction);
  aAction.Enabled:=(aIndicator<>nil) and (aIndicator.HasPropertyWindow);
{  if aAction.Enabled then
    aAction.Caption:=FIndicatorEditPrefix+' '+TrimCaption(aIndicator.Caption)
  else
    aAction.Caption:=FIndicatorEditPrefix;}
end;

procedure TfrmStockChart.acGetTicksInRegionExecute(Sender: TObject);
var
  aRegion: TSCHilightRegion;
  aX1,aX2: TDateTime;
begin
  aRegion:=FChart.MainIndicatorWindow.HilightRegions.FindById(IHL);
  if aRegion<>nil then
  begin
    aX1:=FChart.InputData.DirectGetItem_DataDateTime(Max(0,Min(aRegion.FirstIndex,aRegion.LastIndex)));
    aX2:=FChart.InputData.DirectGetItem_DataDateTime(Min(Max(aRegion.FirstIndex,aRegion.LastIndex),FChart.InputData.Count-1));
    TfmGetTicksDialog.Run(FMeAsInterface,aX1,aX2);
  end;
end;

procedure TfrmStockChart.acHelpIndicatorExecute(Sender: TObject);
begin
  inherited;
  ContextHelpProvider.ShowHelp(GetSelectedIndicator);
end;

procedure TfrmStockChart.acHelpIndicatorUpdate(Sender: TObject);
var
  aIndicator: ISCIndicator;
begin
  aIndicator:=GetSelectedIndicator;
  (sender as TAction).Enabled:=(aIndicator<>nil) and (ContextHelpProvider<>nil) and (ContextHelpProvider.IsHelpAvailable(aIndicator));
end;

procedure TfrmStockChart.acDeleteIndicatorExecute(Sender: TObject);
var
  aIndicator: ISCIndicatorDrawSupport;
begin
  aIndicator:=GetSelectedIndicator;
  if (aIndicator<>nil) then
  begin
    //Все оставльные индикаторы
    if CheckOnDelete(aIndicator) then
      FChart.Indicators.Remove(aIndicator);
  end;

  aIndicator:=nil; //Для отладки. Это последняя сслыка на объект
end;

procedure TfrmStockChart.acInvalidateIndicatorExecute(Sender: TObject);
var
  aIndicator: ISCIndicatorDrawSupport;
begin
  aIndicator:=GetSelectedIndicator;
  if (aIndicator<>nil) then
  begin
    aIndicator.Invalidate(0,FChart.InputData.Count-1);
    FChart.Repaint();
  end;
end;

procedure TfrmStockChart.acPasteIndicatorExecute(Sender: TObject);
var
  aReader : TSerialize;
  aStream: TMemoryStream;
  Handle: THandle;
  Mem: Pointer;
  aIndicator: ISCIndicator;
begin
  Handle := Clipboard.GetAsHandle(IndicatorClipboardFormat);
  if Handle=0 then
    exit;

  aStream:=TMemoryStream.Create;
  Mem := GlobalLock(Handle);
  try
    aStream.Write(Mem^, GlobalSize(Handle));
    aStream.Seek(0,soFromBeginning);

    aReader:=TSCSerialize.Create;
    try
      aIndicator:=aReader.LoadObject(aStream) as ISCIndicator;
      FChart.InitIndicator(aIndicator as ISCIndicatorDrawSupport);
      FChart.Indicators.Add(aIndicator as ISCIndicatorDrawSupport);
    finally
      aReader.Free;
    end;
  finally
    GlobalUnlock(Handle);
    aStream.Free;
  end;
end;

procedure TfrmStockChart.acPasteIndicatorUpdate(Sender: TObject);
begin
  inherited;
  TAction(Sender).Enabled:=Clipboard.HasFormat(IndicatorClipboardFormat);
end;

procedure TfrmStockChart.acFormPopupExecute(Sender: TObject);
var
  aForm: TCustomForm;
begin
  inherited;
  aForm:=GetParentForm(self);                                   
  if aForm<>nil then
    if TForm(aForm).FormStyle=fsMDIChild then
      TForm(aForm).FormStyle:=fsNormal
    else
      TForm(aForm).FormStyle:=fsMDIChild;
end;

procedure TfrmStockChart.acFormPopupUpdate(Sender: TObject);
var
  aForm: TCustomForm;
begin
  inherited;
  aForm:=GetParentForm(self);
  TAction(Sender).Enabled:=aForm<>nil;
  if aForm<>nil then
    TAction(Sender).Checked:=TForm(aForm).FormStyle<>fsMDIChild;
end;

procedure TfrmStockChart.acIndicatorBringToFrontExecute(Sender: TObject);
var
  aIndicator: ISCIndicatorDrawSupport;
begin
  aIndicator:=GetSelectedIndicator;
  if (aIndicator<>nil) then
  begin
    FChart.BringIndicatorToFront(aIndicator);
    RaiseChangedEvent(ekIndicatorChanged);
  end;
end;

procedure TfrmStockChart.acIndicatorSendToBackExecute(Sender: TObject);
var
  aIndicator: ISCIndicatorDrawSupport;
begin
  aIndicator:=GetSelectedIndicator;
  if (aIndicator<>nil) then
  begin
    FChart.SendIndicatorToBack(aIndicator);
    RaiseChangedEvent(ekIndicatorChanged);
  end;
end;

procedure TfrmStockChart.acIndicatorWindowMoveDownExecute(Sender: TObject);
begin
  FSelectedWindow.HorizontalPosition:=FSelectedWindow.HorizontalPosition+1;
end;

procedure TfrmStockChart.acIndicatorWindowMoveDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(FSelectedWindow<>nil) and FSelectedWindow.CanMoveDown;
end;

procedure TfrmStockChart.acIndicatorWindowMoveUpExecute(Sender: TObject);
begin
  FSelectedWindow.HorizontalPosition:=FSelectedWindow.HorizontalPosition-1;
end;

procedure TfrmStockChart.acIndicatorWindowMoveUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=(FSelectedWindow<>nil) and FSelectedWindow.CanMoveUp;
end;

procedure TfrmStockChart.acInvalidateAllExecute(Sender: TObject);
begin
  FChart.InvalidateAll
end;

procedure TfrmStockChart.acDeleteIndicatorUpdate(Sender: TObject);
var
  aIndicator: ISCIndicator;
  aAction : TAction;
begin
  aIndicator:=GetSelectedIndicator;
  aAction:=(sender as TAction);
  aAction.Enabled:=(aIndicator<>nil);
{  if aAction.Enabled then
    aAction.Caption:=FIndicatorDeletePrefix+' '+TrimCaption(aIndicator.Caption)
  else
    aAction.Caption:=FIndicatorDeletePrefix;}
end;

procedure TfrmStockChart.OnShowCharacteristic(Sender: TObject);
var
  aIndicator:ISCIndicatorCompositeCharacteristic;
begin
  aIndicator:=CreateIndicator(IndicatorFactory.GetIndicatorInfo(ISCIndicatorCompositeCharacteristic),false) as ISCIndicatorCompositeCharacteristic;
  aIndicator.SetIndicatorCharacteristic((sender as TCharacteristicMenuItem).Indicator,(sender as TCharacteristicMenuItem).Characteristic);
end;

procedure UpdateMenuItems(aMenu: TMenu);
var
  i          : integer;
begin
  if aMenu<>nil then
    with aMenu do
      for i := 0 to Items.Count - 1 do
        Items[I].InitiateAction;
end;

procedure TfrmStockChart.OnStockChartContextMenu(Sender: TObject; aPos: TPoint; var Handled: Boolean);
var
  aIndicator : ISCIndicatorDrawSupport;
  aItem      : TSCIndicatorCollectionItem;
  aMI_UT     : TUnitTaskMenuItem;
  aMI_CH     : TCharacteristicMenuItem;
  aMI2       : TMenuItem;
  aRegion    : TSCHilightRegion;
  aX         : integer;
  aY         : TStockRealNumber;
  i          : integer;
  s          : string;
  aCharacteristics: ISCIndicatorCharacteristicCollection;
begin
  FSelectingRegion:=false;
  FChart.GetCrosshairPos(FCrosshairPos.X,FCrosshairPos.Y);

  FSelectedWindow:=FChart.GetIndicatorWindowFromPoint(aPos,aPos);
  aRegion:=nil;
  if (FSelectedWindow <> nil) then
  begin
    aIndicator:=FSelectedWindow.GetIndicatorFromDevicePoint(aPos);
    if aIndicator<>nil then
    begin
      aItem:=FChart.Indicators.FindByIndicator(aIndicator);
      if (aItem<>nil) then
      begin
        FSelectedIndicatorId:=aItem.Indicator.GetID;
        PopupMenu:=pmIndicator;
      end;
    end
    else begin
      PopupMenu:=pmChart;
    end;

    aRegion:=FSelectedWindow.HilightRegions.FindById(IHL);
    if aRegion<>nil then
      if (FSelectedWindow.GetXYFromDevicePoint(aPos,aX,aY)) and
         (aX>=min(aRegion.FirstIndex,aRegion.LastIndex)) and
         (aX<=max(aRegion.FirstIndex,aRegion.LastIndex)) then
        ///
      else
        aRegion:=nil;
  end
  else begin
    PopupMenu:=pmChart;
  end;

  acSyncRegion.Visible:=aRegion<>nil;
  acGetTicksInRegion.Visible:=aRegion<>nil;
  acReplayTicksInRegion.Visible:=aRegion<>nil;
  acReplayTicksInRegion2.Visible:=aRegion<>nil;


  //Bold
  acBoldIndicator.Visible:=(GetSelectedIndicator<>nil) and (Supports(GetSelectedIndicator,ISCIndicatorHilightSupport));
  if acBoldIndicator.Visible then
  begin
    acBoldIndicator.Checked:=(GetSelectedIndicator as ISCIndicatorHilightSupport).GetHilighted;
  end;

  //Indicator Caption
  if GetSelectedIndicator<>nil then
  begin
    miIndicatorName.Caption:=TrimCaption(GetSelectedIndicator.Caption);
  end;

  //Tasks
  if GetSelectedIndicator<>nil then
  begin
    //miTasks.Caption:=TrimCaption(GetSelectedIndicator.Caption)+' Tasks';
    miTasks.Clear;

    aMI2:=TMenuItem.Create(pmIndicator);
    aMI2.Action:=acChangeIndicatorDataSource;
    miTasks.Add(aMI2);

    aMI2:=TMenuItem.Create(pmIndicator);
    aMI2.Caption:='-';
    miTasks.Add(aMI2);


    for i := 0 to StockUnitTaskRegistry.UnitTaskCount - 1 do
      if StockUnitTaskRegistry.UnitTasks[i].CanApply(GetSelectedIndicator,s) then
      begin
        aMI_UT:=TUnitTaskMenuItem.Create(pmIndicator);
        aMI_UT.Caption:=s;
        aMI_UT.OnClick:=OnPerformTask;
        aMI_UT.UnitTask:=StockUnitTaskRegistry.UnitTasks[i];
        //aItem2.OnClick:=miAddIndicatorClick;
        miTasks.Add(aMI_UT);
      end;

    miTasks.Enabled:=miTasks.Count>0;
  end;

  //Characteristics
  if GetSelectedIndicator<>nil then
  begin
//    miCharacteristics.Caption:=TrimCaption(GetSelectedIndicator.Caption)+' Characteristcis';
    miCharacteristics.Clear;

    aCharacteristics:=GetSelectedIndicator.GetCharacteristics;
    for i := 0 to aCharacteristics.Count - 1 do
    begin
      aMI_CH:=TCharacteristicMenuItem.Create(pmIndicator);
      aMI_CH.Caption:=aCharacteristics[i].GetName;
      aMI_CH.Hint:=aCharacteristics[i].GetDescription;
      aMI_CH.OnClick:=OnShowCharacteristic;
      aMI_CH.Characteristic:=aCharacteristics[i];
      aMI_CH.Indicator:=GetSelectedIndicator;
      miCharacteristics.Add(aMI_CH);
    end;


    miCharacteristics.Enabled:=(miCharacteristics.Count>0) and (IndicatorFactory.GetIndicatorInfo(ISCIndicatorCompositeCharacteristic)<>nil);
  end;

  //Обязательно нужно выполнить Update до тех пор пока меню не нарисовали
  if PopupMenu<>nil then
    UpdateMenuItems(PopupMenu);
end;

procedure TfrmStockChart.LoadTheme;
var
  aTheme: ISCTheme;
  aRegistry : TRegistry;
  aKeyName  : string;
begin
  if FThemeName='' then
    exit;
  aKeyName:=Workspace.MainRegistryKey+'\'+FThemeName;

  aRegistry:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    if not aRegistry.KeyExists(aKeyName) then
      exit;
    aRegistry.OpenKey(aKeyName,false);
    if not aRegistry.ValueExists('Data') then
      exit;
  finally
    aRegistry.Free;
  end;

  try
    aTheme:=TSCTheme.Create;
    aTheme.DeserializeFromRegistryKey(HKEY_CURRENT_USER,aKeyName,'Data');
    FChart.CurrentTheme:=aTheme;
  except
    on E: Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;
end;

procedure TfrmStockChart.LocateTo(const aX: integer; aLocateMode:TStockChartLocateMode);
begin
  case aLocateMode of
    lmLeft  : FChart.FirstVisible:=max(0,aX-FChart.VisibleValuesCount+1);
    lmCenter: FChart.FirstVisible:=max(0,aX-FChart.VisibleValuesCount div 2);
    lmRight : FChart.FirstVisible:=aX;
  end;
end;

procedure TfrmStockChart.SaveTheme;
var
  aKeyName  : string;
begin
  if FThemeName='' then exit;

  aKeyName:=Workspace.MainRegistryKey+'\'+FThemeName;
  try
    FChart.CurrentTheme.SerializeToRegistryKey(HKEY_CURRENT_USER,aKeyName,'Data');
  except
    on E:Exception do
      Workspace.ExceptionManager.Publish(E,self);
  end;
end;
(*
procedure TfrmStockChart.LoadTheme(aStream: TStream);
var
  aTheme: ISCTheme;
begin
  aTheme:=TSCTheme.Create;
  aTheme.DeserializeFromStream(aStream);
  FChart.CurrentTheme:=aTheme;
end;

procedure TfrmStockChart.SaveTheme(aStream: TStream);
begin
  FChart.CurrentTheme.SerializeToStream(aStream);
end;
*)

procedure TfrmStockChart.SetTheme(const aThemeName: string);
begin
  FThemeName:=aThemeName;
  LoadTheme;
end;

function TfrmStockChart.GetPriceLine(const aID: TGUID; aCreate: boolean):ISCIndicatorPriceLine;
begin
  if aCreate then
    result:=PriceLineByIDSafe(aID)
  else
    result:=PriceLineByID(aID);
end;

procedure TfrmStockChart.SaveData(aStream: TStream);
var
  aSerialize: TSCSerialize;
  aProxy: TfrmStockChartSerializationProxy;
begin
  aSerialize:=TSCSerialize.Create;
  aProxy:=TfrmStockChartSerializationProxy.Create(self);
  try
    aSerialize.SaveObject(aProxy,aStream);
  finally
    aSerialize.Free;
    aProxy.Free;
  end;
end;

procedure TfrmStockChart.LoadData(aStream: TStream);
var
  aSerialize: TSCSerialize;
  aProxy: TfrmStockChartSerializationProxy;
begin
  aSerialize:=TSCSerialize.Create;
  aProxy:=TfrmStockChartSerializationProxy.Create(self);
  try
    aSerialize.LoadObject(aStream, TObject(aProxy));
  finally
    aSerialize.Free;
    aProxy.Free;
  end;
end;

procedure TfrmStockChart.SetShowCrosshair(aValue: boolean);
begin
  FChart.Crosshair.Visible:=aValue;
end;

procedure TfrmStockChart.SetShowRuler(aValue: boolean);
begin
  FChart.Ruler.Visible:=aValue;
end;

procedure TfrmStockChart.SetShowPosition(aValue: boolean);
var
  aRect: TRect;
begin
  laPosition.Visible:=aValue;
  if aValue<>GetShowPosition then
    RaiseChangedEvent(ekPropertyChanged);

  if aValue then
  begin
    aRect:=FChart.ClientRect;
    FChart.AdjustClientRect(aRect);

    laPosition.Caption:='100%';
    laPosition.Left:=aRect.Right-laPosition.Width-8;

    SetPositionLabelTitle;
    if (laPosition.Parent<>nil) and (GetParentForm(self)<>nil) then
      laPosition.BringToFront;
  end;
end;

procedure TfrmStockChart.SetStockSymbol(const aSymbolName: string; aInterval: TStockTimeInterval);
begin
  FreeAndNil(FStockSymbol);
  FStockSymbol := TStockSymbol.Create(aSymbolName,aInterval);
  FChart.Tag:=FStockSymbol.GetTimeIntervalValue;
  FChart.Name:='Chart_'+IntToStr(FStockSymbol.GetTimeIntervalValue);
end;

procedure TfrmStockChart.SetDragCursor(shift: TShiftState);
begin
   if (ssShift in Shift) or (ssCtrl in Shift) then
     imScale.Cursor:=crSizeNS
   else
     imScale.Cursor:=crSizeAll;


 if TFriendControl(imScale).MouseCapture or (FChart.ControlAtPos(FChart.ScreenToClient(Mouse.CursorPos),false)=imScale) then
 begin
  Windows.SetCursor(Screen.Cursors[imScale.Cursor]);
 end;
end;

procedure TfrmStockChart.SetFixedVerticalScale(aValue: boolean);
var
  y1,y2: TSCRealNumber;
  aRect: TRect;
begin
  if aValue<>GetFixedVerticalScale then
    RaiseChangedEvent(ekPropertyChanged);

  if FChart.MainIndicatorWindow.YScale.Fixed<>aValue then
  begin
    y1:=FChart.MainIndicatorWindow.MinYValue;
    y2:=FChart.MainIndicatorWindow.MaxYValue;
    FChart.MainIndicatorWindow.YScale.Fixed:=aValue;
    FChart.MainIndicatorWindow.YScale.MinValue:=y1;
    FChart.MainIndicatorWindow.YScale.MaxValue:=y2;
  end;

  imScale.Visible:=aValue;
  if aValue then
  begin
    aRect:=FChart.ClientRect;
    FChart.AdjustClientRect(aRect);
    imScale.Left:=aRect.Right-50;

    if (imScale.Parent<>nil) and (GetParentForm(self)<>nil) then
      imScale.BringToFront;
  end;
end;

function TfrmStockChart.CreateIndicator(aIndicatorInfo: ISCIndicatorInfo; aShowPropDialog: boolean=true): ISCIndicator;
begin
  result:=FChart.CreateAndInitIndicator(aIndicatorInfo.IID,aShowPropDialog);
end;

function TfrmStockChart.CreateExpert(aExpertInfo: ISCExpertInfo;aShowPropDialog: boolean): ISCExpert;
var
  aExpert: ISCExpert;
begin
  aExpert:=CreateIndicator(aExpertInfo,aShowPropDialog) as ISCExpert;
  if aExpert=nil then
    exit;
  RaiseChangedEvent(ekIndicatorCollectionChanged);
end;

function TfrmStockChart.CreateShape(aShapeInfo: ISCIndicatorInfo;aWindow: TSCIndicatorWindow): ISCIndicatorShape;
var
  p: TPoint;
begin
  result:=IndicatorFactory.CreateIndicator(aShapeInfo.IID) as ISCIndicatorShape;
  FChart.InitIndicator(result as ISCIndicatorDrawSupport);
  FChart.SetMRUProperties(result as ISCIndicatorDrawSupport);

(*
  if (result.HasPropertyWindow) and (aShowPropDialog) then
  if not result.ShowPropertyWindow then
  begin
    result:=nil;
    exit;
  end;
*)

  aWindow.AddIndicator(result as ISCIndicatorDrawSupport);
  if not (GetCursorPos(p)) then
    p:=Point(aWindow.ClientWidth div 2, aWindow.ClientHeight div 2)
  else
    p:=aWindow.ScreenToClient(p);

  aWindow.InitPlacement(result,p.X,p.Y);
  aWindow.BeginMoveLoop(result,p.X,p.Y);
  RaiseChangedEvent(ekIndicatorCollectionChanged);
end;

procedure TfrmStockChart.Activate;
var
  i: integer;
  aFocusedControl: TWinControl;
begin
  inherited;
  FActive:=true;

  for i:=0 to FChart.IndicatorWindowCount-1 do
    FChart.IndicatorWindows[i].HilightRegions.RemoveById(IMark);

  aFocusedControl:=nil;
  for i:=0 to ControlCount-1 do
    if (Controls[i] is TWinControl) then
      if TWinControl(Controls[i]).Focused then
      begin
        aFocusedControl:=TWinControl(Controls[i]);
        break;
      end;

  if aFocusedControl=nil then
    FChart.SetFocus;

  laSymbol.Font.Color:=clHighlight;
  laPosition.Font.Color:=clHighlight;

  SetShowPosition(GetShowPosition); //Обновить состояние Position
  SetFixedVerticalScale(GetFixedVerticalScale); //Нужно для пеового запуска чтобы вытащить картинку наверх

  if (not FActivatedEarly) then
  begin
    FActivatedEarly:=true;

    i:=FChart.InputData.FindBestMatched(FDefaultPositionDate);
    if i<>-1 then
      FChart.FirstVisible:=i;
  end;
  PostMessage(Handle,CM_POSTACTIVATE,0,0);  
end;

procedure TfrmStockChart.Deactivate;
begin
  laSymbol.Font.Color:=clGrayText;
  FActive:=false;
end;

procedure TfrmStockChart.OnTraderAdded(aTrader: IStockTrader);
begin
end;

procedure TfrmStockChart.OnTraderRemoved(aTrader: IStockTrader);
var
  aValue: ISCIndicatorTradeLine;
begin
  if FTraderLineMap.Lookup(aTrader.GetID,aValue) then
    FChart.Indicators.Remove(aValue as ISCIndicatorDrawSupport);

  FTraderLineMap.Delete(aTrader.GetID);
end;

procedure TfrmStockChart.OnTraderSetMark(const aSender: IStockTrader; const aTime: TDateTime;
  const aPrice: TStockRealNumber; const aMarkType: TSCChartMarkKind; const aMessage: string);
var
  aLine: ISCIndicatorTradeLine;
  aLeft,aRight: TDateTime;
begin
  aLine:=TradeLineByTraderSafe(aSender);
  aLine.BeginUpdate;

  try
    TStockDataUtils.AlignTime(aTime,StockSymbol.TimeInterval,aLeft,aRight);
    with aLine.AddItem do
    begin
      SetKind(aMarkType);
      SetOpenTime(aLeft);
      SetOpenPrice(aPrice);
      SetText(aMessage);
    end;
  finally
    aLine.EndUpdate;
  end;
end;

procedure TfrmStockChart.OnInvalidateTraderLine(const aSender: IStockTrader);
begin
  TradeLineByTraderSafe(aSender).Clear;
end;

procedure TfrmStockChart.OnPerformTask(Sender: TObject);
var
  aPos: TSCPoint;
begin
  aPos.X:=FCrosshairPos.X;
  aPos.Y:=FCrosshairPos.Y;

  (sender as TUnitTaskMenuItem).UnitTask.Perform(GetSelectedIndicator,FMeAsInterface,aPos);
end;

procedure TfrmStockChart.OnPropertyChanged(aNotifier: TProperty);
begin
  if aNotifier=FPropGridX then
    FChart.Colors.GridX:=FPropGridX.Value;
  if aNotifier=FPropGridY then
    FChart.Colors.GridY:=FPropGridY.Value;
end;

procedure TfrmStockChart.OnPropertyCreated(aNotifier: TProperty);
begin

end;

{ TStockChartInterfaceMediator }

destructor TStockChartInterfaceMediator.Destroy;
begin
  FTarget:=nil;
  inherited;
end;

procedure TfrmStockChart.OnTraderOpenOrder(aOrder: IStockOrder);
var
  aLine: ISCIndicatorTradeLine;
  aTrader : IStockTrader;
begin
  aTrader:=aOrder.GetTrader;
  aLine:=TradeLineByTraderSafe(aTrader); //Если линии нет, здесь она и создасться
  aLine.BeginUpdate;

  try
    with aLine.AddItem do
    begin
      SetOpenTime(aOrder.GetOpenTime);
      SetOpenPrice(aOrder.GetOpenPrice);
      SetOrderKind(aOrder.GetKind);
      SetID(aOrder.GetID);
    end;

    if aOrder.GetStopLoss<>0 then
      OnChangeOrderStopLoss(aOrder);
    if aOrder.GetTakeProfit<>0 then
      OnChangeOrderTakeProfit(aOrder);

  finally
    aLine.EndUpdate;
  end;
end;

procedure TfrmStockChart.OnTraderCloseOrder(aOrder: IStockOrder);
var
  i: integer;
  aTraderLine: ISCIndicatorTradeLine;
  aOrderLine : ISCIndicatorTradeLineItem;
begin
  aTraderLine:=TradeLineByTraderID(aOrder.GetTrader.GetID);

  if aTraderLine=nil then //такого, по идее, быть не может, потому как всегда сначала должна проходить операция Open
    exit;

  i:=aTraderLine.IndexOfItem(aOrder.GetID);
  if (i<>-1) then
  begin
    aOrderLine:=aTraderLine.GetItem(i);

    aOrderLine.SetCloseTime (aOrder.GetCloseTime);
    aOrderLine.SetClosePrice(aOrder.GetClosePrice);
    aOrderLine.SetText(aOrder.GetCloseComment);
    aOrderLine.SetWorstPrice(aOrder.GetWorstPrice);
    aOrderLine.SetBestPrice(aOrder.GetBestPrice);
  end;
end;

function TfrmStockChart.TradeLineByTraderIDSafe(const aTraderID: TGUID): ISCIndicatorTradeLine;
var
  aColor: TColorRef;
begin
  result:=TradeLineByTraderID(aTraderID);
  if result=nil then
  begin
    Assert(FChart.Indicators.FindByIndicator(aTraderID)=nil);

    result:=IndicatorFactory.CreateIndicator(ISCIndicatorTradeLine) as ISCIndicatorTradeLine;
    result.SetID(aTraderID);

    FChart.InitIndicator(result as ISCIndicatorDrawSupport);
    FChart.SetMRUProperties(result as ISCIndicatorDrawSupport);//????
    FChart.Indicators.Add(result as ISCIndicatorDrawSupport);
    result.SetBuyWidth(2);
    result.SetSellWidth(2);

    aColor:=ColorToRGB(TraderLineColors[FCurrentTradeColorIndex]);

    inc(FCurrentTradeColorIndex);
    if FCurrentTradeColorIndex>High(TraderLineColors) then
      FCurrentTradeColorIndex:=0;

    result.SetBuyColor(aColor);
    if aColor=RGB(255,255,255) then
      aColor:=0
    else
      aColor:=RGB(GetBValue(aColor),GetGValue(aColor),GetRValue(aColor));
    result.SetSellColor(aColor);

    FTraderLineMap.Add(aTraderID,result);
  end;
end;


function TfrmStockChart.TradeLineByTraderSafe(aTrader: IStockTrader): ISCIndicatorTradeLine;
var
  aGUID: TGUID;
begin
  if aTrader=nil then
    ZeroMemory(@aGUID,sizeof(aGUID))
  else
    aGUID:=aTrader.GetID;

  result:=TradeLineByTraderID(aGUID);
  if result=nil then
  begin
    result:=TradeLineByTraderIDSafe(aGUID);
    Assert(result<>nil);

    if aTrader<>nil then
      (result as ISCWritableName).SetName('Trader "'+aTrader.GetName+'"');
  end;
end;

function TfrmStockChart.TradeLineByTraderID(const aTraderID: TGUID): ISCIndicatorTradeLine;
var
  aValue: ISCIndicatorTradeLine;
begin
  if not FTraderLineMap.Lookup(aTraderID,aValue) then
  begin
    result:=nil
  end
  else begin
    result:=aValue as ISCIndicatorTradeLine;
  end;
end;

function TfrmStockChart.PriceLineByID(const aID: TGUID): ISCIndicatorPriceLine;
var
  i: integer;
  aIndicator: ISCIndicatorPriceLine;
begin
  result:=nil;

  for i := 0 to FChart.Indicators.Count - 1 do
  begin
    if Supports(FChart.Indicators2[i],ISCIndicatorPriceLine,aIndicator) and
       (IsEqualGUID(aIndicator.GetID,aID)) then
    begin
      result:=aIndicator;
      break;
    end;
  end;
end;

function TfrmStockChart.PriceLineByIDSafe(const aID: TGUID): ISCIndicatorPriceLine;
begin
  result:=PriceLineByID(aID);
  if result=nil then
  begin
    Assert(FChart.Indicators.FindByIndicator(aID)=nil);

    result:=IndicatorFactory.CreateIndicator(ISCIndicatorPriceLine) as ISCIndicatorPriceLine;
    result.SetID(aID);

    FChart.InitIndicator(result as ISCIndicatorDrawSupport);
    FChart.SetMRUProperties(result as ISCIndicatorDrawSupport);//????
    FChart.Indicators.Add(result as ISCIndicatorDrawSupport);
  end;
end;

function TfrmStockChart.TrimCaption(const aValue: string): string;
begin
  result:=TruncateString(aValue,60);
end;

function TfrmStockChart.IsTraderLineExists(aIndicator: ISCIndicator):boolean;
var
  it    : TTraderLineMapIterator;
begin
  result:=false;

  FTraderLineMap.GetFirst(it);
  while it.Valid do
  begin
    result:=(it.Value as ISCIndicator).IsThis(aIndicator);
    if result then
      break;
    FTraderLineMap.GetNext(it);
  end;
end;

function TfrmStockChart.IsTraderLineExists(aTrader: IStockTrader): boolean;
begin
  result:=FTraderLineMap.Lookup(aTrader.GetID);
end;

procedure TfrmStockChart.RaiseChangedEvent(aChangeKind: TStockChartEventKind);
var
  i: integer;
begin
  if (csDestroying in cOmponentState) or (FLoading) then
    exit;

  for i:=0 to High(FEventHandlers) do
    FEventHandlers[i].OnChanged(self.FMeAsInterface,aChangeKind);
end;

procedure TfrmStockChart.RaiseHilightedEvent(aX1, aX2: TDateTime);
var
  i: integer;
begin
  if (csDestroying in cOmponentState) then
    exit;
  for i:=0 to High(FEventHandlers) do
    FEventHandlers[i].OnHilighted(self.FMeAsInterface,aX1,aX2);
end;

procedure TfrmStockChart.OnStockChartIndicatorChanged(sender: TStockChart;aObject: ISCIndicatorDrawSupport);
begin
  RaiseChangedEvent(ekIndicatorChanged);
end;

procedure TfrmStockChart.OnStockChartIndicatorDeleted(sender: TStockChart; aObject: ISCIndicatorDrawSupport);
var
  i: integer;
begin
  FTraderLineMap.Delete(aObject.GetID);
  RaiseChangedEvent(ekIndicatorCollectionChanged);

  for I := aObject.GetAttributes.Count-1 downto 0 do
    if Supports(aObject.GetAttributes.Items[i],IStockChartAttribute) then
      aObject.GetAttributes.Remove(aObject.GetAttributes.Items[i]);
end;

procedure TfrmStockChart.OnStockChartIndicatorAdded(sender: TStockChart; aObject: ISCIndicatorDrawSupport);
var
  i: integer;
begin
  for I := aObject.GetAttributes.Count-1 downto 0 do
    if Supports(aObject.GetAttributes.Items[i],IStockChartAttribute) then
      aObject.GetAttributes.Remove(aObject.GetAttributes.Items[i]);

  aObject.GetAttributes.Add(TStockChartAttribute.Create(self));

  if (Supports(aObject,ISCIndicatorCalendar)) then
  begin
    try
      (aObject as ISCIndicatorCalendar).SetData(CalendarManager.Calendar);
    except
      on E:Exception do
        Application.Definitions.Workspace.ExceptionManager.Publish(E,self);
    end;
  end;

  RaiseChangedEvent(ekIndicatorCollectionChanged);
end;

procedure TfrmStockChart.OnStockChartPropertyChanged(sender: TStockChart);
begin
  if FLockCount=0 then
    RaiseChangedEvent(ekPropertyChanged);
end;

procedure TfrmStockChart.OnStockChartVisibleRangeChanged(sender: TStockChart);
begin
  if FLockCount=0 then
    RaiseChangedEvent(ekVisibleRangeChanged);

  if GetShowPosition then
    SetPositionLabelTitle;
end;

procedure TfrmStockChart.OnStockChartMouseDblClick(Sender: TObject);
var
  aIndicator : ISCIndicatorDrawSupport;
  aItem      : TSCIndicatorCollectionItem;
  aPos       : TPoint;
begin
  aPos:=FChart.ScreenToClient(Mouse.CursorPos);
  FSelectedWindow:=FChart.GetIndicatorWindowFromPoint(aPos,aPos);
  if (FSelectedWindow <> nil) then
  begin
    aIndicator:=FSelectedWindow.GetIndicatorFromDevicePoint(aPos);
    if aIndicator<>nil then
    begin
      aItem:=FChart.Indicators.FindByIndicator(aIndicator);
      if (aItem<>nil) then
      begin
        FSelectedIndicatorId:=aItem.Indicator.GetID;
        acEditIndicator.Execute;
      end;
    end;
  end;
end;

procedure TfrmStockChart.OnStockChartMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aPt: TPoint;
  aX : integer;
  aY : TStockRealNumber;
begin
  laSymbol.BringToFront;

  FSelectingRegion:=false;

  if (Shift=[ssShift,ssLeft]) and (Button=mbLeft) and (FChart.DraggingObject=nil) then
  begin
    FSelectedWindow:=FChart.GetIndicatorWindowFromPoint(FChart.ScreenToClient(Mouse.CursorPos),aPt);
    if (FSelectedWindow<>nil) and FSelectedWindow.GetXYFromDevicePoint(aPt,aX,aY) then
    begin
      FSelectingRegion:=true;
      Hilight(aX,aX);
    end;
  end
end;

procedure TfrmStockChart.OnStockChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  aX: integer;
  aY: TStockRealNumber;
  aRegion: TSCHilightRegion;
begin
  inherited;
  if not FActive then
    exit;

  if FSelectingRegion and (FChart.DraggingObject=nil) then
  begin
    if FSelectedWindow.GetXYFromDevicePoint(FSelectedWindow.ScreenToClient(Mouse.CursorPos),aX,aY) then
    begin
      aRegion:=FChart.MainIndicatorWindow.HilightRegions.FindById(IHL);
      if aRegion<>nil then
      begin
        Hilight(aRegion.FirstIndex,aX);
        //aRegion.LastIndex:=aX;
      end;
    end;
  end;

  RaiseChangedEvent(ekCrosshairChanged);
end;

procedure TfrmStockChart.Hilight(aX1, aX2: integer);
var
  aRegion: TSCHilightRegion;
  i: integer;
begin
  for i:=0 to FChart.IndicatorWindowCount-1 do
  begin
    aRegion:=FChart.IndicatorWindows[i].HilightRegions.FindById(IHL);
    if aRegion=nil then
    begin
      aRegion:=TSCHilightRegion.Create;
      aRegion.ID:=IHL;
      FChart.IndicatorWindows[i].HilightRegions.Add(aRegion);
    end;

    aRegion.FirstIndex:=ax1;
    aRegion.LastIndex:=ax2;
  end;
end;

procedure TfrmStockChart.imScaleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMovingSchartMouseYPos:=Y;
  FMovingSchartMouseXPos:=X;
  FMovingSchartMinValue:=FChart.MainIndicatorWindow.YScale.MinValue;
  FMovingSchartMaxValue:=FChart.MainIndicatorWindow.YScale.MaxValue;
  TFriendControl(imScale).MouseCapture:=true;
  SetDragCursor(shift);  
end;

procedure TfrmStockChart.imScaleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if TFriendControl(imScale).MouseCapture then
  begin
    if (ssShift in Shift)  or (ssCtrl in Shift) then
    begin
      FChart.MainIndicatorWindow.YScale.MinValue:=min(FMovingSchartMinValue+FChart.InputData.PointToPrice(y-FMovingSchartMouseYPos),
                                                      FChart.MainIndicatorWindow.YScale.MaxValue);

      FMovingSchartMinValue:=FChart.MainIndicatorWindow.YScale.MinValue;
    end
    else begin
      FChart.MainIndicatorWindow.YScale.MinValue:=FMovingSchartMinValue+ FChart.InputData.PointToPrice(-y+FMovingSchartMouseYPos);
      FChart.MainIndicatorWindow.YScale.MaxValue:=FMovingSchartMaxValue+ FChart.InputData.PointToPrice(-y+FMovingSchartMouseYPos);

      FMovingSchartMinValue:=FChart.MainIndicatorWindow.YScale.MinValue;
      FMovingSchartMaxValue:=FChart.MainIndicatorWindow.YScale.MaxValue;

      FChart.Shift(x-FMovingSchartMouseXPos);
    end;

    FMovingSchartMouseYPos:=Y;
    FMovingSchartMouseXPos:=X;
  end;

  SetDragCursor(shift);  
end;

procedure TfrmStockChart.Mark(aX: integer);
var
  aRegion: TSCHilightRegion;
  i: integer;
begin
  for i:=0 to FChart.IndicatorWindowCount-1 do
  begin
    aRegion:=FChart.IndicatorWindows[i].HilightRegions.FindById(IMark);
    if aRegion=nil then
    begin
      aRegion:=TSCHilightRegion.Create;
      aRegion.ID:=IMark;
      aRegion.DrawLineIfEmty:=true;
      aRegion.FillColor:=clRed;

      FChart.IndicatorWindows[i].HilightRegions.Add(aRegion);
    end;

    aRegion.FirstIndex:=ax;
    aRegion.LastIndex:=ax;
  end;
end;

procedure TfrmStockChart.OnStockChartMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectingRegion:=false;
end;

procedure TfrmStockChart.OnStockChartExit(Sender: TObject);
begin
  FSelectingRegion:=false;
end;

procedure TfrmStockChart.acShiftTicksExecute(Sender: TObject);
var
  aDateTime: TDateTime;
begin
  inherited;

  aDateTime:=FChart.InputData.DirectGetItem_DataDateTime(FCrosshairPos.X);
  FProject.ShiftTicks(aDateTime);
end;

procedure TfrmStockChart.acChartShowCrosshairExecute(aAction: TCustomAction);
begin
  SetShowCrosshair(not FChart.Crosshair.Visible);
end;

procedure TfrmStockChart.acChartShowRulerExecute(aAction: TCustomAction);
begin
  SetShowRuler(not FChart.Ruler.Visible);
end;

procedure TfrmStockChart.acChartShowCrosshairUpdate(aAction: TCustomAction);
begin
  aAction.Checked:=GetShowCrosshair;
end;

procedure TfrmStockChart.acChartShowRulerUpdate(aAction: TCustomAction);
begin
  aAction.Checked:=GetShowRuler;
end;

procedure TfrmStockChart.acChartShowPositionExecute(Sender: TCustomAction);
begin
  inherited;
  SetShowPosition(not GetShowPosition);
end;

procedure TfrmStockChart.acChartShowPositionUpdate(Sender: TCustomAction);
begin
  inherited;
  TAction(Sender).Checked:=GetShowPosition;
end;

procedure TfrmStockChart.acCopyIndicatorExecute(Sender: TObject);
var
  aCopier: TSCCopier;
  aWriter : TSCSerialize;
  aStream: TMemoryStream;
  Handle: THandle;
  Mem: Pointer;
  aSrcIndicator: ISCIndicatorDrawSupport;
  aDstIndicator: ISCIndicatorDrawSupport;
  i: integer;
begin
  aStream:=TMemoryStream.Create;
  aWriter:=TSCSerialize.Create;
  try
    aCopier:=TSCCopier.Create;

    try
      aSrcIndicator:=GetSelectedIndicator;
      aDstIndicator:=IndicatorFactory.CreateIndicator(aSrcIndicator.GetIID) as ISCIndicatorDrawSupport;
      //Копируем свойства индикатора, и ничего больше
      for i := 0 to aDstIndicator.GetProperties.Count-1 do
        aCopier.CopyPropertiesShallow(aSrcIndicator.GetProperties[i] as IPersistentObject,aDstIndicator.GetProperties[i] as IPersistentObject);
    finally
      aCopier.Free;
    end;

    aDstIndicator.SetInputData(nil);
    aDstIndicator.SetWindow(nil);
    aDstIndicator.SetStockChart(nil);

    aWriter.SaveObject(aDstIndicator,aStream);
    aDstIndicator:=nil;

    // Allocate a global memory object for the text.
    Handle := GlobalAlloc(GMEM_MOVEABLE, aStream.Size);
    if (Handle = 0) then
      exit;

    // Lock the handle and copy the text to the buffer.
    Mem := GlobalLock(Handle);
    Move(aStream.Memory^,Mem^,aStream.Size);
    GlobalUnlock(Handle);

    Clipboard.Open;
    Clipboard.SetAsHandle(IndicatorClipboardFormat, Handle);
    Clipboard.Close;
  finally
    aWriter.Free;
  end;
end;

procedure TfrmStockChart.acCopyIndicatorUpdate(Sender: TObject);
var
  aIndicator: ISCIndicator;
  aAction : TAction;
begin
  aIndicator:=GetSelectedIndicator;
  aAction:=(sender as TAction);
  aAction.Enabled:=(aIndicator<>nil);
end;

procedure TfrmStockChart.acSyncRegionExecute(Sender: TObject);
var
  aRegion: TSCHilightRegion;
  aX1,aX2: TDateTime;
begin
  aRegion:=FChart.MainIndicatorWindow.HilightRegions.FindById(IHL);
  if aRegion<>nil then
  begin
    aX1:=FChart.InputData.DirectGetItem_DataDateTime(Max(0,Min(aRegion.FirstIndex,aRegion.LastIndex)));
    aX2:=FChart.InputData.DirectGetItem_DataDateTime(Min(Max(aRegion.FirstIndex,aRegion.LastIndex),FChart.InputData.Count-1));
    RaiseHilightedEvent(aX1,aX2);
  end;
end;

procedure TfrmStockChart.OnStockChartWindowsChanged(sender: TSCIndicatorWindowCollection);
begin
  if (not FLoading) and (not (csDestroying in ComponentState)) then
  begin
    //Нужно вытащить лейблы на сасый верх. BringToFront не работает
    //Приходится использовать Parent
    laSymbol.Parent:=nil;
    laSymbol.Parent:=FChart;

    laPosition.Parent:=nil;
    laPosition.Parent:=FChart;

    imScale.Parent:=nil;
    imScale.Parent:=FChart;
  end;
end;

procedure TfrmStockChart.acReplayTicksInRegionExecute(Sender: TObject);
var
  aRegion: TSCHilightRegion;
  aX1,aX2: TDateTime;
begin
  aRegion:=FChart.MainIndicatorWindow.HilightRegions.FindById(IHL);
  if aRegion<>nil then
  begin
    aX1:=FChart.InputData.DirectGetItem_DataDateTime(Min(aRegion.FirstIndex,aRegion.LastIndex));
    aX2:=FChart.InputData.DirectGetItem_DataDateTime(Max(aRegion.FirstIndex,aRegion.LastIndex));
    RaiseHilightedEvent(aX1,aX2);
    FProject.ReplayTicks(aX1,aX2,true);
  end;
end;

procedure TfrmStockChart.acReplayTicksInRegion2Execute(Sender: TObject);
var
  aRegion: TSCHilightRegion;
  aX1,aX2: TDateTime;
begin
  aRegion:=FChart.MainIndicatorWindow.HilightRegions.FindById(IHL);
  if aRegion<>nil then
  begin
    aX1:=FChart.InputData.DirectGetItem_DataDateTime(Min(aRegion.FirstIndex,aRegion.LastIndex));
    aX2:=FChart.InputData.DirectGetItem_DataDateTime(Max(aRegion.FirstIndex,aRegion.LastIndex));
    RaiseHilightedEvent(aX1,aX2);
    FProject.ReplayTicks(aX1,aX2,false);
  end;
end;

procedure TfrmStockChart.acBoldIndicatorExecute(Sender: TObject);
begin
  acBoldIndicator.Checked:=not acBoldIndicator.Checked;
  (GetSelectedIndicator as ISCIndicatorHilightSupport).SetHilighted(acBoldIndicator.Checked);
end;

procedure TfrmStockChart.acChangeIndicatorDataSourceExecute(Sender: TObject);

function DependsOfUs(const aWe,aIndicator:ISCIndicator):boolean;
var
  aMediator: IStockIndicatorToInputDataCollectionMediator;
begin
  result:=false;

  if aIndicator.GetInputData=nil then
    exit;

  if not Supports(aIndicator.GetInputData,IStockIndicatorToInputDataCollectionMediator,aMediator) then
    exit;

  if aMediator.GetDataSource.IsThis(aWe) then
    result:=true
  else
    result:=DependsOfUs(aWe,aMediator.GetDataSource);
end;

var
  aIndicator: ISCIndicator;
  aSourceIndicators: TStringList;
  aCustomData: IStockIndicatorToInputDataCollectionMediator;
  I,j: Integer;
begin
  inherited;
  aIndicator:=GetSelectedIndicator;
  if aIndicator=nil then
    exit;

  aSourceIndicators:=TStringList.Create;
  aSourceIndicators.Add('Stock data');

  try
    for I := 0 to FChart.Indicators2.Count - 1 do
      if Supports(FChart.Indicators2[i],ISCIndicatorValueSupport) then
        if not aIndicator.IsThis(FChart.Indicators2[i]) then
          if not DependsOfUs(aIndicator,FChart.Indicators2[i]) then
            aSourceIndicators.AddObject(FChart.Indicators2[i].Caption,pointer(FChart.Indicators2[i]));


    j:=0;
    if Supports(aIndicator.GetInputData,IStockIndicatorToInputDataCollectionMediator,aCustomData) then
    begin
      j:=-1;
      for i := 0 to aSourceIndicators.Count - 1 do
        if aSourceIndicators.Objects[i]<>nil then
          if aCustomData.GetDataSource.IsThis(ISCIndicator(pointer(aSourceIndicators.Objects[i]))) then
          begin
            j:=i;
            break;
          end;
    end;

    if TfmChooseItem.Run('Select Data Source for '+aIndicator.Caption, 'Data Source:',aSourceIndicators,j,i) then
    begin
      if i=0 then
        aIndicator.SetInputData(FChart.InputData)
      else
        aIndicator.SetInputData(TStockIndicatorToInputDataCollectionMediator.Create(ISCIndicator(pointer(aSourceIndicators.Objects[i])),nil));

      aIndicator.Invalidate(0,aIndicator.GetInputData.Count-1); //В принципе, это лишнее, должно выплоняться в самом индикаторе
      FChart.Repaint;
      RaiseChangedEvent(ekIndicatorChanged);
    end;
  finally
    aSourceIndicators.Free;
  end;

end;

procedure TfrmStockChart.acChangeIndicatorWindowExecute(Sender: TObject);
var
  i,j,aSelectedIndex: integer;
  aStrings : TStringList;
  s: string;
begin
  inherited;
  aSelectedIndex:=-1;

  aStrings:=TStringList.Create;
  try
    for i:=0 to FChart.IndicatorWindowCount-1 do
    begin
      s:='';
      for j := 0 to FChart.IndicatorWindows[i].IndicatorCount-1 do
      begin
        s:=s+FChart.IndicatorWindows[i].GetIndicator(j).Caption+'; ';
        if FChart.IndicatorWindows[i].GetIndicator(j).IsThis(GetSelectedIndicator) then
          aSelectedIndex:=i;
      end;

      aStrings.Add(s);
    end;

    if TfmChooseItem.Run('Chart Windows', 'Select chart window to place indicator',aStrings,aSelectedIndex,j) then
    begin
      if aSelectedIndex<>j then
      begin
        GetSelectedIndicator.SetWindow(FChart.IndicatorWindows[j]);
      end;
    end;
  finally
    aStrings.Free;
  end;
end;

procedure TfrmStockChart.acChangeIndicatorWindowUpdate(Sender: TObject);
begin
 (sender as TAction).Enabled:=(GetSelectedIndicator<>nil);
end;

procedure TfrmStockChart.acChartFitAllExecute(Sender: TCustomAction);
begin
  FChart.Apppearance.FitAllVertically:=not FChart.Apppearance.FitAllVertically;
end;

procedure TfrmStockChart.acChartFitAllUpdate(Sender: TCustomAction);
begin
  Sender.Enabled:=not GetFixedVerticalScale;
  Sender.Checked:=FChart.Apppearance.FitAllVertically;
end;

procedure TfrmStockChart.acChartFixVerticalScaleExecute(Sender: TCustomAction);
begin
  SetFixedVerticalScale(not GetFixedVerticalScale);
end;

procedure TfrmStockChart.acChartFixVerticalScaleUpdate(Sender: TCustomAction);
begin
  Sender.Checked:=FChart.MainIndicatorWindow.YScale.Fixed;
end;

procedure TfrmStockChart.acChartOptionsExecute(Sender: TObject);
var
  aDialog: TfmPropertiesDialog;
  aPropList: TPropertyList;
  I: Integer;
begin
  aPropList:=TPropertyList.Create;
  try
    aPropList.Add(FPropGridX);
    aPropList.Add(FPropGridY);
    for I := 0 to aPropList.Count - 1 do
      aPropList[i].EventHandler:=self;

    //GetProperties(aPropList);
    aDialog:=TfmPropertiesDialog.Create(aPropList,'Chart');
    try
      aDialog.ShowModal;
    finally
      aDialog.Free;
    end;
  finally
    for I := 0 to aPropList.Count - 1 do
      aPropList[i].EventHandler:=self;

    aPropList.Free;
  end;
end;

procedure TfrmStockChart.OnInsertIndicatorExecute(aAction: TCustomAction);
var
  aSelected : ISCIndicatorInfo;
begin
  aSelected:=nil;
  if TfmCreateIndicatorDialog.RunSingleSelection(IndicatorFactory.GetIndicators,ikIndicator, aSelected) then
  begin
    CreateIndicator(aSelected);
  end;
end;

procedure TfrmStockChart.OnInsertShapeExecute(aAction: TCustomAction);
var
  aSelected : ISCIndicatorInfo;
begin
  aSelected:=nil;
  if TfmCreateIndicatorDialog.RunSingleSelection(IndicatorFactory.GetIndicators,ikShape, aSelected) then
  begin
    Forms.Application.ProcessMessages; //Нужно пропустить все Mouse Events

    if SelectedWindow<>nil then
      CreateShape(aSelected,SelectedWindow);
  end;
end;

procedure TfrmStockChart.OnChangeOrderStopLoss(const aOrder: IStockOrder);
var
  i: integer;
  aTraderLine: ISCIndicatorTradeLine;
  aOrderLine : ISCIndicatorTradeLineItem;
begin
  aTraderLine:=TradeLineByTraderID(aOrder.GetTrader.GetID);

  if aTraderLine=nil then //такое может быть, однако пока ордер не открыт, ничего не делаем
    exit;

  i:=aTraderLine.IndexOfItem(aOrder.GetID);
  if (i<>-1) then
  begin
    aOrderLine:=aTraderLine.GetItem(i);

    aOrderLine.ChangeStopLoss(
      TStockDataUtils.AlignTimeToLeft(aOrder.GetStopLossSetTime,StockSymbol.TimeInterval),
      aOrder.GetStopLoss);
  end;
end;

procedure TfrmStockChart.OnChangeOrderTakeProfit(const aOrder: IStockOrder);
var
  i: integer;
  aTraderLine: ISCIndicatorTradeLine;
  aOrderLine : ISCIndicatorTradeLineItem;
begin
  aTraderLine:=TradeLineByTraderID(aOrder.GetTrader.GetID);

  if aTraderLine=nil then //такое может быть, однако пока ордер не открыт, ничего не делаем
    exit;

  i:=aTraderLine.IndexOfItem(aOrder.GetID);
  if (i<>-1) then
  begin
    aOrderLine:=aTraderLine.GetItem(i);

    aOrderLine.ChangeTakeProfit(
      TStockDataUtils.AlignTimeToLeft(aOrder.GetTakeProfitSetTime,StockSymbol.TimeInterval),
      aOrder.GetTakeProfit);
  end;
end;

procedure TfrmStockChart.OnInsertExpertExecute(aAction: TCustomAction);
var
  aSelected : ISCIndicatorInfo;
begin
  aSelected:=nil;
  if TfmCreateIndicatorDialog.RunSingleSelection(IndicatorFactory.GetIndicators,ikExpert, aSelected) then
  begin
    CreateExpert(aSelected);
  end;
end;

procedure TfrmStockChart.OnLoadThemeExecute(aAction: TCustomAction);
var
  aStream: TFileStream;
begin
  odTheme.InitialDir:=Workspace.Storage(self).ReadString('Interface\Themes','InitialDir','');
  if odTheme.Execute(Handle) then
  begin
    Workspace.Storage(self).WriteString('Interface\Themes', 'InitialDir', odTheme.InitialDir);
    aStream:=TFileStream.Create(odTheme.FileName,fmOpenRead);
    try
      try
        //frmStockChart.LoadData(aStream);
      except
        on E:Exception do
        begin
          //RecreateStockChart;
          raise EBase.CreateFmt(SCannotLoadTheme_s,[odTheme.FileName],E);
        end;
      end;
    finally
      aStream.Free;
    end;
  end;
end;


procedure TfrmStockChart.OnSaveThemeExecute(aAction: TCustomAction);
var
  aStream: TFileStream;
begin
  sdTheme.InitialDir:=Workspace.Storage(self).ReadString('Interface\Themes','InitialDir','');
  if sdTheme.Execute(Handle) then
  begin
    Workspace.Storage(self).WriteString('Interface\Themes', 'InitialDir', sdTheme.InitialDir);
    aStream:=TFileStream.Create(sdTheme.FileName,fmCreate);
    try
      try
        //frmStockChart.SaveData(aStream);
      except
        on E:Exception do
          raise EBase.CreateFmt(SCannotSaveTheme_s,[sdTheme.FileName],E);
      end;
    finally
      aStream.Free;
    end;
  end;
end;


{ TCaptionMenuItem }

procedure TCaptionMenuItem.CalcBounds;
begin
  inherited CalcBounds;
  Height:=Height+4;
end;

constructor TCaptionMenuItem.Create(aOwner: TComponent);
begin
  inherited;
  Font.Style:=Font.Style + [fsBold];
end;

procedure TCaptionMenuItem.DrawBackground(var PaintRect: TRect);
begin
  FillGradientRect(ActionBar.ColorMap.Color,Menu.ColorMap.MenuColor,PaintRect,Canvas,Horizontal);

  Canvas.Pen.Color:=ActionBar.ColorMap.Color;
  Canvas.MoveTo(PaintRect.Left,PaintRect.Bottom-1);
  Canvas.LineTo(PaintRect.Right,PaintRect.Bottom-1);
end;

procedure TCaptionMenuItem.DrawShadowedText(Rect: TRect; Flags: Cardinal; Text: String; TextColor, ShadowColor: TColor);
begin
  DrawText(Rect,Flags,Text);
end;

procedure TCaptionMenuItem.DrawText(var Rect: TRect; var Flags: Cardinal; Text: String);
begin
  inc(Rect.Bottom,4); //Не знаю почему, но приходит сюда исходный Rect (СМ. CalcBounds)
  inc(Rect.Top,1); //????

  Canvas.Font.Color:=ActionBar.ColorMap.FontColor;
  Canvas.TextRect(Rect,FNoPrefix,[tfVerticalCenter,tfCenter]);
end;





initialization
  Factory.RegisterCreator(TObjectCreator_Unique.Create(IStockChart,TStockChartContainer));
  Serialization.TClassFactory.RegisterClass(TStockChartAttribute);


end.




