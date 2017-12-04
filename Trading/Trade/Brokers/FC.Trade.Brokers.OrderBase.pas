unit FC.Trade.Brokers.OrderBase;
{$I Compiler.inc}

interface
uses
  SysUtils,BaseUtils,Graphics,
  Serialization, FC.Definitions,FC.Trade.Brokers.BrokerBase,
  StockChart.Definitions;

type
  TStockOrderBase = class (TInterfacedObject,IStockOrder)
  protected
    FAttributes      : IStockAttributeCollection;
    FTrader : IStockTrader;
    FBrokerCallBack   : IStockBroker;
    FCreateTime : TDateTime;
    FOpenTime : TDateTime;
    FOpenComment: string;
    FOpenPrice: TStockRealNumber;
    FQueriedOpenPrice: TStockRealNumber;
    FPendingType: TStockOrderPendingType;
    FPendingOpenTime: TDateTime;
    FPendingExpirationTime: TDateTime;
    FSymbol: string;
    FNotes: string;
    FColor: TColor;

    FCloseTime: TDateTime;
    FCloseComment: string;
    FClosePrice: TStockRealNumber;

    FKind     : TStockOrderKind;
    FID        : TGUID;
    FState     : TStockOrderState;
    FLots      : TStockOrderLots;

    FTakeProfit: TStockRealNumber;
    FTakeProfitSetTime: TDateTime;
    FStopLoss  : TStockRealNumber;
    FStopLossSetTime: TDateTime;
    FTrailingStop: TStockRealNumber;
    FTrailingStopSetTime: TStockRealNumber;

    FPendingSuspended: boolean;

    procedure InitEventArgs(out aArgs: TStockOrderModifyEventArgs; aModifyType: TStockOrderModifyType);
  protected
    FWorstProfit   : TStockRealNumber;
    FBestProfit    : TStockRealNumber;
    FWorstPrice    : TStockRealNumber;
    FBestPrice     : TStockRealNumber;
    FCurrentProfit : TStockRealNumber;

    //������� ����� �� ������� ����
    procedure OpenInternal(const aSymbol: string; aKind: TStockOrderKind; aLots: TStockOrderLots;const aComment: string);
    procedure SetStopLossInternal(aPrice: TStockRealNumber; aFromTrailingStop: boolean); virtual;

    procedure OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs); virtual;
  public
    //���������� ������������� ������
    function  GetID: TGUID; virtual;

    //��� ����� �������� ����� �����
    function GetBroker: IStockBroker;

    //����� ������� ������ �����. C����� ��������� ������, ��������� ID
    function GetTrader: IStockTrader;

    //�������������  �������� ������
    function GetNotes: string;
    procedure SetNotes(const aNotes: string);

    function GetColor: TColor;
    procedure SetColor(const aColor: TColor);

    //��� ������ - �� ������� ��� �������
    function GetKind : TStockOrderKind; virtual;

    //�������� ����. ����� ����� �������� ������ ���� GetState<>osNothing
    function GetSymbol: string; virtual;

    //������� ��������� - ������, ������, ������
    function GetState: TStockOrderState; virtual;

    //���-�� �����.
    function GetLots: TStockOrderLots; virtual;

    //����� �������� ������
    function GetCreateTime: TDateTime;

    //�������� ��������
    function GetOpenTime : TDateTime; virtual;
    function GetOpenPrice: TStockRealNumber; virtual;
    function GetOpenComment: string; virtual;

    //�������� ��������
    function GetCloseTime : TDateTime; virtual;
    function GetClosePrice: TStockRealNumber; virtual;
    function GetCloseComment: string; virtual;

    //������ ��������� �� ��� ����� ������
    function GetWorstProfit: TStockRealNumber; virtual;

    //������ ���� �� ��� ����� ������
    function GetWorstPrice: TStockRealNumber; virtual;

    //������������ ������, ������� ��� �� ��� ����� ������
    function GetBestProfit: TStockRealNumber; virtual;

    //������ ���� �� ��� ����� ������
    function GetBestPrice: TStockRealNumber; virtual;

    //���� ������� ������
    function GetCurrentProfit: TStockRealNumber; virtual;

    //���� ��� ������� ��������� �������, ��� ��������� Trailing Stop
    //���� Trailing Stop �� ���������, ������������ 0
    function  GetCurrentTrailingStopTriggerPrice: TStockRealNumber;

    //���������� ������� StopLoss (� �������� ������� ��������)
    procedure SetTrailingStop(aPriceDelta: TStockRealNumber); virtual;
    function  GetTrailingStop:TStockRealNumber; virtual;
    //����� ���������� ��������� StopLoss
    function  GetTrailingStopSetTime: TDateTime;

    //���������� ������� StopLoss (� �������� ������� ��������)
    procedure SetStopLoss(aPrice: TStockRealNumber); virtual;
    function  GetStopLoss:TStockRealNumber; virtual;
    //����� ���������� ��������� StopLoss
    function  GetStopLossSetTime: TDateTime;

    //���������� ������� TakeProfit (� �������)
    procedure SetTakeProfit(aPrice: TStockRealNumber); virtual;
    function  GetTakeProfit:TStockRealNumber; virtual;
    //����� ���������� ��������� StopLoss
    function  GetTakeProfitSetTime: TDateTime;

    //������� �� ������� ����
    procedure Close(const aComment: string); virtual;

    //������� ����� �� ������� ����
    procedure Open(const aSymbol: string; aKind: TStockOrderKind; aLots: TStockOrderLots;const aComment: string); virtual;

    //������� ����� �� ��������� ����
    procedure OpenAt(const aSymbol: string; aKind: TStockOrderKind; aPrice: TStockRealNumber; aLots:TStockOrderLots; const aComment: string); overload; virtual;
    procedure OpenAt(const aSymbol: string; aKind: TStockOrderKind; aPrice: TStockRealNumber; aLots:TStockOrderLots; aStopLoss, aTakeProfit,aTrailingStop: TSCRealNumber; const aComment: string=''); overload;

    //�������� ����� �������� (���������) ����������� ������ (��. OpenAt)
    //��� ������� ����� ����� ������, ���� ����� ���������� ����� OpenAt. � ��������� ������ ����� ��������� ����������
    function GetPendingOpenTime: TStockRealNumber; virtual;

    //�������� ������������� ���� �������� (��. OpenAt)
    //��� ������� ����� ����� ������, ���� ����� ���������� ����� OpenAt. � ��������� ������ ����� ��������� ����������
    function GetPendingOpenPrice: TStockRealNumber; virtual;

    //�������� ���� ����������� ��������
    //��� ������� ����� ����� ������������ ������ ���� ����� ���������� ����� OpenAt.
    //� ��������� ������ ����� ��������� ����������
    procedure SetPendingOpenPrice(const aPrice: TStockRealNumber); virtual;

    //��� ����������� ������ ���������� ���� ���������
    procedure SetPendingExpirationTime(const aDateTime: TDateTime);

    //�������� ��� ����������� ������ (�������� ��� ��������)
    //��� ������� ����� ����� ������, ���� ����� ���������� ����� OpenAt. � ��������� ������ ����� ��������� ����������
    function GetPendingType: TStockOrderPendingType; virtual;

    //���� ��������� ����������� ������
    function GetPendingExpirationTime: TDateTime;

    //�������� ���������� �����. ����� �������� ������ ���� GetState=osPending
    procedure RevokePending;

    //������������� ������ ����������� ������. ����� �������� � ��������� ��������, ��
    //"������������" ����� �������� ������ ���� GetState=osPending.
    //��������! Suspend ������������� ������������ ��� ������ OpenAt
    procedure SuspendPending;
    //"�������������" ���������� �����
    procedure ResumePending;
    //
    function  IsPendingSuspended: boolean;

    //��� ���� ���������� ����������. � ������ ����� ��������� ����� ������� ������
    function  GetAttributes:IStockAttributeCollection;

    constructor Create(const aStockBroker: IStockBroker; const aStockTrader: IStockTrader); overload;

    destructor Destroy; override;
    procedure  Dispose; virtual;
  end;


implementation
  uses Math,StockChart.Obj;

{ TStockOrderBase }

constructor TStockOrderBase.Create(const aStockBroker: IStockBroker; const aStockTrader: IStockTrader);
begin
  inherited Create;
  FTrader:=aStockTrader;

  FBrokerCallBack:=aStockBroker;
  CreateGUID(FID);
  FAttributes:=TSCAttributeCollection.Create;
  FColor:=clWindow;
  FCreateTime:=FBrokerCallBack.GetCurrentTime;
end;

destructor TStockOrderBase.Destroy;
begin
  FAttributes:=nil;
  inherited;
end;

procedure TStockOrderBase.Dispose;
begin
  FBrokerCallBack:=nil;
end;

function TStockOrderBase.GetOpenTime: TDateTime;
begin
  result:=FOpenTime;
end;

function TStockOrderBase.GetPendingExpirationTime: TDateTime;
begin
  result:=FPendingExpirationTime;
end;

function TStockOrderBase.GetPendingOpenPrice: TStockRealNumber;
begin
  //���� ����������� �� ����� OpenAt
  if FQueriedOpenPrice=0 then
    raise EStockOrderErrorBadQuery.Create('Order is not pending');

  result:=FQueriedOpenPrice;
end;

function TStockOrderBase.GetPendingOpenTime: TStockRealNumber;
begin
  //���� ����������� �� ����� OpenAt
  if FQueriedOpenPrice=0 then
    raise EStockOrderErrorBadQuery.Create('Order is not pending');

  result:=FPendingOpenTime;
end;

function TStockOrderBase.GetPendingType: TStockOrderPendingType;
begin
  //���� ����������� �� ����� OpenAt
  if FQueriedOpenPrice=0 then
    raise EStockOrderErrorBadQuery.Create('Order is not pending');

  result:=FPendingType;
end;

function TStockOrderBase.GetKind: TStockOrderKind;
begin
  result:=FKind;
end;

function TStockOrderBase.GetNotes: string;
begin
  result:=FNotes;
end;

function TStockOrderBase.GetTakeProfit: TStockRealNumber;
begin
  result:=FTakeProfit;
end;

function TStockOrderBase.GetTakeProfitSetTime: TDateTime;
begin
  result:=FTakeProfitSetTime;
end;

function TStockOrderBase.GetTrader: IStockTrader;
begin
  result:=FTrader;
end;

function TStockOrderBase.GetTrailingStop: TStockRealNumber;
begin
  result:=FTrailingStop;
end;

function TStockOrderBase.GetTrailingStopSetTime: TDateTime;
begin
  result:=FTrailingStopSetTime;
end;

procedure TStockOrderBase.SetColor(const aColor: TColor);
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if aColor<>FColor then
  begin
    FColor:=aColor;
    InitEventArgs(aEventArgs,omtChangeColor);
    OnModifyOrder(aEventArgs);
  end;
end;

procedure TStockOrderBase.SetNotes(const aNotes: string);
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if aNotes<>FNotes then
  begin
    FNotes:=aNotes;
    InitEventArgs(aEventArgs,omtChangeNotes);
    OnModifyOrder(aEventArgs);
  end;
end;

procedure TStockOrderBase.SetPendingExpirationTime(const aDateTime: TDateTime);
begin
  if FPendingOpenTime>=aDateTime then
    raise EStockOrderError.Create('Expiration time must be grater than open time');

  FPendingExpirationTime:=aDateTime;
end;

procedure TStockOrderBase.SetPendingOpenPrice(const aPrice: TStockRealNumber);
begin
  if FQueriedOpenPrice=aPrice then
    exit;
  OpenAt(FSymbol,FKind,aPrice,FLots,FOpenComment);
end;

procedure TStockOrderBase.SetStopLoss(aPrice: TStockRealNumber);
begin
  SetStopLossInternal(aPrice,false);
end;

procedure TStockOrderBase.SetStopLossInternal(aPrice: TStockRealNumber; aFromTrailingStop: boolean);
var
  aMarketPrice: TStockRealNumber;
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if FState in [osClosed,osNothing] then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if aPrice<0 then
    raise EStockOrderErrorInvalidPrice.Create('Stop Loss must be greater or equal to 0');

  aPrice:=FBrokerCallBack.RoundPrice(GetSymbol,aPrice);

  if FStopLoss=aPrice then
    exit;

  //���� ���-�� ���������������
  if aPrice<>0 then
  begin
    if FState=osPending then
      aMarketPrice:=FQueriedOpenPrice
    else begin
      if GetKind= okBuy then
        aMarketPrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkBid)
      else
        aMarketPrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkAsk);
    end;

    //��������, ����� SL �� ��� ������� ������ � ������-����
    if GetBroker.PriceToPoint(GetSymbol,Abs(aPrice-aMarketPrice))<GetBroker.GetMarketInfo(GetSymbol).StopLevel then
      raise EStockOrderErrorInvalidPrice.Create('Stop Loss is too close to the market price');

    //��������, ����� Stop Loss ���������� � ���������� �������
    if (GetKind=okBuy) then
      if (GetState=osOpened) and (aPrice>aMarketPrice) then
        raise EStockOrderErrorInvalidPrice.Create('Stop Loss is greater than the market price')
      else if (GetState=osPending) and (aPrice>GetPendingOpenPrice) then
        raise EStockOrderErrorInvalidPrice.Create('Stop Loss is greater than the queried open price');

    //��������, ����� Stop Loss ���������� � ���������� �������
    if (GetKind=okSell) then
      if  (GetState=osOpened) and (aPrice<aMarketPrice) then
        raise EStockOrderErrorInvalidPrice.Create('Stop Loss is less than the market price')
      else if (GetState=osPending) and (aPrice<GetPendingOpenPrice) then
        raise EStockOrderErrorInvalidPrice.Create('Stop Loss is less than the queried open price');

  end;

  FStopLoss:=aPrice;
  FStopLossSetTime:=FBrokerCallBack.GetCurrentTime;
  InitEventArgs(aEventArgs,omtChangeStopLoss);
  aEventArgs.StopLossFromTrailingStop:=aFromTrailingStop;
  OnModifyOrder(aEventArgs);
end;

procedure TStockOrderBase.SetTakeProfit(aPrice: TStockRealNumber);
var
  aMarketPrice: TStockRealNumber;
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if FState in [osClosed,osNothing] then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if aPrice<0 then
    raise EStockOrderErrorInvalidPrice.Create('Take Profit must be greater or equal to 0');

  if FTakeProfit=aPrice then
    exit;

  aPrice:=FBrokerCallBack.RoundPrice(GetSymbol,aPrice);
  //���� ������������� ����� ���������� TakeProfit (� �� �������� �������
  if aPrice<>0 then
  begin
    if FState=osPending then
      aMarketPrice:=FQueriedOpenPrice
    else begin
      if GetKind= okBuy then
        aMarketPrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkBid)
      else
        aMarketPrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkAsk);
    end;

    //��������, ����� SL �� ��� ������� ������ � ������-����
    if GetBroker.PriceToPoint(GetSymbol,Abs(aPrice-aMarketPrice))<GetBroker.GetMarketInfo(GetSymbol).StopLevel then
      raise EStockOrderErrorInvalidPrice.Create('Take Profit is too close to the market price');

    //��������, ����� Take Profit ���������� � ���������� �������
    if (GetKind=okBuy) and (aPrice<aMarketPrice) then
      raise EStockOrderErrorInvalidPrice.Create('Take Profit is less than the market price');

    //��������, ����� Take Profit ���������� � ���������� �������
    if (GetKind=okSell) and (aPrice>aMarketPrice) then
      raise EStockOrderErrorInvalidPrice.Create('Take Profit is greater than the market price');
  end;

  if FTakeProfit<>aPrice then
  begin
    FTakeProfit:=aPrice;
    FTakeProfitSetTime:=FBrokerCallBack.GetCurrentTime;
    InitEventArgs(aEventArgs,omtChangeTakeProfit);
    OnModifyOrder(aEventArgs);
  end;
end;

procedure TStockOrderBase.SetTrailingStop(aPriceDelta: TStockRealNumber);
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if FState in [osClosed,osNothing] then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if aPriceDelta<0 then
    raise EStockOrderErrorInvalidPrice.Create('Trailing Stop must be greater or equal to 0');

  if aPriceDelta=FTrailingStop then
    exit;

  if aPriceDelta<>0 then
  begin
    if GetBroker.PriceToPoint(GetSymbol,aPriceDelta)<GetBroker.GetMarketInfo(GetSymbol).StopLevel then
      raise EStockOrderErrorInvalidPrice.Create('Trailing Stop is too small');
  end;

  FTrailingStop:=aPriceDelta;
  FTrailingStopSetTime:=FBrokerCallBack.GetCurrentTime;
  InitEventArgs(aEventArgs,omtChangeTrailingStop);
  OnModifyOrder(aEventArgs);
end;

procedure TStockOrderBase.SuspendPending;
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if (FState <>osPending) then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if FPendingSuspended then
    exit;

  FPendingSuspended:=true;

  InitEventArgs(aEventArgs,omtPendingSuspend);
  OnModifyOrder(aEventArgs);
end;

function TStockOrderBase.GetOpenComment: string;
begin
  result:=FOpenComment;
end;

function TStockOrderBase.GetOpenPrice: TStockRealNumber;
begin
  result:=FOpenPrice;
end;

function TStockOrderBase.GetCloseTime: TDateTime;
begin
  result:=FCloseTime;
end;

function TStockOrderBase.GetColor: TColor;
begin
  result:=FColor;
end;

function TStockOrderBase.GetCreateTime: TDateTime;
begin
  result:=FCreateTime;
end;

function TStockOrderBase.GetClosePrice: TStockRealNumber;
begin
  result:=FClosePrice;
end;

procedure TStockOrderBase.Close(const aComment: string);
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if FState<>osOpened then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  //���� ��������!
  if FKind=okBuy then
    FClosePrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkBid)
  else
    FClosePrice:=FBrokerCallBack.GetCurrentPrice(FSymbol,bpkAsk);

  FCloseTime:=FBrokerCallBack.GetCurrentTime;
  FCloseComment:=aComment;
  FState:=osClosed;

  Assert(FBrokerCallBack<>nil);
  InitEventArgs(aEventArgs,omtClose);
  OnModifyOrder(aEventArgs);
end;

procedure TStockOrderBase.OnModifyOrder(const aModifyEventArgs: TStockOrderModifyEventArgs);
begin

end;

procedure TStockOrderBase.Open(const aSymbol: string; aKind: TStockOrderKind; aLots:TStockOrderLots;const aComment: string);
begin
  //������ ��������� ����� � ������� ��� ���-�� ���������
  if not (FState in [osNothing,osPending]) then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if FState=osPending then
    RevokePending;
    
  OpenInternal(aSymbol, aKind,aLots,aComment);
end;

procedure TStockOrderBase.OpenAt(const aSymbol: string; aKind: TStockOrderKind;
  aPrice: TStockRealNumber; aLots:TStockOrderLots; aStopLoss, aTakeProfit, aTrailingStop: TSCRealNumber;
  const aComment: string);
begin
  OpenAt(aSymbol,aKind,aPrice,aLots,aComment);
  SetStopLoss(aStopLoss);
  SetTakeProfit(aTakeProfit);
  SetTrailingStop(aTrailingStop);  
end;

procedure TStockOrderBase.OpenAt(const aSymbol: string; aKind: TStockOrderKind; aPrice: TStockRealNumber; aLots:TStockOrderLots; const aComment: string);
var
  aCurrentPrice: TStockRealNumber;
  aPendingType: TStockOrderPendingType;
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if not (FState in [osNothing,osPending]) then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if aPrice<=0 then
    raise EStockOrderErrorInvalidPrice.Create('Price must be greater than 0');

  case aKind of
    //�������
    okBuy: begin
      aCurrentPrice:=FBrokerCallBack.GetCurrentPrice(aSymbol,bpkAsk);

      //�������� ����� �� �������
      if (aPrice>aCurrentPrice) then
        aPendingType:=ptStop
      //�������� ����� �� �������
      else
        aPendingType:=ptLimit;
    end;
    //�������
    okSell:begin
      aCurrentPrice:=FBrokerCallBack.GetCurrentPrice(aSymbol,bpkBid);
      //�������� ����� �� �������
      if (aPrice<aCurrentPrice)  then
       aPendingType:=ptStop
      //�������� ����� �� �������
      else
       aPendingType:=ptLimit;
    end
    //???
    else
     raise EAlgoError.Create;
  end;

  //��������, ����� ���� ������� �� ���� ������� ������ � ������-����
  if GetBroker.PriceToPoint(GetSymbol,Abs(aPrice-aCurrentPrice))<GetBroker.GetMarketInfo(GetSymbol).StopLevel then
    raise EStockOrderErrorInvalidPrice.Create('Desired price is too close to the market price');

  if (FState=osPending) and IsPendingSuspended then
    ResumePending; //���� ����� ��� ���������, �� ��� ������� ��������

  FKind:=aKind;
  FLots:=aLots;
  FState:=osPending;
  FPendingType:=aPendingType;
  FSymbol:=aSymbol;
  FOpenComment:=aComment;
  FPendingOpenTime:=FBrokerCallBack.GetCurrentTime;

  if FQueriedOpenPrice<>aPrice then
  begin
    FQueriedOpenPrice:=aPrice;
    InitEventArgs(aEventArgs,omtChangeOpenPrice);
    OnModifyOrder(aEventArgs);
  end;
end;

procedure TStockOrderBase.OpenInternal(const aSymbol: string; aKind: TStockOrderKind; aLots:TStockOrderLots; const aComment: string);
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  //������ ��������� ����� � ������� ��� ���-�� ���������
  if FState in [osOpened, osClosed] then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if aKind=okBuy then
    FOpenPrice :=FBrokerCallBack.GetCurrentPrice(aSymbol,bpkAsk)
  else
    FOpenPrice:=FBrokerCallBack.GetCurrentPrice(aSymbol,bpkBid);

  FSymbol:=aSymbol;
  FKind:=aKind;
  FLots:=aLots;
  FState:=osOpened;
  FOpenTime:=FBrokerCallBack.GetCurrentTime;
  FOpenComment:=aComment;
  FWorstPrice :=FOpenPrice;
  FBestPrice := FOpenPrice;

  Assert(FBrokerCallBack<>nil);
  InitEventArgs(aEventArgs,omtOpen);
  OnModifyOrder(aEventArgs);
end;

procedure TStockOrderBase.ResumePending;
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if (FState <>osPending) then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  if not FPendingSuspended then
    exit;

  FPendingSuspended:=false;

  InitEventArgs(aEventArgs,omtPendingSuspend);
  OnModifyOrder(aEventArgs);
end;

procedure TStockOrderBase.RevokePending;
var
  aEventArgs: TStockOrderModifyEventArgs;
begin
  if (FState <>osPending) then
    raise EStockOrderErrorBadQuery.Create('Invalid state');

  FState:=osNothing;
  InitEventArgs(aEventArgs,omtPendingRevoke);
  OnModifyOrder(aEventArgs);
end;

function TStockOrderBase.GetID: TGUID;
begin
  result:=FID;
end;

function TStockOrderBase.GetWorstPrice: TStockRealNumber;
begin
  result:=FWorstPrice;
end;

function TStockOrderBase.GetWorstProfit: TStockRealNumber;
begin
  result:=FWorstProfit;
end;

procedure TStockOrderBase.InitEventArgs(out aArgs: TStockOrderModifyEventArgs; aModifyType: TStockOrderModifyType);
begin
  FillChar(aArgs,sizeof(aArgs),0);
  aArgs.ModifyType:=aModifyType;
end;

function TStockOrderBase.IsPendingSuspended: boolean;
begin
  result:=FPendingSuspended;
end;

function TStockOrderBase.GetAttributes: IStockAttributeCollection;
begin
  result:=FAttributes;
end;

function TStockOrderBase.GetBestPrice: TStockRealNumber;
begin
  result:=FBestPrice;
end;

function TStockOrderBase.GetBestProfit: TStockRealNumber;
begin
  result:=FBestProfit;
end;

function TStockOrderBase.GetBroker: IStockBroker;
begin
  result:=FBrokerCallBack as IStockBroker;
end;

function TStockOrderBase.GetState: TStockOrderState;
begin
  result:=FState;
end;

function TStockOrderBase.GetStopLoss: TStockRealNumber;
begin
  result:=FStopLoss;
end;

function TStockOrderBase.GetStopLossSetTime: TDateTime;
begin
  result:=FStopLossSetTime;
end;

function TStockOrderBase.GetSymbol: string;
begin
  result:=FSymbol;
end;

function TStockOrderBase.GetCurrentProfit: TStockRealNumber;
begin
  result:=FCurrentProfit;
end;

function TStockOrderBase.GetCurrentTrailingStopTriggerPrice: TStockRealNumber;
begin
  if GetTrailingStop=0 then
    result:=0
  else
    result:=GetBestPrice-GetTrailingStop;
end;

function TStockOrderBase.GetCloseComment: string;
begin
  result:=FCloseComment;
end;

function TStockOrderBase.GetLots: TStockOrderLots;
begin
  result:=FLots;
end;

end.
