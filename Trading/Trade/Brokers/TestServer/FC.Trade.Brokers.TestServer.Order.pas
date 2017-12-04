{-----------------------------------------------------------------------------
 Author:    Roman Fadeyev
 Purpose:   ���������� ������ ��� TestServer �� ������
            "X:\Trade\Forecaster\Stock Server"

            ��������
 History:
-----------------------------------------------------------------------------}
unit FC.Trade.Brokers.TestServer.Order;
{$I Compiler.inc}

interface
uses
  SysUtils,BaseUtils, StockChart,StockServer_TLB,
  Serialization, FC.Definitions,StockChart.Definitions;

type

  //����������� ���������� ��� ��������� ���
  IStockBrokerTestServerSupport = interface
  ['{CAFBB2D1-1E2C-499E-A585-99EB8FCEF041}']
    function GetNative: IStockServerBroker;
  end;

 { TStockOrder }

  TStockOrder = class (TInterfacedObject,IStockOrder)
  private
    FTraderID : TGUID;
    FBroker   : IStockBroker; //TStockBroker
    FID       : TGUID;
    FNative   : IStockServerOrder;
  public
    //���������� ������������� ������
    function  GetID: TGUID;

    //��� ����� �������� ����� �����
    function GetBroker: IStockBroker;

    //����� ������� ������ �����. C����� ��������� ������, ��������� ID
    function GetTraderID: TGUID;

    //��� ������ - �� ������� ��� �������
    function GetKind : TStockOrderKind;

    //������� ��������� - ������, ������, ������
    function GetState: TStockOrderState;

    //���-�� �����. Rate = 1 �������� 0.1 �����
    function GetRate: integer;

    //�������� ��������
    function GetOpenTime : TDateTime;
    function GetOpenPrice: TStockRealNumber;
    function GetOpenComment: string;

    //�������� ��������
    function GetCloseTime : TDateTime;
    function GetClosePrice: TStockRealNumber;
    function GetCloseComment: string;

    //������ ��������� �� ��� ����� ������
    function GetWorstProfit: TStockRealNumber;

    //������������ ������, ������� ��� �� ��� ����� ������
    function GetBestProfit: TStockRealNumber;

    //���� ������� ������
    function GetCurrentProfit: TStockRealNumber;

    //���������� ������� StopLoss (� �������� ������� ��������)
    procedure SetTrailingStop(aPriceDelta: TStockRealNumber);
    function  GetTrailingStop:TStockRealNumber;

    //���������� ������� StopLoss (� �������)
    procedure SetStopLoss(aPrice: TStockRealNumber);
    function  GetStopLoss:TStockRealNumber;

    //���������� ������� TakeProfit (� �������)
    procedure SetTakeProfit(aPrice: TStockRealNumber);
    function  GetTakeProfit:TStockRealNumber;

    //������� �� ������� ����
    procedure Close(const aComment: string);

    //������� ����� �� ������� ����
    procedure Open(aKind: TStockOrderKind; aRate: integer);

    //������� ����� �� ��������� ����
    //aRate ��������� ��������� pt:$, ��������� 1:1 �������� 0.1 ����
    procedure OpenAt(aKind: TStockOrderKind; aPrice: TStockRealNumber; aRate: integer);

    //�������� ������������� ���� �������� (��. OpenAt)
    //��� ������� ����� ����� ������, ���� ����� ���������� ����� OpenAt. � ��������� ������ ����� ��������� ����������
    function GetPendingOpenPrice: TStockRealNumber;

    //�������� ��� ����������� ������ (�������� ��� ��������)
    //��� ������� ����� ����� ������, ���� ����� ���������� ����� OpenAt. � ��������� ������ ����� ��������� ����������
    function GetPendingType: TStockOrderPendingType;

    //Implementation
    procedure Dispose;

    property  Native:IStockServerOrder read FNative;

    constructor Create(const aStockBroker: IStockBrokerTestServerSupport; const aStockTrader: IStockTrader);
  end;

implementation
  uses Math,FC.Trade.Brokers.TestServer.Broker;


{ TStockOrder }

constructor TStockOrder.Create(const aStockBroker: IStockBrokerTestServerSupport; const aStockTrader: IStockTrader);
begin
  FTraderID:=aStockTrader.GetID;
  FBroker:=aStockBroker as IStockBroker;
  FNative:=aStockBroker.GetNative.CreateOrder;

  CreateGUID(FID);
end;

procedure TStockOrder.Dispose;
begin
  FBroker:=nil;
end;

function TStockOrder.GetOpenTime: TDateTime;
begin
  result:=FNative.GetOpenTime;
end;

function TStockOrder.GetPendingOpenPrice: TStockRealNumber;
begin
  //���� ����������� �� ����� OpenAt
  raise ENotSupported.Create;
  //if FNative.GetQueriedOpenPrice=0 then
  //  raise EStockError.Create('Order is not pending');

  //result:=FNative.GetQueriedOpenPrice;
end;

function TStockOrder.GetPendingType: TStockOrderPendingType;
begin
  raise ENotSupported.Create;
end;

function TStockOrder.GetKind: TStockOrderKind;
begin
  result:=TStockOrderKind(FNative.GetKind);
end;

function TStockOrder.GetTakeProfit: TStockRealNumber;
begin
  result:=FNative.GetTakeProfit;
end;

function TStockOrder.GetTraderID: TGUID;
begin
  result:=FTraderID;
end;

function TStockOrder.GetTrailingStop: TStockRealNumber;
begin
  result:=FNative.GetTrailingStop;
end;

procedure TStockOrder.SetStopLoss(aPrice: TStockRealNumber);
begin
  FNative.SetStopLoss(aPrice);
end;

procedure TStockOrder.SetTakeProfit(aPrice: TStockRealNumber);
begin
  FNative.SetTakeProfit(aPrice);
end;

procedure TStockOrder.SetTrailingStop(aPriceDelta: TStockRealNumber);
begin
  FNative.SetTrailingStop(aPriceDelta);
end;

function TStockOrder.GetOpenComment: string;
begin
  result:=FNative.GetOpenComment;
end;

function TStockOrder.GetOpenPrice: TStockRealNumber;
begin
  result:=FNative.GetOpenPrice;
end;

function TStockOrder.GetCloseTime: TDateTime;
begin
  result:=FNative.GetCloseTime;
end;

function TStockOrder.GetClosePrice: TStockRealNumber;
begin
  result:=FNative.GetClosePrice;
end;

procedure TStockOrder.Close(const aComment: string);
begin
  FNative.Close(aComment);
end;

procedure TStockOrder.Open(aKind: TStockOrderKind; aRate: integer);
begin
  FNative.Open(integer(aKind),aRate);
end;

procedure TStockOrder.OpenAt(aKind: TStockOrderKind; aPrice: TStockRealNumber; aRate: integer);
begin
  FNative.OpenAt(integer(aKind),aPrice,aRate);
end;

function TStockOrder.GetID: TGUID;
begin
  result:=FID;
end;

function TStockOrder.GetBroker: IStockBroker;
begin
  result:=FBroker;
end;

function TStockOrder.GetState: TStockOrderState;
begin
  result:=TStockOrderState(FNative.GetState);
end;

function TStockOrder.GetStopLoss: TStockRealNumber;
begin
  result:=FNative.GetStopLoss;
end;

function TStockOrder.GetCurrentProfit: TStockRealNumber;
begin
  result:=FNative.GetCurrentProfit;
end;

function TStockOrder.GetCloseComment: string;
begin
  result:=FNative.GetCloseComment;
end;

function TStockOrder.GetRate: integer;
begin
  result:=FNative.GetRate;
end;

function TStockOrder.GetWorstProfit: TStockRealNumber;
begin
  result:=-1;
end;

function TStockOrder.GetBestProfit: TStockRealNumber;
begin
  result:=-1;
end;


end.

