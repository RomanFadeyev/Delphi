inherited frmTerminalFrame: TfrmTerminalFrame
  ParentFont = False
  inherited pcPages: TJvPageControl
    inherited tsOrderHistory: TTabSheet
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    inherited tsEquity: TTabSheet
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  inherited acActions: TActionList
    object acModifyOrder: TAction [6]
      Category = 'Terminal'
      Caption = '&Modify Order'
      OnExecute = acModifyOrderExecute
      OnUpdate = acModifyOrderUpdate
    end
    object acCloseOrder: TAction
      Category = 'Terminal'
      Caption = '&Close Order'
      OnUpdate = acCloseOrderUpdate
    end
  end
  inherited taOrders: TMemoryDataSet
    inherited taOrdersStopLoss: TCurrencyField
      DisplayLabel = 'Stop Loss'
    end
    inherited taOrdersTakeProfit: TCurrencyField
      DisplayLabel = 'Take Profit'
    end
    inherited taOrdersTrailingStop: TCurrencyField
      DisplayLabel = 'Trailing Stop'
    end
    inherited taOrdersPendingOpenPrice: TCurrencyField
      DisplayLabel = 'Pending Price'
      DisplayFormat = '0.0000'
    end
  end
  inherited pmGrid: TPopupActionBar
    object miModifyOrder: TMenuItem [0]
      Action = acModifyOrder
    end
    object CloseOrder1: TMenuItem [1]
      Action = acCloseOrder
    end
    object N2: TMenuItem [2]
      Caption = '-'
    end
  end
end
