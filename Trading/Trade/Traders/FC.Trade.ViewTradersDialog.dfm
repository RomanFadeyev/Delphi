inherited fmViewTradersDialog: TfmViewTradersDialog
  Caption = 'Traders'
  ExplicitWidth = 496
  ExplicitHeight = 269
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [1]
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Traders'
  end
  inherited buOK: TExtendButton
    OnDialogKey = buOKDialogKey
  end
  object lvTraders: TExtendListView
    Left = 8
    Top = 24
    Width = 393
    Height = 169
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end>
    HideSelection = False
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvTradersDblClick
    OnEdited = lvTradersEdited
    
  end
  object buProperties: TButton
    Left = 408
    Top = 56
    Width = 75
    Height = 25
    Action = acTraderProperties
    TabOrder = 2
  end
  object Button1: TButton
    Left = 407
    Top = 88
    Width = 75
    Height = 25
    Action = acDeleteTrader
    TabOrder = 3
  end
  object Button2: TButton
    Left = 407
    Top = 25
    Width = 75
    Height = 25
    Action = accAddTrader
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 296
    Top = 144
    object acTraderProperties: TAction
      Caption = 'Properties'
      OnExecute = acTraderPropertiesExecute
      OnUpdate = acTraderPropertiesUpdate
    end
    object acDeleteTrader: TAction
      Caption = 'Delete'
      OnExecute = acDeleteTraderExecute
      OnUpdate = acDeleteTraderUpdate
    end
    object accAddTrader: TAction
      Caption = 'Add'
      OnExecute = accAddTraderExecute
    end
  end
end
