inherited fmViewAlertersDialog: TfmViewAlertersDialog
  ActiveControl = lvAlerters
  Caption = 'Alerters'
  ExplicitWidth = 496
  ExplicitHeight = 269
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [1]
    Left = 8
    Top = 8
    Width = 35
    Height = 13
    Caption = 'Alerters'
  end
  inherited buOK: TExtendButton
    TabOrder = 3
    OnDialogKey = buOKDialogKey
  end
  object lvAlerters: TExtendListView
    Left = 8
    Top = 24
    Width = 393
    Height = 169
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end>
    HideSelection = False
    RowSelect = True
    PopupMenu = pmAlerter
    TabOrder = 0
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = lvAlertersAdvancedCustomDrawItem
    OnChange = lvAlertersChange
    OnDblClick = lvAlertersDblClick
    OnEdited = lvAlertersEdited
    
  end
  object buProperties: TButton
    Left = 407
    Top = 56
    Width = 75
    Height = 25
    Action = acAlerterProperties
    TabOrder = 2
  end
  object buAdd: TButton
    Left = 407
    Top = 25
    Width = 75
    Height = 25
    Action = accAddAlerter
    TabOrder = 1
  end
  object Button1: TButton
    Left = 407
    Top = 87
    Width = 75
    Height = 25
    Action = acDeleteAlerter
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 296
    Top = 144
    object acAlerterProperties: TAction
      Caption = 'Properties'
      OnExecute = acAlerterPropertiesExecute
      OnUpdate = acAlerterPropertiesUpdate
    end
    object acDeleteAlerter: TAction
      Caption = 'Delete'
      OnExecute = acDeleteAlerterExecute
      OnUpdate = acDeleteAlerterUpdate
    end
    object accAddAlerter: TAction
      Caption = 'Add'
      OnExecute = accAddAlerterExecute
    end
  end
  object pmAlerter: TPopupActionBar
    Left = 224
    Top = 160
    object Add1: TMenuItem
      Action = accAddAlerter
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Properties1: TMenuItem
      Action = acAlerterProperties
    end
    object Delete1: TMenuItem
      Action = acDeleteAlerter
    end
    object TMenuItem
    end
  end
end
