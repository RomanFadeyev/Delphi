inherited fmSelectBrokerConnectionDialog: TfmSelectBrokerConnectionDialog
  ActiveControl = lvBrokerConnections
  Caption = 'Broker Connection'
  ClientWidth = 570
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 576
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Width = 570
    ExplicitWidth = 570
  end
  inherited buOK: TExtendButton
    Left = 409
    TabOrder = 2
    ExplicitLeft = 409
  end
  inherited buCancel: TExtendButton
    Left = 489
    TabOrder = 3
    ExplicitLeft = 489
  end
  object paWorkSpace: TPanel [3]
    Left = 0
    Top = 0
    Width = 470
    Height = 206
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 462
      Height = 13
      Align = alTop
      Caption = 'Select Active Broker Connection'
      ExplicitWidth = 154
    end
    object lvBrokerConnections: TExtendListView
      Left = 4
      Top = 17
      Width = 462
      Height = 168
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 151
        end
        item
          Caption = 'Description'
          Width = 300
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = lvBrokerConnectionsDblClick
    end
    object ckConnectAtStartup: TExtendCheckBox
      Left = 4
      Top = 185
      Width = 462
      Height = 17
      Align = alBottom
      Caption = 'Connect automatically at startup'
      TabOrder = 1
      FlatFontColor = clBlack
      
    end
  end
  object Panel1: TPanel [4]
    Left = 470
    Top = 0
    Width = 100
    Height = 206
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Properties...'
      TabOrder = 0
    end
  end
  inherited acDialog: TActionList
    Left = 72
    Top = 56
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
end
