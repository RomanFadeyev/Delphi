inherited fmCreateAlerterDialog: TfmCreateAlerterDialog
  Caption = 'Select Alerter to Create'
  ClientHeight = 261
  ClientWidth = 445
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 222
    Width = 445
  end
  inherited buOK: TExtendButton
    Left = 284
    Top = 232
  end
  inherited buCancel: TExtendButton
    Left = 364
    Top = 232
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 0
    Width = 445
    Height = 222
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    Caption = 'Panel1'
    TabOrder = 2
    object laHeader: TLabel
      Left = 8
      Top = 8
      Width = 429
      Height = 13
      Align = alTop
      Caption = 'Select Alerter'
    end
    object tvAlerters: TExtendTreeView
      Left = 8
      Top = 21
      Width = 429
      Height = 193
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnDblClick = tvAlertersDblClick
    end
  end
  inherited acDialog: TActionList
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
end
