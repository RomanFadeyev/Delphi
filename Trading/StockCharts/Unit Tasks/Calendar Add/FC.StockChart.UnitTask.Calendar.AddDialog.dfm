inherited fmDialogOkCancel_B1: TfmDialogOkCancel_B1
  Caption = 'Add Calendar Items'
  ClientHeight = 436
  ClientWidth = 611
  ExplicitWidth = 617
  ExplicitHeight = 463
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 397
    Width = 611
  end
  inherited buOK: TExtendButton
    Left = 450
    Top = 407
  end
  inherited buCancel: TExtendButton
    Left = 530
    Top = 407
  end
  object ToolBar1: TToolBar [3]
    Left = 0
    Top = 0
    Width = 611
    Height = 21
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 105
    Caption = 'ToolBar1'
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'From SaxoBank.com'
      ImageIndex = 0
    end
  end
  inherited acDialog: TActionList
    Left = 288
    Top = 72
  end
  object WordDocument: TWordDocument
    AutoConnect = False
    ConnectKind = ckNewInstance
    Left = 392
    Top = 184
  end
end
