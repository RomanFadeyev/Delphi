inherited fmCreateIndicatorDialog: TfmCreateIndicatorDialog
  ActiveControl = tvIndicators
  BorderStyle = bsSizeable
  Caption = 'Select Indicator'
  ClientHeight = 332
  ClientWidth = 449
  ExplicitWidth = 457
  ExplicitHeight = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 293
    Width = 449
    ExplicitTop = 222
    ExplicitWidth = 445
  end
  inherited buOK: TExtendButton
    Left = 288
    Top = 303
    ExplicitLeft = 288
    ExplicitTop = 303
  end
  inherited buCancel: TExtendButton
    Left = 368
    Top = 303
    ExplicitLeft = 368
    ExplicitTop = 303
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 0
    Width = 449
    Height = 293
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    Caption = 'Panel1'
    TabOrder = 2
    object laHeader: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 433
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      Caption = 'Select indicator'
      ExplicitWidth = 73
    end
    object tvIndicators: TExtendTreeView
      Left = 8
      Top = 24
      Width = 433
      Height = 261
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RowSelect = True
      ShowRoot = False
      TabOrder = 0
      OnAdvancedCustomDrawItem = tvIndicatorsAdvancedCustomDrawItem
      OnDblClick = tvIndicatorsDblClick
      
    end
  end
  inherited acDialog: TActionList
    Top = 48
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
end
