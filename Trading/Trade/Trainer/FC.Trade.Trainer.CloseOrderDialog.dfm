inherited fmTrainerCloseOrderDialog: TfmTrainerCloseOrderDialog
  Caption = 'fmTrainerCloseOrderDialog'
  ClientHeight = 99
  ExplicitHeight = 126
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 60
    ExplicitLeft = -6
    ExplicitTop = 56
  end
  object laCommentTitle: TLabel [1]
    Left = 8
    Top = 32
    Width = 44
    Height = 13
    Caption = 'Comment'
  end
  object Label8: TLabel [2]
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Symbol'
  end
  object laSymbol: TLabel [3]
    Left = 68
    Top = 8
    Width = 19
    Height = 13
    Caption = 'xxx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object laPrice: TLabel [4]
    Left = 164
    Top = 8
    Width = 83
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '.... Price'
  end
  object laPriceValue: TLabel [5]
    Left = 253
    Top = 8
    Width = 25
    Height = 13
    Caption = 'XXX'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object laProfit: TLabel [6]
    Left = 405
    Top = 8
    Width = 25
    Height = 13
    Caption = 'XXX'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel [7]
    Left = 316
    Top = 8
    Width = 83
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Profit (pt)'
  end
  inherited buOK: TExtendButton
    Top = 70
  end
  inherited buCancel: TExtendButton
    Top = 70
  end
  object edComment: TExtendEdit [10]
    Left = 63
    Top = 29
    Width = 445
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 2
  end
  inherited acDialog: TActionList
    Left = 272
    Top = 64
  end
end
