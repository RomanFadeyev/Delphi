inherited fmTrainerNewOrderDialog: TfmTrainerNewOrderDialog
  Caption = 'New Order'
  ClientHeight = 183
  ClientWidth = 532
  ExplicitWidth = 538
  ExplicitHeight = 210
  DesignSize = (
    532
    183)
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 144
    Width = 532
    ExplicitTop = 91
    ExplicitWidth = 532
  end
  object laLotsTitle: TLabel [1]
    Left = 8
    Top = 59
    Width = 20
    Height = 13
    Caption = 'Lots'
  end
  object Label2: TLabel [2]
    Left = 8
    Top = 89
    Width = 47
    Height = 13
    Caption = 'Stop Loss'
  end
  object Label3: TLabel [3]
    Left = 192
    Top = 89
    Width = 55
    Height = 13
    Caption = 'Take  Profit'
  end
  object Label4: TLabel [4]
    Left = 364
    Top = 88
    Width = 59
    Height = 13
    Caption = 'Trailing Stop'
  end
  object buStopLoss: TSpeedButton [5]
    Left = 164
    Top = 86
    Width = 14
    Height = 21
    Flat = True
    Glyph.Data = {
      F6000000424DF60000000000000076000000280000000A000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
      0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
    OnClick = buStopLossClick
  end
  object buTakeProfit: TSpeedButton [6]
    Left = 336
    Top = 85
    Width = 14
    Height = 21
    Flat = True
    Glyph.Data = {
      F6000000424DF60000000000000076000000280000000A000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
      0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
    OnClick = buTakeProfitClick
  end
  object buTrailingStop: TSpeedButton [7]
    Left = 508
    Top = 86
    Width = 14
    Height = 21
    Flat = True
    Glyph.Data = {
      F6000000424DF60000000000000076000000280000000A000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
      0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
    OnClick = buTrailingStopClick
  end
  object SpeedButton2: TSpeedButton [8]
    Left = 164
    Top = 57
    Width = 14
    Height = 21
    Flat = True
    Glyph.Data = {
      F6000000424DF60000000000000076000000280000000A000000100000000100
      04000000000080000000C40E0000C40E00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
      0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
      0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
    OnClick = SpeedButton2Click
  end
  object laCommentTitle: TLabel [9]
    Left = 8
    Top = 116
    Width = 44
    Height = 13
    Caption = 'Comment'
  end
  object laSymbol: TLabel [10]
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
  object Label8: TLabel [11]
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Symbol'
  end
  object laAsk: TLabel [12]
    Left = 188
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
  object laAskTitle: TLabel [13]
    Left = 164
    Top = 8
    Width = 18
    Height = 13
    Alignment = taRightJustify
    Caption = 'Ask'
  end
  object laOpenPriceTitle: TLabel [14]
    Left = 8
    Top = 33
    Width = 53
    Height = 13
    Caption = 'Open Price'
  end
  object laOrderType: TLabel [15]
    Left = 167
    Top = 35
    Width = 342
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'XXX'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object laBid: TLabel [16]
    Left = 287
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
  object Label5: TLabel [17]
    Left = 266
    Top = 8
    Width = 15
    Height = 13
    Alignment = taRightJustify
    Caption = 'Bid'
  end
  inherited buOK: TExtendButton
    Left = 371
    Top = 154
    TabOrder = 5
    ExplicitLeft = 371
    ExplicitTop = 154
  end
  inherited buCancel: TExtendButton
    Left = 451
    Top = 154
    TabOrder = 6
    ExplicitLeft = 451
    ExplicitTop = 154
  end
  object edStopLoss: TExtendEdit [20]
    Left = 68
    Top = 86
    Width = 93
    Height = 21
    NumLock = False
    KbrdRus = False
    DigitOnly = True
    CapitalizeStyle.WordBreaks = ',. -!?'
    EditType = etDigits
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = True
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 1
  end
  object edTakeProfit: TExtendEdit [21]
    Left = 253
    Top = 85
    Width = 81
    Height = 21
    NumLock = False
    KbrdRus = False
    DigitOnly = True
    CapitalizeStyle.WordBreaks = ',. -!?'
    EditType = etDigits
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = True
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 2
  end
  object edTrailingStop: TExtendEdit [22]
    Left = 425
    Top = 85
    Width = 81
    Height = 21
    NumLock = False
    KbrdRus = False
    DigitOnly = True
    CapitalizeStyle.WordBreaks = ',. -!?'
    EditType = etDigits
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 3
  end
  object edLots: TExtendEdit [23]
    Left = 68
    Top = 56
    Width = 93
    Height = 21
    NumLock = False
    KbrdRus = False
    DigitOnly = True
    CapitalizeStyle.WordBreaks = ',. -!?'
    EditType = etDigits
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = True
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 0
    Text = '0.1'
  end
  object edComment: TExtendEdit [24]
    Left = 68
    Top = 113
    Width = 453
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 4
  end
  object edOpenPrice: TExtendEdit [25]
    Left = 68
    Top = 30
    Width = 93
    Height = 21
    NumLock = False
    KbrdRus = False
    DigitOnly = True
    CapitalizeStyle.WordBreaks = ',. -!?'
    EditType = etDigits
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = True
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 7
  end
  inherited acDialog: TActionList
    Left = 336
    Top = 48
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
  object pmStopLoss: TPopupActionBar
    Left = 175
    Top = 93
    object N10pt1: TMenuItem
      Tag = 10
      Caption = '&10 pt'
      OnClick = miStopLossClick
    end
    object N201: TMenuItem
      Tag = 20
      Caption = '&20 pt'
      OnClick = miStopLossClick
    end
    object N30pt1: TMenuItem
      Tag = 30
      Caption = '&30 pt'
      OnClick = miStopLossClick
    end
    object N40pt1: TMenuItem
      Tag = 40
      Caption = '&40 pt'
      OnClick = miStopLossClick
    end
    object N50pt1: TMenuItem
      Tag = 50
      Caption = '&50 pt'
      OnClick = miStopLossClick
    end
    object N50pt2: TMenuItem
      Tag = 60
      Caption = '&60 pt'
      OnClick = miStopLossClick
    end
    object N50pt3: TMenuItem
      Tag = 70
      Caption = '&70 pt'
      OnClick = miStopLossClick
    end
    object N50pt4: TMenuItem
      Tag = 80
      Caption = '&80 pt'
      OnClick = miStopLossClick
    end
    object N80pt1: TMenuItem
      Tag = 90
      Caption = '&90 pt'
      OnClick = miStopLossClick
    end
    object N100pt1: TMenuItem
      Tag = 100
      Caption = '1&00 pt'
      OnClick = miStopLossClick
    end
  end
  object pmTakeProfit: TPopupActionBar
    Left = 343
    Top = 85
    object N10pt2: TMenuItem
      Tag = 10
      Caption = '&10 pt'
      OnClick = miTakeProfit
    end
    object N20pt1: TMenuItem
      Tag = 20
      Caption = '&20 pt'
      OnClick = miTakeProfit
    end
    object N30pt2: TMenuItem
      Tag = 30
      Caption = '&30 pt'
      OnClick = miTakeProfit
    end
    object N40pt2: TMenuItem
      Tag = 40
      Caption = '&40 pt'
      OnClick = miTakeProfit
    end
    object N50pt5: TMenuItem
      Tag = 50
      Caption = '&50 pt'
      OnClick = miTakeProfit
    end
    object N60pt1: TMenuItem
      Tag = 60
      Caption = '&60 pt'
      OnClick = miTakeProfit
    end
    object N70pt1: TMenuItem
      Tag = 70
      Caption = '&70 pt'
      OnClick = miTakeProfit
    end
    object N80pt2: TMenuItem
      Tag = 80
      Caption = '&80 pt'
      OnClick = miTakeProfit
    end
    object N90pt1: TMenuItem
      Tag = 90
      Caption = '&90 pt'
      OnClick = miTakeProfit
    end
    object N100pt2: TMenuItem
      Tag = 100
      Caption = '1&00 pt'
      OnClick = miTakeProfit
    end
  end
  object pmTrailingStop: TPopupActionBar
    Left = 503
    Top = 101
    object N10pt3: TMenuItem
      Tag = 10
      Caption = '&10 pt'
      OnClick = miTrailingStopClick
    end
    object N20pt2: TMenuItem
      Tag = 20
      Caption = '&20 pt'
      OnClick = miTrailingStopClick
    end
    object N30pt3: TMenuItem
      Tag = 30
      Caption = '&30 pt'
      OnClick = miTrailingStopClick
    end
    object N40pt3: TMenuItem
      Tag = 40
      Caption = '&40 pt'
      OnClick = miTrailingStopClick
    end
    object N50pt6: TMenuItem
      Tag = 50
      Caption = '&50 pt'
      OnClick = miTrailingStopClick
    end
    object N60pt2: TMenuItem
      Tag = 60
      Caption = '&60 pt'
      OnClick = miTrailingStopClick
    end
    object N70pt2: TMenuItem
      Tag = 70
      Caption = '&70 pt'
      OnClick = miTrailingStopClick
    end
    object N80pt3: TMenuItem
      Tag = 80
      Caption = '&80 pt'
      OnClick = miTrailingStopClick
    end
    object N90pt2: TMenuItem
      Tag = 90
      Caption = '&90 pt'
      OnClick = miTrailingStopClick
    end
    object N100pt3: TMenuItem
      Tag = 100
      Caption = '1&00 pt'
      OnClick = miTrailingStopClick
    end
  end
  object pmLots: TPopupActionBar
    Left = 183
    Top = 53
    object MenuItem21: TMenuItem
      Caption = '0.1'
    end
    object MenuItem22: TMenuItem
      Caption = '0.2'
    end
    object MenuItem23: TMenuItem
      Caption = '0.3'
    end
    object MenuItem24: TMenuItem
      Caption = '0.4'
    end
    object MenuItem25: TMenuItem
      Caption = '0.5'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItem26: TMenuItem
      Caption = '1'
    end
    object MenuItem27: TMenuItem
      Caption = '5'
    end
    object MenuItem28: TMenuItem
      Caption = '10'
    end
    object MenuItem29: TMenuItem
      Caption = '20'
    end
    object MenuItem30: TMenuItem
      Caption = '50'
    end
  end
end
