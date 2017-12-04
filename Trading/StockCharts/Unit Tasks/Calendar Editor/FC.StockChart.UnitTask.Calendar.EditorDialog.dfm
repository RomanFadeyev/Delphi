inherited fmCalendarEditorDialog: TfmCalendarEditorDialog
  Caption = 'Edit Calendar Item'
  ClientHeight = 248
  ClientWidth = 549
  ExplicitWidth = 555
  ExplicitHeight = 275
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 209
    Width = 549
    ExplicitLeft = -1
    ExplicitTop = 194
    ExplicitWidth = 572
  end
  object Label1: TLabel [1]
    Left = 8
    Top = 12
    Width = 23
    Height = 13
    Caption = 'Time'
    FocusControl = edOpenTime
  end
  object Label6: TLabel [2]
    Left = 8
    Top = 37
    Width = 36
    Height = 13
    Caption = 'Country'
  end
  object Label2: TLabel [3]
    Left = 8
    Top = 60
    Width = 41
    Height = 13
    Caption = 'Indicator'
  end
  object Label3: TLabel [4]
    Left = 8
    Top = 84
    Width = 30
    Height = 13
    Caption = 'Period'
  end
  object Label4: TLabel [5]
    Left = 328
    Top = 84
    Width = 31
    Height = 13
    Caption = 'Priority'
  end
  object Label5: TLabel [6]
    Left = 211
    Top = 108
    Width = 41
    Height = 13
    Caption = 'Forecast'
  end
  object Label7: TLabel [7]
    Left = 398
    Top = 108
    Width = 21
    Height = 13
    Caption = 'Fact'
  end
  object Label8: TLabel [8]
    Left = 9
    Top = 178
    Width = 28
    Height = 13
    Caption = 'Notes'
  end
  object Label9: TLabel [9]
    Left = 8
    Top = 132
    Width = 62
    Height = 13
    Caption = 'Previous-fact'
  end
  object Label10: TLabel [10]
    Left = 299
    Top = 132
    Width = 62
    Height = 13
    Caption = 'Forecast fact'
  end
  object Label11: TLabel [11]
    Left = 7
    Top = 108
    Width = 41
    Height = 13
    Caption = 'Previous'
  end
  object buPaste: TSpeedButton [12]
    Left = 468
    Top = 8
    Width = 73
    Height = 21
    Caption = 'Paste   '
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
    Layout = blGlyphRight
    OnClick = buPasteClick
  end
  object Label12: TLabel [13]
    Left = 9
    Top = 154
    Width = 34
    Height = 13
    Caption = 'Source'
  end
  inherited buOK: TExtendButton
    Left = 308
    Top = 219
    TabOrder = 14
    ExplicitLeft = 308
    ExplicitTop = 219
  end
  inherited buCancel: TExtendButton
    Left = 388
    Top = 219
    TabOrder = 15
    ExplicitLeft = 388
    ExplicitTop = 219
  end
  object edOpenDate: TExtendDateTimePicker [16]
    Left = 80
    Top = 8
    Width = 120
    Height = 21
    Date = 38761.538175462960000000
    Time = 38761.538175462960000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TransparentBorder = False
    
  end
  object edCountry: TExtendComboBox [17]
    Left = 80
    Top = 32
    Width = 201
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 3
    
  end
  object edIndicator: TExtendEdit [18]
    Left = 80
    Top = 56
    Width = 460
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object edPeriod: TExtendEdit [19]
    Left = 80
    Top = 80
    Width = 235
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object edPriority: TExtendEdit [20]
    Left = 368
    Top = 80
    Width = 172
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 6
  end
  object edForecast: TExtendEdit [21]
    Left = 256
    Top = 104
    Width = 113
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 8
  end
  object edFact: TExtendEdit [22]
    Left = 427
    Top = 104
    Width = 113
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 9
  end
  object edNotes: TExtendEdit [23]
    Left = 80
    Top = 175
    Width = 460
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 13
  end
  object cbPFTrend: TExtendComboBox [24]
    Left = 80
    Top = 128
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 10
    
  end
  object cbFFTrend: TExtendComboBox [25]
    Left = 368
    Top = 128
    Width = 172
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 11
    
  end
  object edPrevious: TExtendEdit [26]
    Left = 80
    Top = 104
    Width = 113
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 7
  end
  inherited buApply: TExtendButton
    Left = 468
    Top = 219
    TabOrder = 16
    ExplicitLeft = 468
    ExplicitTop = 219
  end
  object edOpenTime: TExtendMinutePicker [28]
    Left = 206
    Top = 8
    Width = 51
    Height = 21
    NumLock = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object cbTimeZone: TExtendComboBox [29]
    Left = 263
    Top = 8
    Width = 130
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    
  end
  object edSource: TExtendEdit [30]
    Left = 80
    Top = 151
    Width = 460
    Height = 21
    NumLock = False
    KbrdRus = False
    CapitalizeStyle.WordBreaks = ',. -!?'
    DigitsStyle.MaxValue = 0
    DigitsStyle.AllowMinus = False
    DigitsStyle.AllowDecimalSeparator = False
    DigitsStyle.NoKeepEmpty = False
    TabOrder = 12
  end
  inherited acDialog: TActionList
    Left = 237
    Top = 32
  end
  inherited alActions: TActionList
    Left = 405
  end
  inherited ilImages: TImageList
    Left = 437
  end
  object pmPaste: TPopupActionBar
    Left = 340
    Top = 32
    object miFromIfcMarkets: TMenuItem
      Caption = 'From ifcmarkets.com'
      OnClick = miFromIfcMarketsClick
    end
    object miFromSaxoBankcom: TMenuItem
      Caption = 'From SaxoBank.com'
      OnClick = miFromSaxoBankcomClick
    end
  end
  object WordDocument: TWordDocument
    AutoConnect = False
    ConnectKind = ckNewInstance
    Left = 392
    Top = 184
  end
end
