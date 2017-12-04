inherited fmFilterDialog: TfmFilterDialog
  Caption = 'Enter Filter'
  OnClose = FormClose
  OnShow = FormShow
  ExplicitHeight = 273
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [1]
    AlignWithMargins = True
    Left = 6
    Top = 143
    Width = 504
    Height = 13
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 6
    Margins.Bottom = 0
    Align = alBottom
    Caption = 'Available fields'
    ExplicitWidth = 70
  end
  inherited buOK: TExtendButton
    TabOrder = 1
  end
  inherited buCancel: TExtendButton
    TabOrder = 3
  end
  object mmAvailableFields: TMemo [4]
    AlignWithMargins = True
    Left = 6
    Top = 160
    Width = 504
    Height = 40
    Margins.Left = 6
    Margins.Top = 4
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    ReadOnly = True
    TabOrder = 0
  end
  object mmFilter: TJvHLEditor [5]
    AlignWithMargins = True
    Left = 6
    Top = 6
    Width = 482
    Height = 134
    Cursor = crIBeam
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 0
    Completion.Enabled = True
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    BracketHighlighting.Active = True
    BracketHighlighting.StringEscape = #39#39
    OnCompletionIdentifier = mmFilterCompletionIdentifier
    OnCompletionDrawItem = mmFilterCompletionDrawItem
    OnCompletionMeasureItem = mmFilterCompletionMeasureItem
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Highlighter = hlNone
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clOlive
    Colors.Comment.BackColor = clWindow
    Colors.Number.ForeColor = clPurple
    Colors.Number.BackColor = clWindow
    Colors.Strings.ForeColor = clGreen
    Colors.Strings.BackColor = clWindow
    Colors.Symbol.ForeColor = clBlue
    Colors.Symbol.BackColor = clWindow
    Colors.Reserved.Style = [fsBold]
    Colors.Reserved.ForeColor = clBlue
    Colors.Reserved.BackColor = clWindow
    Colors.Identifier.ForeColor = clGray
    Colors.Identifier.BackColor = clWindow
    Colors.Preproc.ForeColor = clGreen
    Colors.Preproc.BackColor = clWindow
    Colors.FunctionCall.ForeColor = clRed
    Colors.FunctionCall.BackColor = clWindow
    Colors.Declaration.ForeColor = clWindowText
    Colors.Declaration.BackColor = clWindow
    Colors.Statement.Style = [fsBold]
    Colors.Statement.ForeColor = clWindowText
    Colors.Statement.BackColor = clWindow
    Colors.PlainText.ForeColor = clWindowText
    Colors.PlainText.BackColor = clWindow
  end
  object Panel1: TPanel [6]
    Left = 488
    Top = 0
    Width = 28
    Height = 143
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    object buRecentlyFilters: TSpeedButton
      Left = 2
      Top = 6
      Width = 23
      Height = 22
      Hint = 'Recently filters'
      Flat = True
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
        000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000000000000000
        00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      ParentShowHint = False
      ShowHint = True
      OnClick = buRecentlyFiltersClick
    end
  end
  object rlFilters: TRecentlyList
    TruncatePaths = False
    RecentlyMenu = pmRecentlyFilters
    MaxCount = 16
    CaseSensivity = False
    OnRecentlyMenuClick = rlFiltersRecentlyMenuClick
    Left = 10
    Top = 10
  end
  object pmRecentlyFilters: TPopupActionBar
    Left = 424
    Top = 16
  end
end
