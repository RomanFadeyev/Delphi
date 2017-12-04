inherited fmMBBAdjustWidthDialog: TfmMBBAdjustWidthDialog
  Left = 0
  Top = 189
  Caption = 'Adjust Width '
  ClientHeight = 261
  ClientWidth = 521
  ExplicitWidth = 527
  ExplicitHeight = 288
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 222
    Width = 521
    ExplicitTop = 269
    ExplicitWidth = 521
  end
  object Label1: TLabel [1]
    Left = 89
    Top = 203
    Width = 209
    Height = 13
    Caption = 'Best value (TrendPeriod, Period,Deviations):'
  end
  object laBestValue: TLabel [2]
    Left = 304
    Top = 203
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel [3]
    Left = 8
    Top = 8
    Width = 505
    Height = 38
    AutoSize = False
    Caption = 
      'The wizard searchers bands and signal crosses. Ideally signal ha' +
      's to ping-pong from one band to antother with minimal slippage.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel [4]
    Left = 8
    Top = 52
    Width = 22
    Height = 13
    Caption = 'Jitter'
  end
  object Label4: TLabel [5]
    Left = 162
    Top = 52
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel [6]
    Left = 290
    Top = 52
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  inherited buOK: TExtendButton
    Left = 440
    Top = 232
    TabOrder = 1
    ExplicitLeft = 440
    ExplicitTop = 232
  end
  object buAdjust: TButton
    Left = 8
    Top = 77
    Width = 75
    Height = 25
    Caption = '&Adjust'
    TabOrder = 0
    OnClick = buAdjustClick
  end
  object grReport: TEditDBGrid
    Left = 89
    Top = 77
    Width = 424
    Height = 120
    DataSource = DataSource1
    Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Options1 = []
    Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
    ColorCheme.UnderCursor = clWindow
    ColorCheme.UnderCursorText = clWindowText
    ColorCheme.HighightCols = 13811126
  end
  object edJitter: TExtendSpinEdit
    Left = 89
    Top = 50
    Width = 64
    Height = 21
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 20
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 236
    Width = 150
    Height = 17
    Step = 1
    TabOrder = 4
    Visible = False
  end
  object DateTimePicker1: TExtendDateTimePicker
    Left = 200
    Top = 50
    Width = 81
    Height = 21
    Date = 38856.991204965280000000
    Time = 38856.991204965280000000
    TabOrder = 5
    TransparentBorder = False
    
  end
  object DateTimePicker2: TExtendDateTimePicker
    Left = 328
    Top = 50
    Width = 81
    Height = 21
    Date = 38856.991204965280000000
    Time = 38856.991204965280000000
    TabOrder = 6
    TransparentBorder = False
    
  end
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 395
    Top = 72
    object taReportNumber: TIntegerField
      DisplayLabel = 'N'
      DisplayWidth = 10
      FieldName = 'Number'
    end
    object taReportTrendPeriod: TIntegerField
      FieldName = 'TrendPeriod'
    end
    object taReportPeriod: TIntegerField
      FieldName = 'Period'
    end
    object taReportProfit: TFloatField
      DisplayLabel = 'Profit, pt'
      DisplayWidth = 20
      FieldName = 'Profit'
      DisplayFormat = '0.0000'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 363
    Top = 72
  end
end
