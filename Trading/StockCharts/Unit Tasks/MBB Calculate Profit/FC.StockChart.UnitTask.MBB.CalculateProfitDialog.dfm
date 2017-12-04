inherited fmMBBCalculateProfitDialog: TfmMBBCalculateProfitDialog
  Left = 0
  Top = 189
  Caption = 'Calculate profit'
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
    Width = 54
    Height = 13
    Caption = 'Total Profit:'
  end
  object laBestValue: TLabel [2]
    Left = 153
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
    Height = 25
    AutoSize = False
    Caption = 
      'The wizard calculates possible profit/loss based on channel cros' +
      's'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  inherited buOK: TExtendButton
    Left = 440
    Top = 232
    TabOrder = 1
    ExplicitLeft = 440
    ExplicitTop = 232
  end
  object buCalculate: TButton
    Left = 8
    Top = 46
    Width = 75
    Height = 25
    Caption = 'Calculate'
    TabOrder = 0
    OnClick = buCalculateClick
  end
  object grReport: TEditDBGrid
    Left = 89
    Top = 46
    Width = 424
    Height = 151
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
  object pbProgress: TProgressBar
    Left = 8
    Top = 236
    Width = 150
    Height = 17
    Step = 1
    TabOrder = 3
    Visible = False
  end
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 395
    Top = 72
    object taReportNumber: TIntegerField
      DisplayLabel = 'N'
      DisplayWidth = 3
      FieldName = 'Number'
    end
    object taReportOpenTime: TDateTimeField
      DisplayLabel = 'Open Time'
      FieldName = 'OpenTime'
    end
    object taReportOpenPrice: TCurrencyField
      DisplayLabel = 'Open Price'
      DisplayWidth = 6
      FieldName = 'OpenPrice'
      DisplayFormat = '0.0000'
      currency = False
    end
    object taReportCloseTime: TDateTimeField
      DisplayLabel = 'Close Time'
      FieldName = 'CloseTime'
    end
    object taReportClosePrice: TCurrencyField
      DisplayLabel = 'Close Price'
      DisplayWidth = 6
      FieldName = 'ClosePrice'
      DisplayFormat = '0.0000'
    end
    object taReportProfitPt: TIntegerField
      DisplayLabel = 'Profit(Pt)'
      DisplayWidth = 4
      FieldName = 'ProfitPt'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 363
    Top = 72
  end
end
