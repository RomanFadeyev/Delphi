inherited fmValueSupportStatisticsDialog: TfmValueSupportStatisticsDialog
  Left = 0
  Top = 189
  Caption = 'Statistics'
  ClientHeight = 424
  ClientWidth = 579
  ExplicitWidth = 587
  ExplicitHeight = 453
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 385
    Width = 579
    ExplicitTop = 269
    ExplicitWidth = 521
  end
  object laHelp: TTipPanel [1]
    Left = 0
    Top = 387
    Width = 492
    Height = 37
    Align = alNone
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    WordWrap = True
    ControlIndent.YImageIndent = 10
    ExplicitTop = 407
    ExplicitWidth = 459
  end
  inherited buOK: TExtendButton
    Left = 498
    Top = 395
    ExplicitLeft = 498
    ExplicitTop = 395
  end
  object pbProgress: TProgressBar [3]
    Left = 8
    Top = 359
    Width = 150
    Height = 17
    Step = 1
    TabOrder = 1
    Visible = False
  end
  object pcPages: TPageControl [4]
    Left = 0
    Top = 0
    Width = 579
    Height = 385
    ActivePage = tsIntradays
    Align = alClient
    TabOrder = 2
    OnChange = pcPagesChange
    object tsIntradays: TTabSheet
      Caption = 'Intraday Peaks'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 186
        Width = 571
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 173
      end
      object chIntraday: TChart
        Left = 0
        Top = 189
        Width = 571
        Height = 168
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        Legend.Visible = False
        MarginBottom = 0
        MarginLeft = 5
        MarginRight = 1
        MarginTop = 0
        Title.Font.Height = -9
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.DateTimeFormat = 'hh:mm'
        BottomAxis.Increment = 0.041666666666666660
        BottomAxis.LabelsFont.Height = -9
        BottomAxis.LabelsMultiLine = True
        BottomAxis.LabelsSeparation = 0
        BottomAxis.MinorTickCount = 0
        BottomAxis.MinorTickLength = 0
        BottomAxis.TickLength = 0
        BottomAxis.TickOnLabelsOnly = False
        LeftAxis.AxisValuesFormat = '#.####'
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 11
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object chIntradayValue: TBarSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Font.Height = -9
          Marks.Visible = False
          Title = 'Value'
          Gradient.Direction = gdTopBottom
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
          Data = {
            0006000000613255302AA9633FFCA9F1D24D62503FFCA9F1D24D62603FFCA9F1
            D24D62503FFA7E6ABC7493683FFCA9F1D24D62703F}
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 571
        Height = 186
        Align = alTop
        TabOrder = 1
        DesignSize = (
          571
          186)
        object Label2: TLabel
          Left = 183
          Top = 8
          Width = 91
          Height = 13
          Caption = 'Weekday Statistics'
        end
        object Label1: TLabel
          Left = 4
          Top = 8
          Width = 117
          Height = 13
          Caption = 'Average Value (Intraday)'
        end
        object laChartPropsHeader: TLabel
          Left = 375
          Top = 8
          Width = 75
          Height = 13
          Caption = 'Chart Properties'
        end
        object grWeekdayStatistics: TEditDBGrid
          Left = 183
          Top = 22
          Width = 186
          Height = 158
          DataSource = DataSource2
          Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
          ReadOnly = True
          TabOrder = 0
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
        object grReport: TEditDBGrid
          Left = 4
          Top = 22
          Width = 173
          Height = 158
          DataSource = DataSource1
          Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
          ReadOnly = True
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnChangeRecord = grReportChangeRecord
          Options1 = []
          Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
          ColorCheme.UnderCursor = clWindow
          ColorCheme.UnderCursorText = clWindowText
          ColorCheme.HighightCols = 13811126
        end
        object mmChartProps: TExtendMemo
          Left = 375
          Top = 22
          Width = 192
          Height = 158
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          
        end
      end
    end
    object tsMonthVolatility: TTabSheet
      Caption = 'Month Volatility'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        571
        357)
      object Label3: TLabel
        Left = 3
        Top = 3
        Width = 16
        Height = 13
        Caption = 'Bar'
      end
      object cbValueSupport: TExtendComboBox
        Left = 25
        Top = 0
        Width = 167
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbValueSupportChange
        
      end
      object chMonthVolatility: TChart
        Left = 3
        Top = 27
        Width = 561
        Height = 327
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        Legend.Visible = False
        MarginBottom = 0
        MarginLeft = 5
        MarginRight = 1
        MarginTop = 0
        Title.Font.Height = -9
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.DateTimeFormat = 'MM.YYYY'
        BottomAxis.ExactDateTime = False
        BottomAxis.Increment = 1.000000000000000000
        BottomAxis.LabelsFont.Height = -9
        BottomAxis.LabelsMultiLine = True
        BottomAxis.LabelsSeparation = 0
        BottomAxis.MinorTickCount = 0
        BottomAxis.MinorTickLength = 0
        BottomAxis.TickLength = 0
        BottomAxis.TickOnLabelsOnly = False
        LeftAxis.AxisValuesFormat = '#.####'
        LeftAxis.ExactDateTime = False
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 20
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        Anchors = [akLeft, akTop, akRight, akBottom]
        object chMonthVolatilityValue: TBarSeries
          BarPen.Visible = False
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Font.Height = -9
          Marks.Style = smsXValue
          Marks.Visible = True
          Title = 'Value'
          Gradient.Direction = gdTopBottom
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
          Data = {
            00060000009A9999999999B93F9A9999999999C93F333333333333D33F9A9999
            999999B93F9A9999999999C93F333333333333D33F}
        end
      end
    end
    object tsAllPeaks: TTabSheet
      Caption = 'All Peaks'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 571
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object edValueTreshold: TExtendEdit
          Left = 84
          Top = 9
          Width = 70
          Height = 21
          NumLock = False
          KbrdRus = False
          DigitOnly = True
          CapitalizeStyle.WordBreaks = ',. -!?'
          EditType = etDigits
          DigitsStyle.MaxValue = 0
          DigitsStyle.AllowMinus = True
          DigitsStyle.AllowDecimalSeparator = True
          DigitsStyle.NoKeepEmpty = False
          TabOrder = 0
          Text = '0'
          OnChange = edValueTresholdChange
        end
        object ckValueTreshold: TExtendCheckBox
          Left = 8
          Top = 11
          Width = 70
          Height = 17
          Caption = 'Value >='
          TabOrder = 1
          OnClick = ckHLClick
          FlatFontColor = clBlack
          
        end
        object ckAllPeaksShowMarks: TExtendCheckBox
          Left = 8
          Top = 34
          Width = 97
          Height = 17
          Caption = 'Show Marks'
          TabOrder = 2
          OnClick = ckAllPeaksShowMarksClick
          FlatFontColor = clBlack
          
        end
      end
      object chAllPeaks: TChart
        Left = 0
        Top = 57
        Width = 571
        Height = 300
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        Legend.Visible = False
        MarginBottom = 0
        MarginLeft = 5
        MarginRight = 1
        MarginTop = 0
        Title.Font.Height = -9
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.DateTimeFormat = 'DD.MM.YYYY ddd hh:mm'
        BottomAxis.ExactDateTime = False
        BottomAxis.Increment = 1.000000000000000000
        BottomAxis.LabelsFont.Height = -9
        BottomAxis.LabelsMultiLine = True
        BottomAxis.LabelsSeparation = 0
        BottomAxis.MinorTickCount = 0
        BottomAxis.MinorTickLength = 0
        BottomAxis.TickLength = 0
        BottomAxis.TickOnLabelsOnly = False
        DepthAxis.Automatic = False
        DepthAxis.AutomaticMaximum = False
        DepthAxis.AutomaticMinimum = False
        DepthAxis.Maximum = 0.500000000000000000
        DepthAxis.Minimum = -0.500000000000000000
        LeftAxis.AxisValuesFormat = '#.####'
        LeftAxis.ExactDateTime = False
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 20
        RightAxis.Automatic = False
        RightAxis.AutomaticMaximum = False
        RightAxis.AutomaticMinimum = False
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object chAllPeaksValue: TBarSeries
          BarPen.Visible = False
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Font.Height = -9
          Marks.Style = smsXValue
          Marks.Visible = False
          Title = 'HL'
          Gradient.Direction = gdTopBottom
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 80
    Top = 296
  end
  inherited buSeparateWindow: TJvCaptionButton
    Left = 48
    Top = 296
  end
  inherited ilCaption: TImageList
    Left = 112
    Top = 296
  end
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 275
    Top = 128
    object taReportTime: TTimeField
      DisplayWidth = 12
      FieldName = 'Time'
    end
    object taReportValue: TFloatField
      DisplayWidth = 10
      FieldName = 'Value'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 243
    Top = 128
  end
  object DataSource2: TDataSource
    DataSet = taWeekdayStatistics
    Left = 243
    Top = 96
  end
  object taWeekdayStatistics: TMemoryDataSet
    FieldDefs = <>
    Left = 275
    Top = 96
    object taWeekdayStatisticsWeekday: TStringField
      DisplayWidth = 10
      FieldName = 'Weekday'
      Size = 32
    end
    object taWeekdayStatisticsValue: TFloatField
      DisplayWidth = 10
      FieldName = 'Value'
    end
  end
end
