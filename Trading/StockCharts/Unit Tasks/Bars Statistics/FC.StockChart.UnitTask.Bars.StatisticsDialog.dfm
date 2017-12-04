inherited fmBarsStatisticsDialog: TfmBarsStatisticsDialog
  Left = 0
  Top = 189
  Caption = 'Statistics'
  ClientHeight = 424
  ClientWidth = 565
  ExplicitWidth = 573
  ExplicitHeight = 453
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 385
    Width = 565
    ExplicitTop = 269
    ExplicitWidth = 521
  end
  object laHelp: TTipPanel [1]
    Left = 0
    Top = 387
    Width = 478
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
    Left = 484
    Top = 395
    ExplicitLeft = 484
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
    Width = 565
    Height = 385
    ActivePage = tsIntradays
    Align = alClient
    TabOrder = 2
    OnChange = pcPagesChange
    object tsIntradays: TTabSheet
      Caption = 'Intraday Peaks'
      DesignSize = (
        557
        357)
      object Label1: TLabel
        Left = 4
        Top = 3
        Width = 184
        Height = 13
        Caption = 'Bar Average Absolute Height (Intraday)'
      end
      object Label2: TLabel
        Left = 271
        Top = 3
        Width = 91
        Height = 13
        Caption = 'Weekday Statistics'
      end
      object grReport: TEditDBGrid
        Left = 4
        Top = 22
        Width = 261
        Height = 158
        DataSource = DataSource1
        Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
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
      object grWeekdayStatistics: TEditDBGrid
        Left = 271
        Top = 22
        Width = 280
        Height = 158
        Anchors = [akLeft, akTop, akRight]
        DataSource = DataSource2
        Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 1
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
      object chIntraday: TChart
        Left = 3
        Top = 186
        Width = 547
        Height = 168
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        MarginBottom = 0
        MarginLeft = 0
        MarginRight = 0
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
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 11
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 2
        Anchors = [akLeft, akTop, akRight, akBottom]
        object chIntradayHL: TBarSeries
          Marks.Callout.Brush.Color = clBlack
          Marks.Font.Height = -9
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
        object chIntradayOC: TBarSeries
          Marks.Callout.Brush.Color = clBlack
          Marks.Font.Height = -9
          Marks.Visible = False
          Title = 'OC'
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
    object tsMonthVolatility: TTabSheet
      Caption = 'Month Volatility'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        557
        357)
      object Label3: TLabel
        Left = 3
        Top = 3
        Width = 16
        Height = 13
        Caption = 'Bar'
      end
      object cbBars: TExtendComboBox
        Left = 25
        Top = 0
        Width = 167
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbBarsChange
        
      end
      object chMonthVolatility: TChart
        Left = 3
        Top = 27
        Width = 547
        Height = 327
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        MarginBottom = 0
        MarginLeft = 0
        MarginRight = 0
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
        LeftAxis.AxisValuesFormat = '#,##'
        LeftAxis.ExactDateTime = False
        LeftAxis.Increment = 10.000000000000000000
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 20
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        Anchors = [akLeft, akTop, akRight, akBottom]
        object chMonthVolatilityHL: TBarSeries
          BarPen.Visible = False
          Marks.Callout.Brush.Color = clBlack
          Marks.Font.Height = -9
          Marks.Style = smsXValue
          Marks.Visible = True
          Title = 'HL'
          Gradient.Direction = gdTopBottom
          MultiBar = mbNone
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
        object chMonthVolatilityOC: TBarSeries
          BarPen.Visible = False
          Marks.Callout.Brush.Color = clBlack
          Marks.Font.Height = -9
          Marks.Style = smsXValue
          Marks.Visible = False
          Title = 'OC'
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
        Width = 557
        Height = 81
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          Left = 165
          Top = 13
          Width = 9
          Height = 13
          Caption = 'pt'
        end
        object Label5: TLabel
          Left = 165
          Top = 59
          Width = 9
          Height = 13
          Caption = 'pt'
        end
        object Bevel1: TBevel
          Left = 0
          Top = 0
          Width = 185
          Height = 81
          Align = alLeft
          Shape = bsRightLine
        end
        object edOC: TExtendSpinEdit
          Left = 108
          Top = 9
          Width = 51
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          OnChange = edOCChange
        end
        object ckOC: TExtendCheckBox
          Left = 8
          Top = 11
          Width = 94
          Height = 17
          Caption = '|Open-Close| >='
          TabOrder = 1
          OnClick = ckHLClick
          FlatFontColor = clBlack
          
        end
        object ckHL: TExtendCheckBox
          Left = 8
          Top = 57
          Width = 94
          Height = 17
          Caption = 'High-Low >='
          TabOrder = 2
          OnClick = ckHLClick
          FlatFontColor = clBlack
          
        end
        object edHL: TExtendSpinEdit
          Left = 108
          Top = 55
          Width = 51
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          OnChange = edOCChange
        end
        object buOR: TRadioButton
          Left = 75
          Top = 34
          Width = 49
          Height = 17
          Caption = 'OR'
          TabOrder = 4
        end
        object buAND: TRadioButton
          Left = 20
          Top = 34
          Width = 49
          Height = 17
          Caption = 'AND'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object ckAllPeaksShowMarks: TExtendCheckBox
          Left = 191
          Top = 33
          Width = 97
          Height = 17
          Caption = 'Show Marks'
          TabOrder = 6
          OnClick = ckAllPeaksShowMarksClick
          FlatFontColor = clBlack
          
        end
      end
      object chAllPeaks: TChart
        Left = 0
        Top = 81
        Width = 557
        Height = 276
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -8
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        MarginBottom = 0
        MarginLeft = 0
        MarginRight = 0
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
        LeftAxis.AxisValuesFormat = '#,##'
        LeftAxis.ExactDateTime = False
        LeftAxis.Increment = 10.000000000000000000
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
        object chAllPeaksHL: TBarSeries
          BarPen.Visible = False
          Marks.Callout.Brush.Color = clBlack
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
        object chAllPeaksOC: TBarSeries
          BarPen.Visible = False
          Marks.Callout.Brush.Color = clBlack
          Marks.Font.Height = -9
          Marks.Style = smsXValue
          Marks.Visible = False
          Title = 'OC'
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
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 379
    Top = 104
    object taReportTime: TTimeField
      DisplayWidth = 12
      FieldName = 'Time'
    end
    object taReportOC: TFloatField
      DisplayLabel = 'OC (pt)'
      FieldName = 'OC'
    end
    object taReportHL: TFloatField
      DisplayLabel = 'HL (pt)'
      FieldName = 'HL'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 347
    Top = 104
  end
  object DataSource2: TDataSource
    DataSet = taWeekdayStatistics
    Left = 347
    Top = 72
  end
  object taWeekdayStatistics: TMemoryDataSet
    FieldDefs = <>
    Left = 379
    Top = 72
    object taWeekdayStatisticsWeekday: TStringField
      DisplayWidth = 10
      FieldName = 'Weekday'
      Size = 32
    end
    object taWeekdayStatisticsOC: TFloatField
      DisplayLabel = 'OC (pt)'
      FieldName = 'OC'
    end
    object taWeekdayStatisticsHL: TFloatField
      DisplayLabel = 'HL(pt)'
      FieldName = 'HL'
    end
  end
end
