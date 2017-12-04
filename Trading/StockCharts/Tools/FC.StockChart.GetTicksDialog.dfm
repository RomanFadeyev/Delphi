inherited fmGetTicksDialog: TfmGetTicksDialog
  Caption = 'Ticks'
  ClientHeight = 480
  ClientWidth = 630
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  ExplicitWidth = 638
  ExplicitHeight = 509
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 441
    Width = 630
    ExplicitLeft = -3
    ExplicitTop = 266
    ExplicitWidth = 412
  end
  inherited buOK: TExtendButton
    Left = 549
    Top = 451
    ExplicitLeft = 549
    ExplicitTop = 451
  end
  object pcPages: TPageControl [2]
    Left = 0
    Top = 0
    Width = 630
    Height = 441
    ActivePage = tsData
    Align = alClient
    TabOrder = 1
    OnChange = pcPagesChange
    object tsData: TTabSheet
      Caption = 'Data'
      OnResize = tsDataResize
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        622
        413)
      object Splitter1: TSplitter
        Left = 0
        Top = 200
        Width = 622
        Height = 2
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 148
      end
      object chTicks: TChart
        Left = 0
        Top = 202
        Width = 622
        Height = 211
        AllowPanning = pmVertical
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.ColorWidth = 15
        Legend.Font.Height = -9
        Legend.HorizMargin = 4
        Legend.LegendStyle = lsSeries
        Legend.Shadow.HorizSize = 0
        Legend.Shadow.VertSize = 0
        Legend.Symbol.Width = 15
        Legend.TextStyle = ltsPlain
        MarginBottom = 0
        MarginLeft = 0
        MarginRight = 1
        MarginTop = 3
        Title.Font.Height = -9
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.DateTimeFormat = 'hh:mm:ss'
        BottomAxis.Increment = 0.000115740740740741
        BottomAxis.LabelsFont.Height = -9
        BottomAxis.LabelsMultiLine = True
        BottomAxis.LabelsSeparation = 0
        BottomAxis.MinorTickCount = 0
        BottomAxis.MinorTickLength = 0
        BottomAxis.TickLength = 0
        BottomAxis.TickOnLabelsOnly = False
        LeftAxis.AxisValuesFormat = '#,##0.0000'
        LeftAxis.LabelsFont.Height = -9
        LeftAxis.LabelsSize = 30
        LeftAxis.StartPosition = 3.000000000000000000
        RightAxis.Visible = False
        TopAxis.Visible = False
        View3D = False
        View3DOptions.HorizOffset = -19
        Zoom.AnimatedSteps = 7
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ColorPaletteIndex = 13
        object chTicksReal: TFastLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Callout.Length = 10
          Marks.Font.Height = -9
          Marks.Style = smsValue
          Marks.Visible = False
          Title = 'Real'
          LinePen.Color = clRed
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
        object chTicksEmulated: TFastLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Style = smsValue
          Marks.Visible = False
          Title = 'Emulated'
          LinePen.Color = clGreen
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 622
        Height = 200
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object paDataReal: TPanel
          Left = 0
          Top = 0
          Width = 313
          Height = 200
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object Label3: TLabel
            AlignWithMargins = True
            Left = 6
            Top = 3
            Width = 51
            Height = 13
            Margins.Left = 6
            Margins.Bottom = 0
            Align = alTop
            BiDiMode = bdLeftToRight
            Caption = 'Data Real:'
            ParentBiDiMode = False
          end
          object grData: TEditDBGrid
            AlignWithMargins = True
            Left = 6
            Top = 19
            Width = 301
            Height = 118
            Margins.Left = 6
            Margins.Right = 6
            Align = alClient
            DataSource = dsData
            Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            Options1 = [dgWrapColumnTitles, dgSearchControls]
            Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos, dgSupressColumnActivation]
            ColorCheme.UnderCursor = clWindow
            ColorCheme.UnderCursorText = clWindowText
            ColorCheme.HighightCols = 13811126
          end
          object Panel1: TPanel
            AlignWithMargins = True
            Left = 6
            Top = 143
            Width = 301
            Height = 54
            Margins.Left = 6
            Margins.Right = 6
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object Label1: TLabel
              Left = 0
              Top = 42
              Width = 120
              Height = 13
              Caption = 'Average ticks per minute:'
            end
            object laAverageTicksPerMinute: TLabel
              Left = 136
              Top = 42
              Width = 25
              Height = 13
              Caption = 'xxxx'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object laTotalTicks: TLabel
              Left = 136
              Top = 10
              Width = 25
              Height = 13
              Caption = 'xxxx'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label4: TLabel
              Left = 0
              Top = 10
              Width = 97
              Height = 13
              Caption = 'Total ticks in interval'
            end
            object Label2: TLabel
              Left = 0
              Top = 26
              Width = 103
              Height = 13
              Caption = 'Minute bars in interval'
            end
            object laTotalMinutes: TLabel
              Left = 136
              Top = 26
              Width = 25
              Height = 13
              Caption = 'xxxx'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Bevel2: TBevel
              Left = 0
              Top = 0
              Width = 301
              Height = 4
              Align = alTop
              Shape = bsBottomLine
              ExplicitWidth = 610
            end
          end
        end
        object Panel6: TPanel
          Left = 313
          Top = 0
          Width = 309
          Height = 200
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel6'
          TabOrder = 1
          object Label8: TLabel
            AlignWithMargins = True
            Left = 6
            Top = 3
            Width = 73
            Height = 13
            Margins.Left = 6
            Margins.Bottom = 0
            Align = alTop
            BiDiMode = bdLeftToRight
            Caption = 'Data Emulated:'
            ParentBiDiMode = False
          end
          object EditDBGrid1: TEditDBGrid
            AlignWithMargins = True
            Left = 6
            Top = 19
            Width = 297
            Height = 178
            Margins.Left = 6
            Margins.Right = 6
            Align = alClient
            Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            Options1 = [dgWrapColumnTitles, dgSearchControls]
            Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos, dgSupressColumnActivation]
            ColorCheme.UnderCursor = clWindow
            ColorCheme.UnderCursorText = clWindowText
            ColorCheme.HighightCols = 13811126
          end
        end
      end
      object ckTickDataShowMarks: TExtendCheckBox
        Left = 559
        Top = 255
        Width = 47
        Height = 17
        Hint = 'Show marks on the chart'
        Anchors = [akTop, akRight]
        Caption = 'Marks'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = ckTickDataShowMarksClick
        FlatFontColor = clBlack
        
      end
    end
    object tsTicksInBar: TTabSheet
      Caption = 'Average Tick Frequency'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 622
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label5: TLabel
          Left = 8
          Top = 5
          Width = 63
          Height = 13
          Caption = 'Time interval:'
        end
        object cbIntervals: TExtendComboBox
          Left = 77
          Top = 1
          Width = 161
          Height = 21
          Align = alCustom
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbIntervalsChange
          
        end
      end
      object tcTabs: TFlatTabControl
        Left = 0
        Top = 397
        Width = 622
        Height = 16
        TabIndex = 0
        Tabs = <
          item
            Caption = 'In Time'
          end
          item
            Caption = 'Percentage'
          end>
        Flat = False
        DrawTopLine = False
        Align = alBottom
        Color = clBtnFace
        ParentColor = False
        TabOrder = 1
        OnChange = tcTabsChange
      end
      object pcAverageTickTabs: TJvPageControl
        Left = 0
        Top = 25
        Width = 622
        Height = 372
        ActivePage = tsAbsoluteValues
        Align = alClient
        TabOrder = 2
        HideAllTabs = True
        object tsAbsoluteValues: TTabSheet
          Caption = 'tsAbsoluteValues'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object chTickInBarStatistics: TChart
            Left = 0
            Top = 0
            Width = 614
            Height = 344
            AllowPanning = pmVertical
            BackWall.Brush.Color = clWhite
            BackWall.Brush.Style = bsClear
            Legend.ColorWidth = 15
            Legend.Font.Height = -8
            Legend.LegendStyle = lsSeries
            Legend.Shadow.HorizSize = 0
            Legend.Shadow.VertSize = 0
            Legend.Symbol.Width = 15
            Legend.TextStyle = ltsPlain
            Legend.Visible = False
            MarginBottom = 0
            MarginLeft = 0
            MarginRight = 1
            MarginTop = 3
            Title.Font.Height = -9
            Title.Text.Strings = (
              'TChart')
            Title.Visible = False
            BottomAxis.DateTimeFormat = 'dd[hh:mm]'
            BottomAxis.Increment = 0.003472222222222222
            BottomAxis.LabelsFont.Height = -9
            BottomAxis.LabelsMultiLine = True
            BottomAxis.LabelsSeparation = 0
            BottomAxis.MinorTickCount = 0
            BottomAxis.MinorTickLength = 0
            BottomAxis.TickLength = 0
            BottomAxis.TickOnLabelsOnly = False
            LeftAxis.LabelsFont.Height = -9
            LeftAxis.LabelsSize = 20
            View3D = False
            View3DOptions.HorizOffset = -19
            Zoom.AnimatedSteps = 7
            Align = alClient
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            ColorPaletteIndex = 13
            object chTickInBarStatisticsValue: TBarSeries
              BarPen.Visible = False
              Marks.Arrow.Visible = True
              Marks.Callout.Brush.Color = clBlack
              Marks.Callout.Arrow.Visible = True
              Marks.Font.Height = -9
              Marks.Visible = False
              Title = 'Count'
              Dark3D = False
              MultiBar = mbNone
              XValues.DateTime = True
              XValues.Name = 'X'
              XValues.Order = loAscending
              YValues.Name = 'Bar'
              YValues.Order = loNone
            end
            object chTickInBarStatisticsVolumes: TFastLineSeries
              Marks.Arrow.Visible = True
              Marks.Callout.Brush.Color = clBlack
              Marks.Callout.Arrow.Visible = True
              Marks.Visible = False
              Title = 'Volume'
              LinePen.Color = clGreen
              XValues.Name = 'X'
              XValues.Order = loAscending
              YValues.Name = 'Y'
              YValues.Order = loNone
            end
          end
        end
        object tsPercentage: TTabSheet
          Caption = 'Percentage'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object chPercentage: TChart
            Left = 0
            Top = 49
            Width = 614
            Height = 295
            AllowPanning = pmNone
            BackWall.Brush.Color = clWhite
            BackWall.Brush.Style = bsClear
            BackWall.Pen.Visible = False
            Legend.ColorWidth = 15
            Legend.Font.Height = -8
            Legend.LegendStyle = lsSeries
            Legend.Shadow.HorizSize = 0
            Legend.Shadow.VertSize = 0
            Legend.Symbol.Width = 15
            Legend.TextStyle = ltsPlain
            Legend.Visible = False
            MarginBottom = 5
            MarginLeft = 0
            MarginRight = 1
            MarginTop = 5
            Title.Font.Height = -9
            Title.Text.Strings = (
              'TChart')
            Title.Visible = False
            AxisVisible = False
            BottomAxis.DateTimeFormat = 'dd[hh:mm]'
            BottomAxis.Increment = 0.003472222222222222
            BottomAxis.LabelsFont.Height = -9
            BottomAxis.LabelsMultiLine = True
            BottomAxis.LabelsSeparation = 0
            BottomAxis.MinorTickCount = 0
            BottomAxis.MinorTickLength = 0
            BottomAxis.TickLength = 0
            BottomAxis.TickOnLabelsOnly = False
            ClipPoints = False
            DepthAxis.Automatic = False
            DepthAxis.AutomaticMaximum = False
            DepthAxis.AutomaticMinimum = False
            DepthAxis.Maximum = 0.830000000000000300
            DepthAxis.Minimum = -0.169999999999999800
            Frame.Visible = False
            LeftAxis.Automatic = False
            LeftAxis.AutomaticMaximum = False
            LeftAxis.AutomaticMinimum = False
            LeftAxis.LabelsFont.Height = -9
            LeftAxis.LabelsSize = 20
            RightAxis.Automatic = False
            RightAxis.AutomaticMaximum = False
            RightAxis.AutomaticMinimum = False
            View3DOptions.Elevation = 315
            View3DOptions.HorizOffset = -19
            View3DOptions.Orthogonal = False
            View3DOptions.Perspective = 0
            View3DOptions.Rotation = 360
            View3DWalls = False
            Zoom.Allow = False
            Zoom.AnimatedSteps = 7
            Align = alClient
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            ColorPaletteIndex = 13
            object BarSeries1: TPieSeries
              Marks.Arrow.Visible = True
              Marks.Callout.Brush.Color = clBlack
              Marks.Callout.Arrow.Visible = True
              Marks.Callout.Length = 10
              Marks.Font.Height = -9
              Marks.Style = smsLabelPercent
              Marks.Visible = True
              Title = 'Count'
              XValues.Order = loAscending
              YValues.Name = 'Pie'
              YValues.Order = loDescending
              Frame.InnerBrush.BackColor = clRed
              Frame.InnerBrush.Gradient.EndColor = clGray
              Frame.InnerBrush.Gradient.MidColor = clWhite
              Frame.InnerBrush.Gradient.StartColor = 4210752
              Frame.InnerBrush.Gradient.Visible = True
              Frame.MiddleBrush.BackColor = clYellow
              Frame.MiddleBrush.Gradient.EndColor = 8553090
              Frame.MiddleBrush.Gradient.MidColor = clWhite
              Frame.MiddleBrush.Gradient.StartColor = clGray
              Frame.MiddleBrush.Gradient.Visible = True
              Frame.OuterBrush.BackColor = clGreen
              Frame.OuterBrush.Gradient.EndColor = 4210752
              Frame.OuterBrush.Gradient.MidColor = clWhite
              Frame.OuterBrush.Gradient.StartColor = clSilver
              Frame.OuterBrush.Gradient.Visible = True
              Frame.Visible = False
              Frame.Width = 4
              OtherSlice.Legend.Visible = False
              OtherSlice.Text = 'Other'
              OtherSlice.Value = 1.000000000000000000
              PiePen.Visible = False
            end
          end
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 614
            Height = 49
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label6: TLabel
              Left = 4
              Top = 4
              Width = 54
              Height = 13
              Caption = 'Weekdays:'
            end
            object Bevel1: TBevel
              Left = 0
              Top = 47
              Width = 614
              Height = 2
              Align = alBottom
              Shape = bsBottomLine
              ExplicitTop = 23
            end
            object Label7: TLabel
              Left = 4
              Top = 23
              Width = 26
              Height = 13
              Caption = 'Time:'
            end
            object laTicksFound: TLabel
              AlignWithMargins = True
              Left = 527
              Top = 3
              Width = 84
              Height = 13
              Align = alRight
              Caption = '%d ticks found'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clHighlight
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object ckThu: TExtendCheckBox
              Left = 301
              Top = 3
              Width = 75
              Height = 17
              Caption = 'ckThu'
              TabOrder = 0
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckWed: TExtendCheckBox
              Left = 215
              Top = 3
              Width = 77
              Height = 17
              Caption = 'ckWed'
              TabOrder = 1
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckMon: TExtendCheckBox
              Left = 64
              Top = 3
              Width = 75
              Height = 17
              Caption = 'ckMon'
              TabOrder = 2
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckTue: TExtendCheckBox
              Left = 143
              Top = 3
              Width = 66
              Height = 17
              Caption = 'ckTue'
              TabOrder = 3
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckFri: TExtendCheckBox
              Left = 380
              Top = 3
              Width = 75
              Height = 17
              Caption = 'ckFri'
              TabOrder = 4
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckTime1: TExtendCheckBox
              Left = 64
              Top = 23
              Width = 81
              Height = 17
              Caption = '02:00-07:59'
              TabOrder = 5
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckTime2: TExtendCheckBox
              Left = 153
              Top = 23
              Width = 81
              Height = 17
              Caption = '08:00-13:59'
              TabOrder = 6
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckTime3: TExtendCheckBox
              Left = 242
              Top = 23
              Width = 81
              Height = 17
              Caption = '14:00-19:59'
              TabOrder = 7
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
            object ckTime4: TExtendCheckBox
              Left = 332
              Top = 23
              Width = 81
              Height = 17
              Caption = '20:00-01:59'
              TabOrder = 8
              OnClick = ckMonClick
              FlatFontColor = clBlack
              
            end
          end
        end
      end
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 64
    Top = 120
  end
  inherited buSeparateWindow: TJvCaptionButton
    Left = 32
    Top = 120
  end
  inherited ilCaption: TImageList
    Left = 96
    Top = 120
    Bitmap = {
      494C01010100040014000A000A00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000280000000A00000001002000000000004006
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F0FB
      FF00F0FBFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F0FB
      FF00F0FBFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F0FB
      FF00F0FBFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F0FB
      FF00F0FBFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000280000000A0000000100010000000000500000000000000000000000
      000000000000000000000000FFFFFF00F7C0000000000000F7C0000000000000
      F7C000000000000080C0000000000000C1C0000000000000C1C0000000000000
      C1C0000000000000C1C0000000000000C1C0000000000000FFC0000000000000
      00000000000000000000000000000000000000000000}
  end
  object dsData: TDataSource
    Left = 256
    Top = 168
  end
end
