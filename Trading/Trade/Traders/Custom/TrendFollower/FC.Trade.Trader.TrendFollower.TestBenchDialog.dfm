inherited fmTrendFollowerTestBenchDialog: TfmTrendFollowerTestBenchDialog
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object paWorkspace: TPanel [2]
    Left = 0
    Top = 0
    Width = 529
    Height = 280
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
  end
  object PageControlEx1: TPageControlEx [3]
    Left = 0
    Top = 0
    Width = 529
    Height = 280
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 2
    
    ShowTabs = True
    object TabSheet1: TTabSheet
      Caption = 'Match by Intervals'
      object JvNetscapeSplitter2: TJvNetscapeSplitter
        Left = 149
        Top = 0
        Height = 252
        Hint = 'click me to have more '#13#10'space for the current demo'
        Align = alLeft
        MinSize = 10
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 159
        ExplicitHeight = 577
      end
      object paLeft: TPanel
        Left = 0
        Top = 0
        Width = 149
        Height = 252
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 13
          Align = alTop
          Caption = 'Time Intervals'
          ExplicitWidth = 66
        end
        object buStart: TButton
          Left = 8
          Top = 166
          Width = 75
          Height = 23
          Caption = 'Start'
          TabOrder = 0
          OnClick = buStartClick
        end
        object lbTimeIntervals: TCheckListBox
          Left = 0
          Top = 13
          Width = 149
          Height = 105
          Align = alTop
          ItemHeight = 13
          TabOrder = 1
        end
        object ckStopAfterEachRecord: TExtendCheckBox
          Left = 8
          Top = 143
          Width = 137
          Height = 17
          Caption = 'Stop after each record'
          TabOrder = 2
          FlatFontColor = clBlack
          
        end
        object buResume: TButton
          Left = 8
          Top = 191
          Width = 75
          Height = 23
          Caption = 'Resume'
          Enabled = False
          TabOrder = 3
          OnClick = buResumeClick
        end
        object buStop: TButton
          Left = 8
          Top = 220
          Width = 75
          Height = 23
          Caption = 'Stop'
          Enabled = False
          TabOrder = 4
          OnClick = buStopClick
        end
      end
      object grOrders: TEditDBGrid
        AlignWithMargins = True
        Left = 162
        Top = 3
        Width = 356
        Height = 246
        Align = alClient
        DataSource = dsTrendFollow
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = grOrdersDblClick
        OnBeforeDrawColumnCell = grOrdersBeforeDrawColumnCell
        Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
        Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
        ColorCheme.UnderCursor = clWindow
        ColorCheme.UnderCursorText = clWindowText
        ColorCheme.HighightCols = 13811126
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Match by symbols'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object JvNetscapeSplitter1: TJvNetscapeSplitter
        Left = 105
        Top = 0
        Height = 252
        Hint = 'click me to have more '#13#10'space for the current demo'
        Align = alLeft
        MinSize = 10
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 159
        ExplicitHeight = 577
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 105
        Height = 252
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          105
          252)
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 105
          Height = 13
          Align = alTop
          Caption = 'Hours'
          ExplicitWidth = 28
        end
        object laStatisticsMatches: TLabel
          Left = 0
          Top = 239
          Width = 105
          Height = 13
          Align = alBottom
          Caption = 'Match/Mism.'
          ExplicitWidth = 62
        end
        object laRecordCount: TLabel
          Left = 0
          Top = 226
          Width = 105
          Height = 13
          Align = alBottom
          Caption = 'Count'
          ExplicitWidth = 28
        end
        object buStart2: TButton
          Left = 0
          Top = 199
          Width = 75
          Height = 23
          Anchors = [akLeft, akBottom]
          Caption = 'Start'
          TabOrder = 0
          OnClick = buStart2Click
        end
        object lbHours: TCheckListBox
          Left = 2
          Top = 19
          Width = 97
          Height = 174
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object Panel2: TPanel
        Left = 115
        Top = 0
        Width = 406
        Height = 252
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object edFilter: TExtendComboBox
          Left = 0
          Top = 231
          Width = 406
          Height = 21
          Hint = 'Filter'
          Align = alBottom
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnKeyPress = edFilterKeyPress
          Items.Strings = (
            'sign(nvl(guess)) <> sign(nvl(fact))')
          
        end
        object EditDBGrid1: TEditDBGrid
          Left = 0
          Top = 0
          Width = 406
          Height = 231
          Align = alClient
          DataSource = dsTrendFollow2
          Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
          ReadOnly = True
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnDblClick = EditDBGrid1DblClick
          OnBeforeDrawColumnCell = EditDBGrid1BeforeDrawColumnCell
          Options1 = [dgFitColumnsToSize, dgWrapColumnTitles, dgSearchControls]
          Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
          ColorCheme.UnderCursor = clWindow
          ColorCheme.UnderCursorText = clWindowText
          ColorCheme.HighightCols = 13811126
        end
      end
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 64
    Top = 48
  end
  inherited ilCaption: TImageList
    Left = 96
    Top = 48
  end
  object taTrendFollow: TMemoryDataSet
    FieldDefs = <>
    Left = 304
    Top = 96
    object taTrendFollowNo: TIntegerField
      FieldName = 'No'
    end
    object taTrendFollowTime: TDateTimeField
      FieldName = 'Time'
    end
    object taTrendFollowClose: TFloatField
      FieldName = 'Close'
    end
    object taTrendFollowTrendDir: TFloatField
      FieldName = 'TrendDir'
    end
  end
  object dsTrendFollow: TDataSource
    DataSet = taTrendFollow
    Left = 360
    Top = 96
  end
  object taTrendFollow2: TMemoryDataSet
    FieldDefs = <
      item
        Name = 'No'
        DataType = ftInteger
      end
      item
        Name = 'Time'
        DataType = ftDateTime
      end
      item
        Name = 'EURUSD_H1_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'EURUSD_H4_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'GBPUSD_H1_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'GBPUSD_H4_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'USDCHF_H1_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'USDCHF_H4_PbSAR'
        DataType = ftFloat
      end
      item
        Name = 'EURUSD_H1_Acc'
        DataType = ftFloat
      end
      item
        Name = 'EURUSD_H4_Acc'
        DataType = ftFloat
      end
      item
        Name = 'GBPUSD_H1_Acc'
        DataType = ftFloat
      end
      item
        Name = 'GBPUSD_H4_Acc'
        DataType = ftFloat
      end
      item
        Name = 'USDCHF_H1_Acc'
        DataType = ftFloat
      end
      item
        Name = 'USDCHF_H4_Acc'
        DataType = ftFloat
      end>
    Left = 304
    Top = 136
    object taTrendFollow2No: TIntegerField
      DisplayWidth = 2
      FieldName = 'No'
    end
    object taTrendFollow2Time: TDateTimeField
      DisplayWidth = 10
      FieldName = 'Time'
      DisplayFormat = 'ddddd hh:mm'
    end
    object taTrendFollow2Price: TFloatField
      DisplayWidth = 5
      FieldName = 'Price'
      DisplayFormat = '###.0000'
    end
    object taTrendFollow2EURUSD_H1_PbSAR: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H1 PbSAR'
      DisplayWidth = 5
      FieldName = 'EURUSD_H1_PbSAR'
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2EURUSD_H1_Acc: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H1 Acc'
      DisplayWidth = 5
      FieldName = 'EURUSD_H1_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2EURUSD_H1_Fr: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H1 Fr'
      DisplayWidth = 5
      FieldName = 'EURUSD_H1_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H1_PbSAR: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H1 PbSAR'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H1_PbSAR'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H1_Acc: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H1 Acc'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H1_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H1_Fr: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H1 Fr'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H1_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H1_PbSAR: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H1 PbSAR'
      DisplayWidth = 5
      FieldName = 'USDCHF_H1_PbSAR'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H1_Acc: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H1 Acc'
      DisplayWidth = 5
      FieldName = 'USDCHF_H1_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H1_Fr: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H1 Fr'
      DisplayWidth = 5
      FieldName = 'USDCHF_H1_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2EURUSD_H4_PbSAR: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H4 PbSAR'
      DisplayWidth = 5
      FieldName = 'EURUSD_H4_PbSAR'
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2EURUSD_H4_Acc: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H4 Acc'
      DisplayWidth = 5
      FieldName = 'EURUSD_H4_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2EURUSD_H4_Fr: TFloatField
      Tag = 1
      DisplayLabel = 'EURUSD H4 Fr'
      DisplayWidth = 5
      FieldName = 'EURUSD_H4_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H4_PbSAR: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H4 PbSAR'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H4_PbSAR'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H4_Acc: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H4 Acc'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H4_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2GBPUSD_H4_Fr: TFloatField
      Tag = 1
      DisplayLabel = 'GBPUSD H4 Fr'
      DisplayWidth = 5
      FieldName = 'GBPUSD_H4_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H4_PbSAR: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H4 PbSAR'
      DisplayWidth = 5
      FieldName = 'USDCHF_H4_PbSAR'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H4_Acc: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H4 Acc'
      DisplayWidth = 5
      FieldName = 'USDCHF_H4_Acc'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2USDCHF_H4_Fr: TFloatField
      Tag = -1
      DisplayLabel = 'USDCHF H4 Fr'
      DisplayWidth = 5
      FieldName = 'USDCHF_H4_Fr'
      Visible = False
      DisplayFormat = '###'
      Precision = 2
    end
    object taTrendFollow2Guess: TFloatField
      Tag = 2
      FieldName = 'Guess'
      DisplayFormat = '###'
    end
    object taTrendFollow2ZigZag1H: TFloatField
      Tag = 2
      FieldName = 'ZigZag1H'
    end
    object taTrendFollow2ZigZag1HTime: TDateTimeField
      FieldName = 'ZigZag1HTime'
    end
    object taTrendFollow2ZigZag4H: TFloatField
      Tag = 2
      DisplayLabel = 'ZigZag (4H)'
      FieldName = 'ZigZag4H'
      DisplayFormat = '###'
    end
    object taTrendFollow2ZigZag4HTime: TDateTimeField
      FieldName = 'ZigZag4HTime'
    end
    object taTrendFollow2AtPbSARReverse: TFloatField
      Tag = 2
      DisplayLabel = 'AtPbSARReverse (4H)'
      FieldName = 'AtPbSARReverse'
      DisplayFormat = '###'
    end
  end
  object dsTrendFollow2: TDataSource
    DataSet = taTrendFollow2
    Left = 360
    Top = 136
  end
end
