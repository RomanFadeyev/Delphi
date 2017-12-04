inherited fmJerkTimeTestBenchDialog: TfmJerkTimeTestBenchDialog
  ClientHeight = 349
  ExplicitWidth = 320
  ExplicitHeight = 378
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 310
    ExplicitTop = 310
  end
  inherited buOK: TExtendButton
    Top = 320
    ExplicitTop = 320
  end
  object paWorkspace: TPanel [2]
    Left = 0
    Top = 0
    Width = 529
    Height = 310
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    ExplicitHeight = 280
    object PageControlEx1: TPageControlEx
      Left = 4
      Top = 4
      Width = 521
      Height = 302
      ActivePage = tsTrendFollow
      Align = alClient
      TabOrder = 0
      
      ShowTabs = True
      ExplicitHeight = 272
      object tsTrendFollow: TTabSheet
        Caption = 'Trend Follow'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 244
        object grOrders: TEditDBGrid
          Left = 0
          Top = 0
          Width = 513
          Height = 216
          Align = alClient
          DataSource = dsTrendFollow
          Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
          ReadOnly = True
          TabOrder = 0
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
        object Panel1: TPanel
          Left = 0
          Top = 216
          Width = 513
          Height = 58
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object buStart: TButton
            Left = 8
            Top = 29
            Width = 75
            Height = 23
            Caption = 'Start'
            TabOrder = 0
            OnClick = buStartClick
          end
          object ckStopAfterEachRecord: TExtendCheckBox
            Left = 8
            Top = 6
            Width = 137
            Height = 17
            Caption = 'Stop after each record'
            TabOrder = 1
            FlatFontColor = clBlack
            
          end
          object buResume: TButton
            Left = 89
            Top = 29
            Width = 75
            Height = 23
            Caption = 'Resume'
            Enabled = False
            TabOrder = 2
            OnClick = buResumeClick
          end
          object buStop: TButton
            Left = 178
            Top = 29
            Width = 75
            Height = 23
            Caption = 'Stop'
            Enabled = False
            TabOrder = 3
            OnClick = buStopClick
          end
        end
      end
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 64
    Top = 208
  end
  inherited ilCaption: TImageList
    Left = 96
    Top = 208
  end
  object taTrendFollow: TMemoryDataSet
    FieldDefs = <>
    Left = 304
    Top = 96
    object taTrendFollowTime: TDateTimeField
      FieldName = 'Time'
    end
    object taTrendFollowPbSAR5m: TFloatField
      FieldName = 'PbSAR5m'
    end
    object taTrendFollowPbSAR15m: TFloatField
      FieldName = 'PbSAR15m'
    end
    object taTrendFollowPbSAR60m: TFloatField
      FieldName = 'PbSAR60m'
    end
    object taTrendFollowFractals5m: TIntegerField
      FieldName = 'Fractals5m'
    end
    object taTrendFollowFractals15m: TIntegerField
      FieldName = 'Fractals15m'
    end
    object taTrendFollowFractals60m: TIntegerField
      FieldName = 'Fractals60m'
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
end
