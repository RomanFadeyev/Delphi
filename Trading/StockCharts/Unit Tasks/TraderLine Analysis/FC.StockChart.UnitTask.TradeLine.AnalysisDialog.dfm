inherited fmTradeLineAnalysisDialog: TfmTradeLineAnalysisDialog
  Left = 0
  Top = 189
  Caption = 'Analysis'
  ClientHeight = 265
  ClientWidth = 521
  ExplicitWidth = 529
  ExplicitHeight = 294
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 226
    Width = 521
    ExplicitTop = 269
    ExplicitWidth = 521
  end
  object JvNetscapeSplitter1: TJvNetscapeSplitter [1]
    Left = 201
    Top = 0
    Height = 226
    Hint = 'click me to have more '#13#10'space for the current demo'
    Align = alLeft
    MinSize = 10
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 257
    ExplicitTop = -131
    ExplicitHeight = 392
  end
  inherited buOK: TExtendButton
    Left = 440
    Top = 236
    ExplicitLeft = 440
    ExplicitTop = 236
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 0
    Width = 201
    Height = 226
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 3
      Width = 192
      Height = 13
      Margins.Left = 6
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Select indicator to compare with orders:'
      ExplicitWidth = 186
    end
    object Panel2: TPanel
      Left = 0
      Top = 168
      Width = 201
      Height = 58
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        201
        58)
      object buStart2: TButton
        Left = 110
        Top = 29
        Width = 85
        Height = 23
        Action = acStart
        Anchors = [akLeft, akBottom]
        TabOrder = 0
      end
      object buOpenOrder: TRadioButton
        Left = 4
        Top = 25
        Width = 83
        Height = 17
        Caption = 'Open Order'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
      object buCloseOrder: TRadioButton
        Left = 4
        Top = 40
        Width = 83
        Height = 17
        Caption = 'Close Order'
        TabOrder = 2
      end
      object ckSyncAllCharts: TExtendCheckBox
        Left = 4
        Top = 2
        Width = 98
        Height = 17
        BiDiMode = bdLeftToRight
        Caption = 'Sync all charts'
        Checked = True
        ParentBiDiMode = False
        State = cbChecked
        TabOrder = 3
        FlatFontColor = clBlack
        
      end
    end
    object lvIndicators: TExtendListView
      AlignWithMargins = True
      Left = 3
      Top = 19
      Width = 195
      Height = 146
      Align = alClient
      Columns = <>
      HideSelection = False
      ReadOnly = True
      ShowColumnHeaders = False
      SortType = stText
      TabOrder = 1
      ViewStyle = vsList
      OnDblClick = lvIndicatorsDblClick
      
      OnCreateItemClass = lvIndicatorsCreateItemClass
    end
  end
  object Panel3: TPanel [4]
    Left = 211
    Top = 0
    Width = 310
    Height = 226
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel3'
    TabOrder = 2
    object edFilter: TExtendComboBox
      Left = 0
      Top = 205
      Width = 310
      Height = 21
      Hint = 'Filter'
      Align = alBottom
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnKeyPress = edFilterKeyPress
      Items.Strings = (
        'Profit>0'
        'Profit<0')
      
    end
    object grReport: TEditDBGrid
      Left = 0
      Top = 0
      Width = 310
      Height = 205
      Align = alClient
      DataSource = DataSource1
      Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      ReadOnly = True
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = grReportDblClick
      OnBeforeDrawColumnCell = grReportBeforeDrawColumnCell
      Options1 = [dgFitColumnsToSize]
      Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
      ColorCheme.UnderCursor = clWindow
      ColorCheme.UnderCursorText = clWindowText
      ColorCheme.HighightCols = 13811126
    end
  end
  inherited lbDockClient: TJvDockClient
    Top = 232
  end
  inherited buSeparateWindow: TJvCaptionButton
    Top = 232
  end
  inherited ilCaption: TImageList
    Top = 232
  end
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 395
    Top = 72
    object taReportNo: TIntegerField
      FieldName = 'No'
    end
    object taReportOrderKind: TStringField
      DisplayLabel = 'Order Kind'
      FieldName = 'OrderKind'
      Size = 10
    end
    object taReportDateTime: TDateTimeField
      DisplayLabel = 'Open/Close DateTime'
      FieldName = 'DateTime'
    end
    object taReportPrice: TFloatField
      DisplayLabel = 'Open/Close Price'
      FieldName = 'Price'
      DisplayFormat = '0.0000'
    end
    object taReportProfit: TIntegerField
      DisplayLabel = 'Profit (pt)'
      FieldName = 'Profit'
    end
    object taReportIndicator: TFloatField
      FieldName = 'Indicator'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 363
    Top = 72
  end
  object alActions: TActionList
    Left = 376
    Top = 120
    object acStart: TAction
      Caption = 'Start'
      OnExecute = buStart2Click
      OnUpdate = acStartUpdate
    end
  end
end
