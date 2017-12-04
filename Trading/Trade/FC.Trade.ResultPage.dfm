object frmStockTradeResult: TfrmStockTradeResult
  Left = 0
  Top = 0
  Width = 718
  Height = 431
  AutoScroll = True
  TabOrder = 0
  TabStop = True
  object pcPages: TJvPageControl
    Left = 0
    Top = 22
    Width = 718
    Height = 393
    ActivePage = tsOperationHistory
    Align = alClient
    OwnerDraw = True
    TabOrder = 0
    OnChange = pcPagesChange
    ClientBorderWidth = 0
    HideAllTabs = True
    object tsOrderHistory: TTabSheet
      Caption = 'tsOrderHistory'
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 225
        Width = 718
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 169
        ExplicitWidth = 710
      end
      object grOrders: TEditDBGrid
        Left = 0
        Top = 0
        Width = 718
        Height = 225
        Align = alTop
        DataSource = dsOrder
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = pmGrid
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = grOrdersDblClick
        OnChangeRecord = grOrdersChangeRecord
        OnBeforeDrawColumnCell = grOrdersBeforeDrawColumnCell
        Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
        Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
        ColorCheme.UnderCursor = clWindow
        ColorCheme.UnderCursorText = clWindowText
        ColorCheme.HighightCols = 13811126
      end
      object grOrderDetails: TEditDBGrid
        Left = 0
        Top = 228
        Width = 718
        Height = 145
        Align = alClient
        DataSource = dsOrderDetails
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = grJournalDblClick
        OnBeforeDrawColumnCell = grOrderDetailsBeforeDrawColumnCell
        Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
        Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
        ColorCheme.UnderCursor = clWindow
        ColorCheme.UnderCursorText = clWindowText
        ColorCheme.HighightCols = 13811126
      end
    end
    object tsOperationHistory: TTabSheet
      Caption = 'tsOperationHistory'
      ImageIndex = 4
      object grOperationHistory: TEditDBGrid
        Left = 0
        Top = 0
        Width = 718
        Height = 373
        Align = alClient
        DataSource = dsOperationHistory
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = pmGrid
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = grOperationHistoryDblClick
        Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
        Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
        ColorCheme.UnderCursor = clWindow
        ColorCheme.UnderCursorText = clWindowText
        ColorCheme.HighightCols = 13811126
      end
    end
    object tsGraph: TTabSheet
      Caption = 'tsGraph'
      ImageIndex = 1
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object se: TChart
        Left = 0
        Top = 0
        Width = 718
        Height = 373
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.Visible = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        DepthAxis.Automatic = False
        DepthAxis.AutomaticMaximum = False
        DepthAxis.AutomaticMinimum = False
        DepthAxis.Maximum = 0.680000000000000000
        DepthAxis.Minimum = -0.319999999999999900
        DepthTopAxis.Automatic = False
        DepthTopAxis.AutomaticMaximum = False
        DepthTopAxis.AutomaticMinimum = False
        DepthTopAxis.Maximum = 0.680000000000000000
        DepthTopAxis.Minimum = -0.319999999999999900
        RightAxis.Automatic = False
        RightAxis.AutomaticMaximum = False
        RightAxis.AutomaticMinimum = False
        RightAxis.ExactDateTime = False
        RightAxis.Title.Caption = 'Balance'
        View3D = False
        Align = alClient
        BevelOuter = bvNone
        Color = clWindow
        TabOrder = 0
        object chartBalance: TFastLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          AutoRepaint = False
          LinePen.Color = clRed
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
    object tsEquity: TTabSheet
      Caption = 'tsEquity'
      ImageIndex = 3
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object chEquity: TChart
        Left = 0
        Top = 0
        Width = 718
        Height = 373
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Legend.Visible = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.Increment = 1.000000000000000000
        DepthAxis.Automatic = False
        DepthAxis.AutomaticMaximum = False
        DepthAxis.AutomaticMinimum = False
        DepthAxis.Maximum = 0.680000000000000000
        DepthAxis.Minimum = -0.319999999999999900
        DepthTopAxis.Automatic = False
        DepthTopAxis.AutomaticMaximum = False
        DepthTopAxis.AutomaticMinimum = False
        DepthTopAxis.Maximum = 0.680000000000000000
        DepthTopAxis.Minimum = -0.319999999999999900
        RightAxis.Automatic = False
        RightAxis.AutomaticMaximum = False
        RightAxis.AutomaticMinimum = False
        RightAxis.ExactDateTime = False
        RightAxis.Title.Caption = 'Balance'
        View3D = False
        Align = alClient
        BevelOuter = bvNone
        Color = clWindow
        TabOrder = 0
        object seEquity: TFastLineSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Visible = False
          AutoRepaint = False
          LinePen.Color = clRed
          XValues.DateTime = True
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Y'
          YValues.Order = loNone
        end
      end
    end
    object tsJournal: TTabSheet
      Caption = 'tsJournal'
      ImageIndex = 2
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grJournal: TEditDBGrid
        Left = 0
        Top = 0
        Width = 718
        Height = 373
        Align = alClient
        DataSource = dsOrderDetails
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = grJournalDblClick
        OnBeforeDrawColumnCell = grOrderDetailsBeforeDrawColumnCell
        Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
        Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
        ColorCheme.UnderCursor = clWindow
        ColorCheme.UnderCursorText = clWindowText
        ColorCheme.HighightCols = 13811126
      end
    end
  end
  object tcTabs: TFlatTabControl
    Left = 0
    Top = 415
    Width = 718
    Height = 16
    TabIndex = 0
    Tabs = <
      item
        Caption = 'Order History'
      end
      item
        Caption = 'Operation History'
      end
      item
        Caption = 'Balance History'
      end
      item
        Caption = 'Equity Day History'
      end
      item
        Caption = 'Message Board'
      end>
    Flat = False
    DrawTopLine = False
    Align = alBottom
    Color = clBtnFace
    ParentColor = False
    TabOrder = 1
    OnChange = tcTabsChange
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 718
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object laBalance: TLabel
      Left = 528
      Top = 0
      Width = 190
      Height = 22
      Align = alRight
      AutoSize = False
      Caption = 'Current balance:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
    end
    object laElapsedTime: TLabel
      Left = 400
      Top = 0
      Width = 128
      Height = 22
      Align = alRight
      AutoSize = False
      Caption = 'Elapsed Time:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
    end
    object ToolBar1: TToolBar
      Left = 0
      Top = 0
      Width = 400
      Height = 22
      Align = alClient
      AutoSize = True
      Images = fmUIDataStorage.ilMain
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object buExportOrders: TToolButton
        Left = 0
        Top = 0
        Action = acExportOrders
      end
      object buGetStatistic: TToolButton
        Left = 23
        Top = 0
        Action = acGetStatistic
      end
      object ToolButton2: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 10
        Style = tbsSeparator
      end
      object buAutoScroll: TToolButton
        Left = 54
        Top = 0
        Action = acAutoScroll
      end
      object ToolButton1: TToolButton
        Left = 77
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 17
        Style = tbsSeparator
      end
      object ToolButton3: TToolButton
        Left = 85
        Top = 0
        Action = acFilter
      end
    end
  end
  object acActions: TActionList
    Images = fmUIDataStorage.ilMain
    Left = 360
    Top = 288
    object acGetStatistic: TAction
      Category = 'Result'
      Caption = 'Get Statistic'
      Hint = 'Get Statistic'
      ImageIndex = 8
      OnExecute = acGetStatisticExecute
      OnUpdate = acGetStatisticUpdate
    end
    object acExportOrders: TAction
      Category = 'Result'
      Caption = 'Export Orders'
      Hint = 'Export Orders'
      ImageIndex = 7
      OnExecute = acExportOrdersExecute
      OnUpdate = acExportOrdersUpdate
    end
    object acChooseColumns: TAction
      Category = 'Grid'
      Caption = 'Choose Columns'
      OnExecute = acChooseColumnsExecute
      OnUpdate = acChooseColumnsUpdate
    end
    object acFitColumns: TAction
      Category = 'Grid'
      Caption = 'Fit Columns'
      OnExecute = acFitColumnsExecute
      OnUpdate = acChooseColumnsUpdate
    end
    object acAutoScroll: TAction
      Category = 'Grid'
      Caption = 'Auto Scroll'
      Checked = True
      Hint = 'Auto Scroll'
      ImageIndex = 16
      OnExecute = acAutoScrollExecute
    end
    object acFilter: TAction
      Category = 'Result'
      Caption = 'Filter results'
      Hint = 'Filter results'
      ImageIndex = 29
      OnExecute = acFilterExecute
    end
    object acHilightOnCharts: TAction
      Category = 'Locate'
      Caption = 'Hilight on Charts'
      OnExecute = grOrdersDblClick
    end
  end
  object taOrders: TMemoryDataSet
    FieldDefs = <>
    AfterOpen = taOrdersAfterOpen
    BeforeClose = taOrdersBeforeClose
    Left = 304
    Top = 96
    object taOrdersNo: TIntegerField
      DisplayLabel = '#'
      DisplayWidth = 3
      FieldName = 'No'
    end
    object taOrdersCreateTime: TDateTimeField
      DisplayLabel = 'Create Time'
      FieldName = 'CreateTime'
    end
    object taOrdersType: TStringField
      DisplayWidth = 4
      FieldName = 'Type'
    end
    object taOrdersNotes: TStringField
      DisplayWidth = 10
      FieldName = 'Notes'
      Size = 255
    end
    object taOrdersOpenTime: TDateTimeField
      DisplayLabel = 'Open Time'
      FieldName = 'OpenTime'
      OnGetText = taOrdersOpenTimeGetText
    end
    object taOrdersOpenBarNo: TStringField
      DisplayLabel = '#Bar'
      DisplayWidth = 15
      FieldName = 'OpenBarNo'
      Size = 64
    end
    object taOrdersOpenPrice: TCurrencyField
      DisplayLabel = 'Open Price'
      DisplayWidth = 6
      FieldName = 'OpenPrice'
      DisplayFormat = '0.0000'
      currency = False
    end
    object taOrdersOpenComment: TStringField
      DisplayLabel = 'Open Comment'
      DisplayWidth = 30
      FieldName = 'OpenComment'
      Size = 255
    end
    object taOrdersCloseTime: TDateTimeField
      DisplayLabel = 'Close Time'
      FieldName = 'CloseTime'
      OnGetText = taOrdersOpenTimeGetText
    end
    object taOrdersCloseBarNo: TStringField
      DisplayLabel = '#Bar'
      DisplayWidth = 15
      FieldName = 'CloseBarNo'
      Size = 64
    end
    object taOrdersClosePrice: TCurrencyField
      DisplayLabel = 'Close Price'
      DisplayWidth = 6
      FieldName = 'ClosePrice'
      DisplayFormat = '0.0000'
    end
    object taOrdersCloseComment: TStringField
      DisplayLabel = 'Close Comment'
      DisplayWidth = 30
      FieldName = 'CloseComment'
      Size = 255
    end
    object taOrdersProfitPt: TIntegerField
      DisplayLabel = 'Profit(Pt)'
      DisplayWidth = 4
      FieldName = 'ProfitPt'
    end
    object taOrdersBestPt: TIntegerField
      DisplayLabel = 'Best(Pt)'
      DisplayWidth = 3
      FieldName = 'BestPt'
    end
    object taOrdersQuality: TIntegerField
      DisplayWidth = 3
      FieldName = 'Quality'
    end
    object taOrdersWorstPt: TIntegerField
      DisplayLabel = 'Worst(Pt)'
      DisplayWidth = 4
      FieldName = 'WorstPt'
    end
    object taOrdersSubsidencePt: TIntegerField
      DisplayLabel = 'Subsidence(Pt)'
      FieldName = 'SubsidencePt'
    end
    object taOrdersLots: TFloatField
      DisplayWidth = 2
      FieldName = 'Lots'
      DisplayFormat = '#.00'
    end
    object taOrdersProfit: TCurrencyField
      DisplayWidth = 4
      FieldName = 'Profit,$'
      DisplayFormat = '0.'
      currency = False
    end
    object taOrdersBalance: TCurrencyField
      DisplayWidth = 6
      FieldName = 'Balance'
      DisplayFormat = '#.'
      currency = False
    end
    object taOrdersOrderID: TStringField
      FieldName = 'OrderID'
      Visible = False
      Size = 40
    end
    object taOrdersDetailNo: TIntegerField
      FieldName = 'DetailNo'
      Visible = False
    end
    object taOrdersStopLoss: TCurrencyField
      FieldName = 'StopLoss'
      Visible = False
      DisplayFormat = '0.0000'
    end
    object taOrdersTakeProfit: TCurrencyField
      FieldName = 'TakeProfit'
      Visible = False
      DisplayFormat = '0.0000'
    end
    object taOrdersTrailingStop: TCurrencyField
      FieldName = 'TrailingStop'
      Visible = False
      DisplayFormat = '0.0000'
    end
    object taOrdersPendingOpenPrice: TCurrencyField
      DisplayLabel = 'Pending Open Price'
      FieldName = 'PendingOpenPrice'
      Visible = False
    end
    object taOrdersColor: TIntegerField
      FieldName = 'Color'
      Visible = False
    end
    object taOrdersState: TIntegerField
      FieldName = 'State'
      Visible = False
    end
  end
  object dsOrder: TDataSource
    DataSet = taOrders
    Left = 360
    Top = 96
  end
  object pmGrid: TPopupActionBar
    Left = 264
    Top = 96
    object Hilightoncharts1: TMenuItem
      Action = acHilightOnCharts
      Default = True
    end
    object Gotothebeginning1: TMenuItem
      Caption = 'Hilight Beginning Only'
      OnClick = Gotothebeginning1Click
    end
    object Gotoend1: TMenuItem
      Caption = 'Hilight End Only'
      OnClick = Gotoend1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miAutoScroll: TMenuItem
      Action = acAutoScroll
    end
    object FitColumns1: TMenuItem
      Action = acFitColumns
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ChooseColumns1: TMenuItem
      Action = acChooseColumns
    end
  end
  object taOrderDetails: TMemoryDataSet
    Filtered = True
    FieldDefs = <>
    AfterOpen = taOrderDetailsAfterOpen
    BeforeClose = taOrderDetailsBeforeClose
    OnFilterRecord = taOrderDetailsFilterRecord
    Left = 304
    Top = 136
    object taOrderDetailsNo: TIntegerField
      FieldName = 'No'
      Visible = False
    end
    object taOrderDetailsNoInGroup: TIntegerField
      DisplayLabel = '#'
      DisplayWidth = 3
      FieldName = 'NoInGroup'
    end
    object taOrderDetailsOrderNo: TIntegerField
      DisplayLabel = 'Order#'
      FieldName = 'OrderNo'
      Visible = False
    end
    object taOrderDetailsOrderID: TStringField
      FieldName = 'OrderID'
      Visible = False
      Size = 40
    end
    object taOrderDetailsWhen: TDateTimeField
      FieldName = 'Time'
    end
    object taOrderDetailsPriceAsk: TCurrencyField
      DisplayLabel = 'Ask'
      DisplayWidth = 5
      FieldName = 'PriceAsk'
      currency = False
    end
    object taOrderDetailsPriceBid: TCurrencyField
      DisplayLabel = 'Bid'
      DisplayWidth = 5
      FieldName = 'PriceBid'
      currency = False
    end
    object taOrderDetailsText: TStringField
      DisplayWidth = 100
      FieldName = 'Text'
      Size = 1024
    end
    object taOrderDetailsColor: TIntegerField
      FieldName = 'Color'
      Visible = False
    end
  end
  object dsOrderDetails: TDataSource
    DataSet = taOrderDetails
    Left = 360
    Top = 136
  end
  object tmFilterDetails: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmFilterDetailsTimer
    Left = 464
    Top = 176
  end
  object taOperationHistory: TMemoryDataSet
    FieldDefs = <>
    AfterOpen = taOperationHistoryAfterOpen
    BeforeClose = taOperationHistoryBeforeClose
    Left = 504
    Top = 312
    object taOperationHistoryNo: TIntegerField
      DisplayLabel = #8470
      FieldName = 'No'
    end
    object taOperationHistoryTime: TDateTimeField
      FieldName = 'Time'
    end
    object taOperationHistoryType: TStringField
      FieldName = 'Type'
    end
    object taOperationHistoryOrderNo: TIntegerField
      DisplayLabel = 'Order'
      FieldName = 'OrderNo'
    end
    object taOperationHistoryLots: TFloatField
      FieldName = 'Lots'
    end
    object taOperationHistoryPrice: TCurrencyField
      FieldName = 'Price'
      DisplayFormat = '0.0000'
    end
    object taOperationHistoryStopLoss: TCurrencyField
      DisplayLabel = 'Stop Loss'
      FieldName = 'StopLoss'
      DisplayFormat = '0.0000'
    end
    object taOperationHistoryTakeProfit: TCurrencyField
      DisplayLabel = 'Take Profit'
      FieldName = 'TakeProfit'
      DisplayFormat = '0.0000'
    end
    object taOperationHistoryProfit: TCurrencyField
      FieldName = 'Profit,$'
    end
    object taOperationHistoryBalance: TCurrencyField
      FieldName = 'Balance'
    end
  end
  object dsOperationHistory: TDataSource
    DataSet = taOperationHistory
    Left = 576
    Top = 312
  end
end
