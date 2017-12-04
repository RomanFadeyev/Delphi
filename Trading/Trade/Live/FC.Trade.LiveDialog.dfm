inherited fmTradeLiveDialog: TfmTradeLiveDialog
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Live Trading'
  ClientHeight = 395
  ClientWidth = 823
  Constraints.MinHeight = 350
  Constraints.MinWidth = 600
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 831
  ExplicitHeight = 424
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 356
    Width = 823
    ExplicitTop = 356
    ExplicitWidth = 823
  end
  inherited buOK: TExtendButton
    Left = 742
    Top = 366
    ExplicitLeft = 742
    ExplicitTop = 366
  end
  object pcPages: TPageControl [2]
    Left = 0
    Top = 0
    Width = 823
    Height = 356
    ActivePage = tsStart
    Align = alClient
    TabOrder = 1
    object tsStart: TTabSheet
      Caption = 'Start'
      object Label1: TLabel
        Left = 0
        Top = 8
        Width = 35
        Height = 13
        Caption = 'Experts'
      end
      object lvTraders: TExtendListView
        Left = 0
        Top = 24
        Width = 385
        Height = 150
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        
      end
      object buStartTesting: TButton
        Left = 0
        Top = 188
        Width = 75
        Height = 23
        Action = acStartTrading
        TabOrder = 1
      end
      object Button1: TButton
        Left = 391
        Top = 24
        Width = 89
        Height = 23
        Action = acTraderProperties
        TabOrder = 2
      end
    end
    object tsResults: TTabSheet
      Caption = 'Results'
      ImageIndex = 1
      inline frmStockTradeResult: TfrmStockTradeResult
        Left = 0
        Top = 0
        Width = 815
        Height = 328
        Align = alClient
        AutoScroll = True
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 815
        ExplicitHeight = 328
        inherited pcPages: TJvPageControl
          Width = 815
          Height = 290
          ExplicitWidth = 815
          ExplicitHeight = 290
          inherited tsOrderHistory: TTabSheet
            ExplicitTop = 0
            ExplicitWidth = 815
            ExplicitHeight = 270
            inherited Splitter1: TSplitter
              Width = 815
              ExplicitWidth = 815
            end
            inherited grOrders: TEditDBGrid
              Width = 815
              TitleFont.Name = 'MS Sans Serif'
            end
            inherited grOrderDetails: TEditDBGrid
              Width = 815
              Height = 42
              TitleFont.Name = 'MS Sans Serif'
            end
          end
          inherited tsGraph: TTabSheet
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          inherited tsEquity: TTabSheet
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          inherited tsJournal: TTabSheet
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            inherited grJournal: TEditDBGrid
              TitleFont.Name = 'MS Sans Serif'
            end
          end
        end
        inherited tcTabs: TFlatTabControl
          Top = 312
          Width = 815
          ExplicitTop = 312
          ExplicitWidth = 815
        end
        inherited Panel1: TPanel
          Width = 815
          ExplicitWidth = 815
          inherited laBalance: TLabel
            Left = 625
            ExplicitLeft = 625
          end
          inherited laElapsedTime: TLabel
            Left = 497
            ExplicitLeft = 497
          end
          inherited ToolBar1: TToolBar
            Width = 497
            ExplicitWidth = 497
          end
        end
      end
    end
  end
  object paTrading: TPanel [3]
    Left = 4
    Top = 364
    Width = 429
    Height = 25
    Anchors = [akLeft, akBottom]
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    Visible = False
    object laProcess: TPanel
      Left = 0
      Top = 0
      Width = 209
      Height = 25
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Live Trading Enabled'
      Color = clSkyBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
    end
    object buStop: TButton
      Left = 214
      Top = 0
      Width = 75
      Height = 23
      Caption = 'Stop'
      TabOrder = 0
      OnClick = buStopClick
    end
  end
  object ActionList1: TActionList
    Images = fmUIDataStorage.ilMain
    Left = 360
    Top = 288
    object acStartTrading: TAction
      Caption = 'Start Trading'
      OnExecute = acStartTradingExecute
      OnUpdate = acStartTradingUpdate
    end
    object acTraderProperties: TAction
      Caption = 'Properties'
      OnExecute = acTraderPropertiesExecute
      OnUpdate = acTraderPropertiesUpdate
    end
    object acGetStatistic: TAction
      Category = 'Result'
      Caption = 'Get Statistic'
      Hint = 'Get Statistic'
      ImageIndex = 8
    end
    object acExportOrders: TAction
      Category = 'Result'
      Caption = 'Export Orders'
      Hint = 'Export Orders'
      ImageIndex = 7
    end
    object acCaptionButton: TAction
      Caption = 'Separate Window'
      ImageIndex = 2
    end
  end
  object tmRefresh: TTimer
    Enabled = False
    Left = 264
    Top = 240
  end
end
