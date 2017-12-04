inherited fmTradeTesterDialog: TfmTradeTesterDialog
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Trade Tester'
  ClientHeight = 426
  ClientWidth = 585
  Constraints.MinHeight = 350
  Constraints.MinWidth = 570
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 593
  ExplicitHeight = 455
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 387
    Width = 585
    ExplicitTop = 356
    ExplicitWidth = 823
  end
  inherited buOK: TExtendButton
    Left = 504
    Top = 397
    ExplicitLeft = 504
    ExplicitTop = 397
  end
  object paWorkspace: TPanel [2]
    Left = 0
    Top = 0
    Width = 585
    Height = 387
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    FullRepaint = False
    TabOrder = 2
    object pcPages: TPageControl
      Left = 4
      Top = 4
      Width = 577
      Height = 379
      ActivePage = tsStart
      Align = alClient
      TabOrder = 0
      object tsStart: TTabSheet
        Caption = 'Start'
        object ScrollBox1: TScrollBox
          Left = 0
          Top = 0
          Width = 569
          Height = 351
          Align = alClient
          BorderStyle = bsNone
          ParentBackground = True
          TabOrder = 0
          object Label1: TLabel
            Left = 8
            Top = 8
            Width = 35
            Height = 13
            Caption = 'Experts'
          end
          object Label2: TLabel
            Left = 320
            Top = 83
            Width = 71
            Height = 13
            Caption = 'Spread (points)'
          end
          object Label3: TLabel
            Left = 320
            Top = 136
            Width = 78
            Height = 13
            Caption = 'Slippage (points)'
            Enabled = False
          end
          object Label4: TLabel
            Left = 0
            Top = 192
            Width = 48
            Height = 13
            Caption = 'Start From'
          end
          object Label5: TLabel
            Left = 320
            Top = 164
            Width = 63
            Height = 13
            Caption = 'Initial Deposit'
          end
          object Label6: TLabel
            Left = 0
            Top = 216
            Width = 35
            Height = 13
            Caption = 'Stop At'
          end
          object Label7: TLabel
            Left = 320
            Top = 56
            Width = 60
            Height = 13
            Caption = 'Getting price'
          end
          object Label8: TLabel
            Left = 320
            Top = 108
            Width = 88
            Height = 13
            Caption = 'Stop Level (points)'
          end
          object Bevel1: TBevel
            Left = 319
            Top = 304
            Width = 217
            Height = 7
            Shape = bsTopLine
          end
          object Bevel2: TBevel
            Left = 319
            Top = 256
            Width = 217
            Height = 7
            Shape = bsTopLine
          end
          object buFindStartDate: TSpeedButton
            Left = 279
            Top = 187
            Width = 23
            Height = 22
            Hint = 'Find first possible date to test'
            Flat = True
            Glyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              0800000000000001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
              FDFDFDFDFDFDF59BEDFDFDFDFDFDFDFDFDFDFDFDFDED5A665CFDFDFDFDFDFDFD
              FDFDFDFDED5A6EBFADFDFDFDFDFDFDFDFDFDFDED5A6EBFADFDFDFDFD09EDA39A
              9AA3F7526EBFADFDFDFDFDF59BA3ECECECA39A65BFADFDFDFDFD099BEC090909
              09F4E39AADFDFDFDFDFDEDEC090909090909ECA39BFDFDFDFDFDF7F509F6F6F6
              090909EC9BFDFDFDFDFDA408F6FFFFFFF60909EC9AFDFDFDFDFDED07FFFFFFFF
              F60909EC9AFDFDFDFDFD09F5F6FFFFFFF60909A3ECFDFDFDFDFD09ED07F6FFF6
              0909EC9A09FDFDFDFDFDFD09ED070708F5EC9BEDFDFDFDFDFDFDFDFD0907EDED
              E4EC09FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD}
            ParentShowHint = False
            ShowHint = True
            OnClick = buFindStartDateClick
          end
          object buFindStopDate: TSpeedButton
            Left = 279
            Top = 211
            Width = 23
            Height = 22
            Hint = 'Find last possible date to test'
            Flat = True
            Glyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              0800000000000001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFDFDFDFDFD
              FDFDFDFDFDFDF59BEDFDFDFDFDFDFDFDFDFDFDFDFDED5A665CFDFDFDFDFDFDFD
              FDFDFDFDED5A6EBFADFDFDFDFDFDFDFDFDFDFDED5A6EBFADFDFDFDFD09EDA39A
              9AA3F7526EBFADFDFDFDFDF59BA3ECECECA39A65BFADFDFDFDFD099BEC090909
              09F4E39AADFDFDFDFDFDEDEC090909090909ECA39BFDFDFDFDFDF7F509F6F6F6
              090909EC9BFDFDFDFDFDA408F6FFFFFFF60909EC9AFDFDFDFDFDED07FFFFFFFF
              F60909EC9AFDFDFDFDFD09F5F6FFFFFFF60909A3ECFDFDFDFDFD09ED07F6FFF6
              0909EC9A09FDFDFDFDFDFD09ED070708F5EC9BEDFDFDFDFDFDFDFDFD0907EDED
              E4EC09FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD}
            OnClick = buFindStopDateClick
          end
          object buShowRecentlyStartDates: TSpeedButton
            Left = 264
            Top = 188
            Width = 14
            Height = 21
            Flat = True
            Glyph.Data = {
              F6000000424DF60000000000000076000000280000000A000000100000000100
              04000000000080000000C40E0000C40E00001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
              0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
            OnClick = buShowRecentlyStartDatesClick
          end
          object buShowRecentlyStopDates: TSpeedButton
            Left = 264
            Top = 212
            Width = 14
            Height = 21
            Flat = True
            Glyph.Data = {
              F6000000424DF60000000000000076000000280000000A000000100000000100
              04000000000080000000C40E0000C40E00001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDD0DDDDD000000DDD000DDDD00
              0000DD00000DDD000000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD00
              0000DDDDDDDDDD000000DDDDDDDDDD000000DDDDDDDDDD000000}
            OnClick = buShowRecentlyStopDatesClick
          end
          object lvTraders: TExtendListView
            Left = 0
            Top = 24
            Width = 313
            Height = 150
            Columns = <
              item
                Caption = 'Name'
                Width = 146
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            SortType = stText
            TabOrder = 0
            ViewStyle = vsReport
            
          end
          object edSpread: TExtendSpinEdit
            Left = 415
            Top = 80
            Width = 121
            Height = 21
            Hint = 'Distance between bid and ask price'
            MaxValue = 0
            MinValue = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object edSlippage: TExtendSpinEdit
            Left = 415
            Top = 132
            Width = 121
            Height = 21
            Enabled = False
            MaxValue = 0
            MinValue = 0
            TabOrder = 2
          end
          object edStartFromDate: TExtendDateTimePicker
            Left = 54
            Top = 185
            Width = 129
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            TabOrder = 8
            TransparentBorder = False
            
          end
          object edStartFromTime: TExtendDateTimePicker
            Left = 190
            Top = 188
            Width = 73
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            Kind = dtkTime
            TabOrder = 4
            TransparentBorder = False
            
          end
          object buStartTesting: TButton
            Left = 0
            Top = 244
            Width = 75
            Height = 23
            Action = acStartTesting
            TabOrder = 5
          end
          object buStartTraining: TButton
            Left = 405
            Top = 24
            Width = 75
            Height = 23
            Action = acStartTraining
            TabOrder = 3
          end
          object cbInitialDeposit: TExtendComboBox
            Left = 415
            Top = 161
            Width = 121
            Height = 21
            Style = csDropDownList
            TabOrder = 6
            
          end
          object Button1: TButton
            Left = 319
            Top = 24
            Width = 75
            Height = 23
            Action = acTraderProperties
            TabOrder = 7
          end
          object edStopAtDate: TExtendDateTimePicker
            Left = 54
            Top = 212
            Width = 129
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            TabOrder = 9
            TransparentBorder = False
            
          end
          object edStopAtTime: TExtendDateTimePicker
            Left = 190
            Top = 212
            Width = 73
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            Kind = dtkTime
            TabOrder = 10
            TransparentBorder = False
            
          end
          object cbGettingPriceType: TExtendComboBox
            Left = 415
            Top = 53
            Width = 121
            Height = 21
            Style = csDropDownList
            TabOrder = 11
            
          end
          object ckUpdateCharts: TExtendCheckBox
            Left = 319
            Top = 235
            Width = 217
            Height = 17
            Hint = 
              'Displays changes of each bar as if it is real trading. Slows tes' +
              't process.'
            Caption = 'Display every bar updating (slow)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 12
            FlatFontColor = clBlack
            
          end
          object ckPauseAfterOpening: TExtendCheckBox
            Left = 319
            Top = 192
            Width = 217
            Height = 17
            Hint = 
              'Stops if an order was opened, it allows you to examine the situa' +
              'tion immediately'
            Caption = 'Pause after each opening order '
            ParentShowHint = False
            ShowHint = True
            TabOrder = 13
            FlatFontColor = clBlack
            
          end
          object ckPauseAfterClosing: TExtendCheckBox
            Left = 319
            Top = 213
            Width = 217
            Height = 17
            Hint = 
              'Stops if an order was closed, it allows you to examine the situa' +
              'tion immediately'
            Caption = 'Pause after each closing order '
            ParentShowHint = False
            ShowHint = True
            TabOrder = 14
            FlatFontColor = clBlack
            
          end
          object ckUpdateOverOldBars: TExtendCheckBox
            Left = 319
            Top = 310
            Width = 217
            Height = 17
            Hint = 
              'If checked, bars are generated just over old ones, otherwise old' +
              ' bars will be deleted'
            Caption = 'Update bars over old ones'
            Enabled = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 15
            FlatFontColor = clBlack
            
          end
          object ckShiftBars: TExtendCheckBox
            Left = 319
            Top = 331
            Width = 217
            Height = 17
            Caption = 'Shift bar align'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 16
            FlatFontColor = clBlack
            
          end
          object ckGenerateFullBarsOnly: TExtendCheckBox
            Left = 319
            Top = 261
            Width = 217
            Height = 17
            Hint = 
              'If checked, only full bars are generated. It'#39's faster than every' +
              ' tick generation but more rough. Some experts may misbehave'
            Caption = 'Generate full bars only (fast)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 17
            OnClick = ckGenerateFullBarsOnlyClick
            FlatFontColor = clBlack
            
          end
          object edStopLevel: TExtendSpinEdit
            Left = 414
            Top = 105
            Width = 121
            Height = 21
            Hint = 
              'The minimal distance between stop levels (StopLoss and TakeProfi' +
              't levels) and market price in points '
            MaxValue = 0
            MinValue = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 18
          end
          object ckEmulateTicks: TExtendCheckBox
            Left = 319
            Top = 284
            Width = 217
            Height = 17
            Hint = 'Allows you to approach almost real market conditions'
            Caption = 'Emulate ticks (slow)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 19
            OnClick = ckEmulateTicksClick
            FlatFontColor = clBlack
            
          end
        end
      end
      object tsResults: TTabSheet
        Caption = 'Results'
        inline frmStockTradeResult: TfrmStockTradeResult
          Left = 0
          Top = 0
          Width = 569
          Height = 351
          Align = alClient
          AutoScroll = True
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 569
          ExplicitHeight = 351
          inherited pcPages: TJvPageControl
            Width = 569
            Height = 313
            ExplicitWidth = 569
            ExplicitHeight = 313
            inherited tsOrderHistory: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 718
              ExplicitHeight = 373
              inherited Splitter1: TSplitter
                ExplicitWidth = 569
              end
              inherited grOrders: TEditDBGrid
                TitleFont.Name = 'MS Sans Serif'
              end
              inherited grOrderDetails: TEditDBGrid
                TitleFont.Name = 'MS Sans Serif'
              end
            end
            inherited tsOperationHistory: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 569
              ExplicitHeight = 293
              inherited grOperationHistory: TEditDBGrid
                Width = 569
                Height = 293
                TitleFont.Name = 'MS Sans Serif'
              end
            end
            inherited tsGraph: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 718
              ExplicitHeight = 373
            end
            inherited tsEquity: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 718
              ExplicitHeight = 373
            end
            inherited tsJournal: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 718
              ExplicitHeight = 373
              inherited grJournal: TEditDBGrid
                TitleFont.Name = 'MS Sans Serif'
              end
            end
          end
          inherited tcTabs: TFlatTabControl
            Top = 335
            Width = 569
            ExplicitTop = 335
            ExplicitWidth = 569
          end
          inherited Panel1: TPanel
            Width = 569
            ExplicitWidth = 569
            inherited laBalance: TLabel
              Left = 379
              ExplicitLeft = 379
            end
            inherited laElapsedTime: TLabel
              Left = 251
              ExplicitLeft = 251
            end
            inherited ToolBar1: TToolBar
              Width = 251
              ExplicitWidth = 251
            end
          end
        end
      end
    end
  end
  object paTesting: TPanel [3]
    Left = 4
    Top = 395
    Width = 453
    Height = 25
    Anchors = [akLeft, akBottom]
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    Visible = False
    object pbTesting: TProgressBar
      Left = 6
      Top = 4
      Width = 233
      Height = 17
      Step = 1
      TabOrder = 0
    end
    object buStop: TButton
      Left = 254
      Top = 2
      Width = 75
      Height = 23
      Caption = 'Stop'
      TabOrder = 1
      OnClick = buStopClick
    end
    object buResumeTesting: TButton
      Left = 336
      Top = 2
      Width = 97
      Height = 23
      Caption = 'Resume testing'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Visible = False
      OnClick = buResumeTestingClick
    end
  end
  inherited buSeparateWindow: TJvCaptionButton
    Left = 16
    Top = 32
  end
  object ActionList1: TActionList [6]
    Images = fmUIDataStorage.ilMain
    Left = 240
    Top = 280
    object acStartTesting: TAction
      Caption = 'Start Testing'
      OnExecute = acStartTestingExecute
      OnUpdate = acStartTestingUpdate
    end
    object acStartTraining: TAction
      Caption = 'Start Training'
      OnExecute = acStartTrainingExecute
      OnUpdate = acStartTestingUpdate
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
  inherited ilCaption: TImageList
    Left = 240
    Top = 312
  end
  object rlStartDates: TRecentlyList
    TruncatePaths = False
    RecentlyMenu = pmStartDates
    MaxCount = 8
    CaseSensivity = False
    OnRecentlyMenuClick = rlStartDatesRecentlyMenuClick
    Left = 272
    Top = 184
  end
  object rlStopDates: TRecentlyList
    TruncatePaths = False
    RecentlyMenu = pmStopDate
    MaxCount = 8
    CaseSensivity = False
    OnRecentlyMenuClick = rlStopDatesRecentlyMenuClick
    Left = 272
    Top = 264
  end
  object pmStartDates: TPopupActionBar
    Left = 240
    Top = 184
  end
  object pmStopDate: TPopupActionBar
    Left = 240
    Top = 264
  end
end
