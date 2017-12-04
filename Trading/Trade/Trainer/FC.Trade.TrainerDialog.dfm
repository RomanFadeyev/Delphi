inherited fmTradeTrainerDialog: TfmTradeTrainerDialog
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Trade Trainer'
  ClientHeight = 289
  ClientWidth = 582
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  ExplicitWidth = 590
  ExplicitHeight = 318
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 250
    Width = 582
    ExplicitTop = 166
    ExplicitWidth = 585
  end
  inherited buOK: TExtendButton
    Left = 501
    Top = 260
    ExplicitLeft = 501
    ExplicitTop = 260
  end
  object paWorkspace: TPanel [2]
    Left = 0
    Top = 0
    Width = 582
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    FullRepaint = False
    TabOrder = 1
    object pcPages: TPageControl
      Left = 4
      Top = 4
      Width = 574
      Height = 242
      ActivePage = tsStart
      Align = alClient
      TabOrder = 0
      object tsStart: TTabSheet
        Caption = 'Start'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object paStart: TScrollBox
          Left = 0
          Top = 0
          Width = 566
          Height = 214
          Align = alClient
          BorderStyle = bsNone
          ParentBackground = True
          TabOrder = 0
          object Label4: TLabel
            Left = 3
            Top = 9
            Width = 48
            Height = 13
            Caption = 'Start From'
          end
          object Label5: TLabel
            Left = 319
            Top = 85
            Width = 63
            Height = 13
            Caption = 'Initial Deposit'
          end
          object Label6: TLabel
            Left = 3
            Top = 33
            Width = 35
            Height = 13
            Caption = 'Stop At'
          end
          object buFindStartDate: TSpeedButton
            Left = 282
            Top = 4
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
            Left = 282
            Top = 28
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
            Left = 267
            Top = 5
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
            Left = 267
            Top = 29
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
          object Label8: TLabel
            Left = 319
            Top = 61
            Width = 88
            Height = 13
            Caption = 'Stop Level (points)'
          end
          object Label2: TLabel
            Left = 319
            Top = 36
            Width = 71
            Height = 13
            Caption = 'Spread (points)'
          end
          object Label7: TLabel
            Left = 319
            Top = 9
            Width = 60
            Height = 13
            Caption = 'Getting price'
          end
          object edStartFromDate: TExtendDateTimePicker
            Left = 57
            Top = 2
            Width = 129
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            TabOrder = 3
            TransparentBorder = False
            
          end
          object edStartFromTime: TExtendDateTimePicker
            Left = 193
            Top = 5
            Width = 73
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            Kind = dtkTime
            TabOrder = 0
            TransparentBorder = False
            
          end
          object buStartTesting: TButton
            Left = 3
            Top = 108
            Width = 86
            Height = 23
            Action = acStartTesting
            TabOrder = 1
          end
          object cbInitialDeposit: TExtendComboBox
            Left = 415
            Top = 82
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            
          end
          object edStopAtDate: TExtendDateTimePicker
            Left = 57
            Top = 29
            Width = 129
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            TabOrder = 4
            TransparentBorder = False
            
          end
          object edStopAtTime: TExtendDateTimePicker
            Left = 193
            Top = 29
            Width = 73
            Height = 21
            Date = 38761.538175462960000000
            Time = 38761.538175462960000000
            Kind = dtkTime
            TabOrder = 5
            TransparentBorder = False
            
          end
          object ckEmulateTicks: TExtendCheckBox
            Left = 319
            Top = 109
            Width = 217
            Height = 17
            Hint = 'Allows you to approach almost real market conditions'
            Caption = 'Emulate ticks (slow)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 6
            FlatFontColor = clBlack
            
          end
          object edStopLevel: TExtendSpinEdit
            Left = 415
            Top = 58
            Width = 121
            Height = 21
            Hint = 
              'The minimal distance between stop levels (StopLoss and TakeProfi' +
              't levels) and market price in points '
            MaxValue = 0
            MinValue = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 7
          end
          object edSpread: TExtendSpinEdit
            Left = 415
            Top = 33
            Width = 121
            Height = 21
            Hint = 'Distance between bid and ask price'
            MaxValue = 0
            MinValue = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 8
          end
          object cbGettingPriceType: TExtendComboBox
            Left = 415
            Top = 6
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 9
            
          end
        end
      end
      object tsResults: TTabSheet
        Caption = 'Process'
        DesignSize = (
          566
          214)
        object Label1: TLabel
          Left = 449
          Top = 9
          Width = 33
          Height = 13
          Caption = 'Skip to'
        end
        object buDropSkipTo: TSpeedButton
          Left = 525
          Top = 6
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
          OnClick = buDropSkipToClick
        end
        object buSkipToApply: TSpeedButton
          Left = 541
          Top = 5
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
            FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD
            FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD04
            04FDFDFDFDFDFDFDFDFDFDFDFDFD04040404FDFDFDFDFDFDFDFDFDFDFD040404
            040404FDFDFDFDFDFDFDFDFD04040404FD040404FDFDFDFDFDFDFDFD040404FD
            FDFD040404FDFDFDFDFDFDFDFDFDFDFDFDFDFD040404FDFDFDFDFDFDFDFDFDFD
            FDFDFDFD040404FDFDFDFDFDFDFDFDFDFDFDFDFDFD040404FDFDFDFDFDFDFDFD
            FDFDFDFDFDFD040404FDFDFDFDFDFDFDFDFDFDFDFDFDFD0404FDFDFDFDFDFDFD
            FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD}
          ParentShowHint = False
          ShowHint = True
          OnClick = buSkipToApplyClick
        end
        object Bevel1: TBevel
          Left = 90
          Top = 3
          Width = 6
          Height = 64
          Shape = bsLeftLine
        end
        object Bevel2: TBevel
          Left = 104
          Top = 33
          Width = 465
          Height = 4
          Shape = bsTopLine
        end
        object buPause: TButton
          Left = 3
          Top = 3
          Width = 80
          Height = 23
          Hint = 'Pause (Ctrl+Enter)'
          Action = acPause
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object buResumeTesting: TButton
          Left = 3
          Top = 32
          Width = 80
          Height = 23
          Hint = 'Resume (Ctrl+Enter)'
          Action = acResume
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object edSkipTo: TExtendMinutePicker
          Left = 485
          Top = 6
          Width = 39
          Height = 21
          NumLock = False
          TabOrder = 2
        end
        object buSkip1Min: TButton
          Tag = 1
          Left = 101
          Top = 4
          Width = 85
          Height = 23
          Caption = 'Complete &1 min'
          TabOrder = 3
          OnClick = buSkip1MinClick
        end
        object buSkip5Min: TButton
          Tag = 5
          Left = 187
          Top = 4
          Width = 85
          Height = 23
          Caption = 'Complete &5 min'
          TabOrder = 4
          OnClick = buSkip1MinClick
        end
        object buSkip15Min: TButton
          Tag = 15
          Left = 273
          Top = 4
          Width = 85
          Height = 23
          Caption = 'Complete 1&5 min'
          TabOrder = 5
          OnClick = buSkip1MinClick
        end
        object buSkip60Min: TButton
          Tag = 60
          Left = 359
          Top = 4
          Width = 85
          Height = 23
          Caption = 'Complete &60 min'
          TabOrder = 6
          OnClick = buSkip1MinClick
        end
        object tbSpeed: TTrackBar
          Left = 472
          Top = 43
          Width = 93
          Height = 20
          Hint = 
            'Use Ctrl+Gray Plus and Ctrl+Gray Minus  to increase to decrease ' +
            'speed'
          ParentShowHint = False
          Position = 10
          ShowHint = True
          ShowSelRange = False
          TabOrder = 7
          ThumbLength = 13
        end
        object Button1: TButton
          Left = 101
          Top = 43
          Width = 80
          Height = 23
          Action = acBuy
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
        end
        object Button2: TButton
          Left = 187
          Top = 44
          Width = 80
          Height = 23
          Action = acSell
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
        end
        object Button3: TButton
          Left = 273
          Top = 44
          Width = 80
          Height = 23
          Action = frmTerminal.acModifyOrder
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
        end
        inline frmTerminal: TfrmTerminalFrame
          Left = 0
          Top = 70
          Width = 566
          Height = 146
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoScroll = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          TabStop = True
          ExplicitTop = 70
          ExplicitWidth = 566
          ExplicitHeight = 146
          inherited pcPages: TJvPageControl
            Width = 566
            Height = 108
            ExplicitWidth = 566
            ExplicitHeight = 108
            inherited tsOrderHistory: TTabSheet
              ExplicitTop = 20
              ExplicitWidth = 566
              ExplicitHeight = 88
              inherited Splitter1: TSplitter
                Top = 0
                Width = 566
                Visible = False
                ExplicitTop = 0
                ExplicitWidth = 588
              end
              inherited grOrders: TEditDBGrid
                Top = 3
                Width = 566
                Height = 85
                Align = alClient
              end
              inherited grOrderDetails: TEditDBGrid
                Width = 568
                Height = 10
                Align = alNone
                Visible = False
              end
            end
          end
          inherited tcTabs: TFlatTabControl
            Top = 130
            Width = 566
            Visible = False
            ExplicitTop = 130
            ExplicitWidth = 566
          end
          inherited Panel1: TPanel
            Width = 566
            Visible = False
            ExplicitWidth = 566
            inherited laBalance: TLabel
              Left = 376
              ExplicitLeft = 398
            end
            inherited laElapsedTime: TLabel
              Left = 248
              ExplicitLeft = 270
            end
            inherited ToolBar1: TToolBar
              Width = 248
              ExplicitWidth = 248
            end
          end
          inherited acActions: TActionList
            inherited acModifyOrder: TAction
              OnExecute = frmTerminalacModifyOrderExecute
            end
            inherited acCloseOrder: TAction
              OnExecute = frmTerminalacCloseOrderExecute
            end
          end
          inherited taOrders: TMemoryDataSet
            inherited taOrdersStopLoss: TCurrencyField
              Visible = True
            end
            inherited taOrdersTakeProfit: TCurrencyField
              Visible = True
            end
            inherited taOrdersTrailingStop: TCurrencyField
              Visible = True
            end
            inherited taOrdersPendingOpenPrice: TCurrencyField
              Visible = True
            end
          end
        end
        object Button4: TButton
          Left = 359
          Top = 44
          Width = 80
          Height = 23
          Action = frmTerminal.acCloseOrder
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
        end
      end
    end
  end
  object paTesting: TPanel [3]
    Left = 232
    Top = 1
    Width = 350
    Height = 23
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    Visible = False
    object laTime: TLabel
      Left = 0
      Top = 0
      Width = 134
      Height = 23
      Align = alLeft
      AutoSize = False
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
    object pbTesting: TProgressBar
      Left = 140
      Top = 5
      Width = 121
      Height = 14
      Step = 1
      TabOrder = 0
    end
    object buStop: TButton
      Left = 266
      Top = 0
      Width = 80
      Height = 23
      Caption = 'Stop'
      TabOrder = 1
      OnClick = buStopClick
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 408
    Top = 168
  end
  inherited buSeparateWindow: TJvCaptionButton
    Left = 440
    Top = 168
  end
  inherited ilCaption: TImageList
    Left = 496
    Top = 104
  end
  object acActions: TActionList
    Images = fmUIDataStorage.ilMain
    Left = 536
    Top = 104
    object acStartTesting: TAction
      Caption = 'Start Training'
      OnExecute = acStartTestingExecute
      OnUpdate = acStartTestingUpdate
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
    object acPause: TAction
      Caption = '&Pause'
      OnExecute = acPauseExecute
      OnUpdate = acPauseUpdate
    end
    object acResume: TAction
      Caption = '&Resume'
      OnExecute = buResumeTestingClick
      OnUpdate = acResumeUpdate
    end
    object acBuy: TAction
      Caption = '&Buy'
      OnExecute = acBuyExecute
      OnUpdate = acBuyUpdate
    end
    object acSell: TAction
      Caption = '&Sell'
      OnExecute = acSellExecute
      OnUpdate = acBuyUpdate
    end
    object acIncSpeed: TAction
      Caption = 'acIncSpeed'
      ShortCut = 24763
      OnExecute = acIncSpeedExecute
    end
    object acDecSpeed: TAction
      Caption = 'acDecSpeed'
      ShortCut = 16573
      OnExecute = acDecSpeedExecute
    end
  end
  object rlStartDates: TRecentlyList
    TruncatePaths = False
    RecentlyMenu = pmStartDates
    MaxCount = 8
    CaseSensivity = False
    OnRecentlyMenuClick = rlStartDatesRecentlyMenuClick
    Left = 192
    Top = 160
  end
  object rlStopDates: TRecentlyList
    TruncatePaths = False
    RecentlyMenu = pmStopDate
    MaxCount = 8
    CaseSensivity = False
    OnRecentlyMenuClick = rlStopDatesRecentlyMenuClick
    Left = 192
    Top = 192
  end
  object pmStartDates: TPopupActionBar
    Left = 224
    Top = 160
  end
  object pmStopDate: TPopupActionBar
    Left = 224
    Top = 192
  end
  object pmSkipTo: TPopupActionBar
    Left = 480
    Top = 168
    object N8001: TMenuItem
      Tag = 480
      Caption = '8:00'
      OnClick = OnSkipToClick
    end
    object N10001: TMenuItem
      Tag = 600
      Caption = '10:00'
      OnClick = OnSkipToClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object N14301: TMenuItem
      Tag = 870
      Caption = '14:30'
      OnClick = OnSkipToClick
    end
    object N15001: TMenuItem
      Tag = 900
      Caption = '15:00'
      OnClick = OnSkipToClick
    end
    object N16001: TMenuItem
      Tag = 960
      Caption = '16:00'
      OnClick = OnSkipToClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object N23001: TMenuItem
      Tag = 1380
      Caption = '23:00'
      OnClick = OnSkipToClick
    end
  end
end
