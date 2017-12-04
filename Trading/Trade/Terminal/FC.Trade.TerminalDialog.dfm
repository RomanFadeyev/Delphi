inherited fmTerminalDialog: TfmTerminalDialog
  Caption = 'Terminal'
  ClientHeight = 313
  ClientWidth = 588
  ExplicitWidth = 596
  ExplicitHeight = 342
  PixelsPerInch = 96
  TextHeight = 13
  inline frmTerminal: TfrmTerminalFrame [0]
    Left = 0
    Top = 0
    Width = 588
    Height = 313
    Align = alClient
    AutoScroll = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 588
    ExplicitHeight = 313
    inherited pcPages: TJvPageControl
      Width = 588
      Height = 275
      ExplicitWidth = 588
      ExplicitHeight = 275
      inherited tsOrderHistory: TTabSheet
        ExplicitTop = 20
        ExplicitWidth = 588
        ExplicitHeight = 255
        inherited Splitter1: TSplitter
          Width = 588
          ExplicitWidth = 588
        end
        inherited grOrders: TEditDBGrid
          Width = 588
        end
        inherited grOrderDetails: TEditDBGrid
          Width = 588
          Height = 27
        end
      end
      inherited tsEquity: TTabSheet
        ExplicitTop = 20
        ExplicitWidth = 588
        ExplicitHeight = 255
        inherited chEquity: TChart
          Width = 588
          Height = 255
          ExplicitWidth = 588
          ExplicitHeight = 255
        end
      end
    end
    inherited tcTabs: TFlatTabControl
      Top = 297
      Width = 588
      ExplicitTop = 297
      ExplicitWidth = 588
    end
    inherited Panel1: TPanel
      Width = 588
      ExplicitWidth = 588
      inherited laBalance: TLabel
        Left = 398
        ExplicitLeft = 398
      end
      inherited laElapsedTime: TLabel
        Left = 270
        ExplicitLeft = 270
      end
      inherited ToolBar1: TToolBar
        Width = 270
        ExplicitWidth = 270
      end
    end
  end
  inherited lbDockClient: TJvDockClient [1]
  end
  inherited buSeparateWindow: TJvCaptionButton [2]
  end
end
