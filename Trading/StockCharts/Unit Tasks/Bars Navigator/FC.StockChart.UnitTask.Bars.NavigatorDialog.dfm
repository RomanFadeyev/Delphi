inherited fmBarsNavigatorDialog: TfmBarsNavigatorDialog
  Caption = 'Navigator'
  ClientHeight = 217
  ClientWidth = 473
  OnClose = FormClose
  ExplicitWidth = 481
  ExplicitHeight = 246
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 178
    Width = 473
    ExplicitTop = 178
    ExplicitWidth = 473
  end
  inherited buOK: TExtendButton
    Left = 392
    Top = 188
    ExplicitLeft = 392
    ExplicitTop = 188
  end
  object Panel2: TPanel [2]
    Left = 0
    Top = 0
    Width = 473
    Height = 137
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    FullRepaint = False
    TabOrder = 1
    object pcPages: TPageControl
      Left = 8
      Top = 8
      Width = 457
      Height = 121
      ActivePage = tsVolatility
      Align = alClient
      TabOrder = 0
      object tsTime: TTabSheet
        Caption = 'Navigate by Time'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label2: TLabel
          Left = 84
          Top = 5
          Width = 93
          Height = 13
          Caption = 'Weekday to search'
        end
        object Label1: TLabel
          Left = 8
          Top = 5
          Width = 70
          Height = 13
          Caption = 'Time to search'
        end
        object dtTime: TExtendMinutePicker
          Left = 8
          Top = 20
          Width = 70
          Height = 21
          NumLock = False
          
          TabOrder = 0
        end
        object cbWeekday: TExtendComboBox
          Left = 84
          Top = 20
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
          
        end
      end
      object tsVolatility: TTabSheet
        Caption = 'Navigate by Volatility'
        ImageIndex = 1
        object Label4: TLabel
          Left = 165
          Top = 13
          Width = 9
          Height = 13
          Caption = 'pt'
        end
        object Label3: TLabel
          Left = 165
          Top = 59
          Width = 9
          Height = 13
          Caption = 'pt'
        end
        object edOC: TExtendSpinEdit
          Left = 108
          Top = 9
          Width = 51
          Height = 21
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
        end
        object ckOC: TExtendCheckBox
          Left = 8
          Top = 11
          Width = 94
          Height = 17
          Caption = '|Open-Close| >='
          TabOrder = 1
          OnClick = ckOCClick
          FlatFontColor = clBlack
          
        end
        object ckHL: TExtendCheckBox
          Left = 8
          Top = 57
          Width = 94
          Height = 17
          Caption = 'High-Low >='
          TabOrder = 2
          OnClick = ckOCClick
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
      end
    end
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 137
    Width = 473
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    DesignSize = (
      473
      41)
    object laFound: TLabel
      Left = 253
      Top = 2
      Width = 3
      Height = 13
    end
    object pbProgress: TProgressBar
      Left = 253
      Top = 20
      Width = 212
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Smooth = True
      Step = 1
      TabOrder = 0
    end
    object buSearchNext: TButton
      Left = 112
      Top = 17
      Width = 41
      Height = 23
      Action = acNext
      TabOrder = 1
    end
    object buReset: TButton
      Left = 8
      Top = 17
      Width = 49
      Height = 23
      Caption = '&Reset'
      TabOrder = 2
      OnClick = buResetClick
    end
    object buPrev: TButton
      Left = 63
      Top = 17
      Width = 45
      Height = 23
      Action = acPrev
      TabOrder = 3
    end
    object Button1: TButton
      Left = 206
      Top = 17
      Width = 41
      Height = 23
      Caption = '&End>>'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 162
      Top = 17
      Width = 42
      Height = 23
      Caption = '<<&Start'
      TabOrder = 5
      OnClick = Button2Click
    end
    object ckCopyToClipboard: TExtendCheckBox
      Left = 114
      Top = 0
      Width = 111
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'Copy to clipboard'
      ParentBiDiMode = False
      TabOrder = 6
      FlatFontColor = clBlack
      
    end
    object ckSyncAllCharts: TExtendCheckBox
      Left = 8
      Top = 0
      Width = 98
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'Sync all charts'
      ParentBiDiMode = False
      TabOrder = 7
      FlatFontColor = clBlack
      
    end
  end
  object ActionList1: TActionList
    Left = 328
    Top = 88
    object acPrev: TAction
      Caption = '< &Prev'
      Hint = 'Find Previous'
      OnExecute = buPrevClick
      OnUpdate = acPrevUpdate
    end
    object acNext: TAction
      Caption = '&Next >'
      Hint = 'Find Next'
      OnExecute = buSearchNextClick
      OnUpdate = acNextUpdate
    end
  end
end
