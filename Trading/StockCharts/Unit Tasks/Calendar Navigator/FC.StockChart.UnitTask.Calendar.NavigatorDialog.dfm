inherited fmCalendarNavigatorDialog: TfmCalendarNavigatorDialog
  Caption = 'Navigator'
  ClientHeight = 213
  ClientWidth = 453
  ExplicitWidth = 461
  ExplicitHeight = 242
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 174
    Width = 453
    ExplicitTop = 180
    ExplicitWidth = 473
  end
  inherited buOK: TExtendButton
    Left = 372
    Top = 184
    ExplicitLeft = 392
    ExplicitTop = 188
  end
  object Panel2: TPanel [2]
    Left = 0
    Top = 0
    Width = 453
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 8
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      453
      49)
    object Label2: TLabel
      Left = 8
      Top = 6
      Width = 75
      Height = 13
      Caption = 'Event to search'
    end
    object cbWeekday: TExtendComboBox
      Left = 8
      Top = 20
      Width = 437
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 0
      TabOrder = 0
      
    end
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 49
    Width = 453
    Height = 125
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    ExplicitTop = 47
    ExplicitHeight = 113
    DesignSize = (
      453
      125)
    object pbProgress: TProgressBar
      Left = 253
      Top = 20
      Width = 192
      Height = 17
      Smooth = True
      Step = 1
      TabOrder = 0
    end
    object buSearchNext: TButton
      Left = 112
      Top = 19
      Width = 41
      Height = 23
      Action = acNext
      TabOrder = 1
    end
    object buReset: TButton
      Left = 8
      Top = 19
      Width = 49
      Height = 23
      Caption = '&Reset'
      TabOrder = 2
      OnClick = buResetClick
    end
    object buPrev: TButton
      Left = 63
      Top = 19
      Width = 45
      Height = 23
      Action = acPrev
      TabOrder = 3
    end
    object Button1: TButton
      Left = 206
      Top = 19
      Width = 41
      Height = 23
      Caption = '&End>>'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 162
      Top = 19
      Width = 42
      Height = 23
      Caption = '<<&Start'
      TabOrder = 5
      OnClick = Button2Click
    end
    object ckCopyToClipboard: TExtendCheckBox
      Left = 114
      Top = 2
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
      Top = 2
      Width = 98
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'Sync all charts'
      ParentBiDiMode = False
      TabOrder = 7
      FlatFontColor = clBlack
      
    end
    object edData: TExtendMemo
      Left = 8
      Top = 48
      Width = 437
      Height = 71
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 8
      
      ExplicitHeight = 63
    end
  end
  inherited lbDockClient: TJvDockClient
    Left = 208
  end
  inherited buSeparateWindow: TJvCaptionButton
    Left = 176
  end
  inherited ilCaption: TImageList
    Left = 240
  end
  object ActionList1: TActionList
    Left = 304
    Top = 120
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
