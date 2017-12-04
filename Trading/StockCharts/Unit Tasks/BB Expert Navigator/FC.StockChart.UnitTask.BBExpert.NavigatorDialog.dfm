inherited fmNavigator: TfmNavigator
  Caption = 'fmNavigator'
  ClientHeight = 217
  ClientWidth = 473
  OnClose = FormClose
  ExplicitWidth = 479
  ExplicitHeight = 244
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
    OnClick = buOKClick
    ExplicitLeft = 392
    ExplicitTop = 188
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 146
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 5
      Width = 91
      Height = 13
      Caption = 'Attributes to search'
    end
    object laFound: TLabel
      Left = 236
      Top = 127
      Width = 3
      Height = 13
    end
    object lbAttributes: TCheckListBox
      Left = 8
      Top = 24
      Width = 222
      Height = 116
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 146
    Width = 473
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    DesignSize = (
      473
      32)
    object pbProgress: TProgressBar
      Left = 236
      Top = 9
      Width = 229
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Smooth = True
      Step = 1
      TabOrder = 0
    end
    object buSearchNext: TButton
      Left = 160
      Top = 6
      Width = 70
      Height = 23
      Caption = '&Next >'
      TabOrder = 1
      OnClick = buSearchNextClick
    end
    object buReset: TButton
      Left = 8
      Top = 6
      Width = 70
      Height = 23
      Caption = '&Reset'
      TabOrder = 2
      OnClick = buResetClick
    end
    object buPrev: TButton
      Left = 84
      Top = 6
      Width = 70
      Height = 23
      Caption = '< &Prev'
      TabOrder = 3
      OnClick = buPrevClick
    end
  end
end
