inherited fmTesterTrainPropsDialog: TfmTesterTrainPropsDialog
  Caption = 'Choose properties to train'
  ClientHeight = 330
  ClientWidth = 317
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 291
    Width = 317
    ExplicitTop = 291
    ExplicitWidth = 317
  end
  inherited buOK: TExtendButton
    Left = 156
    Top = 301
    ExplicitLeft = 156
    ExplicitTop = 301
  end
  inherited buCancel: TExtendButton
    Left = 236
    Top = 301
    ExplicitLeft = 236
    ExplicitTop = 301
  end
  object paWorkPlace: TPanel [3]
    Left = 0
    Top = 0
    Width = 317
    Height = 291
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 309
      Height = 13
      Align = alTop
      Caption = 'Experts and their properties'
      ExplicitWidth = 128
    end
    object tvProperties: TVirtualStringTree
      Left = 4
      Top = 17
      Width = 309
      Height = 270
      Align = alClient
      AutoScrollDelay = 300
      BevelInner = bvLowered
      BevelOuter = bvNone
      BevelKind = bkSoft
      BorderStyle = bsNone
      ButtonStyle = bsTriangle
      CheckImageKind = ckFlat
      DefaultNodeHeight = 19
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Header.AutoSizeIndex = 1
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Height = 16
      Header.MainColumn = -1
      Header.Options = [hoAutoResize, hoColumnResize]
      Header.Style = hsFlatButtons
      HintAnimation = hatNone
      HintMode = hmHint
      Indent = 12
      Margin = 0
      ParentFont = False
      ParentShowHint = False
      ScrollBarOptions.ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoTristateTracking]
      TreeOptions.MiscOptions = [toCheckSupport, toInitOnSave]
      TreeOptions.PaintOptions = [toShowButtons, toShowHorzGridLines, toShowRoot, toShowVertGridLines]
      TreeOptions.SelectionOptions = [toExtendedFocus]
      OnBeforeItemErase = tvPropertiesBeforeItemErase
      OnGetText = tvPropertiesGetText
      OnGetNodeDataSize = tvPropertiesGetNodeDataSize
      Columns = <>
    end
  end
  inherited acDialog: TActionList
    Left = 72
    Top = 40
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
end
