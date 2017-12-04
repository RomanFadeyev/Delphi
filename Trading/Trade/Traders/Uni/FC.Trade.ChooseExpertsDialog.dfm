inherited fmSelectExpertsDialog: TfmSelectExpertsDialog
  Caption = 'Select Experts'
  ClientHeight = 290
  ClientWidth = 357
  ExplicitWidth = 363
  ExplicitHeight = 317
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 251
    Width = 357
    ExplicitTop = 251
    ExplicitWidth = 357
  end
  inherited buOK: TExtendButton
    Left = 196
    Top = 261
    ExplicitLeft = 196
    ExplicitTop = 261
  end
  inherited buCancel: TExtendButton
    Left = 276
    Top = 261
    ExplicitLeft = 276
    ExplicitTop = 261
  end
  object ToolBar1: TToolBar [3]
    Left = 255
    Top = 107
    Width = 185
    Height = 9
    Align = alNone
    AutoSize = True
    ButtonHeight = 9
    Caption = 'ToolBar1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object paWorkspace: TPanel [4]
    Left = 0
    Top = 0
    Width = 357
    Height = 251
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 341
      Height = 13
      Align = alTop
      Caption = 'Experts'
      ExplicitWidth = 35
    end
    object tvProperties: TVirtualStringTree
      Left = 8
      Top = 21
      Width = 341
      Height = 222
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
    Left = 104
    Top = 72
    inherited acOK: TAction
      OnUpdate = acOKUpdate
    end
  end
end
