inherited frmStockChart: TfrmStockChart
  Width = 490
  Height = 316
  ExplicitWidth = 490
  ExplicitHeight = 316
  object laSymbol: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'EURUSD1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    PopupMenu = pmWindow
    Transparent = True
    Visible = False
  end
  object laPosition: TLabel
    Left = 453
    Top = 8
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = '100%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Visible = False
  end
  object imScale: TImage
    Left = 312
    Top = 8
    Width = 13
    Height = 13
    Cursor = crSizeAll
    Hint = 
      'Drag the symbol to change v/h position, hold shift to change v/h' +
      ' scale'
    Anchors = [akTop, akRight]
    AutoSize = True
    ParentShowHint = False
    Picture.Data = {
      07544269746D617006050000424D060500000000000036040000280000000D00
      00000D0000000100080000000000D00000000000000000000000000100000001
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000C0DCC000F0CAA60004040400080808000C0C0C0011111100161616001C1C
      1C002222220029292900555555004D4D4D004242420039393900807CFF005050
      FF009300D600FFECCC00C6D6EF00D6E7E70090A9AD0000003300000066000000
      99000000CC00003300000033330000336600003399000033CC000033FF000066
      00000066330000666600006699000066CC000066FF0000990000009933000099
      6600009999000099CC000099FF0000CC000000CC330000CC660000CC990000CC
      CC0000CCFF0000FF660000FF990000FFCC003300000033003300330066003300
      99003300CC003300FF00333300003333330033336600333399003333CC003333
      FF00336600003366330033666600336699003366CC003366FF00339900003399
      330033996600339999003399CC003399FF0033CC000033CC330033CC660033CC
      990033CCCC0033CCFF0033FF330033FF660033FF990033FFCC0033FFFF006600
      00006600330066006600660099006600CC006600FF0066330000663333006633
      6600663399006633CC006633FF00666600006666330066666600666699006666
      CC00669900006699330066996600669999006699CC006699FF0066CC000066CC
      330066CC990066CCCC0066CCFF0066FF000066FF330066FF990066FFCC00CC00
      FF00FF00CC009999000099339900990099009900CC0099000000993333009900
      66009933CC009900FF00996600009966330099336600996699009966CC009933
      FF009999330099996600999999009999CC009999FF0099CC000099CC330066CC
      660099CC990099CCCC0099CCFF0099FF000099FF330099CC660099FF990099FF
      CC0099FFFF00CC00000099003300CC006600CC009900CC00CC0099330000CC33
      3300CC336600CC339900CC33CC00CC33FF00C56A3100CC66330099666600CC66
      9900CC66CC009966FF00CC990000CC993300CC996600CC999900CC99CC00CC99
      FF00CCCC0000CCCC3300CCCC6600CCCC9900CCCCCC00CCCCFF00CCFF0000CCFF
      330099FF6600CCFF9900CCFFCC00CCFFFF00CC003300FF006600FF009900CC33
      0000FF333300FF336600FF339900FF33CC00FF33FF00FF660000FF663300CC66
      6600FF669900FF66CC00CC66FF00FF990000FF993300FF996600FF999900FF99
      CC00FF99FF00FFCC0000FFCC3300FFCC6600FFCC9900FFCCCC00FFCCFF00FFFF
      3300CCFF6600FFFF9900FFFFCC006666FF0066FF660066FFFF00FF666600FF66
      FF00FFFF66002100A5005F5F5F00777777008686860096969600CBCBCB00B2B2
      B200D7D7D700DDDDDD00E3E3E300EAEAEA00F1F1F100F8F8F800F0FBFF00A4A0
      A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00FDFDFDFDFDFDACFDFDFDFDFDFD000000FDFDFDFDFDACACACFDFDFDFDFD00
      0000FDFDFDFDACACACACACFDFDFDFD000000FDFDFDFDFDFDACFDFDFDFDFDFD00
      0000FDFDACFDFDFDACFDFDFDACFDFD000000FDACACFDFDFDACFDFDFDACACFD00
      0000ACACACACACACACACACACACACCD000000FDACACFDFDFDACFDFDFDACACFD00
      0000FDFDACFDFDFDACFDFDFDACFDFD000000FDFDFDFDFDFDACFDFDFDFDFDFD00
      0000FDFDFDFDACACACACACFDFDFDFD000000FDFDFDFDFDACACACFDFDFDFDFD00
      0000FDFDFDFDFDFDACFDFDFDFDFDFD000000}
    ShowHint = True
    Transparent = True
    Visible = False
    OnMouseDown = imScaleMouseDown
    OnMouseMove = imScaleMouseMove
  end
  object cbGoto: TExtendComboBox
    Left = 342
    Top = 8
    Width = 145
    Height = 21
    AutoDropDown = True
    Anchors = [akTop, akRight]
    TabOrder = 0
    Visible = False
    OnExit = cbGotoExit
    OnKeyDown = cbGotoKeyDown
    
  end
  object alMain: TActionList
    Left = 112
    Top = 104
    object acEditIndicator: TAction
      Category = 'Indicator'
      Caption = 'Edit'
      OnExecute = acEditIndicatorExecute
      OnUpdate = acEditIndicatorUpdate
    end
    object acDeleteIndicator: TAction
      Category = 'Indicator'
      Caption = 'Delete'
      OnExecute = acDeleteIndicatorExecute
      OnUpdate = acDeleteIndicatorUpdate
    end
    object acInvalidateIndicator: TAction
      Category = 'Indicator'
      Caption = 'Invalidate'
      OnExecute = acInvalidateIndicatorExecute
    end
    object acExpertTester: TAction
      Category = 'Expert'
      Caption = 'Expert Tester'
    end
    object acInvalidateAll: TAction
      Category = 'Indicator'
      Caption = 'Invalidate All'
      ShortCut = 16466
      OnExecute = acInvalidateAllExecute
    end
    object acSyncRegion: TAction
      Category = 'Region'
      Caption = 'Sync  Region'
      Hint = 'Select the same hilight region on the others stock charts'
      ShortCut = 16451
      OnExecute = acSyncRegionExecute
    end
    object acReplayTicksInRegion: TAction
      Category = 'Region'
      Caption = 'Replay Ticks in Region'
      Hint = 'Replay Ticks in Region'
      ShortCut = 16464
      OnExecute = acReplayTicksInRegionExecute
    end
    object acReplayTicksInRegion2: TAction
      Category = 'Region'
      Caption = 'Replay Ticks in Region (Over)'
      Hint = 'Replay Ticks in Region (Over)'
      OnExecute = acReplayTicksInRegion2Execute
    end
    object acShiftTicks: TAction
      Caption = 'Shift Ticks from Here'
      OnExecute = acShiftTicksExecute
    end
    object acIndicatorWindowMoveUp: TAction
      Category = 'Window'
      Caption = 'Move Up'
      OnExecute = acIndicatorWindowMoveUpExecute
      OnUpdate = acIndicatorWindowMoveUpUpdate
    end
    object acIndicatorWindowMoveDown: TAction
      Category = 'Window'
      Caption = 'Move Down'
      OnExecute = acIndicatorWindowMoveDownExecute
      OnUpdate = acIndicatorWindowMoveDownUpdate
    end
    object acChangeIndicatorDataSource: TAction
      Category = 'Indicator'
      Caption = 'Change Data Source'
      Hint = 'Change Data Source'
      OnExecute = acChangeIndicatorDataSourceExecute
    end
    object acGetTicksInRegion: TAction
      Category = 'Region'
      Caption = 'Get Ticks In Region'
      OnExecute = acGetTicksInRegionExecute
    end
    object acBoldIndicator: TAction
      Category = 'Indicator'
      Caption = 'Bold'
      OnExecute = acBoldIndicatorExecute
    end
    object acCopyIndicator: TAction
      Category = 'Indicator'
      Caption = 'Copy'
      OnExecute = acCopyIndicatorExecute
      OnUpdate = acCopyIndicatorUpdate
    end
    object acPasteIndicator: TAction
      Category = 'Indicator'
      Caption = 'Paste'
      OnExecute = acPasteIndicatorExecute
      OnUpdate = acPasteIndicatorUpdate
    end
    object acIndicatorBringToFront: TAction
      Category = 'Indicator\Position'
      Caption = 'Bring To Front'
      OnExecute = acIndicatorBringToFrontExecute
    end
    object acIndicatorSendToBack: TAction
      Category = 'Indicator\Position'
      Caption = 'Send To Back'
      OnExecute = acIndicatorSendToBackExecute
    end
    object acFormPopup: TAction
      Category = 'Form'
      Caption = 'Popup'
      OnExecute = acFormPopupExecute
      OnUpdate = acFormPopupUpdate
    end
    object acFormClose: TAction
      Category = 'Form'
      Caption = 'Close'
      OnExecute = acFormCloseExecute
      OnUpdate = acFormCloseUpdate
    end
    object acChartOptions: TAction
      Category = 'Chart'
      Caption = 'Chart Drawing Options'
      OnExecute = acChartOptionsExecute
    end
    object acHelpIndicator: TAction
      Category = 'Indicator'
      Caption = 'Description'
      OnExecute = acHelpIndicatorExecute
      OnUpdate = acHelpIndicatorUpdate
    end
    object acChangeIndicatorWindow: TAction
      Category = 'Indicator'
      Caption = 'Change Window'
      Hint = 'Change Window'
      OnExecute = acChangeIndicatorWindowExecute
      OnUpdate = acChangeIndicatorWindowUpdate
    end
  end
  object pmChart: TPopupActionBar
    OnPopup = pmChartPopup
    Left = 48
    Top = 104
    object miInsertIndicator: TMenuItem
      Caption = 'Insert Indicator'
    end
    object miInsertShape: TMenuItem
      Caption = 'Insert Shape'
    end
    object miInsertExpert: TMenuItem
      Caption = 'Insert Expert'
    end
    object Paste1: TMenuItem
      Action = acPasteIndicator
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miModify: TMenuItem
      Caption = 'Modify...'
      object miModifyIndicator: TMenuItem
        Caption = 'Modify Indicator'
      end
      object miModifyShape: TMenuItem
        Caption = 'Modify Shape'
      end
      object miModifyExpert: TMenuItem
        Caption = 'Modify Expert'
      end
    end
    object miDelete: TMenuItem
      Caption = 'Delete...'
      object miDeleteIndicator: TMenuItem
        Caption = 'Delete Indicator'
      end
      object miDeleteShape: TMenuItem
        Caption = 'Delete Shape'
      end
      object miDeleteExpert: TMenuItem
        Caption = 'Delete Expert'
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miExpertTester: TMenuItem
      Action = acExpertTester
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object SyncCursorPositiononAllCharts1: TMenuItem
        Action = fmUIDataStorage.acChartSyncPosition
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ShowCurrentPosition1: TMenuItem
        Action = fmUIDataStorage.acChartShowPosition
      end
      object ShowCrosshair1: TMenuItem
        Action = fmUIDataStorage.acChartShowCrosshair
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object FixVerticalScale1: TMenuItem
        Action = fmUIDataStorage.acChartFixVerticalScale
      end
      object FitAll1: TMenuItem
        Action = fmUIDataStorage.acChartFitAll
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object ChartOptions1: TMenuItem
        Action = acChartOptions
      end
    end
    object IndicatorWindow1: TMenuItem
      Caption = 'Indicator Window'
      object MoveUp1: TMenuItem
        Action = acIndicatorWindowMoveUp
      end
      object MoveDown1: TMenuItem
        Action = acIndicatorWindowMoveDown
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object InvalidateAll1: TMenuItem
      Action = acInvalidateAll
    end
    object Shiftticksfromhere1: TMenuItem
      Action = acShiftTicks
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object SyncHilightRegion1: TMenuItem
      Action = acSyncRegion
    end
    object SyncHilightRegion3: TMenuItem
      Action = acReplayTicksInRegion
    end
    object ReplayTicksinRegionOver1: TMenuItem
      Action = acReplayTicksInRegion2
    end
    object GetTicksInRegion1: TMenuItem
      Action = acGetTicksInRegion
    end
  end
  object pmIndicator: TPopupActionBar
    OnGetControlClass = pmIndicatorGetControlClass
    Left = 48
    Top = 136
    object miIndicatorName: TMenuItem
      AutoHotkeys = maManual
      Caption = '#Name#'
      Enabled = False
    end
    object miIndicatorEdit: TMenuItem
      Action = acEditIndicator
    end
    object miIndicatorDelete: TMenuItem
      Action = acDeleteIndicator
    end
    object Copy1: TMenuItem
      Action = acCopyIndicator
    end
    object acHelpIndicator1: TMenuItem
      Action = acHelpIndicator
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object miTasks: TMenuItem
      Caption = 'Tasks'
    end
    object Position1: TMenuItem
      Caption = 'Position'
      object BringToFront1: TMenuItem
        Action = acIndicatorBringToFront
      end
      object SendToBack1: TMenuItem
        Action = acIndicatorSendToBack
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Bold2: TMenuItem
        Action = acChangeIndicatorWindow
      end
    end
    object miCharacteristics: TMenuItem
      Caption = 'Show Characteristics'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Bold1: TMenuItem
      Action = acBoldIndicator
    end
    object Invalidate1: TMenuItem
      Action = acInvalidateIndicator
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object SyncHilightRegion2: TMenuItem
      Action = acSyncRegion
    end
    object ReplayTicksinRegion1: TMenuItem
      Action = acReplayTicksInRegion
    end
  end
  object rlGoto: TRecentlyList
    TruncatePaths = False
    MaxCount = 4
    CaseSensivity = False
    Left = 360
    Top = 40
  end
  object odTheme: TOpenDialog
    DefaultExt = 'sctheme'
    Filter = 'Themes (*.sctheme)|*.sctheme|Chart documents (*.scd)|*.scd'
    Title = 'Load Theme'
    Left = 304
    Top = 280
  end
  object sdTheme: TSaveDialog
    DefaultExt = 'sctheme'
    Filter = 'Themes (*.sctheme)|*.sctheme'
    Left = 272
    Top = 280
  end
  object pmWindow: TPopupActionBar
    Left = 48
    Top = 176
    object miClose: TMenuItem
      Action = acFormClose
    end
    object acFormPopup1: TMenuItem
      Action = acFormPopup
    end
  end
end
