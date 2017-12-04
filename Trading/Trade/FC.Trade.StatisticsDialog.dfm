inherited fmTradingStatisticDialog: TfmTradingStatisticDialog
  Caption = 'Trading statistic'
  ClientHeight = 304
  ClientWidth = 394
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 265
    Width = 394
  end
  object Properties: TLabel [1]
    Left = 8
    Top = 16
    Width = 47
    Height = 13
    Caption = 'Properties'
  end
  inherited buOK: TExtendButton
    Left = 313
    Top = 275
  end
  object lvProperties: TExtendListView
    Left = 8
    Top = 32
    Width = 377
    Height = 225
    Columns = <
      item
        Caption = 'Property'
        Width = 150
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    ColumnClick = False
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object ToolBar1: TToolBar
    Left = 352
    Top = 8
    Width = 33
    Height = 24
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Flat = True
    Images = fmUIDataStorage.ilMain
    TabOrder = 2
    object tbExport: TToolButton
      Left = 0
      Top = 0
      Action = acExport
    end
  end
  object alActions: TActionList
    Images = fmUIDataStorage.ilMain
    Left = 264
    Top = 104
    object acExport: TAction
      Caption = 'Export'
      Hint = 'Export'
      ImageIndex = 7
      OnExecute = acExportExecute
    end
  end
end
