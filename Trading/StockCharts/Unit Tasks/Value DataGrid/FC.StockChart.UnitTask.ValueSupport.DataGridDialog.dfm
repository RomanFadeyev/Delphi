inherited fmValueStatisticsDialog: TfmValueStatisticsDialog
  Left = 0
  Top = 189
  Caption = 'Value Statisitcs'
  ClientHeight = 317
  ClientWidth = 521
  ExplicitWidth = 529
  ExplicitHeight = 346
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 278
    Width = 521
    ExplicitTop = 269
    ExplicitWidth = 521
  end
  object Label2: TLabel [1]
    AlignWithMargins = True
    Left = 3
    Top = 25
    Width = 515
    Height = 13
    Align = alTop
    Caption = 'The wizard calculates indicator value statistics'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    ExplicitWidth = 267
  end
  object laStat: TLabel [2]
    Left = 0
    Top = 265
    Width = 521
    Height = 13
    Align = alBottom
    ExplicitWidth = 3
  end
  inherited buOK: TExtendButton
    Left = 440
    Top = 288
    ExplicitLeft = 440
    ExplicitTop = 288
  end
  object grReport: TEditDBGrid [4]
    Left = 0
    Top = 119
    Width = 521
    Height = 146
    Align = alClient
    DataSource = DataSource1
    Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = grReportDblClick
    Options1 = [dgHotTrackTitles]
    Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
    ColorCheme.UnderCursor = clWindow
    ColorCheme.UnderCursorText = clWindowText
    ColorCheme.HighightCols = 13811126
  end
  object pbProgress: TProgressBar [5]
    Left = 8
    Top = 292
    Width = 150
    Height = 17
    Step = 1
    TabOrder = 2
    Visible = False
  end
  object paTop: TPanel [6]
    Left = 0
    Top = 41
    Width = 521
    Height = 78
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    inline frmPeriod: TfrmPeriod
      Left = 8
      Top = 14
      Width = 305
      Height = 63
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 14
      ExplicitWidth = 305
      ExplicitHeight = 63
      inherited Label6: TLabel
        Width = 35
        ExplicitWidth = 35
      end
      inherited Label4: TLabel
        Width = 48
        ExplicitWidth = 48
      end
    end
    object ckFilter: TExtendCheckBox
      Left = 4
      Top = 2
      Width = 98
      Height = 17
      BiDiMode = bdLeftToRight
      Caption = 'Filter'
      ParentBiDiMode = False
      TabOrder = 1
      OnClick = ckFilterClick
      FlatFontColor = clBlack
      
    end
    object buApplyFilter: TExtendButton
      Left = 304
      Top = 23
      Width = 75
      Height = 23
      Caption = 'Apply Filter'
      TabOrder = 2
      OnClick = buApplyFilterClick
      DropDownMode = False
    end
  end
  object ToolBar1: TToolBar [7]
    Left = 0
    Top = 0
    Width = 521
    Height = 22
    AutoSize = True
    Caption = 'ToolBar1'
    Images = fmUIDataStorage.ilMain
    TabOrder = 4
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = acReload
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Hint = 'Export...'
      Caption = 'ToolButton2'
      DropdownMenu = pmExport
      ImageIndex = 14
      ParentShowHint = False
      ShowHint = True
    end
  end
  inherited lbDockClient: TJvDockClient
    Top = 136
  end
  inherited buSeparateWindow: TJvCaptionButton
    Top = 136
  end
  inherited ilCaption: TImageList
    Top = 136
  end
  object taReport: TMemoryDataSet
    FieldDefs = <>
    Left = 403
    Top = 128
    object taReportBarNo: TIntegerField
      FieldName = 'BarNo'
    end
    object taReportDateTime: TDateTimeField
      FieldName = 'DateTime'
    end
    object taReportValue: TFloatField
      FieldName = 'Value'
      DisplayFormat = '0.0000'
    end
  end
  object DataSource1: TDataSource
    DataSet = taReport
    Left = 363
    Top = 128
  end
  object ActionList1: TActionList
    Images = fmUIDataStorage.ilMain
    Left = 408
    Top = 40
    object acReload: TAction
      Caption = 'Reload'
      Hint = 'Reload'
      ImageIndex = 36
      OnExecute = buCalculateClick
    end
    object acExportAsInserts: TAction
      Caption = 'Copy to clipboard as SQL Inserts'
      Hint = 'Copy to clipboard as SQL Inserts'
      OnExecute = acExportAsInsertsExecute
    end
    object acCopyAsText: TAction
      Caption = 'Copy to clipboard as comma separated text'
      Hint = 'Copy to clipboard as comma separated text'
      OnExecute = acCopyAsTextExecute
    end
    object acExportToDB: TAction
      Caption = 'Export to DB'
      Hint = 'Export to DB'
      OnExecute = acExportToDBExecute
    end
  end
  object pmExport: TPopupMenu
    Left = 120
    Top = 24
    object Copytoclipboardascommaseparatedtext1: TMenuItem
      Action = acCopyAsText
    end
    object ExportasSQLInserts1: TMenuItem
      Action = acExportAsInserts
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExporttoDB1: TMenuItem
      Action = acExportToDB
    end
  end
end
