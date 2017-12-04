inherited fmBarsDataGridDialog: TfmBarsDataGridDialog
  Caption = 'Data Grid'
  ClientWidth = 550
  ExplicitWidth = 558
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Width = 550
    ExplicitWidth = 550
  end
  inherited buOK: TExtendButton
    Left = 469
    ExplicitLeft = 469
  end
  object grData: TEditDBGrid [2]
    Left = 0
    Top = 0
    Width = 550
    Height = 279
    Align = alClient
    DataSource = dsData
    Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Options1 = [dgAutoStretchColumns, dgFitColumnsToSize, dgWrapColumnTitles, dgSearchControls]
    Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos, dgSupressColumnActivation]
    ColorCheme.UnderCursor = clWindow
    ColorCheme.UnderCursorText = clWindowText
    ColorCheme.HighightCols = 13811126
  end
  object dsData: TDataSource
    Left = 256
    Top = 168
  end
end
