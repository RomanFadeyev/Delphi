inherited fmCalendarBarListDialog: TfmCalendarBarListDialog
  Caption = 'Data Grid'
  ClientHeight = 164
  ClientWidth = 763
  ExplicitWidth = 771
  ExplicitHeight = 193
  PixelsPerInch = 96
  TextHeight = 13
  inherited bvBottom: TBevel
    Top = 125
    Width = 763
    ExplicitTop = 125
    ExplicitWidth = 550
  end
  inherited buOK: TExtendButton
    Left = 682
    Top = 135
    ExplicitLeft = 469
    ExplicitTop = 135
  end
  object grData: TEditDBGrid [2]
    Left = 0
    Top = 0
    Width = 763
    Height = 125
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
    Options1 = [dgAutoStretchColumns, dgWrapColumnTitles, dgSearchControls]
    Options2 = [dgFrameCursor, dgEnterToNextCell, dgHighlightCursorPos]
    ColorCheme.UnderCursor = clWindow
    ColorCheme.UnderCursorText = clWindowText
    ColorCheme.HighightCols = 13811126
  end
  object dsData: TDataSource
    DataSet = taData
    Left = 256
    Top = 168
  end
  object taData: TMemoryDataSet
    FieldDefs = <>
    Left = 392
    Top = 96
    object taDataDate: TDateTimeField
      FieldName = 'Date'
    end
    object taDataCountry: TStringField
      DisplayWidth = 10
      FieldName = 'Country'
      Size = 255
    end
    object taDataClass: TIntegerField
      DisplayWidth = 3
      FieldName = 'Class'
    end
    object taDataIndicator: TStringField
      DisplayWidth = 60
      FieldName = 'Indicator'
      Size = 255
    end
    object taDataPeriod: TStringField
      DisplayWidth = 10
      FieldName = 'Period'
      Size = 255
    end
    object taDataPriority: TStringField
      DisplayWidth = 5
      FieldName = 'Priority'
      Size = 255
    end
    object taDataPrevious: TStringField
      DisplayWidth = 8
      FieldName = 'Previous'
      Size = 255
    end
    object taDataForecast: TStringField
      DisplayWidth = 8
      FieldName = 'Forecast'
      Size = 255
    end
    object taDataFact: TStringField
      DisplayWidth = 8
      FieldName = 'Fact'
      Size = 255
    end
    object taDataPFTrend: TStringField
      DisplayLabel = 'PF Trend'
      DisplayWidth = 8
      FieldName = 'PFTrend'
    end
    object taDataFFTrend: TStringField
      DisplayLabel = 'FF Trend'
      DisplayWidth = 8
      FieldName = 'FFTrend'
    end
  end
end
