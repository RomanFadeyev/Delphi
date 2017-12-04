inherited fmTesterTrainReportDialog: TfmTesterTrainReportDialog
  Caption = 'Train Report'
  PixelsPerInch = 96
  TextHeight = 13
  object paWorkSpace: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 203
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 32
      Height = 13
      Align = alTop
      Caption = 'Report'
    end
    object lvReport: TExtendListView
      Left = 4
      Top = 17
      Width = 482
      Height = 182
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 20
        end>
      HideSelection = False
      ReadOnly = True
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = lvReportColumnClick
      
    end
  end
  object buStopTrain: TButton
    Left = 329
    Top = 213
    Width = 75
    Height = 23
    Caption = 'Start Train'
    TabOrder = 2
    OnClick = buStopTrainClick
  end
end
