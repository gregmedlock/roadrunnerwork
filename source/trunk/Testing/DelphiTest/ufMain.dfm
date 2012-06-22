object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Test uRoadRunnerAPI.pas'
  ClientHeight = 563
  ClientWidth = 907
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    907
    563)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottm: TPanel
    Left = 0
    Top = 522
    Width = 907
    Height = 41
    Align = alBottom
    TabOrder = 0
    object lblProgress: TLabel
      Left = 8
      Top = 16
      Width = 13
      Height = 13
      Caption = 'Ok'
    end
    object Label1: TLabel
      Left = 417
      Top = 16
      Width = 66
      Height = 13
      Caption = 'Temp Folder: '
    end
    object lblTempFolder: TEdit
      Left = 485
      Top = 13
      Width = 317
      Height = 21
      TabOrder = 0
      Text = 'C:\'
    end
  end
  object btnGetCopyright: TButton
    Left = 358
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Get Copyright'
    TabOrder = 1
    OnClick = btnGetCopyrightClick
  end
  object btnLoadSBML: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Load SBML'
    TabOrder = 2
    OnClick = btnLoadSBMLClick
  end
  object grid: TStringGrid
    Left = 485
    Top = 0
    Width = 422
    Height = 522
    Align = alRight
    Anchors = [akLeft, akTop, akBottom]
    DefaultColWidth = 84
    FixedCols = 0
    TabOrder = 3
  end
  object btnGetReactionNames: TButton
    Left = 358
    Top = 82
    Width = 121
    Height = 25
    Caption = 'Get Reaction Names'
    TabOrder = 4
    OnClick = btnGetReactionNamesClick
  end
  object btnGetAvailableSymbols: TButton
    Left = 358
    Top = 113
    Width = 121
    Height = 25
    Caption = 'Get Available Symbols'
    TabOrder = 5
    OnClick = btnGetAvailableSymbolsClick
  end
  object lstSummary: TListBox
    Left = 8
    Top = 199
    Width = 468
    Height = 317
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object btnSteadyState: TButton
    Left = 8
    Top = 97
    Width = 121
    Height = 25
    Caption = 'Steady State'
    TabOrder = 7
    OnClick = btnSteadyStateClick
  end
  object edtModelName: TEdit
    Left = 8
    Top = 39
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'ss_MinusOneError.xml'
  end
  object btnLoadTwoModels: TButton
    Left = 8
    Top = 168
    Width = 121
    Height = 25
    Caption = 'Load Two Models Test'
    TabOrder = 9
    OnClick = btnLoadTwoModelsClick
  end
  object chkConservationLaws: TCheckBox
    Left = 135
    Top = 16
    Width = 153
    Height = 17
    Caption = 'Use Conservation Laws'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = chkConservationLawsClick
  end
  object btnSimulate: TButton
    Left = 8
    Top = 66
    Width = 121
    Height = 25
    Caption = 'Simulate'
    TabOrder = 11
    OnClick = btnSimulateClick
  end
end
