object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Test uRoadRunnerAPI.pas'
  ClientHeight = 563
  ClientWidth = 814
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    814
    563)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBuildDate: TLabel
    Left = 10
    Top = 48
    Width = 14
    Height = 13
    Caption = 'NA'
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Load DLL'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object pnlBottm: TPanel
    Left = 0
    Top = 522
    Width = 814
    Height = 41
    Align = alBottom
    TabOrder = 1
    object lblProgress: TLabel
      Left = 8
      Top = 16
      Width = 13
      Height = 13
      Caption = 'Ok'
    end
    object Label1: TLabel
      Left = 352
      Top = 16
      Width = 66
      Height = 13
      Caption = 'Temp Folder: '
    end
    object lblTempFolder: TEdit
      Left = 420
      Top = 13
      Width = 269
      Height = 21
      TabOrder = 0
      Text = 'C:\'
    end
  end
  object btnGetCopyright: TButton
    Left = 8
    Top = 75
    Width = 121
    Height = 25
    Caption = 'Get Copyright'
    TabOrder = 2
    OnClick = btnGetCopyrightClick
  end
  object btnLoadSBML: TButton
    Left = 8
    Top = 106
    Width = 121
    Height = 25
    Caption = 'Load SBML'
    TabOrder = 3
    OnClick = btnLoadSBMLClick
  end
  object grid: TStringGrid
    Left = 392
    Top = 0
    Width = 422
    Height = 522
    Align = alRight
    Anchors = [akLeft, akTop, akBottom]
    DefaultColWidth = 84
    FixedCols = 0
    TabOrder = 4
  end
  object btnGetReactionNames: TButton
    Left = 262
    Top = 75
    Width = 121
    Height = 25
    Caption = 'Get Reaction Names'
    TabOrder = 5
    OnClick = btnGetReactionNamesClick
  end
  object btnGetAvailableSymbols: TButton
    Left = 135
    Top = 106
    Width = 121
    Height = 25
    Caption = 'Get Available Symbols'
    TabOrder = 6
    OnClick = btnGetAvailableSymbolsClick
  end
  object lstSummary: TListBox
    Left = 8
    Top = 199
    Width = 375
    Height = 317
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 7
  end
  object btnSteadyState: TButton
    Left = 135
    Top = 75
    Width = 121
    Height = 25
    Caption = 'Steady State'
    TabOrder = 8
    OnClick = btnSteadyStateClick
  end
  object edtModelName: TEdit
    Left = 135
    Top = 18
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'ss_threestep.xml'
  end
  object btnLoadTwoModels: TButton
    Left = 262
    Top = 106
    Width = 121
    Height = 25
    Caption = 'Load Two Models'
    TabOrder = 10
    OnClick = btnLoadTwoModelsClick
  end
end
