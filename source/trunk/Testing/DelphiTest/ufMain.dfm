object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Test uRoadRunnerAPI.pas'
  ClientHeight = 611
  ClientWidth = 1068
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
    1068
    611)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCommon: TLabel
    Left = 8
    Top = 161
    Width = 76
    Height = 13
    Caption = 'Common double'
  end
  object Label2: TLabel
    Left = 8
    Top = 117
    Width = 80
    Height = 13
    Caption = 'Common Integer'
  end
  object pnlBottm: TPanel
    Left = 0
    Top = 570
    Width = 1068
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 522
    ExplicitWidth = 907
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
    Left = 328
    Top = 8
    Width = 151
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
    Left = 646
    Top = 0
    Width = 422
    Height = 570
    Align = alRight
    Anchors = [akLeft, akTop, akBottom]
    DefaultColWidth = 84
    FixedCols = 0
    TabOrder = 3
    ExplicitLeft = 485
    ExplicitHeight = 522
  end
  object btnGetReactionNames: TButton
    Left = 328
    Top = 61
    Width = 151
    Height = 25
    Caption = 'Get Reaction Names'
    TabOrder = 4
    OnClick = btnGetReactionNamesClick
  end
  object btnGetAvailableSymbols: TButton
    Left = 328
    Top = 88
    Width = 151
    Height = 25
    Caption = 'Get Available Symbols'
    TabOrder = 5
    OnClick = btnGetAvailableSymbolsClick
  end
  object btnSteadyState: TButton
    Left = 8
    Top = 86
    Width = 121
    Height = 25
    Caption = 'Steady State'
    TabOrder = 6
    OnClick = btnSteadyStateClick
  end
  object edtModelName: TEdit
    Left = 8
    Top = 37
    Width = 121
    Height = 21
    TabOrder = 7
    Text = 'ss_MinusOneError.xml'
  end
  object btnLoadTwoModels: TButton
    Left = 8
    Top = 201
    Width = 121
    Height = 25
    Caption = 'Load Two Models Test'
    TabOrder = 8
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
    TabOrder = 9
    OnClick = chkConservationLawsClick
  end
  object btnSimulate: TButton
    Left = 8
    Top = 60
    Width = 121
    Height = 25
    Caption = 'Simulate'
    TabOrder = 10
    OnClick = btnSimulateClick
  end
  object btnGetCode: TButton
    Left = 328
    Top = 35
    Width = 151
    Height = 25
    Caption = 'Get Generated Code'
    TabOrder = 11
    OnClick = btnGetCodeClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 232
    Width = 471
    Height = 332
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 12
    ExplicitHeight = 322
    object TabSheet1: TTabSheet
      Caption = 'List Box'
      ExplicitHeight = 289
      object lstSummary: TListBox
        Left = 0
        Top = 0
        Width = 463
        Height = 304
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitHeight = 289
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Source Code'
      ImageIndex = 1
      ExplicitHeight = 289
      object MemoSource: TMemo
        Left = 0
        Top = 0
        Width = 463
        Height = 304
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitHeight = 289
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Header File'
      ImageIndex = 2
      ExplicitHeight = 289
      object memoHeader: TMemo
        Left = 0
        Top = 0
        Width = 463
        Height = 304
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitHeight = 289
      end
    end
  end
  object btnSetFloatingSpeciesByIndex: TButton
    Left = 328
    Top = 115
    Width = 151
    Height = 25
    Caption = 'Set Float Species Index'
    TabOrder = 13
    OnClick = btnSetFloatingSpeciesByIndexClick
  end
  object btnSetBoundarySpeciesByIndex: TButton
    Left = 328
    Top = 141
    Width = 151
    Height = 25
    Caption = 'Set Boundary Species Index'
    TabOrder = 14
    OnClick = btnSetBoundarySpeciesByIndexClick
  end
  object edtCommonFloat: TEdit
    Left = 8
    Top = 177
    Width = 121
    Height = 22
    TabOrder = 15
    Text = '0.0'
  end
  object edtCommonInteger: TEdit
    Left = 8
    Top = 133
    Width = 121
    Height = 22
    TabOrder = 16
    Text = '0'
  end
  object btnDisplayModelSumamry: TButton
    Left = 135
    Top = 60
    Width = 121
    Height = 25
    Caption = 'Display Model Summary'
    TabOrder = 17
    OnClick = btnDisplayModelSumamryClick
  end
  object Button1: TButton
    Left = 328
    Top = 168
    Width = 151
    Height = 25
    Caption = 'Set Global Parameter Index'
    TabOrder = 18
  end
  object btnGetGlobalParameterIndex: TButton
    Left = 485
    Top = 61
    Width = 151
    Height = 25
    Caption = 'Get Global Parameter Index'
    TabOrder = 19
    OnClick = btnGetGlobalParameterIndexClick
  end
  object btnGetFloatingSpeciesByIndex: TButton
    Left = 485
    Top = 8
    Width = 151
    Height = 25
    Caption = 'Get Float Species Index'
    Enabled = False
    TabOrder = 20
  end
  object btnGetBoundarySpeciesByIndex: TButton
    Left = 485
    Top = 35
    Width = 151
    Height = 25
    Caption = 'Get Boundary Species Index'
    Enabled = False
    TabOrder = 21
  end
end
