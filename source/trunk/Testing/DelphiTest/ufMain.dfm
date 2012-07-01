object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Test uRoadRunnerAPI.pas'
  ClientHeight = 708
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
    708)
  PixelsPerInch = 96
  TextHeight = 13
  object lblCommon: TLabel
    Left = 8
    Top = 187
    Width = 76
    Height = 13
    Caption = 'Common double'
  end
  object Label2: TLabel
    Left = 8
    Top = 143
    Width = 80
    Height = 13
    Caption = 'Common Integer'
  end
  object pnlBottm: TPanel
    Left = 0
    Top = 667
    Width = 1068
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 570
    object Label1: TLabel
      Left = 487
      Top = 16
      Width = 66
      Height = 13
      Caption = 'Temp Folder: '
    end
    object lblTempFolder: TEdit
      Left = 555
      Top = 13
      Width = 317
      Height = 21
      TabOrder = 0
      Text = 'C:\'
    end
    object edtProgress: TEdit
      Left = 8
      Top = 12
      Width = 409
      Height = 21
      TabOrder = 1
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
    Height = 667
    Align = alRight
    Anchors = [akLeft, akTop, akBottom]
    DefaultColWidth = 84
    FixedCols = 0
    TabOrder = 3
    ExplicitHeight = 570
  end
  object btnGetAvailableSymbols: TButton
    Left = 328
    Top = 62
    Width = 151
    Height = 25
    Caption = 'Get Available Symbols'
    TabOrder = 4
    OnClick = btnGetAvailableSymbolsClick
  end
  object btnSteadyState: TButton
    Left = 8
    Top = 112
    Width = 121
    Height = 25
    Caption = 'Steady State'
    TabOrder = 5
    OnClick = btnSteadyStateClick
  end
  object btnLoadTwoModels: TButton
    Left = 328
    Top = 200
    Width = 151
    Height = 25
    Caption = 'Load Two Models Test'
    TabOrder = 6
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
    TabOrder = 7
    OnClick = chkConservationLawsClick
  end
  object btnSimulate: TButton
    Left = 8
    Top = 86
    Width = 121
    Height = 25
    Caption = 'Simulate'
    TabOrder = 8
    OnClick = btnSimulateClick
  end
  object btnGetCode: TButton
    Left = 328
    Top = 35
    Width = 151
    Height = 25
    Caption = 'Get Generated Code'
    TabOrder = 9
    OnClick = btnGetCodeClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 232
    Width = 471
    Height = 429
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 10
    ExplicitHeight = 332
    object TabSheet1: TTabSheet
      Caption = 'List Box'
      ExplicitHeight = 304
      object lstSummary: TListBox
        Left = 0
        Top = 0
        Width = 463
        Height = 401
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitHeight = 304
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Source Code'
      ImageIndex = 1
      ExplicitHeight = 304
      object MemoSource: TMemo
        Left = 0
        Top = 0
        Width = 463
        Height = 401
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitHeight = 304
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Header File'
      ImageIndex = 2
      ExplicitHeight = 304
      object memoHeader: TMemo
        Left = 0
        Top = 0
        Width = 463
        Height = 401
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        ExplicitHeight = 304
      end
    end
    object TabSheetCapabilities: TTabSheet
      Caption = 'Capabilities'
      ImageIndex = 3
      ExplicitHeight = 304
      object memoCapabilities: TMemo
        Left = 0
        Top = 0
        Width = 463
        Height = 401
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 304
      end
    end
  end
  object btnSetFloatingSpeciesByIndex: TButton
    Left = 328
    Top = 116
    Width = 151
    Height = 25
    Caption = 'Set Float Species Index'
    TabOrder = 11
    OnClick = btnSetFloatingSpeciesByIndexClick
  end
  object btnSetBoundarySpeciesByIndex: TButton
    Left = 328
    Top = 142
    Width = 151
    Height = 25
    Caption = 'Set Boundary Species Index'
    TabOrder = 12
    OnClick = btnSetBoundarySpeciesByIndexClick
  end
  object edtCommonFloat: TEdit
    Left = 8
    Top = 203
    Width = 121
    Height = 21
    TabOrder = 13
    Text = '0.0'
  end
  object edtCommonInteger: TEdit
    Left = 8
    Top = 159
    Width = 121
    Height = 21
    TabOrder = 14
    Text = '0'
  end
  object btnDisplayModelSumamryByGetValue: TButton
    Left = 135
    Top = 60
    Width = 130
    Height = 36
    Caption = 'Display Model Summary by getValue'
    TabOrder = 15
    WordWrap = True
    OnClick = btnDisplayModelSumamryByGetValueClick
  end
  object Button1: TButton
    Left = 328
    Top = 169
    Width = 151
    Height = 25
    Caption = 'Set Global Parameter Index'
    TabOrder = 16
    OnClick = Button1Click
  end
  object btnGetGlobalParameterIndex: TButton
    Left = 485
    Top = 88
    Width = 151
    Height = 25
    Caption = 'Get Global Parameter Index'
    TabOrder = 17
    OnClick = btnGetGlobalParameterIndexClick
  end
  object btnGetFloatingSpeciesByIndex: TButton
    Left = 485
    Top = 35
    Width = 151
    Height = 25
    Caption = 'Get Float Species Index'
    TabOrder = 18
    OnClick = btnGetFloatingSpeciesByIndexClick
  end
  object btnGetBoundarySpeciesByIndex: TButton
    Left = 485
    Top = 62
    Width = 151
    Height = 25
    Caption = 'Get Boundary Species Index'
    TabOrder = 19
    OnClick = btnGetBoundarySpeciesByIndexClick
  end
  object btnGetSBML: TButton
    Left = 135
    Top = 136
    Width = 130
    Height = 25
    Caption = 'Get SBML and Compare'
    TabOrder = 20
    OnClick = btnGetSBMLClick
  end
  object btnGetCompartmentVolumeByIndex: TButton
    Left = 485
    Top = 8
    Width = 151
    Height = 25
    Caption = 'Get Compartment Vol Index'
    TabOrder = 21
    OnClick = btnGetCompartmentVolumeByIndexClick
  end
  object btnSetCompartmentVolumeByIndex: TButton
    Left = 328
    Top = 89
    Width = 151
    Height = 25
    Caption = 'Set Compartment by Index'
    TabOrder = 22
    OnClick = btnSetCompartmentVolumeByIndexClick
  end
  object btnDisplayModelSumamryByGetIndex: TButton
    Left = 135
    Top = 98
    Width = 130
    Height = 36
    Caption = 'Display Model Summary by getIndex'
    TabOrder = 23
    WordWrap = True
    OnClick = btnDisplayModelSumamryByGetIndexClick
  end
  object btnGetCapabilities: TButton
    Left = 135
    Top = 167
    Width = 130
    Height = 25
    Caption = 'Get Capabilities'
    TabOrder = 24
    OnClick = btnGetCapabilitiesClick
  end
  object btnEvalModel: TButton
    Left = 135
    Top = 201
    Width = 130
    Height = 25
    Caption = 'Eval Model'
    TabOrder = 25
    OnClick = btnEvalModelClick
  end
  object btnGetFullMatrix: TButton
    Left = 485
    Top = 142
    Width = 151
    Height = 25
    Caption = 'Get Full Jacobian'
    TabOrder = 26
    OnClick = btnGetFullMatrixClick
  end
  object btnGetReducedMatrix: TButton
    Left = 485
    Top = 169
    Width = 151
    Height = 25
    Caption = 'Get Reduced Jacobian'
    TabOrder = 27
    OnClick = btnGetReducedMatrixClick
  end
  object lstModelName: TListBox
    Left = 8
    Top = 35
    Width = 121
    Height = 47
    ItemHeight = 13
    Items.Strings = (
      'ss_MinusOneError.xml'
      'ss_threeSpecies.xml')
    TabOrder = 28
  end
end
