object MForm: TMForm
  Left = 0
  Top = 0
  Caption = 'RoadRunner UI'
  ClientHeight = 728
  ClientWidth = 1217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 709
    Align = alLeft
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 1
      Top = 608
      Width = 215
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = -17
      ExplicitTop = 637
    end
    object Splitter3: TSplitter
      Left = 1
      Top = 490
      Width = 215
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 4
      ExplicitTop = 529
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 215
      Height = 128
      Align = alTop
      Caption = 'Settings'
      TabOrder = 0
      object RadioGroup1: TRadioGroup
        Left = 3
        Top = 16
        Width = 94
        Height = 49
        Caption = 'Model Compiler'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'tcc'
          'bcc')
        TabOrder = 0
      end
      object Button1: TButton
        Left = 174
        Top = 94
        Width = 23
        Height = 25
        Action = selectModelsFolder
        TabOrder = 1
      end
      object modelFoldersCB: TComboBox
        Left = 3
        Top = 96
        Width = 165
        Height = 21
        AutoComplete = False
        AutoCloseUp = True
        Style = csDropDownList
        ParentShowHint = False
        ShowHint = False
        TabOrder = 2
        OnChange = modelFoldersCBChange
        OnSelect = modelFoldersCBSelect
      end
    end
    object GroupBox3: TGroupBox
      Left = 1
      Top = 493
      Width = 215
      Height = 115
      Align = alBottom
      Caption = 'Settings'
      TabOrder = 1
      ExplicitTop = 456
      object mStartTimeE: mtkFloatLabeledEdit
        Left = 15
        Top = 40
        Width = 74
        Height = 21
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'Start Time'
        Enabled = False
        TabOrder = 0
        Text = '0.00'
      end
      object mEndTimeE: mtkFloatLabeledEdit
        Left = 15
        Top = 80
        Width = 74
        Height = 21
        EditLabel.Width = 43
        EditLabel.Height = 13
        EditLabel.Caption = 'End Time'
        Enabled = False
        TabOrder = 1
        Text = '0.00'
      end
      object mNrOfSimulationPointsE: mtkIntLabeledEdit
        Left = 111
        Top = 41
        Width = 74
        Height = 21
        EditLabel.Width = 56
        EditLabel.Height = 13
        EditLabel.Caption = 'Nr of Points'
        Enabled = False
        TabOrder = 2
        Text = '0'
      end
    end
    object TFileSelectionFrame1: TFileSelectionFrame
      Left = 1
      Top = 129
      Width = 215
      Height = 361
      Align = alClient
      TabOrder = 2
      ExplicitHeight = 324
    end
    object SelList: TCheckListBox
      Left = 1
      Top = 611
      Width = 215
      Height = 97
      Align = alBottom
      ItemHeight = 13
      TabOrder = 3
      ExplicitLeft = 32
      ExplicitTop = 648
      ExplicitWidth = 121
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 709
    Width = 1217
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 217
    Top = 0
    Width = 1000
    Height = 709
    Align = alClient
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 1
      Top = 341
      Width = 998
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 470
    end
    object Chart1: TChart
      Left = 1
      Top = 1
      Width = 998
      Height = 340
      Border.Color = clNavy
      Border.Visible = True
      BorderRound = 10
      Gradient.EndColor = clGray
      Gradient.Visible = True
      Legend.Brush.Gradient.Direction = gdTopBottom
      Legend.Brush.Gradient.EndColor = clYellow
      Legend.Brush.Gradient.StartColor = clWhite
      Legend.Brush.Gradient.Visible = True
      Title.Text.Strings = (
        'TChart')
      View3D = False
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ColorPaletteIndex = 13
    end
    object Panel3: TPanel
      Left = 1
      Top = 344
      Width = 998
      Height = 364
      Align = alBottom
      Caption = 'Panel3'
      TabOrder = 1
      object mLogMemo: TMemo
        Left = 1
        Top = 30
        Width = 996
        Height = 333
        Align = alClient
        PopupMenu = MemoPopup
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object ToolBar1: TToolBar
        Left = 1
        Top = 1
        Width = 996
        Height = 29
        ButtonHeight = 21
        ButtonWidth = 32
        Caption = 'ToolBar1'
        ShowCaptions = True
        TabOrder = 1
        object ToolButton1: TToolButton
          Left = 0
          Top = 0
          Action = ClearMemoA
        end
        object Button2: TButton
          Left = 32
          Top = 0
          Width = 94
          Height = 21
          Action = SimulateA
          TabOrder = 0
        end
      end
    end
  end
  object RRActions: TActionList
    Left = 320
    Top = 464
    object CompileA: TAction
      Caption = 'CompileA'
    end
    object selectModelsFolder: TAction
      Caption = '...'
      OnExecute = selectModelsFolderExecute
    end
    object LoadFromTreeViewA: TAction
      Caption = 'Load'
      OnExecute = LoadFromTreeViewAExecute
    end
    object logModelFileA: TAction
      Caption = 'Log Model File'
      OnExecute = logModelFileAExecute
    end
    object LoadModelA: TAction
      Caption = 'Load'
      OnExecute = LoadModelAExecute
    end
    object SimulateA: TAction
      Caption = 'Simulate'
      Enabled = False
      OnExecute = SimulateAExecute
    end
    object loadAvailableSymbolsA: TAction
      Caption = 'loadAvailableSymbolsA'
      OnExecute = loadAvailableSymbolsAExecute
    end
  end
  object mIniFileC: mtkIniFileC
    IniFileName = 'RR.ini'
    RootFolder = '.'
    Left = 144
    Top = 304
  end
  object startupTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = startupTimerTimer
    Left = 528
    Top = 344
  end
  object TVPopupMenu: TPopupMenu
    Left = 128
    Top = 528
    object Load1: TMenuItem
      Action = LoadModelA
    end
    object LogModelFile1: TMenuItem
      Action = logModelFileA
    end
  end
  object MiscActions: TActionList
    Left = 592
    Top = 496
    object ClearMemoA: TAction
      Caption = 'Clear'
      OnExecute = ClearMemoAExecute
    end
  end
  object MemoPopup: TPopupMenu
    Left = 808
    Top = 480
    object Clear1: TMenuItem
      Action = ClearMemoA
    end
  end
end
