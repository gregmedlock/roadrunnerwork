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
    Width = 305
    Height = 709
    Align = alLeft
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 303
      Height = 169
      Align = alTop
      Caption = 'Settings'
      TabOrder = 0
      object RadioGroup1: TRadioGroup
        Left = 3
        Top = 16
        Width = 129
        Height = 65
        Caption = 'Model Compiler'
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
    object GroupBox2: TGroupBox
      Left = 1
      Top = 571
      Width = 303
      Height = 137
      Align = alBottom
      Caption = 'Actions'
      TabOrder = 1
      object Button2: TButton
        Left = 3
        Top = 16
        Width = 94
        Height = 41
        Caption = 'Button2'
        TabOrder = 0
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 3
        Top = 72
        Width = 94
        Height = 41
        Caption = 'Button3'
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 1
      Top = 456
      Width = 303
      Height = 115
      Align = alBottom
      Caption = 'Settings'
      TabOrder = 2
      object mtkFloatLabeledEdit1: mtkFloatLabeledEdit
        Left = 15
        Top = 40
        Width = 74
        Height = 21
        EditLabel.Width = 49
        EditLabel.Height = 13
        EditLabel.Caption = 'Start Time'
        TabOrder = 0
        Text = '0.00'
      end
      object mtkFloatLabeledEdit2: mtkFloatLabeledEdit
        Left = 15
        Top = 80
        Width = 74
        Height = 21
        EditLabel.Width = 43
        EditLabel.Height = 13
        EditLabel.Caption = 'End Time'
        TabOrder = 1
        Text = '0.00'
      end
      object mtkIntLabeledEdit1: mtkIntLabeledEdit
        Left = 111
        Top = 41
        Width = 74
        Height = 21
        EditLabel.Width = 56
        EditLabel.Height = 13
        EditLabel.Caption = 'Nr of Points'
        TabOrder = 2
        Text = '0'
      end
    end
    object TFileSelectionFrame1: TFileSelectionFrame
      Left = 1
      Top = 170
      Width = 303
      Height = 286
      Align = alClient
      TabOrder = 3
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
    Left = 305
    Top = 0
    Width = 912
    Height = 709
    Align = alClient
    TabOrder = 2
    object mLogMemo: TMemo
      Left = 1
      Top = 471
      Width = 910
      Height = 237
      Align = alBottom
      TabOrder = 0
    end
    object Chart1: TChart
      Left = 1
      Top = 1
      Width = 910
      Height = 470
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
      TabOrder = 1
      ColorPaletteIndex = 13
    end
  end
  object ActionList1: TActionList
    Left = 216
    Top = 456
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
  end
  object mIniFileC: mtkIniFileC
    IniFileName = 'RR.ini'
    RootFolder = '.'
    Left = 208
    Top = 312
  end
  object startupTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = startupTimerTimer
    Left = 528
    Top = 344
  end
end
