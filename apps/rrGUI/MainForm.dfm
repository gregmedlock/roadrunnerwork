object MForm: TMForm
  Left = 0
  Top = 0
  Caption = 'RoadRunner UI'
  ClientHeight = 688
  ClientWidth = 1220
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
    Height = 669
    Align = alLeft
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 1
      Top = 568
      Width = 215
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = -17
      ExplicitTop = 637
    end
    object Splitter3: TSplitter
      Left = 1
      Top = 450
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
      Top = 453
      Width = 215
      Height = 115
      Align = alBottom
      Caption = 'Settings'
      TabOrder = 1
      object mStartTimeE: mtkFloatLabeledEdit
        Left = 15
        Top = 40
        Width = 50
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
        Width = 50
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
        Width = 57
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
      Height = 321
      Align = alClient
      TabOrder = 2
    end
    object SelList: TCheckListBox
      Left = 1
      Top = 571
      Width = 215
      Height = 97
      Align = alBottom
      ItemHeight = 13
      TabOrder = 3
      OnClick = SelListClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 669
    Width = 1220
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 217
    Top = 0
    Width = 1003
    Height = 669
    Align = alClient
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 1
      Top = 387
      Width = 1001
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
      Width = 1001
      Height = 386
      Border.Color = 9423874
      Border.Width = 7
      Gradient.MidColor = clNone
      Legend.Alignment = laBottom
      Legend.Brush.Gradient.Direction = gdTopBottom
      Legend.Brush.Gradient.EndColor = 13556735
      Legend.Brush.Gradient.MidColor = 14739177
      Legend.Brush.Gradient.StartColor = 16774122
      Legend.ColorWidth = 40
      Legend.CustomPosition = True
      Legend.DividingLines.Color = clSilver
      Legend.Font.Color = 6553600
      Legend.Frame.Color = clGray
      Legend.Left = 701
      Legend.LeftPercent = 70
      Legend.LegendStyle = lsSeries
      Legend.PositionUnits = muPercent
      Legend.ResizeChart = False
      Legend.Shadow.Color = 13421772
      Legend.Shadow.Visible = False
      Legend.Symbol.Shadow.Visible = False
      Legend.Symbol.Width = 40
      Legend.Title.Text.Strings = (
        '')
      Legend.Top = 19
      Legend.TopPercent = 5
      Legend.TopPos = 5
      Title.Brush.Gradient.MidColor = clNone
      Title.Color = clBlack
      Title.Frame.Color = 10083835
      Title.Frame.Width = 2
      Title.Shadow.HorizSize = 4
      Title.Shadow.Transparency = 70
      Title.Shadow.VertSize = 4
      Title.Text.Strings = (
        'TChart')
      Title.Visible = False
      BottomAxis.Grid.Visible = False
      BottomAxis.MinorGrid.Color = 15066597
      BottomAxis.Title.Caption = 'Time'
      BottomAxis.Title.Font.Height = -19
      BottomAxis.Title.Font.Name = 'Arial Rounded MT Bold'
      LeftAxis.Grid.Visible = False
      LeftAxis.Title.Caption = 'Species Concentrations'
      LeftAxis.Title.Font.Height = -19
      LeftAxis.Title.Font.Name = 'Arial Rounded MT Bold'
      LeftAxis.Title.ShapeStyle = fosRoundRectangle
      Shadow.Color = clBlack
      Shadow.Visible = False
      TopAxis.MinorGrid.Color = 15066597
      View3D = False
      Zoom.Animated = True
      Zoom.Pen.Color = clRed
      Align = alClient
      Color = clWhite
      PopupMenu = ChartPopup
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitTop = -1
      ColorPaletteIndex = 15
      object Series1: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Frame.Color = clGray
        Marks.Shadow.Color = 13421772
        Marks.Shadow.HorizSize = 2
        Marks.Shadow.VertSize = 2
        Marks.Visible = False
        LinePen.Color = 4210816
        LinePen.Width = 3
        Pointer.Brush.Gradient.EndColor = 10593629
        Pointer.Gradient.EndColor = 10593629
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 390
      Width = 1001
      Height = 278
      Align = alBottom
      Caption = 'Panel3'
      TabOrder = 1
      object mLogMemo: TMemo
        Left = 1
        Top = 58
        Width = 999
        Height = 219
        Align = alClient
        PopupMenu = MemoPopup
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object ToolBar1: TToolBar
        Left = 1
        Top = 1
        Width = 999
        Height = 24
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
      object TeeCommander1: TTeeCommander
        Left = 1
        Top = 25
        Width = 999
        Height = 33
        Align = alTop
        ParentShowHint = False
        TabOrder = 2
      end
    end
  end
  object RRActions: TActionList
    Left = 528
    Top = 512
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
    Left = 712
    Top = 512
  end
  object TVPopupMenu: TPopupMenu
    Left = 144
    Top = 520
    object Load1: TMenuItem
      Action = LoadModelA
    end
    object LogModelFile1: TMenuItem
      Action = logModelFileA
    end
  end
  object MiscActions: TActionList
    Left = 584
    Top = 512
    object ClearMemoA: TAction
      Caption = 'Clear'
      OnExecute = ClearMemoAExecute
    end
  end
  object MemoPopup: TPopupMenu
    Left = 648
    Top = 512
    object Clear1: TMenuItem
      Action = ClearMemoA
    end
  end
  object ChartEditor1: TChartEditor
    Chart = Chart1
    GalleryHeight = 0
    GalleryWidth = 0
    Height = 0
    Width = 0
    Left = 384
    Top = 112
  end
  object ChartPopup: TPopupMenu
    Left = 552
    Top = 144
    object ChartEditor2: TMenuItem
      Caption = 'ChartEditor'
      OnClick = ChartEditor2Click
    end
  end
end
