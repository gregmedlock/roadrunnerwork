object MForm: TMForm
  Left = 0
  Top = 0
  Caption = 'RoadRunner UI'
  ClientHeight = 699
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
    Height = 680
    Align = alLeft
    TabOrder = 0
    object Splitter3: TSplitter
      Left = 1
      Top = 429
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
      Caption = 'Model Selection'
      TabOrder = 0
      object Button1: TButton
        Left = 174
        Top = 21
        Width = 23
        Height = 25
        Action = selectModelsFolder
        TabOrder = 0
      end
      object modelFoldersCB: TComboBox
        Left = 8
        Top = 23
        Width = 160
        Height = 21
        AutoComplete = False
        AutoCloseUp = True
        Style = csDropDownList
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        OnChange = modelFoldersCBChange
        OnSelect = modelFoldersCBSelect
      end
      object filterEdit: mtkSTDStringEdit
        Left = 8
        Top = 64
        Width = 121
        Height = 21
        EditLabel.Width = 24
        EditLabel.Height = 13
        EditLabel.Caption = 'Filter'
        TabOrder = 2
        Text = '*.xml'
        OnKeyDown = filterEditKeyDown
        Value = '*.xml'
      end
      object Button4: TButton
        Left = 8
        Top = 97
        Width = 75
        Height = 25
        Caption = 'Button4'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
    object TFileSelectionFrame1: TFileSelectionFrame
      Left = 1
      Top = 129
      Width = 215
      Height = 300
      Align = alClient
      TabOrder = 1
    end
    object Panel4: TPanel
      Left = 1
      Top = 432
      Width = 215
      Height = 247
      Align = alBottom
      TabOrder = 2
      object GroupBox3: TGroupBox
        Left = 1
        Top = 1
        Width = 213
        Height = 64
        Align = alTop
        Caption = 'Simulation Settings'
        TabOrder = 0
        object mNrOfSimulationPointsE: mtkIntLabeledEdit
          Left = 139
          Top = 35
          Width = 57
          Height = 21
          EditLabel.Width = 56
          EditLabel.Height = 13
          EditLabel.Caption = 'Nr of Points'
          Enabled = False
          TabOrder = 0
          Text = '0'
        end
        object mStartTimeE: mtkFloatLabeledEdit
          Left = 7
          Top = 35
          Width = 50
          Height = 21
          EditLabel.Width = 49
          EditLabel.Height = 13
          EditLabel.Caption = 'Start Time'
          Enabled = False
          TabOrder = 1
          Text = '0.00'
        end
        object mEndTimeE: mtkFloatLabeledEdit
          Left = 71
          Top = 35
          Width = 50
          Height = 21
          EditLabel.Width = 43
          EditLabel.Height = 13
          EditLabel.Caption = 'End Time'
          Enabled = False
          TabOrder = 2
          Text = '0.00'
        end
      end
      object Panel5: TPanel
        Left = 1
        Top = 65
        Width = 213
        Height = 181
        Align = alClient
        TabOrder = 1
        object SelList: TCheckListBox
          Left = 1
          Top = 1
          Width = 79
          Height = 179
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = SelListClick
        end
        object GroupBox2: TGroupBox
          Left = 80
          Top = 1
          Width = 132
          Height = 179
          Align = alRight
          Caption = 'Model'
          TabOrder = 1
          object mModelNameLbl: TLabel
            Left = 24
            Top = 16
            Width = 58
            Height = 13
            Caption = 'Model Name'
          end
          object Button2: TButton
            Left = 24
            Top = 136
            Width = 75
            Height = 25
            Action = UnLoadModelA
            TabOrder = 0
          end
          object Button3: TButton
            Left = 24
            Top = 96
            Width = 75
            Height = 25
            Action = SimulateA
            TabOrder = 1
          end
          object Button5: TButton
            Left = 24
            Top = 35
            Width = 75
            Height = 25
            Action = LoadFromTreeViewA
            TabOrder = 2
          end
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 680
    Width = 1220
    Height = 19
    Panels = <>
  end
  object Panel2: TPanel
    Left = 217
    Top = 0
    Width = 1003
    Height = 680
    Align = alClient
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 1
      Top = 426
      Width = 1001
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 17
      ExplicitTop = 452
    end
    object Chart1: TChart
      Left = 1
      Top = 1
      Width = 1001
      Height = 425
      Border.Color = 9423874
      Border.Width = 7
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
      Legend.Top = 21
      Legend.TopPercent = 5
      Legend.TopPos = 5
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
    object PageControl1: TPageControl
      Left = 1
      Top = 429
      Width = 1001
      Height = 250
      ActivePage = TabSheet2
      Align = alBottom
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'Log'
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 993
          Height = 222
          Align = alClient
          Caption = 'Panel3'
          TabOrder = 0
          object mLogMemo: TMemo
            Left = 1
            Top = 22
            Width = 991
            Height = 199
            Align = alClient
            PopupMenu = MemoPopup
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
          end
          object ToolBar1: TToolBar
            Left = 1
            Top = 1
            Width = 991
            Height = 21
            AutoSize = True
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
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Settings'
        ImageIndex = 1
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
    object UnLoadModelA: TAction
      Caption = 'Unload'
      OnExecute = UnLoadModelAExecute
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
    Left = 112
    Top = 208
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
