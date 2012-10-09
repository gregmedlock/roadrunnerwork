object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 626
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 413
    Width = 884
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 1
    ExplicitTop = 1
    ExplicitWidth = 414
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 413
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 438
    object Button1: TButton
      Left = 16
      Top = 91
      Width = 75
      Height = 25
      Action = SVNUpdate
      TabOrder = 0
    end
    object Button2: TButton
      Left = 16
      Top = 122
      Width = 75
      Height = 25
      Action = BuildVisualStudioA
      TabOrder = 1
    end
    object Button3: TButton
      Left = 16
      Top = 153
      Width = 75
      Height = 25
      Action = CreateDownloadPageWiki
      Caption = 'Update Wiki'
      TabOrder = 2
    end
    object Button4: TButton
      Left = 16
      Top = 184
      Width = 75
      Height = 25
      Action = SVNCommitA
      TabOrder = 3
    end
    object Button5: TButton
      Left = 16
      Top = 25
      Width = 75
      Height = 25
      Action = BuildCheckA
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 113
    Top = 0
    Width = 771
    Height = 413
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 438
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 769
      Height = 411
      ActivePage = TabSheet2
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 436
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
        ExplicitHeight = 408
      end
      object TabSheet2: TTabSheet
        Caption = 'Setup'
        ImageIndex = 1
        ExplicitHeight = 408
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 761
          Height = 224
          Align = alTop
          Caption = 'GroupBox1'
          TabOrder = 0
          DesignSize = (
            761
            224)
          object VSBuildRootFolderE: mtkSTDStringEdit
            Left = 16
            Top = 84
            Width = 706
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 144
            EditLabel.Height = 13
            EditLabel.Caption = 'Visual Studio Build Root Folder'
            TabOrder = 0
          end
          object SandBoxFolderE: mtkSTDStringEdit
            Left = 16
            Top = 35
            Width = 706
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 68
            EditLabel.Height = 13
            EditLabel.Caption = 'SandBox Root'
            TabOrder = 1
          end
          object SandBoxBtn: TButton
            Left = 731
            Top = 33
            Width = 27
            Height = 25
            Action = BrowseForFolderA
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object VSBuildBtn: TButton
            Left = 731
            Top = 82
            Width = 27
            Height = 25
            Action = BrowseForFolderA
            Anchors = [akTop, akRight]
            TabOrder = 3
          end
          object svnExecutableE: mtkSTDStringEdit
            Left = 16
            Top = 138
            Width = 706
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 75
            EditLabel.Height = 13
            EditLabel.Caption = 'SVN Executable'
            TabOrder = 4
          end
          object svnExecutableBtn: TButton
            Left = 731
            Top = 136
            Width = 27
            Height = 25
            Action = BrowseForFolderA
            Anchors = [akTop, akRight]
            TabOrder = 5
          end
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 416
    Width = 884
    Height = 191
    Align = alBottom
    TabOrder = 2
    object mLogMemo: TMemo
      Left = 1
      Top = 1
      Width = 882
      Height = 189
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
      ExplicitLeft = 73
      ExplicitTop = -23
      ExplicitHeight = 186
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 607
    Width = 884
    Height = 19
    Panels = <>
    ExplicitLeft = 8
  end
  object BuildActions: TActionList
    Left = 528
    Top = 304
    object CreateDownloadPageWiki: TAction
      Caption = 'CreateDownloadPageWiki'
    end
    object SVNUpdate: TAction
      Caption = 'SVN Update'
      OnExecute = SVNUpdateExecute
    end
    object BuildVisualStudioA: TAction
      Caption = 'Build VS'
    end
    object SVNCommitA: TAction
      Caption = 'SVN Commit'
    end
    object BuildCheckA: TAction
      Caption = 'Check'
      OnExecute = BuildCheckAExecute
    end
  end
  object MiscActions: TActionList
    Left = 608
    Top = 184
    object BrowseForFolderA: TAction
      Caption = '...'
      OnExecute = BrowseForFolderAExecute
    end
    object BrowseForFileA: TAction
      Caption = '...'
    end
  end
  object mIniFile: mtkIniFileC
    IniFileName = 'AppIni.ini'
    RootFolder = '.'
    Left = 224
    Top = 296
  end
  object ShutDownTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ShutDownTimerTimer
    Left = 632
    Top = 280
  end
  object BrowseForFileDlg: TFileOpenDialog
    DefaultExtension = '*.exe'
    FavoriteLinks = <>
    FileName = 'C:\Program Files\TortoiseSVN\bin\svn.exe'
    FileTypes = <>
    Options = []
    Left = 336
    Top = 272
  end
  object BrowseForFolderDlg: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 360
    Top = 376
  end
end
