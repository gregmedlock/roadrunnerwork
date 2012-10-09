object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 612
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 424
    Align = alLeft
    TabOrder = 0
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
    Width = 578
    Height = 424
    Align = alClient
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 576
      Height = 422
      ActivePage = TabSheet2
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheet2: TTabSheet
        Caption = 'Setup'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 568
          Height = 224
          Align = alTop
          Caption = 'GroupBox1'
          TabOrder = 0
          DesignSize = (
            568
            224)
          object VSBuildRootFolderE: mtkSTDStringEdit
            Left = 16
            Top = 99
            Width = 513
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
            Width = 513
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            EditLabel.Width = 68
            EditLabel.Height = 13
            EditLabel.Caption = 'SandBox Root'
            TabOrder = 1
          end
          object Button6: TButton
            Left = 538
            Top = 33
            Width = 27
            Height = 25
            Action = BrowseForFolderA
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object Button7: TButton
            Left = 538
            Top = 97
            Width = 27
            Height = 25
            Action = BrowseForFolderA
            Anchors = [akTop, akRight]
            TabOrder = 3
          end
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 424
    Width = 691
    Height = 188
    Align = alBottom
    TabOrder = 2
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 689
      Height = 186
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object BuildActions: TActionList
    Left = 528
    Top = 304
    object CreateDownloadPageWiki: TAction
      Caption = 'CreateDownloadPageWiki'
    end
    object SVNUpdate: TAction
      Caption = 'SVN Update'
    end
    object BuildVisualStudioA: TAction
      Caption = 'Build VS'
    end
    object SVNCommitA: TAction
      Caption = 'SVN Commit'
    end
    object BuildCheckA: TAction
      Caption = 'Check'
    end
  end
  object MiscActions: TActionList
    Left = 608
    Top = 184
    object BrowseForFolderA: TAction
      Caption = '...'
      OnExecute = BrowseForFolderAExecute
    end
  end
  object mIniFile: mtkIniFileC
    IniFileName = 'AppIni.ini'
    RootFolder = '.'
    Left = 224
    Top = 296
  end
end
