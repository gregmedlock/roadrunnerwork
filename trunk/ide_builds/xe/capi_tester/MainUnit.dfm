object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RoadRunner C API Tester'
  ClientHeight = 479
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 225
    Height = 479
    Align = alLeft
    TabOrder = 0
    object FileNameE: TEdit
      Left = 8
      Top = 16
      Width = 169
      Height = 21
      TabOrder = 0
      Text = '<select rr dll>'
    end
    object Button1: TButton
      Left = 192
      Top = 14
      Width = 27
      Height = 25
      Action = FileOpen1
      TabOrder = 1
    end
    object Button2: TButton
      Left = 8
      Top = 64
      Width = 75
      Height = 25
      Action = LoadDLL
      TabOrder = 2
    end
    object Button3: TButton
      Left = 102
      Top = 64
      Width = 75
      Height = 25
      Action = UnloadDLL
      TabOrder = 3
    end
    object ListBox1: TListBox
      Left = 1
      Top = 192
      Width = 223
      Height = 286
      Align = alBottom
      ItemHeight = 13
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 225
    Top = 0
    Width = 626
    Height = 479
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 231
    object Memo1: TMemo
      Left = 1
      Top = 288
      Width = 624
      Height = 190
      Align = alBottom
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 80
    Top = 112
    object LoadDLL: TAction
      Caption = 'Load'
      OnExecute = LoadDLLExecute
    end
    object UnloadDLL: TAction
      Caption = 'Unload'
    end
    object SelectDLLA: TAction
      Caption = 'SelectDLLA'
    end
    object FileOpen1: TFileOpen
      Caption = '...'
      Dialog.Filter = '*.dll'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      BeforeExecute = FileOpen1BeforeExecute
      OnAccept = FileOpen1Accept
    end
  end
end
