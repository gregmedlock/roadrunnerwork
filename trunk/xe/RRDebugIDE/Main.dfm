object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 657
  ClientWidth = 1078
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
  object Splitter1: TSplitter
    Left = 273
    Top = 0
    Height = 657
    ExplicitLeft = 216
    ExplicitTop = 32
  end
  object GroupBox1: TGroupBox
    Left = 296
    Top = 96
    Width = 193
    Height = 201
    Caption = 'Actions'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 520
    Top = 112
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 657
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 2
    object fsf: TFileSelectionFrame
      Left = 1
      Top = 1
      Width = 271
      Height = 655
      Align = alClient
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 276
    Top = 0
    Width = 802
    Height = 657
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 276
    Top = 0
    Width = 802
    Height = 657
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Log1: TMemo
        Left = 0
        Top = 297
        Width = 794
        Height = 332
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 794
        Height = 297
        Align = alTop
        TabOrder = 1
        DesignSize = (
          794
          297)
        object Button1: TButton
          Left = 16
          Top = 104
          Width = 129
          Height = 73
          Action = CompileModelA
          TabOrder = 0
        end
        object mModelFileName: TEdit
          Left = 16
          Top = 32
          Width = 761
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 1
          Text = 'mModelFileName'
        end
        object RGLogLevel: TRadioGroup
          Left = 376
          Top = 104
          Width = 209
          Height = 169
          Caption = 'RGLogLevel'
          ItemIndex = 2
          Items.Strings = (
            'Errors'
            'Warnings'
            'Info'
            'Debug'
            'Debug1'
            'Debug2'
            'Debug3'
            'Debug4'
            'Debug5')
          TabOrder = 2
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'XML'
      ImageIndex = 1
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 794
        Height = 629
        Align = alClient
        Lines.Strings = (
          'Memo2')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      object Splitter2: TSplitter
        Left = 393
        Top = 0
        Height = 629
        ExplicitLeft = 440
        ExplicitTop = 216
        ExplicitHeight = 100
      end
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 393
        Height = 629
        Align = alLeft
        Lines.Strings = (
          'Memo3')
        TabOrder = 0
      end
      object Memo4: TMemo
        Left = 396
        Top = 0
        Width = 398
        Height = 629
        Align = alClient
        Lines.Strings = (
          'Memo4')
        TabOrder = 1
        ExplicitLeft = 368
        ExplicitWidth = 426
      end
    end
  end
  object ActionList1: TActionList
    Left = 592
    Top = 64
    object LoadModelA: TAction
      Caption = 'Load'
      OnExecute = LoadModelAExecute
    end
    object CompileModelA: TAction
      Caption = 'Compile'
      OnExecute = CompileModelAExecute
    end
    object updateFileName: TAction
      Caption = 'updateFileName'
    end
  end
end
