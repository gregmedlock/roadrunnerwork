object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test uRoadRunnerAPI.pas'
  ClientHeight = 339
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Load DLL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object pnlBottm: TPanel
    Left = 0
    Top = 298
    Width = 481
    Height = 41
    Align = alBottom
    TabOrder = 1
    object lblProgress: TLabel
      Left = 8
      Top = 16
      Width = 13
      Height = 13
      Caption = 'Ok'
    end
  end
  object btnGetCopyright: TButton
    Left = 24
    Top = 47
    Width = 89
    Height = 25
    Caption = 'Get Copyright'
    TabOrder = 2
    OnClick = btnGetCopyrightClick
  end
end
