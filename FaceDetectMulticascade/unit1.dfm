object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Exemplo Facedetect'
  ClientHeight = 528
  ClientWidth = 645
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  SnapBuffer = 100
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object Image1: TImage
    Left = 2
    Top = 3
    Width = 640
    Height = 480
  end
  object Panel1: TPanel
    Left = 0
    Top = 487
    Width = 645
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 7
      Top = 8
      Width = 64
      Height = 19
      Caption = 'Detectar:'
    end
    object RadioButton1: TRadioButton
      Left = 83
      Top = 9
      Width = 58
      Height = 17
      Caption = 'Face'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 140
      Top = 9
      Width = 65
      Height = 17
      Caption = 'Olhos'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object RadioButton3: TRadioButton
      Left = 211
      Top = 9
      Width = 58
      Height = 17
      Caption = 'Boca'
      TabOrder = 2
      OnClick = RadioButton1Click
    end
    object RadioButton4: TRadioButton
      Left = 272
      Top = 9
      Width = 149
      Height = 17
      Caption = 'Face com Ombros'
      TabOrder = 3
      OnClick = RadioButton1Click
    end
    object RadioButton5: TRadioButton
      Left = 426
      Top = 9
      Width = 72
      Height = 17
      Caption = 'Sorriso'
      TabOrder = 4
      OnClick = RadioButton1Click
    end
  end
end
