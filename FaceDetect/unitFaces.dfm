object formfaces: Tformfaces
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Faces Form'
  ClientHeight = 554
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 0
    Width = 85
    Height = 19
    Caption = 'Saved Faces'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Listface: TListView
    Left = 9
    Top = 19
    Width = 163
    Height = 503
    Columns = <>
    IconOptions.AutoArrange = True
    StyleElements = []
    TabOrder = 0
  end
  object Button1: TButton
    Left = 10
    Top = 528
    Width = 163
    Height = 25
    Caption = 'Delete'
    TabOrder = 1
    OnClick = Button1Click
  end
end
