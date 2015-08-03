object EditForm: TEditForm
  Left = 145
  Top = 143
  Width = 550
  Height = 319
  Caption = 'Import Files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnSavX: TButton
    Left = 80
    Top = 0
    Width = 89
    Height = 25
    Caption = 'Save As X Data'
    TabOrder = 5
    Visible = False
    OnClick = btnSavXClick
  end
  object Memo: TMemo
    Left = 0
    Top = 24
    Width = 537
    Height = 265
    Lines.Strings = (
      'Memo')
    TabOrder = 0
    WordWrap = False
  end
  object btnPaste: TButton
    Left = 0
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Coller'
    TabOrder = 1
    Visible = False
    OnClick = btnPasteClick
  end
  object btnSavLis: TButton
    Left = 80
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Save As List'
    TabOrder = 2
    Visible = False
    OnClick = btnSavLisClick
  end
  object btnSavLXY: TButton
    Left = 80
    Top = 0
    Width = 105
    Height = 25
    Caption = 'Save As Single File'
    TabOrder = 3
    Visible = False
    OnClick = btnSavLXYClick
  end
  object BitBtnOK: TBitBtn
    Left = 464
    Top = 0
    Width = 75
    Height = 25
    TabOrder = 4
    OnClick = BitBtnOKClick
    Kind = bkOK
  end
  object btnSavY: TButton
    Left = 176
    Top = 0
    Width = 89
    Height = 25
    Caption = 'Save as Y Data'
    TabOrder = 6
    Visible = False
    OnClick = btnSavYClick
  end
  object btnSavDat: TButton
    Left = 160
    Top = 0
    Width = 137
    Height = 25
    Caption = 'Save As Data'
    TabOrder = 7
    Visible = False
    OnClick = btnSavDatClick
  end
end
