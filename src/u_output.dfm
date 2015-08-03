object OutputForm: TOutputForm
  Left = 192
  Top = 143
  BorderStyle = bsToolWindow
  Caption = 'OutputSelector'
  ClientHeight = 242
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object rgpDiagramSize: TRadioGroup
    Left = 8
    Top = 8
    Width = 289
    Height = 105
    Caption = 'Diagram Style'
    ItemIndex = 1
    Items.Strings = (
      '1 Diagram, 160 x 160 mm'
      '1 Diagram, 90 x 90 mm'
      '1 Diagram, 100 x 75 mm'
      '1 to 6 Diagrams 75 x 75 mm')
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 56
    Top = 192
    Width = 41
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
  object cbPsLabels: TCheckBox
    Left = 16
    Top = 136
    Width = 81
    Height = 17
    Caption = 'With Labels'
    TabOrder = 2
    Visible = False
  end
  object edX: TEdit
    Left = 152
    Top = 128
    Width = 145
    Height = 21
    TabOrder = 3
    Text = 'X'
  end
  object edY: TEdit
    Left = 152
    Top = 152
    Width = 145
    Height = 21
    TabOrder = 4
    Text = 'Y'
  end
  object edTitle: TEdit
    Left = 152
    Top = 176
    Width = 145
    Height = 21
    TabOrder = 5
    Text = 'Title'
  end
  object cbPsFrame: TCheckBox
    Left = 16
    Top = 152
    Width = 97
    Height = 17
    Caption = 'With Frame'
    TabOrder = 6
  end
  object cbPsSymbol: TCheckBox
    Left = 16
    Top = 168
    Width = 97
    Height = 17
    Caption = 'No Symbols'
    TabOrder = 7
  end
  object cbPsGreyScale: TCheckBox
    Left = 16
    Top = 120
    Width = 97
    Height = 17
    Caption = 'Grey Scale'
    TabOrder = 8
  end
  object cbExcelView: TCheckBox
    Left = 16
    Top = 224
    Width = 97
    Height = 17
    Caption = 'ExcelView'
    TabOrder = 9
  end
end
