object TntMemoInputQueryFrm: TTntMemoInputQueryFrm
  Left = 179
  Top = 163
  BorderStyle = bsDialog
  Caption = 'TntMemoInputQueryFrm'
  ClientHeight = 413
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PromptLbl: TTntLabel
    Left = 16
    Top = 8
    Width = 47
    Height = 13
    Caption = 'PromptLbl'
  end
  object TntMemo1: TTntMemo
    Left = 16
    Top = 32
    Width = 561
    Height = 329
    Lines.Strings = (
      'TntMemo1')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object OKBtn: TTntButton
    Left = 306
    Top = 376
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TTntButton
    Left = 210
    Top = 376
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
