object TntCancelableForm: TTntCancelableForm
  Left = 394
  Top = 283
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Please wait'
  ClientHeight = 72
  ClientWidth = 121
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MsgLbl: TTntLabel
    Left = 28
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Please wait...'
  end
  object CancelBtn: TTntButton
    Left = 23
    Top = 36
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelBtnClick
  end
end
