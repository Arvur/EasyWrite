object TntLXSafeWinInetAdvancedForm: TTntLXSafeWinInetAdvancedForm
  Left = 325
  Top = 257
  BorderStyle = bsDialog
  Caption = 'Advanced Internet Connection Settings'
  ClientHeight = 228
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = TntFormLXCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MaxAttemptsLbl: TTntLabel
    Left = 191
    Top = 58
    Width = 90
    Height = 13
    Caption = 'Maximum attempts'
    Enabled = False
  end
  object UrlToUseLbl: TTntLabel
    Left = 191
    Top = 86
    Width = 52
    Height = 13
    Caption = 'URL to use'
    Enabled = False
  end
  object DelayLbl: TTntLabel
    Left = 191
    Top = 113
    Width = 156
    Height = 13
    Caption = 'Delay (msec) after each attempt'
    Enabled = False
  end
  object TntBevel1: TTntBevel
    Left = 8
    Top = 175
    Width = 369
    Height = 2
  end
  object InternetConnectionLbl: TTntLabel
    Left = 8
    Top = 8
    Width = 252
    Height = 13
    Caption = 'Detecting Availability of Internet Connection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object AutoPrimeEnableBox: TTntCheckBox
    Left = 8
    Top = 32
    Width = 361
    Height = 17
    Caption = 'Force preliminary connection attempts.'
    TabOrder = 0
    OnClick = AutoPrimeEnableBoxClick
  end
  object AutoPrimeMaxAttemptsEdit: TSpinEdit
    Left = 32
    Top = 55
    Width = 153
    Height = 22
    Enabled = False
    MaxValue = 12
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object AutoPrimeUrlEdit: TTntEdit
    Left = 32
    Top = 83
    Width = 153
    Height = 21
    Enabled = False
    MaxLength = 240
    TabOrder = 2
    Text = 'http://www.google.com/'
  end
  object AutoPrimeDelayEdit: TSpinEdit
    Left = 32
    Top = 110
    Width = 153
    Height = 22
    Enabled = False
    MaxValue = 2000
    MinValue = 100
    TabOrder = 3
    Value = 100
  end
  object AlwaysReportConnectionBox: TTntCheckBox
    Left = 8
    Top = 148
    Width = 361
    Height = 17
    Caption = 'Always report an Internet connection as being available.'
    TabOrder = 4
  end
  object CancelBtn: TTntButton
    Left = 294
    Top = 190
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object OKBtn: TTntButton
    Left = 206
    Top = 190
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
end
