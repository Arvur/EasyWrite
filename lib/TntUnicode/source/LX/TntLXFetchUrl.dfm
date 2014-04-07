object TntWebDownloadProgressForm: TTntWebDownloadProgressForm
  Left = 266
  Top = 128
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Downloading file from Web...'
  ClientHeight = 86
  ClientWidth = 290
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TTntLabel
    Left = 6
    Top = 6
    Width = 223
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Connecting...'
  end
  object CancelBtn: TTntButton
    Left = 80
    Top = 53
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 3
    TabOrder = 0
  end
  object Animate1: TAnimate
    Left = 241
    Top = 0
    Width = 16
    Height = 16
    StopFrame = 8
  end
  object ProgressBar1: TTntProgressBar
    Left = 42
    Top = 28
    Width = 150
    Height = 16
    Smooth = True
    TabOrder = 2
    Visible = False
  end
end
