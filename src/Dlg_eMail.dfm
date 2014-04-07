object DlgEMail: TDlgEMail
  Left = 441
  Top = 215
  Width = 328
  Height = 102
  ActiveControl = edt_EMail
  AlphaBlend = True
  AlphaBlendValue = 180
  BorderStyle = bsSizeToolWin
  Caption = 'Insert eMail ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pnl_Dialog: TTBXToolWindow
    Left = 0
    Top = 0
    Width = 320
    Height = 75
    Align = alClient
    Caption = 'pnl_Dialog'
    ClientAreaHeight = 75
    ClientAreaWidth = 320
    TabOrder = 0
    DesignSize = (
      320
      75)
    object pnl_Buttons: TTBXAlignmentPanel
      Left = 0
      Top = 40
      Width = 320
      Height = 35
      Align = alBottom
      TabOrder = 0
      object btn_Cancel: TSpTBXButton
        Left = 250
        Top = 0
        Width = 70
        Height = 35
        Caption = 'Cancel'
        Align = alRight
        Margins.Left = 2
        Margins.Top = 8
        Margins.Right = 2
        Margins.Bottom = 3
        TabOrder = 0
        Cancel = True
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Sans Serif'
        LinkFont.Style = [fsUnderline]
        ModalResult = 2
        ThemeType = thtTBX
      end
      object btn_Ok: TSpTBXButton
        Left = 180
        Top = 0
        Width = 70
        Height = 35
        Caption = 'Ok'
        Align = alRight
        Margins.Left = 2
        Margins.Top = 8
        Margins.Right = 2
        Margins.Bottom = 3
        TabOrder = 1
        OnClick = ProcessEMail
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Sans Serif'
        LinkFont.Style = [fsUnderline]
        ModalResult = 1
        ThemeType = thtTBX
      end
      object pnl_Protect: TTBXAlignmentPanel
        Left = 0
        Top = 0
        Width = 169
        Height = 35
        Align = alLeft
        TabOrder = 2
        object chk_Protect: TSpTBXCheckBox
          Left = 0
          Top = 0
          Width = 169
          Height = 15
          Caption = 'Protect eMail'
          Align = alTop
          Margins.Left = 5
          Margins.Right = 5
          TabOrder = 0
          OnClick = CheckBoxGroupping
          Checked = True
          State = cbChecked
        end
        object chk_Aggresive: TSpTBXCheckBox
          Left = 0
          Top = 15
          Width = 169
          Height = 15
          Hint = 'Inserts image instead'
          Caption = 'Aggresive protection!'
          Align = alTop
          Margins.Left = 10
          Margins.Right = 5
          TabOrder = 1
          Checked = True
          State = cbChecked
        end
      end
    end
    object edt_EMail: TSpTBXEdit
      Left = 8
      Top = 8
      Width = 304
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object DKLang: TDKLanguageController
    IgnoreList.Strings = (
      'btn_*'
      'pnl_*'
      'PropStorage.*')
    SectionName = 'DlgEMail'
    Left = 272
    Top = 8
    LangData = {
      0800446C67454D61696C010100000001000000070043617074696F6E01090000
      000A00706E6C5F4469616C6F6700000B00706E6C5F427574746F6E7300000A00
      62746E5F43616E63656C0000060062746E5F4F6B00000B00706E6C5F50726F74
      65637400000B0063686B5F50726F746563740101000000080000000700436170
      74696F6E000D0063686B5F41676772657369766501020000000A000000070043
      617074696F6E09000000040048696E740009006564745F454D61696C00000B00
      50726F7053746F726167650000}
  end
  object PropStorage: TPropStorageEh
    Section = 'DlgEMail'
    StorageManager = DmMain.PropStorMan
    StoredProps.Strings = (
      'pnl_Dialog.pnl_Buttons.pnl_Protect.chk_Aggresive.<P>.Checked'
      'pnl_Dialog.pnl_Buttons.pnl_Protect.chk_Protect.<P>.Checked')
    Left = 239
    Top = 8
  end
end
