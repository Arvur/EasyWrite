object DlgLinks: TDlgLinks
  Left = 441
  Top = 215
  Width = 328
  Height = 179
  ActiveControl = memo_Links
  AlphaBlend = True
  AlphaBlendValue = 180
  BorderStyle = bsSizeToolWin
  Caption = 'Insert Link(s) ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object pnl_Dialog: TTBXToolWindow
    Left = 0
    Top = 0
    Width = 320
    Height = 152
    Align = alClient
    Caption = 'pnl_Dialog'
    ClientAreaHeight = 152
    ClientAreaWidth = 320
    TabOrder = 0
    object pnl_Buttons: TTBXAlignmentPanel
      Left = 0
      Top = 117
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
        OnClick = ProcessLinks
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
          Caption = 'Protect links ...'
          Align = alTop
          Margins.Left = 5
          Margins.Right = 5
          TabOrder = 0
          OnClick = CheckBoxGroupping
          Checked = True
          State = cbChecked
        end
        object chk_Friendly: TSpTBXCheckBox
          Left = 0
          Top = 15
          Width = 169
          Height = 15
          Hint = 'Using _http instead of hxxp, etc'
          Caption = 'DragDrop friendly protection'
          Align = alTop
          Margins.Left = 10
          Margins.Right = 5
          TabOrder = 1
          OnClick = CheckBoxGroupping
        end
      end
    end
    object pnl_Memo: TTBXAlignmentPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 117
      Align = alClient
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 1
      object memo_Links: TTntMemo
        Left = 2
        Top = 2
        Width = 316
        Height = 113
        Align = alClient
        BorderStyle = bsNone
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object DKLang: TDKLanguageController
    IgnoreList.Strings = (
      'btn_*'
      'pnl_*'
      'PropStorage.*')
    SectionName = 'DlgLinks'
    Left = 272
    Top = 8
    LangData = {
      0800446C674C696E6B73010100000001000000070043617074696F6E010A0000
      000A00706E6C5F4469616C6F6700000B00706E6C5F427574746F6E7300000A00
      62746E5F43616E63656C0000060062746E5F4F6B00000B00706E6C5F50726F74
      65637400000B0063686B5F50726F746563740101000000080000000700436170
      74696F6E000C0063686B5F467269656E646C7901020000000A00000007004361
      7074696F6E09000000040048696E74000800706E6C5F4D656D6F00000A006D65
      6D6F5F4C696E6B7300000B0050726F7053746F726167650000}
  end
  object PropStorage: TPropStorageEh
    Section = 'DlgLinks'
    StorageManager = DmMain.PropStorMan
    StoredProps.Strings = (
      'pnl_Dialog.pnl_Buttons.pnl_Protect.chk_Friendly.<P>.Checked'
      'pnl_Dialog.pnl_Buttons.pnl_Protect.chk_Protect.<P>.Checked')
    Left = 240
    Top = 8
  end
end
