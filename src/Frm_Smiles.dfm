object FrmSmiles: TFrmSmiles
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  OnResize = FrameResize
  object pCtrl_Smiles: TrmTBXTabControl
    Left = 0
    Top = 25
    Width = 443
    Height = 252
    Align = alClient
    MultiLine = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TabPosition = tpRight
    Tabs.Strings = (
      '1'
      '2'
      '3')
    TabIndex = 0
    OnChange = EOnCollection
    UseFancyTabs = True
    inline FrmHtml: TFrmHtml
      Left = 4
      Top = 4
      Width = 416
      Height = 244
      Align = alClient
      TabOrder = 0
      inherited html_Viewer: THTMLViewer
        Width = 416
        Height = 244
        OnImageClick = EOnImageClick
      end
    end
  end
  object pnl_Collection: TTBXAlignmentPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 25
    Align = alTop
    Margins.Left = 5
    Margins.Right = 5
    TabOrder = 1
    object lbl_Collection: TSpTBXLabel
      Left = 5
      Top = 0
      Width = 51
      Height = 25
      Caption = 'Collection'
      Align = alLeft
      Margins.Right = 5
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'Tahoma'
      LinkFont.Style = [fsUnderline]
    end
    object cmb_Collection: TSpTBXComboBox
      Left = 70
      Top = 2
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnChange = EOnCollection
    end
  end
  object tbl_Smiles: TTntCSVTable
    Left = 3
    Top = 44
  end
  object DKLang: TDKLanguageController
    IgnoreList.Strings = (
      '*.html_Viewer.Def*FontName')
    SectionName = 'FrmSmiles'
    OnLanguageChanged = EOnLanguageChanged
    Left = 226
    Top = 8
    LangData = {
      090046726D536D696C65730001060000000C00704374726C5F536D696C657300
      00070046726D48746D6C0001020000000B0068746D6C5F566965776572000008
      00746D725F48696E7400000E00706E6C5F436F6C6C656374696F6E00000E006C
      626C5F436F6C6C656374696F6E010100000001000000070043617074696F6E00
      0E00636D625F436F6C6C656374696F6E00000A0074626C5F536D696C65730000}
  end
end
