object FrmTables: TFrmTables
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  object pnl_Tables: TTBXToolWindow
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    Align = alClient
    Caption = 'pnl_Tables'
    ClientAreaHeight = 277
    ClientAreaWidth = 443
    TabOrder = 0
    object pnl_Grid: TTBXAlignmentPanel
      Left = 0
      Top = 21
      Width = 443
      Height = 200
      Align = alClient
      Margins.Left = 2
      Margins.Top = 5
      Margins.Right = 2
      Margins.Bottom = 5
      TabOrder = 0
      object grid_Table: TTntStringGrid
        Left = 2
        Top = 5
        Width = 439
        Height = 190
        Align = alClient
        BorderStyle = bsNone
        ColCount = 4
        Ctl3D = False
        DefaultColWidth = 55
        RowCount = 4
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object btn_InsertTable: TSpTBXButton
      Left = 0
      Top = 242
      Width = 443
      Height = 35
      Caption = 'Insert Table >>'
      Align = alBottom
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      OnClick = EOnClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Sans Serif'
      LinkFont.Style = [fsUnderline]
      ThemeType = thtTBX
    end
    object tbar_Table_Caption: TSpTBXToolbar
      Left = 0
      Top = 0
      Width = 443
      Height = 21
      Align = alTop
      Caption = 'tbar_Table_Caption'
      TabOrder = 2
      object lbl_Tbl_Caption: TSpTBXLabelItem
        Caption = 'Caption'
      end
      object edt_Table_Caption: TSpTBXEditItem
        AutoCheck = True
        EditWidth = 120
        ExtendedAccept = True
      end
    end
    object tbar_Table_Dims: TSpTBXToolbar
      Left = 0
      Top = 221
      Width = 443
      Height = 21
      Align = alBottom
      Caption = 'tbar_Table_Dims'
      TabOrder = 3
      TabStop = True
      object lbl_Tbl_Rows: TSpTBXLabelItem
        Caption = 'Rows'
      end
      object edt_Tbl_Rows: TTBXSpinEditItem
        AutoCheck = True
        EditWidth = 40
        ExtendedAccept = True
        Value = 3.000000000000000000
        OnValueChange = EOnValueChange
      end
      object lbl_Tbl_Cols: TSpTBXLabelItem
        Caption = 'Columns'
      end
      object edt_Tbl_Cols: TTBXSpinEditItem
        AutoCheck = True
        EditWidth = 40
        ExtendedAccept = True
        Value = 3.000000000000000000
        OnValueChange = EOnValueChange
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object cmb_Tbl_Type: TSpTBXComboBoxItem
        EditWidth = 75
        RadioItem = True
        Text = 'Bulleted'
        DropDownList = True
        Strings.Strings = (
          'Bulleted'
          'Numbered'
          'Alphabetic')
      end
    end
  end
  object DKLang: TDKLanguageController
    IgnoreList.Strings = (
      'pnl_*.Caption'
      'tbar_*.Caption')
    SectionName = 'FrmTables'
    Left = 232
    Top = 8
    LangData = {
      090046726D5461626C657300010E0000000A00706E6C5F5461626C6573000008
      00706E6C5F4772696400000A00677269645F5461626C6500000F0062746E5F49
      6E736572745461626C65010100000002000000070043617074696F6E00120074
      6261725F5461626C655F43617074696F6E00000F006C626C5F54626C5F436170
      74696F6E010100000007000000070043617074696F6E0011006564745F546162
      6C655F43617074696F6E00000F00746261725F5461626C655F44696D7300000C
      006C626C5F54626C5F526F7773010100000008000000070043617074696F6E00
      0C006564745F54626C5F526F777300000C006C626C5F54626C5F436F6C730101
      00000009000000070043617074696F6E000C006564745F54626C5F436F6C7300
      0013005370544258536570617261746F724974656D3100000C00636D625F5462
      6C5F5479706501020000000B0000000700537472696E67730600000004005465
      787400}
  end
end
