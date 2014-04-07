object FmMain: TFmMain
  Left = 380
  Top = 251
  Width = 650
  Height = 500
  Caption = 'Ru.Board EasyWrite'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0003000000070000000700000007000000070000000800000008000000070000
    0007000000070000000500000001FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
    0072323232C0272727B8272727B8272727B81F1F20BC212123BA272727B82727
    27B8282829BA1F1F20B200000023FFFFFF00FFFFFF00FFFFFF00FFFFFF004646
    46A8FFFFFFFFFFFFFFFFFFFFFFFFDADADAFFD9D9D8FFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFF0F0F0FF00000036FFFFFF00FFFFFF00FFFFFF00FFFFFF00443F
    3EA7FFFFFFFFFFFBF5FFD6D0CBFFD7D1CDFFFFF8F2FFFFFBF5FFFFF9F3FFFFF9
    F3FFFFFCF6FFEFE8E4FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF004441
    3EA7FFFFFFFFEFEAE7FFC8C4C0FFFFFFFCFFFFFCF8FFFFF9F5FFFFF9F5FFFFF9
    F5FFFFFEF8FFEFEAE4FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF004441
    3EA7FFFFFFFFFFFCF8FFC7C2C0FFA5A5A4FFFFFFFCFFFFFEF9FFFFFBF6FFFFFB
    F6FFFFFEF9FFEFEAE5FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
    3DA7FFFFFFFFFCF6F2FFFEF8F3FF000000FF3C3835FFFCFBF6FFFFFBF5FFFCF5
    EFFFFFF9F2FFEDE7DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
    3DA7FFFFFFFFF9F3EDFFFFFCF8FFBDB9B5FF000000FF302D2AFFF6F2EAFFFFF8
    F2FFFCF5EFFFECE5DEFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433E
    3BA7FFFFFFFFF6F0ECFFF6F0ECFFFFFFFCFFBFBAB5FF000000FF191918FFEDE7
    E1FFFFFBF6FFEAE5DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00413E
    3BA7FFFFFEFFF3EDE8FFF3EDE8FFF3EDE7FFFFFFF9FFB1ADA8FF000000FF8E8C
    8DFFF5EFE7FFECE5DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00413E
    3BA7FFFFFCFFF0ECE7FFF2ECE7FFF2ECE7FFF2ECE5FFFCF8F2FFECE7E2FFA2A0
    9FFFABABAAFFE4DFDCFF00000031FFFFFF00FFFFFF00FFFFFF00FFFFFF003F3C
    38A7FFFCF6FFEFE7E1FFEFE7E2FFEFE8E2FFEFE8E2FFEFE8E2FFF5EDE7FFECE5
    E1FF979594FF94918EFF00000044FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
    3EA8FFFFFFFFF2EDEAFFF3EDEAFFF3EFEAFFF3EFEAFFF3EFEAFFF3EFEAFFF9F5
    EFFFD8D4D1FF919191FF3B3B3BD00000001EFFFFFF00FFFFFF00FFFFFF003233
    3494E5E5E7F9D1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1
    D3EED8D8D8EEB4B4B3EAA8A8A8FA434343D300000027FFFFFF00FFFFFF000000
    000C000000180000001700000017000000170000001700000017000000170000
    0017000000170000000E00000044B0B0AFFD4C4C4CD400000015FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00000000380000007A0000000AFFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Dock_Menu: TSpTBXDock
    Left = 0
    Top = 0
    Width = 642
    Height = 52
    object tbar_Menu: TSpTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Main Menu'
      CloseButton = False
      DockPos = 0
      FullSize = True
      Images = DmMain.iLst_Main
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      object mnu_File: TSpTBXSubmenuItem
        Caption = 'File'
        object grp_Files: TTBGroupItem
          LinkSubitems = mnu_Files
        end
        object sep_File_1: TSpTBXSeparatorItem
        end
        object mnu_File_Exit: TSpTBXItem
          Caption = 'E&xit'
          Hint = 'Exit|Quits the application'
          Action = DmMain.act_Exit
        end
      end
      object mnu_Edit: TSpTBXSubmenuItem
        Caption = 'Edit'
        object mnu_Edit_Undo: TSpTBXItem
          Caption = '&Undo'
          Hint = 'Undo|Reverts the last action'
          Action = DmMain.act_Edit_Undo
        end
        object sep_Edit_1: TSpTBXSeparatorItem
        end
        object mnu_Edit_Cut: TSpTBXItem
          Caption = 'Cu&t'
          Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
          Action = DmMain.act_Edit_Cut
        end
        object mnu_Edit_Copy: TSpTBXItem
          Caption = '&Copy'
          Hint = 'Copy|Copies the selection and puts it on the Clipboard'
          Action = DmMain.act_Edit_Copy
        end
        object mnu_Edit_Paste: TSpTBXItem
          Caption = '&Paste'
          Hint = 'Paste|Inserts Clipboard contents'
          Action = DmMain.act_Edit_Paste
        end
        object mnu_Edit_Delete: TSpTBXItem
          Caption = '&Delete'
          Hint = 'Delete|Erases the selection'
          Action = DmMain.act_Edit_Delete
        end
        object sep_Edit_2: TSpTBXSeparatorItem
        end
        object mnu_Edit_SelectAll: TSpTBXItem
          Caption = 'Select &All'
          Hint = 'Select All|Selects the entire document'
          Action = DmMain.act_Edit_SelectAll
        end
      end
      object mnu_View: TSpTBXSubmenuItem
        Caption = 'View'
        object mnu_View_ToolBars: TSpTBXSubmenuItem
          Caption = 'Toolbars'
          object vis_Preview: TSpTBXItem
            Caption = 'Preview'
            Control = pnl_Preview
          end
          object vis_Files: TSpTBXItem
            Caption = 'Files Toolbar'
            Control = tbar_Files
          end
          object vis_Editor: TSpTBXItem
            Caption = 'Editor Toolbar'
            Control = tbar_Editor
          end
          object vis_Formats: TSpTBXItem
            Caption = 'Formats Toolbar'
            Control = tbar_Format
          end
          object vis_Insert: TSpTBXItem
            Caption = 'Insert Toolbar'
            Control = tbar_Insert
          end
          object vis_Specials: TSpTBXItem
            Caption = 'Special Functions'
            Control = tbar_Specials
          end
          object vis_StatusBar: TSpTBXItem
            Caption = 'Status Bar'
            OnClick = EOnClick
            Control = StatusBar
          end
        end
        object mnu_View_Themes: TSpTBXSubmenuItem
          Caption = 'Themes'
          object grp_Themes: TSpTBXThemeGroupItem
          end
        end
        object mnu_View_Languages: TSpTBXSubmenuItem
          Caption = 'Languages'
          OnPopup = EOnPopup
        end
      end
      object mnu_Format: TSpTBXSubmenuItem
        Caption = 'Format'
        object mnu_Fmt_Bold: TSpTBXItem
          Caption = 'Bold'
          Action = DmMain.act_Fmt_Bold
        end
        object mnu_Fmt_Italic: TSpTBXItem
          Caption = 'Italic'
          Action = DmMain.act_Fmt_Italic
        end
        object mnu_Fmt_Underline: TSpTBXItem
          Caption = 'Underline'
          Action = DmMain.act_Fmt_Underline
        end
        object mnu_Fmt_Striked: TSpTBXItem
          Caption = 'Strikethrough'
          Action = DmMain.act_Fmt_Striked
        end
        object sep_Fmt_1: TSpTBXSeparatorItem
        end
        object mnu_Fmt_Small: TSpTBXItem
          Caption = 'Small'
          Action = DmMain.act_Fmt_Small
        end
        object mnu_Fmt_Lower: TSpTBXItem
          Caption = 'Subscript'
          Action = DmMain.act_Fmt_Lower
        end
        object mnu_Fmt_Upper: TSpTBXItem
          Caption = 'Superscript'
          Action = DmMain.act_Fmt_Upper
        end
        object sep_Fmt_2: TSpTBXSeparatorItem
        end
        object mnu_Fmt_Code: TSpTBXItem
          Caption = 'Code'
          Action = DmMain.act_Fmt_Code
        end
        object mnu_Fmt_Quote: TSpTBXItem
          Caption = 'Quote'
          Action = DmMain.act_Fmt_Quote
        end
        object mnu_Fmt_Plain: TSpTBXItem
          Caption = 'Plain'
          Hint = 'Plain (No Codes)'
          Action = DmMain.act_Fmt_Plain
        end
        object mnu_Fmt_Hide: TSpTBXItem
          Caption = 'Hide'
          Hint = 'Hidden text'
          Action = DmMain.act_Fmt_Hide
        end
        object sep_Fmt_3: TSpTBXSeparatorItem
        end
        object mnu_Fmt_Center: TSpTBXItem
          Caption = 'Center'
          Action = DmMain.act_Fmt_Center
        end
      end
      object mnu_Insert: TSpTBXSubmenuItem
        Caption = 'Insert'
        object mnu_Ins_Line: TSpTBXItem
          Caption = 'Line'
          Action = DmMain.act_Ins_Line
        end
        object mnu_Ins_Break: TSpTBXItem
          Caption = 'Break'
          Action = DmMain.act_Ins_Break
        end
        object mnu_Ins_Bullet: TSpTBXItem
          Caption = 'Bullet'
          Action = DmMain.act_Ins_Bullet
        end
        object sep_Ins_1: TSpTBXSeparatorItem
        end
        object mnu_Ins_Link: TSpTBXItem
          Caption = 'Link'
          Action = DmMain.act_Ins_Link
        end
        object mnu_Ins_eMail: TSpTBXItem
          Caption = 'eMail'
          Action = DmMain.act_Ins_eMail
        end
        object mnu_Ins_Image: TSpTBXItem
          Caption = 'Image'
          Action = DmMain.act_Ins_Image
        end
        object sep_Ins_2: TSpTBXSeparatorItem
        end
      end
      object mnu_Specials: TSpTBXSubmenuItem
        Caption = 'Specials'
        object mnu_Spec_Links: TSpTBXItem
          Caption = 'Multiple Links'
          Action = DmMain.act_Spec_Links
        end
        object mnu_Spec_Spam: TSpTBXItem
          Caption = 'eMail AntiSpam'
          Action = DmMain.act_Spec_Spam
        end
        object mnu_Spec_Browser: TSpTBXItem
          Caption = 'Preview in browser'
          Hint = 'Opens message preview in default browser'
          Action = DmMain.act_Spec_Browser
        end
      end
      object mnu_Options: TSpTBXSubmenuItem
        Caption = 'Options'
        object mnu_Opt_AutoSave: TSpTBXItem
          Caption = 'AutoSave'
          AutoCheck = True
          Checked = True
        end
        object mnu_Opt_ShortTags: TSpTBXItem
          Caption = 'Short tags'
          Hint = 'Use Short tags when possible'
          AutoCheck = True
        end
        object mnu_Opt_PSelection: TSpTBXItem
          Caption = 'Persistent selection'
          AutoCheck = True
          Checked = True
        end
        object sep_Opts_1: TSpTBXSeparatorItem
        end
        object mnu_Opt_Defaults: TSpTBXItem
          Caption = 'Load Defaults'
          OnClick = EOnClick
        end
      end
      object mnu_Tools: TSpTBXSubmenuItem
        Caption = 'Tools'
        Visible = False
      end
      object sep_Menu_1: TSpTBXSeparatorItem
        Size = 10
      end
      object mnu_Color: TTBControlItem
        Control = btn_Color
      end
      object vis_Smiles: TSpTBXItem
        Caption = 'Smiles Panel'
        ImageIndex = 0
        Images = DmMain.iLst_Trash
        OnClick = EOnVisible
      end
      object mnu_List: TSpTBXItem
        Caption = 'List'
        ImageIndex = 27
        OnClick = EOnClick
      end
      object mnu_Table: TSpTBXSubmenuItem
        Caption = 'Table'
        ImageIndex = 28
        Options = [tboDropdownArrow]
        OnClick = EOnClick
        DropdownCombo = True
        OnPopup = EOnPopup
      end
      object sep_Menu_2: TSpTBXSeparatorItem
      end
      object sizer_MainMenu: TTBXSizerItem
      end
      object mnu_Help: TSpTBXSubmenuItem
        Caption = 'Help'
        object mnu_Links: TSpTBXSubmenuItem
          Caption = 'Links'
          ImageIndex = 33
          object mnu_Lnk_Developers: TSpTBXItem
            Caption = 'For Developers ...'
            Hint = 'Browse URL'
            Action = DmMain.act_Lnk_Developers
          end
          object mnu_Lnk_Forum: TSpTBXItem
            Caption = 'Support Forum ...'
            Hint = 'Browse URL'
            Action = DmMain.act_Lnk_Forum
          end
        end
        object sep_Help_1: TSpTBXSeparatorItem
          Visible = False
        end
        object mnu_Help_About: TSpTBXItem
          Caption = '&About ...'
          Action = DmMain.act_About
        end
      end
      object btn_Color: TColorPickerButton
        Left = 274
        Top = 0
        Width = 45
        Height = 22
        CustomText = 'Custom ...'
        DefaultText = 'Default'
        Flat = True
        PopupSpacing = 8
        SelectionColor = clNone
        ShowSystemColors = False
        OnChange = EOnClick
        OnClick = EOnClick
        OnDefaultSelect = EOnColorDefault
      end
    end
    object tbar_Editor: TSpTBXToolbar
      Left = 133
      Top = 26
      Caption = 'Editor Toolbar'
      DockPos = 73
      DockRow = 2
      Images = DmMain.iLst_Main
      LinkSubitems = mnu_Edit
      TabOrder = 1
    end
    object tbar_Files: TSpTBXToolbar
      Left = 0
      Top = 26
      Caption = 'Files Toolbar'
      DockPos = 0
      DockRow = 2
      Images = DmMain.iLst_Main
      LinkSubitems = mnu_Files
      TabOrder = 2
    end
    object tbar_Specials: TSpTBXToolbar
      Left = 293
      Top = 26
      Caption = 'Special Functions'
      DockPos = 224
      DockRow = 2
      Images = DmMain.iLst_Main
      LinkSubitems = mnu_Specials
      TabOrder = 3
    end
  end
  object Dock_L: TSpTBXMultiDock
    Left = 0
    Top = 59
    Width = 200
    Height = 357
    Position = dpxLeft
    object pnl_Table: TSpTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'Create Table/List'
      DockableTo = [dpLeft, dpRight]
      DockedWidth = 196
      DockPos = 0
      TabOrder = 0
      Visible = False
      OnVisibleChanged = EOnVisible
    end
  end
  object Dock_R: TSpTBXMultiDock
    Left = 460
    Top = 59
    Width = 182
    Height = 357
    Position = dpxRight
    object pnl_Smiles: TSpTBXDockablePanel
      Left = 0
      Top = 0
      Caption = 'Smiles'
      DockableTo = [dpLeft, dpRight]
      DockedWidth = 178
      DockPos = 0
      TabOrder = 0
      Visible = False
      OnVisibleChanged = EOnVisible
    end
  end
  object Dock_B: TSpTBXMultiDock
    Left = 0
    Top = 416
    Width = 642
    Height = 7
    Position = dpxBottom
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 449
    Width = 642
    Height = 24
    object lbl_AutoHint: TSpTBXLabelItem
      Wrapping = twEndEllipsis
    end
    object spacer_StatusBar: TSpTBXRightAlignSpacerItem
      CustomWidth = 620
      CustomHeight = 20
    end
  end
  object pnl_Back: TTBXAlignmentPanel
    Left = 200
    Top = 59
    Width = 260
    Height = 357
    Align = alClient
    TabOrder = 5
    object memo_Main: TTntMemo
      Left = 0
      Top = 216
      Width = 260
      Height = 141
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = []
      HideSelection = False
      ParentFont = False
      PopupMenu = pop_Editor
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = EOnChange
    end
    object Dock_T2: TSpTBXMultiDock
      Left = 0
      Top = 0
      Width = 260
      Height = 216
      object pnl_Preview: TSpTBXDockablePanel
        Left = 0
        Top = 0
        Caption = 'Preview'
        DockableTo = [dpTop, dpBottom]
        DockedWidth = 178
        DockedHeight = 212
        DockPos = 0
        TabOrder = 0
        inline FrmPreview: TFrmHtml
          Left = 0
          Top = 26
          Width = 240
          Height = 186
          Align = alClient
          TabOrder = 1
          inherited html_Viewer: THTMLViewer
            Width = 240
            Height = 186
          end
        end
      end
    end
  end
  object Dock_T1: TSpTBXMultiDock
    Left = 0
    Top = 52
    Width = 642
    Height = 7
  end
  object Dock_Footer: TSpTBXDock
    Left = 0
    Top = 423
    Width = 642
    Height = 26
    Position = dpBottom
    object tbar_Insert: TSpTBXToolbar
      Left = 304
      Top = 0
      Caption = 'Insert Toolbar'
      DockPos = 232
      DockRow = 1
      Images = DmMain.iLst_Main
      LinkSubitems = mnu_Insert
      TabOrder = 0
    end
    object tbar_Format: TSpTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Formats Toolbar'
      DockPos = 0
      DockRow = 1
      Images = DmMain.iLst_Main
      LinkSubitems = mnu_Format
      TabOrder = 1
    end
  end
  object TrayIcon: TCoolTrayIcon
    CycleInterval = 0
    Hint = 'Ru.Board EasyWrite'
    Icon.Data = {
      0000010001001010000001002000680400001600000028000000100000002000
      0000010020000000000040040000000000000000000000000000000000000000
      0003000000070000000700000007000000070000000800000008000000070000
      0007000000070000000500000001FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0072323232C0272727B8272727B8272727B81F1F20BC212123BA272727B82727
      27B8282829BA1F1F20B200000023FFFFFF00FFFFFF00FFFFFF00FFFFFF004646
      46A8FFFFFFFFFFFFFFFFFFFFFFFFDADADAFFD9D9D8FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF0F0F0FF00000036FFFFFF00FFFFFF00FFFFFF00FFFFFF00443F
      3EA7FFFFFFFFFFFBF5FFD6D0CBFFD7D1CDFFFFF8F2FFFFFBF5FFFFF9F3FFFFF9
      F3FFFFFCF6FFEFE8E4FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF004441
      3EA7FFFFFFFFEFEAE7FFC8C4C0FFFFFFFCFFFFFCF8FFFFF9F5FFFFF9F5FFFFF9
      F5FFFFFEF8FFEFEAE4FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF004441
      3EA7FFFFFFFFFFFCF8FFC7C2C0FFA5A5A4FFFFFFFCFFFFFEF9FFFFFBF6FFFFFB
      F6FFFFFEF9FFEFEAE5FF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
      3DA7FFFFFFFFFCF6F2FFFEF8F3FF000000FF3C3835FFFCFBF6FFFFFBF5FFFCF5
      EFFFFFF9F2FFEDE7DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
      3DA7FFFFFFFFF9F3EDFFFFFCF8FFBDB9B5FF000000FF302D2AFFF6F2EAFFFFF8
      F2FFFCF5EFFFECE5DEFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00433E
      3BA7FFFFFFFFF6F0ECFFF6F0ECFFFFFFFCFFBFBAB5FF000000FF191918FFEDE7
      E1FFFFFBF6FFEAE5DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00413E
      3BA7FFFFFEFFF3EDE8FFF3EDE8FFF3EDE7FFFFFFF9FFB1ADA8FF000000FF8E8C
      8DFFF5EFE7FFECE5DFFF00000034FFFFFF00FFFFFF00FFFFFF00FFFFFF00413E
      3BA7FFFFFCFFF0ECE7FFF2ECE7FFF2ECE7FFF2ECE5FFFCF8F2FFECE7E2FFA2A0
      9FFFABABAAFFE4DFDCFF00000031FFFFFF00FFFFFF00FFFFFF00FFFFFF003F3C
      38A7FFFCF6FFEFE7E1FFEFE7E2FFEFE8E2FFEFE8E2FFEFE8E2FFF5EDE7FFECE5
      E1FF979594FF94918EFF00000044FFFFFF00FFFFFF00FFFFFF00FFFFFF00433F
      3EA8FFFFFFFFF2EDEAFFF3EDEAFFF3EFEAFFF3EFEAFFF3EFEAFFF3EFEAFFF9F5
      EFFFD8D4D1FF919191FF3B3B3BD00000001EFFFFFF00FFFFFF00FFFFFF003233
      3494E5E5E7F9D1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1D3EED1D1
      D3EED8D8D8EEB4B4B3EAA8A8A8FA434343D300000027FFFFFF00FFFFFF000000
      000C000000180000001700000017000000170000001700000017000000170000
      0017000000170000000E00000044B0B0AFFD4C4C4CD400000015FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000380000007A0000000AFFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    IconIndex = 0
    PopupMenu = pop_Tray
    MinimizeToTray = True
    OnClick = EOnClick
    Left = 560
    Top = 24
  end
  object pop_Tray: TSpTBXPopupMenu
    Images = DmMain.iLst_Main
    Left = 560
    Top = 52
    object mnu_Tray_Show: TSpTBXItem
      Caption = 'Show'
      FontSettings.Bold = tsTrue
      OnClick = EOnClick
    end
    object sep_Tray_1: TSpTBXSeparatorItem
    end
    object grp_Help: TTBGroupItem
      LinkSubitems = mnu_Help
    end
    object sep_Tray_2: TSpTBXSeparatorItem
    end
    object mnu_Tray_Exit: TSpTBXItem
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 0
      OnClick = EOnClick
    end
  end
  object DKLang: TDKLanguageController
    IgnoreList.Strings = (
      '*.html_Viewer.Def*FontName'
      'PropStorage.*')
    SectionName = 'FmMain'
    StoreList.Strings = (
      'tbar_*.ChevronHint')
    Left = 592
    Top = 24
    LangData = {
      0600466D4D61696E010100000001000000070043617074696F6E017000000009
      00446F636B5F4D656E7500000900746261725F4D656E75010200000003000000
      070043617074696F6E5A0000000B0043686576726F6E48696E740008006D6E75
      5F46696C65010100000012000000070043617074696F6E0009006772705F4669
      6C657300000A007365705F46696C655F3100000D006D6E755F46696C655F4578
      6974010200000013000000070043617074696F6E2E000000040048696E740008
      006D6E755F45646974010100000014000000070043617074696F6E0008006D6E
      755F56696577010100000015000000070043617074696F6E0011006D6E755F56
      6965775F546F6F6C42617273010100000016000000070043617074696F6E000B
      007669735F50726576696577010100000025000000070043617074696F6E0009
      007669735F46696C6573010100000026000000070043617074696F6E000A0076
      69735F456469746F72010100000027000000070043617074696F6E000B007669
      735F466F726D617473010100000028000000070043617074696F6E000A007669
      735F496E73657274010100000029000000070043617074696F6E000A00766973
      5F536D696C657301010000002A000000070043617074696F6E000C007669735F
      5370656369616C7301010000002B000000070043617074696F6E000D00766973
      5F53746174757342617201010000002C000000070043617074696F6E000F006D
      6E755F566965775F5468656D6573010100000017000000070043617074696F6E
      000A006772705F5468656D657300000A006D6E755F466F726D61740101000000
      18000000070043617074696F6E000A006D6E755F496E73657274010100000019
      000000070043617074696F6E000C006D6E755F5370656369616C730101000000
      1A000000070043617074696F6E000B006D6E755F4F7074696F6E730101000000
      1B000000070043617074696F6E0010006D6E755F4F70745F4175746F53617665
      01010000001C000000070043617074696F6E0009006D6E755F546F6F6C730101
      0000001D000000070043617074696F6E000A007365705F4D656E755F31000009
      006D6E755F436F6C6F7200000A007365705F4D656E755F3200000E0073697A65
      725F4D61696E4D656E75000008006D6E755F48656C7001010000001E00000007
      0043617074696F6E0009006D6E755F4C696E6B7301010000001F000000070043
      617074696F6E0012006D6E755F4C6E6B5F446576656C6F706572730102000000
      20000000070043617074696F6E2F000000040048696E74000D006D6E755F4C6E
      6B5F466F72756D010200000021000000070043617074696F6E30000000040048
      696E74000A007365705F48656C705F3100000E006D6E755F48656C705F41626F
      7574010100000022000000070043617074696F6E00090062746E5F436F6C6F72
      0102000000040000000A00437573746F6D54657874050000000B004465666175
      6C7454657874000B00746261725F456469746F72010200000006000000070043
      617074696F6E5B0000000B0043686576726F6E48696E74000A00746261725F46
      696C6573010200000007000000070043617074696F6E5C0000000B0043686576
      726F6E48696E74000D00746261725F5370656369616C73010200000008000000
      070043617074696F6E5D0000000B0043686576726F6E48696E74000600446F63
      6B5F4C00000900706E6C5F5461626C6501010000000900000007004361707469
      6F6E000600446F636B5F5200000A00706E6C5F536D696C657301010000000A00
      0000070043617074696F6E000600446F636B5F42000009005374617475734261
      7200000C006C626C5F4175746F48696E74000010007370616365725F53746174
      757342617200000800706E6C5F4261636B000009006D656D6F5F4D61696E0000
      0700446F636B5F543200000B00706E6C5F5072657669657701010000000C0000
      00070043617074696F6E000A0046726D507265766965770001020000000B0068
      746D6C5F56696577657200000800746D725F48696E7400000700446F636B5F54
      3100000B00446F636B5F466F6F74657200000B00746261725F466F726D617401
      020000000F000000070043617074696F6E5E0000000B0043686576726F6E4869
      6E74000B00746261725F496E7365727401020000001000000007004361707469
      6F6E5F0000000B0043686576726F6E48696E740008005472617949636F6E0101
      00000011000000040048696E74000800706F705F5472617900000D006D6E755F
      547261795F53686F77010100000023000000070043617074696F6E000A007365
      705F547261795F31000008006772705F48656C7000000A007365705F54726179
      5F3200000D006D6E755F547261795F4578697401020000002400000007004361
      7074696F6E31000000040048696E74000C006D6E755F466D745F426F6C640101
      00000032000000070043617074696F6E000E006D6E755F466D745F4974616C69
      63010100000033000000070043617074696F6E0011006D6E755F466D745F556E
      6465726C696E65010100000034000000070043617074696F6E000F006D6E755F
      466D745F537472696B6564010100000035000000070043617074696F6E000900
      7365705F466D745F3100000D006D6E755F466D745F536D616C6C010100000036
      000000070043617074696F6E000D006D6E755F466D745F4C6F77657201010000
      0037000000070043617074696F6E000D006D6E755F466D745F55707065720101
      00000038000000070043617074696F6E0009007365705F466D745F3200000C00
      6D6E755F466D745F436F6465010100000039000000070043617074696F6E000D
      006D6E755F466D745F51756F746501010000003A000000070043617074696F6E
      000D006D6E755F466D745F506C61696E01020000003B00000007004361707469
      6F6E3C000000040048696E740009007365705F466D745F3300000E006D6E755F
      466D745F43656E74657201010000003D000000070043617074696F6E000E006D
      6E755F537065635F4C696E6B7301010000003E000000070043617074696F6E00
      0D006D6E755F537065635F5370616D01010000003F000000070043617074696F
      6E000D006D6E755F456469745F556E646F010200000040000000070043617074
      696F6E41000000040048696E74000A007365705F456469745F3100000C006D6E
      755F456469745F437574010200000042000000070043617074696F6E43000000
      040048696E74000D006D6E755F456469745F436F707901020000004400000007
      0043617074696F6E45000000040048696E74000E006D6E755F456469745F5061
      737465010200000046000000070043617074696F6E47000000040048696E7400
      0F006D6E755F456469745F44656C657465010200000048000000070043617074
      696F6E49000000040048696E74000A007365705F456469745F32000012006D6E
      755F456469745F53656C656374416C6C01020000004A00000007004361707469
      6F6E4B000000040048696E74000800636E745F46696C65000009006D6E755F46
      696C657300000C006D6E755F46696C655F4E657701020000004C000000070043
      617074696F6E4D000000040048696E74000D006D6E755F46696C655F53617665
      01020000004E000000070043617074696F6E4F000000040048696E74000D006D
      6E755F46696C655F4F70656E010200000050000000070043617074696F6E5100
      0000040048696E74000C006D6E755F496E735F4C696E65010100000052000000
      070043617074696F6E000D006D6E755F496E735F427265616B01010000005300
      0000070043617074696F6E000E006D6E755F496E735F42756C6C657401010000
      0054000000070043617074696F6E0009007365705F496E735F3100000C006D6E
      755F496E735F4C696E6B010100000055000000070043617074696F6E000D006D
      6E755F496E735F654D61696C010100000056000000070043617074696F6E000D
      006D6E755F496E735F496D616765010100000057000000070043617074696F6E
      0009007365705F496E735F32000008006D6E755F4C6973740101000000580000
      00070043617074696F6E0009006D6E755F5461626C6501010000005900000007
      0043617074696F6E000A00706F705F456469746F72000012006D6E755F566965
      775F4C616E677561676573010100000060000000070043617074696F6E000B00
      50726F7053746F7261676500000A00437573746F6D697A6572000010006D6E75
      5F4F70745F44656661756C7473010100000061000000070043617074696F6E00
      0A007365705F4F7074735F31000011006D6E755F4F70745F53686F7274546167
      73010200000062000000070043617074696F6E63000000040048696E74001200
      6D6E755F4F70745F5053656C656374696F6E0101000000640000000700436170
      74696F6E000C006D6E755F466D745F4869646501020000006500000007004361
      7074696F6E66000000040048696E740010006D6E755F537065635F42726F7773
      6572010200000067000000070043617074696F6E68000000040048696E7400}
  end
  object cnt_File: TTBItemContainer
    Left = 560
    Top = 88
    object mnu_Files: TSpTBXSubmenuItem
      object mnu_File_New: TSpTBXItem
        Caption = 'New'
        Hint = 'New|Creates new message'
        Action = DmMain.act_File_New
      end
      object mnu_File_Save: TSpTBXItem
        Caption = 'Save &As...'
        Hint = 'Save As|Writes message to file'
        Action = DmMain.act_File_Save
      end
      object mnu_File_Open: TSpTBXItem
        Caption = '&Open...'
        Hint = 'Open|Loads saved message'
        Action = DmMain.act_File_Open
      end
    end
  end
  object pop_Editor: TSpTBXPopupMenu
    LinkSubitems = mnu_Edit
    Left = 528
    Top = 52
  end
  object PropStorage: TPropStorageEh
    Active = False
    Section = 'FmMain'
    StorageManager = DmMain.PropStorMan
    StoredProps.Strings = (
      '<P>.Height'
      '<P>.Left'
      '<P>.PixelsPerInch'
      '<P>.Top'
      '<P>.Width'
      '<P>.WindowState'
      'Dock_Menu.tbar_Menu.mnu_Options.mnu_Opt_AutoSave.<P>.Checked'
      'Dock_Menu.tbar_Menu.mnu_Options.mnu_Opt_PSelection.<P>.Checked'
      'Dock_Menu.tbar_Menu.mnu_Options.mnu_Opt_ShortTags.<P>.Checked'
      'Dock_Menu.tbar_Menu.mnu_View.mnu_View_Languages.<P>.Tag')
    AfterLoadProps = EAfterLoadProps
    Left = 528
    Top = 24
  end
  object Customizer: TSpTBXCustomizer
    MenuBar = tbar_Menu
    Images = DmMain.iLst_Main
    Left = 528
    Top = 89
  end
end
