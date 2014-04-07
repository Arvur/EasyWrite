object SpTBXCustomizeForm: TSpTBXCustomizeForm
  Left = 219
  Top = 115
  Caption = 'Customizer'
  ClientHeight = 321
  ClientWidth = 342
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXTabControl1: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 342
    Height = 286
    Align = alClient
    ActiveTabIndex = 0
    ThemeType = tttTBX
    HiddenItems = <>
    object tabToolbars: TSpTBXTabItem
      Caption = 'Toolbars'
      Checked = True
      ThemeType = tttTBX
    end
    object tabCommands: TSpTBXTabItem
      Caption = 'Commands'
      ThemeType = tttTBX
    end
    object tabShortcuts: TSpTBXTabItem
      Caption = 'Shortcuts'
      ThemeType = tttTBX
    end
    object SpTBXTabSheet3: TSpTBXTabSheet
      Left = 0
      Top = 23
      Width = 342
      Height = 263
      Caption = 'Shortcuts'
      ImageIndex = -1
      DesignSize = (
        342
        263)
      TabItem = 'tabShortcuts'
      object SpTBXPanel5: TSpTBXPanel
        Left = 65
        Top = 228
        Width = 186
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        Color = clNone
        ParentColor = False
        TabOrder = 1
        HotTrack = True
        object Panel1: TPanel
          Left = 2
          Top = 2
          Width = 182
          Height = 18
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            182
            18)
          object HotKey1: THotKey
            Left = -2
            Top = -2
            Width = 188
            Height = 23
            Anchors = [akLeft, akTop, akRight, akBottom]
            HotKey = 0
            Modifiers = []
            TabOrder = 0
          end
        end
      end
      object SpTBXLabel4: TSpTBXLabel
        Left = 11
        Top = 232
        Width = 41
        Height = 13
        Caption = '&Shortcut'
        Anchors = [akLeft, akBottom]
        FocusControl = HotKey1
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object ChangeShortcut: TSpTBXButton
        Left = 256
        Top = 227
        Width = 75
        Height = 25
        Caption = 'C&hange'
        Anchors = [akRight, akBottom]
        TabOrder = 2
        OnClick = ChangeShortcutClick
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
        ThemeType = thtTBX
      end
      object lbShortcuts: TSpTBXListBox
        Left = 8
        Top = 7
        Width = 325
        Height = 210
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 16
        TabOrder = 0
        OnClick = lbShortcutsClick
        OnDrawItem = lbShortcutsDrawItem
      end
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 23
      Width = 342
      Height = 263
      Caption = 'Commands'
      ImageIndex = -1
      DesignSize = (
        342
        263)
      TabItem = 'tabCommands'
      object SpTBXLabel3: TSpTBXLabel
        Left = 2
        Top = 213
        Width = 336
        Height = 46
        Margins.Left = 5
        Margins.Right = 5
        Caption = 
          'To add command buttons, drag and drop commands onto a toolbar. T' +
          'o remove command buttons, drag them off the toolbar and drop the' +
          'm on the commands list.'
        Align = alBottom
        AutoSize = False
        Wrapping = twWrap
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object lbCommands: TSpTBXListBox
        Left = 8
        Top = 7
        Width = 325
        Height = 205
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        ItemHeight = 16
        TabOrder = 0
        OnDragDrop = lbCommandsDragDrop
        OnDragOver = lbCommandsDragOver
        OnDrawItem = lbCommandsDrawItem
        OnEndDrag = lbCommandsEndDrag
        OnStartDrag = lbCommandsStartDrag
      end
    end
    object SpTBXTabSheet2: TSpTBXTabSheet
      Left = 0
      Top = 23
      Width = 342
      Height = 263
      Caption = 'Toolbars'
      ImageIndex = -1
      DesignSize = (
        342
        263)
      TabItem = 'tabToolbars'
      object SpTBXLabel1: TSpTBXLabel
        Left = 8
        Top = 8
        Width = 41
        Height = 13
        Caption = '&Toolbars'
        LinkFont.Charset = DEFAULT_CHARSET
        LinkFont.Color = clBlue
        LinkFont.Height = -11
        LinkFont.Name = 'MS Shell Dlg 2'
        LinkFont.Style = [fsUnderline]
      end
      object SpTBXGroupBox1: TSpTBXGroupBox
        Left = 168
        Top = 18
        Width = 161
        Height = 177
        Caption = 'Options'
        Anchors = [akTop, akRight]
        Color = clNone
        ParentColor = False
        TabOrder = 1
        object cbText: TSpTBXComboBox
          Left = 8
          Top = 64
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnClick = cbTextClick
          Items.Strings = (
            'Selective text on right'
            'No text labels'
            'Show text labels'
            'Text only')
        end
        object cbIcon: TSpTBXComboBox
          Left = 8
          Top = 112
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Large images'
            'Small images')
        end
        object cbTextLabel: TSpTBXLabel
          Left = 8
          Top = 48
          Width = 62
          Height = 13
          Caption = 'T&ext Options'
          FocusControl = cbText
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object cbIconLabel: TSpTBXLabel
          Left = 8
          Top = 96
          Width = 61
          Height = 13
          Caption = '&Icon Options'
          FocusControl = cbIcon
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
        end
        object ResetButton: TSpTBXButton
          Left = 31
          Top = 144
          Width = 105
          Height = 25
          Caption = '&Reset to default'
          TabOrder = 3
          Visible = False
          LinkFont.Charset = DEFAULT_CHARSET
          LinkFont.Color = clBlue
          LinkFont.Height = -11
          LinkFont.Name = 'MS Shell Dlg 2'
          LinkFont.Style = [fsUnderline]
          ThemeType = thtTBX
        end
        object checkVisible: TSpTBXCheckBox
          Left = 8
          Top = 24
          Width = 47
          Height = 15
          Caption = '&Visible'
          TabOrder = 0
          OnClick = checkVisibleClick
        end
      end
      object SpTBXGroupBox2: TSpTBXGroupBox
        Left = 168
        Top = 200
        Width = 161
        Height = 49
        Caption = 'T&hemes'
        Anchors = [akTop, akRight, akBottom]
        Color = clNone
        ParentColor = False
        TabOrder = 2
        object cbThemes: TSpTBXComboBox
          Left = 8
          Top = 20
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnClick = cbThemesClick
        end
      end
      object lbToolbars: TSpTBXCheckListBox
        Left = 8
        Top = 24
        Width = 153
        Height = 225
        OnClickCheck = lbToolbarsClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 20
        TabOrder = 0
        OnClick = lbToolbarsClick
      end
    end
  end
  object ClosePanel: TPanel
    Left = 0
    Top = 286
    Width = 342
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      342
      35)
    object CloseButton: TSpTBXButton
      Left = 262
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Close'
      Anchors = [akRight, akBottom]
      TabOrder = 0
      OnClick = CloseButtonClick
      LinkFont.Charset = DEFAULT_CHARSET
      LinkFont.Color = clBlue
      LinkFont.Height = -11
      LinkFont.Name = 'MS Shell Dlg 2'
      LinkFont.Style = [fsUnderline]
      ThemeType = thtTBX
    end
  end
end
