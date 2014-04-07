object TBActionImportOptionsForm: TTBActionImportOptionsForm
  Left = 225
  Top = 133
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Import/Update from TActionList '
  ClientHeight = 269
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ConvertButton: TButton
    Left = 4
    Top = 239
    Width = 121
    Height = 26
    Caption = '&Convert/Update'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object HelpButton: TButton
    Left = 558
    Top = 239
    Width = 73
    Height = 26
    Caption = '&Help'
    Enabled = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 466
    Top = 239
    Width = 85
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object tbItemCreationOptionsGroup: TGroupBox
    Left = 4
    Top = 94
    Width = 273
    Height = 137
    Caption = 'Item Create/Update Options'
    TabOrder = 3
    object Label3: TLabel
      Left = 6
      Top = 52
      Width = 114
      Height = 13
      Caption = 'Component Name Prefix'
    end
    object Label4: TLabel
      Left = 6
      Top = 84
      Width = 131
      Height = 13
      Caption = 'Exclude Action Name Prefix'
    end
    object editComponentNamePrefix: TEdit
      Left = 146
      Top = 47
      Width = 115
      Height = 21
      TabOrder = 0
      Text = 'tbm'
    end
    object editActionNamePrefix: TEdit
      Left = 146
      Top = 80
      Width = 115
      Height = 21
      TabOrder = 1
      Text = 'ac'
    end
    object cbPullDownMenus: TRadioButton
      Left = 12
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Pull-Down Menus'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object cbToolbarButtons: TRadioButton
      Left = 130
      Top = 18
      Width = 113
      Height = 17
      Caption = 'Toolbar-Buttons'
      TabOrder = 3
    end
  end
  object GroupBox1: TGroupBox
    Left = 282
    Top = 4
    Width = 349
    Height = 229
    Caption = 'Actions'
    TabOrder = 4
    DesignSize = (
      349
      229)
    object ActionsCheckList: TCheckListBox
      Left = 8
      Top = 16
      Width = 331
      Height = 173
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 0
    end
    object ButtonSelAll: TButton
      Left = 8
      Top = 195
      Width = 75
      Height = 25
      Anchors = [akLeft]
      Caption = 'Select All'
      TabOrder = 1
      OnClick = ButtonSelAllClick
    end
    object ButtonUnselAll: TButton
      Left = 86
      Top = 195
      Width = 75
      Height = 25
      Anchors = [akLeft]
      Caption = 'Unselect All'
      TabOrder = 2
      OnClick = ButtonUnselAllClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 8
    Width = 273
    Height = 81
    Caption = 'TActionList'
    TabOrder = 5
    object Label1: TLabel
      Left = 6
      Top = 48
      Width = 42
      Height = 13
      Caption = 'Category'
      FocusControl = CategoryCombo
    end
    object Label2: TLabel
      Left = 8
      Top = 22
      Width = 46
      Height = 13
      Caption = 'ActionList'
      FocusControl = CategoryCombo
    end
    object CategoryCombo: TComboBox
      Left = 62
      Top = 46
      Width = 203
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = CategoryComboChange
    end
    object ActionListCombo: TComboBox
      Left = 62
      Top = 22
      Width = 203
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ActionListComboChange
    end
  end
end
