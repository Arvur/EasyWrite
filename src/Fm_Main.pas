unit Fm_Main;

interface

uses
  StrUtils{RightStr}, StdActns{THintAction}, Htmlsubs{TImageObj},
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TB2Dock, TBX, SpTBXItem, TB2Toolbar, TBXDkPanels, SpTBXDkPanels,
  CoolTrayIcon, TB2Item, Menus, TBXSizer, StdCtrls, TntStdCtrls,
  TB2ExtItems, TBXExtItems, Grids, TntGrids, SpTBXEditors, Frm_Smiles,
  Frm_Tables, ColorPickerButton, TBXToolPals, Htmlview, Frm_Html, DKLang,
  PropFilerEh, PropStorageEh, SpTBXCustomizer, {$I I_Use_Themes.Inc};

type
  TFmMain = class(TForm)
    TrayIcon: TCoolTrayIcon;
    Dock_Menu: TSpTBXDock;
    tbar_Menu: TSpTBXToolbar;
    Dock_L: TSpTBXMultiDock;
    Dock_R: TSpTBXMultiDock;
    Dock_B: TSpTBXMultiDock;
    sizer_MainMenu: TTBXSizerItem;
    mnu_Help: TSpTBXSubmenuItem;
    mnu_Tools: TSpTBXSubmenuItem;
    mnu_View: TSpTBXSubmenuItem;
    mnu_Edit: TSpTBXSubmenuItem;
    mnu_File: TSpTBXSubmenuItem;
    sep_File_1: TSpTBXSeparatorItem;
    mnu_File_Exit: TSpTBXItem;
    sep_Help_1: TSpTBXSeparatorItem;
    StatusBar: TSpTBXStatusBar;
    tbar_Editor: TSpTBXToolbar;
    mnu_View_ToolBars: TSpTBXSubmenuItem;
    lbl_AutoHint: TSpTBXLabelItem;
    spacer_StatusBar: TSpTBXRightAlignSpacerItem;
    tbar_Files: TSpTBXToolbar;
    grp_Files: TTBGroupItem;
    mnu_View_Themes: TSpTBXSubmenuItem;
    grp_Themes: TSpTBXThemeGroupItem;
    mnu_Format: TSpTBXSubmenuItem;
    pnl_Table: TSpTBXDockablePanel;
    mnu_Insert: TSpTBXSubmenuItem;
    pnl_Smiles: TSpTBXDockablePanel;
    mnu_Color: TTBControlItem;
    btn_Color: TColorPickerButton;
    sep_Menu_1: TSpTBXSeparatorItem;
    sep_Menu_2: TSpTBXSeparatorItem;
    tbar_Specials: TSpTBXToolbar;
    mnu_Specials: TSpTBXSubmenuItem;
    mnu_Options: TSpTBXSubmenuItem;
    mnu_Opt_AutoSave: TSpTBXItem;
    pnl_Back: TTBXAlignmentPanel;
    memo_Main: TTntMemo;
    Dock_T2: TSpTBXMultiDock;
    pnl_Preview: TSpTBXDockablePanel;
    Dock_T1: TSpTBXMultiDock;
    Dock_Footer: TSpTBXDock;
    tbar_Format: TSpTBXToolbar;
    tbar_Insert: TSpTBXToolbar;
    mnu_Help_About: TSpTBXItem;
    mnu_Lnk_Forum: TSpTBXItem;
    mnu_Lnk_Developers: TSpTBXItem;
    mnu_Links: TSpTBXSubmenuItem;
    FrmPreview: TFrmHtml;
    pop_Tray: TSpTBXPopupMenu;
    mnu_Tray_Show: TSpTBXItem;
    sep_Tray_1: TSpTBXSeparatorItem;
    grp_Help: TTBGroupItem;
    sep_Tray_2: TSpTBXSeparatorItem;
    mnu_Tray_Exit: TSpTBXItem;
    vis_Preview: TSpTBXItem;
    vis_Files: TSpTBXItem;
    vis_Editor: TSpTBXItem;
    vis_Formats: TSpTBXItem;
    vis_Insert: TSpTBXItem;
    vis_Smiles: TSpTBXItem;
    vis_Specials: TSpTBXItem;
    vis_StatusBar: TSpTBXItem;
    DKLang: TDKLanguageController;
    mnu_Fmt_Bold: TSpTBXItem;
    mnu_Fmt_Italic: TSpTBXItem;
    mnu_Fmt_Underline: TSpTBXItem;
    mnu_Fmt_Striked: TSpTBXItem;
    sep_Fmt_1: TSpTBXSeparatorItem;
    mnu_Fmt_Small: TSpTBXItem;
    mnu_Fmt_Lower: TSpTBXItem;
    mnu_Fmt_Upper: TSpTBXItem;
    sep_Fmt_2: TSpTBXSeparatorItem;
    mnu_Fmt_Code: TSpTBXItem;
    mnu_Fmt_Quote: TSpTBXItem;
    mnu_Fmt_Plain: TSpTBXItem;
    sep_Fmt_3: TSpTBXSeparatorItem;
    mnu_Fmt_Center: TSpTBXItem;
    mnu_Spec_Links: TSpTBXItem;
    mnu_Spec_Spam: TSpTBXItem;
    mnu_Edit_Undo: TSpTBXItem;
    sep_Edit_1: TSpTBXSeparatorItem;
    mnu_Edit_Cut: TSpTBXItem;
    mnu_Edit_Copy: TSpTBXItem;
    mnu_Edit_Paste: TSpTBXItem;
    mnu_Edit_Delete: TSpTBXItem;
    sep_Edit_2: TSpTBXSeparatorItem;
    mnu_Edit_SelectAll: TSpTBXItem;
    cnt_File: TTBItemContainer;
    mnu_Files: TSpTBXSubmenuItem;
    mnu_File_New: TSpTBXItem;
    mnu_File_Save: TSpTBXItem;
    mnu_File_Open: TSpTBXItem;
    mnu_Ins_Line: TSpTBXItem;
    mnu_Ins_Break: TSpTBXItem;
    mnu_Ins_Bullet: TSpTBXItem;
    sep_Ins_1: TSpTBXSeparatorItem;
    mnu_Ins_Link: TSpTBXItem;
    mnu_Ins_eMail: TSpTBXItem;
    mnu_Ins_Image: TSpTBXItem;
    sep_Ins_2: TSpTBXSeparatorItem;
    mnu_List: TSpTBXItem;
    mnu_Table: TSpTBXSubmenuItem;
    pop_Editor: TSpTBXPopupMenu;
    mnu_View_Languages: TSpTBXSubmenuItem;
    PropStorage: TPropStorageEh;
    Customizer: TSpTBXCustomizer;
    mnu_Opt_Defaults: TSpTBXItem;
    sep_Opts_1: TSpTBXSeparatorItem;
    mnu_Opt_ShortTags: TSpTBXItem;
    mnu_Opt_PSelection: TSpTBXItem;
    mnu_Fmt_Hide: TSpTBXItem;
    mnu_Spec_Browser: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure EOnClick(Sender: TObject);
    procedure EOnColorDefault(Sender: TObject);
    procedure EOnChange(Sender: TObject);
    procedure EOnVisible(Sender: TObject);
    procedure EOnPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure EAfterLoadProps(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFrmSmiles: TFrmSmiles;
    FFrmTables: TFrmTables; 
  public
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure LoadProperties;
    procedure SaveProperties;

    property FrmSmiles: TFrmSmiles read FFrmSmiles write FFrmSmiles;
    property FrmTables: TFrmTables read FFrmTables write FFrmTables;
  end;

var
  FmMain: TFmMain;

implementation

uses Dm_Main, U_Cmn_Files, Fm_TblSelect;

{$R *.dfm}

procedure TFmMain.FormCreate(Sender: TObject);
begin
 MsgEditor := memo_Main;
 if not FileExists(DmMain.PropStorMan.IniFileName)
  then memo_Main.Lines.LoadFromFile(HomePath + 'ReadMe.txt');
 EOnChange(memo_Main);
end;

procedure TFmMain.FormShow(Sender: TObject);
begin
 LoadProperties;
end;

procedure TFmMain.FormActivate(Sender: TObject);
begin
 SaveDefaults;
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
 SaveProperties;
end;

procedure TFmMain.LoadProperties;
begin
 Customizer.Load(DmMain.PropStorMan.IniFileName);
 PropStorage.LoadProperties;
end;

procedure TFmMain.SaveProperties;
begin
 Customizer.Save(DmMain.PropStorMan.IniFileName);
 PropStorage.SaveProperties
end;

function TFmMain.ExecuteAction(Action: TBasicAction): Boolean;
begin
 if (Action is THintAction)
  then begin
   lbl_AutoHint.Caption := THintAction(Action).Hint;
   Result := True;
  end
  else Result := inherited ExecuteAction(Action);
end;

procedure TFmMain.EOnPopup(Sender: TTBCustomItem; FromLink: Boolean);
var
 Counter : Integer;
 tmpItem : TSpTBXItem;
begin
 if (Sender = mnu_Table) then begin
  with TFmTblSelect.Create(nil) do
   ShowSelector(FmMain.tbar_Menu, mnu_Table);
  Abort;
 end;

 if (Sender = mnu_View_Languages) then
  with LangManager do begin
   mnu_View_Languages.Clear;

   for Counter := 0 to (LanguageCount - 1) do begin
    tmpItem := TSpTBXItem.Create(mnu_View_Languages);
    tmpItem.Caption := LanguageNames[Counter];
    tmpItem.RadioItem := True;
    tmpItem.OnClick := EOnClick;
    mnu_View_Languages.Add(tmpItem);
   end;

   mnu_View_Languages.Items[LanguageIndex].Checked := True;
  end;
end;

procedure TFmMain.EOnClick(Sender: TObject);
var
 tmpStr : string;
begin
 if (Sender = mnu_Tray_Exit) then begin
  TrayIcon.MinimizeToTray := False;
  Close;
 end;

 if (Sender = TrayIcon) then mnu_Tray_Show.Click;

 if (Sender = mnu_Tray_Show) then begin
  TrayIcon.ShowMainForm;
  TrayIcon.IconVisible := False;
 end;

 if (Sender = btn_Color) and (btn_Color.SelectionColor <> clNone) then begin
  // FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
  tmpStr := ColorToString(btn_Color.SelectionColor);
  if (DmMain.str_w3Colors.Strings.IndexOf(tmpStr) > -1)
   then tmpStr := RightStr(tmpStr, Length(tmpStr) - 2)
   else tmpStr := '#' + ReverseString(Format('%.6x', [btn_Color.SelectionColor]));
  WrapSelection('[color=' + tmpStr + ']', '[/color]');
 end;

 if (Sender = mnu_List) or (Sender = mnu_Table) then begin
  pnl_Table.Visible := True;
  FrmTables.ListMode := (Sender = mnu_List);
 end;

 if (Sender is TSpTBXItem) and (mnu_View_Languages.ContainsItem(Sender as TSpTBXItem)) then
  with LangManager do begin
   mnu_View_Languages.Tag := mnu_View_Languages.IndexOf(Sender as TSpTBXItem);
   LanguageID := LanguageIDs[mnu_View_Languages.Tag];
  end;

 if (Sender = vis_StatusBar) then StatusBar.Top := ClientHeight - StatusBar.Height + 1;

 if (Sender = mnu_Opt_Defaults) then LoadDefaults;
end;

procedure TFmMain.EOnColorDefault(Sender: TObject);
begin
 btn_Color.SelectionColor := clNone;
end;

procedure TFmMain.EOnChange(Sender: TObject);
var
 VertPos : Integer;
begin
 if mnu_Opt_AutoSave.Checked then
  memo_Main.Lines.SaveToFile(HomePath + 'message.txt');

 VertPos := FrmPreview.html_Viewer.VScrollBarPosition;
  FrmPreview.html_Viewer.LoadFromString(DmMain.GetPreview);
 FrmPreview.html_Viewer.VScrollBarPosition := VertPos;
end;

procedure TFmMain.EOnVisible(Sender: TObject);
 procedure ShowSmiles;
 begin
  if not Assigned(FrmSmiles) then begin
   FrmSmiles := TFrmSmiles.Create(Self);
   FrmSmiles.Parent := pnl_Smiles;
  end;
 end;
begin
 if (Sender = pnl_Smiles) then begin
  if pnl_Smiles.Visible
   then begin
    ShowSmiles;
    FrmSmiles.FrameResize(FrmSmiles);
   end
   else FreeAndNil(FFrmSmiles);
  vis_Smiles.Checked := pnl_Smiles.Visible;
 end;

 if (Sender = pnl_Table) then begin
  if pnl_Table.Visible
   then begin
    FrmTables := TFrmTables.Create(Self);
    FrmTables.Parent := pnl_Table;
   end
   else FrmTables.Free;
 end;

 if (Sender = vis_Smiles) then begin
  if not pnl_Smiles.Visible then ShowSmiles;
  pnl_Smiles.Visible := not vis_Smiles.Checked;
 end;
end;

procedure TFmMain.EAfterLoadProps(Sender: TObject);
begin
 with LangManager do
  if (ScanForLangFiles(HomePath + 'languages\', '*.lng', False) > 0) then
   LanguageID := LanguageIDs[mnu_View_Languages.Tag];
end;

end.
