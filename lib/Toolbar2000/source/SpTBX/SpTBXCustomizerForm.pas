unit SpTBXCustomizerForm;

{==============================================================================
Version 1.8.3

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://club.telepolis.com/silverpointdev/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Alex Denisov's TBX
    http://g32.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.

History:
8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - No changes.
  
27 August 2006 - version 1.8
  - No changes.

15 June 2006 - version 1.7
  - No changes.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - No changes.

10 February 2006 - version 1.3
  - No changes.

28 December 2005 - version 1.2
  - Fixed incorrect ShortCut processing.

18 October 2005 - version 1.1
  - Added TBX style selection painting to the ListBoxes of the
    SpTBXCustomizerForm.
  - Added TBX style icon selection painting to the ListBoxes of the
    SpTBXCustomizerForm.
  - Added TBX style checkbox painting to CheckListBox of the
    SpTBXCustomizerForm.

18 August 2005 - version 1.0
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList, CheckLst,
  TntClasses, TntStdCtrls, TntCheckLst,
  TB2Toolbar, TB2Item, TBX,
  SpTBXItem, SpTBXControls, SpTBXEditors, SpTBXTabs, SpTBXCustomizer,
  TBXDkPanels;

resourcestring
  SSpTBXCustomizerFormChangeShortcut = 'The shortcut is already being used by the "%s" command. Do you want to reassign it to the "%s" command?';
  SSpTBXCustomizerFormChangeShortcutTitle = 'Change Shortcut';
  SSpTBXCustomizerFormSeparator = 'Separator';

type
  TSpTBXCustomizeForm = class(TSpTBXCustomCustomizeForm)
    SpTBXTabControl1: TSpTBXTabControl;
    tabCommands: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tabToolbars: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    tabShortcuts: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    ClosePanel: TPanel;
    CloseButton: TSpTBXButton;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXLabel3: TSpTBXLabel;
    HotKey1: THotKey;
    ChangeShortcut: TSpTBXButton;
    SpTBXLabel4: TSpTBXLabel;
    SpTBXPanel5: TSpTBXPanel;
    Panel1: TPanel;
    cbThemes: TSpTBXComboBox;
    cbIconLabel: TSpTBXLabel;
    cbText: TSpTBXComboBox;
    cbIcon: TSpTBXComboBox;
    cbTextLabel: TSpTBXLabel;
    SpTBXGroupBox1: TSpTBXGroupBox;
    ResetButton: TSpTBXButton;
    checkVisible: TSpTBXCheckBox;
    SpTBXGroupBox2: TSpTBXGroupBox;
    lbToolbars: TSpTBXCheckListBox;
    lbCommands: TSpTBXListBox;
    lbShortcuts: TSpTBXListBox;
    procedure lbCommandsStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure lbCommandsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbCommandsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure lbCommandsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbToolbarsClickCheck(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbCommandsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbShortcutsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbShortcutsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbThemesClick(Sender: TObject);
    procedure checkVisibleClick(Sender: TObject);
    procedure cbTextClick(Sender: TObject);
    procedure lbToolbarsClick(Sender: TObject);
    procedure ChangeShortcutClick(Sender: TObject);
  protected
    procedure DoFillCommands(ToolbarList, ItemList, ShortcutsList: TTntStringList); override;
    procedure DoThemeChange; override;
  end;

implementation

uses
  TBXThemes, TBXUtils, ActnList;

{$R *.dfm}

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpDrawListboxItem(L: TTntListBox; Index: Integer; ARect: TRect;
  State: TOwnerDrawState; DrawShortcut: Boolean; DefaultImageList: TCustomImageList);
var
  Item: TTBCustomItem;
  IL: TCustomImageList;
  WS: WideString;
  TextR, ILRect: TRect;
  Y: Integer;
  ACanvas: TCanvas;
begin
  if (Index < 0) or (Index > L.Count - 1) or not (L.Items.Objects[Index] is TTBCustomItem) then Exit;
  Item := L.Items.Objects[Index] as TTBCustomItem;
  ACanvas := L.Canvas;

  // Draw the icon image
  Inc(ARect.Left, 2);  // Apply margins
  if Item.ImageIndex > -1 then begin
    IL := Item.Images;
    if not Assigned(IL) then
      IL := DefaultImageList;
    if Assigned(IL) then begin
      ILRect := Bounds(ARect.Left, ARect.Top + ((L.ItemHeight - IL.Height) div 2), IL.Width, IL.Height);
      if odSelected in State then
        SpDrawXPIconSelection(ACanvas, ILRect, IL, Item.ImageIndex)
      else
        IL.Draw(ACanvas, ILRect.Left, ILRect.Top, Item.ImageIndex);
    end;
  end;

  // Draw the caption
  Inc(ARect.Left, L.ItemHeight); // Apply margins
  WS := L.Items[Index];
  TextR := ARect;
  Y := SpDrawXPText(ACanvas, WS, TextR, DT_CALCRECT);
  ARect.Top := ARect.Top + ((L.ItemHeight - Y) div 2);
  SpDrawXPText(ACanvas, WS, ARect, 0);

  // Draw the shortcut
  if DrawShortcut then begin
    WS := Item.GetShortCutText;
    if (WS <> '0') and (WS <> '') then begin
      ARect.Left := L.Width - 80;
      SpDrawXPText(ACanvas, WS, ARect, 0)
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomizeForm }

procedure TSpTBXCustomizeForm.FormCreate(Sender: TObject);
var
  L: TStringList;
begin
  ClosePanel.Visible := not Embedded;

  // Setup the listboxes
  if Assigned(Customizer.Images) then begin
    lbCommands.ItemHeight := Customizer.Images.Height + 4;
    lbShortcuts.ItemHeight := Customizer.Images.Height + 4;
  end;
  // Hide the Icon Options combobox if necessary
  if not Assigned(Customizer.OnIconOptionsChange) then begin
    cbIconLabel.Visible := False;
    cbIcon.Visible := False;
  end;
  // Fill the Themes combobox
  L := TStringList.Create;
  try
    GetAvailableTBXThemes(L);
    L.Sort;
    cbThemes.Items.Assign(L);
    cbThemes.ItemIndex := L.IndexOf(TBXCurrentTheme);
  finally
    L.Free;
  end;
end;

procedure TSpTBXCustomizeForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  // Allow to change the tab index using Ctrl+Tab and Shift+Ctrl+Tab
  if Key = VK_TAB then
    if Shift = [ssCtrl] then begin
      I := SpTBXTabControl1.ActiveTabIndex;
      if I + 1 < SpTBXTabControl1.PagesCount then
        SpTBXTabControl1.ActiveTabIndex := I + 1
      else
        SpTBXTabControl1.ActiveTabIndex := I - 1
    end
    else
      if Shift = [ssCtrl, ssShift] then begin
        I := SpTBXTabControl1.ActiveTabIndex;
        if I - 1 > -1 then
          SpTBXTabControl1.ActiveTabIndex := I - 1
        else
          SpTBXTabControl1.ActiveTabIndex := I + 1;
      end;
end;

procedure TSpTBXCustomizeForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSpTBXCustomizeForm.DoFillCommands(ToolbarList, ItemList, ShortcutsList: TTntStringList);
var
  I: Integer;
  TB: TSpTBXToolbar;
begin
  lbToolbars.Items.Assign(ToolbarList);
  lbCommands.Items.Assign(ItemList);
  lbShortcuts.Items.Assign(ShortcutsList);
  // Setup the Toolbars check list
  for I := 0 to lbToolbars.Count - 1 do begin
    TB := lbToolbars.Items.Objects[I] as TSpTBXToolbar;
    if TB.Visible then
      lbToolbars.State[I] := cbChecked;
  end;
  if lbToolbars.Count > 0 then begin
    lbToolbars.ItemIndex := 0;
    lbToolbarsClick(nil);
  end;
end;

procedure TSpTBXCustomizeForm.DoThemeChange;
begin
  inherited;
  // [TBXTheme-Change]
  // Change the white Xito color to gray
  if TBXCurrentTheme = 'Xito' then
    ClosePanel.Color := $00E0E0E0
  else
    ClosePanel.Color := CurrentTheme.GetViewColor(VT_TOOLBAR);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Toolbars tab }

procedure TSpTBXCustomizeForm.lbToolbarsClick(Sender: TObject);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  I := lbToolbars.ItemIndex;
  if I > -1 then begin
    TB := lbToolbars.Items.Objects[I] as TTBCustomToolbar;
    checkVisible.Checked := TB.Visible;
    if TB is TSpTBXToolbar then
      cbText.ItemIndex := Ord(TSpTBXToolbar(TB).DisplayMode)
    else
      cbText.ItemIndex := -1;
  end;
end;

procedure TSpTBXCustomizeForm.lbToolbarsClickCheck(Sender: TObject);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  I := lbToolbars.ItemIndex;
  if I > -1 then begin
    TB := lbToolbars.Items.Objects[I] as TTBCustomToolbar;
    TB.Visible := lbToolbars.Checked[I];
  end;
end;

procedure TSpTBXCustomizeForm.checkVisibleClick(Sender: TObject);
var
  I: Integer;
begin
  I := lbToolbars.ItemIndex;
  if I > -1 then begin
    lbToolbars.Checked[I] := checkVisible.Checked;
    lbToolbarsClickCheck(nil);
  end;
end;

procedure TSpTBXCustomizeForm.cbTextClick(Sender: TObject);
var
  I: Integer;
  TB: TSpTBXToolbar;
begin
  I := lbToolbars.ItemIndex;
  if (I > -1) and (cbText.ItemIndex > -1) then
    if lbToolbars.Items.Objects[I] is TSpTBXToolbar then begin
      TB := lbToolbars.Items.Objects[I] as TSpTBXToolbar;
      TB.DisplayMode := TSpTBXToolbarDisplayMode(cbText.ItemIndex);
    end;
end;

procedure TSpTBXCustomizeForm.cbThemesClick(Sender: TObject);
begin
  if cbThemes.ItemIndex > -1 then
    TBXSetTheme(cbThemes.Text);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Commands tab }

procedure TSpTBXCustomizeForm.lbCommandsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  L: TTntListBox;
  I: Integer;
  Item: TTBCustomItem;
  Sep: TTBSeparatorItem;
begin
  L := Sender as TTntListBox;
  I := L.ItemIndex;
  if I > -1 then begin
    if I = 0 then begin
      // When dragging the separator item use the first one available, or create one
      if FSeparatorList.Count = 0 then begin
        Sep := SpCreateUniqueSeparator;
        Customizer.Items.Add(Sep);
        FSeparatorList.AddObject(Sep.Name, Sep);
      end;
      Item := FSeparatorList.Objects[0] as TTBCustomItem;
      // SpOutputDebugString('Start ' + FSeparatorList[0]);
    end
    else
      Item := L.Items.Objects[I] as TTBCustomItem;
    DragObject := TSpTBXItemDragObject.Create(L, Item);
  end;
end;

procedure TSpTBXCustomizeForm.lbCommandsEndDrag(Sender,
  Target: TObject; X, Y: Integer);
var
  Accepted: Boolean;
  I: Integer;
begin
  // When dropping an item on a toolbar we must remove the item from the list
  Accepted := Assigned(Target) and (Target <> Sender);
  if Accepted then begin
    I := lbCommands.ItemIndex;
    if I > -1 then
      if I = 0 then begin
        // SpOutputDebugString('End ' + FSeparatorList[0]);
        FSeparatorList.Delete(0);
      end
      else
        lbCommands.Items.Delete(I);
  end;
end;

procedure TSpTBXCustomizeForm.lbCommandsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(Source) and (Source is TSpTBXItemDragObject);
end;

procedure TSpTBXCustomizeForm.lbCommandsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  OrigItem: TTBCustomItem;
  WS: WideString;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) and
    (TSpTBXItemDragObject(Source).SourceControl <> Sender) then
  begin
    OrigItem := TSpTBXItemDragObject(Source).SouceItem;
    // Remove the item from its parent
    OrigItem.Parent.Remove(OrigItem);
    // Add the item to the Customizer.Items property
    Customizer.Items.Add(OrigItem);
    WS := SpCustomizerGetWideCaption(OrigItem);
    // Add the item entry in the commands list
    if OrigItem is TTBSeparatorItem then
      FSeparatorList.InsertObject(0, WS, OrigItem) // Insert the separator in the first position
    else
      lbCommands.AddItem(WS, OrigItem);
    // SpOutputDebugString('Dropped ' + WS);
  end;
end;

procedure TSpTBXCustomizeForm.lbCommandsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  ItemInfo: TTBXItemInfo;
  R: TRect;
begin
  if Index = 0 then begin
    // Draw the separator
    C := TTntListBox(Control).Canvas;
    SpFillItemInfo(True, False, False, False, ItemInfo);
    R := Rect;
    InflateRect(R, -100, -4);
    CurrentTheme.PaintSeparator(C, R, ItemInfo, True, False);
  end
  else
    SpDrawListboxItem(Control as TTntListBox, Index, Rect, State, False, Customizer.Images);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Shortcuts tab }

procedure TSpTBXCustomizeForm.lbShortcutsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SpDrawListboxItem(Control as TTntListBox, Index, Rect, State, True, Customizer.Images);
end;

procedure TSpTBXCustomizeForm.lbShortcutsClick(Sender: TObject);
var
  Item: TTBCustomItem;
begin
  if lbShortcuts.ItemIndex > -1 then begin
    Item := lbShortcuts.Items.Objects[lbShortcuts.ItemIndex] as TTBCustomItem;
    HotKey1.HotKey := Item.ShortCut;
  end;
end;

procedure TSpTBXCustomizeForm.ChangeShortcutClick(Sender: TObject);
var
  Item, F: TTBCustomItem;
  WS, WS2: WideString;
  T: TShortCut;
  I: integer;
begin
  if lbShortcuts.ItemIndex < 0 then Exit;
  T := HotKey1.HotKey;
  Item := lbShortcuts.Items.Objects[lbShortcuts.ItemIndex] as TTBCustomItem;

  if T = 0 then
    Item.ShortCut := T
  else begin
    WS := '';
    WS2 := SSpTBXCustomizerFormChangeShortcutTitle;

    // Find if the shortcut is already being used
    for I := 0 to lbShortcuts.Count - 1 do begin
      F := lbShortcuts.Items.Objects[I] as TTBCustomItem;
      if F.ShortCut = T then begin
        WS := WideFormat(SSpTBXCustomizerFormChangeShortcut, [SpCustomizerGetWideCaption(F), SpCustomizerGetWideCaption(Item)]);
        Break;
      end;
    end;

    if (WS = '') or (MessageBoxW(Handle, PWideChar(WS), PWideChar(WS2), mb_applmodal+mb_iconwarning+mb_okcancel+mb_defbutton1) = IDOK) then
    begin
      if Assigned(Item.Action) and (Item.Action is TAction) then
        TAction(Item.Action).ShortCut := T
      else
        Item.ShortCut := T;
    end;
  end;
end;

end.

