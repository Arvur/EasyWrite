unit SpTBXReg;

{==============================================================================
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
==============================================================================}

interface

{$I TB2Ver.inc}

uses
  Windows, Classes, Controls, SysUtils, Graphics, ImgList, Dialogs,
  {$IFDEF JR_D6} DesignIntf, DesignEditors, VCLEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB2Reg, TB2Toolbar, TB2Item, TBX, TB2DsgnItemEditor,
  SpTBXItem, SpTBXTabs, SpTBXDkPanels, SpTBXFormPopupMenu,
  SpTBXControls, SpTBXEditors, SpTBXLists, SpTBXCustomizer;

procedure Register;

implementation

uses
  Forms, TBXThemes, TBXStrEdit, TBXUtils, TypInfo;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ THookObj }

type
  THookObj = class
    procedure HookProc(Sender: TTBItemEditForm);
  end;

var SpTBXHook: THookObj;

procedure THookObj.HookProc(Sender: TTBItemEditForm);
var
  TB: TTBToolbar;
  Item: TTBCustomItem;
  NewItem: TTBItem;
  S: string;
  I: Integer;
begin
  // Create our own toolbar in the editor
  TB := TTBToolbar.Create(Sender);
  TB.Top := Sender.Height;
  TB.Parent := Sender;
  TB.Align := alTop;
  TB.Images := Sender.ToolbarItems.SubMenuImages;
  TB.ShowHint := True;
  TB.Name := 'SpTBXToolbar';

  for I := 0 to Sender.MoreMenu.Count - 1 do
  begin
    Item := Sender.MoreMenu.Items[I];
    if Item is TTBCustomItem then
    begin
      S := TTBCustomItemClass(Item.Tag).ClassName;
      if StrLComp(PChar(S), 'TSpTBX', 6) = 0 then
      begin
        NewItem := TTBItem.Create(TB);
        TB.Items.Add(NewItem);
        NewItem.Caption := Item.Caption;
        NewItem.ImageIndex := Item.ImageIndex;
        NewItem.Tag := Item.Tag;
        NewItem.Hint := S;
        NewItem.OnClick := Item.OnClick;
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM

procedure Register;
begin
  {$IFDEF JR_D9}
  ForceDemandLoadState(dlDisable);
  {$ENDIF}
  
  RegisterComponents('SpTBXLib', [TSpTBXDock, TSpTBXMultiDock, TSpTBXToolbar,
    TSpTBXDockablePanel, TSpTBXTabSet, TSpTBXTabControl, TSpTBXStatusBar,
    TSpTBXPopupMenu, TSpTBXFormPopupMenu, TSpTBXTitleBar, TSpTBXLabel, TSpTBXCheckBox,
    TSpTBXRadioButton, TSpTBXButton, TSpTBXSpeedButton, TSpTBXProgressBar, TSpTBXTrackBar,
    TSpTBXSplitter, TSpTBXPanel, TSpTBXGroupBox, TSpTBXRadioGroup,
    TSpTBXEdit, TSpTBXButtonEdit, TSpTBXSpinEdit, TSpTBXComboBox,
    TSpTBXListBox, TSpTBXCheckListBox, TSpTBXCustomizer]);

  RegisterClasses([TSpTBXTabSheet, TSpTBXCustomTabSet, TSpTBXCustomTabControl]);

  // TSpTBXItem
  RegisterNoIcon([TSpTBXItem]);
  RegisterClasses([TSpTBXItem]);
  TBRegisterItemClass(TSpTBXItem, 'New SpTBX Item', HInstance);
  // TSpTBXSubmenuItem
  RegisterNoIcon([TSpTBXSubmenuItem]);
  RegisterClasses([TSpTBXSubmenuItem]);
  TBRegisterItemClass(TSpTBXSubmenuItem, 'New SpTBX Submenu Item', HInstance);
  // TSpTBXSeparatorItem
  RegisterNoIcon([TSpTBXSeparatorItem]);
  RegisterClasses([TSpTBXSeparatorItem]);
  TBRegisterItemClass(TSpTBXSeparatorItem, 'New SpTBX Separator Item', HInstance);
  // TSpTBXSpacerItem
  RegisterNoIcon([TSpTBXRightAlignSpacerItem]);
  RegisterClasses([TSpTBXRightAlignSpacerItem]);
  TBRegisterItemClass(TSpTBXRightAlignSpacerItem, 'New SpTBX RightAlignSpacer Item', HInstance);
  // TSpTBXLabelItem
  RegisterNoIcon([TSpTBXLabelItem]);
  RegisterClasses([TSpTBXLabelItem]);
  TBRegisterItemClass(TSpTBXLabelItem, 'New SpTBX Label Item', HInstance);
  // TSpTBXThemeGroupItem
  RegisterNoIcon([TSpTBXThemeGroupItem]);
  RegisterClasses([TSpTBXThemeGroupItem]);
  TBRegisterItemClass(TSpTBXThemeGroupItem, 'New SpTBX ThemeGroup Item', HInstance);
  // TSpTBXTabItem
  RegisterNoIcon([TSpTBXTabItem]);
  RegisterClasses([TSpTBXTabItem]);
  TBRegisterItemClass(TSpTBXTabItem, 'New SpTBX Tab Item', HInstance);
  // TSpTBXEditItem
  RegisterNoIcon([TSpTBXEditItem]);
  RegisterClasses([TSpTBXEditItem]);
  TBRegisterItemClass(TSpTBXEditItem, 'New SpTBX Edit Item', HInstance);
  // TSpTBXDropDownItem
  RegisterNoIcon([TSpTBXDropDownItem]);
  RegisterClasses([TSpTBXDropDownItem]);
  TBRegisterItemClass(TSpTBXDropDownItem, 'New SpTBX Drop Down Item', HInstance);
  // TSpTBXComboBoxItem
  RegisterNoIcon([TSpTBXComboBoxItem]);
  RegisterClasses([TSpTBXComboBoxItem]);
  TBRegisterItemClass(TSpTBXComboBoxItem, 'New SpTBX Combo Box Item', HInstance);
  // TSpTBXStringList
  RegisterNoIcon([TSpTBXStringList]);
  RegisterClasses([TSpTBXStringList]);
  TBRegisterItemClass(TSpTBXStringList, 'New SpTBX String List Item', HInstance);
  // TSpTBXUndoList
  RegisterNoIcon([TSpTBXUndoList]);
  RegisterClasses([TSpTBXUndoList]);
  TBRegisterItemClass(TSpTBXUndoList, 'New SpTBX Undo List Item', HInstance);

  // Register the components editor, the components must implement IItems interface
  RegisterComponentEditor(TSpTBXCompoundItemsControl, TTBItemsEditor);
  RegisterComponentEditor(TSpTBXDockablePanel, TTBItemsEditor);
  RegisterComponentEditor(TSpTBXCustomizer, TTBItemsEditor);
end;

initialization
  SpTBXHook := THookObj.Create;
  TBUnregisterDsgnEditorHook(SpTBXHook.HookProc);
  TBRegisterDsgnEditorHook(SpTBXHook.HookProc);

finalization
  TBUnregisterDsgnEditorHook(SpTBXHook.HookProc);
  FreeAndNil(SpTBXHook);

end.
