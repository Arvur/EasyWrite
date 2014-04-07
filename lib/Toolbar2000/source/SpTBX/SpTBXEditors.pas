unit SpTBXEditors;

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

Wish list for TB2K/TBX:
  - It's very difficult to enable unicode support on a TTBXEditItem
    descendant, to facilitate this String types should be changed to
    WideString.
  - Make TTBXEditItemViewer's MeasureEditCaption and MeasureTextHeight
    virtual methods.
  - Fixed editors bug, the editors autocomplete was case sensitive,
    this is a TBX bug but it was fixed without patching the source,
    thanks to Daniel Rikowski for reporting this.
    The fix is in TSpTBXComboBoxItem.DoAutoComplete, also CompareStringW
    should be used when comparing strings and not "=" or "<>".
  - Fixed TBXDropDownItem and TBXComboBoxItem bugs, the popup list should
    be closed when F4 is pressed, fixed in
    TSpTBXDropDownItemViewer.HandleEditMessage, thanks to Rune Moberg.
  - Fixed TBXComboBoxItem bug, the ComboBox didn't check the ItemIndex
    bounds when pressing Up or Down keys, fixed in
    TSpTBXComboBoxItemViewer.HandleEditMessage, thanks to Rune Moberg.
  - Fixed TBXComboBoxItem bug, when AutoComplete is set to false
    the ComboBox still autocompletes the text.
  - Fixed TBXEditItemViewer bug (it affects all the TBX edit items), the
    item is not painted using the color of FontSettings and
    EditorFontSettings properties.

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.

To Do:
  - Rotated caption painting.

Known Issues:
  -

History:
8 February 2007 - version 1.8.3
  - Fixed incorrect OnChange event handling in TSpTBXEditItem,
    thanks to Daniel Rikowski for reporting this.
  
17 December 2006 - version 1.8.2
  - Fixed a BDS 2006 bug related to Comboboxes, CM_MOUSEENTER and 
    CM_MOUSELEAVE are fired everytime the mouse is moved over the 
    internal edit control. In D7 these messages were only fired when
    the mouse entered or leaved the combobox.

24 November 2006 - version 1.8.1
  - Fixed incorrect TSpTBXSpinEdit behavior, the Value was not updated
    when the control was unfocused, thanks to Steve and Sebastian for
    reporting this.

27 August 2006 - version 1.8
  - Improved editor's button painting.

15 June 2006 - version 1.7
  - Fixed edit items incorrect painting, the items were not painted
    using the color of FontSettings and EditorFontSettings properties,
    the same happens with the TBX items.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - Added ValueType, ValueAsInteger, Decimals, Prefix and Postfix
    properties to TSpTBXSpinEdit, thanks to Maxim Rylov for his
    code donation.
  - Fixed TSpTBXSpinEdit painting.

27 February 2006 - version 1.4
  - New component added, TSpTBXSpinEdit: a SpinEdit control
    that has TBX themes support.
  - Fixed TSpTBXComboBoxItem bug, when AutoComplete is set to
    false the ComboBox still autocompletes the text, thanks to
    Erwin Denissen for reporting this.
  - Fixed Delphi 2005/2006 bug, CM_MOUSEENTER and CM_MOUSELEAVE
    are fired everytime the mouse enters the combobox internal
    edit control. In prior versions of Delphi these messages
    were only fired when the mouse entered or leaved the combobox,
    including the internal edit control.

10 February 2006 - version 1.3
  - New component added, TSpTBXButtonEdit: an Edit control that
    has a multipurpose button attached.
  - Added new public method, AddEditButton, to TSpTBXEdit.

28 December 2005 - version 1.2
  - No changes.

18 October 2005 - version 1.1
  - New component added, TSpTBXListBox: a ListBox
    with Unicode and TBX themes support that paints
    a hottrack border and TBX theme style selection.
  - New component added, TSpTBXCheckListBox: a CheckListBox
    with Unicode and TBX themes support that paints
    a hottrack border and TBX theme style selection.
  - Fixed TSpTBXComboBoxItem dynamic creation problem.

18 August 2005 - version 1.0
  - No changes.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - Fixed TSpTBXDropDownItem and TSpTBXComboBoxItem bugs, the popup list
    should be closed when F4 is pressed, thanks to Rune Moberg for
    reporting this.
  - Fixed TSpTBXComboBoxItem bug, the ComboBox didn't check the ItemIndex
    bounds when pressing Up or Down keys, thanks to Rune Moberg for
    reporting this.
  - Fixed AV when trying to dock a toolbar with a TSpTBXComboBoxItem
    on a vertical dock, thanks to Pavel for reporting this.

16 February 2005 - version 0.7
  - Fixed unicode support in W9x, thanks to Daniel Rikowski for
    reporting this.
  - Fixed editors bug, the editors autocomplete was case sensitive,
    this is a TBX bug but it was fixed without patching the source,
    thanks to Daniel Rikowski for reporting this.
  - Fixed TSpTBXComboBox painting bug, the edit frame was not
    correctly highlighted when using the Default theme.
  - Added HotTrack property to TSpTBXEdit and TSpTBXComboBox, when
    setted to true a TBX style frame will be painted when the mouse
    is over the control.
  - Added OnDrawBackground event to TSpTBXEdit and TSpTBXComboBox.

23 December 2004 - version 0.6
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList,
  TB2Common, TB2Toolbar, TB2Item, TB2ExtItems,
  TBX, TBXExtItems, TBXThemes, TBXLists, SpTBXItem, SpTBXLists, SpTBXControls,
  TntClasses, TntControls, TntStdCtrls, TntCheckLst, TntSysUtils;

type
  TSpTBXEditChangeEvent = procedure(Sender: TObject; const Text: WideString) of object;
  TSpTBXEditAcceptTextEvent = procedure(Sender: TObject;var NewText: WideString; var Accept: Boolean) of object;

  { TSpTBXEditButton }

  TSpTBXEditButton = class(TSpTBXSpeedButton)
  protected
    function DoDrawDropDownArrow(ACanvas: TCanvas; ARect: TRect): Boolean; override;
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property ThemeType default thtTBX;
  end;

  TSpTBXSpinButton = class(TSpTBXEditButton)
  private
    FState: TSEBtnState;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
  protected
    function DoDrawItem(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage): Boolean; override;
    procedure DoMouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure IsHotTracking(out UpButton, DownButton, EditFrame: Boolean);
    property State: TSEBtnState read FState;
  published
    property Repeating default True;
  end;

  { TSpTBXUnicodeEdit }
  // Do not inherit from TTNTEdit, TBEditItemViewer.GetEditControlClass needs a TEditClass
  TSpTBXUnicodeEdit = class(TEdit)
  private
    FPasswordChar: WideChar;
    FAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure SetSelText(const Value: WideString);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    function GetPasswordChar: WideChar;
    procedure SetPasswordChar(const Value: WideChar);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetSelStart: Integer; reintroduce; virtual;
    procedure SetSelStart(const Value: Integer); reintroduce; virtual;
    function GetSelLength: Integer; reintroduce; virtual;
    procedure SetSelLength(const Value: Integer); reintroduce; virtual;
    function GetSelText: WideString; reintroduce; virtual;
    procedure UpdateEditRect; virtual;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    function AddEditButton(AAlign: TAlign = alRight; AWidth: Integer = -1): TSpTBXEditButton;
    property SelText: WideString read GetSelText write SetSelText;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Text: WideString read GetText write SetText;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property PasswordChar: WideChar read GetPasswordChar write SetPasswordChar default #0;
  end;

  { TSpTBXEdit }

  TSpTBXEdit = class(TSpTBXUnicodeEdit)
  private
    FHotTrack: Boolean;
    FThemeType: TSpTBXThemeType;
    FMouseInControl: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure SetThemeType(const Value: TSpTBXThemeType);
  protected
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateFrame; virtual;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  end;

  { TSpTBXButtonEdit }

  TSpTBXButtonEdit = class(TSpTBXEdit)
  private
    FEditButton: TSpTBXEditButton;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EditButton: TSpTBXEditButton read FEditButton;
  end;

  { TSpTBXSpinEdit }

  TSpTBXSpinEdit = class(TSpTBXEdit)
  private
    FSpinButton: TSpTBXSpinButton;
    FDecimal: TDecimal;
    FIncrement: Extended;
    FMinValue: Extended;
    FMaxValue: Extended;
    FValue: Extended;
    FValueSnap: Boolean;
    FValueType: TSEValueType;
    FPrefix: WideString;
    FPostfix: WideString;
    FOnValueChanged: TNotifyEvent;
    function IsIncrementStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetDecimal(const NewDecimal: TDecimal);
    procedure SetMaxValue(const NewValue: Extended);
    procedure SetMinValue(const NewValue: Extended);
    procedure SetValue(const NewValue: Extended);
    procedure SetValueType(NewType: TSEValueType);
    procedure SetPostfix(const ValueString: WideString);
    procedure SetPrefix(const ValueString: WideString);
    function GetValueAsInteger: Integer;
    procedure SetValueAsInteger(const NewValue: Integer);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoValueChanged; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure UpdateTextFromValue;
    procedure UpdateValueFromText;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateFrame; override;
    procedure ValueInc;
    procedure ValueDec;
    property ValueAsInteger: Integer read GetValueAsInteger write SetValueAsInteger;
  published
    property Alignment default taRightJustify;
    property Text stored False;
    property Decimal: TDecimal read FDecimal write SetDecimal default 2;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinValueStored;
    property Postfix: WideString read FPostfix write SetPostfix;
    property Prefix: WideString read FPrefix write SetPrefix;
    property SpinButton: TSpTBXSpinButton read FSpinButton;
    property Value: Extended read FValue write SetValue stored IsValueStored;
    property ValueSnap: Boolean read FValueSnap write FValueSnap default True;
    property ValueType: TSEValueType read FValueType write SetValueType default evtInteger;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  end;

  { TSpTBXComboBox }

  TSpTBXComboBox = class(TTntComboBox)
  private
    FHotTrack: Boolean;
    FMouseInControl: Boolean;
    FMouseInDropDownButton: Boolean;
    FThemeType: TSpTBXThemeType;
    FMouseTimer: TTimer;
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure MouseTimerHandler(Sender: TObject);
    procedure SetThemeType(const Value: TSpTBXThemeType);
  protected
    procedure CloseUp; override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMouseInDropDownButton: Boolean;
    procedure InvalidateFrame;
    procedure UpdateDropDownButton;
    property MouseInControl: Boolean read FMouseInControl;
  published
    property HotTrack: Boolean read FHotTrack write FHotTrack default True;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnMouseMove;
  end;

  { TSpTBXListBox }

  TSpTBXListBox = class(TTntListBox)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FThemeType: TSpTBXThemeType;
    FChildFocused: Boolean;
    FOnDrawItemBackground: TDrawItemEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemBackground(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBorders;
    property HotTracking: Boolean read FHotTracking;
  published
    property Style default lbOwnerDrawFixed;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
    property OnDrawItemBackground: TDrawItemEvent read FOnDrawItemBackground write FOnDrawItemBackground;
  end;

  { TSpTBXCheckListBox }

  TSpTBXCheckListBox = class(TTntCheckListBox)
  private
    FHotTracking: Boolean;
    FHotTrack: Boolean;
    FThemeType: TSpTBXThemeType;
    FChildFocused: Boolean;
    FOnDrawItemBackground: TDrawItemEvent;
    procedure SetHotTrack(const Value: Boolean);
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DrawItemBackground(Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBorders;
    property HotTracking: Boolean read FHotTracking;
  published
    property Style default lbOwnerDrawFixed;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
    property OnDrawItemBackground: TDrawItemEvent read FOnDrawItemBackground write FOnDrawItemBackground;
  end;

  { TSpTBXEditItem }

  TSpTBXEditItemViewer = class;

  TSpTBXEditItem = class(TTBEditItem)
  private
    FEditCaption: WideString;
    FText: WideString;
    FHint: WideString;
    FAlignment: TAlignment;
    FAutoCompleteCounter: Integer;
    FEditorFontSettings: TFontSettings;
    FFontSettings: TFontSettings;
    FIsChanging: Boolean;
    FLastEditChange: WideString;
    FPasswordChar: WideChar;
    FReadOnly: Boolean;
    FShowImage: Boolean;
    FOnAcceptText: TSpTBXEditAcceptTextEvent;
    FOnChange: TSpTBXEditChangeEvent;
    FOnDrawHint: TSpTBXDrawHintEvent;
    procedure FontSettingsChanged(Sender: TObject);
    procedure SetAlignment(Value: TAlignment);
    procedure SetEditCaption(const Value: WideString);
    procedure SetHint(const Value: WideString);
    procedure SetText(Value: WideString);
    procedure SetPasswordChar(Value: WideChar);
    procedure SetShowImage(const Value: Boolean);
    procedure SetFontSettings(Value: TFontSettings);
  protected
    FActiveEditItemViewer: TSpTBXEditItemViewer;
    procedure DefineProperties(Filer: TFiler); override;
    function DoAcceptText(var NewText: string): Boolean; override;
    function DoAcceptTextW(var NewText: WideString): Boolean; virtual;
    function DoAutoComplete(var AText: WideString): Boolean; virtual;
    procedure DoBeginEdit(Viewer: TTBEditItemViewer); override;
    procedure DoChange(const AText: WideString); virtual;
    procedure DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean); virtual;
    procedure DoTextChanged(Reason: Integer); override;
    procedure DoTextChanging(const OldText: WideString; var NewText: WideString; Reason: Integer); reintroduce; virtual;
    function GetImageIndex: Integer; virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure GetPopupPosition(ParentView: TTBView; PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec); override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure HandleEditChange(Edit: TSpTBXUnicodeEdit); virtual;
    procedure SetTextEx(Value: WideString; Reason: Integer); virtual;
  public
    function StartEditing(AView: TTBView): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property EditorFontSettings: TFontSettings read FEditorFontSettings write FEditorFontSettings;
    property ExtendedAccept;
    property FontSettings: TFontSettings read FFontSettings write SetFontSettings;
    property ImageIndex;
    property Images;
    property PasswordChar: WideChar read FPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowImage: Boolean read FShowImage write SetShowImage default False;
    property OnSelect;
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property EditCaption: WideString read FEditCaption write SetEditCaption; // Hides the inherited EditCaption
    property Hint: WideString read FHint write SetHint; // Hides the inherited Hint
    property Text: WideString read FText write SetText; // Hides the inherited Text
    property OnChange: TSpTBXEditChangeEvent read FOnChange write FOnChange;
    property OnAcceptText: TSpTBXEditAcceptTextEvent read FOnAcceptText write FOnAcceptText; // Hides the inherited OnAcceptText
    property OnDrawHint: TSpTBXDrawHintEvent read FOnDrawHint write FOnDrawHint;
  end;

  TSpTBXEditItemViewer = class(TTBEditItemViewer)
  private
    function GetItem: TSpTBXEditItem;
    function GetEditControl: TSpTBXUnicodeEdit;
    procedure EditChangeHandler(Sender: TObject);
    function MeasureEditCaption: TSize;
    function MeasureTextHeight: Integer;
    procedure GetEditHeight(const DC: HDC; out EditHeight, ExternalLeading: Integer);
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    OldWndProc: TWndMethod;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function DoExecute: Boolean; override;
    function HandleEditMessage(var Message: TMessage): Boolean; virtual;
    function  GetAccRole: Integer; override;
    procedure GetItemInfo(out ItemInfo: TTBXItemInfo; IsHoverItem, IsPushed, UseMenuColor: Boolean); virtual;
    function GetEditControlClass: TEditClass; override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); virtual;
    function  GetIndentBefore: Integer; virtual;
    function  GetIndentAfter: Integer; virtual;
    procedure GetEditRect(var R: TRect); override;
    function  IsToolbarSize: Boolean; override;
    procedure NewEditWndProc(var Message: TMessage);
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect; IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
    function  ShowImage: Boolean; virtual;
  public
    destructor Destroy; override;
    function  IsToolbarStyle: Boolean; override;
    property EditControl: TSpTBXUnicodeEdit read GetEditControl;
    function GetHintText: Widestring;           // Hides the inherited TB2K GetHintText function
    property Item: TSpTBXEditItem read GetItem; // Hides the inherited TB2K Item property
  end;

  { TSpTBXDropDownItem }

  TSpTBXCustomDropDownItem = class(TSpTBXEditItem)
  private
    FAlwaysSelectFirst: Boolean;
    FDropDownList: Boolean;
  protected
    function CreatePopup(const ParentView: TTBView; const ParentViewer: TTBItemViewer;
      const PositionAsSubmenu, SelectFirstItem, Customizing: Boolean;
      const APopupPoint: TPoint; const Alignment: TTBPopupAlignment): TTBPopupWindow; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property AlwaysSelectFirst: Boolean read FAlwaysSelectFirst write FAlwaysSelectFirst default True;
    property DropDownList: Boolean read FDropDownList write FDropDownList default False;
  end;

  TSpTBXDropDownItem = class(TSpTBXCustomDropDownItem)
  published
    property AlwaysSelectFirst;
    property DropDownList;
    property LinkSubitems;
    property SubMenuImages;
  end;

  TSpTBXDropDownItemViewer = class(TSpTBXEditItemViewer)
  protected
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    procedure GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo); override;
    function GetIndentAfter: Integer; override;
    function HandleEditMessage(var Message: TMessage): Boolean; override;
    function IsPtInButtonPart(X, Y: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TSpTBXComboBoxItem }

  TSpTBXComboBoxItem = class;
  TSpTBXCAdjustImageIndex = procedure(Sender: TSpTBXComboBoxItem; const AText: WideString;
    AIndex: Integer; var ImageIndex: Integer) of object;

  TSpTBXComboBoxItem = class(TSpTBXCustomDropDownItem)
  private
    FAutoComplete: Boolean;
    FList: TSpTBXStringList;
    FOnItemClick: TNotifyEvent;
    FOnAdjustImageIndex: TSpTBXCAdjustImageIndex;
    procedure AdjustImageIndexHandler(Sender: TTBXCustomList; AItemIndex: Integer; var ImageIndex: Integer);
    function GetItemIndex: Integer;
    function GetMaxVisibleItems: Integer;
    function GetMaxWidth: Integer;
    function GetMinWidth: Integer;
    function GetStrings: TTntStrings;
    function GetShowListImages: Boolean;
    function GetOnClearItem: TTBXLPaintEvent;
    function GetOnDrawItem: TTBXLPaintEvent;
    function GetOnMeasureHeight: TTBXLMeasureHeight;
    function GetOnMeasureWidth: TTBXLMeasureWidth;
    procedure ListChangeHandler(Sender: TObject);
    procedure ListClickHandler(Sender: TObject);
    procedure SetItemIndex(Value: Integer);
    procedure SetMaxVisibleItems(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetOnClearItem(Value: TTBXLPaintEvent);
    procedure SetOnDrawItem(Value: TTBXLPaintEvent);
    procedure SetOnMeasureHeight(Value: TTBXLMeasureHeight);
    procedure SetOnMeasureWidth(Value: TTBXLMeasureWidth);
    procedure SetStrings(Value: TTntStrings);
    procedure SetShowListImages(Value: Boolean);
  protected
    CachedImageIndex: Integer;
    CacheValid: Boolean;
    IsChanging: Boolean;
    procedure AdjustImageIndex(const AText: WideString; AIndex: Integer; var ImageIndex: Integer); virtual;
    function DoAutoComplete(var AText: WideString): Boolean; override;
    procedure DoListChange; virtual;
    procedure DoListClick; virtual;
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
    function GetImageIndex: Integer; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetStringListClass: TSpTBXStringListClass; virtual;
    procedure HandleEditChange(Edit: TSpTBXUnicodeEdit); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex default -1;
  published
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property DropDownList;
    property MaxListWidth: Integer read GetMaxWidth write SetMaxWidth default 0;
    property MaxVisibleItems: Integer read GetMaxVisibleItems write SetMaxVisibleItems default 8;
    property MinListWidth: Integer read GetMinWidth write SetMinWidth default 64;
    property ShowListImages: Boolean read GetShowListImages write SetShowListImages default False;
    property Strings: TTntStrings read GetStrings write SetStrings;
    property SubMenuImages;
    property OnChange;
    property OnAdjustImageIndex: TSpTBXCAdjustImageIndex read FOnAdjustImageIndex write FOnAdjustImageIndex;
    property OnClearItem: TTBXLPaintEvent read GetOnClearItem write SetOnClearItem;
    property OnDrawItem: TTBXLPaintEvent read GetOnDrawItem write SetOnDrawItem;
    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnMeasureHeight: TTBXLMeasureHeight read GetOnMeasureHeight write SetOnMeasureHeight;
    property OnMeasureWidth: TTBXLMeasureWidth read GetOnMeasureWidth write SetOnMeasureWidth;
    property OnPopup;
  end;

  TSpTBXComboBoxItemViewer = class(TSpTBXDropDownItemViewer)
  protected
    function HandleEditMessage(var Message: TMessage): Boolean; override;
  end;

{ Helpers }
function SpStartsTextW(const ASubText, AText: WideString): Boolean;
function SpFocusEditItem(Item: TTBCustomItem; View: TTBView): Boolean;

{ Painting helpers }
procedure SpDrawXPComboBoxFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack, DroppedDown: Boolean; ThemeType: TSpTBXThemeType; ButtonWidth: Integer);
procedure SpDrawXPComboButton(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack, DroppedDown: Boolean; ThemeType: TSpTBXThemeType);
procedure SpDrawXPSpinButton(ACanvas: TCanvas; ARect: TRect; Enabled, UpHotTrack, DownHotTrack, UpPushed, DownPushed: Boolean; ThemeType: TSpTBXThemeType);

implementation

uses
  Math, TntActnList, TntWindows, TBXUxThemes;

type
  TTBViewAccess = class(TTBView);
  TFontSettingsAccess = class(TFontSettings);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

function SpStartsTextW(const ASubText, AText: WideString): Boolean;
var
  L, L2: Integer;
begin
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then Result := False
  else Result := TntWindows.Tnt_CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    PWideChar(AText), L, PWideChar(ASubText), L) = 2;
end;

function SpFocusEditItem(Item: TTBCustomItem; View: TTBView): Boolean;
var
  IV: TTBItemViewer;
begin
  Result := False;
  IV := View.Find(Item);
  if Assigned(IV) then begin
    View.Select(IV, False);
    View.EnterToolbarLoop([tbetExecuteSelected]);
    Result := True;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

procedure SpDrawXPComboBoxFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack, DroppedDown: Boolean;
  ThemeType: TSpTBXThemeType; ButtonWidth: Integer);
// Paints the ComboBox frame including the combo button.
// If ButtonWidth >= ARect width only the button is painted.
var
  ItemInfo: TTBXItemInfo;
  EditInfo: TTBXEditInfo;
  ButtonState: Integer;
  ButtonR: TRect;
  DrawButtonOnly: Boolean;
begin
  DrawButtonOnly := ButtonWidth >= (ARect.Right - ARect.Left);

  ThemeType := SpXPThemeType(ThemeType);
  case ThemeType of
    thtWindows:
      begin
        if not DrawButtonOnly then
          DrawThemeBackground(COMBO_THEME, ACanvas.Handle, 0, 0, ARect, nil);

        if ButtonWidth > 0 then begin
          if not Enabled then ButtonState := CBXS_DISABLED
          else if DroppedDown then ButtonState := CBXS_PRESSED
          else if HotTrack then ButtonState := CBXS_HOT
          else ButtonState := CBXS_NORMAL;
          if not DrawButtonOnly then begin
            InflateRect(ARect, -1, -1);
            ARect.Left := ARect.Right - ButtonWidth;
          end;
          DrawThemeBackground(COMBO_THEME, ACanvas.Handle, CP_DROPDOWNBUTTON, ButtonState, ARect, nil);
        end;
      end;
    thtNone: ;
    thtTBX:
      begin
        SpFillItemInfo(Enabled, DroppedDown, HotTrack, False, ItemInfo);
        ItemInfo.ViewType := 0; // needed to draw the dropdown button background
        FillChar(EditInfo, SizeOf(TTBXEditInfo), 0);
        if ButtonWidth > 0 then begin
          if not Enabled then ButtonState := EBDS_DISABLED
          else if DroppedDown then ButtonState := EBDS_PRESSED
          else if HotTrack then ButtonState := EBDS_HOT
          else ButtonState := 0;
          EditInfo.RightBtnWidth := ButtonWidth;
          EditInfo.RightBtnInfo.ButtonType := EBT_DROPDOWN;
          EditInfo.RightBtnInfo.ButtonState := ButtonState;
        end;
        // Draw the dropdown button background
        ButtonR := ARect;
        InflateRect(ButtonR, -2, -2);
        if CurrentTheme.PaintDockBackground then
          CurrentTheme.PaintBackgnd(ACanvas, ButtonR, ButtonR, ButtonR, CurrentTheme.GetViewColor(TVT_NORMALTOOLBAR), False, TVT_NORMALTOOLBAR)
        else
          CurrentTheme.PaintDock(ACanvas, ButtonR, ButtonR, DP_TOP);
        // Draw the edit frame and dropdown button
        CurrentTheme.PaintEditFrame(ACanvas, ARect, ItemInfo, EditInfo)
      end;
  end;
end;

procedure SpDrawXPComboButton(ACanvas: TCanvas; ARect: TRect; Enabled,
  HotTrack, DroppedDown: Boolean; ThemeType: TSpTBXThemeType);
begin
  SpDrawXPComboBoxFrame(ACanvas, ARect, Enabled, HotTrack, DroppedDown, ThemeType, ARect.Right - ARect.Left);
end;

procedure SpDrawXPSpinButton(ACanvas: TCanvas; ARect: TRect; Enabled,
  UpHotTrack, DownHotTrack, UpPushed, DownPushed: Boolean; ThemeType: TSpTBXThemeType);
var
  ItemInfo: TTBXItemInfo;
  ButtonR, BR: TRect;
  StateFlags: Integer;
  Flags: Cardinal;
  X, Y: Integer;
const
  CDisabled: array [Boolean] of Integer = (EBSS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBSS_HOT);
begin
  ButtonR := ARect;
  SpFillItemInfo(Enabled, False, UpHotTrack or DownHotTrack, False, ItemInfo);

  ThemeType := SpXPThemeType(ThemeType);
  case ThemeType of
    thtWindows:
      begin
        InflateRect(ButtonR, 1, 1);
        // Up button
        BR := ButtonR;
        BR.Bottom := (ButtonR.Top + ButtonR.Bottom - 1) div 2;
        if not Enabled then StateFlags := UPS_DISABLED
        else if UpPushed then StateFlags := UPS_PRESSED
        else if UpHotTrack then StateFlags := UPS_HOT
        else StateFlags := UPS_NORMAL;
        DrawThemeBackground(SPIN_THEME, ACanvas.Handle, SPNP_UP, StateFlags, BR, nil);
        // Down button
        BR := ButtonR;
        BR.Top := (ButtonR.Top + ButtonR.Bottom) div 2;
        if not Enabled then StateFlags := DNS_DISABLED
        else if DownPushed then StateFlags := DNS_PRESSED
        else if DownHotTrack then StateFlags := DNS_HOT
        else StateFlags := DNS_NORMAL;
        DrawThemeBackground(SPIN_THEME, ACanvas.Handle, SPNP_DOWN, StateFlags, BR, nil);
      end;
    thtNone:
      begin
        // Up button
        Flags := DFCS_SCROLLUP;
        if UpPushed then
          Flags := Flags or DFCS_PUSHED;
        BR := Rect(ButtonR.Left, ButtonR.Top, ButtonR.Right, 1 + (ButtonR.Bottom - ButtonR.Top) div 2);
        DrawFrameControl(ACanvas.Handle, BR, DFC_SCROLL, Flags);
        // Down button
        Flags := DFCS_SCROLLDOWN;
        if DownPushed then
          Flags := Flags or DFCS_PUSHED;
        BR := Rect(ButtonR.Left, BR.Bottom - 1, ButtonR.Right, ButtonR.Bottom);
        DrawFrameControl(ACanvas.Handle, BR, DFC_SCROLL, Flags);
      end;
    thtTBX:
      begin
        // Up button
        BR := Rect(ButtonR.Left, ButtonR.Top, ButtonR.Right, 1 + (ButtonR.Bottom - ButtonR.Top) div 2);
        SpDrawXPRectButton(ACanvas, BR, Enabled, UpPushed, UpHotTrack, False, False, False, ThemeType);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom - 1) div 2;
        ACanvas.Pen.Color := clBlack;
        ACanvas.Brush.Color := clBlack;
        ACanvas.Polygon([Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)]);
        // Down button
        BR := Rect(ButtonR.Left, BR.Bottom - 1, ButtonR.Right, ButtonR.Bottom);
        SpDrawXPRectButton(ACanvas, BR, Enabled, DownPushed, DownHotTrack, False, False, False, ThemeType);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom - 1) div 2;
        ACanvas.Pen.Color := clBlack;
        ACanvas.Brush.Color := clBlack;
        ACanvas.Polygon([Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)]);
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditButton }

constructor TSpTBXEditButton.Create(AOwner: TComponent);
var
  Index: Integer;
const
  DefaultName = 'SubEditButton';
begin
  inherited;

  // Find unique name
  if Assigned(AOwner) then begin
    Index := 0;
    while AOwner.FindComponent(DefaultName + IntToStr(Index)) <> nil do
      Inc(Index);
    Name := DefaultName + IntToStr(Index);
  end;

  // Change the FPopupControl, we need to align
  // the DropdownMenu to the Edit control not the button.
  // FPopupControl is used in TSpTBXCustomButton.Click
  if Assigned(AOwner) and (AOwner is TControl) then
    FPopupControl := AOwner as TControl;

  SetSubComponent(True);
  ThemeType := thtTBX;
end;

procedure TSpTBXEditButton.Click;
begin
  if Assigned(Parent) and SpCanFocus(Parent) then
    Parent.SetFocus;
  inherited;
end;

function TSpTBXEditButton.DoDrawDropDownArrow(ACanvas: TCanvas;
  ARect: TRect): Boolean;
var
  T: TSpTBXThemeType;
begin
  if (Caption = '') and not IsImageShown then begin
    T := SpXPThemeType(ThemeType);
    if T = thtWindows then begin
      // Paint the default WindowsXP combo button
      Result := False;
      Exit;
    end;
  end;

  Result := inherited DoDrawDropDownArrow(ACanvas, ARect);
end;

function TSpTBXEditButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  T: TSpTBXThemeType;
begin
  T := SpXPThemeType(ThemeType);
  if T = thtWindows then
    InflateRect(ARect, 1, 1);

  // Draw rectangle buttons
  if (PaintStage = pstPrePaint) and not BitmapValid then begin
    Result := True;
    if Assigned(OnDraw) then OnDraw(Self, ACanvas, ARect, PaintStage, Result);
    if Result then begin
      if (Caption = '') and (T = thtWindows) and not IsImageShown then begin
        // Paint the default WindowsXP combo button
        SpDrawXPComboButton(ACanvas, ARect, Enabled, MouseInControl, Pushed, ThemeType);
      end
      else
        SpDrawXPRectButton(ACanvas, ARect, Enabled, Pushed, MouseInControl,
          Checked, Focused, Default, ThemeType);
    end;
  end
  else
    Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinButton }

constructor TSpTBXSpinButton.Create(AOwner: TComponent);
begin
  inherited;
  Repeating := True;
end;

procedure TSpTBXSpinButton.Click;
var
  P: TPoint;
begin
  FState := ebsNone;
  if Enabled then begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if P.Y < Height div 2 then begin
      FState := ebsUp;
      if Assigned(FOnUpClick) then FOnUpClick(Self);
    end
    else begin
      FState := ebsDown;
      if Assigned(FOnDownClick) then FOnDownClick(Self);
    end;
  end;

  inherited;
end;

function TSpTBXSpinButton.DoDrawItem(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage): Boolean;
var
  UpHotTrack, DownHotTrack, EditFrameHotTrack: Boolean;
begin
  // Draw rectangle buttons
  if (PaintStage = pstPrePaint) and not BitmapValid then begin
    Result := True;
    if Assigned(OnDraw) then OnDraw(Self, ACanvas, ARect, PaintStage, Result);
    if Result then begin
      IsHotTracking(UpHotTrack, DownHotTrack, EditFrameHotTrack);
      SpDrawXPSpinButton(ACanvas, ARect, Enabled, UpHotTrack, DownHotTrack, State = ebsUp, State = ebsDown, ThemeType);
    end;
  end
  else
    Result := inherited DoDrawItem(ACanvas, ARect, PaintStage);
end;

procedure TSpTBXSpinButton.IsHotTracking(out UpButton, DownButton, EditFrame: Boolean);
var
  Edit: TSpTBXEdit;
  P: TPoint;
  R: TRect;
begin
  UpButton := False;
  DownButton := False;
  EditFrame := False;

  if GetCursorPos(P) then begin
    P := ScreenToClient(P);
    R := Rect(0, 0, Width, Height div 2);
    UpButton := PtInRect(R, P);
    if not UpButton then begin
      R := Rect(0, Height div 2, Width, Height);
      DownButton := PtInRect(R, P);
    end;
  end;

  if Assigned(Owner) and (Owner is TSpTBXEdit) then begin
    Edit := Owner as TSpTBXEdit;
    if Edit.HotTrack then
      EditFrame := Edit.MouseInControl or Edit.Focused;
  end;
end;

procedure TSpTBXSpinButton.DoMouseLeave;
begin
  FState := ebsNone;
  inherited;
end;

procedure TSpTBXSpinButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FState := ebsNone;
  inherited;
end;

procedure TSpTBXSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Enabled then
    Repaint;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXUnicodeEdit }

procedure TSpTBXUnicodeEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  TntCustomEdit_CreateWindowHandle(Self, Params);
end;

procedure TSpTBXUnicodeEdit.CreateWnd;
begin
  inherited;
  TntCustomEdit_AfterInherited_CreateWnd(Self, FPasswordChar);
  if HandleAllocated then UpdateEditRect;
end;

procedure TSpTBXUnicodeEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Cardinal = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  // WS_CLIPCHILDREN needed for edit buttons
  Params.Style := Params.Style or Alignments[FAlignment] or WS_CLIPCHILDREN;
end;

procedure TSpTBXUnicodeEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TSpTBXUnicodeEdit.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TSpTBXUnicodeEdit.GetSelStart: Integer;
begin
  Result := TntCustomEdit_GetSelStart(Self);
end;

procedure TSpTBXUnicodeEdit.SetSelStart(const Value: Integer);
begin
  TntCustomEdit_SetSelStart(Self, Value);
end;

function TSpTBXUnicodeEdit.GetSelLength: Integer;
begin
  Result := TntCustomEdit_GetSelLength(Self);
end;

procedure TSpTBXUnicodeEdit.SetSelLength(const Value: Integer);
begin
  TntCustomEdit_SetSelLength(Self, Value);
end;

function TSpTBXUnicodeEdit.GetSelText: WideString;
begin
  Result := TntCustomEdit_GetSelText(Self);
end;

procedure TSpTBXUnicodeEdit.SetSelText(const Value: WideString);
begin
  TntCustomEdit_SetSelText(Self, Value);
end;

function TSpTBXUnicodeEdit.GetPasswordChar: WideChar;
begin
  Result := TntCustomEdit_GetPasswordChar(Self, FPasswordChar);
end;

procedure TSpTBXUnicodeEdit.SetPasswordChar(const Value: WideChar);
begin
  TntCustomEdit_SetPasswordChar(Self, FPasswordChar, Value);
end;

function TSpTBXUnicodeEdit.GetText: WideString;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXUnicodeEdit.SetText(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;

function TSpTBXUnicodeEdit.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TSpTBXUnicodeEdit.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TSpTBXUnicodeEdit.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXUnicodeEdit.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;

function TSpTBXUnicodeEdit.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;

function TSpTBXUnicodeEdit.AddEditButton(AAlign: TAlign;
  AWidth: Integer): TSpTBXEditButton;
begin
  Result := TSpTBXEditButton.Create(Self);
  Result.Parent := Self;
  Result.FreeNotification(Self);
  if AAlign = alLeft then
    Result.Align := alLeft
  else
    Result.Align := alRight;
  if AWidth = -1 then
    Result.Width := GetSystemMetrics(SM_CXVSCROLL)
  else
    Result.Width := AWidth;
  UpdateEditRect;
end;

procedure TSpTBXUnicodeEdit.UpdateEditRect;
var
  I, X1, X2: Integer;
  B: TSpTBXEditButton;
begin
  if not HandleAllocated then Exit;

  X1 := 0;
  X2 := 0;

  for I := 0 to ControlCount - 1 do begin
    if Controls[I] is TSpTBXEditButton then begin
      B := Controls[I] as TSpTBXEditButton;
      if B.Visible then
        case B.Align of
          alLeft: X1 := X1 + B.Width;
          alRight: X2 := X2 + B.Width;
        end;
    end;
  end;

  if X1 > 0 then Inc(X1, 2);
  if X2 > 0 then Inc(X2, 2);

  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLong(X1, X2));
end;

procedure TSpTBXUnicodeEdit.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TSpTBXEditButton then
      TSpTBXEditButton(Controls[I]).Enabled := Enabled;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEdit }

constructor TSpTBXEdit.Create(AOwner: TComponent);
begin
  inherited;
  FThemeType := thtTBX;
  FHotTrack := True;
  AddThemeNotification(Self);
end;

destructor TSpTBXEdit.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TSpTBXEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;  
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FMouseInControl := True;  
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  FMouseInControl := False;
  if FHotTrack then InvalidateFrame;
end;

procedure TSpTBXEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl then begin
    FMouseInControl := True;
    if FHotTrack then InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl then begin
    FMouseInControl := False;
    if FHotTrack then InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXEdit.InvalidateFrame;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
end;

procedure TSpTBXEdit.SetThemeType(const Value: TSpTBXThemeType);
var
  I: Integer;
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TSpTBXEditButton then
        TSpTBXEditButton(Controls[I]).ThemeType := Value;
    InvalidateFrame;
  end;
end;

procedure TSpTBXEdit.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateFrame;
end;

procedure TSpTBXEdit.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  ACanvas: TCanvas;
  R: TRect;
  PaintDefault, HotTrackFrame: Boolean;
begin
  if SpXPThemeType(FThemeType) = thtNone then begin
    inherited;
    Exit;
  end;

  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ExcludeClipRect(ACanvas.Handle, 2, 2, R.Right - 2, R.Bottom - 2);

      if FThemeType = thtTBX then begin
        // SpDrawParentBackground uses ClientToScreen to get the control
        // coordinates on the parent's DC, the problem is we're not
        // painting the client area, but the NC area, that's why we need
        // Delta to be (-2, -2)
        SpDrawParentBackground(Self, ACanvas.Handle, R, -2, -2);
      end;

      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPrePaint, PaintDefault);
      if PaintDefault then begin
        if FHotTrack then
          HotTrackFrame := FMouseInControl or Focused
        else
          HotTrackFrame := False;
        SpDrawXPEditFrame(ACanvas, R, Enabled, HotTrackFrame, FThemeType);
      end;

      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPostPaint, PaintDefault);
    finally
      SelectClipRgn(ACanvas.Handle, 0);
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXButtonEdit }

constructor TSpTBXButtonEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FEditButton := AddEditButton(alRight, 20);
end;

destructor TSpTBXButtonEdit.Destroy;
begin
  FreeAndNil(FEditButton);
  inherited;
end;

procedure TSpTBXButtonEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditButton) and (Operation = opRemove) then
    FEditButton := nil;
end;

procedure TSpTBXButtonEdit.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if not (csLoading in ComponentState) then begin
    FEditButton.Caption := '...';
    Text := '';
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSpinEdit }

constructor TSpTBXSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  Alignment := taRightJustify;
  FSpinButton := TSpTBXSpinButton.Create(Self);
  FSpinButton.Parent := Self;
  FSpinButton.FreeNotification(Self);
  FSpinButton.OnUpClick := UpClick;
  FSpinButton.OnDownClick := DownClick;
  FSpinButton.Align := alRight;
  FSpinButton.Width := 15;
  UpdateEditRect;

  FDecimal := 2;
  FIncrement := 1;
  FMaxValue := 100;
  FValueSnap := True;
  FValueType := evtInteger;
  Text := '0';
end;

destructor TSpTBXSpinEdit.Destroy;
begin
  FreeAndNil(FSpinButton);
  inherited;
end;

procedure TSpTBXSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FSpinButton) and (Operation = opRemove) then
    FSpinButton := nil;
end;

procedure TSpTBXSpinEdit.InvalidateFrame;
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TSpTBXSpinButton then
      TSpTBXSpinButton(Controls[I]).Invalidate;
end;

procedure TSpTBXSpinEdit.DoValueChanged;
begin
  if Assigned(FOnValueChanged) then FOnValueChanged(Self);
end;

procedure TSpTBXSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP: ValueInc;
    VK_DOWN: ValueDec;
  end;
end;

procedure TSpTBXSpinEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    Key := #0;
    UpdateValueFromText;
  end;
end;

procedure TSpTBXSpinEdit.UpClick(Sender: TObject);
begin
  ValueInc;
end;

procedure TSpTBXSpinEdit.DownClick(Sender: TObject);
begin
  ValueDec;
end;

procedure TSpTBXSpinEdit.ValueInc;
var
  NewValue: Extended;
begin
  if FValueSnap then
    NewValue := Math.Floor(FValue / Increment + 1 + Increment * 0.0001) * Increment
  else
    NewValue := FValue + FIncrement;
  SetValue(NewValue);
end;

procedure TSpTBXSpinEdit.ValueDec;
var
  NewValue: Extended;
begin
  if FValueSnap then
    NewValue := Math.Ceil(FValue / Increment - 1 - Increment * 0.0001) * Increment
  else
    NewValue := FValue - FIncrement;
  SetValue(NewValue);
end;

function TSpTBXSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1;
end;

function TSpTBXSpinEdit.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 100;
end;

function TSpTBXSpinEdit.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

function TSpTBXSpinEdit.IsValueStored: Boolean;
begin
  Result := FValue <> 0;
end;

procedure TSpTBXSpinEdit.SetDecimal(const NewDecimal: TDecimal);
begin
  if NewDecimal <> FDecimal then begin
    FDecimal := NewDecimal;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEdit.SetMaxValue(const NewValue: Extended);
begin
  if NewValue <> FMaxValue then begin
    FMaxValue := NewValue;
    if FValue > NewValue then SetValue(NewValue);
  end;
end;

procedure TSpTBXSpinEdit.SetMinValue(const NewValue: Extended);
begin
  if NewValue <> FMinValue then begin
    FMinValue := NewValue;
    if FValue < NewValue then SetValue(NewValue);
  end;
end;

procedure TSpTBXSpinEdit.SetPrefix(const ValueString: WideString);
begin
  if FPrefix <> ValueString then begin
    FPrefix := ValueString;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEdit.SetPostfix(const ValueString: WideString);
begin
  if FPostfix <> ValueString then begin
    FPostfix := ValueString;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEdit.SetValue(const NewValue: Extended);
begin
  if (NewValue <> FValue) and (NewValue >= FMinValue) and (NewValue <= FMaxValue) then begin
    FValue := NewValue;
    DoValueChanged;
    UpdateTextFromValue;
  end;
end;

procedure TSpTBXSpinEdit.SetValueType(NewType: TSEValueType);
begin
  if NewType <> FValueType then begin
    FValueType := NewType;
    if NewType in [evtInteger, evtHex] then FIncrement := Max(Round(FIncrement), 1);
    UpdateTextFromValue;
  end;
end;

function TSpTBXSpinEdit.GetValueAsInteger: Integer;
begin
  Result := Round(Value);
end;

procedure TSpTBXSpinEdit.SetValueAsInteger(const NewValue: Integer);
begin
  Value := NewValue;
end;

procedure TSpTBXSpinEdit.UpdateTextFromValue;
var
  WS: WideString;
begin
  WS := '';
  case FValueType of
    evtInteger: WS := IntToStr(Round(FValue));
    evtFloat:   WS := FloatToStrF(FValue, ffFixed, 15, FDecimal);
    evtHex:     WS := IntToHex(Round(FValue), 1);
  end;
  Text := FPrefix + WS + FPostfix;
end;

procedure TSpTBXSpinEdit.UpdateValueFromText;
var
  WS: WideString;
  NewValue: Extended;
  I: Integer;
begin
  NewValue := FValue;
  WS := Text;

  // Remove the Prefix and Postfix from the text
  I := Pos(Prefix, WS);
  if I > 0 then
    Delete(WS, I, Length(Prefix));
  I := Pos(Postfix, WS);
  if I > 0 then
    Delete(WS, I, Length(Postfix));

  // Try to parse the text to get the value
  WS := Trim(WS);
  if Length(WS) > 0 then begin
    case FValueType of
      evtInteger: NewValue := StrToIntDef(WS, Round(NewValue));
      evtFloat:   NewValue := StrToFloatDef(WS, NewValue);
      evtHex:     NewValue := StrToIntDef(WS, Round(NewValue));
    end;
  end;

  SetValue(NewValue);
  UpdateTextFromValue;
end;

procedure TSpTBXSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  UpdateValueFromText;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXComboBox }

constructor TSpTBXComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  FThemeType := thtTBX;
  AddThemeNotification(Self);
  DoubleBuffered := True;
  FMouseTimer := nil;
end;

destructor TSpTBXComboBox.Destroy;
begin
  RemoveThemeNotification(Self);
  if Assigned(FMouseTimer) then begin
    FMouseTimer.Enabled := False;
    FreeAndNil(FMouseTimer);
  end;
  inherited;
end;

procedure TSpTBXComboBox.CloseUp;
begin
  inherited;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  FMouseInControl := True;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  FMouseInControl := False;
  InvalidateFrame;
end;

procedure TSpTBXComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  // [Bugfix] Delphi 2006 bug:
  // CM_MOUSEENTER and CM_MOUSELEAVE are fired everytime the mouse
  // enters the combobox internal edit control.
  // In D7 these messages were only fired when the mouse entered or leaved
  // the combobox, including the internal edit control.
  // We need to update the mouse state based on the mouse position using
  // a timer.
  if Message.LParam = 0 then begin
    if not FMouseInControl then begin
      FMouseInControl := True;
      if FHotTrack then
        InvalidateFrame;
      if not Assigned(FMouseTimer) then begin
        FMouseTimer := TTimer.Create(nil);
        FMouseTimer.Enabled := False;
        FMouseTimer.Interval := 125;
        FMouseTimer.OnTimer := MouseTimerHandler;
        FMouseTimer.Enabled := True;
      end;
    end;
  end;
end;

procedure TSpTBXComboBox.MouseTimerHandler(Sender: TObject);
var
  P: TPoint;
  R: TRect;
  InControl: Boolean;
begin
  if not DroppedDown and GetCursorPos(P) then begin
    GetWindowRect(Handle, R);
    InControl := PtInRect(R, P);
    if InControl <> FMouseInControl then begin
      FMouseInControl := InControl;
      if FHotTrack then InvalidateFrame;
    end;

    if not InControl then begin
      FMouseTimer.Enabled := False;
      FreeAndNil(FMouseTimer);
    end;
  end;
end;

procedure TSpTBXComboBox.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

function TSpTBXComboBox.GetMouseInDropDownButton: Boolean;
var
  P: TPoint;
  ButtonR: TRect;
  ButtonWidth: Integer;
begin
  Result := False;

  if GetCursorPos(P) then begin
    P := ScreenToClient(P);
    if Style = csSimple then
      ButtonWidth := 0
    else
      ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);
    ButtonR.Left := Width - ButtonWidth;
    ButtonR.Top := 0;
    ButtonR.Right := ButtonR.Left + ButtonWidth;
    ButtonR.Bottom := Height;

    Result := PtInRect(ButtonR, P);
  end;
end;

procedure TSpTBXComboBox.InvalidateFrame;
begin
  if HandleAllocated then
    Invalidate;
end;

procedure TSpTBXComboBox.UpdateDropDownButton;
var
  ButtonState: Boolean;
  T: TSpTBXThemeType;
begin
  if not DroppedDown then begin
    T := SpXPThemeType(FThemeType);
    if T = thtWindows then begin
      ButtonState := GetMouseInDropDownButton;
      if ButtonState <> FMouseInDropDownButton then
        InvalidateFrame;
      FMouseInDropDownButton := ButtonState;
    end;
  end;
end;

procedure TSpTBXComboBox.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    InvalidateFrame;
  end;
end;

procedure TSpTBXComboBox.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateFrame;
end;

procedure TSpTBXComboBox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  UpdateDropDownButton;
end;

procedure TSpTBXComboBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  // [Bugfix] Delphi 2006 bug:
  // Do nothing, fix Delphi 2005/2006 bug: http://qc.borland.com/wc/qcmain.aspx?d=13852
end;

procedure TSpTBXComboBox.WMPaint(var Message: TWMPaint);
var
  ACanvas: TControlCanvas;
  R: TRect;
  ButtonWidth: Integer;
  T: TSpTBXThemeType;
  PaintDefault, HotTrackFrame: Boolean;
begin
  inherited;

  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := Self;
    ACanvas.Lock; // lock the canvas to prevent flicker on mouse click
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    if Style = csSimple then
      ButtonWidth := 0
    else
      ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);

    T := SpXPThemeType(FThemeType);
    ExcludeClipRect(ACanvas.Handle, 2, 2, R.Right - 2 - ButtonWidth, R.Bottom - 2);
    try
      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPrePaint, PaintDefault);
      HotTrackFrame := False;
      if PaintDefault and (T <> thtNone) then begin
        case T of
          thtTBX:
            begin
              SpDrawParentBackground(Self, ACanvas.Handle, R);
              if FHotTrack then
                HotTrackFrame := FMouseInControl or Focused
              else
                HotTrackFrame := DroppedDown
            end;
          thtWindows:
            HotTrackFrame := GetMouseInDropDownButton;
        end;
        SpDrawXPComboBoxFrame(ACanvas, R, Enabled, HotTrackFrame, DroppedDown, FThemeType, ButtonWidth);
      end;

      PaintDefault := True;
      DoDrawBackground(ACanvas, R, pstPostPaint, PaintDefault);
    finally
      SelectClipRgn(ACanvas.Handle, 0);
    end;
  finally
    ACanvas.UnLock;
    ACanvas.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXListBox }

constructor TSpTBXListBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  FThemeType := thtTBX;
  AddThemeNotification(Self);
  Style := lbOwnerDrawFixed;
end;

procedure TSpTBXListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Force the ListBox to be owner draw
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

destructor TSpTBXListBox.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TSpTBXListBox.InvalidateBorders;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TSpTBXListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := Self = Message.Sender;
    if FChildFocused <> FHotTracking then begin
      FHotTracking := FChildFocused;
      InvalidateBorders;
    end;
  end;
end;

procedure TSpTBXListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then begin
    FHotTracking := True;
    InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then begin
    FHotTracking := False;
    InvalidateBorders;
  end;
end;

procedure TSpTBXListBox.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateBorders;
end;

procedure TSpTBXListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Integer;
begin
  // Draw the item text
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    // Add a margin to the rect
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    Canvas.Brush.Style := bsClear;
    Tnt_DrawTextW(Canvas.Handle, PWideChar(Items[Index]), Length(Items[Index]), Rect, Flags);
  end;
end;

procedure TSpTBXListBox.DrawItemBackground(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  // Draw the background and the focus
  if Assigned(OnDrawItemBackground) then
    OnDrawItemBackground(Self, Index, Rect, State)
  else
    SpDrawXPListItemBackground(Canvas, Rect, odSelected in State, False, odFocused in State, SpXPThemeType(ThemeType));
end;

procedure TSpTBXListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Lock;
    try
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if (Integer(itemID) >= 0) and (Integer(itemID) < Items.Count) then begin
        DrawItemBackground(itemID, rcItem, State);

        if not UseRightToLeftAlignment then
          rcItem.Left := rcItem.Left + 1
        else
          rcItem.Right := rcItem.Right - 1;
        DrawItem(itemID, rcItem, State);
      end
      else
        Canvas.FillRect(rcItem);
    finally
      Canvas.UnLock;
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TSpTBXListBox.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if SpXPThemeType(FThemeType) <> thtNone then
    SpDrawXPEditFrame(Self, FHotTracking, FThemeType);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCheckListBox }

constructor TSpTBXCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  FHotTrack := True;
  FThemeType := thtTBX;
  AddThemeNotification(Self);
  Style := lbOwnerDrawFixed;
end;

destructor TSpTBXCheckListBox.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TSpTBXCheckListBox.InvalidateBorders;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TSpTBXCheckListBox.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then begin
    FHotTrack := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.SetThemeType(const Value: TSpTBXThemeType);
begin
  if Value <> FThemeType then begin
    FThemeType := Value;
    InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if FHotTrack and Assigned(Message.Sender) then begin
    FChildFocused := Self = Message.Sender;
    if FChildFocused <> FHotTracking then begin
      FHotTracking := FChildFocused;
      InvalidateBorders;
    end;
  end;
end;

procedure TSpTBXCheckListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FHotTrack and not FHotTracking then begin
    FHotTracking := True;
    InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotTrack and FHotTracking and not FChildFocused then begin
    FHotTracking := False;
    InvalidateBorders;
  end;
end;

procedure TSpTBXCheckListBox.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateBorders;
end;

procedure TSpTBXCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Integer;
begin
  // Draw the item text
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    // Add a margin to the rect
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    Canvas.Brush.Style := bsClear;
    Tnt_DrawTextW(Canvas.Handle, PWideChar(Items[Index]), Length(Items[Index]), Rect, Flags);
  end;
end;

procedure TSpTBXCheckListBox.DrawItemBackground(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  T: TSpTBXThemeType;
  ACheckWidth: Integer;
  R: TRect;
begin
  // Draw the checkbox, background and focus
  if Assigned(OnDrawItemBackground) then
    OnDrawItemBackground(Self, Index, Rect, State)
  else
    if not Header[Index] then begin
      T := SpXPThemeType(ThemeType);

      // Draw the checkbox
      ACheckWidth := GetCheckWidth;
      if not UseRightToLeftAlignment then begin
        R.Right := Rect.Left;
        R.Left := R.Right - ACheckWidth;
      end
      else begin
        R.Left := Rect.Right;
        R.Right := R.Left + ACheckWidth;
      end;
      R.Top := Rect.Top + (Rect.Bottom - Rect.Top - ACheckWidth) div 2;
      R.Bottom := R.Top + ACheckWidth;
      InflateRect(R, -1, -1);

      Canvas.FillRect(R);
      SpDrawXPCheckBoxGlyph(Canvas, R, ItemEnabled[Index], Self.State[Index], False, False, False, T);

      // Draw the background and focus
      SpDrawXPListItemBackground(Canvas, Rect, odSelected in State, False, odFocused in State, T);
    end
    else begin
      Canvas.Font.Color := HeaderColor;
      Canvas.Brush.Color := HeaderBackgroundColor;
      Canvas.FillRect(Rect);
      if odFocused in State then
        SpDrawFocusRect(Canvas, Rect);
    end;
end;

procedure TSpTBXCheckListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  if Items.Count = 0 then Exit;

  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Lock;
    try
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      if (Integer(itemID) >= 0) and (Integer(itemID) < Items.Count) then begin
        // Exclude the checkbox area
        if not Header[itemID] then
          if not UseRightToLeftAlignment then
            rcItem.Left := rcItem.Left + GetCheckWidth
          else
            rcItem.Right := rcItem.Right - GetCheckWidth;

        DrawItemBackground(itemID, rcItem, State);

        if not UseRightToLeftAlignment then
          rcItem.Left := rcItem.Left + 1
        else
          rcItem.Right := rcItem.Right - 1;
        DrawItem(itemID, rcItem, State);
      end
      else
        Canvas.FillRect(rcItem);
    finally
      Canvas.UnLock;
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TSpTBXCheckListBox.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if SpXPThemeType(FThemeType) <> thtNone then
    SpDrawXPEditFrame(Self, FHotTracking, FThemeType);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditItem }

constructor TSpTBXEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FActiveEditItemViewer := nil;
  FEditorFontSettings := TFontSettings.Create;
  FFontSettings := TFontSettings.Create;
  TFontSettingsAccess(FEditorFontSettings).OnChange := FontSettingsChanged;
  TFontSettingsAccess(FFontSettings).OnChange := FontSettingsChanged;
end;

destructor TSpTBXEditItem.Destroy;
begin
  FActiveEditItemViewer := nil;
  FFontSettings.Free;
  FEditorFontSettings.Free;
  inherited;
end;

procedure TSpTBXEditItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TSpTBXEditItem.DoAcceptText(var NewText: string): Boolean;
var
  WS: WideString;
  Accept: Boolean;
begin
  // What a mess, we need to change the Text property when the edit control
  // is changed (Enter is being pressed), this is done by
  // TTBEditItemViewer.EditWndProc when using Ansi strings, there's no way
  // to override this.
  // The only solution I've found is to override DoAcceptText without calling
  // inherited, and setting the Result to allways return False to avoid automatic
  // Text property setting in TTBEditItemViewer.EditWndProc
  Result := False;
  Accept := False;

  // Get the Edit control text and change the Self.Text property
  if Assigned(FActiveEditItemViewer) then begin
    if ExtendedAccept and not Assigned(FActiveEditItemViewer.EditControl) then begin
      WS := FLastEditChange;
      if WS = '' then
        FLastEditChange := FText
      else
        Accept := (WS <> '') and DoAcceptTextW(WS);
    end
    else begin
      WS := FActiveEditItemViewer.EditControl.Text;
      Accept := DoAcceptTextW(WS);
    end;
    if Accept then
      SetTextEx(WS, tcrEditControl);
  end;
end;

function TSpTBXEditItem.DoAcceptTextW(var NewText: WideString): Boolean;
begin
  Result := True;
  if Assigned(FOnAcceptText) then FOnAcceptText(Self, NewText, Result);
end;

function TSpTBXEditItem.DoAutoComplete(var AText: WideString): Boolean;
begin
  Result := False;
end;

procedure TSpTBXEditItem.DoBeginEdit(Viewer: TTBEditItemViewer);
var
  IV: TSpTBXEditItemViewer;
begin
  if Viewer is TSpTBXEditItemViewer then begin
    IV := Viewer as TSpTBXEditItemViewer;
    with IV do begin
      EditControl.Alignment := Alignment;
      EditControl.PasswordChar := PasswordChar;
      EditControl.Text := FText;
      EditControl.SelectAll;
      EditControl.ReadOnly := ReadOnly;
      EditorFontSettings.Apply(EditControl.Font);
      FAutoCompleteCounter := 0;
      inherited;
      EditControl.OnChange := IV.EditChangeHandler;
      IV.OldWndProc := EditControl.WindowProc;
      EditControl.WindowProc := IV.NewEditWndProc;
    end;
  end;
end;

procedure TSpTBXEditItem.DoChange(const AText: WideString);
begin
  if Assigned(FOnChange) then FOnChange(Self, AText);
end;

procedure TSpTBXEditItem.DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawHint) then FOnDrawHint(Self, AHintBitmap, AHint, PaintDefault);
end;

procedure TSpTBXEditItem.DoTextChanged(Reason: Integer);
begin
  // Case Sensitive, fire the event when the text is changed
  if (Reason <> tcrEditControl) or (Text <> FLastEditChange) then
    DoChange(Text);
end;

procedure TSpTBXEditItem.DoTextChanging(const OldText: WideString;
  var NewText: WideString; Reason: Integer);
begin
  case CharCase of
    ecUpperCase: NewText := Tnt_WideUpperCase(NewText);
    ecLowerCase: NewText := Tnt_WideLowerCase(NewText);
  end;
end;

procedure TSpTBXEditItem.FontSettingsChanged(Sender: TObject);
begin
  Change(True);
end;

function TSpTBXEditItem.GetImageIndex: Integer;
begin
  Result := ImageIndex;
end;

function TSpTBXEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TSpTBXEditItemViewer;
end;

procedure TSpTBXEditItem.GetPopupPosition(ParentView: TTBView;
  PopupWindow: TTBPopupWindow; var PopupPositionRec: TTBPopupPositionRec);
var
  VT: Integer;
begin
  inherited;
  VT := GetWinViewType(PopupWindow);
  PopupPositionRec.PlaySound := not (VT and PVT_LISTBOX = PVT_LISTBOX);
end;

function TSpTBXEditItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TTBXPopupWindow;
end;

procedure TSpTBXEditItem.HandleEditChange(Edit: TSpTBXUnicodeEdit);
var
  S, S2: WideString;
begin
  if not FIsChanging then
  begin
    FIsChanging := True;
    try
      S := Edit.Text;
      S2 := S;
      if (Length(S) > 0) and (FAutoCompleteCounter > 0) and DoAutoComplete(S2) then
      begin
        Edit.Text := S2;
        Edit.SelStart := Length(S);
        Edit.SelLength := Length(S2) - Length(S);
        S := S2;
      end;
      if not SpSameText(S, FLastEditChange) then
      begin
        DoChange(S); // note, Edit.Text may be different from Self.Text
        FLastEditChange := S;
      end;
    finally
      FIsChanging := False;
    end;
  end;
end;

procedure TSpTBXEditItem.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Change(True);
  end;
end;

procedure TSpTBXEditItem.SetFontSettings(Value: TFontSettings);
begin
  FFontSettings.Assign(Value);
end;

procedure TSpTBXEditItem.SetPasswordChar(Value: WideChar);
begin
  if Value <> FPasswordChar then
  begin
    FPasswordChar := Value;
    Change(True);
  end;
end;

procedure TSpTBXEditItem.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
  Change(True);
end;

function TSpTBXEditItem.StartEditing(AView: TTBView): Boolean;
var
  V: TTBItemViewer;
  SaveText: WideString;
begin
  Result := False;
  V := AView.Find(Self);
  if V is TSpTBXEditItemViewer then
  begin
    SaveText := Text;
    TSpTBXEditItemViewer(V).DoExecute;
    // Case Sensitive, Result is true when the text is changed
    Result := Text <> SaveText;
  end;
end;

procedure TSpTBXEditItem.SetEditCaption(const Value: WideString);
begin
  if FEditCaption <> Value then begin
    FEditCaption := Value;
    inherited EditCaption := Value;
  end;
end;

procedure TSpTBXEditItem.SetHint(const Value: WideString);
begin
  if FHint <> Value then begin
    FHint := Value;
    inherited Hint := Value;
  end;
end;

procedure TSpTBXEditItem.SetText(Value: WideString);
begin
  SetTextEx(Value, tcrSetProperty);
end;

procedure TSpTBXEditItem.SetTextEx(Value: WideString; Reason: Integer);
begin
  DoTextChanging(FText, Value, Reason);
  // Case Sensitive, fire the event when the text is changed
  if FText <> Value then begin
    FText := Value;
    Change(False);
    DoTextChanged(Reason);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditItemViewer }

destructor TSpTBXEditItemViewer.Destroy;
begin
  Item.FActiveEditItemViewer := nil;
  inherited;
end;

procedure TSpTBXEditItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
var
  W, B: Integer;
  EditBoxHeight: Integer;
  EditCaptionSize: TSize;
begin
  if Self.Item is TSpTBXEditItem then with CurrentTheme do
  begin
    B := EditFrameWidth;

    AWidth := Item.EditWidth;
    if not IsToolbarStyle then
    begin
      EditCaptionSize := MeasureEditCaption;
      W := EditCaptionSize.CX;
      if W > 0 then Inc(W, MenuLeftCaptionMargin + MenuRightCaptionMargin + MenuImageTextSpace);
      Inc(AWidth, GetPopupMargin(Self) + MenuImageTextSpace + W + EditMenuRightIndent);
    end
    else
    begin
      EditCaptionSize.CX := 0;
      EditCaptionSize.CY := 0;
    end;

    EditBoxHeight := MeasureTextHeight + 1;
    Inc(EditBoxHeight, EditTextMarginVert * 2 + B * 2);
    AHeight := Max(EditBoxHeight, EditCaptionSize.CY);
    if not IsToolbarStyle then AHeight := AHeight;
    if EditHeightEven then AHeight := (AHeight + 1) and not $01
    else AHeight := AHeight or $01;
  end
  else inherited;
end;

procedure TSpTBXEditItemViewer.EditChangeHandler(Sender: TObject);
begin
  Item.HandleEditChange(Sender as TSpTBXUnicodeEdit);
end;

procedure TSpTBXEditItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo;
  const ItemInfo: TTBXItemInfo);
begin
  FillChar(EditInfo, SizeOf(EditInfo), 0);
  EditInfo.LeftBtnWidth := GetIndentBefore;
  EditInfo.RightBtnWidth := GetIndentAfter;
end;

function TSpTBXEditItemViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_SPINBUTTON = $34;
  ROLE_SYSTEM_COMBOBOX = $2E;
begin
  Result := inherited GetAccRole;
  // if Self is TSpTBXSpinEditItemViewer then Result := ROLE_SYSTEM_SPINBUTTON;
  if Self is TSpTBXDropDownItemViewer then Result := ROLE_SYSTEM_COMBOBOX;
end;

procedure TSpTBXEditItemViewer.GetItemInfo(out ItemInfo: TTBXItemInfo;
  IsHoverItem, IsPushed, UseMenuColor: Boolean);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
begin
  FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[IsToolbarStyle]
    or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := Item.Checked;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not View.MouseOverSelected then
      ItemInfo.HoverKind := hkKeyboardHover
    else
      if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  if not IsToolbarStyle then ItemInfo.PopupMargin := GetPopupMargin(Self);
end;

procedure TSpTBXEditItemViewer.GetEditRect(var R: TRect);
const
  TB2K_EDIT_BORDER = 3;
var
  W, B: Integer;
begin
  with CurrentTheme do begin
    R := BoundsRect;
    if not IsToolbarStyle then
    begin
      W := MeasureEditCaption.CX;
      if W > 0 then Inc(W, MenuLeftCaptionMargin + MenuRightCaptionMargin + MenuImageTextSpace);
      Inc(R.Left, GetPopupMargin(Self) + MenuImageTextSpace + W);
      Dec(R.Right, EditMenuRightIndent);
    end;

    B := EditFrameWidth - TB2K_EDIT_BORDER;
    InflateRect(R, -B - EditTextMarginHorz , -B - EditTextMarginVert);
    Inc(R.Left, GetIndentBefore);
    Dec(R.Right, GetIndentAfter);
  end;
end;

function TSpTBXEditItemViewer.GetHintText: Widestring;
var
  I: Integer;
begin
  // Get the short hint
  I := Pos('|', Item.Hint);
  if I = 0 then
    Result := Item.Hint
  else
    Result := Copy(Item.Hint, 1, I - 1);
  // Use the caption if there is no hint
  if (Result = '') and not(tboNoAutoHint in Item.EffectiveOptions) and
     (not(tbisSubmenu in Item.ItemStyle) or (tbisCombo in Item.ItemStyle) or
      not CaptionShown) then
  begin
    Result := SpStripAccelChars(SpStripTrailingPunctuation(Item.Caption));
  end;
  // Add shortcut text
  if (Result <> '') and Application.HintShortCuts and (Item.ShortCut <> scNone) then
    Result := Result + ' (' + ShortCutToText(Item.ShortCut) + ')';
end;

function TSpTBXEditItemViewer.GetIndentAfter: Integer;
begin
  Result := 0;
end;

function TSpTBXEditItemViewer.GetIndentBefore: Integer;
var
  ImgList: TCustomImageList;
begin
  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then Result := ImgList.Width + 2
    else Result := 0;
  end
  else Result := 0;
end;

function TSpTBXEditItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
const
  CharKeys = [VK_SPACE, $30..$5A, VK_NUMPAD0..VK_DIVIDE, $BA..$F5];
begin
  Item.FActiveEditItemViewer := Self;
  if Message.Msg = WM_KEYDOWN then
  begin
    if Message.WParam in CharKeys then Inc(Item.FAutoCompleteCounter)
  end
  else if Message.Msg = WM_KEYUP then
  begin
    if Message.WParam in CharKeys then Dec(Item.FAutoCompleteCounter);
  end;
  Result := False;
end;

procedure TSpTBXEditItemViewer.NewEditWndProc(var Message: TMessage);
begin
  if Assigned(OldWndProc) and not HandleEditMessage(Message) then OldWndProc(Message);
end;

procedure TSpTBXEditItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed,
  UseDisabledShadow: Boolean);
const
  FillColors: array [Boolean] of Integer = (COLOR_BTNFACE, COLOR_WINDOW);
  TextColors: array [Boolean] of Integer = (COLOR_GRAYTEXT, COLOR_WINDOWTEXT);
  Alignments: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  DC: HDC;
  S: WideString;
  R, R2: TRect;
  M, W: Integer;
  ItemInfo: TTBXItemInfo;
  EditInfo: TTBXEditInfo;
  ImgList: TCustomImageList;
  ImgIndex: Integer;
  Fnt, OldFnt: HFont;
  C, OldColor: TColor;
begin
  // We must paint the WideString Text property manually.
  DC := Canvas.Handle;
  GetItemInfo(ItemInfo, IsHoverItem, IsPushed, UseDisabledShadow);
  GetEditInfo(EditInfo, ItemInfo);
  R := ClientAreaRect;

  if not IsToolbarStyle then with CurrentTheme do
  begin
    S := Item.EditCaption;

    if Length(S) > 0 then
    begin
      { measure EditCaption }
      Fnt := Item.FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
      OldFnt := SelectObject(DC, Fnt);
      W := SpGetTextSize(DC, S, True).cx + MenuImageTextSpace + MenuLeftCaptionMargin + MenuRightCaptionMargin;
      SelectObject(DC, OldFnt);
    end
    else
    begin
      Fnt := 0; // to suppress compiler warning
      W := 0;
    end;

    M := GetPopupMargin(Self);
    if not EditMenuFullSelect then R.Right := M + W
    else Dec(R.Right, EditMenuRightIndent);
    PaintMenuItemFrame(Canvas, R, ItemInfo);
    Inc(R.Left, M + MenuImageTextSpace);
    R.Right := ClientAreaRect.Right - EditMenuRightIndent;

    if Length(S) > 0 then
    begin
      Inc(R.Left, MenuLeftCaptionMargin);
      if Item.FontSettings.Color = clNone then
        C := ColorToRGB(GetItemTextColor(ItemInfo))
      else
        C := ColorToRGB(Item.FontSettings.Color);
      OldFnt := SelectObject(DC, Fnt);
      OldColor := SetTextColor(DC, C);
      SpDrawXPText(Canvas, S, R, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      SetTextColor(DC, OldColor);
      W := SpGetTextSize(DC, S, True).cx;
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
      Inc(R.Left, W + MenuRightCaptionMargin + MenuImageTextSpace);
    end;
  end;

  CurrentTheme.PaintEditFrame(Canvas, R, ItemInfo, EditInfo);
  W := CurrentTheme.EditFrameWidth;
  InflateRect(R, -W - CurrentTheme.EditTextMarginHorz, -W - CurrentTheme.EditTextMarginVert);

  if ShowImage then
  begin
    ImgList := GetImageList;
    if ImgList <> nil then
    begin
      R2.Left := R.Left;
      R2.Right := R.Left + ImgList.Width;
      R2.Top := (R.Top + R.Bottom + 1 - ImgList.Height) div 2;
      R2.Bottom := R2.Top + ImgList.Height;
      ImgIndex := Item.GetImageIndex;
      if Item.Enabled then ImgList.Draw(Canvas, R.Left, R2.Top, ImgIndex)
      else DrawTBXImage(Canvas, R2, ImgList, ImgIndex, ISF_DISABLED);
    end;
  end;
  Inc(R.Left, EditInfo.LeftBtnWidth);
  Dec(R.Right, EditInfo.RightBtnWidth + 1);

  if Item.Text <> '' then
  begin
    DC := Canvas.Handle;
    S := Item.Text;
    if Item.PasswordChar <> #0 then S := StringOfChar(Item.PasswordChar, Length(S));
    Fnt := Item.EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, C);
    OldFnt := SelectObject(DC, Fnt);
    try
      SetBkMode(DC, TRANSPARENT);
      SetBkColor(DC, GetSysColor(FillColors[Item.Enabled]));
      if (Item.EditorFontSettings.Color = clNone) or not Item.Enabled then
        C := GetSysColor(TextColors[Item.Enabled])
      else
        C := ColorToRGB(Item.EditorFontSettings.Color);
      SetTextColor(DC, C);
      SpDrawXPText(Canvas, S, R, DT_SINGLELINE or DT_NOPREFIX or Alignments[Item.Alignment]);
    finally
      SelectObject(DC, OldFnt);
      DeleteObject(Fnt);
    end;
  end;
end;

function TSpTBXEditItemViewer.GetEditControlClass: TEditClass;
begin
  Result := TSpTBXUnicodeEdit;
end;

procedure TSpTBXEditItemViewer.GetEditHeight(const DC: HDC;
  out EditHeight, ExternalLeading: Integer);
var
  TextMetricA: TTextMetricA;
  TextMetricW: TTextMetricW;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
    Windows.GetTextMetricsA(DC, TextMetricA);
    EditHeight := TextMetricA.tmHeight;
    ExternalLeading := TextMetricA.tmExternalLeading;
  end
  else begin
    Windows.GetTextMetricsW(DC, TextMetricW);
    EditHeight := TextMetricW.tmHeight;
    ExternalLeading := TextMetricW.tmExternalLeading;
  end;
end;

function TSpTBXEditItemViewer.ShowImage: Boolean;
begin
  Result := Item.ShowImage;
end;

function TSpTBXEditItemViewer.IsToolbarSize: Boolean;
begin
  Result := inherited IsToolbarSize;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TSpTBXEditItemViewer.IsToolbarStyle: Boolean;
begin
  Result := inherited IsToolbarStyle;
  Result := Result or ((GetViewType(View) and PVT_TOOLBOX) = PVT_TOOLBOX);
end;

function TSpTBXEditItemViewer.MeasureEditCaption: TSize;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  S: WideString;
  H, I: Integer;
begin
  Result.cx := 0;
  Result.cy := 0;
  S := SpStripAccelChars(Item.EditCaption);
  if Length(S) > 0 then
  begin
    DummyColor := clWhite;
    DC := GetDC(0);
    Fnt := Item.FontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
    OldFnt := SelectObject(DC, Fnt);
    Result := SpGetTextSize(DC, S, False);
    GetEditHeight(DC, H, I);
    Inc(Result.cy, I);
    SelectObject(DC, OldFnt);
    DeleteObject(Fnt);
    ReleaseDC(0, DC);
  end;
end;

function TSpTBXEditItemViewer.MeasureTextHeight: Integer;
var
  DC: HDC;
  Fnt, OldFnt: HFont;
  DummyColor: TColor;
  I: Integer;
begin
  DummyColor := clWhite;
  DC := GetDC(0);
  Fnt := Item.EditorFontSettings.CreateTransformedFont(TTBViewAccess(View).GetFont.Handle, DummyColor);
  OldFnt := SelectObject(DC, Fnt);
  GetEditHeight(DC, Result, I);
  Inc(Result, I);
  SelectObject(DC, OldFnt);
  DeleteObject(Fnt);
  ReleaseDC(0, DC);
end;

function TSpTBXEditItemViewer.DoExecute: Boolean;
begin
  if Item is TSpTBXEditItem then
  begin
    Item.FLastEditChange := Item.Text;
    Result := inherited DoExecute;
    with TSpTBXEditItem(Item) do
    begin
      if FLastEditChange <> Text then DoChange(Text);
      FLastEditChange := Text;;
    end;
  end
  else Result := inherited DoExecute;
end;

function TSpTBXEditItemViewer.GetEditControl: TSpTBXUnicodeEdit;
begin
  Result := (inherited EditControl) as TSpTBXUnicodeEdit;
end;

function TSpTBXEditItemViewer.GetItem: TSpTBXEditItem;
begin
  Result := (inherited Item) as TSpTBXEditItem;
end;

procedure TSpTBXEditItemViewer.CMHintShow(var Message: TMessage);
// Handle the CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
var
  HintInfo: PHintInfo;
  WideHint: Widestring;
  R, TextR: TRect;
  PaintDefault: Boolean;
begin
  WideHint := GetHintText;

  // Prepare the HintInfo
  HintInfo := TCMHintShow(Message).HintInfo;
  HintInfo.HintStr := WideHint;
  HintInfo.CursorRect := BoundsRect;
  HintInfo.HintWindowClass := TBitmapHint;   // Custom HintWindow class
  HintInfo.HintData := SpStockHintBitmap;  // TApplication.ActivateHint will pass the data to the HintWindow
  HintInfo.HideTimeout := 60000; // 1 minute

  // Prepare the HintBitmap
  SpStockHintBitmap.Canvas.Font.Assign(Screen.HintFont);
  SpStockHintBitmap.Canvas.Font.Color := clInfoText;
  SpStockHintBitmap.Canvas.Pen.Color := clBlack;
  SpStockHintBitmap.Canvas.Brush.Color := clInfoBk;
  TextR := Rect(0, 0, 1, 1);
  SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX or DT_CALCRECT);
  SpStockHintBitmap.Width := TextR.Right + 8;
  SpStockHintBitmap.Height := TextR.Bottom + 4;

  // Draw the hint in the HintBitmap
  PaintDefault := True;
  Item.DoDrawHint(SpStockHintBitmap, WideHint, PaintDefault);
  if PaintDefault then begin
    R := Rect(0, 0, SpStockHintBitmap.Width, SpStockHintBitmap.Height);
    SpStockHintBitmap.Canvas.FillRect(R);
    OffsetRect(TextR, ((R.Right - TextR.Right) div 2) - 2, (R.Bottom - TextR.Bottom) div 2);
    SpDrawXPText(SpStockHintBitmap.Canvas, WideHint, TextR, DT_NOPREFIX);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomDropDownItem }

constructor TSpTBXCustomDropDownItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisCombo, tbisSubmenu, tbisSubitemsEditable] - [tbisDontSelectFirst];
  FAlwaysSelectFirst := True;
end;

function TSpTBXCustomDropDownItem.CreatePopup(const ParentView: TTBView;
  const ParentViewer: TTBItemViewer; const PositionAsSubmenu,
  SelectFirstItem, Customizing: Boolean; const APopupPoint: TPoint;
  const Alignment: TTBPopupAlignment): TTBPopupWindow;
var
  SelectFirst: Boolean;
begin
  if AlwaysSelectFirst then SelectFirst := True
  else SelectFirst := SelectFirstItem;
  Result := inherited CreatePopup(ParentView, ParentViewer, PositionAsSubmenu,
    SelectFirst, Customizing, APopupPoint, Alignment);
end;

function TSpTBXCustomDropDownItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TSpTBXDropDownItemViewer;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDropDownItemViewer }

procedure TSpTBXDropDownItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
begin
  if not TSpTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

procedure TSpTBXDropDownItemViewer.GetEditInfo(out EditInfo: TTBXEditInfo; const ItemInfo: TTBXItemInfo);
const
  CDisabled: array [Boolean] of Integer = (EBDS_DISABLED, 0);
  CHot: array [Boolean] of Integer = (0, EBDS_HOT);
  CPressed: array [Boolean] of Integer = (0, EBDS_PRESSED);
begin
  inherited GetEditInfo(EditInfo, ItemInfo);
  EditInfo.RightBtnInfo.ButtonType := EBT_DROPDOWN;
  EditInfo.RightBtnInfo.ButtonState := CDisabled[ItemInfo.Enabled] or
    CHot[ItemInfo.HoverKind = hkMouseHover] or CPressed[ItemInfo.Pushed];
end;

function TSpTBXDropDownItemViewer.GetIndentAfter: Integer;
begin
  if IsToolbarStyle then Result := CurrentTheme.EditBtnWidth
  else Result := GetSystemMetrics(SM_CXMENUCHECK) + 2;
end;

function TSpTBXDropDownItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
var
  WasAlreadyOpen: Boolean;
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if TWMKeyDown(Message).CharCode = VK_F4 then
    begin
      WasAlreadyOpen := (View.OpenViewer = Self);
      if WasAlreadyOpen then
        View.CloseChildPopups
      else
        View.OpenChildPopup(True);
      Result := True;
      Exit;
    end;
  end;

  Result := inherited HandleEditMessage(Message);
end;

function TSpTBXDropDownItemViewer.IsPtInButtonPart(X, Y: Integer): Boolean;
begin
  Result := not (tbisSubmenu in TSpTBXCustomDropDownItem(Item).ItemStyle);
  if TSpTBXCustomDropDownItem(Item).DropDownList then Result := False
  else if (tbisCombo in TSpTBXCustomDropDownItem(Item).ItemStyle) then
    Result := X < (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
end;

procedure TSpTBXDropDownItemViewer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not TSpTBXCustomDropDownItem(Item).DropDownList then inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXComboBoxItem }

procedure TSpTBXComboBoxItem.AdjustImageIndex(const AText: WideString;
  AIndex: Integer; var ImageIndex: Integer);
begin
  if Assigned(FOnAdjustImageIndex) then FOnAdjustImageIndex(Self, AText, AIndex, ImageIndex);
end;

procedure TSpTBXComboBoxItem.AdjustImageIndexHandler(Sender: TTBXCustomList;
  AItemIndex: Integer; var ImageIndex: Integer);
begin
  AdjustImageIndex(FList.Strings[AItemIndex], AItemIndex, ImageIndex);
end;

constructor TSpTBXComboBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSubItemsEditable];
  FAutoComplete := True;
  FList := GetStringListClass.Create(Self);
  FList.OnChange := ListChangeHandler;
  FList.OnClick := ListClickHandler;
  FList.OnAdjustImageIndex := AdjustImageIndexHandler;
  MinListWidth := 64;

  if not (csDesigning in ComponentState) then Add(FList);
end;

function TSpTBXComboBoxItem.DoAutoComplete(var AText: WideString): Boolean;
var
  I: Integer;
  S, R: WideString;
  TemplateL, MinL, L: Integer;
begin
  Result := False;
  if (AutoComplete) and (Length(AText) > 0) then
  begin
    { choose the shortest matching WideString from items }
    TemplateL := Length(AText);
    MinL := MaxInt;
    SetLength(R, 0);
    for I := 0 to FList.Strings.Count - 1 do
    begin
      S := FList.Strings[I];
      L := Length(S);
      if (L >= TemplateL) and (L < MinL) and SpStartsTextW(AText, S) then
      begin
        R := S;
        MinL := L;
        if MinL = TemplateL then Break;
      end;
    end;
    Result := Length(R) > 0;
    if Result then AText := AText + Copy(R, TemplateL + 1, MaxInt);
  end;
end;

procedure TSpTBXComboBoxItem.DoListChange;
begin
  { Update text in edit item. This will call OnChange automatically }
  if (FList.ItemIndex >= 0) and (FList.ItemIndex < FList.Strings.Count) then
  begin
    IsChanging := True;
    try
      if not SpSameText(Text, FList.Strings[Flist.ItemIndex]) then
        SetTextEx(FList.Strings[FList.ItemIndex], tcrList);
    finally
      IsChanging := False;
    end;
  end;
end;

procedure TSpTBXComboBoxItem.DoListClick;
begin
  if Assigned(FOnItemClick) then FOnItemClick(Self);
end;

procedure TSpTBXComboBoxItem.DoPopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  inherited;
  FList.ItemIndex := FList.Strings.IndexOf(Text);
end;

function TSpTBXComboBoxItem.GetImageIndex: Integer;
begin
  if not CacheValid then
  begin
    CachedImageIndex := ImageIndex;
    if ItemIndex >= 0 then CachedImageIndex := ItemIndex;
    AdjustImageIndex(Text, -1, CachedImageIndex);
    CacheValid := True;
  end;
  Result := CachedImageIndex;
end;

function TSpTBXComboBoxItem.GetItemIndex: Integer;
begin
  Result := FList.ItemIndex;
end;

function TSpTBXComboBoxItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not (tboUseEditWhenVertical in EditOptions) and (AView.Orientation = tbvoVertical) then
    Result := TTBXItemViewer
  else
    Result := TSpTBXComboBoxItemViewer;
end;

function TSpTBXComboBoxItem.GetMaxVisibleItems: Integer;
begin
  Result := FList.MaxVisibleItems;
end;

function TSpTBXComboBoxItem.GetMaxWidth: Integer;
begin
  Result := FList.MaxWidth;
end;

function TSpTBXComboBoxItem.GetMinWidth: Integer;
begin
  Result := FList.MinWidth;
end;

function TSpTBXComboBoxItem.GetOnClearItem: TTBXLPaintEvent;
begin
  Result := FList.OnClearItem;
end;

function TSpTBXComboBoxItem.GetOnDrawItem: TTBXLPaintEvent;
begin
  Result := FList.OnDrawItem;
end;

function TSpTBXComboBoxItem.GetOnMeasureHeight: TTBXLMeasureHeight;
begin
  Result := FList.OnMeasureHeight;
end;

function TSpTBXComboBoxItem.GetOnMeasureWidth: TTBXLMeasureWidth;
begin
  Result := FList.OnMeasureWidth;
end;

function TSpTBXComboBoxItem.GetShowListImages: Boolean;
begin
  Result := FList.ShowImages;
end;

function TSpTBXComboBoxItem.GetStringListClass: TSpTBXStringListClass;
begin
  Result := TSpTBXStringList;
end;

function TSpTBXComboBoxItem.GetStrings: TTntStrings;
begin
  Result := FList.Strings;
end;

procedure TSpTBXComboBoxItem.HandleEditChange(Edit: TSpTBXUnicodeEdit);
begin
  CacheValid := False;
  inherited;
end;

procedure TSpTBXComboBoxItem.ListChangeHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListChange;
end;

procedure TSpTBXComboBoxItem.ListClickHandler(Sender: TObject);
begin
  CacheValid := False;
  DoListClick;
end;

procedure TSpTBXComboBoxItem.Loaded;
begin
  inherited;
  if FList.Strings.IndexOf(Text) >= 0 then
  begin
    IsChanging := True;
    try
      FList.ItemIndex := FList.Strings.IndexOf(Text);
    finally
      IsChanging := False;
    end;
  end;
end;

procedure TSpTBXComboBoxItem.SetItemIndex(Value: Integer);
begin
  FList.ItemIndex := Value;
end;

procedure TSpTBXComboBoxItem.SetMaxVisibleItems(Value: Integer);
begin
  FList.MaxVisibleItems := Value;
end;

procedure TSpTBXComboBoxItem.SetMaxWidth(Value: Integer);
begin
  FList.MaxWidth := Value;
end;

procedure TSpTBXComboBoxItem.SetMinWidth(Value: Integer);
begin
  FList.MinWidth := Value;
end;

procedure TSpTBXComboBoxItem.SetOnClearItem(Value: TTBXLPaintEvent);
begin
  FList.OnClearItem := Value;
end;

procedure TSpTBXComboBoxItem.SetOnDrawItem(Value: TTBXLPaintEvent);
begin
  FList.OnDrawItem := Value;
end;

procedure TSpTBXComboBoxItem.SetOnMeasureHeight(Value: TTBXLMeasureHeight);
begin
  FList.OnMeasureHeight := Value;
end;

procedure TSpTBXComboBoxItem.SetOnMeasureWidth(Value: TTBXLMeasureWidth);
begin
  FList.OnMeasureWidth := Value;
end;

procedure TSpTBXComboBoxItem.SetShowListImages(Value: Boolean);
begin
  FList.ShowImages := Value;
end;

procedure TSpTBXComboBoxItem.SetStrings(Value: TTntStrings);
begin
  FList.Strings := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXComboBoxItemViewer }

function TSpTBXComboBoxItemViewer.HandleEditMessage(var Message: TMessage): Boolean;
begin
  if (Message.Msg = WM_KEYDOWN) then with TSpTBXComboBoxItem(Item) do
  begin
    case Message.wParam of
      VK_UP:
        begin
          if ItemIndex > 0 then ItemIndex := ItemIndex - 1;
          EditControl.Text := Text;
          EditControl.SelectAll;
          Result := True;
        end;

      VK_DOWN:
        begin
          if ItemIndex < Strings.Count - 1 then ItemIndex := ItemIndex + 1;
          EditControl.Text := Text;
          EditControl.SelectAll;
          Result := True;
        end;
    else
      Result := inherited HandleEditMessage(Message);
    end
  end
  else Result := inherited HandleEditMessage(Message);
end;

end.
