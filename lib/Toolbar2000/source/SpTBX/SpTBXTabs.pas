unit SpTBXTabs;

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

Development Notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.
  - When an item is hidden the ItemViewer.BoundsRect property is invalid.
  - tbicDeleting item notification is fired after the ItemViewer of the
    Item is destroyed by TTBView, but the Items array still has the Item.

TODO:
  - Make the NC area of the tabset doublebuffered.

History:
8 February 2007 - version 1.8.3
  - Added accel char handling to TSpTBXTabSet and TSpTBXTabControl

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - Fixed incorrect Tab painting when the Default theme was used
    and the ThemeType was tttFlat.

27 August 2006 - version 1.8
  - Fixed incorrect OnActiveTabChanging handling when
    ActiveTabIndex is changed on this event, thanks to
    Serg Chechenin for reporting this.

15 June 2006 - version 1.7
  - Fixed incorrect Tab painting when the default theme was used,
    the captions were painted in a pushed state, thanks to
    Mikalai Arapau for reporting this.
  - Fixed incorrect Tab aligning when Autofit was used and the
    tab control was parented by a Frame, thanks to
    Henk van Kampen for reporting this.

12 April 2006 - version 1.5
  - Fixed incorrect Tab painting when TabAutofit was true.

27 February 2006 - version 1.4
  - Fixed flicker when reordering TSpTBXTabSet and TSpTBXTabControl
    tabs, thanks to Alexey Naumov for reporting this.
  - Fixed incorrect context menu handling in TSpTBXTabSet and
    TSpTBXTabControl, thanks to Boris Yankov for reporting this.
  - Added OnActiveTabReorder event to TSpTBXTabSet and TSpTBXTabControl.

10 February 2006 - version 1.3
  - Added TabDragReorder property to TSpTBXTabSet and TSpTBXTabControl,
    when this property is true it allows tabs reordering with
    drag and drop.
  - Added TabAutofit and TabAutofitMaxSize properties to TSpTBXTabSet
    and TSpTBXTabControl. When TabAutofit is true the tabs are resized
    to fit the tabset.

28 December 2005 - version 1.2
  - Fixed incorrect TSpTBXTabControl background painting on some themes.
  - Fixed incorrect OnActiveTabChange call when the component is being
    loaded, thanks to Leroy Casterline for reporting this.
  - Fixed incorrect tab scrolling when an item is deleted, thanks to
    Daniel Rikowski for reporting this.

18 October 2005 - version 1.1
  - Fixed incorrect TSpTBXTabItem painting on some themes.
  - Added Margins property to TSpTBXPageControl.

18 August 2005 - version 1.0
  - Added TabVisible property to TSpTBXTabSet and TSpTBXPageControl.
  - Added OnActiveTabChanging event to TSpTBXTabSet and TSpTBXPageControl.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.
  - Fixed AV in TSpTBXTabSet and TSpTBXPageControl when used in a Frame
    with TabPosition setted to dpBottom, thanks to Cyril Velter for the fix.

20 May 2005 - version 0.8
  - Fixed tab scrolling of TSpTBXTabSet and TSpTBXPageControl, the tabs
    were not allowed to scroll when one single tab was visible, thanks
    to Anders Olsson for the fix.
  - Added MakeVisible method to the TSpTBXTabSet and TSpTBXPageControl,
    it scrolls the tabset, if necessary, to ensure a Tab is in view.

16 February 2005 - version 0.7
  - No changes.

23 December 2004 - version 0.6
  - Fixed TSpTBXTabControl reordering bug.
  - Changed the order of the TSpTBXTabThemeType enumerated type.
  - Added ActivePage property to the TSpTBXTabControl.
  - Added Caption, ImageIndex, TabVisible and PopupMenu properties
    to TSpTBXTabControl.

30 August 2004 - version 0.5
  - No changes.

21 July 2004 - version 0.4
  - Fixed TSpTBXTabControl design time bug, it was allowing to drop
    components when ActiveTabIndex = -1
  - Fixed TSpTBXTabSet and TSpTBXTabControl design time bug, the
    hidden items were not streamed to the DFM.
  - Changed TSpTBXTabControl.OnTabClick event for OnActiveTabChange.
  - Added GetPage method to TSpTBXTabControl to get the TSpTBXTabSheet
    linked to a TSpTBXTabItem.

12 July 2004 - version 0.3.1
  - Fixed nasty AV when setting TBXSwitcher.EnableXPStyles to false,
    thanks to Alfred for reporting this.
    Note: TBXThemeManager unloads the theme library and the theme
    parts when some conditions are met, we must handle extra theme
    parts outside TBXThemeManager space.
  - Fixed incorrect TSpTBXTabSet.ActiveTabIndex property update at
    design time.
  - Fixed incorrect TSpTBXTabSet painting on some TBX themes, thanks
    to Tim for reporting this.

9 July 2004 - version 0.3
  - Fixed design time AVs when moving or deleting TabSheets.
  - Published ThemeType and TabPosition properties for TSpTBXTabItem.
  - New component added, TSpTBXTabSet, a fully customizable TabSet
    with unicode and toolbar items support.

28 June 2004 - version 0.2
  - No changes.

22 June 2004 - version 0.1
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, TB2Common, TBX, TBXDkPanels,
  TBXThemes, SpTBXItem, SpTBXControls;

const
  C_SpTBXTabGroupIndex = 7777;
  WM_INVALIDATETABBACKGROUND = WM_USER + 7777;

type
  TSpTBXTabThemeType = (
    tttNone,         // No themes, no hovered tabs
    tttWindows,      // Windows XP theme if available, otherwise don't use themes
    tttTBX,          // TBX themes for active and hovered tabs
    tttFlat          // Flat active tab and TBX themes on hovered tabs
  );

  TSpTBXTabEdge = (
    tedNone,         // No edge needed
    tedLeft,         // Left edge of the tab
    tedRight         // Right edge of the tab
  );

  TSpTBXTabPosition = (
    ttpTop,          // Top aligned tabset
    ttpBottom        // Bottom aligned tabset
  );

  TSpTBXTabChangeEvent = procedure(Sender: TObject; TabIndex: Integer) of object;
  TSpTBXTabChangingEvent = procedure(Sender: TObject; TabIndex, NewTabIndex: Integer; var Allow: Boolean) of object;

  TSpTBXTabToolbar = class;
  TSpTBXCustomTabSet = class;
  TSpTBXCustomTabControl = class;
  TSpTBXTabSheet = class;

  TSpTBXTabItemDragObject = class(TSpTBXCustomDragObject)
  public
    constructor Create(ASourceControl: TControl; AItem: TTBCustomItem); override;
  end;

  { TSpTBXTabItem }

  TSpTBXTabItem = class(TSpTBXCustomItem)
  private
    FTabPosition: TSpTBXTabPosition;
    FThemeType: TSpTBXTabThemeType;
    function GetTabColor: TColor;
    function GetTabPosition: TSpTBXTabPosition;
    function GetThemeType: TSpTBXTabThemeType;
    procedure SetTabPosition(const Value: TSpTBXTabPosition);
    procedure SetThemeType(const Value: TSpTBXTabThemeType);
  protected
    function DialogChar(CharCode: Word): Boolean; override;
    procedure DrawBottomBorder(ACanvas: TCanvas; ARect: TRect);
    procedure DoDrawAdjustFont(AFont: TFont; StateFlags: Integer); override;
    procedure DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ClientAreaRect: TRect;
      var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
      IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawImage(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
      const PaintStage: TSpTBXPaintStage; AImageList: TCustomImageList;
      var AImageIndex: Integer; var ARect: TRect;
      var PaintDefault: Boolean); override;
    procedure DoDrawTab(ACanvas: TCanvas; ARect: TRect;
      AEnabled, AChecked, AHoverItem: Boolean; Position: TSpTBXTabPosition;
      ASeparator: Boolean = False; AEdge: TSpTBXTabEdge = tedNone); virtual;
    function GetTabToolbar(out TabToolbar: TSpTBXTabToolbar): Boolean;
    procedure ToggleControl; override;
    property Control;  // TabSheet
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    function GetNextTabItemViewer(GoForward: Boolean; Inmediate: Boolean = True): TTBItemViewer;
    function GetNextTab(GoForward: Boolean; Inmediate: Boolean = True): TSpTBXTabItem;
    function IsFirstTab: Boolean;
    function IsLastTab: Boolean;
  published
    property Action;
    property Checked;
    // Hide DisplayMode
    // property DisplayMode default nbdmImageAndText;
    property Enabled;
    property FontSettings;
    // Hide GroupIndex, all the TabItems must have the same GroupIndex
    // property GroupIndex;
    property HelpContext;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property ShortCut;
    // Hide Stretch, all the TabItems must have Stretch setted to True
    // property Stretch default True;
    property Visible;
    property OnAdjustFont;
    property OnClick;
    property OnSelect;
    // TSpTBXCustomItem properties
    property Alignment;
    property CustomWidth;
    property CustomHeight;
    property Margins default 4;
    property Wrapping default twEndEllipsis;
    property OnDrawImage;
    property OnDrawItem;
    property OnDrawHint;
    property OnDrawCaption;
    // TSpTBXTabItem properties
    property TabColor: TColor read GetTabColor;
    property TabPosition: TSpTBXTabPosition read GetTabPosition write SetTabPosition default ttpTop;
    property ThemeType: TSpTBXTabThemeType read GetThemeType write SetThemeType default tttWindows;
  end;

  { TSpTBXTabToolbar }

  TSpTBXTabToolbar = class(TSpTBXToolbar)
  private
    FThemeType: TSpTBXTabThemeType;
    FUpdatingHidden: Boolean;
    FTabAutofit: Boolean;
    FTabAutofitMaxSize: Integer;
    FTabColor: TColor;
    FTabPosition: TSpTBXTabPosition;
    FTabDragReorder: Boolean;
    function GetActiveTab: TSpTBXTabItem;
    procedure SetActiveTabIndex(Value: integer);
    procedure SetThemeType(const Value: TSpTBXTabThemeType);
    procedure SetTabAutofit(const Value: Boolean);
    procedure SetTabAutofitMaxSize(const Value: Integer);
    procedure SetTabColor(const Value: TColor);
    procedure SetTabPosition(const Value: TSpTBXTabPosition);
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    FHiddenTabs: TSpTBXItemCacheCollection;
    FActiveTabIndex: Integer;
    FOwnerTabControl: TSpTBXCustomTabSet;
    procedure Autofit;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); override;
    procedure RightAlignItems; override;

    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateActiveTab;
    procedure InvalidateNC;
    procedure MakeVisible(ATab: TSpTBXTabItem);
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure ScrollState(out CanScrollToLeft, CanScrollToRight: Boolean);
  published
    property ActiveTab: TSpTBXTabItem read GetActiveTab;
    property ActiveTabIndex: Integer read FActiveTabIndex write SetActiveTabIndex;
    property TabAutofit: Boolean read FTabAutofit write SetTabAutofit default False;
    property TabAutofitMaxSize: Integer read FTabAutofitMaxSize write SetTabAutofitMaxSize default 200;
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
    property TabPosition: TSpTBXTabPosition read FTabPosition write SetTabPosition default ttpTop;
    property TabDragReorder: Boolean read FTabDragReorder write FTabDragReorder default False;
    property ThemeType: TSpTBXTabThemeType read FThemeType write SetThemeType default tttWindows;
  end;

  { TSpTBXTabSheet }

  TSpTBXTabSheet = class(TCustomControl)
  private
    FTabControl: TSpTBXCustomTabControl;
    FItem: TSpTBXTabItem;
    FItemName: String;
    FPrevFocused: TWincontrol;
    procedure ReadItemName(Reader: TReader);
    procedure WriteItemName(Writer: TWriter);
    function GetCaption: WideString;
    function GetTabVisible: Boolean;
    procedure SetCaption(const Value: WideString);
    procedure SetTabVisible(const Value: Boolean);
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ReadState(Reader: TReader); override;
    procedure VisibleChanging; override;
    property Align default alClient;
    property PrevFocused: TWincontrol read FPrevFocused;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Item: TSpTBXTabItem read FItem write FItem;
    property TabControl: TSpTBXCustomTabControl read FTabControl write FTabControl;
  published
    property PopupMenu;
    property Caption: WideString read GetCaption write SetCaption;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
  end;

  { TSpTBXTabControl }

  TSpTBXCustomTabSet = class(TSpTBXCompoundItemsControl)
  private
    FItemMoveCount: Integer;
    FItemMoved: TSpTBXTabItem;
    FTabVisible: Boolean;
    FLoadingActiveIndex: Integer;
    FUpdatingIndex: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnActiveTabChange: TSpTBXTabChangeEvent;
    FOnActiveTabChanging: TSpTBXTabChangingEvent;
    FOnActiveTabReorder: TSpTBXTabChangeEvent;
    FOnActiveTabReordering: TSpTBXTabChangingEvent;
    procedure ReadHiddenItems(Reader: TReader);
    procedure WriteHiddenItems(Writer: TWriter);
    function GetActiveTabIndex: Integer;
    procedure SetActiveTabIndex(Value: Integer);
    function GetThemeType: TSpTBXTabThemeType;
    procedure SetThemeType(const Value: TSpTBXTabThemeType);
    function GetTabAutofit: Boolean;
    procedure SetTabAutofit(const Value: Boolean);
    function GetTabAutofitMaxSize: Integer;
    procedure SetTabAutofitMaxSize(const Value: Integer);
    function GetTabBackgroundColor: TColor;
    procedure SetTabBackgroundColor(const Value: TColor);
    function GetTabDragReorder: Boolean;
    procedure SetTabDragReorder(const Value: Boolean);
    function GetTabPosition: TSpTBXTabPosition;
    procedure SetTabPosition(const Value: TSpTBXTabPosition);
    procedure SetTabVisible(const Value: Boolean);
    function GetTabToolbar: TSpTBXTabToolbar;
    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure TBMGetEffectiveColor(var Message: TMessage); message TBM_GETEFFECTIVECOLOR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMInvalidateTabBackground(var Message: TMessage); message WM_INVALIDATETABBACKGROUND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FBackground: TBitmap;
    // Painting
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    function GetFullRepaint: Boolean; virtual;
    procedure InvalidateDockBackground(Resizing: Boolean = False); override;

    // Tabs
    function CanActiveTabChange(const TabIndex, NewTabIndex: Integer): Boolean; virtual;
    procedure DoActiveTabChange(const TabIndex: Integer); virtual;
    function CanActiveTabReorder(const TabIndex, NewTabIndex: Integer): Boolean; virtual;
    procedure DoActiveTabReorder(const TabIndex: Integer); virtual;
    procedure ItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); virtual;  // Items change notification
    procedure TabInserted(Item: TSpTBXTabItem); virtual;
    procedure TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean = True); virtual;

    // Component
    procedure DefineProperties(Filer: TFiler); override;
    function GetToolbarClass: TSpTBXToolbarClass; override;

    procedure Loaded; override;

    property Color default clNone;
    property ParentColor default False;
    property ActiveTabIndex: Integer read GetActiveTabIndex write SetActiveTabIndex;
    property TabAutofit: Boolean read GetTabAutofit write SetTabAutofit default False;
    property TabAutofitMaxSize: Integer read GetTabAutofitMaxSize write SetTabAutofitMaxSize default 200;
    property TabBackgroundColor: TColor read GetTabBackgroundColor write SetTabBackgroundColor default clNone;
    property TabDragReorder: Boolean read GetTabDragReorder write SetTabDragReorder default False;
    property TabPosition: TSpTBXTabPosition read GetTabPosition write SetTabPosition default ttpTop;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property ThemeType: TSpTBXTabThemeType read GetThemeType write SetThemeType default tttWindows;
    property OnActiveTabChange: TSpTBXTabChangeEvent read FOnActiveTabChange write FOnActiveTabChange;
    property OnActiveTabChanging: TSpTBXTabChangingEvent read FOnActiveTabChanging write FOnActiveTabChanging;
    property OnActiveTabReorder: TSpTBXTabChangeEvent read FOnActiveTabReorder write FOnActiveTabReorder;
    property OnActiveTabReordering: TSpTBXTabChangingEvent read FOnActiveTabReordering write FOnActiveTabReordering;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(ACaption: WideString): TSpTBXTabItem;
    function Insert(NewIndex: Integer; ACaption: WideString): TSpTBXTabItem;
    function DrawBackground(DC: HDC; ARect: TRect): Boolean;
    function GetTabSetHeight: Integer;
    procedure MakeVisible(ATab: TSpTBXTabItem);
    procedure ScrollLeft;
    procedure ScrollRight;
    procedure ScrollState(out Left, Right: Boolean);
    procedure TabClick(const ATab: TSpTBXTabItem); virtual;
    property Canvas;
    property Toolbar: TSpTBXTabToolbar read GetTabToolbar;
  end;

  TSpTBXTabSet = class(TSpTBXCustomTabSet)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomTabSet properties
    property ActiveTabIndex;
    property Images;
    property TabAutofit;
    property TabAutofitMaxSize;
    property TabBackgroundColor;
    property TabDragReorder;
    property TabPosition;
    property TabVisible;
    property ThemeType;
    property OnActiveTabChange;
    property OnActiveTabChanging;
    property OnActiveTabReorder;
    property OnActiveTabReordering;    
    property OnDrawBackground;
  end;

  { TSpTBXTabControl }

  TSpTBXCustomTabControl = class(TSpTBXCustomTabSet)
  private
    FEmptyTabSheet: TSpTBXTabSheet;
    FMargins: TTBXControlMargins;
    procedure MarginsChangeHandler(Sender: TObject);
    procedure SetMargins(Value: TTBXControlMargins);
    function GetActivePage: TSpTBXTabSheet;
    function GetPages(Index: Integer): TSpTBXTabSheet;
    function GetPagesCount: Integer;
    procedure SetActivePage(const Value: TSpTBXTabSheet);
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    FPages: TList;
    procedure DoActiveTabChange(const ItemIndex: Integer); override;
    function GetFullRepaint: Boolean; override;
    procedure TabInserted(Item: TSpTBXTabItem); override;
    procedure TabDeleting(Item: TSpTBXTabItem; FreeTabSheet: Boolean = True); override;
    property Margins: TTBXControlMargins read FMargins write SetMargins;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPage(Item: TSpTBXTabItem): TSpTBXTabSheet;
    property ActivePage: TSpTBXTabSheet read GetActivePage write SetActivePage;
    property Pages[Index: Integer]: TSpTBXTabSheet read GetPages;
    property PagesCount: Integer read GetPagesCount;
  end;

  TSpTBXTabControl = class(TSpTBXCustomTabControl)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // TSpTBXCustomTabControl properties
    property ActiveTabIndex;
    property Images;
    property Margins;
    property TabAutofit;
    property TabAutofitMaxSize;
    property TabBackgroundColor;
    property TabDragReorder;
    property TabPosition;
    property TabVisible;
    property ThemeType;
    property OnActiveTabChange;
    property OnActiveTabChanging;
    property OnActiveTabReorder;
    property OnActiveTabReordering;    
    property OnDrawBackground;
  end;

function SpXPTabThemeType(T: TSpTBXTabThemeType): TSpTBXTabThemeType;
procedure SpDrawXPTab(ACanvas: TCanvas; ARect: TRect; Enabled, Selected, MouseOver, Focused: Boolean; Position: TSpTBXTabPosition; ThemeType: TSpTBXTabThemeType = tttWindows; Edge: TSpTBXTabEdge = tedNone);
procedure SpDrawXPTabControlBackground(ACanvas: TCanvas; ARect: TRect; AColor: TColor; Position: TSpTBXTabPosition; ThemeType: TSpTBXTabThemeType = tttWindows);

implementation

uses
  TBXUxThemes, TBXUtils, Types;

type
  TTBRootItemAccess = class(TTBRootItem);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

function SpXPTabThemeType(T: TSpTBXTabThemeType): TSpTBXTabThemeType;
begin
  Result := T;
  if (Result = tttTBX) and (TBXCurrentTheme = 'Default') then
    Result := tttWindows;
  if (Result = tttWindows) and not SpXPThemesAvailable then
    Result := tttNone;
end;

procedure SpDrawXPTab(ACanvas: TCanvas; ARect: TRect;
  Enabled, Selected, MouseOver, Focused: Boolean; Position: TSpTBXTabPosition;
  ThemeType: TSpTBXTabThemeType = tttWindows; Edge: TSpTBXTabEdge = tedNone);
var
  Part, Flags: Cardinal;
  B: TBitmap;
  R: TRect;
  ItemInfo: TTBXItemInfo;
begin
  ThemeType := SpXPTabThemeType(ThemeType);
  if (ThemeType = tttNone) and not Selected then
    Exit;

  B := TBitmap.Create;
  try
    B.Width := ARect.Right - ARect.Left;
    B.Height := ARect.Bottom - ARect.Top;
    R := Rect(0, 0, B.Width, B.Height);

    if ThemeType = tttWindows then begin
      case Edge of
        tedLeft: Part := TABP_TABITEMLEFTEDGE;
        tedRight: Part := TABP_TABITEMRIGHTEDGE;
      else
        Part := TABP_TABITEM;
      end;

      if not Enabled then
        Flags := TIS_DISABLED
      else
        if Selected then
          Flags := TIS_SELECTED
        else
          if MouseOver then
            Flags := TIS_HOT
          else
            Flags := TIS_NORMAL;
      DrawThemeBackground(SP_TAB_THEME, B.Canvas.Handle, Part, Flags, R, nil);
    end
    else begin
      if ThemeType = tttTBX then begin
        // [TBXTheme-Change]
        // Flip the tab background on some themes.
        if (TBXCurrentTheme = 'Dream') or (TBXCurrentTheme = 'Miranda') or
          (TBXCurrentTheme = 'Nexos2') or (TBXCurrentTheme = 'NexosX') or
          (TBXCurrentTheme = 'OfficeC') or (TBXCurrentTheme = 'OfficeK') or
          (TBXCurrentTheme = 'Relifer') or (TBXCurrentTheme = 'SentimoX') or
          (TBXCurrentTheme = 'Tristan') or (TBXCurrentTheme = 'Tristan2') or
          (TBXCurrentTheme = 'Zezio') then
        begin
          Position := ttpBottom; // Flip
        end
        else
          Position := ttpTop; // Don't flip
        SpFillItemInfo(Enabled, False, MouseOver, Selected, ItemInfo);
        CurrentTheme.PaintButton(B.Canvas, R, ItemInfo);
      end
      else
        if Selected then begin
          Position := ttpTop; // Don't flip
          B.Canvas.Brush.Color := ACanvas.Brush.Color;
          B.Canvas.FillRect(R);

          if ThemeType = tttFlat then
            ExtCtrls.Frame3D(B.Canvas, R, clWindow, clBtnShadow, 1)
          else begin
            ExtCtrls.Frame3D(B.Canvas, R, clWindow, clWindowFrame, 1);
            ExtCtrls.Frame3D(B.Canvas, R, B.Canvas.Brush.Color, clBtnShadow, 1);
          end;
        end;
    end;

    R := Rect(0, 0, B.Width, B.Height);
    if Selected and Focused then begin
      InflateRect(R, -3, -3);
      SpDrawFocusRect(B.Canvas, R);
    end;

    // Flip top to bottom
    if Position = ttpBottom then begin
      // Unclear why extra "-1" is needed here.
      R.Top := B.Height - 1;
      R.Bottom := -1;
    end;

    ACanvas.CopyRect(ARect, B.Canvas, R);
  finally
    B.Free;
  end;
end;

procedure SpDrawXPTabControlBackground(ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  Position: TSpTBXTabPosition; ThemeType: TSpTBXTabThemeType = tttWindows);
var
  C: TColor;
  ItemInfo: TTBXItemInfo;
  B: TBitmap;
  R: TRect;
begin
  ThemeType := SpXPTabThemeType(ThemeType);
  B := TBitmap.Create;
  try
    B.Width := ARect.Right - ARect.Left;
    B.Height := ARect.Bottom - ARect.Top;
    R := Rect(0, 0, B.Width, B.Height);

    if ThemeType = tttWindows then
      DrawThemeBackground(SP_TAB_THEME, B.Canvas.Handle, TABP_PANE, 0, R, nil)
    else begin
      // Draw the top/bottom border
      if ThemeType = tttTBX then begin
        Position := ttpTop; // Don't flip
        B.Canvas.Brush.Color := clWhite;
        B.Canvas.FillRect(R);
        SpFillItemInfo(True, False, False, True, ItemInfo);
        CurrentTheme.PaintButton(B.Canvas, R, ItemInfo);
      end
      else begin
        Position := ttpTop; // Don't flip
        C := B.Canvas.Brush.Color;
        B.Canvas.Brush.Color := AColor;
        B.Canvas.FillRect(R);

        if ThemeType = tttFlat then
          ExtCtrls.Frame3D(B.Canvas, R, clWindow, clBtnShadow, 1)
        else begin
          ExtCtrls.Frame3D(B.Canvas, R, clWindow, clWindowFrame, 1);
          ExtCtrls.Frame3D(B.Canvas, R, AColor, clBtnShadow, 1);
        end;

        B.Canvas.Brush.Color := C;
      end;
    end;

    R := Rect(0, 0, B.Width, B.Height);

    case Position of
      ttpBottom:
        begin
          // Flip top to bottom
          // Unclear why extra "-1" is needed here.
          R.Top := B.Height - 1;
          R.Bottom := -1
        end;
    end;

    ACanvas.CopyRect(ARect, B.Canvas, R);
  finally
    B.Free;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabItemDragObject }

constructor TSpTBXTabItemDragObject.Create(ASourceControl: TControl;
  AItem: TTBCustomItem);
begin
  inherited Create(ASourceControl, AItem);
  DragCursorAccept := crDefault;
  DragCursorCancel := crDefault;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabItem }

constructor TSpTBXTabItem.Create(AOwner: TComponent);
begin
  inherited;
  DisplayMode := nbdmImageAndText;
  GroupIndex := C_SpTBXTabGroupIndex;
  Wrapping := twEndEllipsis;
  Margins := 4;
  FTabPosition := ttpTop;
  FThemeType := tttWindows;
end;

procedure TSpTBXTabItem.Click;
var
  T: TSpTBXTabToolbar;
  I: Integer;
begin
  // Set the Checked property to True, Autocheck is False
  // Checked will call Item.Invalidate, the TabSet.ItemNotification will
  // handle the invalidation and set the ActiveTabIndex.
  if not Checked and Enabled and Visible then begin
    if GetTabToolbar(T) then begin
      I := T.Items.IndexOf(Self);
      if T.FOwnerTabControl.CanActiveTabChange(T.ActiveTabIndex, I) then
        Checked := True;
    end;
    inherited;
  end;
end;

function TSpTBXTabItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := inherited DialogChar(CharCode);
  if Enabled and Visible and IsAccel(CharCode, Caption) then begin
    Click;
    Result := True;
  end;
end;

procedure TSpTBXTabItem.DoDrawTab(ACanvas: TCanvas; ARect: TRect;
  AEnabled, AChecked, AHoverItem: Boolean; Position: TSpTBXTabPosition;
  ASeparator: Boolean = False; AEdge: TSpTBXTabEdge = tedNone);
var
  C: TColor;
  ItemInfo: TTBXItemInfo;
  TT: TSpTBXTabThemeType;
begin
  TT := ThemeType;

  if ASeparator then begin
    // TBX Bug!!! PaintSeparator treats the Rect as zero based
    // It assumes Rect.Left = 0
    // I had to multiply the R.Right by 2 to solve this issue,
    // but in the Default theme it won't work because it uses DrawEdge
    if (TT <> tttTBX) or (CurrentTheme.Name = 'Default') then begin
      C := ACanvas.Pen.Color;
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(ARect.Right - 2, ARect.Top + 3);
      ACanvas.LineTo(ARect.Right - 2, ARect.Bottom - 3);
      ACanvas.Pen.Color := clBtnHighlight;
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top + 3);
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 3);
      ACanvas.Pen.Color := C;
    end
    else begin
      FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
      with ItemInfo do begin
        ViewType    := TVT_MENUBAR;
        ItemOptions := IO_APPACTIVE or IO_TOOLBARSTYLE or IO_DESIGNING;
        Enabled     := True;
        IsVertical  := True;
      end;
      ARect := Rect(0, ARect.Top + 3, (ARect.Right - 2)*2, ARect.Bottom - 3);
      CurrentTheme.PaintSeparator(ACanvas, ARect, ItemInfo, false, true);
    end;
  end
  else begin
    if not Checked and AHoverItem and (TT = tttFlat) then
      TT := tttTBX; // Hover using TBX style
    C := ACanvas.Brush.Color;
    ACanvas.Brush.Color := TabColor;
    SpDrawXPTab(ACanvas, ARect, AEnabled, AChecked, AHoverItem, False, Position, TT, AEdge);
    ACanvas.Brush.Color := C;
  end;
end;

procedure TSpTBXTabItem.DrawBottomBorder(ACanvas: TCanvas; ARect: TRect);
var
  CR, R: TRect;
  Edge: TSpTBXTabEdge;
  LeftT, RightT: Boolean;
  Position: TSpTBXTabPosition;
  T: TSpTBXTabToolbar;
  B: TBitmap;
  BottomOffset, BitmapBottomOffset: Integer;
  TT: TSpTBXTabThemeType;
begin
  TT := SpXPTabThemeType(ThemeType);
  Position := GetTabPosition;
  Edge := tedNone;
  CR := ARect;

  // [TBXTheme-Change]
  // Add the bottom borders
  // Some buttons are smaller, we need to inc the Height by 1
  BottomOffset := 1;
  if TT = tttTBX then
    if (TBXCurrentTheme = 'Dream') or (TBXCurrentTheme = 'Miranda') or (TBXCurrentTheme = 'Monai') or
      (TBXCurrentTheme = 'MonaiXP') or (TBXCurrentTheme = 'NexosX') or (TBXCurrentTheme = 'Stripes') or
      (TBXCurrentTheme = 'Xito') or (TBXCurrentTheme = 'Zezio') then
    begin
      BottomOffset := 2
    end;

  BitmapBottomOffset := 4;
  case Position of
    ttpTop:
      Inc(CR.Bottom, BottomOffset);
    ttpBottom:
      begin
        // When tttNone the bottom border size is 2
        if TT = tttNone then
          Dec(CR.Top, 2)
        else
          Dec(CR.Top, BottomOffset);
      end;
  end;

  if GetTabToolbar(T) and (TT = tttWindows) then begin
    LeftT := Assigned(GetNextTabItemViewer(False, True));
    RightT := Assigned(GetNextTabItemViewer(True, True));
    if IsFirstTab then
      Edge := tedLeft;

    if Edge = tedLeft then begin
      CR.Left := CR.Left - 2;
      if RightT then
        CR.Right := CR.Right + 2;
    end
    else begin
      if LeftT then
        CR.Left := CR.Left - 2;
      if RightT then
        CR.Right := CR.Right + 2;
    end;
  end;

  B := TBitmap.Create;
  try
    B.Width := CR.Right - CR.Left;
    B.Height := CR.Bottom - CR.Top + BitmapBottomOffset; // Larger than CR
    R := Rect(0, 0, B.Width, B.Height);
    DoDrawTab(B.Canvas, R, True, True, False, Position, False, Edge);

    case Position of
      ttpTop:
        R  := Bounds(0, 0, CR.Right - CR.Left, CR.Bottom - CR.Top); // Copy from Y = 0
      ttpBottom:
        R  := Bounds(0, 2, CR.Right - CR.Left, CR.Bottom - CR.Top + 2); // Copy from Y = 2
    end;

    ACanvas.CopyRect(CR, B.Canvas, R);
  finally
    B.Free;
  end;
end;

procedure TSpTBXTabItem.DoDrawAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  inherited;
  if (StateFlags and ISF_DISABLED = 0) and (ThemeType <> tttTBX) then
    AFont.Color := clBtnText;
end;

procedure TSpTBXTabItem.DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
var
  LeftT, RightT: TTBItemViewer;
  IsHoverItem: Boolean;
  R: TRect;
  Position: TSpTBXTabPosition;
  TT: TSpTBXTabThemeType;
  T: TSpTBXTabToolbar;
begin
  inherited;

  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;
    IsHoverItem := ItemInfo.HoverKind <> hkNone;
    Position := GetTabPosition;

    if GetTabToolbar(T) then begin
      LeftT := GetNextTabItemViewer(False, True);
      RightT := GetNextTabItemViewer(True, True);
    end
    else begin
      LeftT := nil;
      RightT := nil;
    end;

    // Match the bottom of the Tab with the bottom of the TabSet
    case Position of
      ttpTop:    ARect.Bottom := ARect.Bottom + 1;
      ttpBottom: ARect.Top := ARect.Top - 1;
    end;
    R := ARect;

    TT := SpXPTabThemeType(ThemeType);
    if TT = tttWindows then begin
      if Checked then begin
        // The left border of the Tab will be painted by the Left tab
        if Assigned(LeftT) or IsFirstTab then
          R.Left := R.Left - 2;
        // The right border of the Tab will be painted by the Right tab
        if Assigned(RightT) then
          R.Right := R.Right + 2;
      end
      else begin
        // Non checked tabs should be smaller
        case Position of
          ttpTop:    Inc(R.Top, 2);
          ttpBottom: Dec(R.Bottom, 2);
        end;
      end;

      // Draw the Tab
      DoDrawTab(ACanvas, R, Enabled, Checked, IsHoverItem, Position);

      // If the Tab is not checked then it should paint the active tab borders
      if not Checked then begin
        R := ARect;
        // Draw the left border
        if Assigned(LeftT) and LeftT.Item.Checked then begin
          R.Right := R.Left + 2;
          R.Left := R.Right - 10;
          DoDrawTab(ACanvas, R, LeftT.Item.Enabled, True, IsHoverItem, Position);
        end
        else
          // Draw the right border
          if Assigned(RightT) and RightT.Item.Checked then begin
            R.Left := R.Right - 2;
            R.Right := R.Left + 10;
            DoDrawTab(ACanvas, R, RightT.Item.Enabled, True, IsHoverItem, Position);
          end;
      end;
    end
    else begin
      // Custom or TBX theme
      if Checked or (IsHoverItem and (TT <> tttNone) and not ((TT = tttFlat) and (TBXCurrentTheme = 'Default')) ) then begin
        case Position of
          ttpTop:    Inc(R.Bottom, 5);
          ttpBottom: Dec(R.Top, 5);
        end;
        DoDrawTab(ACanvas, R, Enabled, Checked, IsHoverItem, Position);
      end
      else begin
        // Draw the separators
        RightT := GetNextTabItemViewer(True, True);
        if Assigned(RightT) and not RightT.Item.Checked then
          DoDrawTab(ACanvas, R, Enabled, Checked, IsHoverItem, Position, True);
      end;
    end;
  end;
end;

procedure TSpTBXTabItem.DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ClientAreaRect: TRect; var ACaption: WideString; var CaptionRect: TRect;
  var CaptionFormat: Cardinal; IsTextRotated: Boolean;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if not Checked then
    case GetTabPosition of
      ttpTop:    OffsetRect(CaptionRect, 0, 2);
      ttpBottom: OffsetRect(CaptionRect, 0, -2);
    end;

  // [TBXTheme-Change]
  // The Default theme paints the caption of the pushed button in a down
  // state, this only happens when the item is in a toolbarstyle
  if ItemInfo.Pushed and (TBXCurrentTheme = 'Default') then
    OffsetRect(CaptionRect, -1, -1);

  inherited DoDrawCaption(ACanvas, ItemInfo, ClientAreaRect, ACaption, CaptionRect,
    CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
end;

procedure TSpTBXTabItem.DoDrawImage(ACanvas: TCanvas;
  const ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
  AImageList: TCustomImageList; var AImageIndex: Integer; var ARect: TRect;
  var PaintDefault: Boolean);
begin
  if not Checked then
    case GetTabPosition of
      ttpTop:    OffsetRect(ARect, 0, 2);
      ttpBottom: OffsetRect(ARect, 0, -2);
    end;
  inherited DoDrawImage(ACanvas, ItemInfo, PaintStage, AImageList, AImageIndex,
    ARect, PaintDefault);
end;

function TSpTBXTabItem.GetNextTabItemViewer(GoForward: Boolean; Inmediate: Boolean = True): TTBItemViewer;
// Returns the left or right Tab item depending on GoForward
// When Inmediate is false it will iterate until a tab is found
var
  T: TSpTBXTabToolbar;
  DefIV, IV: TTBItemViewer;
begin
  Result := nil;
  if GetTabToolbar(T) then begin
    DefIV := SpFindItemViewer(T.View, Self);
    if Assigned(DefIV) then begin
      IV := DefIV;
      while Result = nil do begin
        IV := SpGetNextItem(IV, GoForward, True, False, True);
        if not Assigned(IV) then
          Break
        else
          if IV.Item is TSpTBXTabItem then begin
            Result := IV;
            Break;
          end
          else
            if Inmediate then
              Break;
      end;
    end;
  end;
end;

function TSpTBXTabItem.GetNextTab(GoForward: Boolean; Inmediate: Boolean = True): TSpTBXTabItem;
var
  IV: TTBItemViewer;
begin
  Result := nil;
  IV := GetNextTabItemViewer(GoForward, Inmediate);
  if Assigned(IV) then
    if IV.Item is TSpTBXTabItem then
      Result := IV.Item as TSpTBXTabItem
end;

function TSpTBXTabItem.GetTabColor: TColor;
var
  T: TSpTBXTabToolbar;
begin
  Result := clBtnFace;
  if GetTabToolbar(T) then
    Result := T.TabColor;
end;

function TSpTBXTabItem.GetTabPosition: TSpTBXTabPosition;
var
  T: TSpTBXTabToolbar;
begin
  if GetTabToolbar(T) then
    FTabPosition := T.TabPosition;
  Result := FTabPosition;
end;

procedure TSpTBXTabItem.SetTabPosition(const Value: TSpTBXTabPosition);
var
  T: TSpTBXTabToolbar;
begin
  // Don't change the TabPosition if the item is inside a TabToolbar
  if not GetTabToolbar(T) then begin
    FTabPosition := Value;
    Invalidate;
  end;
end;

function TSpTBXTabItem.GetThemeType: TSpTBXTabThemeType;
var
  T: TSpTBXTabToolbar;
begin
  if GetTabToolbar(T) then
    FThemeType := T.ThemeType;
  Result := FThemeType;
end;

procedure TSpTBXTabItem.SetThemeType(const Value: TSpTBXTabThemeType);
var
  T: TSpTBXTabToolbar;
begin
  // Don't change the ThemeType if the item is inside a TabToolbar
  if not GetTabToolbar(T) then begin
    FThemeType := Value;
    Invalidate;
  end;
end;

function TSpTBXTabItem.IsFirstTab: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  Result := False;
  if GetTabToolbar(T) then
    if T.View.ViewerCount > 0 then
      Result := T.View.Viewers[0].Item = Self;
end;

function TSpTBXTabItem.IsLastTab: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  if GetTabToolbar(T) then
    Result := not Assigned(GetNextTabItemViewer(True, False))
  else
    Result := False;
end;

function TSpTBXTabItem.GetTabToolbar(out TabToolbar: TSpTBXTabToolbar): Boolean;
var
  C: TComponent;
begin
  C := GetParentComponent;
  if Assigned(C) and (C is TSpTBXTabToolbar) then
    TabToolbar := C as TSpTBXTabToolbar
  else
    TabToolbar := nil;
  Result := Assigned(TabToolbar);
end;

procedure TSpTBXTabItem.ToggleControl;
begin
  // Do nothing, the Control property is the Tabsheet, and its visibility
  // is setted by TabSet.ActiveTabIndex
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabToolbar }

constructor TSpTBXTabToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FHiddenTabs := TSpTBXItemCacheCollection.Create(TSpTBXItemCache);
  if Owner is TSpTBXCustomTabSet then
    FOwnerTabControl := Owner as TSpTBXCustomTabSet
  else
    FOwnerTabControl := nil;
  FActiveTabIndex := -1;
  FThemeType := tttWindows;
  FTabAutofitMaxSize := 200;
  FTabColor := clBtnFace;
  FTabPosition := ttpTop;
end;

destructor TSpTBXTabToolbar.Destroy;
begin
  FHiddenTabs.Free;
  inherited;
end;

procedure TSpTBXTabToolbar.DoItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
begin
  inherited;
  if not FUpdatingHidden and not IsItemMoving then
    if Action = tbicInvalidateAndResize then
      InvalidateNC;
end;

procedure TSpTBXTabToolbar.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
var
  DC: HDC;
  B: TBitmap;
  R, ExcludeR, BitmapR, DestR: TRect;
  PaintDefault: Boolean;
  ACanvas: TCanvas;
  ToolbarInfo: TTBXToolbarInfo;
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
begin
  inherited;

  if (csDestroying in ComponentState) or not Docked or not HandleAllocated or
    not Assigned(FOwnerTabControl) then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    GetToolbarInfo(ToolbarInfo);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      ExcludeR := R;
      with ToolbarInfo.BorderSize, ExcludeR do
      begin
        InflateRect(ExcludeR, -X, -Y);
        if ToolbarInfo.IsVertical then Inc(Top, GetTBXDragHandleSize(ToolbarInfo))
        else Inc(Left, GetTBXDragHandleSize(ToolbarInfo));
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
    end;

    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;

      // Draw the bottom border of the tabs pane
      B := TBitmap.Create;
      try
        case FTabPosition of
          ttpTop:
            DestR := Rect(R.Left, R.Bottom - 2, R.Right, R.Bottom + 4);
          ttpBottom:
            DestR := Rect(R.Left, R.Top - 4, R.Right, R.Top + 2);
        end;
        BitmapR := Bounds(0, 0, DestR.Right - DestR.Left, DestR.Bottom - DestR.Top);
        B.Width := BitmapR.Right;
        B.Height := BitmapR.Bottom;

        PaintDefault := True;
        FOwnerTabControl.DoDrawBackground(B.Canvas, BitmapR, pstPrePaint, PaintDefault);
        if PaintDefault then
          SpDrawXPTabControlBackground(B.Canvas, BitmapR, FTabColor, FTabPosition, FThemeType);
        PaintDefault := True;
        FOwnerTabControl.DoDrawBackground(B.Canvas, BitmapR, pstPostPaint, PaintDefault);

        ACanvas.Draw(DestR.Left, DestR.Top, B);
      finally
        B.Free;
      end;

      // Draw the bottom border of the active tab
      Tab := ActiveTab;
      if Assigned(Tab) and Tab.Visible then begin
        IV := SpFindItemViewer(View, Tab);
        if Assigned(IV) then begin
          DestR := IV.BoundsRect;
          OffsetRect(DestR, 2, 2);  // Add the toolbar margins
          Tab.DrawBottomBorder(ACanvas, DestR);
        end;
      end;

    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXTabToolbar.InvalidateActiveTab;
var
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
begin
  Tab := ActiveTab;
  if Assigned(Tab) then begin
    IV := SpFindItemViewer(View, Tab);
    if Assigned(IV) then
      View.Invalidate(IV);
  end;
end;

procedure TSpTBXTabToolbar.InvalidateNC;
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME);
end;

function TSpTBXTabToolbar.GetActiveTab: TSpTBXTabItem;
var
  Item: TTBCustomItem;
begin
  Result := nil;
  if Assigned(Items) and (FActiveTabIndex > -1) and (Items.Count > 0) and
    (FActiveTabIndex < Items.Count) then
  begin
    Item := Items[FActiveTabIndex];
    if not (csDestroying in Item.ComponentState) and (Item is TSpTBXTabItem) and Assigned(Item.Parent) then
      Result := Items[FActiveTabIndex] as TSpTBXTabItem;
  end;
end;

procedure TSpTBXTabToolbar.Autofit;
var
  I, TabsCount, TabsWidth, TabsArea, NonTabsArea, RightAlignWidth: Integer;
  IV: TTBItemViewer;
  R: TRect;
begin
  if not FTabAutofit or FUpdatingHidden or (Items.Count = 0) then Exit;

  FUpdatingHidden := True;
  try
    // Make all the clipped items visible
    for I := 0 to FHiddenTabs.Count - 1 do
      FHiddenTabs.Items[I].Item.Visible := True;
    FHiddenTabs.Clear;

    View.ValidatePositions;
    View.BeginUpdate;
    try
      TabsCount := 0;
      TabsWidth := 0;
      NonTabsArea := 0;
      RightAlignWidth := 0;

      // Get TabsCount and NonTabsArea
      for I := 0 to View.ViewerCount - 1 do begin
        IV := View.Viewers[I];
        if IV.Item.Visible then begin
          if IV.Item is TSpTBXTabItem then
            Inc(TabsCount)
          else
            if IV.Item is TSpTBXRightAlignSpacerItem then
              Inc(RightAlignWidth, 20)
            else begin
              R := SpGetBoundsRect(IV, Items);
              Inc(NonTabsArea, R.Right - R.Left);
            end;
        end;
      end;

      // Get TabsArea
      if TabsCount > 0 then begin
        TabsArea := CurrentDock.ClientWidth - 4 - NonTabsArea - RightAlignWidth;
        TabsWidth := TabsArea div TabsCount;
        if TabsWidth > FTabAutofitMaxSize then
          TabsWidth := FTabAutofitMaxSize;
      end;

      // Get RightAlignWidth
      Inc(RightAlignWidth, CurrentDock.Width - ((TabsWidth * TabsCount) + NonTabsArea + RightAlignWidth));

      // Set TabsWidth and RightAlignWidth to the Items
      for I := 0 to View.ViewerCount - 1 do begin
        IV := View.Viewers[I];
        if IV.Item.Visible then begin
          if IV.Item is TSpTBXTabItem then
            TSpTBXTabItem(IV.Item).CustomWidth := TabsWidth
          else
            if IV.Item is TSpTBXRightAlignSpacerItem then
              TSpTBXRightAlignSpacerItem(IV.Item).CustomWidth := RightAlignWidth - GetRightAlignMargin;
        end;
      end;
    finally
      View.EndUpdate;
    end;
  finally
    FUpdatingHidden := False;
  end;
end;

procedure TSpTBXTabToolbar.RightAlignItems;
// Hide the items on resizing
var
  I, J, VisibleWidth, RightAlignedWidth, SpacerW, RightAlignedBorder: integer;
  FirstIV, LastIV, IV: TTBItemViewer;
  Spacer: TSpTBXItemViewer;
  RightAlignedList: TList;
  IsRotated: Boolean;
begin
  if FUpdatingHidden or (csDestroying in ComponentState) or not Assigned(CurrentDock) or
    (CurrentDock.Width = 0) or (CurrentDock.Height = 0) or (Items.Count <= 0) or
    not Stretch or (ShrinkMode <> tbsmNone) then
      Exit;

  if FTabAutofit then begin
    Autofit;
    Exit;
  end;

  IsRotated := CurrentDock.Position in [dpLeft, dpRight];
  FirstIV := View.NextSelectable(nil, True);
  if Assigned(FirstIV) then begin
    View.ValidatePositions;
    View.BeginUpdate;
    RightAlignedList := TList.Create;
    try
      // Find the spacer and the right aligned items
      Spacer := SpGetRightAlignedItems(View, RightAlignedList, IsRotated, VisibleWidth, RightAlignedWidth);
      if Assigned(Spacer) then begin
        SpacerW := TSpTBXCustomItemAccess(Spacer.Item).CustomWidth;
        RightAlignedBorder := CurrentDock.Width - 8 - RightAlignedWidth + SpacerW;
        VisibleWidth := VisibleWidth - SpacerW;
        SpacerW := CurrentDock.Width - VisibleWidth - 4;
      end
      else begin
        SpacerW := 0;
        RightAlignedBorder := CurrentDock.Width - 8;
      end;

      LastIV := View.NextSelectable(Spacer, False);

      // Show items
      for I := LastIV.Index to View.ViewerCount - 1 do begin
        IV := View.Viewers[I];
        if (IV <> FirstIV) and not IV.Item.Visible and (RightAlignedList.IndexOf(IV) = -1) then begin
          // If the item was hidden and can be showed remove it from the HiddenList
          J := FHiddenTabs.IndexOf(IV.Item);
          if J > -1 then begin
            VisibleWidth := VisibleWidth + FHiddenTabs[J].Width;
            if (VisibleWidth < CurrentDock.Width - 8) then begin
              SpacerW := SpacerW - FHiddenTabs[J].Width;
              FHiddenTabs.Delete(J);
              IV.Item.Visible := True;
            end
            else
              Break;
          end;
        end;
      end;

      // Hide items
      for I := View.ViewerCount - 1 downto 0 do begin
        IV := View.Viewers[I];
        if (IV <> FirstIV) and IV.Item.Visible and (IV.BoundsRect.Right > RightAlignedBorder) then
          if RightAlignedList.IndexOf(IV) = -1 then begin
            // If the item can't be showed add it to the HiddenList
            SpacerW := SpacerW + (IV.BoundsRect.Right - IV.BoundsRect.Left);
            FHiddenTabs.Add(IV.Item, IV.BoundsRect); // Add it before it's hidden, otherwise IV.BoundsRect is invalid
            IV.Item.Visible := False;
          end;
      end;

      // Resize the spacer
      if Assigned(Spacer) then
        TSpTBXCustomItemAccess(Spacer.Item).CustomWidth := SpacerW;

      View.UpdatePositions;
    finally
      RightAlignedList.Free;
      View.EndUpdate;
    end;
  end;
end;

procedure TSpTBXTabToolbar.MakeVisible(ATab: TSpTBXTabItem);
var
  TabIV, FirstIV, LastIV: TTBItemViewer;
  I: integer;
  Spacer: TSpTBXItemViewer;
begin
  if (Items.Count > 1) and Assigned(ATab) and (ATab.Visible = False) then begin
    TabIV := View.Find(ATab);
    FirstIV := View.NextSelectable(nil, True);

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then
      LastIV := View.NextSelectable(Spacer, False)
    else
      LastIV := View.NextSelectable(nil, False);

    if Assigned(FirstIV) and Assigned(LastIV) then begin
      if TabIV.Index > FirstIV.Index then
        for I := 0 to View.ViewerCount - 1 do begin
          ScrollRight;
          if TabIV.Item.Visible then Break;
        end
      else
        for I := 0 to View.ViewerCount - 1 do begin
          ScrollLeft;
          if TabIV.Item.Visible then Break;
        end
    end;
  end;
end;

procedure TSpTBXTabToolbar.ScrollLeft;
var
  FirstIV, LastIV: TTBItemViewer;
  I, ClippedIndex: integer;
  Spacer: TSpTBXItemViewer;
begin
  if (Items.Count > 1) and not FTabAutofit then begin
    FirstIV := View.NextSelectable(nil, True);

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then
      LastIV := View.NextSelectable(Spacer, False)
    else
      LastIV := View.NextSelectable(nil, False);

    if Assigned(FirstIV) and Assigned(LastIV) then begin
      // Find the first clipped tab from the left side of the tabset
      for I := FirstIV.Index - 1 downto 0 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then begin
          View.BeginUpdate;
          FUpdatingHidden := True;
          try
            // Hide the first visible tab from the right side of the tabset
            FHiddenTabs.Add(LastIV.Item, LastIV.BoundsRect);
            LastIV.Item.Visible := False;
            // Show the clipped tab
            FHiddenTabs[ClippedIndex].Item.Visible := True;
            FHiddenTabs.Delete(ClippedIndex);
          finally
            FUpdatingHidden := False;
            if Assigned(Spacer) then
              RightAlignItems
            else
              InvalidateNC;
            View.EndUpdate;
          end;
          Break;          
        end;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.ScrollRight;
var
  FirstIV, LastIV: TTBItemViewer;
  I, ClippedIndex: integer;
  Spacer: TSpTBXItemViewer;
begin
  if (Items.Count > 1) and not FTabAutofit then begin
    FirstIV := View.NextSelectable(nil, True);

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then
      LastIV := View.NextSelectable(Spacer, False)
    else
      LastIV := View.NextSelectable(nil, False);

    if Assigned(FirstIV) and Assigned(LastIV) then begin
      // Find the first clipped tab from the right side of the tabset
      for I := LastIV.Index + 1 to View.ViewerCount - 1 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then begin
          // Hide the first visible tab from the left side of the tabset
          View.BeginUpdate;
          FUpdatingHidden := True;
          try
            FHiddenTabs.Add(FirstIV.Item, FirstIV.BoundsRect);
            FirstIV.Item.Visible := False;
            // Show first clipped tab
            FHiddenTabs[ClippedIndex].Item.Visible := True;
            FHiddenTabs.Delete(ClippedIndex);
          finally
            FUpdatingHidden := False;
            if Assigned(Spacer) then
              RightAlignItems
            else
              InvalidateNC;
            View.EndUpdate;
          end;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.ScrollState(out CanScrollToLeft, CanScrollToRight: Boolean);
var
  FirstIV, LastIV: TTBItemViewer;
  I, ClippedIndex: integer;
  Spacer: TSpTBXItemViewer;
begin
  CanScrollToLeft := False;
  CanScrollToRight := False;
  if (FHiddenTabs.Count > 0) and not FTabAutofit then begin
    FirstIV := View.NextSelectable(nil, True);
    if Assigned(FirstIV) then begin
      // Find the first clipped tab from the left side of the tabset
      for I := FirstIV.Index - 1 downto 0 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then
          CanScrollToLeft := True;
      end;
    end;

    // LastIV minus the right aligned items
    Spacer := SpGetFirstRightAlignSpacer(View);
    if Assigned(Spacer) then
      LastIV := View.NextSelectable(Spacer, False)
    else
      LastIV := View.NextSelectable(nil, False);
    // Find the first clipped tab from the right side of the tabset
    if Assigned(LastIV) then begin
      for I := LastIV.Index + 1 to View.ViewerCount - 1 do begin
        ClippedIndex := FHiddenTabs.IndexOf(View.Viewers[I].Item);
        if ClippedIndex > -1 then
          CanScrollToRight := True;
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetActiveTabIndex(Value: integer);
var
  ATab, APrevTab: TSpTBXTabItem;
  I: Integer;
begin
  if not Assigned(FOwnerTabControl) then Exit;

  if (Value > -1) and (Value < Items.Count) and not (csDestroying in Items[Value].ComponentState) then begin
    if not (Items[Value] is TSpTBXTabItem) then
      Value := FActiveTabIndex;
  end
  else
    Value := -1;

  if (Value <> FActiveTabIndex) and FOwnerTabControl.CanActiveTabChange(FActiveTabIndex, Value) then
  begin
    I := FActiveTabIndex;
    FActiveTabIndex := Value;

    // Hide the previous TabSheet
    if (I > -1)  and (I < Items.Count) and not (csDestroying in Items[I].ComponentState) and
      (Items[I] is TSpTBXTabItem) then
    begin
      APrevTab := Items[I] as TSpTBXTabItem;
      APrevTab.Checked := False;
      if Assigned(APrevTab.Control) then
        APrevTab.Control.Visible := False;
    end;

    // Check the item and invalidate NC
    if FActiveTabIndex > -1 then begin
      // Show the TabSheet
      ATab := Items[FActiveTabIndex] as TSpTBXTabItem;
      ATab.Checked := True;
      if Assigned(ATab.Control) then begin
        ATab.Control.Visible := True;
        ATab.Control.BringToFront;
      end;
      MakeVisible(ATab);
    end;

    FOwnerTabControl.DoActiveTabChange(FActiveTabIndex);
    InvalidateNC;
  end;
end;

procedure TSpTBXTabToolbar.SetTabAutofit(const Value: Boolean);
begin
  if FTabAutofit <> Value then begin
    FTabAutofit := Value;
    if FTabAutofit then begin
      Autofit;
      InvalidateNC;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetTabAutofitMaxSize(const Value: Integer);
begin
  if FTabAutofitMaxSize <> Value then begin
    FTabAutofitMaxSize := Value;
    if FTabAutofit then Autofit;
  end;
end;

procedure TSpTBXTabToolbar.SetTabColor(const Value: TColor);
begin
  if (FTabColor <> Value) then begin
    FTabColor := Value;
    if FThemeType <> tttTBX then begin
      Invalidate;
      InvalidateNC;
    end;
  end;
end;

procedure TSpTBXTabToolbar.SetTabPosition(const Value: TSpTBXTabPosition);
begin
  if FTabPosition <> Value then
    FTabPosition := Value;
end;

procedure TSpTBXTabToolbar.SetThemeType(const Value: TSpTBXTabThemeType);
begin
  if FThemeType <> Value then begin
    FThemeType := Value;
    Invalidate;
    InvalidateNC;
  end;
end;

procedure TSpTBXTabToolbar.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  P: TPoint;
  IV: TTBItemViewer;
  Shift: TShiftState;
begin
  // Allow left-clicks on TabItems at design time
  Shift := KeysToShiftState(Message.Keys);
  if (csDesigning in ComponentState) and (ssLeft in Shift) and Assigned(View) then begin
    P := SmallPointToPoint(Message.Pos);
    IV := View.ViewerFromPoint(P);
    if Assigned(IV) and Assigned(IV.Item) and (IV.Item is TSpTBXTabItem) then
      IV.Item.Click;
  end;

  inherited;
end;

function TSpTBXTabToolbar.CanDragCustomize(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and (Button = mbLeft) then begin
    IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
    if Assigned(IV) and (IV.Item is TSpTBXTabItem) and IV.Item.Enabled then begin
      // Click the item on mouse down
      if not IV.Item.Checked then begin
        Result := True; // Bypass the inherited mouse down
        IV.Item.Click;
        if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
      end;
      // Drag reorder
      if FTabDragReorder and Assigned(IV.Item) and IV.Item.Visible and IV.Item.Checked then begin
        Result := True; // Bypass the inherited mouse down
        FBeginDragIV := IV;
        BeginDrag(False, 2);
      end;
    end;
  end;
end;

procedure TSpTBXTabToolbar.DoStartDrag(var DragObject: TDragObject);
begin
  if FTabDragReorder and Assigned(FBeginDragIV) and Assigned(FBeginDragIV.Item) then begin
    DragObject := TSpTBXTabItemDragObject.Create(Self, FBeginDragIV.Item);
    inherited DoStartDrag(DragObject);
  end
  else
    inherited DoStartDrag(DragObject);
end;

procedure TSpTBXTabToolbar.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  D: TSpTBXTabItemDragObject;
  DestIV, RightAlignIV: TTBItemViewer;
  OrigItem: TTBCustomItem;
  OrigPos, DestPos, RightAlignPos: Integer;
begin
  inherited DragOver(Source, X, Y, State, Accept);

  if FTabDragReorder and Assigned(Source) and (Source is TSpTBXTabItemDragObject) then begin
    D := Source as TSpTBXTabItemDragObject;
    OrigItem := D.SouceItem;
    OrigPos := OrigItem.Parent.IndexOf(OrigItem);

    // Move the dragging item in the toolbar
    if OrigItem.Parent = Items then begin
      Accept := True;
      SpGetDropPosItemViewer(Items, View, Point(X, Y), OrigPos, DestIV, DestPos);
      RightAlignIV := SpGetFirstRightAlignSpacer(View);
      if Assigned(RightAlignIV) then
        RightAlignPos := Items.IndexOf(RightAlignIV.Item)
      else
        RightAlignPos := -1;
      if (OrigPos <> DestPos) and (DestPos > -1) and (DestPos < Items.Count) and (OrigItem <> DestIV.Item) and
        not ((RightAlignPos > -1) and (DestPos >= RightAlignPos)) then
      begin
        if FOwnerTabControl.CanActiveTabReorder(OrigPos, DestPos) then begin
          BeginItemMove;
          View.BeginUpdate;
          try
            // The item is the active tab, we need to update the ActiveTabIndex
            // Just set the internal value because the page didn't change
            FActiveTabIndex := DestPos;
            Items.Move(OrigPos, DestPos);
            FOwnerTabControl.DoActiveTabReorder(DestPos);
          finally
            View.EndUpdate;
            EndItemMove;
            InvalidateNC;
          end;
        end;
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTabSheet }

procedure TSpTBXTabSheet.AdjustClientRect(var Rect: TRect);
var
  Margin, XPMargin: Integer;
begin
  inherited AdjustClientRect(Rect);

  if Assigned(FTabControl) then begin
    Margin := 2;
    XPMargin := 2;
    // [TBXTheme-Change]
    // WinXP theme needs to have 4 pixel margin
    if SpXPTabThemeType(FTabControl.ThemeType) = tttWindows then
      XPMargin := Margin + 2;

    inc(Rect.Left, Margin);
    dec(Rect.Right, XPMargin);
    case FTabControl.TabPosition of
      ttpTop:    dec(Rect.Bottom, XPMargin);
      ttpBottom: inc(Rect.Top, XPMargin);
    end;

    with FTabControl.Margins do
    begin
      Inc(Rect.Left, Left);
      Inc(Rect.Top, Top);
      Dec(Rect.Right, Right);
      Dec(Rect.Bottom, Bottom);
    end;
  end;
end;

constructor TSpTBXTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque, csSetCaption];
  Align := alClient;
  Visible := False;
end;

procedure TSpTBXTabSheet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TabItem', ReadItemName, WriteItemName, True);
end;

destructor TSpTBXTabSheet.Destroy;
begin
  FTabControl := nil;
  FItem := nil;
  inherited;
end;

procedure TSpTBXTabSheet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FPrevFocused then FPrevFocused := nil;
end;

procedure TSpTBXTabSheet.VisibleChanging;
begin
  if not (csDesigning in ComponentState) then
    if Visible then begin
      // TabSheet will be hidden, save the focused control
      if Assigned(FPrevFocused) then FPrevFocused.RemoveFreeNotification(Self);
      SpIsFocused(Self, FPrevFocused);
      if Assigned(FPrevFocused) then FPrevFocused.FreeNotification(Self);
    end;

  inherited;
end;

procedure TSpTBXTabSheet.CMVisiblechanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    if Visible then begin
      // TabSheet was showed.
      // Focus the previous focused control, or focus the first child
      if Assigned(FPrevFocused) then begin
        if SpCanFocus(FPrevFocused) then
          FPrevFocused.SetFocus;
        FPrevFocused.RemoveFreeNotification(Self);
        FPrevFocused := nil;
      end
      else
        SpFocusFirstChild(Self);
    end;

  inherited;
end;

function TSpTBXTabSheet.GetCaption: WideString;
begin
  if Assigned(FItem) then Result := FItem.Caption
  else Result := '';
end;

function TSpTBXTabSheet.GetImageIndex: Integer;
begin
  if Assigned(FItem) then Result := FItem.ImageIndex
  else Result := -1;
end;

function TSpTBXTabSheet.GetTabVisible: Boolean;
begin
  if Assigned(FItem) then Result := FItem.Visible
  else Result := False;
end;

procedure TSpTBXTabSheet.SetCaption(const Value: WideString);
begin
  if Assigned(FItem) then FItem.Caption := Value;
end;

procedure TSpTBXTabSheet.SetImageIndex(const Value: Integer);
begin
  if Assigned(FItem) then FItem.ImageIndex := Value;
end;

procedure TSpTBXTabSheet.SetTabVisible(const Value: Boolean);
begin
  if Assigned(FItem) then FItem.Visible := Value;
end;

procedure TSpTBXTabSheet.ReadItemName(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
     FItemName := Reader.ReadString;
  else
    FItemName := Reader.ReadWideString;
  end;
end;

procedure TSpTBXTabSheet.WriteItemName(Writer: TWriter);
begin
  if Assigned(Item) then
    FItemName := Item.Name;
  Writer.WriteWideString(FItemName);
end;

procedure TSpTBXTabSheet.ReadState(Reader: TReader);
var
  C: TComponent;
  TC: TSpTBXCustomTabControl;
begin
  // The TabSheet is being created from the DFM stream
  // We must set the initial values of TabControl, Item and add itself to
  // the Pages list of the parent TabControl.

  inherited ReadState(Reader);
  if Reader.Parent is TSpTBXCustomTabControl then begin
    // Set TabControl
    TC := TSpTBXCustomTabControl(Reader.Parent);
    TabControl := TC;
    // Set Item and add Self to TabControl.Pages
    if not Assigned(FItem) and (FItemName <> '') then begin
      C := Owner.FindComponent(FItemName);
      if Assigned(C) and (C is TSpTBXTabItem) then begin
        FItem := C as TSpTBXTabItem;
        FItem.Control := Self;
        if TC.FPages.IndexOf(Self) = -1 then
          TC.FPages.Add(Self);
      end;
    end;
  end;
end;

procedure TSpTBXTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  if Assigned(FTabControl) then begin
    R := ClientRect;
    if FTabControl.TabVisible then begin
      case FTabControl.TabPosition of
        ttpTop:    dec(R.Top, 4);
        ttpBottom: inc(R.Bottom, 4);
      end;
    end;

    if FTabControl.DrawBackground(Message.DC, R) then
      Message.Result := 1
    else
      inherited;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTabSet }

constructor TSpTBXCustomTabSet.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  FTabVisible := True;
  FBackground := TBitmap.Create;

  Width := 289;
  Height := FDock.Height + 2;
  ParentColor := False;

  if SpXPTabThemeType(ThemeType) in [tttTBX, tttWindows] then
    Color := clNone // needed for transparency, used by the child controls
  else
    Color := clBtnFace;

  FToolbar.Items.RegisterNotification(ItemNotification);
end;

destructor TSpTBXCustomTabSet.Destroy;
begin
  FToolbar.Items.UnRegisterNotification(ItemNotification);
  FreeAndNil(FBackground);
  inherited;
end;

procedure TSpTBXCustomTabSet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('HiddenItems', ReadHiddenItems, WriteHiddenItems, True);
end;

procedure TSpTBXCustomTabSet.ReadHiddenItems(Reader: TReader);
begin
  if Reader.ReadValue = vaCollection then
    Reader.ReadCollection(Toolbar.FHiddenTabs);
end;

procedure TSpTBXCustomTabSet.WriteHiddenItems(Writer: TWriter);
begin
  Writer.WriteCollection(Toolbar.FHiddenTabs);
end;

procedure TSpTBXCustomTabSet.Loaded;
var
  I: Integer;
  CacheCollection: TSpTBXItemCacheCollection;
  Cache: TSpTBXItemCache;
  C: TComponent;
begin
  ActiveTabIndex := FLoadingActiveIndex;

  inherited;
  // Read the HiddenTabs collection, and fill the Item property of the
  // collection items reading the Name from the DFM
  CacheCollection := Toolbar.FHiddenTabs;
  if Assigned(CacheCollection) then
    for I := CacheCollection.Count - 1 downto 0 do begin
      Cache := CacheCollection[I];
      if not Assigned(Cache.Item) then begin
        if Cache.Name = '' then
          CacheCollection.Delete(I)
        else begin
          C := Owner.FindComponent(Cache.Name);
          if Assigned(C) and (C is TTBCustomItem) then
            Cache.Item := C as TTBCustomItem;
        end;
      end;
    end;

  if TabAutofit then
    Toolbar.Autofit;
end;

function TSpTBXCustomTabSet.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXTabToolbar;
end;

function TSpTBXCustomTabSet.GetFullRepaint: Boolean;
begin
  Result := True;
end;

function TSpTBXCustomTabSet.Add(ACaption: WideString): TSpTBXTabItem;
var
  I: Integer;
  SpacerIV: TSpTBXItemViewer;
begin
  Result := TSpTBXTabItem.Create(Self);
  try
    Result.Caption := ACaption;
    SpacerIV := SpGetFirstRightAlignSpacer(View);
    if Assigned(SpacerIV) then begin
      I := Items.IndexOf(SpacerIV.Item);
      if I > -1 then
        Items.Insert(I, Result);
    end
    else
      Items.Add(Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

function TSpTBXCustomTabSet.Insert(NewIndex: Integer; ACaption: WideString): TSpTBXTabItem;
begin
  Result := TSpTBXTabItem.Create(Self);
  try
    Result.Caption := ACaption;
    Items.Insert(NewIndex, Result);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure TSpTBXCustomTabSet.TabClick(const ATab: TSpTBXTabItem);
begin
  ATab.Click; // calls TabToolbar.DoTabClick and Self.DoTabClick
end;

function TSpTBXCustomTabSet.CanActiveTabChange(const TabIndex, NewTabIndex: Integer): Boolean;
begin
  Result := True;
  if not (csLoading in ComponentState) then
    if Assigned(FOnActiveTabChanging) then FOnActiveTabChanging(Self, TabIndex, NewTabIndex, Result);
end;

procedure TSpTBXCustomTabSet.DoActiveTabChange(const TabIndex: Integer);
begin
  if not (csLoading in ComponentState) then
    if Assigned(FOnActiveTabChange) then FOnActiveTabChange(Self, TabIndex);
end;

function TSpTBXCustomTabSet.CanActiveTabReorder(const TabIndex, NewTabIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnActiveTabReordering) then FOnActiveTabReordering(Self, TabIndex, NewTabIndex, Result);
end;

procedure TSpTBXCustomTabSet.DoActiveTabReorder(const TabIndex: Integer);
begin
  if Assigned(FOnActiveTabReorder) then FOnActiveTabReorder(Self, TabIndex);
end;

procedure TSpTBXCustomTabSet.MakeVisible(ATab: TSpTBXTabItem);
begin
  if Assigned(FToolbar) then Toolbar.MakeVisible(ATab);
end;

procedure TSpTBXCustomTabSet.ScrollLeft;
begin
  if Assigned(FToolbar) then Toolbar.ScrollLeft;
end;

procedure TSpTBXCustomTabSet.ScrollRight;
begin
  if Assigned(FToolbar) then Toolbar.ScrollRight;
end;

procedure TSpTBXCustomTabSet.ScrollState(out Left, Right: Boolean);
begin
  if Assigned(FToolbar) then Toolbar.ScrollState(Left, Right);
end;

function TSpTBXCustomTabSet.GetActiveTabIndex: integer;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.ActiveTabIndex
  else
    Result := -1;
end;

procedure TSpTBXCustomTabSet.SetActiveTabIndex(Value: Integer);
begin
  // When the component is reading from the DFM the Items are not created.
  // We must save the value setted at design time and use it when the
  // form is finally loaded.
  if csReading in ComponentState then
    FLoadingActiveIndex := Value
  else
    if Assigned(FToolbar) then
      Toolbar.ActiveTabIndex := Value;
end;

function TSpTBXCustomTabSet.GetTabAutofit: Boolean;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabAutofit
  else
    Result := False;
end;

procedure TSpTBXCustomTabSet.SetTabAutofit(const Value: Boolean);
begin
  if Assigned(FToolbar) then
    Toolbar.TabAutofit := Value;
end;

function TSpTBXCustomTabSet.GetTabAutofitMaxSize: Integer;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabAutofitMaxSize
  else
    Result := -1;
end;

procedure TSpTBXCustomTabSet.SetTabAutofitMaxSize(const Value: Integer);
begin
  if Assigned(FToolbar) then
    Toolbar.TabAutofitMaxSize := Value;
end;

function TSpTBXCustomTabSet.GetTabBackgroundColor: TColor;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Color
  else
    Result := clNone;
end;

procedure TSpTBXCustomTabSet.SetTabBackgroundColor(const Value: TColor);
begin
  if Assigned(FToolbar) then FToolbar.Color := Value
end;

function TSpTBXCustomTabSet.GetTabDragReorder: Boolean;
begin
  Result := False;
  if Assigned(FToolbar) then
    Result := Toolbar.TabDragReorder;
end;

procedure TSpTBXCustomTabSet.SetTabDragReorder(const Value: Boolean);
begin
  if Assigned(FToolbar) then
    Toolbar.TabDragReorder := Value;
end;

function TSpTBXCustomTabSet.GetTabToolbar: TSpTBXTabToolbar;
begin
  Result := FToolbar as TSpTBXTabToolbar;
end;

function TSpTBXCustomTabSet.GetThemeType: TSpTBXTabThemeType;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.ThemeType
  else
    Result := tttNone;
end;

procedure TSpTBXCustomTabSet.SetThemeType(const Value: TSpTBXTabThemeType);
begin
  if Assigned(FToolbar) then begin
    Toolbar.ThemeType := Value;
    if SpXPTabThemeType(Value) in [tttTBX, tttWindows] then
      Color := clNone // needed for transparency, used by the child controls
    else
      Color := clBtnFace;
    InvalidateDockBackground;
  end;
end;

function TSpTBXCustomTabSet.GetTabPosition: TSpTBXTabPosition;
begin
  if Assigned(FToolbar) then
    Result := Toolbar.TabPosition
  else
    Result := ttpTop;
end;

procedure TSpTBXCustomTabSet.SetTabPosition(const Value: TSpTBXTabPosition);
var
  T: TSpTBXTabToolbar;
begin
  if Assigned(FToolbar) and Assigned(FDock) then begin
    T := Toolbar;
    if T.TabPosition <> Value then begin
      T.Visible := False;
      T.Parent := nil;
      T.TabPosition := Value;
      case Value of
        ttpTop:    FDock.Position := dpTop;
        ttpBottom: FDock.Position := dpBottom;
      end;
      T.CurrentDock := FDock;
      T.Visible := True;

      InvalidateDockBackground;
    end;
  end;
end;

procedure TSpTBXCustomTabSet.SetTabVisible(const Value: Boolean);
begin
  if FTabVisible <> Value then begin
    FTabVisible := Value;
    if Assigned(FDock) then
      FDock.Visible := Value;
  end;
end;

function TSpTBXCustomTabSet.GetTabSetHeight: Integer;
begin
  if Assigned(FDock) then
    Result := FDock.Height
  else
    Result := 0;
end;

procedure TSpTBXCustomTabSet.DoDrawBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXCustomTabSet.DrawBackground(DC: HDC; ARect: TRect): Boolean;
var
  ACanvas: TCanvas;
  PaintDefault: Boolean;
  R: TRect;
begin
  Result := False;
  if (csDestroying in ComponentState) or not Assigned(FDock) or
    not Assigned(FBackground) or IsRectEmpty(ARect) then Exit;

  ACanvas := TCanvas.Create;
  FBackground.Canvas.Lock;
  try
    ACanvas.Handle := DC;
    R := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

    if (FBackground.Width = R.Right) and (FBackground.Height = R.Bottom) and not Assigned(FOnDrawBackground) then
      ACanvas.Draw(ARect.Left, ARect.Top, FBackground)
    else begin
      FBackground.Width := R.Right;
      FBackground.Height := R.Bottom;
      FBackground.Canvas.Brush.Color := clWhite;
      FBackground.Canvas.FillRect(R);

      PaintDefault := True;
      DoDrawBackground(FBackground.Canvas, R, pstPrePaint, PaintDefault);
      if PaintDefault then
        SpDrawXPTabControlBackground(FBackground.Canvas, R, Color, TabPosition, ThemeType);

      PaintDefault := True;
      DoDrawBackground(FBackground.Canvas, R, pstPostPaint, PaintDefault);

      ACanvas.Draw(ARect.Left, ARect.Top, FBackground);
    end;
    Result := True;
  finally
    FBackground.Canvas.Unlock;
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXCustomTabSet.InvalidateDockBackground(Resizing: Boolean = False);
begin
  // Force background repaint
  if (csDestroying in ComponentState) or not Assigned(FToolbar) then Exit;
  if Assigned(FBackground) then FBackground.Width := 1;
  if Resizing then
    SpInvalidateTBXControl(Self, True, True)
  else
    SpInvalidateTBXControl(Self, True, False);
end;

procedure TSpTBXCustomTabSet.ItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  I: Integer;
  Tab: TSpTBXTabItem;
begin
  inherited;

  if Assigned(FToolbar) and not Relayed and not FToolbar.IsItemMoving then
    case Action of
      tbicSubitemsBeginUpdate:
        begin
          // When a Tab item is moved (TTBCustomItem.Move)
          // tbicDeleting and tbicInserted change actions are fired
          // but we don't want the associated TabSheet to be recreated
          // because the children will be destroyed.
          // When a TTBCustomItem is moved it is not recreated, it simply
          // deletes and reinserts its reference in the items array.
          // We need to find out if the item is being moved and stop the
          // TabSheet recreation.
          // The action sequence for a move operation is the following:
          // tbicSubitemsBeginUpdate    (FItemMoveCount = 1)
          //   tbicDeleting             (FItemMoveCount = 2)
          //   tbicSubitemsBeginUpdate  (FItemMoveCount = 1)
          //     tbicInserted           (FItemMoveCount = 0)
          //   tbicSubitemsEndUpdate    (FItemMoveCount = 0)
          // tbicSubitemsEndUpdate      (FItemMoveCount = 0)
          FItemMoveCount := 1;
          FItemMoved := nil;
        end;
      tbicSubitemsEndUpdate:
        begin
          // Destroy the TabSheet if the sequence was:
          // tbicSubitemsBeginUpdate - tbicDeleting - tbicSubitemsEndUpdate
          if FItemMoveCount = 2 then
            TabDeleting(FItemMoved);
          FItemMoveCount := 0;
          FItemMoved := nil;
        end;
      tbicInserted:
        if Assigned(Item) then begin
          // Update the index if a new item is inserted before the ActiveTabIndex
          I := Items.IndexOf(Item);
          if (I > -1) and (I <= ActiveTabIndex) then begin
            FUpdatingIndex := True;
            try
              // Don't change the ActiveTabIndex, just set the internal value
              // because the page didn't change
              Toolbar.FActiveTabIndex := Toolbar.FActiveTabIndex + 1;
            finally
              FUpdatingIndex := False;
            end;
          end;
          if (Item is TSpTBXTabItem) then
            TabInserted(Item as TSpTBXTabItem);
          InvalidateDockBackground;
          FItemMoveCount := 0;
          FItemMoved := nil;
        end;
      tbicDeleting:
        // The ItemViewer of the Item is not valid, it was destroyed by TTBView
        // The Items array still has the Item.
        if not (csDestroying in ComponentState) and Assigned(Item) then begin
          FUpdatingIndex := True;
          try
            Tab := nil;
            I := Items.IndexOf(Item);
            if I > -1 then begin
              if I < ActiveTabIndex then
                // Don't change the ActiveTabIndex, just set the internal value
                // because the page didn't change
                Toolbar.FActiveTabIndex := Toolbar.FActiveTabIndex - 1
              else
                if I = ActiveTabIndex then
                  if I = 0 then begin
                    if (Items.Count > 1) and (Items[1] is TSpTBXTabItem) then begin
                      // The first tab was deleted, change the internal value of
                      // ActiveTabIndex to -1 and click the next tab.
                      // Update the checked tab on WM_INVALIDATETABBACKGROUND
                      Tab := Items[1] as TSpTBXTabItem;
                      Tab.Click;
                    end
                    else
                      Toolbar.FActiveTabIndex := -1;
                  end
                  else
                    SetActiveTabIndex(I - 1); // Prev tab
            end;

            if (Item is TSpTBXTabItem) then
              if FItemMoveCount = 1 then begin
                FItemMoveCount := 2;
                FItemMoved := Item as TSpTBXTabItem;
              end
              else begin
                FItemMoveCount := 0;
                TabDeleting(Item as TSpTBXTabItem);
              end;

            if (csDesigning in ComponentState) or Assigned(Tab) then
              PostMessage(Handle, WM_INVALIDATETABBACKGROUND, 0, 0)
            else
              InvalidateDockBackground;
          finally
            FUpdatingIndex := False;
          end;
        end;
      tbicInvalidate:
        // When the Item.Checked property changes we must reset the ActiveTabIndex
        if not FUpdatingIndex and Assigned(Item) and (Item is TSpTBXTabItem) and
          Item.Checked and Item.Enabled then
        begin
          I := Items.IndexOf(Item);
          if I <> ActiveTabIndex then begin
            FUpdatingIndex := True;
            try
              SetActiveTabIndex(I);
            finally
              FUpdatingIndex := False;
            end;
          end;
        end;
    end;
end;

procedure TSpTBXCustomTabSet.TabDeleting(Item: TSpTBXTabItem;
  FreeTabSheet: Boolean);
begin
  if not (csDestroying in ComponentState) then
    ScrollLeft;
end;

procedure TSpTBXCustomTabSet.TabInserted(Item: TSpTBXTabItem);
begin
  if not (csLoading in ComponentState) then
    if Items.Count = 1 then
      Item.Click // Select the first Tab
end;

procedure TSpTBXCustomTabSet.CMColorchanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FToolbar) then begin
    Toolbar.TabColor := Color;
    InvalidateDockBackground;
  end;
end;

procedure TSpTBXCustomTabSet.TBMGetEffectiveColor(var Message: TMessage);
begin
  // [TBXTheme-Change]
  // Since the TabSet.Color = clNone the NC area of the Dock will be clNone
  // if the theme is OfficeXP.
  // We need to fill it with clBtnFace handling TBM_GETEFFECTIVECOLOR.
  // This message is sent by TTBXDock.WMEraseBkgnd:
  // [...]
  //    C := Color;
  //    if C = clNone then C := GetEffectiveColor(Parent);
  //    FillRectEx(Message.DC, R, C);
  // [...]

  if TBXCurrentTheme = 'OfficeXP' then begin
    Message.WParam := clBtnFace;
    Message.Result := 1;
  end;
end;

procedure TSpTBXCustomTabSet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  if not (csDestroying in ComponentState) and GetFullRepaint then begin
    R := ClientRect;
    if FTabVisible then begin
      case TabPosition of
        ttpTop:    Inc(R.Top, GetTabSetHeight - 2);
        ttpBottom: Dec(R.Bottom, GetTabSetHeight - 2);
      end;
    end;
    if DrawBackground(Message.DC, R) then
      Message.Result := 1
    else
      inherited;
  end
  else
    Message.Result := 1;
end;

procedure TSpTBXCustomTabSet.WMInvalidateTabBackground(var Message: TMessage);
var
  Tab: TSpTBXTabItem;
  I: Integer;
begin
  if Assigned(FToolbar) then begin
    Tab := Toolbar.ActiveTab;
    if Assigned(Tab) then begin
      I := Items.IndexOf(Tab);
      FUpdatingIndex := True;
      try
        Toolbar.FActiveTabIndex := -1;
        SetActiveTabIndex(I);
      finally
        FUpdatingIndex := False;
      end;
    end;
    if HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TSpTBXCustomTabSet.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if GetFullRepaint then
    InvalidateDockBackground(True);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTabControl }

constructor TSpTBXCustomTabControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FPages := TList.Create;
  FMargins := TTBXControlMargins.Create;
  FMargins.OnChange := MarginsChangeHandler;

  // FEmptyTabSheet is used to hide the rest of the TabSheets
  // when ActiveTabIndex = -1 at design time.
  FEmptyTabSheet := TSpTBXTabSheet.Create(Self);
  FEmptyTabSheet.Parent := Self;
  FEmptyTabSheet.TabControl := Self;
  FEmptyTabSheet.Item := nil;
  FEmptyTabSheet.Visible := True;
  FEmptyTabSheet.BringToFront;
  FEmptyTabSheet.ControlStyle := FEmptyTabSheet.ControlStyle - [csAcceptsControls];

  Width := 289;
  Height := 193;
end;

destructor TSpTBXCustomTabControl.Destroy;
begin
  FPages.Free;
  FMargins.Free;
  inherited;
end;

procedure TSpTBXCustomTabControl.DoActiveTabChange(const ItemIndex: Integer);
begin
  if ItemIndex = -1 then begin
    FEmptyTabSheet.Visible := True;
    FEmptyTabSheet.BringToFront;
  end
  else
    FEmptyTabSheet.Visible := False;
  inherited;
end;

procedure TSpTBXCustomTabControl.MarginsChangeHandler(Sender: TObject);
var
  I, C: Integer;
begin
  C := PagesCount;
  for I := 0 to C - 1 do
    Pages[I].Realign;
end;

procedure TSpTBXCustomTabControl.SetMargins(Value: TTBXControlMargins);
begin
  FMargins.Assign(Value);
end;

function TSpTBXCustomTabControl.GetFullRepaint: Boolean;
begin
  if not (csDestroying in ComponentState) then
    Result := not Assigned(FPages) or (FPages.Count = 0) or not Assigned(FToolbar) or
      not Assigned(Toolbar.ActiveTab) or not Toolbar.ActiveTab.Checked
  else
    Result := False;
end;

function TSpTBXCustomTabControl.GetPage(Item: TSpTBXTabItem): TSpTBXTabSheet;
var
  I: Integer;
begin
  Result := nil;
  I := Items.IndexOf(Item);
  if (I > - 1) and Assigned(Item.Control) and (Item.Control is TSpTBXTabSheet) then
    Result := Item.Control as TSpTBXTabSheet;
end;

function TSpTBXCustomTabControl.GetActivePage: TSpTBXTabSheet;
begin
  if ActiveTabIndex > -1 then
    Result := GetPage(Items[ActiveTabIndex] as TSpTBXTabItem)
  else
    Result := nil;
end;

procedure TSpTBXCustomTabControl.SetActivePage(const Value: TSpTBXTabSheet);
var
  I: Integer;
begin
  if Assigned(Value) and (FPages.IndexOf(Value) > -1) and Assigned(FToolbar) then begin
    I := FToolbar.Items.IndexOf(Value.Item);
    if I > -1 then ActiveTabIndex := I;
  end;
end;

function TSpTBXCustomTabControl.GetPages(Index: Integer): TSpTBXTabSheet;
begin
  Result := TSpTBXTabSheet(FPages[Index]);
end;

function TSpTBXCustomTabControl.GetPagesCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TSpTBXCustomTabControl.TabInserted(Item: TSpTBXTabItem);
var
  T: TSpTBXTabSheet;
  I: Integer;
begin
  // Create a TabSheet and Link it to the TabItem, only if the Item is created
  // at DesignTime.
  // If the Item is created from the DFM stream then the TabSheet will be
  // automatically created, because it will also be streamed, but it won't be
  // linked to the Item, this is done in TabSheet.ReadState

  if (csLoading in ComponentState) or not Assigned(Item) then Exit;

  for I := 0 to FPages.Count - 1 do begin
    T := TSpTBXTabSheet(FPages[I]);
    if T.Item = Item then begin
      Exit;
      raise Exception.Create('TabSheet Already Exists');
    end;
  end;

  // Find unique name
  I := 1;
  while Owner.FindComponent('SpTBXTabSheet' + IntToStr(I)) <> nil do
    inc(I);

  // The Form will be the owner, it will stream the tabsheet to the DFM
  T := TSpTBXTabSheet.Create(Owner);
  T.Name := 'SpTBXTabSheet' + IntToStr(I);
  T.Parent := Self;
  T.TabControl := Self;
  T.Item := Item;
  Item.Control := T;
  T.SendToBack;
  FPages.Add(T);

  if FPages.Count = 1 then
    Item.Click; // Select the first Tab
end;

procedure TSpTBXCustomTabControl.TabDeleting(Item: TSpTBXTabItem;
  FreeTabSheet: Boolean);
var
  I: Integer;
  T: TSpTBXTabSheet;
begin
  inherited;
  // The Toolbar will free the Items, and the Form will free the TabSheets
  if (csDestroying in ComponentState) or not Assigned(Item) then Exit;

  for I := 0 to FPages.Count - 1 do begin
    T := TSpTBXTabSheet(FPages[I]);
    if Assigned(T) and Assigned(T.Item) and (T.Item = Item) then begin
      FPages[I] := nil;
      FPages.Delete(I);
      if FreeTabSheet then
        T.Free
      else begin
        // TabSheet deleted at design time, free the linked Item
        T.Item.Free;
        T.Item := nil;
      end;
      Break;
    end;
  end;
end;

procedure TSpTBXCustomTabControl.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    if HandleAllocated then
      MarginsChangeHandler(Margins);
end;

end.
