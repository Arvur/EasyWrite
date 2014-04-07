unit SpTBXItem;

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
  - It would be nice to have a way to get the TTBItemViewer width and height
    even when the Item is not visible (TTBItemViewer.BoundsRect is not valid
    when the item is not visible).
  - It would be nice to allow component writers to override
    TTBCustomToolbar.CreateWrapper so they can create custom TTBControlItem
    items descendants as the dropped control wrappers.
  - It would be nice to have access to the TTBPopupMenu Viewer before the
    popup is showed to initialize the items, for example to set the focus
    on a TTBEditItem.
  - Fix this TBX bug: the Caption property of TBX items doesn't work properly
    at design time. Clicking on the 'more' button raises an AV: "Control '' has
    no parent window".
    The same happens with the ChevronHint property of the toolbars.
  - Fixed incorrect caption painting when the font is italic, this bug
    is present in TBX items, TLabel, TBitBtn, TSpeedButton, TGroupBox,
    TRadioGroup, and any other control that uses DrawText to draw the
    caption. To reproduce this, drop a TBitBtn, change the caption to
    'WWW' and the font to italic, the last W is cropped.
    The fix is in the SpDrawXPText utility function.

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.
  - Windows XP theme data parts that are not loaded by TBX are loaded outside
    TBXThemeManager space, they must be handled by this unit only.
  - UTF8Decode is not supported on Delphi 5, it was introduced on D6.
  - TSpTBXCompoundItemsControl is used as the base class for TB2K items enabled
    Controls, it uses the ITBItems interface and streams the items to the DFM.
  - When a control is dropped on the toolbar a TTBControlItem is created by
    TTBCustomToolbar.CreateWrapper, unfortunately it is created with the
    Toolbar.Owner instead of the Form (Owner.Owner for TSpTBXCompoundItemsControl
    like the TSpTBXTabSet or TSpTBXStatusBar). The workaround is to handle the
    CM_CONTROLCHANGE message in the compound toolbar, stream all the
    TTBControlItem.Control to the DFM (the TSpTBXCompoundItemsControl must be
    the parent in the DFM), and finally reset the TTBControlItem.Control
    parentship to the Toolbar in TSpTBXCompoundItemsControl.Loaded.

To Do:
  - Rotated caption painting.
  - Anchored items on vertical toolbars.

Known Issues:
  - TSpTBXSkinOptions: has partial unicode support, Tnt doesn't have a TIniFile
    class that has unicode capable LoadFromFile/SaveToFile methods.

History:
8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - Added Toolbar public property to TSpTBXStatusBar.

24 November 2006 - version 1.8.1
  - Added properties to TSpTBXRightAlignSpacer: ImageIndex, Images,
    OnAdjustFont, OnClick, OnDrawHint, OnDrawImage, OnDrawItem.
  - Added unicode shortcut-text support for menu items, thanks
    Steve for reporting this.
  - Fixed incorrect TSpTBXTitleBar positioning when the
    taskbar is moved around the screen, thanks to
    Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXStatusBar size grip painting when
    Windows XP themes are disabled, thanks to Alexey Naumov
    for reporting this.

27 August 2006 - version 1.8
  - Fixed incorrect DropdownCombo item painting, thanks to
    François Rivierre for reporting this.
  - Fixed incorrect TSpTBXTitleBar SystemMenu painting,
    GetMenuStringW doesn't work correctly on Win2K/WinXP,
    when a DBCS code page is active (e.g. Japanese), thanks
    to Jordan Russell for reporting this.
    http://news.jrsoftware.org/read/article.php?id=12268&group=jrsoftware.toolbar2000.thirdparty
  - Fixed bug in TSpTBXLabelItem, clicking a TSpTBXLabelItem
    on a popup menu causes the menu to close, thanks to
    Piotr Janus for reporting this.

15 June 2006 - version 1.7
  - Added vertical caption painting to toolbar items.
  - Added CaptionGlow and CaptionGlowColor properties to
    toolbar Items.
  - Added Margins property to toolbar items.
  - Fixed incorrect TSpTBXTitleBar resizing when the form is
    maximized and the titlebar is activated and deactivated
    multiple times, thanks to Costas Stergiou for reporting this.

4 May 2006 - version 1.6
  - Fixed incorrect TSpTBXStatusBar behavior, the size grip
    disappeared when the parent was a TSpTBXTitleBar, thanks to
    Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXStatusBar's size grip painting when the
    Default theme was used.
  - Fixed incorrect TSpTBXLabelItem painting when the label was used
    in a submenu, thanks to Costas Stergiou for reporting this.
  - Added OnSystemMenuPopup event to TSpTBXTitleBar.

12 April 2006 - version 1.5
  - Fixed incorrect TSpTBXStatusBar behavior, it didn't resized
    the form if the mouse click was on the non-client area of the
    status bar, thanks to Frank de Groot for reporting this.
  - Fixed incorrect mouse handling in TSpTBXTitlebar, thanks to
    Marten Pape for reporting this.
  - Fixed incorrect TSpTBXLabelItem alignment on menus, thanks to
    Costas Stergiou for reporting this.
  - Added ClickedItem parameter to TSpTBXSubmenuItem.OnClosePopup
    event.

27 February 2006 - version 1.4
  - Added SizeGrip property to TSpTBXStatusBar.
  - Added FullScreenMaximize property to TSpTBXTitleBar.

10 February 2006 - version 1.3
  - Fixed AV in TSpTBXTitleBar at designtime, thanks to
    Alexey Naumov for reporting this.
  - Fixed incorrect system popupmenu visibility in TSpTBXTitleBar.
  - Added Active property to TSpTBXTitleBar.
  - Added OnDrawBackground event to TSpTBXTitleBar.
  - Added OnClosePopup event to TSpTBXSubmenuItem.

28 December 2005 - version 1.2
  - Fixed incorrect items anchoring.
  - Fixed range check errrors.

18 October 2005 - version 1.1
  - Fixed incorrect TSpTBXStatusBar margins when the form is
    maximazed.
  - Fixed incorrect accel char handling in TSpTBXToolbar.
  - Fixed incorrect TntAction support, the previous version of
    TntActions didn't supported unicode enabled ActionLinks.
  - Fixed incorrect tab stop chars handling in TSpTBXItem.
  - Added MaxSize property to TSpTBXToolbar, determines
    the maximum height the toolbar can have.
  - Added TBX themes support to TSpTBXTitleBar's system menu.
  - Added radio item painting support.

18 August 2005 - version 1.0
  - Added DisplayOptions property to TSpTBXToolbar, determines
    whether the item's image is to be displayed.
  - Added Customizable property to TSpTBXToolbar, determines
    whether the toolbar is customizable or not.
  - Added TitleBarSize property to the Options of the
    TSpTBXTitlebar.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - Fixed incorrect caption centering in TSpTBXItem.
  - Fixed incorrect TSpTBXItem hint when accessing TntApplication,
    thanks to Erik Maly for reporting this.
  - Changed the Options property of TSpTBXTitleBar to use a base class
    for default buttons.
  - Added ChevronVertical property to TSpTBXToolbar, it changes the
    layout of the chevron popup to be vertical.
  - Added Wrapping property to TSpTBXItem, it determines the wrapping
    type of the item's caption.
  - Added FixedSize property to TSpTBXTitleBar, it determines if
    the TitleBar can be resized.

16 February 2005 - version 0.7
  - Fixed TSpTBXThemeGroupItem theme sync bug, it now correctly selects
    the current TBX theme.
  - Fixed unicode support in W9x.
  - Fixed TSpTBXTitleBar painting flicker.
  - Fixed TSpTBXStatusBar right align margin.
  - Added TBXStyleBackground property to TSpTBXTitleBar, when setted to
    true it paints a TBX style background.
  - Added AutoCheck property to TSpTBXItem.

23 December 2004 - version 0.6
  - Fixed hint bug, ampersands were not removed in auto-generated hints.
  - Fixed incorrect caption painting when the font is italic, this bug
    is present in TBX items, TLabel, TBitBtn, TSpeedButton, TGroupBox,
    TRadioGroup, and any other control that uses DrawText to draw the
    caption. To reproduce this, drop a TBitBtn, change the caption to
    'WWW' and the font to italic, the last W is cropped.
  - Fixed incorrect caption painting when the Default theme is used,
    the caption was not painted in a down state when the toolbarstyle
    item was pushed, thanks Daniel Rikowski for reporting this.
  - Changed the default value of DisplayMode to nbdmDefault.
  - New component added, TSpTBXTitleBar: a fully customizable
    TitleBar with Unicode text and TBX themes support.
  - New component added, TSpTBXPopupMenu: a TTBXPopupMenu descendant
    with an OnPopupMenuInit event to setup the items before the popup
    is showed, it could be used for example to set the focus to an
    EditItem.
  - Added SpChangeThemeType utility function, this makes it easier to
    switch the theme type of any given control and its children.

30 August 2004 - version 0.5
  - Reworked the hint show event of the items.

21 July 2004 - version 0.4
  - Fixed TTBControlItem.Control streaming bug on TSpTBXStatusBar.
  - Fixed bad sync of the items unicode caption and hint properties
    when an Action was assigned.

12 July 2004 - version 0.3.1
  - Unchanged.

9 July 2004 - version 0.3
  - Fixed incorrect TSpTBXItem caption painting when DisplayMode
    was nbdmDefault, thanks to Cyril for reporting this.
  - Added anchors support for TTBControlItem items, if the associated
    Control is client aligned or has akRight in its Anchors property.
  - The theme items in TSpTBXThemeGroupItem are now sorted.
  - Added OnUpdate event to TSpTBXThemeGroupItem, this event is fired
    every time the theme items list is recreated, use this event
    to sort or change the items properties.

27 June 2004 - version 0.2
  - Fixed Toolbar custom painting event.
  - Fixed incorrect Shortcut painting in submenus.
  - Fixed incorrect Shortcut hint painting.
  - Removed thtBitmapSkin from TSpTBXThemeType.
  - Published more properties for TSpTBXLabelItem.
  - New Toolbar item added, TSpTBXSeparator.
  - New component added, TSpTBXStatusBar: a fully customizable
    StatusBar with Unicode text and TBX themes support.

22 June 2004 - version 0.1
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation
{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ActnList, IniFiles, TB2Common, TB2Item, TB2Dock, TB2Toolbar,
  TBX, TBXThemes, TBXDefaultTheme, TntClasses, TntControls;

const
  C_SpTBXRadioGroupIndex = 8888;      // Default GroupItem of TSpTBXRadioGroupItem
  rvSpTBXThemeName = 'SpTBXTheme';    // Constant for TSpTBXDefaultTheme and TSpTBXTheme. Do not localize!
  rvSpTBXDisplayMode = 'DisplayMode'; // Constant used to save the Toolbar DisplayMode with the Customizer. Do not localize!
  CM_SPPOPUPCLOSE = CM_BASE + 1111;   // Message sent to the PopupControl to update its state after the Popup is closed

type
  TSpTBXCustomItem = class;
  TSpTBXToolbar = class;
  TSpTBXStatusToolbar = class;
  TSpTBXPopupMenu = class;

  TSpTBXThemeType = (
    thtNone,                   // No themes
    thtWindows,                // Use Windows themes
    thtTBX                     // Use TBX themes
  );

  TSpTBXPaintStage = (
    pstPrePaint,               // Pre paint stage
    pstPostPaint               // Post paint stage
  );

  TSpTBXButtonPaintType = (
    bptDefault,                // SpTBXLib default painting
    bptTBX,                    // TBX style button painting
    bptTBXToolbarItem,         // TBX toolbar items style painting
    bptTBXDock                 // TBX dock style painting
  );

  TSpTBXToolbarDisplayMode = (
    tbdmSelectiveCaption,      // The caption is displayed if the Item.DisplayMode = nbdmImageAndText
    tbdmImageOnly,             // Only the images are displayed
    tbdmImageAboveCaption,     // The images are displayed above the caption
    tbdmTextOnly               // Show the caption only
  );

  TSpGlyphLayout = (
    ghlGlyphLeft,              // Glyph icon on the left of the caption
    ghlGlyphTop                // Glyph icon on the top of the caption
  );

  TSpBorderIcon = (
    briSystemMenu,             // SystemMenu item on the title bar
    briMinimize,               // Minimize item on the title bar
    briMaximize,               // Maximize item on the title bar
    briClose                   // Close item on the title bar
  );

  TSpGlowDirection = (
    gldNone,                   // No glow
    gldAll,                    // Glow on Left, Top, Right and Bottom of the text
    gldTopLeft,                // Glow on Top-Left of the text
    gldBottomRight             // Glow on Bottom-Right of the text
  );

  TSpTextRotationAngle = (
    tra0,                      // No rotation
    tra90,                     // 90 degree rotation
    tra270                     // 270 degree rotation
  );

  TSpBorderIcons = set of TSpBorderIcon;

  TSpTBXGetImageIndexEvent = procedure(Sender: TObject;
    var AImageList: TCustomImageList; var AItemIndex: integer) of object;

  TSpTBXDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean) of object;

  TSpTBXDrawImageEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    const ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
    var AImageList: TCustomImageList; var AImageIndex: integer;
    var ARect: TRect; var PaintDefault: Boolean) of object;

  TSpTBXDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean) of object;

  TSpTBXDrawPosEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    X, Y: Integer; var PaintDefault: Boolean) of object;

  TSpTBXDrawTextEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ClientAreaRect: TRect; var ACaption: WideString; var CaptionRect: TRect;
    var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean) of object;

  TSpTBXDrawHintEvent = procedure(Sender: TObject; AHintBitmap: TBitmap;
    var AHint: WideString; var PaintDefault: Boolean) of object;

  TSpTBXItemNotificationEvent = procedure(Sender: TObject;
    Ancestor: TTBCustomItem; Relayed: Boolean; Action: TTBItemChangedAction;
    Index: Integer; Item: TTBCustomItem) of object;

  TSpTBXGetRadioGroupCaptionEvent = procedure(Sender: TObject;
    var ACaption: WideString) of object;

  TSpTBXPopupEvent = procedure(Sender: TObject; PopupView: TTBView) of object;

  TSpTBXClosePopupEvent = procedure(Sender: TObject; ClickedItem: TTBCustomItem) of object;

  // Method called by the GetNext routine for each item
  TSpTBXGetNextCallBack = function(AItemViewer: TTBItemViewer; Default: Boolean; var EndProcessing: Boolean): Boolean of object;

  TSpTBXIterateAllCallBack = procedure(AItem: TTBCustomItem) of object;

  { TSpTBXCustomDragObject }

  TSpTBXCustomDragObject = class(TDragObjectEx)
  private
    FDragCursorAccept: TCursor;
    FDragCursorCancel: TCursor;
    FSourceControl: TControl;
    FSourceItem: TTBCustomItem;
  protected
    function GetDragCursor(Accepted: Boolean; X: Integer; Y: Integer): TCursor; override;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  public
    constructor Create(ASourceControl: TControl; AItem: TTBCustomItem); virtual;
    property DragCursorAccept: TCursor read FDragCursorAccept write FDragCursorAccept;
    property DragCursorCancel: TCursor read FDragCursorCancel write FDragCursorCancel;
    property SouceItem: TTBCustomItem read FSourceItem;
    property SourceControl: TControl read FSourceControl;
  end;

  { TSpTBXItemDragObject }

  TSpTBXItemDragObject = class(TSpTBXCustomDragObject);

  { TSpTBXCustomItemActionLink }

  TSpTBXCustomItemActionLink = class(TTBCustomItemActionLink)
  protected
    FUnicodeClient: TSpTBXCustomItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    procedure SetCaption(const Value: String); override;
    procedure SetHint(const Value: String); override;
  end;

  { TSpTBXItem }

  TSpTBXItemViewer = class;

  TSpTBXCustomItem = class(TTBXCustomItem)
  private
    FCaption: WideString;
    FCaptionGlow: TSpGlowDirection;
    FCaptionGlowColor: TColor;
    FHint: WideString;
    FAlignment: TAlignment;
    FCustomWidth: Integer;
    FCustomHeight: Integer;
    FMargins: Integer;
    FControl: TControl;
    FAnchored: Boolean;
    FToolbarStylePopup: Boolean;
    FWrapping: TTextWrapping;
    FOnClosePopup: TSpTBXClosePopupEvent;
    FOnDrawCaption: TSpTBXDrawTextEvent;
    FOnDrawHint: TSpTBXDrawHintEvent;
    FOnDrawItem: TSpTBXDrawItemEvent;
    FOnDrawImage: TSpTBXDrawImageEvent;
    procedure SetCaption(const Value: WideString);
    procedure ReadCaptionW(Reader: TReader);
    procedure SetHint(const Value: WideString);
    procedure ReadHintW(Reader: TReader);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAnchored(const Value: Boolean);
    procedure SetCaptionGlow(const Value: TSpGlowDirection);
    procedure SetCaptionGlowColor(const Value: TColor);
    procedure SetCustomWidth(Value: Integer);
    procedure SetCustomHeight(Value: Integer);
    procedure SetMargins(Value: Integer);
    procedure SetControl(const Value: TControl);
    procedure SetWrapping(const Value: TTextWrapping);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function DialogChar(CharCode: Word): Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDrawAdjustFont(AFont: TFont; StateFlags: Integer); virtual;
    procedure DoDrawHint(AHintBitmap: TBitmap; var AHint: Widestring; var PaintDefault: Boolean); virtual;
    procedure DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ClientAreaRect: TRect;
      var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
      IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawImage(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
      const PaintStage: TSpTBXPaintStage; AImageList: TCustomImageList;
      var AImageIndex: integer; var ARect: TRect; var PaintDefault: Boolean); virtual;
    function GetActionLinkClass: TTBCustomItemActionLinkClass; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function GetPopupWindowClass: TTBPopupWindowClass; override;
    procedure ToggleControl; virtual;
    procedure UpdateProps; virtual;
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Anchored: Boolean read FAnchored write SetAnchored default False;
    property CaptionGlow: TSpGlowDirection read FCaptionGlow write SetCaptionGlow default gldNone;
    property CaptionGlowColor: TColor read FCaptionGlowColor write SetCaptionGlowColor default clYellow;
    property Control: TControl read FControl write SetControl;
    property CustomWidth: Integer read FCustomWidth write SetCustomWidth default -1;
    property CustomHeight: Integer read FCustomHeight write SetCustomHeight default -1;
    property Margins: Integer read FMargins write SetMargins default 0;
    property ToolbarStylePopup: Boolean read FToolbarStylePopup write FToolbarStylePopup default False;
    property OnClosePopup: TSpTBXClosePopupEvent read FOnClosePopup write FOnClosePopup;
    property OnDrawHint: TSpTBXDrawHintEvent read FOnDrawHint write FOnDrawHint;
    property OnDrawItem: TSpTBXDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawImage: TSpTBXDrawImageEvent read FOnDrawImage write FOnDrawImage;
    property OnDrawCaption: TSpTBXDrawTextEvent read FOnDrawCaption write FOnDrawCaption;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    function GetShortCutText: WideString; // Reintroduce to support unicode shortcut text
    procedure InitiateAction; override;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: WideString read FCaption write SetCaption; // Hides the inherited Caption
    property Hint: WideString read FHint write SetHint; // Hides the inherited Hint
    property Wrapping: TTextWrapping read FWrapping write SetWrapping default twWrap;
  end;

  TSpTBXItemViewer = class(TTBXItemViewer)
  private
    function GetItem: TSpTBXCustomItem;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    FAnchorSize: TPoint;
    FAnchorDelta: Integer;

    // Custom Painting methods
    procedure DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
      ClientAreaRect: TRect; var ACaption: WideString; var CaptionRect: TRect;
      var CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); reintroduce; virtual;
    procedure DoDrawImage(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
      const PaintStage: TSpTBXPaintStage; AImageList: TCustomImageList;
      var AImageIndex: integer; var ARect: TRect; var PaintDefault: Boolean); virtual;

    // Painting methods
    function CaptionShown: Boolean; override;
    function GetImageShown: Boolean; override;
    procedure DoAdjustFont(AFont: TFont; StateFlags: Integer); override;
    procedure DrawItemImage(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo); override;
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function  GetTextSize(Canvas: TCanvas; const Text: string; TextFlags: Cardinal;
      Rotated: Boolean; StateFlags: Integer): TSize; override;
    function  GetRealTextSize(Canvas: TCanvas; const Text: WideString; TextFlags: Cardinal;
      Rotated: Boolean; StateFlags: Integer; CaptionRect: TRect): TSize;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem: Boolean; IsPushed: Boolean; UseDisabledShadow: Boolean); override;

    // Hints
    procedure Entering(OldSelected: TTBItemViewer); override;
  public
    function GetCaptionText: WideString; reintroduce; virtual; // Hides the inherited TB2K GetCaptionText function
    function GetHintText: Widestring;             // Hides the inherited TB2K GetHintText function
    property Item: TSpTBXCustomItem read GetItem; // Hides the inherited TB2K Item property
  end;

  TSpTBXItem = class(TSpTBXCustomItem)
  published
    property Action;
    property AutoCheck;
    property Checked;
    property DisplayMode;
    property Enabled;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property RadioItem;
    property ShortCut;
    property Stretch default True;
    property Visible;
    property OnAdjustFont;
    // property OnDrawImage; use custom OnDrawImage
    property OnClick;
    property OnSelect;
    // TSpTBXCustomItem properties
    property Alignment;
    property Anchored;
    property CaptionGlow;
    property CaptionGlowColor;
    property Control;
    property CustomWidth;
    property CustomHeight;
    property Margins;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXSubmenuItem }

  TSpTBXSubmenuItem = class(TSpTBXItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AlwaysSelectFirst;
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo default False;
    property LinkSubitems;
    property SubMenuImages;
    property ToolBoxPopup;
    property ToolbarStylePopup;
    property OnPopup;
    property OnClosePopup;
  end;

  { TSpTBXLabelItem }

  TSpTBXCustomLabelItem = class(TSpTBXCustomItem)
  protected
    function DialogChar(CharCode: Word): Boolean; override;
    procedure DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure ToggleControl; override;
    procedure UpdateProps; override;
    property Alignment default taLeftJustify;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXLabelItemViewer = class(TSpTBXItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    function DoExecute: Boolean; override;
  end;

  TSpTBXLabelItem = class(TSpTBXCustomLabelItem)
  published
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property FontSettings;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    property Visible;
    property OnAdjustFont;
    property OnClick;
    // TSpTBXCustomItem properties
    property Alignment;
    property Anchored;
    property CaptionGlow;
    property CaptionGlowColor;
    property Control;
    property CustomWidth;
    property CustomHeight;
    property Margins;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXSeparatorItem }

  TSpTBXSeparatorItem = class(TTBXSeparatorItem)
  public
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TSpTBXSeparatorItemViewer = class(TTBXSeparatorItemViewer)
  protected
    function IsStatusBarSeparator(out T: TSpTBXStatusToolbar): Boolean;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
  end;

  { TSpTBXRightAlignSpacerItem }

  TSpTBXRightAlignSpacerItem = class(TSpTBXCustomLabelItem)
  published
    property ImageIndex;
    property Images;
    property FontSettings;
    property MaskOptions;
    property Options;
    property OnAdjustFont;
    property OnClick;
    // TSpTBXCustomItem properties
    property Alignment;
    property CaptionGlow;
    property CaptionGlowColor;
    property CustomWidth;
    property CustomHeight;
    property OnDrawCaption;
    property OnDrawHint;
    property OnDrawImage;
    property OnDrawItem;
  end;

  { TSpTBXRadioGroupItem }

  TSpTBXRadioGroupItem = class(TTBGroupItem)
  private
    FDefaultIndex: Integer;
    FLastClickedIndex: Integer;
    FOnClick: TNotifyEvent;
    FOnGetCaption: TSpTBXGetRadioGroupCaptionEvent;
    FOnUpdate: TNotifyEvent;
  protected
    FStrings: TTntStringList;
    procedure Loaded; override;
    procedure ItemClickEvent(Sender: TObject); virtual;
    procedure DoClick(AItem: TSpTBXItem); virtual;
    function DoGetItemCaption(ACaption: WideString): WideString; virtual;
    procedure FillStringList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Recreate; virtual;
    property DefaultIndex: Integer read FDefaultIndex write FDefaultIndex;
    property LastClickedIndex: Integer read FLastClickedIndex;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnGetCaption: TSpTBXGetRadioGroupCaptionEvent read FOnGetCaption write FOnGetCaption;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  { TSpTBXThemeGroupItem }

  TSpTBXThemeGroupItem = class(TSpTBXRadioGroupItem)
  private
    FSkinDir: WideString;
    FOnThemeChange: TNotifyEvent;
    procedure SetSkinDir(Value: WideString);
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    procedure DoClick(AItem: TSpTBXItem); override;
    function DoGetItemCaption(ACaption: WideString): WideString; override;
    procedure DoThemeChange; virtual;
    procedure FillStringList; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SkinDir: WideString read FSkinDir write SetSkinDir;
  published
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

  { TSpTBXItemCacheList }

  TSpTBXItemCache = class(TCollectionItem)
  private
    FName: TComponentName;
    FItem: TTBCustomItem;
    FLeft, FTop, FWidth, FHeight: Integer;
    FParentWidth, FParentHeight: Integer;
    function GetName: TComponentName;
  public
    procedure Assign(Source: TPersistent); override;
    property Item: TTBCustomItem read FItem write FItem;
  published
    property Name: TComponentName read GetName write FName;
    property Left: Integer read FLeft write FLeft default 0;
    property Top: Integer read FTop write FTop default 0;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property ParentWidth: Integer read FParentWidth write FParentWidth default 0;
    property ParentHeight: Integer read FParentHeight write FParentHeight default 0;
  end;

  TSpTBXItemCacheCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TSpTBXItemCache;
    procedure SetItem(Index: Integer; const Value: TSpTBXItemCache);
  public
    function Add: TSpTBXItemCache; overload; virtual;
    function Add(Item: TTBCustomItem; RBounds: TRect): Integer; overload;
    function IndexOf(Item: TTBCustomItem): Integer;
    property Items[Index: Integer]: TSpTBXItemCache read GetItem write SetItem; default;
  end;

  { TSpTBXDock }

  TSpTBXDock = class(TTBXDock)
  private
    FOnDrawBackground: TSpTBXDrawEvent;
    FPrevWidth: Integer;
    FPrevHeight: Integer;
  protected
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    function ThemedBackground: Boolean; override;
    procedure Resize; override;
  public
    procedure DrawBackground(DC: HDC; const DrawRect: TRect); override;
    function DrawDockBackground(DC: HDC; const DrawRect: TRect): Boolean;
    property PrevWidth: Integer read FPrevWidth;
    property PrevHeight: Integer read FPrevHeight;
  published
    property OnCanResize;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  end;

  TSpTBXDockClass = class of TSpTBXDock;

  { TSpTBXToolbar }

  TSpTBXToolbarView = class(TTBXToolbarView)
  private
    FMaxSize: Integer;
    procedure SetMaxSize(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property MaxSize: Integer read FMaxSize write SetMaxSize;
  end;

  TSpTBXToolbar = class(TTBXToolbar)
  private
    FChevronVertical: Boolean;
    FCompoundToolbar: Boolean;
    FCustomizable: Boolean;
    FCustomizingCount: Integer;
    FItemMovingCount: Integer;
    FDisplayMode: TSpTBXToolbarDisplayMode;
    FIsResizing: Boolean;
    FLastDropMark: TRect;
    FLastSelectableWidth: Integer;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnItemNotification: TSpTBXItemNotificationEvent;
    function IsCaptionStored: Boolean;
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    procedure SetDisplayMode(const Value: TSpTBXToolbarDisplayMode);
    function GetMaxSize: Integer;
    procedure SetMaxSize(const Value: Integer);
    function CreateWrapper(Index: Integer; Ctl: TControl): TTBControlItem;
    function IsAnchoredControlItem(Item: TTBCustomItem): TTBControlItem;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    FBeginDragIV: TTBItemViewer;
    FAnchoredControlItems: TSpTBXItemCacheCollection;

    // Component
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    procedure Resize; override;

    // Hints
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    // Customizer
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure BeginCustomize;
    procedure EndCustomize;
    procedure BeginItemMove;
    procedure EndItemMove;

    // Get class
    function GetChevronItemClass: TTBChevronItemClass; override;
    function GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;
    function GetRightAlignMargin: Integer; virtual;
    function GetViewClass: TTBToolbarViewClass; override;

    // Misc
    procedure AnchorItems(UpdateControlItems: Boolean = True); virtual;
    procedure RightAlignItems; virtual;
    function CanItemClick(Item: TTBCustomItem; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DoItemClick(Item: TTBCustomItem; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoItemNotification(Ancestor: TTBCustomItem; Relayed: Boolean;
      Action: TTBItemChangedAction; Index: Integer; Item: TTBCustomItem); virtual;

    property CompoundToolbar: Boolean read FCompoundToolbar write FCompoundToolbar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
    function IsCustomizing: Boolean;
    function IsItemMoving: Boolean;
    property IsResizing: Boolean read FIsResizing;
    property MaxSize: Integer read GetMaxSize write SetMaxSize default -1;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored; // Hides the inherited Caption
    property Hint: WideString read GetHint write SetHint stored False; // Hint is set dynamically in MouseMove, don't save it
    property Customizable: Boolean read FCustomizable write FCustomizable default True;
    property ChevronVertical: Boolean read FChevronVertical write FChevronVertical default True;
    property DisplayMode: TSpTBXToolbarDisplayMode read FDisplayMode write SetDisplayMode default tbdmSelectiveCaption;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnItemNotification: TSpTBXItemNotificationEvent read FOnItemNotification write FOnItemNotification;
  end;

  TSpTBXToolbarClass = class of TSpTBXToolbar;

  { TSpTBXFloatingWindowParent }

  TSpTBXFloatingWindowParent = class(TTBXFloatingWindowParent)
  private
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
  protected
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat); override;
    procedure VisibleChanging; override;
  end;

  { TSpTBXPopupWindow }

  TSpTBXPopupWindow = class(TTBXPopupWindow)
  protected
    function GetViewClass: TTBViewClass; override;
  public
    constructor CreatePopupWindow(AOwner: TComponent; const AParentView: TTBView;
      const AItem: TTBCustomItem; const ACustomizing: Boolean); override;
    destructor Destroy; override;
  end;

  { TSpTBXPopupWindowView }

  TSpTBXPopupWindowView = class(TTBXPopupView)
  public
    procedure SetIsToolbar(const Value: Boolean);
  published
    property IsToolbar;
  end;

  { TSpTBXChevronItem }

  TSpTBXChevronItem = class(TTBXChevronItem)
  public
    function GetPopupWindowClass: TTBPopupWindowClass; override;
  end;

  TSpTBXChevronPopupWindow = class(TTBXChevronPopupWindow)
  private
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    function GetViewClass: TTBViewClass; override;
  end;

  { TSpTBXPopupMenu }

  ISpTBXPopupMenu = interface
    ['{C576A225-6E42-49F6-96E5-712510C5D85C}']
    function InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
      PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): Boolean;
  end;

  TSpTBXPopupMenu = class(TTBXPopupMenu, ISpTBXPopupMenu)
  private
    FOnInitPopup: TSpTBXPopupEvent;
  protected
    procedure DoInitPopup(PopupView: TTBView); virtual;
    function InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
      PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): Boolean; virtual;
  public
    procedure Popup(X: Integer; Y: Integer); override;
    function PopupEx(X, Y: Integer; PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): TTBCustomItem; virtual;
  published
    property OnInitPopup: TSpTBXPopupEvent read FOnInitPopup write FOnInitPopup;
  end;

  { TSpTBXCompoundItemsControl }

  TSpTBXCompoundItemsControl = class(TCustomControl, ITBItems)
  private
    FThemeType: TSpTBXThemeType;
    procedure DockRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean);
    function GetItems: TTBCustomItem;  // For ITBItems interface
    function GetRootItems: TTBRootItem;
    function GetView: TSpTBXToolbarView;
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetThemeType(const Value: TSpTBXThemeType);
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    FDock: TSpTBXDock;
    FToolbar: TSpTBXToolbar;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDockClass: TSpTBXDockClass; virtual;
    function GetToolbarClass: TSpTBXToolbarClass; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // For ITBItems interface
    procedure InvalidateDockBackground(Resizing: Boolean = False); virtual;
    procedure Loaded; override;
    property Images: TCustomImageList read GetImages write SetImages;
    property ThemeType: TSpTBXThemeType read FThemeType write SetThemeType default thtTBX;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property View: TSpTBXToolbarView read GetView;
  published
    property Items: TTBRootItem read GetRootItems;
  end;

  { TSpTBXCompoundBar }

  TSpTBXCompoundBar = class(TSpTBXCompoundItemsControl)
  private
    FOnDrawDockBackground: TSpTBXDrawEvent;
    procedure DrawDockBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure DockResize(Sender: TObject);
  protected
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    property OnDrawDockBackground: TSpTBXDrawEvent read FOnDrawDockBackground write FOnDrawDockBackground;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSpTBXButtonOptions }

  TSpTBXButtonOptions = class(TPersistent)
  private
    FCloseButton: TSpTBXItem;
    FMinimizeButton: TSpTBXItem;
    FMaximizeButton: TSpTBXItem;
    FRightAlignSpacer: TSpTBXRightAlignSpacerItem;
    FCaptionImageIndex: Integer;
    FCloseImageIndex: Integer;
    FMinimizeImageIndex: Integer;
    FMaximizeImageIndex: Integer;
    FRestoreImageIndex: Integer;
    FCaptionLabel: WideString;
    FCaption: Boolean;
    FClose: Boolean;
    FMinimize: Boolean;
    FMaximize: Boolean;
    FButtonBorders: Boolean;
    FTitleBarSize: Integer;
    procedure SetCaptionImageIndex(Value: Integer);
    procedure SetCloseImageIndex(Value: Integer);
    procedure SetMaximizeImageIndex(Value: Integer);
    procedure SetRestoreImageIndex(Value: Integer);
    procedure SetMinimizeImageIndex(Value: Integer);
    procedure SetCaption(const Value: Boolean);
    procedure SetClose(const Value: Boolean);
    procedure SetMaximize(const Value: Boolean);
    procedure SetMinimize(const Value: Boolean);
    procedure SetTitleBarSize(const Value: Integer);
    procedure SetCaptionLabel(const Value: WideString);
  protected
    FParentControl: TWinControl;
    FToolbar: TSpTBXToolbar;
    procedure ButtonsDrawImage(Sender: TObject; ACanvas: TCanvas;
      const ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
      var AImageList: TCustomImageList; var AImageIndex: Integer; var ARect:
      TRect; var PaintDefault: Boolean); virtual;
    procedure ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); virtual;
    procedure ButtonsClick(Sender: TObject); virtual; abstract;
    procedure CreateButtons; virtual;
    procedure UpdateButtonsVisibility; virtual;
    procedure SetupButton(B: TSpTBXCustomItem); virtual;
    function Restoring(B: TSpTBXCustomItem): Boolean; virtual; abstract;
  public
    constructor Create(AParent: TWinControl); virtual;
    procedure ReorderButtons; virtual;
    procedure SetupButtonIcon(B: TSpTBXCustomItem); virtual;
    property RightAlignSpacer: TSpTBXRightAlignSpacerItem read FRightAlignSpacer;
    property MinimizeButton: TSpTBXItem read FMinimizeButton;
    property MaximizeButton: TSpTBXItem read FMaximizeButton;
    property CloseButton: TSpTBXItem read FCloseButton;
    property CaptionLabel: WideString read FCaptionLabel write SetCaptionLabel;
  published
    property ButtonBorders: Boolean read FButtonBorders write FButtonBorders default False;
    property Caption: Boolean read FCaption write SetCaption default True;
    property Close: Boolean read FClose write SetClose default True;
    property Minimize: Boolean read FMinimize write SetMinimize default True;
    property Maximize: Boolean read FMaximize write SetMaximize default True;
    property CaptionImageIndex: Integer read FCaptionImageIndex write SetCaptionImageIndex default -1;
    property CloseImageIndex: Integer read FCloseImageIndex write SetCloseImageIndex default -1;
    property MinimizeImageIndex: Integer read FMinimizeImageIndex write SetMinimizeImageIndex default -1;
    property MaximizeImageIndex: Integer read FMaximizeImageIndex write SetMaximizeImageIndex default -1;
    property RestoreImageIndex: Integer read FRestoreImageIndex write SetRestoreImageIndex default -1;
    property TitleBarSize: Integer read FTitleBarSize write SetTitleBarSize default 17;
  end;

  { TSpTBXStatusBar }

  TSpTBXStatusToolbar = class(TSpTBXToolbar)
  private
    FSizeGrip: Boolean;
    FThemeType: TSpTBXThemeType;
    procedure SetSizeGrip(const Value: Boolean);
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
  protected
    FParentForm: TCustomForm;
    FUsingOfficeTheme: Boolean;
    function IsPointInGrip(P: TPoint): Boolean;
    function GetRightAlignMargin: Integer; override;
    function GetParentFormWindowState: TWindowState;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetGripRect: TRect;
    function NeedsSeparatorRepaint: Boolean;
  published
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
  end;

  TSpTBXCustomStatusBar = class(TSpTBXCompoundBar)
  private
    function GetSizeGrip: Boolean;
    procedure SetSizeGrip(const Value: Boolean);
    function GetStatusToolbar: TSpTBXStatusToolbar;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FPrevState: TWindowState;
    function CanResize(var NewWidth: Integer; var NewHeight: Integer): Boolean; override;
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DrawSeparators(ACanvas: TCanvas; ARect, AGripRect: TRect); virtual;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    property Align default alBottom;
    property SizeGrip: Boolean read GetSizeGrip write SetSizeGrip default True;
  public
    constructor Create(AOwner: TComponent); override;
    property Toolbar: TSpTBXStatusToolbar read GetStatusToolbar;
  end;

  TSpTBXStatusBar = class(TSpTBXCustomStatusBar)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
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
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    // TSpTBXCustomStatusBar properties
    property Images;
    property SizeGrip;
    property ThemeType;
    property OnDrawDockBackground;
  end;

  { TSpTBXTitleBar }

  TSpTBXCustomTitleBar = class;

  TSpTBXTitleToolbar = class(TSpTBXToolbar)
  protected
    function GetTitleBar: TSpTBXCustomTitleBar;
    function GetRightAlignMargin: Integer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSpTBXTitleBarButtonOptions = class(TSpTBXButtonOptions)
  private
    FSystemMenu: Boolean;
    FSystemButton: TSpTBXItem;
    FSystemImageIndex: Integer;
    FSystemMenuIcon: TIcon;
    procedure SetSystemImageIndex(Value: Integer);
    procedure SetSystemMenu(const Value: Boolean);
  protected
    FTitleBar: TSpTBXCustomTitleBar;
    procedure ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean); override;
    procedure ButtonsClick(Sender: TObject); override;
    procedure CreateButtons; override;
    function Restoring(B: TSpTBXCustomItem): Boolean; override;
  public
    constructor Create(AParent: TWinControl); override;
    destructor Destroy; override;
    procedure ReorderButtons; override;
    procedure SetupButtonIcon(B: TSpTBXCustomItem); override;
    property SystemButton: TSpTBXItem read FSystemButton;
  published
    property SystemMenu: Boolean read FSystemMenu write SetSystemMenu default True;
    property SystemImageIndex: Integer read FSystemImageIndex write SetSystemImageIndex default -1;
  end;

  TSpTBXCustomTitleBar = class(TSpTBXCompoundBar)
  private
    FOptions: TSpTBXTitleBarButtonOptions;
    FGradientB: TBitmap;
    FTBXStyleBackground: Boolean;
    FFixedSize: Boolean;
    FFullScreenMaximize: Boolean;
    FActive: Boolean;
    FOnDrawBackground: TSpTBXDrawEvent;
    FOnSystemMenuPopup: TSpTBXPopupEvent;
    FOldParentFormWndProc: TWndMethod;
    procedure NewParentFormWndProc(var Message: TMessage);
    procedure SetActive(const Value: Boolean);
    procedure SetFullScreenMaximize(const Value: Boolean);
    procedure SetTBXStyleBackground(const Value: Boolean);
    function GetWindowState: TWindowState;
    procedure SetWindowState(const Value: TWindowState);
    function GetCaption: TWideCaption;
    procedure SetCaption(const Value: TWideCaption);
    function IsCaptionStored: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  protected
    FBorderWidth: Integer;
    FParentForm: TCustomForm;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure ChangeTitleBarState(Activate: Boolean);
    procedure DoDrawDockBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    procedure GetSizeCursor(MousePos: TPoint; var SizeCursor, SizeCode: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    property Active: Boolean read FActive write SetActive default True;
    property Align default alClient;
    property FixedSize: Boolean read FFixedSize write FFixedSize default False;
    property FullScreenMaximize: Boolean read FFullScreenMaximize write SetFullScreenMaximize default False;
    property Options: TSpTBXTitleBarButtonOptions read FOptions write FOptions;
    property TBXStyleBackground: Boolean read FTBXStyleBackground write SetTBXStyleBackground default False;
    property WindowState: TWindowState read GetWindowState write SetWindowState;
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
    property OnSystemMenuPopup: TSpTBXPopupEvent read FOnSystemMenuPopup write FOnSystemMenuPopup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateBackground(InvalidateChildren: Boolean = True);
    procedure ShowSystemMenu(ScreenPos: TPoint); virtual;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored; // Hides the inherited Caption
  end;

  TSpTBXTitleBar = class(TSpTBXCustomTitleBar)
  published
    property Align;
    property Anchors;
    property Color;
    property BiDiMode;
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
    // TSpTBXCustomTitleBar properties
    property Active;
    property FixedSize;
    property FullScreenMaximize;
    property Images;
    property Options;
    property TBXStyleBackground;
    property OnDrawBackground;
    property OnDrawDockBackground;
    property OnSystemMenuPopup;
  end;

  { ISpTBXSplitter Interface }

  ISpTBXSplitter = interface
    ['{8DCE14EF-807F-4720-B28E-151B49F38D41}']
    function ValidateSplitControl(var SplitControl: TControl): Boolean;
    procedure Minimize;
    procedure Restore;
    procedure Toggle;
  end;

  { TBitmapHint }

  TBitmapHint = class(THintWindow)
  private
    FHintBitmap: TBitmap;
    FActivating: Boolean;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Paint; override;
  public
    property Activating: Boolean read FActivating;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); override;
  end;

  { Skins }

  TSpTBXSkinSingleStateComponentsType = (sknssDock, sknssToolbar, sknssPopup, sknssTitleBar, sknssWindowFrames, sknssStatusBar);
  TSpTBXSkinMultiStateComponentsType = (sknmsToolbarItem, sknmsMenuItem);

  TSpTBXSkinStatesType = (sknsNormal, sknsDisabled, sknsHotTrack, sknsPushed, sknsChecked);
  TSpTBXSkinPartsType = (sknpBody, sknpBorders, sknpInternalBorders);

  TSpTBXDefaultTheme = class(TTBXDefaultTheme)
  public
    // Abstract methods
    procedure PaintTitleBarNCArea(Canvas: TCanvas; R: TRect; IsVertical, IsFloating, CloseButtonVisible, CloseButtonPushed, CloseButtonHot: Boolean; Caption: WideString; CaptionBarSize: Integer; BorderSize: Integer = 2); virtual; abstract;
  end;

  TSpTBXSkinOptionEntry = class(TPersistent)
  private
    FEntryType: Integer;
    FColor1, FColor2, FColor3, FColor4: TColor;
  public
    constructor Create; virtual;
    procedure ReadFromString(S: string);
    function WriteToString: string;
    function IsEmpty: Boolean;
    procedure Reset;
  published
    property EntryType: Integer read FEntryType write FEntryType;
    property Color1: TColor read FColor1 write FColor1;
    property Color2: TColor read FColor2 write FColor2;
    property Color3: TColor read FColor3 write FColor3;
    property Color4: TColor read FColor4 write FColor4;
  end;

  TSpTBXSkinOptionCategory = class(TPersistent)
  private
    FImage: TPicture;
    FBody: TSpTBXSkinOptionEntry;
    FBorders: TSpTBXSkinOptionEntry;
    FInternalBorders: TSpTBXSkinOptionEntry;
    FEffectiveColor: TColor;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsEmpty: Boolean;
    procedure Reset;
    procedure LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
    procedure SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
  published
    property Body: TSpTBXSkinOptionEntry read FBody write FBody;
    property Borders: TSpTBXSkinOptionEntry read FBorders write FBorders;
    property InternalBorders: TSpTBXSkinOptionEntry read FInternalBorders write FInternalBorders;
    property EffectiveColor: TColor read FEffectiveColor write FEffectiveColor;
    property Image: TPicture read FImage write FImage;
  end;

  TSpTBXSkinOptions = class(TPersistent)
  private
    FSingleStateOptions: array [TSpTBXSkinSingleStateComponentsType] of TSpTBXSkinOptionCategory;
    FMultiStateOptions: array [TSpTBXSkinMultiStateComponentsType, TSpTBXSkinStatesType] of TSpTBXSkinOptionCategory;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsActive: Boolean;
    function GetTheme: TSpTBXDefaultTheme;
    procedure LoadFromFile(Filename: WideString);
    procedure SaveToFile(Filename: WideString);
    procedure Reset;
    function Options(Component: TSpTBXSkinSingleStateComponentsType): TSpTBXSkinOptionCategory; overload;
    function Options(Component: TSpTBXSkinMultiStateComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory; overload;
  end;

{ Item helpers }
procedure SpFillItemInfo(AEnabled, APushed, AMouseInControl, AChecked: Boolean; var ItemInfo: TTBXItemInfo); overload;
procedure SpFillItemInfo(ItemViewer: TTBItemViewer; var ItemInfo: TTBXItemInfo); overload;
function SpGetBoundsRect(IV: TTBItemViewer; Root: TTBRootItem): TRect;
procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TStringList; ClearFirst: Boolean = True); overload;
procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TTntStringList; ClearFirst: Boolean = True); overload;
function SpGetItemViewerFromPoint(Root: TTBRootItem; View: TTBView; P: TPoint; ProcessGroupItems: Boolean = True): TTBItemViewer;
function SpGetNextItem(AItemViewer: TTBItemViewer; GoForward, Visible, Showed, SameLevel: Boolean; Inmediate: Boolean = False; ACallBack: TSpTBXGetNextCallBack = nil): TTBItemViewer;
function SpGetFirstRightAlignSpacer(View: TTBView): TSpTBXItemViewer;
function SpGetRightAlignedItems(View: TTBView; RightAlignedList: TList; IsRotated: Boolean; out VisibleTotalWidth, RightAlignedTotalWidth: Integer): TSpTBXItemViewer;
procedure SpInvalidateItem(View: TTBView; Item: TTBCustomItem);
function SpFindItemViewer(View: TTBView; Item: TTBCustomItem): TTBItemViewer;
function SpFindControlItem(Item: TTBCustomItem; Ctl: TControl): TTBControlItem;
procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint; out DestIV: TTBItemViewer; out DestItemPos: Integer; out DropMark: TRect); overload;
procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint; SourceItemPos: Integer; out DestIV: TTBItemViewer; out DestItemPos: Integer); overload;
procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);

{ Painting helpers }
function SpXPThemesAvailable: Boolean;
function SpXPThemeType(T: TSpTBXThemeType): TSpTBXThemeType;
procedure SpChangeThemeType(Control: TWinControl; TM: TSpTBXThemeType; Recursive: Boolean = True);
procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect; DeltaX: Integer = 0; DeltaY: Integer = 0);
procedure SpDrawArrow(DC: HDC; X, Y: Integer; AColor: TColor; Vertical: Boolean; Size: Integer =3);
procedure SpDrawFocusRect(ACanvas: TCanvas; const R: TRect);
procedure SpHighlightBitmap(ACanvas: TCanvas; X, Y: Integer; Bitmap: TBitmap; TransparentColor, HighlightColor: TColor; Amount: Byte);
procedure SpDrawMDIButton(ACanvas: TCanvas; X, Y: Integer; ButtonKind: Cardinal);
procedure SpDrawXPBeveledButton(ACanvas: TCanvas; ARect: TRect; Pushed: Boolean; BaseColor: TColor);
procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, MouseInControl, Checked, Focused, Defaulted: Boolean; ThemeType: TSpTBXThemeType; PaintType: TSpTBXButtonPaintType = bptDefault);
procedure SpDrawXPRectButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, MouseInControl, Checked, Focused, Defaulted: Boolean; ThemeType: TSpTBXThemeType; PaintType: TSpTBXButtonPaintType = bptDefault; BorderColor: TColor = clNone);
procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean; State: TCheckBoxState; MouseInControl, Pushed, Focused: Boolean; ThemeType: TSpTBXThemeType);
procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled, Checked, MouseInControl, Pushed, Focused: Boolean; ThemeType: TSpTBXThemeType);
procedure SpDrawXPStatusBar(ACanvas: TCanvas; ARect, AGripRect: TRect; ThemeType: TSpTBXThemeType; UsingOfficeTheme: Boolean);
procedure SpDrawXPGradientTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean);
procedure SpDrawXPTitleBar(WindowHandle: THandle; ACanvas: TCanvas; ARect: TRect; TitleBarWidth, TitleBarHeight: Integer; IsActive, TBXStyleBackground: Boolean);
procedure SpDrawXPDockablePanelTitleBar(WindowHandle: THandle; ACanvas: TCanvas; X, Y, AWidth, AHeight: Integer; IsActive, IsFloating: Boolean);
procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean; ThemeType: TSpTBXThemeType; ClipContent: Boolean = False); overload;
procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean; ThemeType: TSpTBXThemeType); overload;
procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; Selected, Pushed, Focused: Boolean; ThemeType: TSpTBXThemeType);
procedure SpDrawXPIconSelection(ACanvas: TCanvas; ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer);
procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; Centered, Vertical: Boolean);
function SpIsEmptyColor(C: TColor): Boolean;

{ WideString helpers }
function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
function SpDrawRotatedText(const DC: HDC; AText: WideString; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionAlignment: TAlignment; Flags: Cardinal; GlyphWidth, GlyphHeight: Integer; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect: TRect; RotationAngle: TSpTextRotationAngle = tra0): Integer;
function SpDrawXPText(ACanvas: TCanvas; Caption: WideString; var ARect: TRect; Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone; CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; Glyph: TBitmap; Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect: TRect; CalcRectOnly: Boolean = False; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString; CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment; Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout; Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect; CalcRectOnly: Boolean = False; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
function SpGetTextSize(DC: HDC; WS: WideString; NoPrefix: Boolean): TSize;
function SpSameText(W1, W2: WideString): Boolean;
function SpStripAccelChars(S: WideString): WideString;
function SpStripShortcut(S: WideString): WideString;
function SpStripTrailingPunctuation(S: WideString): WideString;
function SpRectToString(R: TRect): string;
function SpStringToRect(S: string; out R: TRect): Boolean;
function SpColorToString(C: TColor): string;
function SpGetDirectories(Path: WideString; L: TTntStrings): Boolean;

{ Menu helpers }
function SpCalcPopupPosition(const X, Y, Width, Height: Integer; PopupControl: TControl = nil; IsVertical: Boolean = False): TPoint;
function SpHMenuGetCaption(Menu: HMenu; Index: Integer): WideString;
function SpHMenuToTBMenuItem(Menu: HMenu; ParentItem: TTBCustomItem): Boolean;
function SpShowSystemPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; DoDefault: Boolean = True): Integer;
function SpFillSystemSpTBXPopupMenu(ParentForm: TCustomForm; ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean; OutPopup: TSpTBXPopupMenu): Boolean;
function SpShowSystemSpTBXPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean; PopupEvent: TSpTBXPopupEvent; DoDefault: Boolean = True): Integer;

{ Misc helpers }
function SpCanFocus(WinControl: TWinControl): Boolean;
function SpIsFocused(WinControl: TWinControl; out FocusedChild: TWinControl): Boolean;
function SpFocusFirstChild(WinControl: TWinControl): TWinControl;
function SpFindControl(Parent: TWinControl; Child: TControl): Integer;
function SpFindParent(Control: TControl; ParentClass: TClass): TWinControl;
function SpGetFormWindowState(F: TCustomForm; out RestoreBoundsRect: TRect): TWindowState;
procedure SpSetFormWindowState(F: TCustomForm; WindowState: TWindowState; RestoreBoundsRect: TRect);
function SpGetTaskBar(out State, Edge: Cardinal; out Bounds: TRect): Boolean;
procedure SpCustomizeAllToolbars(AParentComponent: TComponent; Reset: Boolean);
procedure SpBeginUpdateAllToolbars(AParentComponent: TComponent);
procedure SpEndUpdateAllToolbars(AParentComponent: TComponent);

var
  SP_TAB_THEME: THandle;
  SP_PROGRESS_THEME: THandle;
  SP_TRACKBAR_THEME: THandle;
  SpStockHintBitmap: TBitmap;
  SpStockHandCursor: HCURSOR;
  CurrentSkin: TSpTBXSkinOptions = nil;
  MDIButtonsImgList: TImageList = nil;

const
  crSpTBXCustomization = 101;
  crSpTBXCustomizationCancel = 102;

implementation

{$R SpTBXGlyphs.res}

uses
  TBXUxThemes, TBXUtils, TypInfo, ComCtrls, CommCtrl, ShellApi,
  TntWindows, TntSysUtils, TntActnList, TntForms, Types;

const
  SSpTBXCustomizerRepeatedInstance = 'There''s already another instance of TSpTBXCustomizer';
  SSpTBXCustomizerInvalidParent = 'TSpTBXCustomizer must be dropped only on a Form or on a Frame';
  SSpTBXSkinSingleStateComponentsString: array [TSpTBXSkinSingleStateComponentsType] of string = ('Dock', 'Toolbar', 'Popup', 'TitleBar', 'WindowFrames', 'StatusBar');
  SSpTBXSkinMultiStateComponentsString: array [TSpTBXSkinMultiStateComponentsType] of string = ('ToolbarItem', 'MenuItem');
  SSpTBXSkinStatesString: array [TSpTBXSkinStatesType] of string = ('Normal', 'Disabled', 'HotTrack', 'Pushed', 'Checked');

  { Constants for TSpTBXThemeGroupItem. Do not localize! }
  rvThemeNamePrefix = 'SpTBXTheme:';
  rvSkinIniFilename = 'Skin.Ini';

type
  TTBXRootItemAccess = class(TTBXRootItem);
  TTBCustomItemAccess = class(TTBCustomItem);
  TTBXItemAccess = class(TTBXCustomItem);
  TTBXItemViewerAccess = class(TTBXItemViewer);
  TTBViewAccess = class(TTBView);
  TDockAccess = class(TTBDock);
  TTBCustomDockableWindowAccess = class(TTBCustomDockableWindow);
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TCustomFormAccess = class(TCustomForm);

  // TBXThemes.TBXThemeManager loads/unloads the XP theme library and data parts.
  // The problem is that it doesn't load the Tab data parts, because TBX doesn't
  // need it. For that we need to load Tab parts (declared in TBXUxThemes unit),
  // and load/unload the theme library OUTSIDE of the TBXThemeManager, because
  // some of its properties may unload the theme library for example
  // EnableVisualStyles.
  // Fortunately we can use the TBXThemeManager notifications to load/unload
  // the Tab parts.

  { TSpTabThemeNexus }

  TSpThemeNexus = class
  private
    FUXThemeDllHandle: THandle;
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
    procedure OpenData;
    procedure CloseData;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  SpThemeNexus: TSpThemeNexus;
  SpOpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
  SpCloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTabThemeNexus }

constructor TSpThemeNexus.Create;
begin
  inherited;
  SP_TAB_THEME := 0;
  SP_PROGRESS_THEME := 0;
  FUXThemeDllHandle := LoadLibrary('uxtheme.dll');
  if FUXThemeDllHandle > 0 then begin
    SpOpenThemeData := GetProcAddress(FUXThemeDllHandle, 'OpenThemeData');
    SpCloseThemeData := GetProcAddress(FUXThemeDllHandle, 'CloseThemeData');
  end;
  OpenData;
  AddTBXSysChangeNotification(Self);
end;

destructor TSpThemeNexus.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  CloseData;
  if FUXThemeDllHandle <> 0 then begin
    FreeLibrary(FUXThemeDllHandle);
    FUXThemeDllHandle := 0;
    SpOpenThemeData := nil;
    SpCloseThemeData := nil;
  end;
  inherited;
end;

procedure TSpThemeNexus.OpenData;
begin
  if FUXThemeDllHandle > 0 then begin
    SP_TAB_THEME := SpOpenThemeData(Application.Handle, 'TAB');
    SP_PROGRESS_THEME := SpOpenThemeData(Application.Handle, 'PROGRESS');
    SP_TRACKBAR_THEME := SpOpenThemeData(Application.Handle, 'TRACKBAR');
  end;
end;

procedure TSpThemeNexus.CloseData;

  procedure Close(var ATheme: THandle);
  begin
    if ATheme <> 0 then begin
      SpCloseThemeData(ATheme);
      ATheme := 0;
    end;
  end;

begin
  if FUXThemeDllHandle > 0 then begin
    Close(SP_TAB_THEME);
    Close(SP_PROGRESS_THEME);
    Close(SP_TRACKBAR_THEME);
  end;
end;

procedure TSpThemeNexus.TBXSysCommand(var Message: TMessage);
begin
  if Message.Msg = TBX_SYSCOMMAND then
    case Message.WParam of
      TSC_BEFOREVIEWCHANGE:
        CloseData;
      TSC_AFTERVIEWCHANGE:
        OpenData;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Item Helpers }

procedure SpFillItemInfo(AEnabled, APushed, AMouseInControl, AChecked: Boolean; var ItemInfo: TTBXItemInfo);
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  with ItemInfo do begin
    ItemInfo.ViewType := TVT_NORMALTOOLBAR;
    ItemInfo.ItemOptions := IO_TOOLBARSTYLE or IO_APPACTIVE;
    ItemInfo.Enabled := AEnabled;
    ItemInfo.Pushed := APushed and AMouseInControl;
    if AMouseInControl then
      ItemInfo.HoverKind := hkMouseHover
    else
      ItemInfo.HoverKind := hkNone;
    ItemInfo.Selected := AChecked;
    IsVertical := False;
    ItemInfo.ComboPart := cpNone;
    IsPopupParent := False;

    ItemInfo.ImageShown := False;
    ItemInfo.ImageWidth := 0;
    ItemInfo.ImageHeight := 0;
  end;
end;

procedure SpFillItemInfo(ItemViewer: TTBItemViewer; var ItemInfo: TTBXItemInfo);
const
  CToolbarStyle: array [Boolean] of Integer = (0, IO_TOOLBARSTYLE);
  CCombo: array [Boolean] of Integer = (0, IO_COMBO);
  CSubmenuItem: array [Boolean] of Integer = (0, IO_SUBMENUITEM);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
  CAppActive: array [Boolean] of Integer = (0, IO_APPACTIVE);
var
  Item: TTBXCustomItem;
  View: TTBViewAccess;

  ClientAreaRect: TRect;
  IsHoverItem, IsOpen, IsPushed: Boolean;
  ToolbarStyle: Boolean;
  HasArrow: Boolean;
  IsSplit: Boolean;
  ImageIsShown: Boolean;
  ImgSize: TSize;
  IsComboPushed: Boolean;
begin
  Item := TTBXCustomItem(ItemViewer.Item);
  View := TTBViewAccess(ItemViewer.View);

  ClientAreaRect := ItemViewer.BoundsRect;
  OffsetRect(ClientAreaRect, -ClientAreaRect.Left, -ClientAreaRect.Top);

  IsOpen := ItemViewer = View.OpenViewer;
  IsHoverItem := ItemViewer = View.Selected;
  IsPushed := IsHoverItem and (IsOpen or (View.MouseOverSelected and View.Capture));

  ToolbarStyle := ItemViewer.IsToolbarStyle;
  IsSplit := tbisCombo in TTBXItemAccess(Item).ItemStyle;
  IsComboPushed := IsSplit and IsPushed and not View.Capture;
  if IsComboPushed then IsPushed := False;

  if TTBXItemViewerAccess(ItemViewer).GetImageShown then
  begin
    ImgSize := TTBXItemViewerAccess(ItemViewer).GetImageSize;
    with ImgSize do if (CX <= 0) or (CY <= 0) then
    begin
      CX := 0;
      CY := 0;
      ImageIsShown := False;
    end
    else ImageIsShown := True;
  end
  else
  begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
    ImageIsShown := False;
  end;

  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.ViewType := GetViewType(View);
  ItemInfo.ItemOptions := CToolbarStyle[ToolbarStyle] or CCombo[IsSplit] or
    CDesigning[csDesigning in Item.ComponentState] or CSubmenuItem[tbisSubmenu in TTBXItemAccess(Item).ItemStyle] or
    CAppActive[Application.Active];
  ItemInfo.Enabled := Item.Enabled or View.Customizing;
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := Item.Checked;
  ItemInfo.ImageShown := ImageIsShown;
  ItemInfo.ImageWidth := ImgSize.CX;
  ItemInfo.ImageHeight := ImgSize.CY;
  if IsHoverItem then
  begin
    if not ItemInfo.Enabled and not View.MouseOverSelected then ItemInfo.HoverKind := hkKeyboardHover
    else if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  ItemInfo.IsPopupParent := ToolbarStyle and
    (((vsModal in View.State) and Assigned(View.OpenViewer)) or (tbisSubmenu in TTBXItemAccess(Item).ItemStyle)) and
    ((IsSplit and IsComboPushed) or (not IsSplit and IsPushed));
  ItemInfo.IsVertical := (View.Orientation = tbvoVertical) and not IsSplit;
//  if not ToolbarStyle then ItemInfo.PopupMargin := GetPopupMargin(Self);
  ItemInfo.PopupMargin := GetPopupMargin(ItemViewer);

  HasArrow := (tbisSubmenu in TTBXItemAccess(Item).ItemStyle) and
    ((tbisCombo in TTBXItemAccess(Item).ItemStyle) or (tboDropdownArrow in Item.EffectiveOptions));

  if ToolbarStyle then
  begin
    if HasArrow then
      ItemInfo.ComboPart := cpCombo;
    if IsSplit then
      ItemInfo.ComboPart := cpSplitLeft;
  end;
end;

function SpGetBoundsRect(IV: TTBItemViewer; Root: TTBRootItem): TRect;
var
  G: TTBItemViewer;
  V: TTBView;
  I, J: integer;
  R: TRect;
  FirstItemFound: Boolean;
begin
  Result := Rect(0, 0, 0, 0);
  if Assigned(IV) then
    if IV.Item is TTBGroupItem then begin
      // Sum all the ItemViewers of the GroupItem
      V := IV.View;
      J := IV.Index + 1;
      FirstItemFound := False;
      for I := J to V.ViewerCount - 1 do begin
        G := V.Viewers[I];
        if (G.Item.Parent = Root) then
          Break
        else
          if G.Item.Visible and not (G.Item is TTBGroupItem) then
            if not FirstItemFound then begin
              FirstItemFound := True;
              Result := G.BoundsRect;
            end
            else begin
              R := G.BoundsRect;
              Result.Left := Min(Result.Left, R.Left);
              Result.Top := Min(Result.Top, R.Top);
              Result.Right  := Max(Result.Right, R.Right);
              Result.Bottom := Max(Result.Bottom, R.Bottom);
            end;
      end;
    end
    else
      Result := IV.BoundsRect;
end;

procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TStringList; ClearFirst: Boolean = True);
// Returns a StringList with all the items, subitems and linked items from AParentItem.
// The ItemsList.Strings[] contains the items name
// The ItemsList.Objects[] contains the items reference

  procedure Iterate(AParentItem: TTBCustomItem; LinkDepth: Integer);
  var
    I: Integer;
    NewParentItem, Item: TTBCustomItem;
  begin
    NewParentItem := AParentItem;
    if Assigned(NewParentItem.LinkSubitems) then begin
      NewParentItem := NewParentItem.LinkSubitems;
      Inc(LinkDepth);
      if LinkDepth > 25 then
        Exit;  { prevent infinite link recursion }
    end;
    for I := 0 to NewParentItem.Count - 1 do begin
      Item := NewParentItem.Items[I];
      ItemsList.AddObject(Item.Name, Item);
      Iterate(Item, LinkDepth);
    end;
  end;

begin
  if ClearFirst then
    ItemsList.Clear;
  Iterate(AParentItem, 0);
end;

procedure SpGetAllItems(AParentItem: TTBCustomItem; ItemsList: TTntStringList; ClearFirst: Boolean = True);
// Returns a StringList with all the items, subitems and linked items from AParentItem.
// The ItemsList.Strings[] contains the items name
// The ItemsList.Objects[] contains the items reference

  procedure Iterate(AParentItem: TTBCustomItem; LinkDepth: Integer);
  var
    I: Integer;
    NewParentItem, Item: TTBCustomItem;
  begin
    NewParentItem := AParentItem;
    if Assigned(NewParentItem.LinkSubitems) then begin
      NewParentItem := NewParentItem.LinkSubitems;
      Inc(LinkDepth);
      if LinkDepth > 25 then
        Exit;  { prevent infinite link recursion }
    end;
    for I := 0 to NewParentItem.Count - 1 do begin
      Item := NewParentItem.Items[I];
      ItemsList.AddObject(Item.Name, Item);
      Iterate(Item, LinkDepth);
    end;
  end;

begin
  if ClearFirst then
    ItemsList.Clear;
  Iterate(AParentItem, 0);
end;

function SpGetItemViewerFromPoint(Root: TTBRootItem; View: TTBView; P: TPoint;
  ProcessGroupItems: Boolean = True): TTBItemViewer;
// Returns the ItemViewer at the given position
// If ProcessGroupItems is true and the ItemViewer is on a GroupItem return
// the GroupItem's ItemViewer instead.
var
  IV: TTBItemViewer;
  I, X: Integer;
  G: TTBItemViewer;
begin
  Result := nil;
  if Assigned(Root) and Assigned(View) then begin
    IV := View.ViewerFromPoint(P);

    // If the Item is not on the Root it must be part of a GroupItem
    if ProcessGroupItems and Assigned(IV) and not (IV.Item is TTBChevronItem) and (IV.Item.Parent <> Root) then begin
      // Get the parent GroupItem ItemViewer
      X := IV.Index;
      for I := X downto 0 do begin
        G := IV.View.Viewers[I];
        if G.Item is TTBGroupItem then begin
          Result := G;
          Break;
        end;
      end;
    end
    else
      Result := IV;
  end;
end;

function SpGetNextItem(AItemViewer: TTBItemViewer; GoForward, Visible, Showed,
  SameLevel: Boolean; Inmediate: Boolean = False;
  ACallBack: TSpTBXGetNextCallBack = nil): TTBItemViewer;

  function ProcessItemViewer(IV: TTBItemViewer; var EndProcess: Boolean): Boolean;
  begin
    Result := False;
    if Visible then
      Result := IV.Item.Visible = Visible;
    if SameLevel then
      Result := Result and (IV.OffEdge = AItemViewer.OffEdge) and (IV.BoundsRect.Top = AItemViewer.BoundsRect.Top);
    if Showed then
      Result := Result and IV.Show;
    if Assigned(ACallBack) then
      Result := ACallback(IV, Result, EndProcess);
  end;

var
  V: TTBView;
  I: Integer;
  EndProcess: Boolean;
begin
  Result := nil;
  EndProcess := false;
  V := AItemViewer.View;
  V.ValidatePositions;
  I := V.IndexOf(AItemViewer);

  while not Assigned(Result) do begin
    if GoForward then
      Inc(I)
    else
      Dec(I);
    if (I > V.ViewerCount - 1) or (I < 0) then
      Break
    else
      if ProcessItemViewer(V.Viewers[I], EndProcess) then
        Result := V.Viewers[I];
    if (Inmediate and not Assigned(Result)) or EndProcess then
      Break;
  end;
end;

function SpGetFirstRightAlignSpacer(View: TTBView): TSpTBXItemViewer;
var
  I: integer;
  IV: TTBItemViewer;
begin
  Result := nil;
  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV.Item.Visible and (IV.Item is TSpTBXRightAlignSpacerItem) then
      Result := IV as TSpTBXItemViewer;
  end;
end;

function SpGetRightAlignedItems(View: TTBView; RightAlignedList: TList;
  IsRotated: Boolean; out VisibleTotalWidth, RightAlignedTotalWidth: Integer): TSpTBXItemViewer;

  function GetWidth(R: TRect): Integer;
  begin
    if IsRotated then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end;

var
  I: Integer;
  IV: TTBItemViewer;
begin
  Result := nil;

  if Assigned(RightAlignedList) then
    RightAlignedList.Clear;
  VisibleTotalWidth := 0;
  RightAlignedTotalWidth := 0;

  for I := 0 to View.ViewerCount - 1 do begin
    IV := View.Viewers[I];
    if IV.Item.Visible then
      VisibleTotalWidth := VisibleTotalWidth + GetWidth(IV.BoundsRect);
    if not Assigned(Result) and (IV.Item.Visible) and (IV.Item is TSpTBXRightAlignSpacerItem) then
      Result := IV as TSpTBXItemViewer;
    if Assigned(Result) then begin
      if Assigned(RightAlignedList) then
        RightAlignedList.Add(IV);
      RightAlignedTotalWidth := RightAlignedTotalWidth + GetWidth(IV.BoundsRect);
    end;
  end;
end;

procedure SpInvalidateItem(View: TTBView; Item: TTBCustomItem);
var
  IV: TTBItemViewer;
begin
  IV := View.Find(Item);
  if Assigned(IV) then View.Invalidate(IV);
end;

function SpFindItemViewer(View: TTBView; Item: TTBCustomItem): TTBItemViewer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to View.ViewerCount - 1 do
    if View.Viewers[I].Item = Item then begin
      Result := View.Viewers[I];
      Exit;
    end;
end;

function SpFindControlItem(Item: TTBCustomItem; Ctl: TControl): TTBControlItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Item.Count - 1 do begin
    if (Item[I] is TTBControlItem) and (TTBControlItem(Item[I]).Control = Ctl) then begin
      Result := TTBControlItem(Item[I]);
      Break;
    end;
  end;
end;

procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint;
  out DestIV: TTBItemViewer; out DestItemPos: Integer; out DropMark: TRect);
// Returns the ItemViewer and Item index at the given position for Drag & Drop
// operations that needs a DropMark rect.
// Use this when the items are dropped when the drag operation is finished.
var
  DestR: TRect;
const
  DropMarkSize = 4;
begin
  DestItemPos := -1;
  DestIV := SpGetItemViewerFromPoint(Root, View, P);
  DropMark := Rect(0, 0, 0, 0);
  if Assigned(DestIV) then begin
    // Get the destination item position
    DestItemPos := Root.IndexOf(DestIV.Item);
    DestR := SpGetBoundsRect(DestIV, Root);
    if View.Orientation = tbvoVertical then begin
      if P.Y > ((DestR.Bottom - DestR.Top) div 2) + DestR.Top then begin
        Inc(DestItemPos);
        DropMark := Rect(0, DestR.Bottom, View.BaseSize.X, DestR.Bottom + DropMarkSize);
      end
      else
        DropMark := Rect(0, DestR.Top, View.BaseSize.X, DestR.Top + DropMarkSize);
    end
    else
      if P.X > ((DestR.Right - DestR.Left) div 2) + DestR.Left then begin
        Inc(DestItemPos);
        DropMark := Rect(DestR.Right, 0, DestR.Right + DropMarkSize, View.BaseSize.Y);
      end
      else
        DropMark := Rect(DestR.Left, 0, DestR.Left + DropMarkSize, View.BaseSize.Y);
  end;
end;

procedure SpGetDropPosItemViewer(Root: TTBRootItem; View: TTBView; P: TPoint;
  SourceItemPos: Integer; out DestIV: TTBItemViewer; out DestItemPos: Integer);
// Returns the ItemViewer and Item index at the given position for inmediate
// Drag & Drop operations without a DropMark.
// Use this when the items are moved while the mouse is being dragged.
var
  DestR: TRect;
begin
  DestItemPos := -1;
  DestIV := SpGetItemViewerFromPoint(Root, View, P);
  if Assigned(DestIV) then begin
    // Get the destination item position
    DestItemPos := Root.IndexOf(DestIV.Item);
    DestR := SpGetBoundsRect(DestIV, Root);

    if View.Orientation = tbvoVertical then begin
      if P.Y > ((DestR.Bottom - DestR.Top) div 2) + DestR.Top then begin
        if DestItemPos - 1 <> SourceItemPos then Inc(DestItemPos);
      end
      else begin
        if DestItemPos - 1 = SourceItemPos then Dec(DestItemPos);
      end;
    end
    else
      if P.X > ((DestR.Right - DestR.Left) div 2) + DestR.Left then begin
        if DestItemPos - 1 <> SourceItemPos then Inc(DestItemPos);
      end
      else begin
        if DestItemPos - 1 = SourceItemPos then Dec(DestItemPos);
      end;
  end;
end;

procedure SpDrawDropMark(ACanvas: TCanvas; DropMark: TRect);
var
  C: TColor;
  R: TRect;
begin
  if IsRectEmpty(DropMark) then Exit;
  C := ACanvas.Brush.Color;
  try
    R := Rect(DropMark.Left + 1, DropMark.Top, DropMark.Right - 1, DropMark.Top + 2);
    ACanvas.Rectangle(R);
    R := Rect(DropMark.Left + 1, DropMark.Bottom - 2, DropMark.Right - 1, DropMark.Bottom);
    ACanvas.Rectangle(R);

    R := Rect(DropMark.Left, DropMark.Top + 1, DropMark.Right, DropMark.Top + 3);
    ACanvas.Rectangle(R);
    R := Rect(DropMark.Left, DropMark.Bottom - 3, DropMark.Right, DropMark.Bottom - 1);
    ACanvas.Rectangle(R);

    R := Rect(DropMark.Left + 1, DropMark.Top + 4, DropMark.Right - 1, DropMark.Bottom - 4);
    ACanvas.Rectangle(R);

    {
    // Standard DropMark

    R := Rect(DropMark.Left, DropMark.Top, DropMark.Right - 1, DropMark.Bottom - 1);
    if IsRectEmpty(R) then Exit;
    ACanvas.Brush.Color := clBlack;
    ACanvas.Polygon([
      Point(R.Left, R.Top),
      Point(R.Left + 2, R.Top + 2),
      Point(R.Left + 2, R.Bottom - 2),
      Point(R.Left, R.Bottom),
      Point(R.Right, R.Bottom),
      Point(R.Right - 2, R.Bottom - 2),
      Point(R.Right - 2, R.Top + 2),
      Point(R.Right, R.Top)
    ]);
    }
  finally
    ACanvas.Brush.Color := C;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Painting helpers }

function SpXPThemesAvailable: Boolean;
begin
  Result := USE_THEMES and CanUseXPThemes;
end;

function SpXPThemeType(T: TSpTBXThemeType): TSpTBXThemeType;
begin
  Result := T;
  if (Result = thtTBX) and (TBXCurrentTheme = 'Default') then
    Result := thtWindows;
  if (Result = thtWindows) and not SpXPThemesAvailable then
    Result := thtNone;
end;

procedure SpChangeThemeType(Control: TWinControl; TM: TSpTBXThemeType; Recursive: Boolean = True);

  procedure ChangeThemeTypeProperty(Component: TComponent; TM: TSpTBXThemeType);
  var
    PropInfo: PPropInfo;
  begin
    PropInfo := GetPropInfo(Component, 'ThemeType');
    if (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkEnumeration) then begin
      SetOrdProp(Component, PropInfo, Integer(TM));
    end;
  end;

var
  I: Integer;
  C: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do begin
    C := Control.Controls[I];
    ChangeThemeTypeProperty(C, TM);
    if Recursive and (C is TWinControl) then
      SpChangeThemeType(C as TWinControl, TM, Recursive);
  end;
end;

procedure SpDrawParentBackground(Control: TControl; DC: HDC; R: TRect; DeltaX: Integer = 0; DeltaY: Integer = 0);
// TBX 2.2 alpha compatible
var
  Parent: TWinControl;
  Shift, Pt: TPoint;
  Res: Integer;
  ParentColor: TColor;
begin
  Parent := Control.Parent;
  if Parent = nil then
    FillRectEx(DC, R, clBtnFace)
  else if Parent.HandleAllocated then begin
    Shift.X := DeltaX;
    Shift.Y := DeltaY;
    Shift := Parent.ScreenToClient(Control.ClientToScreen(Shift));
    SaveDC(DC);
    try
      OffsetWindowOrgEx(DC, Shift.X, Shift.Y, nil);
      GetBrushOrgEx(DC, Pt);
      SetBrushOrgEx(DC, Pt.X + Shift.X, Pt.Y + Shift.Y, nil);
      Res := Parent.Perform(WM_ERASEBKGND, Integer(DC), 0)
    finally
      RestoreDC(DC, -1);
    end;

    // If it fails fill the rect with the Parent Color.
    if Res = 0 then begin
      ParentColor := TControlAccess(Parent).Color;
      if ParentColor <> clNone then
        FillRectEx(DC, R, ParentColor);
    end;
  end;
end;

procedure SpDrawArrow(DC: HDC; X, Y: Integer; AColor: TColor; Vertical: Boolean; Size: Integer =3);
begin
  if Vertical then
    PolygonEx(DC, [Point(X - Size, Y), Point(X + Size, Y), Point(X, Y + Size)], AColor, AColor)
  else
    PolygonEx(DC, [Point(X, Y - Size), Point(X, Y + Size), Point(X + Size, Y)], AColor, AColor);
end;

procedure SpDrawFocusRect(ACanvas: TCanvas; const R: TRect);
var
  DC: HDC;
  C1, C2: TColor;
begin
  if not IsRectEmpty(R) then begin
    DC := ACanvas.Handle;
    C1 := SetTextColor(DC, clBlack);
    C2 := SetBkColor(DC, clWhite);
    ACanvas.DrawFocusRect(R);
    SetTextColor(DC, C1);
    SetBkColor(DC, C2);
  end;
end;

procedure SpHighlightBitmap(ACanvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
  TransparentColor, HighlightColor: TColor; Amount: Byte);
var
  IL: TImageList;
  R: TRect;
begin
  IL := TImageList.CreateSize(Bitmap.Width, Bitmap.Height);
  try
    IL.AddMasked(Bitmap, TransparentColor);
    R := Bounds(X, Y, IL.Width, IL.Height);
    TBXUtils.HighlightTBXIcon(ACanvas, R, IL, 0, HighlightColor, Amount);
  finally
    IL.Free;
  end;
end;

procedure SpDrawMDIButton(ACanvas: TCanvas; X, Y: Integer; ButtonKind: Cardinal);
var
  Index: Integer;
  Bmp: TBitmap;
begin
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONMAX: Index := 1;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  Bmp := TBitmap.Create;
  try
    Bmp.Monochrome := True;
    MDIButtonsImgList.GetBitmap(Index, Bmp);
    ACanvas.Brush.Color := clBlack; //GetPartColor(ItemInfo, ipText);
    SetTextColor(ACanvas.Handle, clBlack);
    SetBkColor(ACanvas.Handle, clWhite);
    BitBlt(ACanvas.Handle, X, Y, MDIButtonsImgList.Width, MDIButtonsImgList.Height,
      Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
  end;
end;

procedure SpDrawXPBeveledButton(ACanvas: TCanvas; ARect: TRect; Pushed: Boolean; BaseColor: TColor);
var
  R: TRect;
  Amount: Integer;
begin
  R := ARect;
  InflateRect(R, -2, -2);
  if Pushed then
    Amount := -16
  else
    Amount := 16;
  TBXUtils.GradFill(ACanvas.Handle, R, Lighten(BaseColor, Amount), Lighten(BaseColor, -Amount), gkVert);

  R := ARect;
  InflateRect(R, -1, -1);
  if Pushed then
    Amount := -24
  else
    Amount := 24;
  TBXUtils.Frame3D(ACanvas.Handle, R, Lighten(BaseColor, Amount), Lighten(BaseColor, -Amount), False);

  R := ARect;
  TBXUtils.RoundRectEx(ACanvas.Handle, R, 2, 2, Lighten(BaseColor, -64), clNone);
end;

procedure SpDrawXPButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, MouseInControl, Checked, Focused, Defaulted: Boolean;
  ThemeType: TSpTBXThemeType; PaintType: TSpTBXButtonPaintType = bptDefault);

  procedure DrawFixedTBXButtons(ACanvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo);
  begin
    // [TBXTheme-Change]
    // On the Aluminum theme draw the Edit Button
    if TBXCurrentTheme = 'Aluminum' then begin
      SpDrawXPBeveledButton(ACanvas, ARect, ItemInfo.Pushed, TColor($DFDFDF));
      Exit;
    end;

    // [TBXTheme-Change]
    // On the OfficeXP theme the embedded selection looks better, use this
    // when not pushed and not hovering
    if (TBXCurrentTheme = 'OfficeXP') and not (ItemInfo.Pushed or MouseInControl or Checked) then begin
      ItemInfo.ViewType := ItemInfo.ViewType or TVT_EMBEDDED;
      ItemInfo.HoverKind := hkNone;
      CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
      Exit;
    end;

    // [TBXTheme-Change]
    // On the Athen and Dream theme the Menu selection looks better, use this
    // when not pushed and not hovering
    if (TBXCurrentTheme = 'Athen') or (TBXCurrentTheme = 'Dream') then begin
      if not (ItemInfo.Pushed or MouseInControl or Checked) then begin
        ItemInfo.ViewType := VT_POPUP;
        CurrentTheme.PaintFrame(ACanvas, ARect, ItemInfo);
        Exit;
      end;
      if Checked then begin
        ItemInfo.Selected := True;
        ItemInfo.Pushed := False;
        if MouseInControl then
          ItemInfo.HoverKind := hkMouseHover
        else
          ItemInfo.HoverKind := hkNone;
      end;
    end;

    CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
  end;

var
  Flags: Cardinal;
  C: TColor;
  ItemInfo: TTBXItemInfo;
  B: TBitmap;
  R: TRect;
begin
  case PaintType of
    bptTBX, bptTBXToolbarItem:
      begin
        SpFillItemInfo(Enabled, Pushed, MouseInControl, Checked, ItemInfo);
        if PaintType = bptTBX then
          ItemInfo.ViewType := VT_TOOLBAR or TVT_EMBEDDED;
        CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
      end;
    bptTBXDock:
      CurrentTheme.PaintDock(ACanvas, ARect, ARect, DP_TOP);
    bptDefault:
      begin
        ThemeType := SpXPThemeType(ThemeType);
        case ThemeType of
          thtNone:
            begin
              ACanvas.Brush.Color := clBtnFace;
              ACanvas.FillRect(ARect);
              if Defaulted or Focused then begin
                C := ACanvas.Brush.Color;
                ACanvas.Brush.Color := clWindowFrame;
                ACanvas.FrameRect(ARect);
                ACanvas.Brush.Color := C;
                InflateRect(ARect, -1, -1); // Reduce the Rect for the focus rect
              end;
              if Pushed or Checked then begin
                C := ACanvas.Brush.Color;
                ACanvas.Brush.Color := clBtnShadow;
                ACanvas.FrameRect(ARect);
                ACanvas.Brush.Color := C;
              end
              else
                DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
            end;
          thtWindows:
            begin
              if not Enabled then Flags := PBS_DISABLED
              else if Pushed or Checked then Flags := PBS_PRESSED
              else if MouseInControl then Flags := PBS_HOT
              else if Defaulted or Focused then Flags := PBS_DEFAULTED
              else Flags := PBS_NORMAL;
              DrawThemeBackground(BUTTON_THEME, ACanvas.Handle, BP_PUSHBUTTON, Flags, ARect, nil);
            end;
          thtTBX:
            if MouseInControl and not (Pushed or Checked) then begin
              B := TBitmap.Create;
              try
                B.Width := ARect.Right - ARect.Left;
                B.Height := ARect.Bottom - ARect.Top;
                R := Rect(0, 0, B.Width, B.Height);
                StretchBlt(B.Canvas.Handle, 0, 0, B.Width, B.Height,
                  ACanvas.Handle, ARect.Left, ARect.Top, B.Width, B.Height, cmSrcCopy);
                SpFillItemInfo(Enabled, False, True, False, ItemInfo);
                DrawFixedTBXButtons(B.Canvas, R, ItemInfo);
                SpHighlightBitmap(ACanvas, ARect.Left, ARect.Top, B, clNone, clWhite, 180);
              finally
                B.Free;
              end;
            end
            else begin
              Pushed := Pushed or Checked;
              SpFillItemInfo(Enabled, Pushed, True, Pushed, ItemInfo);
              DrawFixedTBXButtons(ACanvas, ARect, ItemInfo);
            end;
        end;
      end;
  end;

  if Focused then begin
    InflateRect(ARect, -3, -3);
    SpDrawFocusRect(ACanvas, ARect);
  end;
end;

procedure SpDrawXPRectButton(ACanvas: TCanvas; ARect: TRect; Enabled, Pushed, MouseInControl, Checked, Focused, Defaulted: Boolean;
  ThemeType: TSpTBXThemeType; PaintType: TSpTBXButtonPaintType = bptDefault; BorderColor: TColor = clNone);
var
  DefC, C: TColor;
begin
  SpDrawXPButton(ACanvas, ARect, Enabled, Pushed, MouseInControl, Checked, Focused, Defaulted, ThemeType, PaintType);

  C := ACanvas.Pixels[ARect.Left + 4, ARect.Top];
  DefC := ACanvas.Brush.Color;
  try
    // Rectangle border
    if BorderColor <> clNone then
      ACanvas.Brush.Color := BorderColor
    else
      ACanvas.Brush.Color := C;
    ACanvas.FrameRect(ARect);

    // Fill top pixels
    ACanvas.Pixels[ARect.Left + 1, ARect.Top + 1] := ACanvas.Pixels[ARect.Left + 2, ARect.Top + 1];
    ACanvas.Pixels[ARect.Right - 2, ARect.Top + 1] := ACanvas.Pixels[ARect.Right - 3, ARect.Top + 1];

    // Fill bottom pixels
    ACanvas.Pixels[ARect.Left + 1, ARect.Bottom - 2] := ACanvas.Pixels[ARect.Left + 2, ARect.Bottom - 2];
    ACanvas.Pixels[ARect.Right - 2, ARect.Bottom - 2] := ACanvas.Pixels[ARect.Right - 3, ARect.Bottom - 2];
  finally
    ACanvas.Brush.Color := DefC;
  end;
end;

procedure SpDrawXPCheckBoxGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  State: TCheckBoxState; MouseInControl, Pushed, Focused: Boolean;
  ThemeType: TSpTBXThemeType);
const
  EnabledState: array [Boolean] of Integer = (PFS_DISABLED, 0);
  StateFlags: array [TCheckBoxState] of Integer = (0, PFS_CHECKED, PFS_MIXED);
  HotState: array [Boolean] of Integer = (0, PFS_HOT);
  PushedState: array [Boolean] of Integer = (0, PFS_PUSHED);
  FocusedState: array [Boolean] of Integer = (0, PFS_FOCUSED);
var
  Flags: Integer;
  C: TColor;
begin
  ThemeType := SpXPThemeType(ThemeType);
  Flags := 0;

  C := ACanvas.Brush.Color;
  try
    case ThemeType of
      thtNone:
        begin
          case State of
            cbChecked: Flags := DFCS_BUTTONCHECK or DFCS_CHECKED;
            cbGrayed: Flags := DFCS_BUTTON3STATE or DFCS_CHECKED;
            cbUnChecked: Flags := DFCS_BUTTONCHECK;
          end;
          if not Enabled then
            Flags := Flags or DFCS_INACTIVE;
          if Pushed then
            Flags := Flags or DFCS_PUSHED;
          DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
        end;
      thtWindows:
        begin
          case State of
            cbChecked: Flags := CBS_CHECKEDNORMAL;
            cbGrayed: Flags := CBS_MIXEDNORMAL;
            cbUnChecked: Flags := CBS_UNCHECKEDNORMAL;
          end;

          if not Enabled then Inc(Flags, 3)
          else
            if Pushed then Inc(Flags, 2)
            else if MouseInControl then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, ACanvas.Handle, BP_CHECKBOX, Flags, ARect, nil);
        end;
      thtTBX:
        begin
          Flags := EnabledState[Enabled] or StateFlags[State];
          if Enabled then Flags := Flags or HotState[MouseInControl] or PushedState[Pushed] or FocusedState[Focused];
          CurrentTheme.PaintFrameControl(ACanvas, ARect, PFC_CHECKBOX, Flags, nil);
        end;
    end;
  finally
    ACanvas.Brush.Color := C;
  end;
end;

procedure SpDrawXPRadioButtonGlyph(ACanvas: TCanvas; ARect: TRect; Enabled: Boolean;
  Checked, MouseInControl, Pushed, Focused: Boolean; ThemeType: TSpTBXThemeType);
const
  EnabledState: array [Boolean] of Integer = (PFS_DISABLED, 0);
  StateFlags: array [Boolean] of Integer = (0, PFS_CHECKED);
  HotState: array [Boolean] of Integer = (0, PFS_HOT);
  PushedState: array [Boolean] of Integer = (0, PFS_PUSHED);
  FocusedState: array [Boolean] of Integer = (0, PFS_FOCUSED);
var
  Flags: Integer;
  C: TColor;
begin
  ThemeType := SpXPThemeType(ThemeType);
  C := ACanvas.Brush.Color;
  try
    case ThemeType of
      thtNone:
        begin
          Flags := DFCS_BUTTONRADIO;
          if Checked then
            Flags := Flags or DFCS_CHECKED;
          if not Enabled then
            Flags := Flags or DFCS_INACTIVE;
          if Pushed then
            Flags := Flags or DFCS_PUSHED;
          DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, Flags);
        end;
      thtWindows:
        begin
          if Checked then Flags := RBS_CHECKEDNORMAL
          else Flags := RBS_UNCHECKEDNORMAL;

          if not Enabled then Inc(Flags, 3)
          else
            if Pushed then Inc(Flags, 2)
            else if MouseInControl then Inc(Flags);
          DrawThemeBackground(BUTTON_THEME, ACanvas.Handle, BP_RADIOBUTTON, Flags, ARect, nil);
        end;
      thtTBX:
        begin
          Flags := EnabledState[Enabled] or StateFlags[Checked];
          if Enabled then Flags := Flags or HotState[MouseInControl] or PushedState[Pushed] or FocusedState[Focused];
          CurrentTheme.PaintFrameControl(ACanvas, ARect, PFC_RADIOBUTTON, Flags, nil);
        end;
    end;
  finally
    ACanvas.Brush.Color := C;
  end;
end;

procedure SpDrawXPStatusBar(ACanvas: TCanvas; ARect, AGripRect: TRect; ThemeType: TSpTBXThemeType; UsingOfficeTheme: Boolean);
begin
  ThemeType := SpXPThemeType(ThemeType);
  case ThemeType of
    thtNone:
      begin
        TBXUtils.FillRectEx(ACanvas.Handle, ARect, clBtnFace);
        TBXUtils.Frame3D(ACanvas.Handle, ARect, clBtnShadow, clWindow, False);
        if not IsRectEmpty(AGripRect) then begin
          InflateRect(AGripRect, 0, -1);
          DrawFrameControl(ACanvas.Handle, AGripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
        end;
      end;
    thtWindows:
      begin
        DrawThemeBackground(STATUSBAR_THEME, ACanvas.Handle, 0, 0, ARect, nil);
        if not IsRectEmpty(AGripRect) then
          DrawThemeBackground(STATUSBAR_THEME, ACanvas.Handle, SP_GRIPPER, 0, AGripRect, nil)
      end;
    thtTBX:
      begin
        // [TBXTheme-Change]
        // Office themes have rectangle panels
        if UsingOfficeTheme then begin
          TBXUtils.FillRectEx(ACanvas.Handle, ARect, clBtnFace);
          TBXUtils.FrameRectEx(ACanvas.Handle, ARect, clWindow, False);
        end
        else
          CurrentTheme.PaintStatusBar(ACanvas, ARect, SBP_BODY);

        if not IsRectEmpty(AGripRect) then
          CurrentTheme.PaintStatusBar(ACanvas, AGripRect, SBP_GRIPPER);
      end;
  end;
end;

procedure SpDrawXPGradientTitleBar(ACanvas: TCanvas; ARect: TRect; IsActive: Boolean);
const
  SPI_GETGRADIENTCAPTIONS = $1008;
  DC_GRADIENT = $20;
  ActiveCaptionFlags: array [Boolean] of Integer = (0, DC_ACTIVE);
  GradientCaptionFlags: array [Boolean] of Integer = (0, DC_GRADIENT);
  CaptionBkColors: array [Boolean, Boolean] of Integer =
    ((COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION),
    (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION));
var
  Gradient: Boolean;
  B: BOOL;
begin
  ACanvas.Lock;
  try
    Gradient := SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @B, 0) and B;
    Windows.DrawCaption(GetDesktopWindow, ACanvas.Handle, ARect, DC_TEXT or ActiveCaptionFlags[IsActive] or GradientCaptionFlags[Gradient]);
  finally
    ACanvas.Unlock;
  end;
end;

procedure SpDrawXPTitleBar(WindowHandle: THandle; ACanvas: TCanvas; ARect: TRect;
  TitleBarWidth, TitleBarHeight: Integer; IsActive, TBXStyleBackground: Boolean);
var
  R, BufferRect: TRect;
  W, H: Integer;
  FloatingBorderSize: TPoint;
  WindowInfo: TTBXWindowInfo;
  Buffer, B: TBitmap;
begin
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  BufferRect := ARect;
  OffsetRect(BufferRect, -ARect.Left, -ARect.Top);

//  FloatingBorderSize.X := 3; // [TBXTheme-Change]
//  FloatingBorderSize.Y := 3; // [TBXTheme-Change]
  FloatingBorderSize.X := GetSystemMetrics(SM_CXFRAME); // [TBXTheme-Change]
  FloatingBorderSize.Y := GetSystemMetrics(SM_CYFRAME); // [TBXTheme-Change]

  Buffer := TBitmap.Create;
  try
    Buffer.Width := W;
    Buffer.Height := H;
    Buffer.Canvas.Brush.Color := ACanvas.Brush.Color;
    Buffer.Canvas.FillRect(BufferRect);
    // Fill the canvas
    // [TBXTheme-Change]
    // Don't fill the background when using the Default theme
    if TBXStyleBackground and (TBXCurrentTheme <> 'Default') then begin
      R := BufferRect;
      InflateRect(R, 3, 3);
      SpDrawXPButton(Buffer.Canvas, R, True, False, True, False, False, False, thtTBX, bptTBXDock);
    end;

    // Draw the Borders
    if H > TitleBarHeight then begin
      if CurrentSkin.IsActive then
        CurrentSkin.GetTheme.PaintTitleBarNCArea(Buffer.Canvas, BufferRect, True,
          True, False, False, False, '', 0, FloatingBorderSize.Y - 1)
      else begin
        FillChar(WindowInfo, SizeOf(WindowInfo), 0);
        WindowInfo.ParentHandle := WindowHandle;
        WindowInfo.WindowHandle := WindowHandle;
        WindowInfo.ViewType := TVT_NORMALTOOLBAR or TVT_FLOATING or TVT_RESIZABLE;
        WindowInfo.ClientWidth := W;
        WindowInfo.ClientHeight := H;
        WindowInfo.EffectiveColor := Buffer.Canvas.Brush.Color;
        WindowInfo.ShowCaption := True;
        WindowInfo.FloatingBorderSize := FloatingBorderSize;
        WindowInfo.Active := True;
        WindowInfo.RedrawPart := WRP_BORDER;
        CurrentTheme.PaintFloatingBorder(Buffer.Canvas, BufferRect, WindowInfo);
      end;
    end;

    // Stretch draw the title bar
    if (TitleBarWidth > 0) and (TitleBarHeight > 0) then begin
      B := TBitmap.Create;
      try
        // [TBXTheme-Change]
        // Some themes must be painted manually
        if CurrentSkin.IsActive then begin
          B.Width := TitleBarWidth;
          B.Height := TitleBarHeight;
          R := Rect(0, 0, B.Width, B.Height);
          CurrentSkin.GetTheme.PaintTitleBarNCArea(B.Canvas, R, True,
            True, False, False, False, '', TitleBarHeight, 0);
          Buffer.Canvas.Draw(BufferRect.Left, BufferRect.Top, B);
        end
        else
        if (TBXCurrentTheme = 'Stripes') or (TBXCurrentTheme = 'Miranda') or
          (TBXCurrentTheme = 'Monai') or (TBXCurrentTheme = 'MonaiXP') then
        begin
          B.Width := TitleBarWidth;
          B.Height := TitleBarHeight;
          R := Rect(0, 0, B.Width, B.Height);
          TBXUtils.GradFill(B.Canvas.Handle, R, Lighten(clBtnFace, 12), Lighten(clBtnFace, -12), gkVert);
          B.Canvas.Pen.Color := clBtnShadow;
          B.Canvas.PenPos := Point(R.Left, R.Bottom - 1);
          B.Canvas.LineTo(R.Right, R.Bottom - 1);
          Buffer.Canvas.Draw(BufferRect.Left, BufferRect.Top, B);
        end
        else begin
          B.Width := W;
          B.Height := H + 100;

          FillChar(WindowInfo, SizeOf(WindowInfo), 0);
          WindowInfo.ParentHandle := WindowHandle;
          WindowInfo.WindowHandle := WindowHandle;
          WindowInfo.ViewType := TVT_NORMALTOOLBAR or TVT_FLOATING or TVT_RESIZABLE;
          WindowInfo.ClientWidth := B.Width;
          WindowInfo.ClientHeight := B.Height;
          WindowInfo.EffectiveColor := Buffer.Canvas.Brush.Color;
          WindowInfo.ShowCaption := True;
          WindowInfo.Active := True;
          WindowInfo.RedrawPart := WRP_BORDER or WRP_CAPTION;

          R := Rect(0, 0, B.Width, B.Height);
          CurrentTheme.PaintFloatingBorder(B.Canvas, R, WindowInfo);
          StretchBlt(Buffer.Canvas.Handle, BufferRect.Left, BufferRect.Top, TitleBarWidth, TitleBarHeight,
            B.Canvas.Handle, 0, 0, B.Width, GetSystemMetrics(SM_CYSMCAPTION) - 1, SRCCOPY);
        end;
      finally
        B.Free;
      end;
    end;

    ACanvas.Draw(ARect.Left, ARect.Top, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure SpDrawXPDockablePanelTitleBar(WindowHandle: THandle; ACanvas: TCanvas; X, Y, AWidth, AHeight: Integer;
  IsActive, IsFloating: Boolean);
var
  R: TRect;
  DockPanelInfo: TTBXDockPanelInfo;
  B: TBitmap;
begin
  // There are two ways of painting the panel caption: using
  // CurrentTheme.PaintDockPanelNCArea or CurrentTheme.PaintFloatingBorder.
  // But both paint ALL the NC area!!! and is too small to
  // draw, the Height depends on GetSystemMetrics(SM_CYSMCAPTION).
  // We have to get the title bar and paint it in the canvas.

  B := TBitmap.Create;
  try
    // [TBXTheme-Change]
    // Some themes must be painted manually, when the Dream theme is used
    // the DockablePanel drag handles must be removed.
    // When the Default, Stripes, Miranda, Monai or MonaiXP themes are used
    // just paint a gradient rect.
    if CurrentSkin.IsActive then begin
      B.Width := AWidth;
      B.Height := AHeight;
      R := Rect(0, 0, B.Width, B.Height);
      CurrentSkin.GetTheme.PaintTitleBarNCArea(B.Canvas, R, True,
        IsFloating, False, False, False, '', B.Height);
      ACanvas.Draw(X, Y, B);
    end
    else
    if TBXCurrentTheme = 'Dream' then begin
      B.Width := AWidth;
      B.Height := AHeight;
      R := Rect(0, 0, B.Width, B.Height);
      SpDrawXPTitleBar(WindowHandle, B.Canvas, R, B.Width, B.Height, True, True);
      ACanvas.Draw(X, Y, B);
    end
    else
    if (TBXCurrentTheme = 'Default') or (TBXCurrentTheme = 'Stripes') or
      (TBXCurrentTheme = 'Miranda') or (TBXCurrentTheme = 'Monai') or (TBXCurrentTheme = 'MonaiXP') then
    begin
      B.Width := AWidth;
      B.Height := AHeight;
      B.Canvas.Brush.Color := clBtnFace;
      R := Rect(0, 0, B.Width, B.Height);
      B.Canvas.FrameRect(R);
      InflateRect(R, -1, 0);
      TBXUtils.GradFill(B.Canvas.Handle, R, Lighten(clBtnFace, 12), Lighten(clBtnFace, -12), gkVert);
      Windows.DrawEdge(B.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
      ACanvas.Draw(X, Y, B);
    end
    else begin
      B.Width := AWidth;
      B.Height := AHeight + 100;
      FillChar(DockPanelInfo, SizeOf(DockPanelInfo), 0);
      DockPanelInfo.WindowHandle := WindowHandle;
      DockPanelInfo.ViewType := DPVT_NORMAL or DPVT_RESIZABLE;
      DockPanelInfo.IsVertical := True;
      DockPanelInfo.BorderStyle := bsSingle;
      CurrentTheme.GetViewBorder(DockPanelInfo.ViewType, DockPanelInfo.BorderSize);
      DockPanelInfo.ClientWidth := B.Width;
      DockPanelInfo.ClientHeight := B.Height;
      DockPanelInfo.EffectiveColor := ACanvas.Brush.Color;
      DockPanelInfo.ShowCaption := True;

      R := Rect(0, 0, B.Width, B.Height);
      CurrentTheme.PaintDockPanelNCArea(B.Canvas, R, DockPanelInfo);

      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      StretchBlt(ACanvas.Handle, X, Y, AWidth, AHeight,
        B.Canvas.Handle, 0, 0, B.Width, GetSystemMetrics(SM_CYSMCAPTION) + 1, SRCCOPY)
    end;
  finally
    B.Free;
  end;
end;

procedure SpDrawXPEditFrame(ACanvas: TCanvas; ARect: TRect; Enabled, HotTrack: Boolean;
  ThemeType: TSpTBXThemeType; ClipContent: Boolean);
var
  ItemInfo: TTBXItemInfo;
  EditInfo: TTBXEditInfo;
  BorderR: TRect;
begin
  ThemeType := SpXPThemeType(ThemeType);

  if ClipContent then begin
    BorderR := ARect;
    if HotTrack then
      InflateRect(BorderR, -1, -1)
    else
      InflateRect(BorderR, -2, -2);
    ExcludeClipRect(ACanvas.Handle, BorderR.Left, BorderR.Top, BorderR.Right, BorderR.Bottom);
  end;
  try
    case ThemeType of
      thtWindows:
        DrawThemeBackground(COMBO_THEME, ACanvas.Handle, 0, 0, ARect, nil);
      thtNone: ;
      thtTBX:
        begin
          SpFillItemInfo(Enabled, False, HotTrack, False, ItemInfo);
          FillChar(EditInfo, SizeOf(TTBXEditInfo), 0);
          CurrentTheme.PaintEditFrame(ACanvas, ARect, ItemInfo, EditInfo);
        end;
    end;
  finally
    if ClipContent then
      SelectClipRgn(ACanvas.Handle, 0);
  end;
end;

procedure SpDrawXPEditFrame(AWinControl: TWinControl; HotTracking: Boolean; ThemeType: TSpTBXThemeType);
var
  R: TRect;
  DC: HDC;
  ACanvas: TCanvas;
begin
  DC := GetWindowDC(AWinControl.Handle);
  try
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      GetWindowRect(AWinControl.Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      with R do
        ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
      SpDrawParentBackground(AWinControl, ACanvas.Handle, R);
      SpDrawXPEditFrame(ACanvas, R, AWinControl.Enabled, HotTracking, ThemeType, True);
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    ReleaseDC(AWinControl.Handle, DC);
  end;
end;

procedure SpDrawXPListItemBackground(ACanvas: TCanvas; ARect: TRect; Selected, Pushed, Focused: Boolean; ThemeType: TSpTBXThemeType);
var
  ItemInfo: TTBXItemInfo;
begin
  if ThemeType = thtTBX then begin
    ACanvas.FillRect(ARect);
    if Selected then begin
      SpFillItemInfo(True, Pushed, True, False, ItemInfo);
      ACanvas.Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);

      // [TBXTheme-Change]
      // On the Dream and SentimoX themes the Menu selection looks better
      // than the button painting
      if (TBXCurrentTheme = 'Dream') or (TBXCurrentTheme = 'SentimoX') then
        CurrentTheme.PaintMenuItem(ACanvas, ARect, ItemInfo)
      else
        CurrentTheme.PaintButton(ACanvas, ARect, ItemInfo);
    end;
  end
  else begin
    if Selected then begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end;
    ACanvas.FillRect(ARect);
    if Focused then
      SpDrawFocusRect(ACanvas, ARect);
  end;
end;

procedure SpDrawXPIconSelection(ACanvas: TCanvas; ARect: TRect; ImageList: TCustomImageList; ImageIndex: Integer);
begin
  DrawTBXIconShadow(ACanvas, ARect, ImageList, ImageIndex, 1);
  OffsetRect(ARect, 1, 1);
  DrawTBXIconShadow(ACanvas, ARect, ImageList, ImageIndex, 1);
  OffsetRect(ARect, -2, -2);
  DrawTBXIcon(ACanvas, ARect, ImageList, ImageIndex, False);
end;

procedure SpDrawXPGrip(ACanvas: TCanvas; ARect: TRect; Centered, Vertical: Boolean);
var
  I, CellCount, Y: Integer;
  HiC, LoC: TColor;
begin
  HiC := $DEDEDE; // Gray
  LoC := $8F8F8F; // Black
  if Vertical then begin
    //  3 x 4 cells (Black, Grey, White)
    //  BBG
    //  BWG
    //  GGG
    //  ---

    if Centered then begin
      ARect.Left := (ARect.Right + ARect.Left - 3) div 2;
      ARect.Right := ARect.Left + 3;
    end;

    CellCount := (ARect.Bottom - ARect.Top) div 4;
    for I := 0 to CellCount - 1 do begin
      Y := ARect.Top + (I * 4);
      ACanvas.Pixels[ARect.Left, Y] := LoC;
      ACanvas.Pixels[ARect.Left + 1, Y] := LoC;
      ACanvas.Pixels[ARect.Left + 2, Y] := HiC;

      ACanvas.Pixels[ARect.Left, Y + 1] := LoC;
      ACanvas.Pixels[ARect.Left + 1, Y + 1] := clWhite;
      ACanvas.Pixels[ARect.Left + 2, Y + 1] := HiC;

      ACanvas.Pixels[ARect.Left, Y + 2] := HiC;
      ACanvas.Pixels[ARect.Left + 1, Y + 2] := HiC;
      ACanvas.Pixels[ARect.Left + 2, Y + 2] := HiC;
    end;
  end
  else begin
    //  4 x 3 cells (Black, Grey, White)
    //  BBG-
    //  BWG-
    //  GGG-

    if Centered then begin
      ARect.Top := (ARect.Bottom + ARect.Top - 3) div 2;
      ARect.Bottom := ARect.Top + 3;
    end;

    CellCount := (ARect.Right - ARect.Left) div 4;
    for I := 0 to CellCount - 1 do begin
      Y := ARect.Left + (I * 4);
      ACanvas.Pixels[Y, ARect.Top] := LoC;
      ACanvas.Pixels[Y, ARect.Top + 1] := LoC;
      ACanvas.Pixels[Y, ARect.Top + 2] := HiC;

      ACanvas.Pixels[Y + 1, ARect.Top] := LoC;
      ACanvas.Pixels[Y + 1, ARect.Top + 1] := clWhite;
      ACanvas.Pixels[Y + 1, ARect.Top + 2] := HiC;

      ACanvas.Pixels[Y + 2, ARect.Top] := HiC;
      ACanvas.Pixels[Y + 2, ARect.Top + 1] := HiC;
      ACanvas.Pixels[Y + 2, ARect.Top + 2] := HiC;
    end;
  end;
end;

function SpIsEmptyColor(C: TColor): Boolean;
begin
  Result := (C = clNone) or (C = clDefault);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ WideString helpers}

function EnumFontsProc(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 0;
end;

function SpCreateRotatedFont(DC: HDC; Orientation: Integer = 2700): HFONT;
var
  LogFont: TLogFont;
  TM: TTextMetric;
  VerticalFontName: array[0..LF_FACESIZE-1] of Char;
  VerticalFontExists: Boolean;
begin
  if GetObject(GetCurrentObject(DC, OBJ_FONT), SizeOf(LogFont),
     @LogFont) = 0 then begin
    { just in case... }
    Result := 0;
    Exit;
  end;
  LogFont.lfEscapement := Orientation;
  LogFont.lfOrientation := Orientation;
  LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;  { needed for Win9x }

  { Don't let a random TrueType font be substituted when MS Sans Serif or
    Microsoft Sans Serif are used. On Windows 2000 and later, hard-code Tahoma
    because Arial can't display Japanese or Thai Unicode characters (on Windows
    2000 at least). On earlier versions, hard-code Arial since NT 4.0 doesn't
    ship with Tahoma, and 9x doesn't do Unicode. }
  if (StrIComp(LogFont.lfFaceName, 'MS Sans Serif') = 0) or
     (StrIComp(LogFont.lfFaceName, 'Microsoft Sans Serif') = 0) then begin
    if Win32MajorVersion >= 5 then
      StrPCopy(LogFont.lfFaceName, 'Tahoma')
    else
      StrPCopy(LogFont.lfFaceName, 'Arial');
    { Set lfHeight to the actual height of the current font. This is needed
      to work around a Windows 98 issue: on a clean install of the OS,
      SPI_GETNONCLIENTMETRICS returns -5 for lfSmCaptionFont.lfHeight. This is
      wrong; it should return -11 for an 8 pt font. With normal, unrotated text
      this actually displays correctly, since MS Sans Serif doesn't support
      sizes below 8 pt. However, when we change to a TrueType font like Arial,
      this becomes a problem because it'll actually create a font that small. }
    if GetTextMetrics(DC, TM) then begin
      { If the original height was negative, keep it negative }
      if LogFont.lfHeight <= 0 then
        LogFont.lfHeight := -(TM.tmHeight - TM.tmInternalLeading)
      else
        LogFont.lfHeight := TM.tmHeight;
    end;
  end;

  { Use a vertical font if available so that Asian characters aren't drawn
    sideways }
  if StrLen(LogFont.lfFaceName) < SizeOf(VerticalFontName)-1 then begin
    VerticalFontName[0] := '@';
    StrCopy(@VerticalFontName[1], LogFont.lfFaceName);
    VerticalFontExists := False;
    EnumFonts(DC, VerticalFontName, @EnumFontsProc, @VerticalFontExists);
    if VerticalFontExists then
      StrCopy(LogFont.lfFaceName, VerticalFontName);
  end;

  Result := CreateFontIndirect(LogFont);
end;

function SpDrawRotatedText(const DC: HDC; AText: WideString; var ARect: TRect; const AFormat: Cardinal; RotationAngle: TSpTextRotationAngle = tra270): Integer;
{ The format flag this function respects are
  DT_CALCRECT, DT_NOPREFIX, DT_HIDEPREFIX, DT_CENTER, DT_END_ELLIPSIS, DT_NOCLIP }
var
  RotatedFont, SaveFont: HFONT;
  TextMetrics: TTextMetric;
  X, Y, P, I, SU, FU, W: Integer;
  SaveAlign: UINT;
  Clip: Boolean;
  Pen, SavePen: HPEN;
  AnsiText: AnsiString;
  Sz: TSize;
  Orientation: Integer;
begin
  Result := 0;
  if Length(AText) = 0 then Exit;

  Orientation := 0;
  case RotationAngle of
    tra90: Orientation := 900;   // 90 degrees
    tra270: Orientation := 2700; // 270 degrees
  end;
  RotatedFont := SpCreateRotatedFont(DC, Orientation);
  SaveFont := SelectObject(DC, RotatedFont);

  GetTextMetrics(DC, TextMetrics);
  X := ARect.Left + ((ARect.Right - ARect.Left) - TextMetrics.tmHeight) div 2;

  Clip := AFormat and DT_NOCLIP = 0;

  { Find the index of the character that should be underlined. Delete '&'
    characters from the string. Like DrawText, only the last prefixed character
    will be underlined. }
  P := 0;
  I := 1;
  if AFormat and DT_NOPREFIX = 0 then
    while I <= Length(AText) do
    begin
      if AText[I] = '&' then
      begin
        Delete(AText, I, 1);
        if PWideChar(AText)[I - 1] <> '&' then P := I;
      end;
      Inc(I);
    end;

  if AFormat and DT_END_ELLIPSIS <> 0 then
  begin
    if (Length(AText) > 1) and (SpGetTextSize(DC, AText, False).cx > ARect.Bottom - ARect.Top) then
    begin
      W := ARect.Bottom - ARect.Top;
      if W > 2 then
      begin
        Delete(AText, Length(AText), 1);
        while (Length(AText) > 1) and (SpGetTextSize(DC, AText + '...', False).cx > W) do
          Delete(AText, Length(AText), 1);
      end
      else AText := AText[1];
      if P > Length(AText) then P := 0;
      AText := AText + '...';
    end;
  end;

  Sz := SpGetTextSize(DC, AText, False);
  Result := Sz.cy;

  if AFormat and DT_CALCRECT <> 0 then begin
    ARect.Right := ARect.Left + Sz.cy;
    ARect.Bottom := ARect.Top + Sz.cx;
  end
  else begin
    if AFormat and DT_CENTER <> 0 then
      Y := ARect.Top + ((ARect.Bottom - ARect.Top) - Sz.cx) div 2
    else
      Y := ARect.Top;

    if Clip then
    begin
      SaveDC(DC);
      with ARect do IntersectClipRect(DC, Left, Top, Right, Bottom);
    end;

    case RotationAngle of
      tra90: SaveAlign := SetTextAlign(DC, TA_RIGHT);
      tra270: SaveAlign := SetTextAlign(DC, TA_BOTTOM);
    else
      SaveAlign := SetTextAlign(DC, TA_LEFT);
    end;

    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
      AnsiText := AText;
      Windows.TextOut(DC, X, Y, PChar(AnsiText), Length(AText));
    end
    else
      Windows.TextOutW(DC, X, Y, PWideChar(AText), Length(AText));
    SetTextAlign(DC, SaveAlign);

    { Underline }
    if (P > 0) and (AFormat and DT_HIDEPREFIX = 0) then
    begin
      SU := SpGetTextSize(DC, Copy(AText, 1, P - 1), False).cx;
      FU := SU + SpGetTextSize(DC, PWideChar(AText)[P - 1], False).cx;
      Inc(X, TextMetrics.tmDescent - 2);
      Pen := CreatePen(PS_SOLID, 1, GetTextColor(DC));
      SavePen := SelectObject(DC, Pen);
      MoveToEx(DC, X, Y + SU, nil);
      LineTo(DC, X, Y + FU);
      SelectObject(DC, SavePen);
      DeleteObject(Pen);
    end;

    if Clip then RestoreDC(DC, -1);
  end;

  SelectObject(DC, SaveFont);
  DeleteObject(RotatedFont);
end;

function SpCalcXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionAlignment: TAlignment; Flags: Cardinal; GlyphWidth, GlyphHeight: Integer;
  Layout: TSpGlyphLayout; PushedCaption: Boolean; out ACaptionRect, AGlyphRect: TRect;
  RotationAngle: TSpTextRotationAngle = tra0): Integer;
var
  R: TRect;
  TextOffset, Spacing: TPoint;
  CaptionSz: TSize;
begin
  Result := 0;
  ACaptionRect := Rect(0, 0, 0, 0);
  AGlyphRect := Rect(0, 0, 0, 0);
  TextOffset := Point(0, 0);
  Spacing := Point(0, 0);
  if (Caption <> '') and (GlyphWidth > 0) and (GlyphHeight > 0) then
    if RotationAngle = tra0 then
      Spacing := Point(4, 1)
    else
      Spacing := Point(1, 4);

  Flags := Flags and not DT_CENTER;
  Flags := Flags and not DT_VCENTER;
  if CaptionAlignment = taRightJustify then
    Flags := Flags or DT_RIGHT;

  // Get the caption size
  if ((Flags and DT_WORDBREAK) <> 0) or ((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0) then begin
    if Layout = ghlGlyphLeft then  // Glyph on left or right side
      R := Rect(0, 0, ARect.Right - ARect.Left - GlyphWidth - Spacing.X, 1)
    else  // Glyph on top
      R := Rect(0, 0, ARect.Right - ARect.Left, 1);
  end
  else
    R := Rect(0, 0, 1, 1);

  if (fsBold in ACanvas.Font.Style) and (((Flags and DT_END_ELLIPSIS) <> 0) or ((Flags and DT_PATH_ELLIPSIS) <> 0)) then begin
    // [Bugfix] Windows bug:
    // When the Font is Bold and DT_END_ELLIPSIS or DT_PATH_ELLIPSIS is used
    // DrawTextW returns an incorrect size if the string is unicode.
    // The R.Right is reduced by 3 which cuts down the string and
    // adds the ellipsis.
    // We have to obtain the real size and check if it fits in the Rect.
    CaptionSz := SpGetTextSize(ACanvas.Handle, Caption, True);
    if CaptionSz.cx <= R.Right then begin
      R := Rect(0, 0, CaptionSz.cx, CaptionSz.cy);
      Result := CaptionSz.cy;
    end;
  end;

  if Result <= 0 then begin
    Result := SpDrawXPText(ACanvas, Caption, R, Flags or DT_CALCRECT, gldNone, clYellow, RotationAngle);
    CaptionSz.cx := R.Right;
    CaptionSz.cy := R.Bottom;
  end;

  // ACaptionRect
  if Result > 0 then begin
    R.Top := (ARect.Top + ARect.Bottom - CaptionSz.cy) div 2; // Vertically centered
    R.Bottom := R.Top + CaptionSz.cy;
    case CaptionAlignment of
      taCenter:
        R.Left := (ARect.Right + ARect.Left - CaptionSz.cx) div 2; // Horizontally centered
      taLeftJustify:
        R.Left := ARect.Left;
      taRightJustify:
        R.Left := ARect.Right - CaptionSz.cx;
    end;
    R.Right := R.Left + CaptionSz.cx;
    if PushedCaption then
      OffsetRect(R, 1, 1);
    ACaptionRect := R;
  end;

  // AGlyphRect
  if (GlyphWidth > 0) and (GlyphHeight > 0) then begin
    TextOffset := Point(0, 0);
    R := ARect;

    // If ghlGlyphTop is used the glyph should be centered
    if Layout = ghlGlyphTop then
      CaptionAlignment := taCenter;

    case CaptionAlignment of
      taCenter:
        begin
          // Total width = Icon + Space + Text
          if Layout = ghlGlyphLeft then begin
            AGlyphRect.Left := (R.Right + R.Left - (GlyphWidth + Spacing.X + CaptionSz.cx)) div 2;
            TextOffset.X := (GlyphWidth + Spacing.X) div 2;
          end
          else
            AGlyphRect.Left := (R.Right + R.Left - GlyphWidth) div 2;
        end;
      taLeftJustify:
        begin
          AGlyphRect.Left := R.Left;
          TextOffset.X := GlyphWidth + Spacing.X;
        end;
      taRightJustify:
        begin
          AGlyphRect.Left := R.Right - GlyphWidth;
          TextOffset.X := - Spacing.X - GlyphWidth;
        end;
    end;

    if Layout = ghlGlyphLeft then
      AGlyphRect.Top := (R.Top + R.Bottom - GlyphHeight) div 2
    else begin
      AGlyphRect.Top := (R.Top + R.Bottom - (GlyphHeight + Spacing.Y + CaptionSz.cy)) div 2;
      Inc(TextOffset.Y, (GlyphHeight + Spacing.Y) div 2);
    end;
    AGlyphRect.Right := AGlyphRect.Left + GlyphWidth;
    AGlyphRect.Bottom := AGlyphRect.Top + GlyphHeight;
    if PushedCaption then
      OffsetRect(AGlyphRect, 1, 1);
  end;

  // Move the text according to the icon position
  if Result > 0 then
    OffsetRect(ACaptionRect, TextOffset.X, TextOffset.Y);
end;

function SpDrawXPText(ACanvas: TCanvas; Caption: WideString; var ARect: TRect;
  Flags: Cardinal; CaptionGlow: TSpGlowDirection = gldNone;
  CaptionGlowColor: TColor = clYellow; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;

  function InternalDraw(var R: TRect): Integer;
  var
    S: string;
  begin
    Result := 0;
    case RotationAngle of
      tra0:
        if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
          S := Caption;
          Result := Windows.DrawText(ACanvas.Handle, PChar(S), -1, R, Flags);
        end
        else
          Result := Windows.DrawTextW(ACanvas.Handle, PWideChar(Caption), -1, R, Flags);
      tra90, tra270:
        Result := SpDrawRotatedText(ACanvas.Handle, Caption, R, Flags, RotationAngle);
    end;
  end;

var
  BS: TBrushStyle;
  GlowR: TRect;
  C, FC: TColor;
begin
  BS := ACanvas.Brush.Style;
  C := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Style := bsClear;

    if Pos('utf8=', Caption) = 1 then begin
      Delete(Caption, 1, 5);
      Caption := UTF8Decode(Caption);
    end;

    if (Flags and DT_CALCRECT = 0) and (CaptionGlow <> gldNone) then begin
      FC := ACanvas.Font.Color;
      ACanvas.Font.Color := CaptionGlowColor;
      case CaptionGlow of
        gldAll:
          begin
            GlowR := ARect; OffsetRect(GlowR, 0, -1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 0, 1);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, -1, 0);
            InternalDraw(GlowR);
            GlowR := ARect; OffsetRect(GlowR, 1, 0);
          end;
        gldTopLeft:
          begin
            GlowR := ARect; OffsetRect(GlowR, -1, -1);
            InternalDraw(GlowR);
          end;
        gldBottomRight:
          begin
            GlowR := ARect; OffsetRect(GlowR, 1, 1);
            InternalDraw(GlowR);
          end;
      end;
      ACanvas.Font.Color := FC;
    end;

    Result := InternalDraw(ARect);

    if IsRectEmpty(ARect) then
      Result := 0
    else
      if Flags and DT_CALCRECT <> 0 then begin
        // [Bugfix] Windows bug:
        // When DT_CALCRECT is used and the font is italic the
        // resulting rect is incorrect
        if fsItalic in ACanvas.Font.Style then
          ARect.Right := ARect.Right + 1 + (ACanvas.Font.Size div 8) * 2;
      end;

  finally
    ACanvas.Brush.Style := BS;
    ACanvas.Brush.Color := C;
  end;
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; Glyph: TBitmap; Layout: TSpGlyphLayout; PushedCaption: Boolean;
  out ACaptionRect, AGlyphRect: TRect; CalcRectOnly: Boolean = False;
  RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
begin
  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, Glyph.Width,
    Glyph.Height, Layout, PushedCaption, ACaptionRect, AGlyphRect, RotationAngle);
  if not CalcRectOnly then begin
    SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);
    if Assigned(Glyph) then
      ACanvas.Draw(AGlyphRect.Left, AGlyphRect.Top, Glyph);
  end;
end;

function SpDrawXPText(ACanvas: TCanvas; ARect: TRect; Caption: WideString;
  CaptionGlow: TSpGlowDirection; CaptionGlowColor: TColor; CaptionAlignment: TAlignment;
  Flags: Cardinal; IL: TCustomImageList; ImageIndex: Integer; Layout: TSpGlyphLayout;
  Enabled, PushedCaption, DisabledIconCorrection: Boolean; out ACaptionRect, AGlyphRect: TRect;
  CalcRectOnly: Boolean = False; RotationAngle: TSpTextRotationAngle = tra0): Integer; overload;
var
  W, H: Integer;
begin
  if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then begin
    W := IL.Width;
    H := IL.Height;
  end
  else begin
    W := 0;
    H := 0;
  end;

  Result := SpCalcXPText(ACanvas, ARect, Caption, CaptionAlignment, Flags, W,
    H, Layout, PushedCaption, ACaptionRect, AGlyphRect, RotationAngle);
  if not CalcRectOnly then begin
    SpDrawXPText(ACanvas, Caption, ACaptionRect, Flags and not DT_CALCRECT, CaptionGlow, CaptionGlowColor, RotationAngle);
    if Assigned(IL) and (ImageIndex > -1) and (ImageIndex < IL.Count) then
      if Enabled or not DisabledIconCorrection then IL.Draw(ACanvas, AGlyphRect.Left, AGlyphRect.Top, ImageIndex)
      else DrawTBXImage(ACanvas, AGlyphRect, IL, ImageIndex, ISF_DISABLED);
  end;
end;

function SpGetTextSize(DC: HDC; WS: WideString; NoPrefix: Boolean): TSize;
// Returns the size of the string, if NoPrefix is True, it first removes "&"
// characters as necessary.
// This procedure is 10x faster than using DrawText with the DT_CALCRECT flag
var
  S: AnsiString;
begin
  Result.cx := 0;
  Result.cy := 0;
  if NoPrefix then
    WS := SpStripAccelChars(WS);
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
    S := WS;
    Windows.GetTextExtentPoint32A(DC, PChar(S), Length(S), Result);
  end
  else
    Windows.GetTextExtentPoint32W(DC, PWideChar(WS), Length(WS), Result)
end;

function SpSameText(W1, W2: WideString): Boolean;
begin
  Result := (TntWindows.Tnt_CompareStringW(GetThreadLocale, NORM_IGNORECASE,
    PWideChar(W1), Length(W1), PWideChar(W2), Length(W2)) - 2) = 0;
end;

function SpStripAccelChars(S: WideString): WideString;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '&' then
      System.Delete(Result, I, 1);
    Inc(I);
  end;
end;

function SpStripShortcut(S: WideString): WideString;
var
  P: Integer;
begin
  Result := S;
  P := Pos(#9, Result);
  if P <> 0 then
    SetLength(Result, P - 1);
end;

function SpStripTrailingPunctuation(S: WideString): WideString;
// Removes any colon (':') or ellipsis ('...') from the end of S and returns
// the resulting string
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if (L > 1) and (Result[L] = ':') then
    SetLength(Result, L-1)
  else if (L > 3) and (Result[L-2] = '.') and (Result[L-1] = '.') and
     (Result[L] = '.') then
    SetLength(Result, L-3);
end;

function SpRectToString(R: TRect): string;
begin
  Result := Format('%d, %d, %d, %d', [R.Left, R.Top, R.Right, R.Bottom]);
end;

function SpStringToRect(S: string; out R: TRect): Boolean;
var
  L: TStringList;
begin
  Result := False;
  R := Rect(0, 0, 0, 0);
  L := TStringList.Create;
  try
    L.CommaText := S;
    if L.Count = 4 then begin
      R.Left := StrToIntDef(L[0], 0);
      R.Top := StrToIntDef(L[1], 0);
      R.Right := StrToIntDef(L[2], 0);
      R.Bottom := StrToIntDef(L[3], 0);
      Result := True;
    end;
  finally
    L.Free;
  end;
end;

function SpColorToString(C: TColor): string;
begin
  if not ColorToIdent(C, Result) then
    FmtStr(Result, '$%.8x', [C]);
end;

function SpGetDirectories(Path: WideString; L: TTntStrings): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := False;
  if TntSysUtils.WideDirectoryExists(Path) then begin
    Path := TntSysUtils.WideIncludeTrailingBackslash(Path) + '*.*';
    if FindFirst(Path, faDirectory, SearchRec) = 0 then begin
      try
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            L.Add(SearchRec.Name);
        until FindNext(SearchRec) <> 0;
        Result := True;
      finally
        FindClose(SearchRec);
      end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Menu helpers }

function SpCalcPopupPosition(const X, Y, Width, Height: Integer;
  PopupControl: TControl = nil; IsVertical: Boolean = False): TPoint;
var
  R, MonitorR: TRect;
begin
  if Assigned(PopupControl) then begin
    Result := Point(0, 0);
    if PopupControl.Parent = nil then Exit;

    R := PopupControl.BoundsRect;
    R.TopLeft := PopupControl.Parent.ClientToScreen(R.TopLeft);
    R.BottomRight := PopupControl.Parent.ClientToScreen(R.BottomRight);

    if IsVertical then
      Result := Point(R.Right, R.Top)
    else
      Result := Point(R.Left, R.Bottom);

    MonitorR := GetRectOfMonitorContainingPoint(Result, True);

    if IsVertical then begin
      if Result.X + Width > MonitorR.Right then
        Result.X := R.Left - Width;
      if Result.Y + Height > MonitorR.Bottom then
        if R.Bottom > MonitorR.Bottom then
          Result.Y := MonitorR.Bottom - Height
        else
          Result.Y := R.Bottom - Height;
    end
    else begin
      if Result.X + Width > MonitorR.Right then
        if R.Right > MonitorR.Right then
          Result.X := MonitorR.Right - Width
        else
          Result.X := R.Right - Width;
      if Result.Y + Height > MonitorR.Bottom then
        Result.Y := R.Top - Height;
    end;
  end
  else begin
    Result := Point(X, Y);
    MonitorR := GetRectOfMonitorContainingPoint(Result, True);
    if X + Width > MonitorR.Right then
      Result.X := X - Width;
    if Y + Height > MonitorR.Bottom then
      Result.Y := Y - Height;
  end;
end;

function SpHMenuGetCaption(Menu: HMenu; Index: Integer): WideString;
var
  AnsiBuf: array[0..MAX_PATH] of Char;
  WideBuf: array[0..MAX_PATH] of WideChar;
  Size: Integer;
begin
  Result := '';
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then begin
    FillChar(AnsiBuf, MAX_PATH, #0);
    GetMenuString(Menu, Index, @AnsiBuf, MAX_PATH, MF_BYPOSITION);
    Size := lstrlenA(@AnsiBuf);
    Result := AnsiBuf;
    SetLength(Result, Size);
  end
  else begin
    // [Bugfix] Windows bug:
    // GetMenuStringW when a DBCS code page is active (e.g. Japanese)
    // the result of the function is incorrect (it returns Size * 2)
    // http://news.jrsoftware.org/read/article.php?id=12268&group=jrsoftware.toolbar2000.thirdparty
    FillChar(WideBuf, MAX_PATH, #0);
    GetMenuStringW(Menu, Index, @WideBuf, MAX_PATH, MF_BYPOSITION);
    Size := lstrlenW(@WideBuf);
    Result := WideBuf;
    SetLength(Result, Size);
  end;
end;

function SpHMenuToTBMenuItem(Menu: HMenu; ParentItem: TTBCustomItem): Boolean;
var
  MenuInfo: TMenuItemInfo;
  I, C: Integer;
  Item: TSpTBXItem;
  HasSubMenu: Boolean;
begin
  Result := False;
  if not Assigned(ParentItem) or not IsMenu(Menu) then Exit;

  C := GetMenuItemCount(Menu);

  for I := 0 to C - 1 do begin
    FillChar(MenuInfo, SizeOf(MenuInfo), #0);
    MenuInfo.cbSize := SizeOf(MenuInfo);
    MenuInfo.fMask := MIIM_TYPE or MIIM_STATE or MIIM_ID or MIIM_SUBMENU;
    GetMenuItemInfo(Menu, I, True, MenuInfo);

    if MenuInfo.fType and MFT_SEPARATOR <> 0 then
      ParentItem.Add(TSpTBXSeparatorItem.Create(nil))
    else begin
      HasSubmenu := IsMenu(MenuInfo.hSubMenu);
      if HasSubmenu then
        Item := TSpTBXSubmenuItem.Create(nil)
      else
        Item := TSpTBXItem.Create(nil);


      Item.Caption := SpHMenuGetCaption(Menu, I);
      Item.Tag := MenuInfo.wID;
      if MenuInfo.fState and MFS_DISABLED <> 0 then
        Item.Enabled := False;
      if MenuInfo.fState and MFS_CHECKED <> 0 then
        Item.Checked := True;
      if MenuInfo.fState and MFS_DEFAULT <> 0 then
        Item.Options := Item.Options + [tboDefault];
      ParentItem.Add(Item);
    end;
  end;

  Result := True;
end;

function SpShowSystemPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint; DoDefault: Boolean = True): Integer;
var
  SysMenu: HMENU;
begin
  ReleaseCapture;
  SysMenu := GetSystemMenu(ParentForm.Handle, False);
  case ParentForm.WindowState of
    wsMaximized:
      begin
        EnableMenuItem(SysMenu, SC_RESTORE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_MAXIMIZE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_MOVE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_SIZE, MF_GRAYED);
      end;
    wsNormal:
      begin
        EnableMenuItem(SysMenu, SC_RESTORE, MF_GRAYED);
        EnableMenuItem(SysMenu, SC_MAXIMIZE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_MOVE, MF_ENABLED);
        EnableMenuItem(SysMenu, SC_SIZE, MF_ENABLED);
      end;
  end;
  Result := Integer(TrackPopupMenuEx(SysMenu, TPM_LEFTALIGN or TPM_RETURNCMD or
    TPM_RIGHTBUTTON or TPM_HORIZONTAL or TPM_VERTICAL, ScreenPos.X, ScreenPos.Y, ParentForm.Handle, nil));
  if DoDefault then
    case Result of
      SC_MAXIMIZE: ParentForm.WindowState := wsMaximized;
      SC_RESTORE: ParentForm.WindowState := wsNormal;
    else
      // WindowState := wsMinimized will not minimize the app correctly
      SendMessage(ParentForm.Handle, WM_SYSCOMMAND, Result, 0);
    end;
end;

function SpFillSystemSpTBXPopupMenu(ParentForm: TCustomForm; ShowSize, ShowMinimize,
  ShowMaximize, ShowClose: Boolean; OutPopup: TSpTBXPopupMenu): Boolean;
var
  Menu: HMENU;
  I: Integer;
  Item: TTBCustomItem;
begin
  Result := False;
  Menu := GetSystemMenu(ParentForm.Handle, False);

  if SpHMenuToTBMenuItem(Menu, OutPopup.Items) then begin
    OutPopup.Images := MDIButtonsImgList;
    for I := 0 to OutPopup.Items.Count - 1 do begin
      Item := OutPopup.Items[I];
      Item.Enabled := True;
      case Item.Tag of
        SC_MINIMIZE:
          begin
            Item.Visible := ShowMinimize;
            Item.ImageIndex := 2;
          end;
        SC_RESTORE:
          begin
            Item.Visible := ShowMaximize;
            Item.Enabled := ParentForm.WindowState <> wsNormal;
            Item.ImageIndex := 3;
          end;
        SC_MAXIMIZE:
          begin
            Item.Visible := ShowMaximize;
            Item.Enabled := ParentForm.WindowState <> wsMaximized;
            Item.ImageIndex := 1;
          end;
        SC_CLOSE:
          begin
            Item.Visible := ShowClose;
            Item.Options := Item.Options + [tboDefault];
            Item.ImageIndex := 0;
          end;
        SC_MOVE:
          begin
            Item.Enabled := ParentForm.WindowState <> wsMaximized;
          end;
        SC_SIZE:
          begin
            Item.Visible := ShowSize;
            Item.Enabled := ParentForm.WindowState <> wsMaximized;
          end;
      end;
    end;

    OutPopup.PopupComponent := ParentForm;
    Result := True;
  end;
end;

function SpShowSystemSpTBXPopupMenu(ParentForm: TCustomForm; ScreenPos: TPoint;
  ShowSize, ShowMinimize, ShowMaximize, ShowClose: Boolean;
  PopupEvent: TSpTBXPopupEvent; DoDefault: Boolean = True): Integer;
var
  Popup: TSpTBXPopupMenu;
  ClickedItem: TTBCustomItem;
begin
  Result := 0;
  ReleaseCapture;

  Popup := TSpTBXPopupMenu.Create(ParentForm);
  try
    if SpFillSystemSpTBXPopupMenu(ParentForm, ShowSize, ShowMinimize, ShowMaximize, ShowClose, Popup) then begin
      if Assigned(PopupEvent) then
        Popup.OnInitPopup := PopupEvent;
      Popup.PopupComponent := ParentForm;
      ClickedItem := Popup.PopupEx(ScreenPos.X, ScreenPos.Y, nil, True);
      if Assigned(ClickedItem) then begin
        Result := ClickedItem.Tag;
        // If it's not a SystemMenu item fire the OnClick event of the item
        // We can't use PostClick because the Item will be destroyed by the
        // time the message is handled.
        if (Result < SC_SIZE) or (Result > SC_CONTEXTHELP) then
          ClickedItem.Click;
      end;
    end;
  finally
    Popup.Free;
  end;

  if DoDefault and (Result > 0) then
    case Result of
      SC_MAXIMIZE:
        ParentForm.WindowState := wsMaximized;
      SC_RESTORE:
        ParentForm.WindowState := wsNormal;
      SC_SIZE, SC_MOVE, SC_MINIMIZE, SC_CLOSE:
        begin
          // WindowState := wsMinimized will not minimize the app correctly
          SendMessage(ParentForm.Handle, WM_SYSCOMMAND, Result, 0);
        end;
    end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Misc helpers }

function SpCanFocus(WinControl: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if Assigned(WinControl) and not WinControl.Focused then begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      Result := WinControl.CanFocus;
  end;
end;

function SpIsFocused(WinControl: TWinControl; out FocusedChild: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  FocusedChild := nil;
  if WinControl.Focused then
    Result := True
  else begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      if Assigned(Form.ActiveControl) and Form.ActiveControl.Focused then
        if IsChild(WinControl.Handle, Form.ActiveControl.Handle) then begin
          Result := True;
          FocusedChild := Form.ActiveControl;
        end;
  end;
end;

function SpFocusFirstChild(WinControl: TWinControl): TWinControl;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetParentForm(WinControl);
  if Assigned(Form) and Form.Enabled and Form.Visible then begin
    TWinControlAccess(WinControl).SelectFirst;
    Result := Form.ActiveControl;
  end;
end;

function SpFindControl(Parent: TWinControl; Child: TControl): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I] = Child then begin
      Result := I;
      Break;
    end;
end;

function SpFindParent(Control: TControl; ParentClass: TClass): TWinControl;
var
  P: TWinControl;
begin
  Result := nil;
  if Assigned(Control) then begin
    P := Control.Parent;
    while Assigned(P) do
      if P is ParentClass then begin
        Result := P;
        Break;
      end
      else
        P := P.Parent;
  end;
end;

function SpGetFormWindowState(F: TCustomForm; out RestoreBoundsRect: TRect): TWindowState;
// This method is more accurate than Form.WindowState
var
  P: TWindowPlacement;
begin
  Result := wsNormal;
  RestoreBoundsRect := Rect(0, 0, 0, 0);
  if Assigned(F) and (F.HandleAllocated) then begin
    P.Length := SizeOf(TWindowPlacement);
    if GetWindowPlacement(F.Handle, @P) then begin
      case P.showCmd of
        SW_SHOWMINIMIZED: Result := wsMinimized;
        SW_SHOWMAXIMIZED: Result := wsMaximized;
      end;
      // rcNormalPosition contains the window's coordinates when the window is in the restored position
      with P.rcNormalPosition do
        RestoreBoundsRect := Rect(Left, Top, Right - Left, Bottom - Top);
    end;
  end;
end;

procedure SpSetFormWindowState(F: TCustomForm; WindowState: TWindowState; RestoreBoundsRect: TRect);
// This method is more accurate than Form.WindowState
var
  P: TWindowPlacement;
begin
  if Assigned(F) and (F.HandleAllocated) then begin
    P.Length := SizeOf(TWindowPlacement);
    case WindowState of
      wsMinimized: P.showCmd := SW_SHOWMINIMIZED;
      wsMaximized: P.showCmd := SW_SHOWMAXIMIZED;
    else
      P.showCmd := SW_SHOWNORMAL;
    end;
    // rcNormalPosition contains the window's coordinates when the window is in the restored position
    if not IsRectEmpty(RestoreBoundsRect) then
      with RestoreBoundsRect do
        P.rcNormalPosition := Bounds(Left, Top, Right, Bottom);

    SetWindowPlacement(F.Handle, @P);
  end;
end;

function SpGetTaskBar(out State, Edge: Cardinal; out Bounds: TRect): Boolean;
// Returns the TaskBar state and bounds
// State can be: 0, ABS_ALWAYSONTOP, ABS_AUTOHIDE
// Edge can be: ABE_LEFT, ABE_RIGHT, ABE_TOP, ABE_BOTTOM
// ABM_GETSTATE

var
	AppData: TAppBarData;
begin
  Result := False;
  State := 0;
  Edge := 0;
  Bounds := Rect(0, 0, 0, 0);

  // 'Shell_TrayWnd' is the name of the task bar's window
  AppData.Hwnd := FindWindow('Shell_TrayWnd', nil);
  if AppData.Hwnd <> 0 then begin
    AppData.cbSize := SizeOf(TAppBarData);
    if SHAppBarMessage(ABM_GETTASKBARPOS, AppData) <> 0 then begin
      Edge := AppData.uEdge;
      Bounds := AppData.rc;

      AppData.cbSize := SizeOf(TAppBarData);
      State := SHAppBarMessage(ABM_GETSTATE, AppData);

      Result := True;
    end;
  end;
end;

procedure SpCustomizeAllToolbars(AParentComponent: TComponent; Reset: Boolean);
var
  I: Integer;
  TB: TSpTBXToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TSpTBXToolbar then begin
        TB := AParentComponent.Components[I] as TSpTBXToolbar;
        if Reset then
          TB.EndCustomize
        else
          TB.BeginCustomize;
      end;
  end;
end;

procedure SpBeginUpdateAllToolbars(AParentComponent: TComponent);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TTBCustomToolbar then begin
        TB := AParentComponent.Components[I] as TTBCustomToolbar;
        TB.BeginUpdate;
      end;
  end;
end;

procedure SpEndUpdateAllToolbars(AParentComponent: TComponent);
var
  I: Integer;
  TB: TTBCustomToolbar;
begin
  if Assigned(AParentComponent) then begin
    for I := 0 to AParentComponent.ComponentCount - 1 do
      if AParentComponent.Components[I] is TTBCustomToolbar then begin
        TB := AParentComponent.Components[I] as TTBCustomToolbar;
        TB.View.UpdatePositions;
        TB.EndUpdate;
        TB.Invalidate;
      end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomDragObject }

constructor TSpTBXCustomDragObject.Create(ASourceControl: TControl; AItem: TTBCustomItem);
begin
  FSourceControl := ASourceControl;
  FSourceItem := AItem;
  FDragCursorAccept := crSpTBXCustomization;
  FDragCursorCancel := crNo;
end;

procedure TSpTBXCustomDragObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
  inherited;
  if not Accepted then begin
    if Assigned(FSourceControl) then
      TControlAccess(FSourceControl).DragCanceled;
    Target := nil;
  end;

  if Assigned(FSourceControl) then
    TControlAccess(FSourceControl).DoEndDrag(Target, X, Y);
end;

function TSpTBXCustomDragObject.GetDragCursor(Accepted: Boolean; X,
  Y: Integer): TCursor;
begin
  if Accepted then Result := DragCursorAccept
  else Result := DragCursorCancel;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomItemActionLink }

procedure TSpTBXCustomItemActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FUnicodeClient := AClient as TSpTBXCustomItem;
end;

function TSpTBXCustomItemActionLink.IsCaptionLinked: Boolean;
begin
  if Action is TTntAction then
    Result := (Action is TCustomAction) and (FUnicodeClient.Caption = (Action as TTntAction).Caption)
  else
    Result := inherited IsCaptionLinked;
end;

function TSpTBXCustomItemActionLink.IsHintLinked: Boolean;
begin
  if Action is TTntAction then
    Result := (Action is TCustomAction) and (FUnicodeClient.Hint = (Action as TTntAction).Hint)
  else
    Result := inherited IsCaptionLinked;
end;

procedure TSpTBXCustomItemActionLink.SetCaption(const Value: String);
begin
  if IsCaptionLinked then
    if Action is TTntAction then
      FUnicodeClient.Caption := TntActnList.TntAction_GetNewCaption(Action as TCustomAction, Value)
    else
      FUnicodeClient.Caption := Value;
end;

procedure TSpTBXCustomItemActionLink.SetHint(const Value: String);
begin
  if IsHintLinked then
    if Action is TTntAction then
      FUnicodeClient.Hint := TntActnList.TntAction_GetNewHint(Action as TCustomAction, Value)
    else
      FUnicodeClient.Hint := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomItem }

constructor TSpTBXCustomItem.Create(AOwner: TComponent);
begin
  inherited;
  FCaption := '';
  FAlignment := taCenter;
  FCaptionGlowColor := clYellow;
  FCustomWidth := -1;
  FCustomHeight := -1;
  FMargins := 0;
  FWrapping := twWrap;

  Stretch := True;
end;

procedure TSpTBXCustomItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TSpTBXCustomItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  // For backwards compatibility
  Filer.DefineProperty('CaptionW', ReadCaptionW, nil, False);
  Filer.DefineProperty('HintW', ReadHintW, nil, False);
  // Don't let the streaming system store the WideStrings
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TSpTBXCustomItem.ReadCaptionW(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetCaption(Reader.ReadString);
  else
    SetCaption(Reader.ReadWideString);
  end;
end;

procedure TSpTBXCustomItem.ReadHintW(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetHint(Reader.ReadString);
  else
    SetHint(Reader.ReadWideString);
  end;
end;

procedure TSpTBXCustomItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TTntAction then begin
    with TTntAction(Sender) do begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
    end;
  end
  else
    if Action is TCustomAction then
      with TCustomAction(Sender) do begin
        if not CheckDefaults or (Self.Caption = '') then
          Self.Caption := Caption;
        if not CheckDefaults or (Self.Hint = '') then
          Self.Hint := Hint;
      end;

  inherited;
end;

function TSpTBXCustomItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := False;
end;

procedure TSpTBXCustomItem.DoDrawAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  // Do nothing
end;

procedure TSpTBXCustomItem.DoDrawHint(AHintBitmap: TBitmap;
  var AHint: Widestring; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawHint) then FOnDrawHint(Self, AHintBitmap, AHint, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawItem) then FOnDrawItem(Self, ACanvas, ARect, ItemInfo, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ClientAreaRect: TRect; var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawCaption) then FOnDrawCaption(Self, ACanvas, ClientAreaRect,
    ACaption, CaptionRect, CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
end;

procedure TSpTBXCustomItem.DoDrawImage(ACanvas: TCanvas;
  const ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
  AImageList: TCustomImageList; var AImageIndex: integer;
  var ARect: TRect; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawImage) then FOnDrawImage(Self, ACanvas, ItemInfo, PaintStage,
    AImageList, AImageIndex, ARect, PaintDefault);
end;

function TSpTBXCustomItem.GetActionLinkClass: TTBCustomItemActionLinkClass;
begin
  Result := TSpTBXCustomItemActionLink;
end;

function TSpTBXCustomItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXItemViewer;
end;

function TSpTBXCustomItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TSpTBXPopupWindow;
end;

function TSpTBXCustomItem.GetShortCutText: WideString;
var
  P: Integer;
begin
  P := Pos(#9, Caption);
  {if P = 0 then begin} {vb-}
  if (P = 0) or (P = Length(Caption)) then begin {vb+}
    if ShortCut <> 0 then
      Result := ShortCutToText(ShortCut)
    else
      Result := '';
  end
  else
    Result := Copy(Caption, P+1, Maxint);
end;

procedure TSpTBXCustomItem.Click;
begin
  if Assigned(FControl) then ToggleControl;
  inherited;
end;

procedure TSpTBXCustomItem.InitiateAction;
begin
  inherited;
  UpdateProps;
end;

procedure TSpTBXCustomItem.SetCaption(const Value: WideString);
var
  S, PrevS: string;
begin
  if FCaption <> Value then begin
    FCaption := Value;

    // We need to compare the Ansi inherited Caption
    // to force the change.
    // Sometimes '???' = '???' and the change is not executed.
    S := inherited Caption;
    PrevS := Value;
    if S <> PrevS then
      inherited Caption := Value
    else
      Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetHint(const Value: WideString);
begin
  if FHint <> Value then begin
    FHint := Value;
    inherited Hint := Value;
  end;
end;

procedure TSpTBXCustomItem.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetAnchored(const Value: Boolean);
begin
  if FAnchored <> Value then begin
    FAnchored := Value;
  end;
end;

procedure TSpTBXCustomItem.SetCaptionGlow(const Value: TSpGlowDirection);
begin
  if FCaptionGlow <> Value then begin
    FCaptionGlow := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetCaptionGlowColor(const Value: TColor);
begin
  if FCaptionGlowColor <> Value then begin
    FCaptionGlowColor := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    FControl := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    UpdateProps;
  end;
end;

procedure TSpTBXCustomItem.SetCustomWidth(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if FCustomWidth <> Value then begin
    FCustomWidth := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetCustomHeight(Value: Integer);
begin
  if Value < -1 then Value := -1;
  if FCustomHeight <> Value then begin
    FCustomHeight := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetMargins(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMargins <> Value then begin
    FMargins := Value;
    Change(True);
  end;
end;

procedure TSpTBXCustomItem.SetWrapping(const Value: TTextWrapping);
begin
  if FWrapping <> Value then begin
    FWrapping := Value;
    Change(False);
  end;
end;

procedure TSpTBXCustomItem.ToggleControl;
begin
  FControl.Visible := not FControl.Visible;
end;

procedure TSpTBXCustomItem.UpdateProps;
begin
  if Assigned(Control) then
    if (ComponentState * [csDesigning, csLoading, csDestroying] = []) then
      Checked := Control.Visible;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemViewer }

function TSpTBXItemViewer.CaptionShown: Boolean;
var
  T: TSpTBXToolbar;
begin
  Result := inherited CaptionShown;

  if Assigned(View) and Assigned(View.Owner) and (View.Owner is TSpTBXToolbar) then begin
    T := View.Owner as TSpTBXToolbar;
    case T.DisplayMode of
      tbdmImageOnly:
        if GetImageShown then Result := False;
      tbdmTextOnly:
        Result := True;
    end;
  end;
end;

function TSpTBXItemViewer.GetImageShown: Boolean;
var
  T: TSpTBXToolbar;
begin
  Result := inherited GetImageShown;

  if Assigned(View) and Assigned(View.Owner) and (View.Owner is TSpTBXToolbar) then begin
    T := View.Owner as TSpTBXToolbar;
    case T.DisplayMode of
      tbdmTextOnly:
        Result := False;
    end;
  end;
end;

procedure TSpTBXItemViewer.DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  Item.DoDrawButton(ACanvas, ItemInfo, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXItemViewer.DoDrawCaption(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ClientAreaRect: TRect; var ACaption: WideString; var CaptionRect: TRect; var CaptionFormat: Cardinal;
  IsTextRotated: Boolean; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
var
  Angle: TSpTextRotationAngle;
begin
  Item.DoDrawCaption(ACanvas, ItemInfo, ClientAreaRect, ACaption, CaptionRect,
    CaptionFormat, IsTextRotated, PaintStage, PaintDefault);
  if PaintDefault and (PaintStage = pstPrePaint) then begin
    PaintDefault := False;
    if IsTextRotated then
      Angle := tra270
    else
      Angle := tra0;
    SpDrawXPText(ACanvas, ACaption, CaptionRect, CaptionFormat, Item.CaptionGlow, Item.CaptionGlowColor, Angle);
  end;
end;

procedure TSpTBXItemViewer.DoDrawImage(ACanvas: TCanvas;
  const ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
  AImageList: TCustomImageList; var AImageIndex: integer;
  var ARect: TRect; var PaintDefault: Boolean);
begin
  Item.DoDrawImage(ACanvas, ItemInfo, PaintStage, AImageList, AImageIndex, ARect, PaintDefault);
end;

procedure TSpTBXItemViewer.DoAdjustFont(AFont: TFont; StateFlags: Integer);
begin
  Item.DoDrawAdjustFont(AFont, StateFlags);
  inherited;
end;

procedure TSpTBXItemViewer.DrawItemImage(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo);
var
  PaintDefault: Boolean;
  ImgList: TCustomImageList;
  ImgIndex: integer;
begin
  ImgList := GetImageList;
  ImgIndex := Item.ImageIndex;

  PaintDefault := True;
  DoDrawImage(Canvas, ItemInfo, pstPrePaint, ImgList, ImgIndex, ARect, PaintDefault);
  if PaintDefault and Assigned(ImgList) and (ImgIndex >= 0) and (ImgIndex < ImgList.Count) then
    CurrentTheme.PaintImage(Canvas, ARect, ItemInfo, ImgList, ImgIndex);

  PaintDefault := True;
  DoDrawImage(Canvas, ItemInfo, pstPostPaint, ImgList, ImgIndex, ARect, PaintDefault);
end;

procedure TSpTBXItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
var
  I, W, H: Integer;
  AnsiS: string;
  WideS: WideString;
begin
  inherited CalcSize(Canvas, AWidth, AHeight);

  // Recalculate the width if the shortcut is set, we need to do this in case
  // the shortcut is unicode
  if not IsToolbarStyle then begin
    WideS := Item.GetShortCutText;
    if Length(WideS) > 0 then begin
      // Decrease the Ansi shortcut width (TTBXItemViewer.CalcSize)
      AnsiS := WideS;
      Dec(AWidth, (AHeight - 6) + GetTextWidth(Canvas.Handle, AnsiS, True));
      // Increase the Wide shortcut width
      Inc(AWidth, (AHeight - 6) + SpGetTextSize(Canvas.Handle, WideS, True).cx);
    end;
  end;

  if IsRotated then begin
    // Reverse
    H := AWidth + Item.Margins;
    W := AHeight;
  end
  else begin
    W := AWidth + Item.Margins;
    H := AHeight;
  end;

  if Item.CustomWidth > -1 then
    W := Item.CustomWidth;
  if Item.CustomHeight > -1 then
    H := Item.CustomHeight;
  if IsToolbarStyle and Item.Anchored then
    W := W + FAnchorDelta;
  if W < Item.MinWidth then W := Item.MinWidth;
  if H < Item.MinHeight then H := Item.MinHeight;
  // Apply View.MaxSize to the height of the item
  if View is TSpTBXToolbarView then begin
    I := TSpTBXToolbarView(View).MaxSize;
    if (I > -1) and (H > I) then
      H := I;
  end;

  if IsRotated then begin
    // Reverse
    AWidth := H;
    AHeight := W;
  end
  else begin
    AWidth := W;
    AHeight := H;
  end;
end;

function TSpTBXItemViewer.GetCaptionText: WideString;
begin
  Result := SpStripShortcut(Item.Caption);
end;

function TSpTBXItemViewer.GetTextSize(Canvas: TCanvas;
  const Text: string; TextFlags: Cardinal; Rotated: Boolean;
  StateFlags: Integer): TSize;
begin
  TextFlags := TextFlags and not (DT_SINGLELINE or DT_WORDBREAK or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS);
  Result := GetRealTextSize(Canvas, GetCaptionText, TextFlags, Rotated, StateFlags, BoundsRect);
end;

function TSpTBXItemViewer.GetRealTextSize(Canvas: TCanvas;
  const Text: WideString; TextFlags: Cardinal; Rotated: Boolean;
  StateFlags: Integer; CaptionRect: TRect): TSize;
var
  R: TRect;
  Angle: TSpTextRotationAngle;
begin
  Result.CX := 0;
  Result.CY := 0;

  { Select proper font }
  Canvas.Font := TTBViewAccess(View).GetFont;
  DoAdjustFont(Canvas.Font, StateFlags);

  TextFlags := TextFlags and not DT_SINGLELINE;
  if (TextFlags and (DT_WORDBREAK or DT_END_ELLIPSIS or DT_PATH_ELLIPSIS)) <> 0 then
    R := Rect(0, 0, CaptionRect.Right - CaptionRect.Left, 1)
  else
    R := Rect(0, 0, 1, 1);

  if Rotated then
    Angle := tra270
  else
    Angle := tra0;

  SpDrawXPText(Canvas, Text, R, TextFlags or DT_CALCRECT, gldNone, clYellow, Angle);
  Result.CX := R.Right;
  Result.CY := R.Bottom;
end;

procedure TSpTBXItemViewer.Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
  IsHoverItem, IsPushed, UseDisabledShadow: Boolean);
var
  View: TTBViewAccess;
  ItemInfo: TTBXItemInfo;

  R: TRect;
  ComboRect: TRect;
  CaptionRect: TRect;
  ImageRect: TRect;
  C: TColor;

  ToolbarStyle: Boolean;
  HasArrow: Boolean;
  IsSplit: Boolean;
  ImageIsShown: Boolean;
  ImageOrCheckShown: Boolean;
  ImgAndArrowWidth: Integer;
  ImgSize: TSize;
  IsComboPushed: Boolean;
  IsCaptionShown: Boolean;
  IsTextRotated: Boolean;
  PaintDefault: Boolean;
  WS: WideString;
  StateFlags: Integer;
  IsSpecialDropDown: Boolean;
  TextFlags: Cardinal;
  TextMetrics: TTextMetric;
  TextSize: TSize;
  Margins: TTBXMargins;
  GlyphLayout: TSpGlyphLayout;
  TextAngle: TSpTextRotationAngle;
  OverrideArrowPainting: Boolean;
const
  WordWraps: array [TTextWrapping] of Cardinal = (0,
    DT_SINGLELINE or DT_END_ELLIPSIS,
    DT_SINGLELINE or DT_PATH_ELLIPSIS, DT_WORDBREAK);
begin
  CaptionRect := Rect(0, 0, 0, 0);
  ImageRect := Rect(0, 0, 0, 0);

  View := TTBViewAccess(Self.View);
  SpFillItemInfo(Self, ItemInfo);

  ToolbarStyle := IsToolbarStyle;
  IsSplit := tbisCombo in Item.ItemStyle;
  IsComboPushed := IsSplit and IsPushed and not View.Capture;
  if IsComboPushed then IsPushed := False;

  case Item.Layout of
    tbxlGlyphLeft:
      GlyphLayout := ghlGlyphLeft;
    tbxlGlyphTop:
      GlyphLayout := ghlGlyphTop;
    tbxlAuto:
      begin
        if tboImageAboveCaption in Item.EffectiveOptions then GlyphLayout := ghlGlyphTop
        else if View.Orientation <> tbvoVertical then GlyphLayout := ghlGlyphLeft
        else GlyphLayout := ghlGlyphTop;
      end;
  else
    GlyphLayout := ghlGlyphLeft;
  end;

  HasArrow := (tbisSubmenu in Item.ItemStyle) and
    ((tbisCombo in Item.ItemStyle) or (tboDropdownArrow in Item.EffectiveOptions));

  if GetImageShown then
  begin
    ImgSize := GetImageSize;
    with ImgSize do if (CX <= 0) or (CY <= 0) then
    begin
      CX := 0;
      CY := 0;
      ImageIsShown := False;
    end
    else ImageIsShown := True;
  end
  else
  begin
    ImgSize.CX := 0;
    ImgSize.CY := 0;
    ImageIsShown := False;
  end;
  ImageOrCheckShown := ImageIsShown or (not ToolbarStyle and Item.Checked);

  StateFlags := GetStateFlags(ItemInfo);

  Canvas.Font := TTBViewAccess(View).GetFont;
  Canvas.Font.Color := CurrentTheme.GetItemTextColor(ItemInfo);
  DoAdjustFont(Canvas.Font, StateFlags);  // Let the Item adjust the font
  C := Canvas.Font.Color;

  { Setup font }
  TextFlags := GetTextFlags or DT_VCENTER or WordWraps[Self.Item.Wrapping];
  IsCaptionShown := CaptionShown;
  IsTextRotated := (View.Orientation = tbvoVertical) and ToolbarStyle;
  if IsCaptionShown then begin
    WS := GetCaptionText;
    if (Item.Layout <> tbxlAuto) or (tboImageAboveCaption in Item.EffectiveOptions) then
      IsTextRotated := False;
    if IsTextRotated or not ToolbarStyle then TextFlags := TextFlags or DT_SINGLELINE;
    TextSize := GetTextSize(Canvas, WS, TextFlags, IsTextRotated, StateFlags);
  end
  else begin
    SetLength(WS, 0);
    IsTextRotated := False;
    TextSize.CX := 0;
    TextSize.CY := 0;
  end;
  if IsTextRotated then
    TextAngle := tra270
  else
    TextAngle := tra0;

  IsSpecialDropDown := HasArrow and not IsSplit and ToolbarStyle and
    ((Item.Layout = tbxlGlyphTop) or (Item.Layout = tbxlAuto) and (tboImageAboveCaption in Item.EffectiveOptions)) and
    (ImgSize.CX > 0) and not (IsTextRotated) and (Length(Item.Caption) > 0);

  { Border & Arrows }
  R := ClientAreaRect;
  if ToolbarStyle then begin
    CurrentTheme.GetMargins(MID_TOOLBARITEM, Margins);
    if HasArrow then begin
      ItemInfo.ComboPart := cpCombo;
      if IsSplit then begin
        ItemInfo.ComboPart := cpSplitLeft;
        ComboRect := R;
        Dec(R.Right, CurrentTheme.SplitBtnArrowWidth);
        ComboRect.Left := R.Right;
      end
      else
        if not IsSpecialDropDown then begin
          if View.Orientation <> tbvoVertical then
            ComboRect := Rect(R.Right - CurrentTheme.DropdownArrowWidth - CurrentTheme.DropdownArrowMargin, 0,
              R.Right - CurrentTheme.DropdownArrowMargin, R.Bottom)
          else
            ComboRect := Rect(0, R.Bottom - CurrentTheme.DropdownArrowWidth - CurrentTheme.DropdownArrowMargin,
              R.Right, R.Bottom - CurrentTheme.DropdownArrowMargin);
        end
        else begin
          ImgAndArrowWidth := ImgSize.CX + CurrentTheme.DropdownArrowWidth + 2;
          ComboRect.Right := (R.Left + R.Right + ImgAndArrowWidth + 2) div 2;
          ComboRect.Left := ComboRect.Right - CurrentTheme.DropdownArrowWidth;
          ComboRect.Top := (R.Top + R.Bottom - ImgSize.CY - 2 - TextSize.CY) div 2;
          ComboRect.Bottom := ComboRect.Top + ImgSize.CY;
        end;
    end
    else
      SetRectEmpty(ComboRect);

    if not IsSplit then begin
      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, R, pstPrePaint, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintButton(Canvas, R, ItemInfo);
      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, R, pstPostPaint, PaintDefault);

      if HasArrow then begin
        CurrentTheme.PaintDropDownArrow(Canvas, ComboRect, ItemInfo);
        if not IsSpecialDropDown then begin
          if View.Orientation <> tbvoVertical then Dec(R.Right, CurrentTheme.DropdownArrowWidth)
          else Dec(R.Bottom, CurrentTheme.DropdownArrowWidth);
        end;
      end;
    end
    else begin
      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, R, pstPrePaint, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintButton(Canvas, R, ItemInfo);
      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, R, pstPostPaint, PaintDefault);

      ItemInfo.Pushed := IsComboPushed;
      ItemInfo.Selected := False;
      ItemInfo.ComboPart := cpSplitRight;

      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, ComboRect, pstPrePaint, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintButton(Canvas, ComboRect, ItemInfo);
      PaintDefault := True;
      DoDrawButton(Canvas, ItemInfo, ComboRect, pstPostPaint, PaintDefault);

      ItemInfo.ComboPart := cpSplitLeft;
      ItemInfo.Pushed := IsPushed;
      ItemInfo.Selected := Item.Checked;
    end;

    InflateRect(R, -2, -2);
  end
  else begin
    CurrentTheme.GetMargins(MID_MENUITEM, Margins);

    PaintDefault := True;
    DoDrawButton(Canvas, ItemInfo, R, pstPrePaint, PaintDefault);

    // Do not let the CurrentTheme draw the submenu arrow on non-toolbar items
    OverrideArrowPainting := (tbisSubmenu in Item.ItemStyle) and not (tbisCombo in Item.ItemStyle);
    if OverrideArrowPainting then
      ItemInfo.ItemOptions := ItemInfo.ItemOptions and not IO_SUBMENUITEM;
    if PaintDefault then
      CurrentTheme.PaintMenuItem(Canvas, R, ItemInfo);
    if OverrideArrowPainting then
      ItemInfo.ItemOptions := ItemInfo.ItemOptions or IO_SUBMENUITEM;

    PaintDefault := True;
    DoDrawButton(Canvas, ItemInfo, R, pstPostPaint, PaintDefault);

    // Draw the submenu arrow on non-toolbar items
    if PaintDefault and OverrideArrowPainting then
      SpDrawArrow(Canvas.Handle, R.Right - 10, R.Bottom div 2, C, False);

    // Don't apply the margins if the menu item has
    // tbisClicksTransparent itemstyle (like a SpTBXLabelItem)
    // the caption will be automatically centered.
    if not (tbisClicksTransparent in Item.ItemStyle) then begin
      Inc(R.Left, Margins.LeftWidth);
      Dec(R.Right, Margins.RightWidth);
      Inc(R.Top, Margins.TopHeight);
      Dec(R.Bottom, Margins.BottomHeight);
    end;
  end;

  { Caption }
  if IsCaptionShown then
  begin
    WS := GetCaptionText;

    if ToolbarStyle then
    begin
      TextFlags := TextFlags and not DT_VCENTER;
      case Item.Alignment of
        taCenter:
          if GlyphLayout = ghlGlyphTop then TextFlags := TextFlags or DT_CENTER;
        taRightJustify:
          TextFlags := TextFlags or DT_RIGHT;
      end;
      SpCalcXPText(Canvas, R, WS, Item.Alignment, TextFlags, ImgSize.cx, ImgSize.cy, GlyphLayout, False, CaptionRect, ImageRect, TextAngle);

      // [TBXTheme-Change]
      // The Default theme paints the caption of the pushed button in a down
      // state, this only happens when the item is in a toolbarstyle
      if IsPushed and (TBXCurrentTheme = 'Default') then
        OffsetRect(CaptionRect, 1, 1);
    end
    else with CurrentTheme do
    begin
      if tbisClicksTransparent in Item.ItemStyle then begin
        // The caption should be centered on the menu popup if the item has
        // tbisClicksTransparent itemstyle (like a SpTBXLabelItem)
        TextFlags := DT_SINGLELINE or DT_CENTER or DT_VCENTER;
        CaptionRect := R;
      end
      else begin
        TextFlags := DT_LEFT or DT_VCENTER or TextFlags;
        GetTextMetrics(Canvas.Handle, TextMetrics);
        CaptionRect := R;
        Inc(CaptionRect.Left, ItemInfo.PopupMargin + MenuImageTextSpace + MenuLeftCaptionMargin);
        with TextMetrics, CaptionRect do
          if (Bottom - Top) - (tmHeight + tmExternalLeading) = Margins.BottomHeight then Dec(Bottom);
        Inc(CaptionRect.Top, TextMetrics.tmExternalLeading);
        CaptionRect.Right := CaptionRect.Left + TextSize.CX;
      end;
    end;

    Canvas.Font.Color := C;
    PaintDefault := True;
    DoDrawCaption(Canvas, ItemInfo, ClientAreaRect, WS, CaptionRect, TextFlags, IsTextRotated, pstPrePaint, PaintDefault);
    if PaintDefault then
      CurrentTheme.PaintCaption(Canvas, CaptionRect, ItemInfo, WS, TextFlags, IsTextRotated);
    PaintDefault := True;
    DoDrawCaption(Canvas, ItemInfo, ClientAreaRect, WS, CaptionRect, TextFlags, IsTextRotated, pstPostPaint, PaintDefault);
  end;

  { Shortcut and/or submenu arrow (menus only) }
  if not ToolbarStyle then
  begin
    WS := Item.GetShortCutText;
    if Length(WS) > 0 then
    begin
      CaptionRect := R;
      with CaptionRect, TextMetrics do
      begin
        Left := Right - (Bottom - Top) - SpGetTextSize(Canvas.Handle, WS, True).cx;
        if (Bottom - Top) - (tmHeight + tmExternalLeading) = Margins.BottomHeight then Dec(Bottom);
        Inc(Top, TextMetrics.tmExternalLeading);
      end;
      Canvas.Font.Color := C;
      PaintDefault := True;
      DoDrawCaption(Canvas, ItemInfo, ClientAreaRect, WS, CaptionRect, TextFlags, IsTextRotated, pstPrePaint, PaintDefault);
      if PaintDefault then
        CurrentTheme.PaintCaption(Canvas, CaptionRect, ItemInfo, WS, TextFlags, False);
      PaintDefault := True;
      DoDrawCaption(Canvas, ItemInfo, ClientAreaRect, WS, CaptionRect, TextFlags, IsTextRotated, pstPostPaint, PaintDefault);
    end;
  end;

  { Image, or check box }
  if ImageOrCheckShown then
  begin
    if ToolBarStyle then begin
      if IsRectEmpty(ImageRect) then
        ImageRect := R;
      if IsSpecialDropDown then OffsetRect(ImageRect, (-CurrentTheme.DropdownArrowWidth + 1) div 2, 0);
    end
    else begin
      ImageRect := R;
      ImageRect.Right := ImageRect.Left + ItemInfo.PopupMargin;
    end;

    if ImageIsShown then with ImageRect, ImgSize do
    begin
      Left := Left + ((Right - Left) - CX) div 2;
      ImageRect.Top := Top + ((Bottom - Top) - CY) div 2;
      Right := Left + CX;
      Bottom := Top + CY;
      DrawItemImage(Canvas, ImageRect, ItemInfo);
    end
    else
      if not ToolbarStyle and Item.Checked then begin
        if Item.RadioItem then
          ItemInfo.ItemOptions := ItemInfo.ItemOptions or IO_RADIO;
        CurrentTheme.PaintCheckMark(Canvas, ImageRect, ItemInfo);
      end;
  end;
end;

function TSpTBXItemViewer.GetItem: TSpTBXCustomItem;
begin
  Result := (inherited Item) as TSpTBXCustomItem;
end;

function TSpTBXItemViewer.GetHintText: Widestring;
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

procedure TSpTBXItemViewer.Entering(OldSelected: TTBItemViewer);
begin
  // When a Popupmenu is opened the TB2K modal handler will reset
  // the TApplication.Hint in UpdateAppHint subprocedure of
  // TTBModalHandler.Create, this in turn sets TTntApplication.Hint
  // to AnsiString:
  // ...
  // if Assigned(View.FSelected) then
  //    Application.Hint := GetLongHint(View.FSelected.Item.Hint)
  //  else
  //    Application.Hint := '';
  // ...
  // We need to set TTntApplication.Hint before TB2K.
  // TTntStatusBar uses TTntApplication.Hint when AutoHint is true.

  inherited;
  if View.IsPopup then
    TntApplication.Hint := Item.Hint;
end;

procedure TSpTBXItemViewer.CMHintShow(var Message: TMessage);
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
{ TSpTBXSubmenuItem }

constructor TSpTBXSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable];
end;

function TSpTBXSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

procedure TSpTBXSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomLabelItem }

constructor TSpTBXCustomLabelItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSelectable, tbisRedrawOnSelChange,
    tbisRedrawOnMouseOverChange] + [tbisClicksTransparent];
  Alignment := taLeftJustify;
  Stretch := False;
  DisplayMode := nbdmImageAndText;
end;

function TSpTBXCustomLabelItem.DialogChar(CharCode: Word): Boolean;
begin
  Result := inherited DialogChar(CharCode);
  if Enabled and Visible and Assigned(Control) and (Control is TWinControl) and
    IsAccel(CharCode, Caption) and SpCanFocus(TWinControl(Control)) then
  begin
    TWinControl(Control).SetFocus;
    Result := True;
  end;
end;

procedure TSpTBXCustomLabelItem.DoDrawButton(ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  PaintDefault := True;
  inherited DoDrawButton(ACanvas, ItemInfo, ARect, PaintStage, PaintDefault);
  PaintDefault := False;
end;

function TSpTBXCustomLabelItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXLabelItemViewer;
end;

procedure TSpTBXCustomLabelItem.ToggleControl;
begin
  // Do nothing, the Control property is not valid
end;

procedure TSpTBXCustomLabelItem.UpdateProps;
begin
  // Do nothing, the Control property is not valid
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXLabelItemViewer }

procedure TSpTBXLabelItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,
  AHeight: Integer);
var
  TextMetrics: TTextMetric;
begin
  inherited CalcSize(Canvas, AWidth, AHeight);
  if not IsToolbarStyle and (Length(GetCaptionText) > 0) and (Item.CustomHeight <= -1) then begin
    GetTextMetrics(Canvas.Handle, TextMetrics);
    AHeight := TextMetrics.tmHeight;
  end;
end;

function TSpTBXLabelItemViewer.DoExecute: Boolean;
begin
  // Clicking a TSpTBXLabelItem on a popup menu causes the menu to close.
  // This is caused by TTBXItemViewer.MouseUp, which calls
  // TTBItemViewer.DoExecute
  // The TBXLabelItem doesn't fire the click because the ItemViewer descends
  // from TTBItemViewer instead of TTBXItemViewer.
  // TTBXItemViewer.MouseUp is the culprit of firing the DoExecute
  Result := False;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSeparatorItem }

function TSpTBXSeparatorItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXSeparatorItemViewer;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSeparatorItemViewer }

function TSpTBXSeparatorItemViewer.IsStatusBarSeparator(out T: TSpTBXStatusToolbar): Boolean;
var
  C: TComponent;
begin
  Result := False;
  T := nil;
  C := Item.GetParentComponent;
  if C is TSpTBXStatusToolbar then begin
    T := C as TSpTBXStatusToolbar;
    Result := True;
  end;
end;

procedure TSpTBXSeparatorItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsHoverItem, IsPushed,
  UseDisabledShadow: Boolean);
var
  T: TSpTBXStatusToolbar;
begin
  if not (IsStatusBarSeparator(T) and T.NeedsSeparatorRepaint) then
    inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXRadioGroupItem }

constructor TSpTBXRadioGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultIndex := 0;
  FLastClickedIndex := 0;
  FStrings := TTntStringList.Create;
end;

destructor TSpTBXRadioGroupItem.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TSpTBXRadioGroupItem.DoClick(AItem: TSpTBXItem);
begin
  if Assigned(FOnClick) then FOnClick(AItem);
end;

function TSpTBXRadioGroupItem.DoGetItemCaption(ACaption: WideString): WideString;
begin
  Result := ACaption;
  if Assigned(FOnGetCaption) then
    FOnGetCaption(Self, ACaption);
end;

procedure TSpTBXRadioGroupItem.FillStringList;
begin
  // do nothing
end;

procedure TSpTBXRadioGroupItem.ItemClickEvent(Sender: TObject);
var
  Item: TSpTBXItem;
begin
  Item := Sender as TSpTBXItem;
  if not Item.Checked and (Item.Tag > -1) and (Item.Tag < FStrings.Count) then
  begin
    Item.Checked := True;
    FLastClickedIndex := IndexOf(Item);
    DoClick(Item);
  end;
end;

procedure TSpTBXRadioGroupItem.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Recreate;
end;

procedure TSpTBXRadioGroupItem.Recreate;
var
  I: Integer;
  A: TSpTBXItem;
begin
  // Delete FStrings items
  FStrings.Clear;
  for I := Count - 1 downto 0 do
    if Items[I].GroupIndex = C_SpTBXRadioGroupIndex then
      Delete(I);

  FillStringList;

  // Create group items
  for I := 0 to FStrings.Count - 1 do begin
    A := TSpTBXItem.Create(Self);
    A.Caption := DoGetItemCaption(FStrings[I]);
    A.AutoCheck := False;
    A.GroupIndex := C_SpTBXRadioGroupIndex;
    A.Tag := I;
    A.OnClick := ItemClickEvent;
    Insert(I, A);
    if I = FDefaultIndex then A.Click;
  end;

  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeGroupItem }

constructor TSpTBXThemeGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  FSkinDir := '';
  AddThemeNotification(Self);
end;

destructor TSpTBXThemeGroupItem.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TSpTBXThemeGroupItem.DoClick(AItem: TSpTBXItem);
var
  S, SkinName, Filename: WideString;
begin
  S := FStrings[AItem.Tag];

  // Load the skin ini file
  if Pos(rvThemeNamePrefix, S) > 0 then begin
    SkinName := S;
    System.Delete(SkinName, 1, Length(rvThemeNamePrefix));
    if SkinName <> '' then begin
      Filename := TntSysUtils.WideIncludeTrailingBackslash(FSkinDir + SkinName) + rvSkinIniFilename;
      CurrentSkin.LoadFromFile(Filename);
    end;
    S := rvSpTBXThemeName;
  end;

  TBXSetTheme(S);
  inherited;
end;

function TSpTBXThemeGroupItem.DoGetItemCaption(ACaption: WideString): WideString;
begin
  if Pos(rvThemeNamePrefix, ACaption) > 0 then
    System.Delete(ACaption, 1, Length(rvThemeNamePrefix));
  Result := inherited DoGetItemCaption(ACaption);
end;

procedure TSpTBXThemeGroupItem.DoThemeChange;
var
  I: Integer;
begin
  I := FStrings.IndexOf(TBXCurrentTheme);
  if I > -1 then
    Items[I].Click;

  if Assigned(FOnThemeChange) then FOnThemeChange(Self);
end;

procedure TSpTBXThemeGroupItem.FillStringList;
var
  I: Integer;
  L: TTntStringList;
begin
  inherited;
  TBXThemes.GetAvailableTBXThemes(FStrings.AnsiStrings);

  // Add the skins if the SpTBXTheme is present
  I := FStrings.IndexOf(rvSpTBXThemeName);
  if I > -1 then begin
    FStrings.Delete(I);
    L := TTntStringList.Create;
    try
      if SpGetDirectories(FSkinDir, L) then begin
        // Add the corresponding TBX theme name
        for I := 0 to L.Count - 1 do
          FStrings.Add(rvThemeNamePrefix + L[I]);
      end;
    finally
      L.Free;
    end;
  end;

  // Sort the list and move the Default theme to the top
  FStrings.Sort;
  I := FStrings.IndexOf('Default');
  if I > -1 then FStrings.Move(I, 0);
  FDefaultIndex := FStrings.IndexOf(TBXCurrentTheme);
end;

procedure TSpTBXThemeGroupItem.SetSkinDir(Value: WideString);
begin
  Value := TntSysUtils.WideIncludeTrailingBackslash(Value);
  if FSkinDir <> Value then begin
    FSkinDir := Value;
    Recreate;
  end;
end;

procedure TSpTBXThemeGroupItem.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    DoThemeChange;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemCache }

procedure TSpTBXItemCache.Assign(Source: TPersistent);
var
  C: TSpTBXItemCache;
begin
  if Source is TSpTBXItemCache then
  begin
    C := Source as TSpTBXItemCache;
    Item := C.Item;
    Left := C.Left;
    Top := C.Top;
    Width := C.Width;
    Height := C.Height;
    ParentWidth := C.ParentWidth;
    ParentHeight := C.ParentHeight;
  end
  else inherited Assign(Source);
end;

function TSpTBXItemCache.GetName: TComponentName;
begin
  if Assigned(FItem) then
    Result := FItem.Name
  else
    Result := FName;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXItemCacheCollection }

function TSpTBXItemCacheCollection.Add: TSpTBXItemCache;
begin
  Result := TSpTBXItemCache(inherited Add);
end;

function TSpTBXItemCacheCollection.Add(Item: TTBCustomItem; RBounds: TRect): Integer;
var
  F: TSpTBXItemCache;
begin
  F := Add;
  F.Item := Item;
  F.Left := RBounds.Left;
  F.Top := RBounds.Top;
  F.Width := RBounds.Right - RBounds.Left;
  F.Height := RBounds.Bottom - RBounds.Top;
  Result := F.Index;
end;

function TSpTBXItemCacheCollection.GetItem(Index: Integer): TSpTBXItemCache;
begin
  Result := TSpTBXItemCache(inherited Items[Index]);
end;

function TSpTBXItemCacheCollection.IndexOf(Item: TTBCustomItem): Integer;
var
  I: integer;
begin
  Result := -1;
  if Assigned(Item) then
    for I := 0 to Count - 1 do
      if Items[I].Item = Item then begin
        Result := I;
        Break;
      end;
end;

procedure TSpTBXItemCacheCollection.SetItem(Index: Integer;
  const Value: TSpTBXItemCache);
begin
  inherited Items[Index] := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDock }

function TSpTBXDock.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  FPrevWidth := Width;
  FPrevHeight := Height;
  Result := inherited CanResize(NewWidth, NewHeight);
end;

procedure TSpTBXDock.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXDock.DrawDockBackground(DC: HDC; const DrawRect: TRect): Boolean;
var
  Canvas: TCanvas;
  PaintDefault: Boolean;
begin
  Result := False;
  if (csDestroying in ComponentState) then Exit;

  PaintDefault := True;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    DoDrawBackground(Canvas, DrawRect, pstPrePaint, PaintDefault);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;

  if PaintDefault then begin
    Result := True;
    DrawBackground(DC, DrawRect);
  end;
end;

procedure TSpTBXDock.DrawBackground(DC: HDC; const DrawRect: TRect);
var
  Canvas: TCanvas;
  PaintDefault: Boolean;
begin
  // OnDrawBackground should be used to paint all the toolbars + dock, it is
  // used by TSpTBXStatusBar and TSpTBXDockablePanel to paint the whole client
  // area with custom painting.
  // Assigning OnDrawBackground sets ThemedBackground to True.
  // OnDrawBackground is triggered by the Dock and by the docked Toolbar.
  // The Toolbar triggers it only if Dock.ThemedBackground (TTBXDock.UsingBackground)
  // is true, which depends on CurrentTheme.PaintDockBackground, this is done in
  // TTBXToolbar.WMEraseBkgnd.
  // When (ARect.Left = 0) and (ARect.Top = 0) the sender is the Dock, else
  // it's the docked Toolbar.

  if (csDestroying in ComponentState) then Exit;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;

    PaintDefault := True;
    DoDrawBackground(Canvas, DrawRect, pstPrePaint, PaintDefault);
    if PaintDefault then
      inherited;

    PaintDefault := True;
    DoDrawBackground(Canvas, DrawRect, pstPostPaint, PaintDefault);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TSpTBXDock.Resize;
var
  I: Integer;
  ResizeToolbars: Boolean;
begin
  inherited;

  if Position in [dpLeft, dpRight] then
    ResizeToolbars := Height < FPrevHeight
  else
    ResizeToolbars := Width < FPrevWidth;

  if ResizeToolbars then
    for I := 0 to ToolbarCount - 1 do
      if Toolbars[I] is TSpTBXToolbar then
        TSpTBXToolbar(Toolbars[I]).Resize;
end;

function TSpTBXDock.ThemedBackground: Boolean;
begin
  if Assigned(FOnDrawBackground) then
    Result := True
  else
    Result := inherited ThemedBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXToolbarView }

constructor TSpTBXToolbarView.Create(AOwner: TComponent);
begin
  inherited;
  FMaxSize := -1;
end;

procedure TSpTBXToolbarView.SetMaxSize(const Value: Integer);
begin
  if FMaxSize <> Value then begin
    FMaxSize := Value;
    UpdatePositions;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXToolbar }

constructor TSpTBXToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Items.RegisterNotification(DoItemNotification);
  FAnchoredControlItems := TSpTBXItemCacheCollection.Create(TSpTBXItemCache);
  FChevronVertical := True;
  FCustomizable := True;
  FDisplayMode := tbdmSelectiveCaption;
end;

destructor TSpTBXToolbar.Destroy;
begin
  Items.UnRegisterNotification(DoItemNotification);
  FAnchoredControlItems.Free;
  inherited;
end;

procedure TSpTBXToolbar.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TSpTBXToolbar.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TSpTBXToolbar.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect,
    PaintStage, PaintDefault);
end;

procedure TSpTBXToolbar.DoItemClick(Item: TTBCustomItem; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Force OnClick event, by default tbisClicksTransparent Items doesn't get executed
  if (Button = mbLeft) and Item.Enabled then
    if (tbisClicksTransparent in TTBCustomItemAccess(Item).ItemStyle) then
      if Assigned(Item.OnClick) then Item.OnClick(Item);     
end;

procedure TSpTBXToolbar.DoItemNotification(Ancestor: TTBCustomItem;
  Relayed: Boolean; Action: TTBItemChangedAction; Index: Integer;
  Item: TTBCustomItem);
var
  I: Integer;
begin
  if (csDestroying in ComponentState) or (csReading in ComponentState) then Exit;

  if not FIsResizing and not IsItemMoving then begin
    if Assigned(FOnItemNotification) then FOnItemNotification(Self, Ancestor, Relayed, Action, Index, Item);

    case Action of
      tbicInserted:
        begin
          RightAlignItems;
          AnchorItems(True);
        end;
      tbicDeleting:
        begin
          I := FAnchoredControlItems.IndexOf(Item);
          if I > -1 then
            FAnchoredControlItems.Delete(I);
          RightAlignItems;
          AnchorItems(True);
        end;
      tbicInvalidateAndResize:
        RightAlignItems;
    end;
  end;
end;

procedure TSpTBXToolbar.Resize;
begin
  FIsResizing := True;
  try
    RightAlignItems;
    AnchorItems;
  finally
    FIsResizing := False;
  end;
  inherited;
end;

procedure TSpTBXToolbar.AnchorItems(UpdateControlItems: Boolean);
var
  I, J, UpdatedDelta: Integer;
  SpIV: TSpTBXItemViewer;
  Size: TPoint;
  CI: TTBControlItem;
  IV: TTBItemViewer;
  IsRotated: Boolean;
begin
  if (csDestroying in ComponentState) or not Assigned(CurrentDock) or
    (CurrentDock.Width = 0) or (CurrentDock.Height = 0) or
    not Stretch or (ShrinkMode <> tbsmNone) then
      Exit;

  View.BeginUpdate;
  try
    View.ValidatePositions;
    IsRotated := CurrentDock.Position in [dpLeft, dpRight];
    // Adjust the delta, only used when inserting/deleting an item on the toolbar
    UpdatedDelta := 0;
    if (FLastSelectableWidth > 0) and UpdateControlItems then begin
      IV := View.NextSelectable(nil, False);
      if Assigned(IV) then
        if IsRotated then
          UpdatedDelta := FLastSelectableWidth - IV.BoundsRect.Bottom
        else
          UpdatedDelta := FLastSelectableWidth - IV.BoundsRect.Right;
    end;

    // Calculate the Toolbar size
    Size := Point(CurrentDock.Width, CurrentDock.Height);

    // Resize the anchored items
    for I := 0 to View.ViewerCount - 1 do
      if View.Viewers[I] is TSpTBXItemViewer then begin
        SpIV := View.Viewers[I] as TSpTBXItemViewer;
        if SpIV.Item.Anchored then begin
          // Revalidate FAnchorSize and set FAnchorDelta
          if (SpIV.FAnchorSize.X = 0) and (SpIV.FAnchorSize.Y = 0) then
            SpIV.FAnchorSize := Size;

          // Adjust the delta, only used when inserting/deleting an item on
          // the toolbar and resize
          if IsRotated then begin
            SpIV.FAnchorSize.Y := SpIV.FAnchorSize.Y - UpdatedDelta;
            SpIV.FAnchorDelta := Size.Y - SpIV.FAnchorSize.Y;
          end
          else begin
            SpIV.FAnchorSize.X := SpIV.FAnchorSize.X - UpdatedDelta;
            SpIV.FAnchorDelta := Size.X - SpIV.FAnchorSize.X;
          end;
        end;
      end
      else begin
        // Client align TTBControlItem items if the associated Control is client
        // aligned or has akRight in its Anchors property.
        CI := IsAnchoredControlItem(View.Viewers[I].Item);
        J := FAnchoredControlItems.IndexOf(View.Viewers[I].Item);
        if Assigned(CI) then begin
          // Add the TTBControlItem item to the list if its not there
          if J = -1 then begin
            J := FAnchoredControlItems.Add(CI, Rect(0, 0, CI.Control.Width, CI.Control.Height));
            FAnchoredControlItems[J].ParentWidth := Size.X;
            FAnchoredControlItems[J].ParentHeight := Size.Y;
          end;
          // Resize
          FAnchoredControlItems[J].Width := FAnchoredControlItems[J].Width + UpdatedDelta;
          CI.Control.Width := FAnchoredControlItems[J].Width + (Size.X - FAnchoredControlItems[J].ParentWidth);
        end
        else
          // If ControlItem is not valid delete it from the list
          if J > -1 then
            FAnchoredControlItems.Delete(J);
      end;
    View.UpdatePositions;
  finally
    View.EndUpdate;
  end;

  // We can't calculate the delta based on the IV.BoundsRect because
  // the IV is nil on tbicDeleting notification.
  // We have to keep track of the sum of the selectable items width
  IV := View.NextSelectable(nil, False);
  if Assigned(IV) then begin
    if IsRotated then
      FLastSelectableWidth := IV.BoundsRect.Bottom
    else
      FLastSelectableWidth := IV.BoundsRect.Right;
  end
  else
    FLastSelectableWidth := 0;
end;

function TSpTBXToolbar.IsAnchoredControlItem(Item: TTBCustomItem): TTBControlItem;
var
  CI: TTBControlItem;
begin
  Result := nil;
  if Assigned(CurrentDock) and (Item is TTBControlItem) then begin
    CI := Item as TTBControlItem;
    if Assigned(CI.Control) and
      ((CI.Control.Align = alClient) or (akRight in CI.Control.Anchors)) then
    begin
      Result := CI;
    end
    else
      Result := nil;
  end;
end;

procedure TSpTBXToolbar.RightAlignItems;
var
  I, VisibleWidth, RightAlignedWidth: Integer;
  Spacer: TSpTBXItemViewer;
  IsRotated: Boolean;
begin
  if (csDestroying in ComponentState) or not Assigned(CurrentDock) or
    (Items.Count <= 0) or not Stretch or (ShrinkMode <> tbsmNone) or
    ((CurrentDock.Width = 0) and (CurrentDock.Height = 0)) then
      Exit;

  View.ValidatePositions;
  View.BeginUpdate;
  try
    // Find the spacer and the right aligned items
    IsRotated := CurrentDock.Position in [dpLeft, dpRight];
    Spacer := SpGetRightAlignedItems(View, nil, IsRotated, VisibleWidth, RightAlignedWidth);
    if Assigned(Spacer) then begin
      // Resize the spacer
      if IsRotated then
        I := CurrentDock.Height - GetRightAlignMargin - 6 - (VisibleWidth - (Spacer.BoundsRect.Bottom - Spacer.BoundsRect.Top))
      else
        I := CurrentDock.Width - GetRightAlignMargin - (VisibleWidth - (Spacer.BoundsRect.Right - Spacer.BoundsRect.Left));

      if I < 0 then I := 0;
      Spacer.Item.CustomWidth := I;
    end;
    View.UpdatePositions;
  finally
    View.EndUpdate;
  end;
end;

function TSpTBXToolbar.GetChevronItemClass: TTBChevronItemClass;
begin
  Result := TSpTBXChevronItem;
end;

function TSpTBXToolbar.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TSpTBXFloatingWindowParent;
end;

function TSpTBXToolbar.GetRightAlignMargin: Integer;
begin
  Result := NonClientWidth;
end;

function TSpTBXToolbar.GetViewClass: TTBToolbarViewClass;
begin
  Result := TSpTBXToolbarView;
end;

procedure TSpTBXToolbar.DrawNCArea(const DrawToDC: Boolean; const ADC: HDC;
  const Clip: HRGN);
var
  DC: HDC;
  R, CR: TRect;
  TD: TSpTBXDock;
  ToolbarInfo: TTBXToolbarInfo;
begin
  inherited;

  if HandleAllocated and Docked and Assigned(CurrentDock) and
    (CurrentDock is TSpTBXDock) and Assigned(TSpTBXDock(CurrentDock).OnDrawBackground) then
  begin
    TD := CurrentDock as TSpTBXDock;
    if not DrawToDC then DC := GetWindowDC(Handle)
    else DC := ADC;
    try
      GetToolbarInfo(ToolbarInfo);
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      if not DrawToDC then
      begin
        SelectNCUpdateRgn(Handle, DC, Clip);
        CR := R;
        with ToolbarInfo.BorderSize, CR do
        begin
          InflateRect(CR, -X, -Y);
          if ToolbarInfo.IsVertical then Inc(Top, GetTBXDragHandleSize(ToolbarInfo))
          else Inc(Left, GetTBXDragHandleSize(ToolbarInfo));
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
        end;
      end;
      TD.DrawBackground(DC, R);
    finally
      if not DrawToDC then ReleaseDC(Handle, DC);
    end;
  end
  else
    inherited;
end;

procedure TSpTBXToolbar.CMHintShow(var Message: TCMHintShow);
// Dispatch the message to the Item Viewer.
// TSpTBXItemViewer will handle CM_HINTSHOW message to show unicode hints using
// a custom THintWindow.
begin
  with Message.HintInfo^ do
  begin
    HintStr := '';
    if Assigned(View.Selected) then begin
      CursorRect := View.Selected.BoundsRect;
      HintStr := View.Selected.GetHintText;
      View.Selected.Dispatch(Message);
    end;
  end;
end;

procedure TSpTBXToolbar.CMControlChange(var Message: TCMControlChange);
begin
  // When a control is dropped on the toolbar a TTBControlItem is created by
  // TTBCustomToolbar.CreateWrapper, unfortunately it is created with the
  // Self.Owner instead of the Form (Owner.Owner for CompoundToolbars like
  // the TabToolbar or StatusToolbar).

  if CompoundToolbar and Message.Inserting and not(csLoading in ComponentState) and
    not(csUpdating in ComponentState) and
    (SpFindControlItem(Items, Message.Control) = nil) then
  begin
    CreateWrapper(Items.Count, Message.Control);
  end
  else
    inherited;
end;

function TSpTBXToolbar.CreateWrapper(Index: Integer; Ctl: TControl): TTBControlItem;
var
  I: Integer;
  S: String;
  C: TComponent;
begin
  C := Owner.Owner;
  Result := TTBControlItem.CreateControlItem(C, Ctl);
  if (csDesigning in ComponentState) and Assigned(C) then begin
    { Needs a name for compatibility with form inheritance }
    I := 1;
    while True do begin
      S := Format('TBControlItem%d', [I]);
      if C.FindComponent(S) = nil then
        Break;
      Inc(I);
    end;
    Result.Name := S;
  end;
  Items.Insert(Index, Result);
end;

procedure TSpTBXToolbar.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  if Enabled and Visible then
    for I := 0 to Items.Count - 1 do
      if Items[I] is TSpTBXCustomItem then
        if TSpTBXCustomItem(Items[I]).DialogChar(Message.CharCode) then begin
          Message.Result := 1;
          Exit;
        end;
  inherited;
end;

procedure TSpTBXToolbar.CMMouseleave(var Message: TMessage);
begin
  inherited;
  if IsCustomizing and FCustomizable then begin
    // Clear the last DropMark
    InvalidateRect(Handle, @FLastDropMark, True);
  end;
end;

procedure TSpTBXToolbar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Canvas: TCanvas;
  PaintDefault: Boolean;
  R: TRect;
  TD: TSpTBXDock;
begin
  if (csDestroying in ComponentState) then Exit;

  if Assigned(CurrentDock) and (CurrentDock is TSpTBXDock) then begin
    TD := CurrentDock as TSpTBXDock;
    if Assigned(TD.OnDrawBackground) then begin
      R := TD.ClientRect;
      R.TopLeft := ScreenToClient(TD.ClientToScreen(R.TopLeft));
      R.BottomRight := ScreenToClient(TD.ClientToScreen(R.BottomRight));
      Canvas := TCanvas.Create;
      Canvas.Handle := Message.DC;
      try
        PaintDefault := True;
        TD.OnDrawBackground(TD, Canvas, R, pstPrePaint, PaintDefault);
        if PaintDefault then
          TD.DrawBackground(Message.DC, R)
        else begin
          Message.Result := 1;
          Exit;  // The background painting is done by the Dock
        end;
      finally
        Canvas.Handle := 0;
        Canvas.Free;
      end;
    end;
  end;

  if Assigned(FOnDrawBackground) then begin
    Message.Result := 1;
    Canvas := TCanvas.Create;
    Canvas.Handle := Message.DC;
    try
      if Docked then
      begin
        R := CurrentDock.ClientRect;
        R.TopLeft := ScreenToClient(CurrentDock.ClientToScreen(R.TopLeft));
        R.BottomRight := ScreenToClient(CurrentDock.ClientToScreen(R.BottomRight));
      end
      else
        R := ClientRect;

      PaintDefault := True;
      DoDrawBackground(Canvas, R, pstPrePaint, PaintDefault);
      if PaintDefault then
        inherited;

      PaintDefault := True;
      DoDrawBackground(Canvas, R, pstPostPaint, PaintDefault);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  end
  else
    inherited;
end;

procedure TSpTBXToolbar.WMSize(var Message: TWMSize);
begin
  inherited;
  Update;
end;

function TSpTBXToolbar.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TSpTBXToolbar.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXToolbar.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

function TSpTBXToolbar.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

procedure TSpTBXToolbar.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXToolbar.SetDisplayMode(const Value: TSpTBXToolbarDisplayMode);
begin
  if FDisplayMode <> Value then begin
    FDisplayMode := Value;
    if Value = tbdmImageAboveCaption then
      Options := Options + [tboImageAboveCaption, tboSameWidth]
    else
      Options := Options - [tboImageAboveCaption, tboSameWidth];
    View.UpdatePositions;
  end;
end;

function TSpTBXToolbar.GetMaxSize: Integer;
begin
  if Assigned(View) then
    Result := TSpTBXToolbarView(View).MaxSize
  else
    Result := -1;
end;

procedure TSpTBXToolbar.SetMaxSize(const Value: Integer);
begin
  if Assigned(View) then
    TSpTBXToolbarView(View).MaxSize := Value;
end;

procedure TSpTBXToolbar.BeginItemMove;
begin
  Inc(FItemMovingCount);
end;

procedure TSpTBXToolbar.EndItemMove;
begin
  Dec(FItemMovingCount);
  if FItemMovingCount < 0 then FItemMovingCount := 0;
end;

function TSpTBXToolbar.IsItemMoving: Boolean;
begin
  Result := FItemMovingCount > 0;
end;

procedure TSpTBXToolbar.BeginCustomize;
begin
  Inc(FCustomizingCount);
end;

procedure TSpTBXToolbar.EndCustomize;
begin
  Dec(FCustomizingCount);
  if FCustomizingCount < 0 then FCustomizingCount := 0;
end;

function TSpTBXToolbar.IsCustomizing: Boolean;
begin
  Result := FCustomizingCount > 0;
end;

function TSpTBXToolbar.CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and IsCustomizing then begin
    Result := True;
    if FCustomizable then begin
      IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
      if Assigned(IV) and Assigned(IV.Item) and not (IV.Item is TTBChevronItem) then begin
        FBeginDragIV := IV;
        BeginDrag(True);
      end;
    end;
  end;
end;

function TSpTBXToolbar.CanItemClick(Item: TTBCustomItem; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TSpTBXToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: TTBCustomItem;
begin
  inherited;

  // Handle the Toolbar items hints
  // Set the Toolbar.Hint to change the Application.Hint when the
  // mouse is over the Item.
  // From TB2Toolbar.MouseMove
  if not (csDesigning in ComponentState) then begin
    if Assigned(View.Selected) then begin
      Item := View.Selected.Item;
      if not (tboLongHintInMenuOnly in Item.EffectiveOptions) then
        if Item is TSpTBXCustomItem then
          Hint := TSpTBXCustomItem(Item).Hint
        else
          Hint := Item.Hint;
    end;
  end;
end;

procedure TSpTBXToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldParent: TWinControl;
  CurrentPos, OldPos: TPoint;
  Item: TTBCustomItem;
begin
  if not (csDesigning in ComponentState) and not CanDragCustomize(Button, Shift, X, Y) then begin
    OldParent := Parent;
    OldPos := ClientToScreen(Point(Left, Top));
    if Assigned(View.Selected) then
      Item := View.Selected.Item
    else
      Item := nil;

    if CanItemClick(Item, Button, Shift, X, Y) then
      inherited;

    // Check if the Parent was changed due to the toolbar moving between docks
    if (Parent = OldParent) and Assigned(View.Selected) then begin
      // Check if the toolbar was moved across the screen
      CurrentPos := ClientToScreen(Point(Left, Top));
      if (CurrentPos.X = OldPos.X) and (CurrentPos.Y = OldPos.Y) then
        DoItemClick(View.Selected.Item, Button, Shift, X, Y); // Extra click processing
    end;
  end;
end;

procedure TSpTBXToolbar.DoStartDrag(var DragObject: TDragObject);
begin
  if IsCustomizing and FCustomizable and Assigned(FBeginDragIV) and Assigned(FBeginDragIV.Item) then begin
    DragObject := TSpTBXItemDragObject.Create(Self, FBeginDragIV.Item);
    inherited DoStartDrag(DragObject);
  end;
end;

procedure TSpTBXToolbar.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DestIV: TTBItemViewer;
  DestPos: Integer;
  DropMark: TRect;
begin
  inherited DragOver(Source, X, Y, State, Accept);

  if IsCustomizing and FCustomizable then begin
    Accept := True;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), DestIV, DestPos, DropMark);
    if not EqualRect(DropMark, FLastDropMark) then begin
      // Clear the last DropMark
      InvalidateRect(Handle, @FLastDropMark, True);
      // Draw the new DropMark
      SpDrawDropMark(Canvas, DropMark);
      FLastDropMark := DropMark;
    end;
  end;
end;

procedure TSpTBXToolbar.DragDrop(Source: TObject; X, Y: Integer);
var
  D: TSpTBXItemDragObject;
  DestIV: TTBItemViewer;
  OrigItem: TTBCustomItem;
  OrigPos, DestPos: Integer;
  DropMark: TRect;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) then begin
    D := Source as TSpTBXItemDragObject;
    OrigItem := D.SouceItem;
    OrigPos := OrigItem.Parent.IndexOf(OrigItem);

    // Get the destination item position
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
    SpGetDropPosItemViewer(Items, View, Point(X, Y), DestIV, DestPos, DropMark);
    if OrigItem.Parent = Items then begin
      if DestPos > OrigPos then
        dec(DestPos);
      if (OrigPos = DestPos) then begin
        // Clear the last DropMark
        InvalidateRect(Handle, @FLastDropMark, True);
        Exit;
      end;
    end;

    if Assigned(DestIV) and (DestPos < 0) then Exit;

    // Insert the dragging item to the destination toolbar
    OrigItem.Parent.Remove(OrigItem);
    try
      if Assigned(DestIV) then begin
        Items.Insert(DestPos, OrigItem);
      end
      else
        Items.Add(OrigItem);
      OrigItem.Visible := True;
      FLastDropMark := Rect(0, 0, 0, 0);
    except
      OrigItem.Parent.Insert(OrigPos, OrigItem);
    end;
  end;

  inherited;
end;

procedure TSpTBXToolbar.ReadPositionData(const Data: TTBReadPositionData);
begin
  inherited;
  with Data do
    DisplayMode := TSpTBXToolbarDisplayMode(ReadIntProc(Name, rvSpTBXDisplayMode, 0, ExtraData));
end;

procedure TSpTBXToolbar.WritePositionData(const Data: TTBWritePositionData);
begin
  inherited;
  with Data do
    WriteIntProc(Name, rvSpTBXDisplayMode, Integer(DisplayMode), ExtraData);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFloatingWindowParent }

procedure TSpTBXFloatingWindowParent.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN; RedrawWhat: TTBToolWindowNCRedrawWhat);
var
  DC: HDC;
  R, CaptionR: TRect;
  ACanvas: TCanvas;
  DockWindow: TTBCustomDockableWindowAccess;
  FloatingBorderSize: TPoint;
  WideCaption: WideString;
  T: string;
begin
  inherited;
  if not HandleAllocated then Exit;

  DockWindow := TTBCustomDockableWindowAccess(DockableWindow);
  if not DockWindow.ShowCaption then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;
  try
    if not DrawToDC then SelectNCUpdateRgn(Handle, DC, Clip);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    with R do IntersectClipRect(DC, Left, Top, Right, Bottom);
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);

      CaptionR := R;
      FloatingBorderSize := DockWindow.GetFloatingBorderSize;
      with FloatingBorderSize do InflateRect(CaptionR, -X, -Y);
      Dec(CaptionR.Bottom, ClientHeight);
      OffsetRect(CaptionR, 2, -1);

      if DockWindow.CloseButton then
        Dec(CaptionR.Right, GetSystemMetrics(SM_CYSMCAPTION) + 2);

      ACanvas.Font.Assign(TBXUtils.SmCaptionFont);

      // [TBXTheme-Change]
      // Use the default Windows titlebar caption color
      T := TBXCurrentTheme;
      if (T = 'Default') or (T = 'Miranda') or (T = 'Eos') or (T = 'Zezio') then
        ACanvas.Font.Color := clCaptionText;

      ACanvas.Brush.Style := bsClear;
      try
        if DockableWindow is TSpTBXToolbar then
          WideCaption := TSpTBXToolbar(DockWindow).Caption
        else
          WideCaption := '';
        SpDrawXPText(ACanvas, WideCaption, CaptionR, DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX);
      finally
        ACanvas.Brush.Style := bsSolid;
      end;

    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXFloatingWindowParent.VisibleChanging;
begin
  inherited;
  Caption := '';
end;

procedure TSpTBXFloatingWindowParent.WMActivateApp(var Message: TWMActivateApp);
var
  DockWindow: TTBCustomDockableWindowAccess;
  T: string;
begin
  inherited;
  if HandleAllocated then begin
    DockWindow := TTBCustomDockableWindowAccess(DockableWindow);
    // [TBXTheme-Change]
    // The floating window is not repainted correctly if HideWhenInactive is
    // false and the application is deactivated/activated.
    // Invalidate the floating window if the Default theme is used.
    if not DockWindow.HideWhenInactive then begin
      T := TBXCurrentTheme;
      if (T = 'Default') or (T = 'Eos') then
        RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupWindow }

constructor TSpTBXPopupWindow.CreatePopupWindow(AOwner: TComponent;
  const AParentView: TTBView; const AItem: TTBCustomItem;
  const ACustomizing: Boolean);
begin
  inherited;
  if AItem is TSpTBXCustomItem then
    if Assigned(View) and (View is TSpTBXPopupWindowView) then begin
      if TSpTBXCustomItem(AItem).ToolbarStylePopup then
        TSpTBXPopupWindowView(View).SetIsToolbar(True);
    end;
end;

destructor TSpTBXPopupWindow.Destroy;
var
   M: TSpTBXCustomItem;
   ClickedItem: TTBCustomItem;
begin
  if View.ParentItem is TSpTBXCustomItem then begin
    M := View.ParentItem as TSpTBXCustomItem;
    if Assigned(M.OnClosePopup) then begin
      ClickedItem := nil;
      if Assigned(View.ParentView) and (TTBViewAccess(View.ParentView).DoneActionData.DoneAction = tbdaClickItem) then
        ClickedItem := TTBViewAccess(View.ParentView).DoneActionData.ClickItem;
      M.OnClosePopup(M, ClickedItem);
    end;
  end;

  inherited;
end;

function TSpTBXPopupWindow.GetViewClass: TTBViewClass;
begin
  Result := TSpTBXPopupWindowView;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupWindowView }

procedure TSpTBXPopupWindowView.SetIsToolbar(const Value: Boolean);
begin
  // Change the readonly IsToolbar property using RTTI, the property must
  // be published.
  // Tip from: http://hallvards.blogspot.com/2004/05/hack-1-write-access-to-read-only.html
  PBoolean(Integer(Self) + (Integer(GetPropInfo(TSpTBXPopupWindowView, 'IsToolbar').GetProc) and $00FFFFFF))^ := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXChevronItem }

function TSpTBXChevronItem.GetPopupWindowClass: TTBPopupWindowClass;
begin
  Result := TSpTBXChevronPopupWindow;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXChevronPopupWindow }

procedure TSpTBXChevronPopupWindow.CMColorChanged(var Message: TMessage);
var
  V: TSpTBXPopupWindowView;
  PV: TTBView;
begin
  // The private FIsToolbar field of the ChevronItem is setted to True
  // in TTBCustomItem.CreatePopup, we need to reset it to False before
  // the Popup is showed.
  // TTBCustomItem.CreatePopup changes the PopupWindow color to clBtnFace
  // after it changes the FIsToolbar value (and before it is visible),
  // that's why we are trapping CM_COLORCHANGED to reset the field.

  inherited;
  if Assigned(View) and (View is TSpTBXPopupWindowView) then begin
    V := TSpTBXPopupWindowView(View);
    PV := V.ParentView;
    // Do we really need to change it?
    if (Color = clBtnFace) and V.IsToolbar and Assigned(PV) and
      Assigned(PV.Owner) and (PV.Owner is TSpTBXToolbar) and
      (TSpTBXToolbar(PV.Owner).ChevronVertical) then
    begin
      V.SetIsToolbar(False);
    end;
  end;
end;

function TSpTBXChevronPopupWindow.GetViewClass: TTBViewClass;
begin
  Result := TSpTBXPopupWindowView;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupMenu }

procedure TSpTBXPopupMenu.DoInitPopup(PopupView: TTBView);
begin
  if Assigned(FOnInitPopup) then FOnInitPopup(Self, PopupView);
end;

function TSpTBXPopupMenu.InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
  PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): Boolean;
var
  ModalHandler: TTBModalHandler;
  Popup: TTBPopupWindow;
  DoneActionData: TTBDoneActionData;
  State: TTBViewState;
  P: TPoint;
  PopupControlRect: TRect;
  W: TWinControl;
  Msg: TMessage;
begin
  P := Point(X, Y);

  {$IFDEF JR_D9}
  SetPopupPoint(P);
  {$ELSE}
  PPoint(@PopupPoint)^ := P;
  {$ENDIF}

  W := nil;
  if Assigned(PopupControl) and Assigned(PopupControl.Parent) then begin
    PopupControlRect := PopupControl.BoundsRect;
    PopupControlRect.TopLeft := PopupControl.Parent.ClientToScreen(PopupControlRect.TopLeft);
    PopupControlRect.BottomRight := PopupControl.Parent.ClientToScreen(PopupControlRect.BottomRight);
    if PopupControl is TWinControl then
      W := PopupControl as TWinControl;
  end
  else
    PopupControlRect := Rect(X, Y, X, Y);

  ModalHandler := TTBModalHandler.Create(0);
  try
    Popup := TTBXRootItemAccess(Items).CreatePopupEx(False, PopupControlRect, TTBPopupAlignment(Alignment));
    try
      State := Popup.View.State;
      Include(State, vsIgnoreFirstMouseUp);
      TTBViewAccess(Popup.View).SetState(State);

      DoInitPopup(Popup.View);

      ModalHandler.Loop(Popup.View, False, False, False, TrackButton = tbRightButton);
      DoneActionData := TTBViewAccess(Popup.View).DoneActionData;
    finally
      { Remove vsModal state from the root view before any TTBView.Destroy
        methods get called, so that NotifyFocusEvent becomes a no-op }
      State := Popup.View.State;
      Exclude(State, vsModal);
      TTBViewAccess(Popup.View).SetState(State);
      Popup.Free;
    end;
  finally
    ModalHandler.Free;
  end;

  ClickedItem := ProcessDoneAction(DoneActionData, ReturnClickedItemOnly);
  Result := True;

  if Assigned(W) then begin
    // Send a message to the PopupControl and it's children controls
    // to inform that the Popup is closed.
    Msg.Msg := CM_SPPOPUPCLOSE;
    Msg.WParam := Integer(Self);
    Msg.LParam := 0;
    Msg.Result := 0;
    PostMessage(W.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
    W.Broadcast(Msg);
  end;
end;

procedure TSpTBXPopupMenu.Popup(X, Y: Integer);
begin
  PopupEx(X, Y);
end;

function TSpTBXPopupMenu.PopupEx(X, Y: Integer; PopupControl: TControl = nil;
  ReturnClickedItemOnly: Boolean = False): TTBCustomItem;
begin
  InternalPopup(X, Y, Result, PopupControl, ReturnClickedItemOnly);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCompoundItemsControl }

constructor TSpTBXCompoundItemsControl.Create(AOwner: TComponent);
begin
  inherited;

  FDock := GetDockClass.Create(Self);
  FDock.Parent := Self;
  FDock.OnRequestDock := DockRequestDock;

  FToolbar := GetToolbarClass.Create(Self);
  FToolbar.CompoundToolbar := True;
  FToolbar.Parent := FDock;
  FToolbar.CurrentDock := FDock;
  FToolbar.BorderStyle := bsNone;
  FToolbar.DockMode := dmCannotFloatOrChangeDocks;
  FToolbar.DragHandleStyle := dhNone;
  FToolbar.Stretch := True;
  FToolbar.ShrinkMode := tbsmNone;
  FToolbar.ShowCaption := False;

  FThemeType := thtTBX;
  AddThemeNotification(Self);
end;

procedure TSpTBXCompoundItemsControl.CreateParams(var Params: TCreateParams);
begin
  // Disable complete redraws when size changes. CS_HREDRAW and CS_VREDRAW
  // cause flicker and are not necessary for this control at run time
  // Invalidate in WMWindowPosChanged message instead.
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    with Params do
      WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TSpTBXCompoundItemsControl.Destroy;
begin
  RemoveThemeNotification(Self);
  FToolbar.Free;
  FDock.Free;
  inherited;
end;

procedure TSpTBXCompoundItemsControl.Loaded;
var
  I: Integer;
  C: TControl;
begin
  inherited;

  // The parent of TTBControlItem.Control should be the toolbar, not Self
  // (as setted in GetChildren for dfm streaming).
  for I := 0 to Items.Count - 1 do
    if Items[I] is TTBControlItem then begin
      C := TTBControlItem(Items[I]).Control;
      if Assigned(C) and (C.Parent <> FToolbar) then
        C.Parent := FToolbar;
    end;
end;

procedure TSpTBXCompoundItemsControl.DockRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
begin
  if Assigned(FToolbar) then Accept := Bar = FToolbar;
end;

procedure TSpTBXCompoundItemsControl.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  C: TControl;
begin
  // Needed to fake the DFM streaming system because the owner of the items
  // is the Form and not the Toolbar nor Self.
  // But the parent must be the Toolbar.
  // GetChildren is used to pass the children components of Self to the DFM
  // streaming system.
  // We also need to do the same with the controls of TTBControlItems.
  // More info on the Delphi help or Classes.TWriter.WriteData

  TTBXRootItemAccess(Items).GetChildren(Proc, Root);
  for I := 0 to Items.Count - 1 do
    if (Items[I] is TTBControlItem) then
      if Assigned(TTBControlItem(Items[I]).Control) then begin
        C := TTBControlItem(Items[I]).Control;
        if SpFindControl(Self, C) = -1 then Proc(C);
      end;
  inherited;
end;

function TSpTBXCompoundItemsControl.GetItems: TTBCustomItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXCompoundItemsControl.GetRootItems: TTBRootItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXCompoundItemsControl.GetDockClass: TSpTBXDockClass;
begin
  Result := TSpTBXDock;
end;

function TSpTBXCompoundItemsControl.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXToolbar;
end;

function TSpTBXCompoundItemsControl.GetView: TSpTBXToolbarView;
begin
  Result := FToolbar.View as TSpTBXToolbarView;
end;

function TSpTBXCompoundItemsControl.GetImages: TCustomImageList;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Images
  else
    Result := nil;
end;

procedure TSpTBXCompoundItemsControl.InvalidateDockBackground(Resizing: Boolean);
begin
  if not (csDestroying in ComponentState) then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TSpTBXCompoundItemsControl.SetImages(const Value: TCustomImageList);
begin
  if Assigned(FToolbar) then FToolbar.Images := Value;
end;

procedure TSpTBXCompoundItemsControl.SetThemeType(const Value: TSpTBXThemeType);
begin
  if FThemeType <> Value then begin
    FThemeType := Value;
    if HandleAllocated then
      InvalidateDockBackground;
  end;
end;

procedure TSpTBXCompoundItemsControl.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    if HandleAllocated then
      InvalidateDockBackground;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCompoundBar }

constructor TSpTBXCompoundBar.Create(AOwner: TComponent);
begin
  inherited;
  Height := FDock.Height;
  FDock.OnDrawBackground := DrawDockBackground;
  FDock.OnResize := DockResize;
end;

procedure TSpTBXCompoundBar.DockResize(Sender: TObject);
begin
  if Assigned(FDock) then
    if Height <> FDock.Height then
      Height := FDock.Height;
end;

procedure TSpTBXCompoundBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawDockBackground) then FOnDrawDockBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXCompoundBar.DrawDockBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  InternalPaintDefault: Boolean;
begin
  if PaintStage = pstPrePaint then begin
    // OnDrawBackground is triggered by the Dock and by the docked Toolbar.
    // The Toolbar triggers it only if Dock.ThemedBackground is true, which depends
    // on CurrentTheme.PaintDockBackground, this is done in
    // TTBXToolbar.WMEraseBkgnd.
    ACanvas.Brush.Color := clBtnFace;
    InternalPaintDefault := True;
    DoDrawDockBackground(ACanvas, ARect, pstPrePaint, InternalPaintDefault);
    PaintDefault := InternalPaintDefault;
    InternalPaintDefault := True;
    DoDrawDockBackground(ACanvas, ARect, pstPostPaint, InternalPaintDefault);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXButtonOptions }

constructor TSpTBXButtonOptions.Create(AParent: TWinControl);
begin
  inherited Create;

  FToolbar := nil;
  if AParent is TSpTBXCompoundItemsControl then
    FToolbar := TSpTBXCompoundItemsControl(AParent).FToolbar;
  FParentControl := AParent;

  FCaption := True;
  FClose := True;
  FMinimize := True;
  FMaximize := True;
  FCaptionImageIndex := -1;
  FCloseImageIndex := -1;
  FMinimizeImageIndex := -1;
  FMaximizeImageIndex := -1;
  FRestoreImageIndex := -1;
  FTitleBarSize := 17;
  CreateButtons;
end;

procedure TSpTBXButtonOptions.CreateButtons;
begin
  if not (csDesigning in FParentControl.ComponentState) then begin
    FRightAlignSpacer := TSpTBXRightAlignSpacerItem.Create(nil);
    FToolbar.Items.Add(FRightAlignSpacer);

    FCloseButton := TSpTBXItem.Create(nil);
    SetupButton(FCloseButton);
    FCloseButton.Visible := FClose;

    FMaximizeButton := TSpTBXItem.Create(nil);
    SetupButton(FMaximizeButton);
    FMaximizeButton.Visible := FMaximize;

    FMinimizeButton := TSpTBXItem.Create(nil);
    SetupButton(FMinimizeButton);
    FMinimizeButton.Visible := FMinimize;
  end;
end;

procedure TSpTBXButtonOptions.SetupButton(B: TSpTBXCustomItem);
begin
  B.CustomWidth := 17;
  B.CustomHeight := FTitleBarSize;
  B.DisplayMode := nbdmImageAndText;
  B.OnDrawImage := ButtonsDrawImage;
  B.OnDrawItem := ButtonsDrawItem;
  B.OnClick := ButtonsClick;
  FToolbar.Items.Add(B);
  B.Visible := False;
  SetupButtonIcon(B);
end;

procedure TSpTBXButtonOptions.SetupButtonIcon(B: TSpTBXCustomItem);
var
  Index, GlyphIndex: integer;
begin
  if not (csDesigning in FParentControl.ComponentState) and Assigned(B) then begin
    Index := -1;
    GlyphIndex := -1;

    if B = FRightAlignSpacer then begin
      Index := FCaptionImageIndex;
    end else
    if B = FCloseButton then begin
      Index := FCloseImageIndex;
      GlyphIndex := 0;
    end else
    if B = FMaximizeButton then begin
      if Restoring(B) then begin
        Index := FRestoreImageIndex;
        GlyphIndex := 3;
      end
      else begin
        Index := FMaximizeImageIndex;
        GlyphIndex := 1;
      end;
    end else
    if B = FMinimizeButton then begin
      if Restoring(B) then begin
        Index := FRestoreImageIndex;
        GlyphIndex := 3;
      end
      else begin
        Index := FMinimizeImageIndex;
        GlyphIndex := 2;
      end;
    end;

    if Index = -1 then begin
      B.Images := MDIButtonsImgList;
      B.ImageIndex := GlyphIndex;
    end
    else begin
      B.Images := nil;
      B.ImageIndex := Index;
    end;
  end;
end;

procedure TSpTBXButtonOptions.ReorderButtons;
var
  L: TTBRootItem;
  I, C: Integer;
begin
  // ReorderButtons is called by the Loading method of the parent component
  if not (csDesigning in FParentControl.ComponentState) then begin
    FRightAlignSpacer.FontSettings.Bold := tsTrue;
    FRightAlignSpacer.Wrapping := twEndEllipsis;
    SetTitleBarSize(FTitleBarSize);

    L := FToolbar.Items;
    C := L.Count - 1;

    I := L.IndexOf(FRightAlignSpacer);
    L.Move(I, 0);
    I := L.IndexOf(FMinimizeButton);
    L.Move(I, C);
    I := L.IndexOf(FMaximizeButton);
    L.Move(I, C);
    I := L.IndexOf(FCloseButton);
    L.Move(I, C);
    FToolbar.RightAlignItems;
  end;
end;

procedure TSpTBXButtonOptions.UpdateButtonsVisibility;
begin
  FRightAlignSpacer.Visible := FCaption or FClose or FMaximize or FMinimize;
end;

procedure TSpTBXButtonOptions.ButtonsDrawImage(Sender: TObject;
  ACanvas: TCanvas; const ItemInfo: TTBXItemInfo;
  const PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList;
  var AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean);
begin
  // Empty, useful for descendants
end;

procedure TSpTBXButtonOptions.ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  // [TBXTheme-Change]
  // Don't draw the items background if the Default theme is used
  if (PaintStage = pstPrePaint) and (not ButtonBorders or (TBXCurrentTheme = 'Default')) then
    PaintDefault := False;
end;

procedure TSpTBXButtonOptions.SetCaptionLabel(const Value: WideString);
begin
  if FCaptionLabel <> Value then begin
    FCaptionLabel := Value;
    if Assigned(FRightAlignSpacer) then
      FRightAlignSpacer.Caption := Value;
  end;
end;

procedure TSpTBXButtonOptions.SetCaption(const Value: Boolean);
begin
  FCaption := Value;
  if Assigned(FRightAlignSpacer) then begin
    if Value then
      FRightAlignSpacer.Caption := CaptionLabel
    else
      FRightAlignSpacer.Caption := '';
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetClose(const Value: Boolean);
begin
  FClose := Value;
  if Assigned(FCloseButton) then begin
    FCloseButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetMaximize(const Value: Boolean);
begin
  FMaximize := Value;
  if Assigned(FMaximizeButton) then begin
    FMaximizeButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetMinimize(const Value: Boolean);
begin
  FMinimize := Value;
  if Assigned(FMinimizeButton) then begin
    FMinimizeButton.Visible := Value;
    UpdateButtonsVisibility;
  end;
end;

procedure TSpTBXButtonOptions.SetCaptionImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FCaptionImageIndex := Value;
  if Assigned(FRightAlignSpacer) then SetupButtonIcon(FRightAlignSpacer);
end;

procedure TSpTBXButtonOptions.SetCloseImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FCloseImageIndex := Value;
  if Assigned(FCloseButton) then SetupButtonIcon(FCloseButton);
end;

procedure TSpTBXButtonOptions.SetMinimizeImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FMinimizeImageIndex := Value;
  if Assigned(FMinimizeButton) then SetupButtonIcon(FMinimizeButton);
end;

procedure TSpTBXButtonOptions.SetMaximizeImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FMaximizeImageIndex := Value;
  if Assigned(FMaximizeButton) then SetupButtonIcon(FMaximizeButton);
end;

procedure TSpTBXButtonOptions.SetRestoreImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FRestoreImageIndex := Value;
  SetupButtonIcon(FMinimizeButton);
  SetupButtonIcon(FMaximizeButton);
end;

procedure TSpTBXButtonOptions.SetTitleBarSize(const Value: Integer);
begin
  FTitleBarSize := Value;
  TSpTBXToolbarView(FToolbar.View).MaxSize := Value;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXStatusToolbar }

constructor TSpTBXStatusToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FThemeType := thtTBX;
  FSizeGrip := True;
end;

destructor TSpTBXStatusToolbar.Destroy;
begin
  FParentForm := nil;
  inherited;
end;

function TSpTBXStatusToolbar.GetParentFormWindowState: TWindowState;
// This method is more accurate than FParentForm.WindowState
var
  R: TRect;
begin
  if not Assigned(FParentForm) then
    FParentForm := GetParentForm(Self);
  Result := SpGetFormWindowState(FParentForm, R);
end;

function TSpTBXStatusToolbar.IsPointInGrip(P: TPoint): Boolean;
var
  GR: TRect;
begin
  Result := False;
  GR := GetGripRect;
  if not IsRectEmpty(GR) and PtInRect(GR, P) then
    Result := True;
end;

function TSpTBXStatusToolbar.GetGripRect: TRect;
var
  C: TWinControl;
begin
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and FSizeGrip and
    Assigned(CurrentDock) and (GetParentFormWindowState = wsNormal) then
  begin
    C := SpFindParent(Self, TSpTBXTitleBar);
    if (Assigned(C) and TSpTBXTitleBar(C).Active and not TSpTBXTitleBar(C).FixedSize) or
      (GetWindowLong(FParentForm.Handle, GWL_STYLE) and WS_THICKFRAME <> 0) then
    begin
      Result := CurrentDock.ClientRect;
      Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
    end;
  end;
end;

function TSpTBXStatusToolbar.GetRightAlignMargin: Integer;
var
  R: TRect;
begin
  R := GetGripRect;
  Result := R.Right - R.Left;
  if Result = 0 then
    Result := 4;
end;

function TSpTBXStatusToolbar.NeedsSeparatorRepaint: Boolean;
begin
  // [TBXTheme-Change]
  // Office themes have rectangle panels, the separator needs
  // to be painted by the Toolbar.
  Result := FUsingOfficeTheme or (SpXPThemeType(FThemeType) = thtNone);
end;

procedure TSpTBXStatusToolbar.SetSizeGrip(const Value: Boolean);
begin
  if FSizeGrip <> Value then begin
    FSizeGrip := Value;
    if Owner is TSpTBXStatusBar then
      TSpTBXStatusBar(Owner).InvalidateDockBackground;
  end;
end;

procedure TSpTBXStatusToolbar.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    FUsingOfficeTheme := (TBXCurrentTheme = 'OfficeXP') or (TBXCurrentTheme = 'Office11Adaptive');
end;

procedure TSpTBXStatusToolbar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  // Resize the StatusBar if the parent is TSpTBXTitleBar
  if not (csDesigning in ComponentState) and (Button = mbLeft) and Assigned(FParentForm) then begin
    P := Point(X, Y);
    if IsPointInGrip(P) then begin
      ReleaseCapture;
      SendMessage(FParentForm.Handle, WM_SYSCOMMAND, $F008, 0);
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXStatusToolbar.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and Assigned(FParentForm) then begin
    P := ScreenToClient(SmallPointToPoint(TSmallPoint(GetMessagePos())));
    if IsPointInGrip(P) then begin
      ReleaseCapture;
      SendMessage(FParentForm.Handle, WM_SYSCOMMAND, $F008, 0);
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXStatusToolbar.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (Screen.Cursor = crDefault) and Assigned(FParentForm) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    if IsPointInGrip(P) then begin
      Windows.SetCursor(Screen.Cursors[-8]);
      Message.Result := 1;
      Exit;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomStatusBar }

constructor TSpTBXCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  Align := alBottom;
end;

function TSpTBXCustomStatusBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if Assigned(FDock) and (NewHeight <> FDock.Height) then
    Result := False
  else
    Result := inherited CanResize(NewWidth, NewHeight);
end;

procedure TSpTBXCustomStatusBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  G, R: TRect;
begin
  inherited DoDrawDockBackground(ACanvas, ARect, PaintStage, PaintDefault);
  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;
    G := Toolbar.GetGripRect;
    if not IsRectEmpty(G) then begin
      // When it's called by the Toolbar the Gripper position should be corrected
      if (ARect.Left = -2) and (ARect.Top = -2) then
        OffsetRect(G, -2, -2);
    end;

    if Toolbar.NeedsSeparatorRepaint then begin
      TBXUtils.FillRectEx(ACanvas.Handle, ARect, clBtnFace);
      if (ARect.Left = -2) and (ARect.Top = -2) then begin
        R := ARect;
        InflateRect(R, -2, -2);

        if not IsRectEmpty(G) and (SpXPThemeType(ThemeType) = thtNone) then begin
          G.Top := R.Top + 1;
          G.Bottom := R.Bottom - 1;
          G.Right := R.Right - 1;
        end;

        SpDrawXPStatusBar(ACanvas, R, G, ThemeType, Toolbar.FUsingOfficeTheme);
        DrawSeparators(ACanvas, ARect, G);
      end
      else
        TBXUtils.FillRectEx(ACanvas.Handle, ARect, clBtnFace);
    end
    else
      SpDrawXPStatusBar(ACanvas, ARect, G, ThemeType, Toolbar.FUsingOfficeTheme);
  end;
end;

procedure TSpTBXCustomStatusBar.DrawSeparators(ACanvas: TCanvas; ARect, AGripRect: TRect);
var
  I: Integer;
  IV: TTBItemViewer;
  R: TRect;
  C: TColor;
begin
  C := ACanvas.Pen.Color;
  try
    for I := 0 to FToolbar.View.ViewerCount - 1 do begin
      IV := FToolbar.View.Viewers[I];
      if (IV is TSpTBXSeparatorItemViewer) and (not TSpTBXSeparatorItem(IV.Item).Blank) then
      begin
        R := IV.BoundsRect;
        R.Top := 0;
        R.Bottom := Height;
        R.Left := ((R.Right + R.Left) div 2) - 2;
        R.Right := R.Left + 3;

        ACanvas.FillRect(R);
        ACanvas.Pen.Color := clWindow;
        ACanvas.PenPos := Point(R.Left, 0);
        ACanvas.LineTo(R.Left, Height);

        if SpXPThemeType(FThemeType) = thtNone then
          ACanvas.Pen.Color := clBtnShadow
        else
          ACanvas.Pen.Color := clWindow; // for OfficeXP theme
        ACanvas.PenPos := Point(R.Right, 0);
        ACanvas.LineTo(R.Right, Height);
      end;
    end;
  finally
    ACanvas.Pen.Color := C;
  end;
end;

function TSpTBXCustomStatusBar.GetStatusToolbar: TSpTBXStatusToolbar;
begin
  Result := FToolbar as TSpTBXStatusToolbar;
end;

function TSpTBXCustomStatusBar.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXStatusToolbar;
end;

function TSpTBXCustomStatusBar.GetSizeGrip: Boolean;
begin
  Result := Toolbar.SizeGrip;
end;

procedure TSpTBXCustomStatusBar.SetSizeGrip(const Value: Boolean);
begin
  Toolbar.SizeGrip := Value;
end;

procedure TSpTBXCustomStatusBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanelToolbar }

constructor TSpTBXTitleToolbar.Create(AOwner: TComponent);
begin
  inherited;
  CompoundToolbar := True;
end;

function TSpTBXTitleToolbar.GetRightAlignMargin: Integer;
begin
  Result := 4;
end;

function TSpTBXTitleToolbar.GetTitleBar: TSpTBXCustomTitleBar;
begin
  Result := CurrentDock.Parent as TSpTBXCustomTitleBar;
end;

procedure TSpTBXTitleToolbar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TransparentClick: Boolean;
  F: TCustomForm;
  TitleBar: TSpTBXCustomTitleBar;
  P: TPoint;
  IV: TTBItemViewer;
begin
  // Move the Parent Form if the toolbar client area or an item with
  // tbisClicksTransparent itemstyle is clicked (like a TBXLabelItem)
  if not (csDesigning in ComponentState) then begin
    TitleBar := GetTitleBar;
    F := TitleBar.FParentForm;
    if not Assigned(F) or not Assigned(TitleBar) then Exit;

    if Assigned(View.Selected) then
      TransparentClick := tbisClicksTransparent in TTBCustomItemAccess(View.Selected.Item).ItemStyle
    else
      TransparentClick := True;

    case Button of
      mbLeft:
        if TransparentClick then begin
          if ssDouble in Shift then begin
            // Maximize or restore when double clicking the toolbar
            if TitleBar.Options.Maximize and not TitleBar.FixedSize then
              TitleBar.Options.MaximizeButton.Click;
          end
          else
            if F.WindowState <> wsMaximized then begin
              // Drag the form when dragging the toolbar
              ReleaseCapture;
              SendMessage(F.Handle, WM_SYSCOMMAND, $F012, 0);
            end;
          Exit; // Do not process transparent clicks
        end
        else
          if (ssDouble in Shift) and TitleBar.Options.SystemMenu then begin
            // Close the form when the system menu button is double clicked
            IV := View.ViewerFromPoint(Point(X, Y));
            if Assigned(IV) and (IV.Item = TitleBar.Options.SystemButton) then begin
              F.Close;
              Exit; // Do not process transparent clicks
            end;
          end;
      mbRight:
        if TransparentClick and TitleBar.Options.SystemMenu then begin
          P := ClientToScreen(Point(X, Y));
          TitleBar.ShowSystemMenu(P);
          Exit; // Do not process transparent clicks
        end;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXTitleBarButtonOptions }

constructor TSpTBXTitleBarButtonOptions.Create(AParent: TWinControl);
var
  Info: TSHFileInfo;
begin
  FSystemMenu := True;
  FSystemImageIndex := -1;
  FTitleBar := AParent as TSpTBXCustomTitleBar;
  inherited Create(AParent);

  FSystemMenuIcon := TIcon.Create;
  if not (csDesigning in AParent.ComponentState) then begin
    FillChar(Info, SizeOf(TSHFileInfo), 0);
    SHGetFileInfo(PChar(Application.ExeName), 0, Info, SizeOf(TSHFileInfo), SHGFI_ICON or SHGFI_SMALLICON);
    FSystemMenuIcon.Handle := Info.hIcon;
  end;
end;

destructor TSpTBXTitleBarButtonOptions.Destroy;
begin
  FSystemMenuIcon.Free;
  inherited;
end;

procedure TSpTBXTitleBarButtonOptions.CreateButtons;
begin
  if not (csDesigning in FParentControl.ComponentState) then begin
    FSystemButton := TSpTBXItem.Create(nil);
    SetupButton(FSystemButton);
    FSystemButton.Visible := True;
  end;
  inherited;
end;

procedure TSpTBXTitleBarButtonOptions.ReorderButtons;
var
  L: TTBRootItem;
  I: Integer;
begin
  inherited;
  if not (csDesigning in FParentControl.ComponentState) then begin
    L := FToolbar.Items;
    I := L.IndexOf(FSystemButton);
    L.Move(I, 0);
    CaptionLabel := (FParentControl as TSpTBXTitleBar).Caption;
  end;
end;

procedure TSpTBXTitleBarButtonOptions.SetSystemImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  FSystemImageIndex := Value;
  if Assigned(FSystemButton) then FSystemButton.ImageIndex := Value;
end;

procedure TSpTBXTitleBarButtonOptions.SetSystemMenu(const Value: Boolean);
begin
  FSystemMenu := Value;
  if not (csDesigning in FParentControl.ComponentState) then
    FSystemButton.Visible := Value;
end;

procedure TSpTBXTitleBarButtonOptions.SetupButtonIcon(B: TSpTBXCustomItem);
begin
  if not (csDesigning in FParentControl.ComponentState) and (B = FSystemButton) then begin
    B.Images := nil;
    B.ImageIndex := FSystemImageIndex;
  end
  else
    inherited;
end;

procedure TSpTBXTitleBarButtonOptions.ButtonsDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TTBXItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  inherited;
  // Don't draw the items background of the FSystemButton if the ImageIndex is not setted
  if (PaintStage = pstPrePaint) and (Sender = FSystemButton) and (FSystemButton.ImageIndex = -1) then begin
    PaintDefault := False;
    ACanvas.Draw(ARect.Left, ARect.Top, FSystemMenuIcon);
  end;
end;

procedure TSpTBXTitleBarButtonOptions.ButtonsClick(Sender: TObject);
var
  IV: TTBItemViewer;
  R: TRect;
  P: TPoint;
begin
  if not Assigned(FTitleBar.FParentForm) then Exit;

  if Sender = FSystemButton then begin
    IV := FToolbar.View.Find(Sender as TTBCustomItem);
    R := IV.BoundsRect;
    P := FToolbar.ClientToScreen(Point(R.Left, R.Bottom));
    FTitleBar.ShowSystemMenu(P);
  end
  else begin
    if Sender = FMinimizeButton then begin
      FTitleBar.WindowState := wsMinimized
    end
    else
      if Sender = FCloseButton then
        FTitleBar.FParentForm.Close
      else
        if Sender = FMaximizeButton then begin
          if FTitleBar.WindowState = wsNormal then
            FTitleBar.WindowState := wsMaximized
          else
            FTitleBar.WindowState := wsNormal;
        end;
  end;
end;

function TSpTBXTitleBarButtonOptions.Restoring(B: TSpTBXCustomItem): Boolean;
begin
  Result := False;
  if (B = MaximizeButton) and (FTitleBar.WindowState = wsMaximized) then Result := True;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomTitleBar }

constructor TSpTBXCustomTitleBar.Create(AOwner: TComponent);
begin
  inherited;
  FActive := True;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque];

  Align := alClient;
  FDock.OnResize := nil;
  FBorderWidth := GetSystemMetrics(SM_CYFRAME);

  FGradientB := TBitmap.Create;
  FParentForm := GetParentForm(Self);
  FOptions := TSpTBXTitleBarButtonOptions.Create(Self);
end;

procedure TSpTBXCustomTitleBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TSpTBXCustomTitleBar.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

procedure TSpTBXCustomTitleBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

destructor TSpTBXCustomTitleBar.Destroy;
begin
  ChangeTitleBarState(False);
  FOptions.Free;
  FGradientB.Free;
  if Assigned(FParentForm) and Assigned(FOldParentFormWndProc) then begin
    FParentForm.WindowProc := FOldParentFormWndProc;
    FOldParentFormWndProc := nil;
  end;

  inherited;
end;

procedure TSpTBXCustomTitleBar.Loaded;
begin
  inherited;
  // Subclass the ParentForm for the System menu handling
  FParentForm := GetParentForm(Self);
  if Assigned(FParentForm) then begin
    FOldParentFormWndProc :=  FParentForm.WindowProc;
    FParentForm.WindowProc := NewParentFormWndProc;
  end;
  ChangeTitleBarState(Active);
  FOptions.ReorderButtons;
end;

procedure TSpTBXCustomTitleBar.NewParentFormWndProc(var Message: TMessage);
var
  M: TWMSysCommand;
  I: Integer;
  HandleSpaceKey: Boolean;

  MMI: ^TMinMaxInfo;
  WorkArea, TaskBarBounds: TRect;
  TaskBarState, TaskBarEdge: Cardinal;
begin
  if not Assigned(FParentForm) then Exit;

  case Message.Msg of
    WM_GETMINMAXINFO:
      if Active and not FFullScreenMaximize then begin
        MMI := Pointer(Message.lParam);
        SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea,0);

        // Max position
        MMI^.ptMaxPosition.X := WorkArea.Left;
        MMI^.ptMaxPosition.Y := WorkArea.Top;

        // Max Size
        MMI^.ptMaxSize.X := WorkArea.Right - WorkArea.Left;
        MMI^.ptMaxSize.Y := WorkArea.Bottom - WorkArea.Top;

        // Reduce the Max Size if the TaskBar is AutoHidden
        if SpGetTaskBar(TaskBarState, TaskBarEdge, TaskBarBounds) then begin
          if (TaskBarState and ABS_AUTOHIDE) = ABS_AUTOHIDE then
            case TaskBarEdge of
              ABE_LEFT, ABE_RIGHT: MMI^.ptMaxSize.X :=  MMI^.ptMaxSize.X - 2;
              ABE_TOP, ABE_BOTTOM: MMI^.ptMaxSize.Y :=  MMI^.ptMaxSize.Y - 2;
            end;
        end;

        // Max size during window resize
        MMI^.ptMaxTrackSize.X := MMI^.ptMaxSize.X;
        MMI^.ptMaxTrackSize.Y := MMI^.ptMaxSize.Y;

        // Change the ParentForm constraints
        // http://news.jrsoftware.org/read/article.php?id=12734&group=jrsoftware.toolbar2000.thirdparty
        FParentForm.Constraints.MaxWidth := MMI^.ptMaxSize.X;
        FParentForm.Constraints.MaxHeight := MMI^.ptMaxSize.Y;

        Message.Result := 0;
      end;
    WM_SYSCOMMAND:
      begin
        HandleSpaceKey := True;
        M := TWMSysCommand(Message);
        if FActive and (M.CmdType and $FFF0 = SC_KEYMENU) then
          case M.Key of
            VK_SPACE:
              begin
                // Show the custom SysMenu
                FOptions.SystemButton.Click;
                Message.Result := 1;
                Exit;
              end;
            0:
              if GetCapture = 0 then begin
                // When only the Alt key is pressed and a few seconds latter the Space
                // key is pressed the default SysMenu is showed, this only happens
                // when there are no menubars on the form.
                // In this case the WM_SYSCOMMAND is sent when the Alt key is
                // pressed (Key = 0), but not when the space key is pressed.
                // Apparently there's no way to override this, the only solution is to
                // handle the Alt key press (Key = 0).
                //
                // Message log when Alt [...] Space is pressed:
                //  WM_SYSKEYDOWN: VK_MENU
                //  WM_SYSKEYUP: VK_MENU
                //  WM_SYSCOMMAND: Key = 0
                //  WM_ENTERMENULOOP
                //  WM_INITMENU
                //  WM_KEYDOWN: VK_SPACE
                //  WM_CHAR: VK_SPACE
                //  WM_INITMENUPOPUP: system hmenu

                // If the form has a main menu VK_SPACE will be correctly handled
                if Assigned(FParentForm.Menu) then
                  HandleSpaceKey := False
                else
                  for I := 0 to FParentForm.ComponentCount - 1 do
                    if FParentForm.Components[I] is TTBCustomToolbar then
                      if TTBCustomToolbar(FParentForm.Components[I]).MenuBar then begin
                        HandleSpaceKey := False;
                        Break;
                      end;
                if HandleSpaceKey then begin
                  Message.Result := 1;
                  Exit;
                end;
              end;
          end;
      end;
  end;

  if Assigned(FOldParentFormWndProc) then
    FOldParentFormWndProc(Message);
end;

procedure TSpTBXCustomTitleBar.ChangeTitleBarState(Activate: Boolean);
var
  Style: Integer;
  RestoreR: TRect;
  WState: TWindowState;
  OnParentFormShow: TNotifyEvent;
begin
  FParentForm := GetParentForm(Self);
  if Assigned(FParentForm) and ([csDesigning, csDestroying] * FParentForm.ComponentState = []) then
  begin
    FActive := Activate;
    // Changing the BorderStyle of the form will recreate it,
    // causing it to call Form.OnShow everytime Active is changed
    // We need to disable the OnShow calling.
    OnParentFormShow := TCustomFormAccess(FParentForm).OnShow;
    TCustomFormAccess(FParentForm).OnShow := nil;
    try
      WState := SpGetFormWindowState(FParentForm, RestoreR);

      if Activate then begin
        // Remove the border and titlebar from the form, and add the sysmenu
        FParentForm.BorderStyle := bsNone;
        Style := GetWindowLong(FParentForm.Handle, GWL_STYLE);
        Style := Style or WS_SYSMENU;
        SetWindowLong(FParentForm.Handle, GWL_STYLE, Style);

        // Resize the form to retain the same size before it was activated.
        // This is needed to keep the designtime size
        if WState <> wsMaximized then begin
          FParentForm.Height := FParentForm.Height - GetSystemMetrics(SM_CYCAPTION) - (FBorderWidth * 2);
          FParentForm.Width := FParentForm.Width - (GetSystemMetrics(SM_CXFRAME) * 2);
        end;
      end
      else begin
        FParentForm.BorderStyle := bsSizeable;

        // Resize the form to retain the same size before it was deactivated.
        // This is needed to keep the designtime size
        if WState <> wsMaximized then begin
          FParentForm.Height := FParentForm.Height + GetSystemMetrics(SM_CYCAPTION) + (FBorderWidth * 2);
          FParentForm.Width := FParentForm.Width + (GetSystemMetrics(SM_CXFRAME) * 2);
        end;
      end;

      // When Active is changed the Form is recreated, we have to
      // reset the Restore size if the form is currently Maximized.
      if WState = wsMaximized then
        SpSetFormWindowState(FParentForm, WState, RestoreR);

      FDock.Visible := Activate;
      InvalidateDockBackground;
    finally
      TCustomFormAccess(FParentForm).OnShow := OnParentFormShow;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if Active then begin
    if Assigned(FParentForm) then begin
      if FParentForm.WindowState <> wsMaximized then
        InflateRect(Rect, -FBorderWidth, -FBorderWidth);
    end
    else
      InflateRect(Rect, -FBorderWidth, -FBorderWidth);
  end;
end;

procedure TSpTBXCustomTitleBar.DoDrawDockBackground(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
var
  W, H: Integer;
begin
  inherited DoDrawDockBackground(ACanvas, ARect, PaintStage, PaintDefault);
  if (PaintStage = pstPrePaint) and PaintDefault then begin
    PaintDefault := False;
    // [TBXTheme-Change]
    // The Default theme draws double title bars: TTBXDefaultTheme.PaintFloatingBorder (how to paint captionless window frame)
    // TSpTBXCustomTitleBar.WMEraseBkgnd and TSpTBXCustomTitleBar.DoDrawDockBackground handles this issue
    if (TBXCurrentTheme = 'Default') and USE_THEMES then
      InflateRect(ARect, FBorderWidth, FBorderWidth);

    W := ARect.Right - ARect.Left;
    H := ARect.Bottom - ARect.Top;

    // [TBXTheme-Change]
    // The Default Win9x/Win2k theme draws gradient title bars
    // I had to use a temp bitmap, otherwise the Dock paints with a white background...
    ACanvas.Lock;
    try
      if (TBXCurrentTheme = 'Default') and not USE_THEMES then begin
        if Assigned(FGradientB) and ((FGradientB.Width <> W) or (FGradientB.Height <> H)) then begin
          FGradientB.Width := W;
          FGradientB.Height := H;
          SpDrawXPGradientTitleBar(FGradientB.Canvas, ARect, True)
        end;
        ACanvas.Draw(0, 0, FGradientB);
      end
      else
        SpDrawXPTitleBar(Self.Handle, ACanvas, ARect, W, H, True, False);
    finally
      ACanvas.Unlock;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXCustomTitleBar.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

procedure TSpTBXCustomTitleBar.GetSizeCursor(MousePos: TPoint; var SizeCursor,
  SizeCode: Integer);
var
  R: TRect;
  Pt: TPoint;
const
  SC_SizeLeft      = $F001;
  SC_SizeRight     = $F002;
  SC_SizeUp        = $F003;
  SC_SizeUpLeft    = $F004;
  SC_SizeUpRight   = $F005;
  SC_SizeDown      = $F006;
  SC_SizeDownLeft  = $F007;
  SC_SizeDownRight = $F008;
begin
  SizeCursor := 0;
  SizeCode := 0;

  if not Active or (Assigned(FParentForm) and (FParentForm.WindowState = wsMaximized)) then Exit;

  R := ClientRect;
  InflateRect(R, -FBorderWidth, -FBorderWidth);
  Pt := MousePos;
  if not PtInRect(R, Pt) then begin
    if (Pt.X < 10) and (Pt.Y < 10) then SizeCode := SC_SizeUpLeft
    else if (Pt.X > Width - 10) and (Pt.Y < 10) then SizeCode := SC_SizeUpRight
    else if (Pt.X < 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDownLeft
    else if (Pt.X > Width - 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDownRight
    else if (Pt.X > 10) and (Pt.X < Width - 10) and (Pt.Y < 10) then SizeCode := SC_SizeUp
    else if (Pt.X > 10) and (Pt.X < Width - 10) and (Pt.Y > Height - 10) then SizeCode := SC_SizeDown
    else if (Pt.Y > 10) and (Pt.Y < Height - 10) and (Pt.X < 10) then SizeCode := SC_SizeLeft
    else if (Pt.Y > 10) and (Pt.Y < Height - 10) and (Pt.X > Width - 10) then SizeCode := SC_SizeRight;

    case SizeCode of
      SC_SizeLeft, SC_SizeRight: SizeCursor := -9;
      SC_SizeUp, SC_SizeDown: SizeCursor := -7;
      SC_SizeUpLeft, SC_SizeDownRight: SizeCursor := -8;
      SC_SizeDownLeft, SC_SizeUpRight: SizeCursor := -6;
    end;
  end
end;

function TSpTBXCustomTitleBar.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TSpTBXTitleToolbar;
end;

function TSpTBXCustomTitleBar.GetWindowState: TWindowState;
begin
  if Assigned(FParentForm) then
    Result := FParentForm.WindowState
  else
    Result := wsNormal;
end;

procedure TSpTBXCustomTitleBar.InvalidateBackground(InvalidateChildren: Boolean);
begin
  // Invalidate will not fire WM_ERASEBKGND, because csOpaque is setted
  if HandleAllocated then
    if InvalidateChildren then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN)
    else
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
end;

function TSpTBXCustomTitleBar.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

procedure TSpTBXCustomTitleBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  SizeCursor, SizeCode: Integer;
begin
  inherited;
  if not FixedSize and (Button = mbLeft) then begin
    Pt := Point(X, Y);
    GetSizeCursor(Pt, SizeCursor, SizeCode);
    if (SizeCode > 0) and Assigned(FParentForm) then begin
      ReleaseCapture;
      FParentForm.Perform(WM_SYSCOMMAND, SizeCode, 0);
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.ShowSystemMenu(ScreenPos: TPoint);
begin
  if Assigned(FParentForm) then
    SpShowSystemSpTBXPopupMenu(FParentForm, ScreenPos, not FixedSize,
      Options.Minimize, Options.Maximize, Options.Close, FOnSystemMenuPopup);
end;

procedure TSpTBXCustomTitleBar.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
    ChangeTitleBarState(Value);
end;

procedure TSpTBXCustomTitleBar.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TSpTBXCustomTitleBar.SetFullScreenMaximize(const Value: Boolean);
begin
  if FFullScreenMaximize <> Value then begin
    FFullScreenMaximize := Value;
    if Active then
      if FParentForm.BorderStyle = bsNone then
        TCustomFormAccess(FParentForm).RecreateWnd;
  end;
end;

procedure TSpTBXCustomTitleBar.SetTBXStyleBackground(const Value: Boolean);
begin
  if FTBXStyleBackground <> Value then begin
    FTBXStyleBackground := Value;
    InvalidateBackground;
  end;
end;

procedure TSpTBXCustomTitleBar.SetWindowState(const Value: TWindowState);
begin
  if Assigned(FParentForm) then begin
    case Value of
      wsMinimized:
        begin
          // WindowState := wsMinimized will not minimize the app correctly
          SendMessage(FParentForm.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        end;
      wsMaximized, wsNormal:
        FParentForm.WindowState := Value;
    end;
  end;
end;

procedure TSpTBXCustomTitleBar.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FOptions) then
    FOptions.CaptionLabel := Caption;
end;

procedure TSpTBXCustomTitleBar.TBMThemeChange(var Message: TMessage);
begin
  inherited;
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    if Assigned(Options) and Assigned(Options.RightAlignSpacer) then begin
      // [TBXTheme-Change]
      // Adjust the caption text color accordingly
      if TBXCurrentTheme = 'Default' then
        Options.RightAlignSpacer.FontSettings.Color := clCaptionText
      else
        Options.RightAlignSpacer.FontSettings.Color := clNone;
    end;
end;

procedure TSpTBXCustomTitleBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  C: TCanvas;
  R: TRect;
  Maximized, PaintDefault: Boolean;
begin
  Message.Result := 1;
  C := TCanvas.Create;
  try
    C.Handle := Message.DC;
    C.Lock;
    R := GetClientRect;
    C.Brush.Color := Color; // SpDrawXPTitleBar needs it to paint the background

    PaintDefault := True;
    DoDrawBackground(C, R, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      Maximized := WindowState = wsMaximized;
      if Maximized then
        InflateRect(R, 4, 4);
      if Active then begin
        // [TBXTheme-Change]
        // The Default theme draws double title bars: TTBXDefaultTheme.PaintFloatingBorder (how to paint captionless window frame)
        // TSpTBXCustomTitleBar.WMEraseBkgnd and TSpTBXCustomTitleBar.DoDrawDockBackground handles this issue
        if (TBXCurrentTheme = 'Default') and USE_THEMES and Assigned(FDock) and not Maximized then
          SpDrawXPTitleBar(Self.Handle, C, R, FDock.Width + FBorderWidth + FBorderWidth, FDock.Height + FBorderWidth, True, TBXStyleBackground)
        else
          SpDrawXPTitleBar(Self.Handle, C, R, 0, 0, True, TBXStyleBackground);
      end
      else begin
        C.Brush.Color := Color;
        C.FillRect(R);
      end;
    end;

    PaintDefault := True;
    DoDrawBackground(C, R, pstPostPaint, PaintDefault);
  finally
    C.UnLock;
    C.Handle := 0;
    C.Free;
  end;
end;

procedure TSpTBXCustomTitleBar.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  SizeCursor, SizeCode: Integer;
begin
  if not FixedSize and not (csDesigning in ComponentState) and
    (Message.CursorWnd = Handle) and (Screen.Cursor = crDefault) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    GetSizeCursor(P, SizeCursor, SizeCode);
    if SizeCursor <> 0 then begin
      Windows.SetCursor(Screen.Cursors[SizeCursor]);
      Message.Result := 1;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TSpTBXCustomTitleBar.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  InvalidateBackground(False);
  if ([csDesigning, csDestroying] * ComponentState = []) and (FOptions.Maximize) then
    FOptions.SetupButtonIcon(FOptions.MaximizeButton);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TBitmapHint }

procedure TBitmapHint.ActivateHint(Rect: TRect; const AHint: string);
var
  SaveActivating: Boolean;
begin
  SaveActivating := FActivating;
  try
    FActivating := True;
    inherited;
  finally
    FActivating := SaveActivating;
  end;
end;

procedure TBitmapHint.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  //The AData parameter is a bitmap
  FHintBitmap := TBitmap(AData);
  Rect.Right := Rect.Left + FHintBitmap.Width - 2;
  Rect.Bottom := Rect.Top + FHintBitmap.Height - 2;
  inherited ActivateHintData(Rect, AHint, AData);
end;

procedure TBitmapHint.CMTextChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TBitmapHint.Paint;
begin
  if Assigned(FHintBitmap) then
    Canvas.Draw(0, 0, FHintBitmap);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXSkinOptionEntry }

constructor TSpTBXSkinOptionEntry.Create;
begin
  inherited;
  Reset;
end;

function TSpTBXSkinOptionEntry.IsEmpty: Boolean;
begin
  Result := (FEntryType = 0) and SpIsEmptyColor(FColor1) and SpIsEmptyColor(FColor2) and
    SpIsEmptyColor(FColor3) and SpIsEmptyColor(FColor4);
end;

procedure TSpTBXSkinOptionEntry.Reset;
begin
  FEntryType := 0;
  FColor1 := clNone;
  FColor2 := clNone;
  FColor3 := clNone;
  FColor4 := clNone;
end;

procedure TSpTBXSkinOptionEntry.ReadFromString(S: string);
var
  L: TStringList;
begin
  Reset;
  L := TStringList.Create;
  try
    L.CommaText := S;
    try
      if L.Count > 0 then FEntryType := StrToIntDef(L[0], 0);
      if L.Count > 1 then FColor1 := StringToColor(L[1]);
      if L.Count > 2 then FColor2 := StringToColor(L[2]);
      if L.Count > 3 then FColor3 := StringToColor(L[3]);
      if L.Count > 4 then FColor4 := StringToColor(L[4]);
    except
      // do nothing
    end;
  finally
    L.Free;
  end;
end;

function TSpTBXSkinOptionEntry.WriteToString: string;
begin
  Result := Format('%d, %s, %s, %s, %s', [FEntryType,
    SpColorToString(FColor1), SpColorToString(FColor2),
    SpColorToString(FColor3), SpColorToString(FColor4)]);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptionCategory }

constructor TSpTBXSkinOptionCategory.Create;
begin
  inherited;
  FImage := TPicture.Create;
  FBody := TSpTBXSkinOptionEntry.Create;
  FBorders := TSpTBXSkinOptionEntry.Create;
  FInternalBorders := TSpTBXSkinOptionEntry.Create;
  FEffectiveColor := clNone;
end;

destructor TSpTBXSkinOptionCategory.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FBody);
  FreeAndNil(FBorders);
  FreeAndNil(FInternalBorders);
  inherited;
end;

function TSpTBXSkinOptionCategory.IsEmpty: Boolean;
begin
  Result := FBody.IsEmpty and FBorders.IsEmpty and FInternalBorders.IsEmpty and SpIsEmptyColor(FEffectiveColor);
end;

procedure TSpTBXSkinOptionCategory.Reset;
begin
  FBody.Reset;
  FBorders.Reset;
  FInternalBorders.Reset;
  FEffectiveColor := clNone;
end;

procedure TSpTBXSkinOptionCategory.SaveToIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  if not IsEmpty then begin
    MemIni.WriteString(Section, Ident + '.Body', Body.WriteToString);
    MemIni.WriteString(Section, Ident + '.Borders', Borders.WriteToString);
    MemIni.WriteString(Section, Ident + '.InternalBorders', InternalBorders.WriteToString);

    MemIni.WriteString(Section, Ident + '.EffectiveColor', SpColorToString(EffectiveColor));
  end;
end;

procedure TSpTBXSkinOptionCategory.LoadFromIni(MemIni: TMemIniFile; Section, Ident: string);
begin
  Reset;
  if Ident = '' then Ident := SSpTBXSkinStatesString[sknsNormal];

  Body.ReadFromString(MemIni.ReadString(Section, Ident + '.Body', ''));
  Borders.ReadFromString(MemIni.ReadString(Section, Ident + '.Borders', ''));
  InternalBorders.ReadFromString(MemIni.ReadString(Section, Ident + '.InternalBorders', ''));

  EffectiveColor := StringToColor(MemIni.ReadString(Section, Ident + '.EffectiveColor', 'clNone'));
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXThemeOptions }

constructor TSpTBXSkinOptions.Create;
var
  SC: TSpTBXSkinSingleStateComponentsType;
  MC: TSpTBXSkinMultiStateComponentsType;
  S: TSpTBXSkinStatesType;
begin
  inherited;

  for SC := Low(SC) to High(SC) do
    FSingleStateOptions[SC] := TSpTBXSkinOptionCategory.Create;

  for MC := Low(MC) to High(MC) do
    for S := Low(S) to High(S) do
      FMultiStateOptions[MC, S] := TSpTBXSkinOptionCategory.Create;
end;

destructor TSpTBXSkinOptions.Destroy;
var
  SC: TSpTBXSkinSingleStateComponentsType;
  MC: TSpTBXSkinMultiStateComponentsType;
  S: TSpTBXSkinStatesType;
begin
  for SC := Low(SC) to High(SC) do
    FreeAndNil(FSingleStateOptions[SC]);

  for MC := Low(MC) to High(MC) do
    for S := Low(S) to High(S) do
      FreeAndNil(FMultiStateOptions[MC, S]);

  inherited;
end;

function TSpTBXSkinOptions.IsActive: Boolean;
begin
  Result := TBXCurrentTheme = rvSpTBXThemeName;
end;

function TSpTBXSkinOptions.GetTheme: TSpTBXDefaultTheme;
begin
  if IsActive then
    Result := CurrentTheme as TSpTBXDefaultTheme
  else
    Result := nil;
end;

procedure TSpTBXSkinOptions.Reset;
var
  SC: TSpTBXSkinSingleStateComponentsType;
  MC: TSpTBXSkinMultiStateComponentsType;
  S: TSpTBXSkinStatesType;
begin
  for SC := Low(SC) to High(SC) do
    Options(SC).Reset;

  for MC := Low(MC) to High(MC) do
    for S := Low(S) to High(S) do
      Options(MC, S).Reset;
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinSingleStateComponentsType): TSpTBXSkinOptionCategory;
begin
  Result := FSingleStateOptions[Component];
end;

function TSpTBXSkinOptions.Options(Component: TSpTBXSkinMultiStateComponentsType; State: TSpTBXSkinStatesType): TSpTBXSkinOptionCategory;
begin
  Result := FMultiStateOptions[Component, State];
end;

procedure TSpTBXSkinOptions.SaveToFile(Filename: WideString);
var
  MemIni: TMemIniFile;
  SC: TSpTBXSkinSingleStateComponentsType;
  MC: TSpTBXSkinMultiStateComponentsType;
  S: TSpTBXSkinStatesType;
begin
  MemIni := TMemIniFile.Create(Filename);
  try
    for SC := Low(SC) to High(SC) do
      Options(SC).SaveToIni(MemIni, SSpTBXSkinSingleStateComponentsString[SC], '');;

    for MC := Low(MC) to High(MC) do
      for S := Low(S) to High(S) do
        Options(MC, S).SaveToIni(MemIni, SSpTBXSkinMultiStateComponentsString[MC], SSpTBXSkinStatesString[S]);;

    MemIni.UpdateFile;
  finally
    MemIni.Free;
  end;
end;

procedure TSpTBXSkinOptions.LoadFromFile(Filename: WideString);
var
  MemIni: TMemIniFile;
  SC: TSpTBXSkinSingleStateComponentsType;
  MC: TSpTBXSkinMultiStateComponentsType;
  S: TSpTBXSkinStatesType;
begin
  if TntSysUtils.WideFileExists(Filename) then begin
    Reset;
    MemIni := TMemIniFile.Create(Filename);
    try
      for SC := Low(SC) to High(SC) do
        Options(SC).LoadFromIni(MemIni, SSpTBXSkinSingleStateComponentsString[SC], '');;

      for MC := Low(MC) to High(MC) do
        for S := Low(S) to High(S) do
          Options(MC, S).LoadFromIni(MemIni, SSpTBXSkinMultiStateComponentsString[MC], SSpTBXSkinStatesString[S]);;
    finally
      MemIni.Free;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM

procedure InitializeStock;
var
  B: TBitmap;
begin
  SpStockHintBitmap := TBitmap.Create;
  SpStockHandCursor := LoadCursor(0, IDC_HAND);

  Screen.Cursors[crSpTBXCustomization] := LoadCursor(HInstance, 'CZMOVE');
  Screen.Cursors[crSpTBXCustomizationCancel] := LoadCursor(HInstance, 'CZCANCEL');

  SpThemeNexus := TSpThemeNexus.Create;
  CurrentSkin := TSpTBXSkinOptions.Create;

  // Change the MDI buttons from the TBXGLYPHS resource to monochrome and
  // add them to the MDIButtonsImgList.
  MDIButtonsImgList := TImageList.Create(nil);
  B := TBitmap.Create;
  try
    B.Handle := LoadBitmap(HInstance, 'SPTBXGLYPHS');
    MDIButtonsImgList.AddMasked(B, clFuchsia);
  finally
    B.Free;
  end;
end;

procedure FinalizeStock;
begin
  FreeAndNil(SpStockHintBitmap);
  FreeAndNil(CurrentSkin);
  FreeAndNil(SpThemeNexus);
  FreeAndNil(MDIButtonsImgList);
end;

initialization
  InitializeStock;

finalization
  FinalizeStock;

end.
