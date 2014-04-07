unit SpTBXDkPanels;

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

Wish list for TBX:
  - The SplitHeight and SplitWidth properties of the TBXMultiDock are not updated.
    This causes inconsistences in runtime resizing via SplitHeight/SplitWidth.
    The difference between the Height and the SplitHeight of all the docked
    panels is allways 16, the same size as DSGN_DROPZONESIZE declared
    in TTBXMultiDock.ArrangeToolbars.
    DSGN_DROPZONESIZE is used to leave some space at the bottom of the
    MultiDock at design time to add a drop zone.
    To reproduce it:
    1) Drop a TTBXMultiDock rename it to 'M' and set the Position to dpLeft
    2) Drop 3 TBXDockablePanels rename them to 'DP1', 'DP2' and 'DP3'
    3) Resize the dockable panels at design time
    4) Run this line:
       Caption := Format('%d - %d - %d', [M.Height,
         DP1.Height + DP2.Height + DP3.Height,
         DP1.SplitHeight + DP2.SplitHeight + DP3.SplitHeight]);
    The same problem is present with SplitWidth when the panels are
    horizontally docked.
    To workaround this use the following utility function in Form.OnShow and
    TBXMultiDock.OnResize:

    type
      TTBDockAccess = class(TTBDock);

    procedure UpdateDockSplit(MD: TTBXMultiDock);
    // Fix TBX bug, when the Dock is created SplitHeight and SplitWidth are
    // not updated.
    var
      I: Integer;
      DockList: TList;
      P: TTBXDockablePanel;
    begin
      DockList := TTBDockAccess(MD).DockVisibleList;
      MD.BeginUpdate;
      try
        if MD.Position in [dpLeft, dpRight] then begin
          for I := 0 to DockList.Count - 1 do begin
            P:= DockList[I];
            P.SplitHeight := P.Height;
          end;
        end
        else begin
          for I := 0 to DockList.Count - 1 do begin
            P := DockList[I];
            P.SplitWidth := P.Width;
          end;
        end;
      finally
        MD.EndUpdate;
      end;
    end;
  - When a DockablePanel is docked in a MultiDock it resizes the panels to
    the largest docked panel.
    We should avoid this behavior by adjusting the newly DockedPanel, this is
    done by using the FChangingDock flag and changing the result value of
    GetBaseSize, TTBXMultiDock.ArrangeToolbars uses GetBaseSize to resize
    the docked panel (at line 1479: if K > CurRowSize then CurRowSize := K;).
  - Create a TTBXCustomDockablePanel and make TTBXDockablePanel descend from it.
    This will allow to hide undesirable properties of the component.
  - Add a PaintDockPanelCaptionBackground method to the TTBXTheme class: using
    CurrentTheme.PaintDockPanelNCArea draws ALL the NC area with a fixed caption
    height(GetSystemMetrics(SM_CYSMCAPTION)), there's no way to just draw the
    caption background.
  - Add OnDockedResizing event to TTBXMultiDock, this event should be triggered
    every time a docked panel is resized with the left or right splitter.

Development notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.
  - All the DockablePanels rules are marked with '[DockablePanel-Rule]'.
  - There are two ways of painting the panel caption: using
    CurrentTheme.PaintDockPanelNCArea or CurrentTheme.PaintFloatingBorder.
    PaintDockPanelNCArea paints ALL the NC area!!! and is too small to
    draw, the Height depends on GetSystemMetrics(SM_CYSMCAPTION).
    PaintFloatingBorder can adjust the caption size, but some custom painting
    must be done, because the title bar is painted by the themes.

History:
8 February 2007 - version 1.8.3
  - Fixed incorrect TSpTBXDockablePanel alignment when an
    adjacent splitter was being resized.

17 December 2006 - version 1.8.2
  - Added Toolbar public property to TSpTBXDockablePanel.

24 November 2006 - version 1.8.1
  - Fixed incorrect TSpTBXDockablePanel behavior at designtime when
    ShowCaptionWhenDocked or FixedDockedSize where changed.

27 August 2006 - version 1.8
  - Fixed TSpTBXDockablePanel hint not showing unicode text, thanks
    to Costas Stergiou for reporting this.

15 June 2006 - version 1.7
  - No changes.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - No changes.

10 February 2006 - version 1.3
  - Fixed incorrect TSpTBXDockablePanel docking behavior when
    trying to dock on an empty MultiDock, thanks to Costas Stergiou
    for reporting this.

28 December 2005 - version 1.2
  - Fixed incorrect TSpTBXDockablePanel caption resizing, thanks
    to Costas Stergiou for reporting this.
  - Fixed no hints in TSpTBXDockablePanel, thanks to Erwin Denissen
    for reporting this.
  - Published Autosize property on TSpTBXCheckbox and TSpRadioButton.
  - Improved TSpTBXTextObject to support unicode actions links.
  - Improved TSpTBXTextObject painting methods.

18 October 2005 - version 1.1
  - Fixed incorrect TSpTBXDockablePanel painting when Stripes
    theme was used.
  - Fixed incorrect TSpTBXDockablePanel painting when
    HideWhenInactive is false and the application is deactivated,
    thanks to Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXDockablePanel caption resizing, thanks
    to Costas Stergiou for reporting this.
  - Fixed incorrect TSpTBXPanel painting when the parent is a
    TSpTBXGroupBox with TBXBorderStyle enabled, thanks to
    Marco Wünschmann for reporting this.
  - Added Borders property to TSpTBXPanel and TSpTBXGroupBox.
  - Added Margins property to TSpTBXPanel and TSpTBXGroupBox.
  - Added OnDockedResizing event to TSpTBXMultiDock.

18 August 2005 - version 1.0
  - Fixed ShowCaption and ShowCaptionWhenDocked properties of
    TSpTBXDockablePanel.
  - Fixed flicker in TSpTBXDockablePanel, thanks to Costas Stergiou
    for reporting this.
  - Added TitleBarSize property to the Options of the
    TSpTBXDockablePanel.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - Fixed incorrect focusing in TSpTBXButton.
  - Fixed incorrect buttons painting in TSpTBXDockablePanel.
  - Fixed incorrect caption painting in TSpTBXGroupBox when ClearType
    was enabled, thanks to Aleksander Oven for reporting this.
  - Fixed incorrect child controls transparency painting in TSpTBXPanel
    when TBXStyleBackground was enabled.
  - Added OnMouseDown, OnMouseMove, OnMouseUp events to TSpTBXTrackbar.
  - Added GroupIndex property to TSpTBXRadioButton.
  - Added unicode caption support on TSpTBXDockablePanel.
  - Added default minimize, maximize and close buttons to
    TSpTBXDockablePanel.

16 February 2005 - version 0.7
  - Fixed channel painting issues in TSpTBXTrackBar
  - Fixed TSpTBXDockablePanel docking behavior, the MultiDock shouldn't
    be resized when a dockable panel is docked.
  - Fixed TSpTBXMultiDock resizing bug, a client aligned TSpTBXMultiDock
    can't be resized correctly with the splitter if there's another
    MultiDock aligned at the right side of the form.
  - Added DefaultDockedWidth and DefaultDockedHeight properties to
    TSpTBXDockablePanel, determines the size of the dockable panel when
    it's docked on an empty MultiDock.
  - Added DefaultTitleBar property to TSpTBXDockablePanel, when setted
    to true the default caption and close button will be added to the
    dockable panel.
  - Added TBXStyleBackground property to TSpTBXPanel and TSpTBXGroupBox,
    when setted to true it paints a TBX style background.
  - Added OnDrawBackground event to TSpTBXPanel and TSpTBXGroupBox.

23 December 2004 - version 0.6
  - Fixed vertical channel painting issues in TSpTBXTrackbar.
  - Added ThemeType property to TSpTBXPanel and TSpTBXGroupBox.
  - Added HotTrack property to TSpTBXPanel, this could be used to add TBX
    style borders to other controls.
  - Added selection painting to TSpTBXTrackbar, thanks to picturewilly
    for this addition.
  - Added design time unicode support.

25 August 2004 - version 0.5
  - When the ThemeType is thtTBX and Default is selected as the
    current theme the controls will be painted in Windows theme type.
  - New component added, TSpTBXTrackBar, a trackbar with TBX themes
    support and custom painting events.
  - Added Action property to TSpTBXButton, TSpTBXLabel, TSpTBXCheckBox
    and TSpTBXRadioButton components.
  - Added DropDownMenu property to TSpTBXButton.
  - Added Repeating property to TSpTBXButton.

21 July 2004 - version 0.4
  - New component added, TSpTBXProgressBar, a progress bar with
    TBX themes support and custom painting events.
  - Added Cancel and ModalResult properties for TSpTBXButton.
  - Added CaptionGlow and CaptionGlowColor properties to TSpTBXLabel,
    TSpTBXButton, TSpTBXCheckBox, TSpTBXRadioButton and
    TSpTBXProgressBar. Setting these properties will paint the caption
    outline with the desired color.
  - Added maximize/minimize/restore capability to TSpTBXDockablePanel.
  - Added FixedDockedSize property to TSpTBXDockablePanel, it determines
    if the DockabelPanel is resizable when docked.

12 July 2004 - version 0.3.1
  - Unchanged.

9 July 2004 - version 0.3
  - Fixed incorrect painting on TSpTBXGroupBox when the theme
    service was not available, thanks to Cyril for reporting this.
  - Fixed AVs in SpTBXTabControl when the parent was a Frame,
    thanks to Cyril for fix.
  - Fixed incorrect text wrapping on TSpTBXLabel, thanks
    to Cyril for the fix.
  - Added Underline and UnderlineColor properties to TSpTBXLabel.
  - Added LinkText, LinkTextParams, and LinkFont properties to
    TSpTBXLabel and TSpTBXButton. Setting this properties will
    allow the control to browse the specified LinkText, some of
    the values that the control can execute are:
    - mailto:name@name.com
    - http://www.borland.com
    - ftp://www.borland.com
    - news://news.jrsoftware.org
    - C:\Windows (opens a folder in Windows Explorer, use the
      LinkTextParams property to specify the parameters).
    - Notepad.exe (runs an application, use the LinkTextParams
      property to specify the parameters).
    Note: the controls can execute unicode links and params, for
    example you can open a folder named with unicode text or
    browse for unicode URI web sites, LinkText and LinkTextParams
    are WideStrings.

24 June 2004 - version 0.2
  - Fixed incorrect painting on TSpTBXPanel.
  - Fixed incorrect painting on TSpTBXGroupBox.
  - Fixed TSpTBXRadioButton bug, it was uncheckable.
  - Fixed TSpTBXRadioButton focus selection by keyboard, it now
    checks itself when it gets the focus when using arrow keys.
  - Added ThemeType property to TSpTBXCheckBox, it allows the
    CheckBox to override the active TBX theme.
  - Added ThemeType property to TSpTBXRadioButton, it allows the
    RadioButton to override the active TBX theme.

22 June 2004 - version 0.1
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList, TB2Item, TB2Dock, TB2Toolbar,
  TB2Common, TBX, TBXDkPanels, TBXThemes, SpTBXItem, TntControls,
  Dialogs;

type
  TSpTBXDockablePanel = class;

  TSpTBXDockPosition = (
    dpxLeft,
    dpxTop,
    dpxRight,
    dpxBottom,
    dpxClient
  );

  TSpTBXDockingState = class
  public
    Dock: TTBDock;
    MaximizedSibling: TSpTBXDockablePanel;
    ParentSize: TPoint;
    RestoreSize: TPoint;
    FloatingRestoreSize: TPoint;
    Maximized: Boolean;
    Minimized: Boolean;
    FloatingMinimized: Boolean;
    procedure Assign(Source: TSpTBXDockingState);
  end;

  { TSpTBXMultiDock }

  TSpTBXMultiDock = class(TTBXMultiDock)
  private
    FPosition: TSpTBXDockPosition;
    FOnDockedResizing: TTBXDockedResizing;
    FOnRequestDock: TTBRequestDockEvent;
    procedure SetPosition(Value: TSpTBXDockPosition);
  protected
    function DoDockedResizing(Vertical: Boolean; var NewSize: Integer): Boolean; virtual;
    procedure DoCustomRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean); virtual;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
  public
    procedure ArrangeToolbars; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Position: TSpTBXDockPosition read FPosition write SetPosition default dpxTop;
    property OnDockedResizing: TTBXDockedResizing read FOnDockedResizing write FOnDockedResizing;
    // Republish the event OnRequestDock, TTBDock doesn't have DoRequestDock to override
    property OnRequestDock: TTBRequestDockEvent read FOnRequestDock write FOnRequestDock;
  end;

  { TSpTBXDockablePanelButtonOptions }

  TSpTBXDockablePanelButtonOptions = class(TSpTBXButtonOptions)
  protected
    FDockablePanel: TSpTBXDockablePanel;
    procedure ButtonsClick(Sender: TObject); override;
    procedure CreateButtons; override;
    function Restoring(B: TSpTBXCustomItem): Boolean; override;
    procedure SetupButton(B: TSpTBXCustomItem); override;
  public
    constructor Create(AParent: TWinControl); override;
    procedure ReorderButtons; override;    
  published
    property ButtonBorders default True;
    property Maximize default False;
    property Minimize default False;
    property TitleBarSize default 15;
  end;

  { TSpTBXDockablePanelToolbar }

  TSpTBXDockablePanelToolbar = class(TSpTBXToolbar)
  protected
    function CanItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer): Boolean; override;
    function GetRightAlignMargin: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetParentDockablePanel: TTBXDockablePanel;
  end;

  { TSpTBXDockablePanel }

  TSpTBXDockablePanel = class(TTBXDockablePanel, ITBItems)
  private
    FOptions: TSpTBXDockablePanelButtonOptions;
    FCaptionRotation: TDPCaptionRotation;
    FDefaultDockedHeight: Integer;
    FDefaultDockedWidth: Integer;
    FFixedDockedSize: Boolean;
    FRestoring: Boolean;
    FPanel: TPanel;
    FShowCaptionWhenDocked: Boolean;
    FShowCaption: Boolean;
    FOnDrawCaptionPanel: TSpTBXDrawEvent;
    procedure DockRequestDock(Sender: TObject; Bar: TTBCustomDockableWindow; var Accept: Boolean);
    procedure DockResize(Sender: TObject);
    function GetCaption: TWideCaption;
    function GetCaptionPanelSize: TPoint;
    function GetDPToolbar: TSpTBXDockablePanelToolbar;
    function GetHint: WideString;
    function GetImages: TCustomImageList;
    function GetItems: TTBCustomItem;  // For ITBItems interface
    function GetRootItems: TTBRootItem;
    function GetView: TTBToolbarView;
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure SetCaption(const Value: TWideCaption);
    procedure SetCaptionRotation(const Value: TDPCaptionRotation);
    procedure SetDefaultDockedHeight(Value: Integer);
    procedure SetDefaultDockedWidth(Value: Integer);
    procedure SetHint(const Value: WideString);    
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetShowCaptionWhenDocked(const Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMCapturechanged(var Message: TMessage); message WM_CAPTURECHANGED;
  protected
    FDock: TSpTBXDock;
    FToolbar: TSpTBXDockablePanelToolbar;
    FCaptionPanelBitmap: TBitmap;
    FChangingDock: Boolean;

    FDockingState: TSpTBXDockingState;
    FPrevDockingState: TSpTBXDockingState;

    // Sizing
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    procedure GetBaseSize(var ASize: TPoint); override;
    function DoDockedResizing(Vertical: Boolean; var NewSize: Integer): Boolean; override;
    procedure Resize; override;
    function InternalMaximize(Restore: Boolean): Boolean;
    function RestoreAllSiblings(ProportionalSizing: Boolean = False): Boolean;

    // Painting
    procedure DockDrawBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    procedure DoDrawCaptionPanel(ACanvas: TCanvas; ARect: TRect;
      const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;

    // Component
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // For ITBItems interface
    procedure GetDockPanelInfo(out DockPanelInfo: TTBXDockPanelInfo); override;
    function  GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;
    procedure Loaded; override;
    procedure SetParent(AParent: TWinControl); override;

    procedure SetupDocking;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawCaptionPanel(ACanvas: TCanvas; X, Y: Integer);
    procedure InvalidateBackground(InvalidateChildren: Boolean = True);
    function Maximize: Boolean;
    function Minimize(ProportionalSizing: Boolean = False): Boolean;
    function Restore(ProportionalSizing: Boolean = False): Boolean;
    function Minimized: Boolean;
    function Maximized: Boolean;
    function SiblingsCount: Integer;
    function SizeToggle(ToMaximize: Boolean = False; ProportionalSizing: Boolean = False): Boolean;
    property CaptionPanelSize: TPoint read GetCaptionPanelSize;
    property Toolbar: TSpTBXDockablePanelToolbar read GetDPToolbar;
    property View: TTBToolbarView read GetView;
  published
    // Don't let the streaming system store the WideStrings, use DefineProperties instead
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored; // Hides the inherited Caption
    property Hint: WideString read GetHint write SetHint stored IsHintStored; // Hides the inherited Hint
    // Override ShowCaptionWhenDocked, the inherited property should allways be False
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    // Override ShowCaptionWhenDocked, the inherited property should allways be False
    property ShowCaptionWhenDocked: Boolean read FShowCaptionWhenDocked write SetShowCaptionWhenDocked default True;
    // Override CaptionRotation, the inherited property should allways be dpcrAlwaysHorz
    property CaptionRotation: TDPCaptionRotation read FCaptionRotation write SetCaptionRotation default dpcrAlwaysHorz;
    property MinClientHeight default 8;
    property MinClientWidth default 8;
    property DefaultDockedHeight: Integer read FDefaultDockedHeight write SetDefaultDockedHeight default 150;
    property DefaultDockedWidth: Integer read FDefaultDockedWidth write SetDefaultDockedWidth default 150;
    property Images: TCustomImageList read GetImages write SetImages;
    property Items: TTBRootItem read GetRootItems;
    property FixedDockedSize: Boolean read FFixedDockedSize write FFixedDockedSize default False;
    property Options: TSpTBXDockablePanelButtonOptions read FOptions write FOptions;
    property SupportedDocks default [dkStandardDock, dkMultiDock];
    property OnDrawCaptionPanel: TSpTBXDrawEvent read FOnDrawCaptionPanel write FOnDrawCaptionPanel;
  end;

{ Dockable Panel helpers }
procedure SpDkPanelGetSiblingsState(DP: TSpTBXDockablePanel; out SiblingsCount, MinimizedSiblingsCount: Integer; out FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel);
function SpDkPanelInmediateResizableSibling(DP: TTBXDockablePanel): TTBXDockablePanel;
function SpDkPanelResize(DP: TTBXDockablePanel; NewSize: Integer; ProportionalSizing: Boolean = False): Boolean;
procedure SpDkMultiDockResize(M: TTBXMultiDock; NewSize: Integer);

implementation

uses
  TB2Consts, TntClasses;

const
  HT_TB2k_Border = 2000;

type
  TTBRootItemAccess = class(TTBRootItem);
  TTBCustomItemAccess = class(TTBCustomItem);
  TSpTBXCustomItemAccess = class(TSpTBXCustomItem);
  TTBXDockablePanelAccess = class(TTBXDockablePanel);
  TTBDockAccess = class(TTBDock);
  TControlAccess = class(TControl);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpDkPanelGetSiblingsState(DP: TSpTBXDockablePanel; out SiblingsCount, MinimizedSiblingsCount: Integer;
  out FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel);
var
  DockList: TList;
  DkPanel: TSpTBXDockablePanel;
  I: Integer;
begin
  SiblingsCount := 0;
  MinimizedSiblingsCount := 0;
  FirstMinimizedSibling := nil;
  FirstMaximizedSibling := nil;

  if DP.Docked and Assigned(DP.CurrentDock) then begin
    DockList := TTBDockAccess(DP.CurrentDock).DockVisibleList;
    for I := 0 to DockList.Count - 1 do
      if DockList[I] <> DP then begin
        inc(SiblingsCount);
        if (TObject(DockList[I]) is TSpTBXDockablePanel) then begin
          DkPanel := TObject(DockList[I]) as TSpTBXDockablePanel;
          if DkPanel.Minimized then begin
            if FirstMinimizedSibling = nil then FirstMinimizedSibling := DkPanel;
            inc(MinimizedSiblingsCount);
          end
          else
            if DkPanel.Maximized then
              if FirstMaximizedSibling = nil then FirstMaximizedSibling := DkPanel;
        end;
      end;
  end;
end;

function SpDkPanelInmediateResizableSibling(DP: TTBXDockablePanel): TTBXDockablePanel;
var
  I, J: Integer;
  R: TRect;
  Dock: TTBDock;
  DockList: TList;
  DkPanel: TTBXDockablePanel;
begin
  Result := nil;
  DkPanel := nil;
  Dock := DP.CurrentDock;
  if Assigned(Dock) then DockList := TTBDockAccess(Dock).DockVisibleList
  else DockList := nil;
  if not Assigned(DockList) or (DockList.Count <= 1) or (DP.Floating) or
    (Dock.Position in [dpTop, dpBottom]) then Exit;

  // Resize only the inmediate dockable panel sibling
  // Find DP on the DockList
  for I := 0 to DockList.Count - 1 do
    if DockList[I] = DP then begin
      DkPanel := DockList[I];
      Break;
    end;

  // Find the inmediate resizable sibling
  if Assigned(DkPanel) then begin
    // Find the next sibling
    if I + 1 < DockList.Count then
      for J := I + 1 to DockList.Count - 1 do begin
        DkPanel := DockList[J];
        if DkPanel.SplitHeight > DP.SplitHeight then begin
          R := Rect(1, 1, 0, 0);
          TTBXDockablePanelAccess(DkPanel).ConstrainedResize(R.Left, R.Top, R.Right, R.Bottom);
          if R.Top <> R.Bottom then begin
            Result := DkPanel;
            Break;
          end;
        end;
      end;
    // If not found find the prev sibling
    if not Assigned(Result) then
      for J := I - 1 downto 0 do begin
        DkPanel := DockList[J];
        if DkPanel.SplitHeight > DP.SplitHeight then begin
          R := Rect(1, 1, 0, 0);
          TTBXDockablePanelAccess(DkPanel).ConstrainedResize(R.Left, R.Top, R.Right, R.Bottom);
          if R.Top <> R.Bottom then begin
            Result := DkPanel;
            Break;
          end;
        end;
      end;
  end;
end;

function SpDkPanelResize(DP: TTBXDockablePanel; NewSize: Integer;
  ProportionalSizing: Boolean = False): Boolean;
// Resize the dockable panel to NewSize, if ProportionalSizing is true the
// remaining pixels are distributed proportionally, if it's false only the
// inmediate sibling is resized.
// Horizontal resizing is not supported.
var
  I, H, PrevSize: Integer;
  Dock: TTBDock;
  DockList: TList;
  DkPanel: TTBXDockablePanel;
begin
  Result := False;

  if DP.Floating then begin
    DP.FloatingHeight := NewSize;
    Result := True;
  end
  else begin
    Dock := DP.CurrentDock;
    if Assigned(Dock) then DockList := TTBDockAccess(Dock).DockVisibleList
    else DockList := nil;
    if not Assigned(DockList) or (DockList.Count < 2) then Exit;

    Dock.BeginUpdate;
    try
      // Fix TBX bug, when the Dock is resized SplitHeight is not updated.
      for I := 0 to DockList.Count - 1 do begin
        DkPanel := DockList[I];
        DkPanel.SplitHeight := DkPanel.Height;
      end;

      PrevSize := DP.Height;
      DP.SplitHeight := NewSize;
      DP.Height := NewSize;
      // Redistribute the Dock.Height through all the docked panels
      if ProportionalSizing then begin
        // Proportionally distribute the remaining pixels
        H := (Dock.Height - NewSize) div (DockList.Count - 1);
        for I := 0 to DockList.Count - 1 do begin
          DkPanel := DockList[I];
          if DkPanel <> DP then DkPanel.SplitHeight := H;
        end;
      end
      else begin
        // Resize only the inmediate dockable panel sibling
        // Find DP on the DockList
        DkPanel := SpDkPanelInmediateResizableSibling(DP);
        if Assigned(DkPanel) then
          DkPanel.SplitHeight := DkPanel.Height + (PrevSize - NewSize)
      end;
    finally
      Dock.EndUpdate;
    end;

    Result := True;

    // [DockablePanel-Rule]
    // When the DP is horizontal force the resize
    if Dock.Position in [dpTop, dpBottom] then begin
      Result := True;
      DP.Height := NewSize;
    end
    else
      DP.SplitHeight := DP.Height;
  end;
end;

procedure SpDkMultiDockResize(M: TTBXMultiDock; NewSize: Integer);
var
  I: Integer;
  DockList: TList;
  DP: TTBXDockablePanelAccess;
begin
  DockList := TTBDockAccess(M).DockVisibleList;

  M.ResizeVisiblePanels(NewSize);

  if NewSize = 0 then
    for I := 0 to DockList.Count - 1 do begin
      DP := TTBXDockablePanelAccess(DockList[I]);
      DP.Width := NewSize;
    end;
end;

function SpAdjacentSplitter(Dock: TTBDock; Space: Integer = 1): TControl;
var
  P: TPoint;
  R: TRect;
  C: TControl;
  Splitter: ISpTBXSplitter;
begin
  Result := nil;

  R := Dock.BoundsRect;
  case Dock.Position of
    dpLeft:   P := Point(R.Right + Space, R.Top);
    dpRight:  P := Point(R.Left - Space, R.Top);
    dpTop:    P := Point(R.Left, R.Bottom + Space);
    dpBottom: P := Point(R.Left, R.Top - Space);
  end;

  C := Dock.Parent.ControlAtPos(P, False, True);
  if Assigned(C) then
    if C.GetInterface(ISpTBXSplitter, Splitter) then
      Result := C;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockingState }

procedure TSpTBXDockingState.Assign(Source: TSpTBXDockingState);
begin
  if Assigned(Source) then begin
    Dock := Source.Dock;
    MaximizedSibling := Source.MaximizedSibling;
    ParentSize := Source.ParentSize;
    RestoreSize := Source.RestoreSize;
    FloatingRestoreSize := Source.FloatingRestoreSize;
    Maximized := Source.Maximized;
    Minimized := Source.Minimized;
    FloatingMinimized := Source.FloatingMinimized;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXMultiDock }

procedure TSpTBXMultiDock.ArrangeToolbars;
var
  I: Integer;
  P: TTBXDockablePanel;
begin
  if FPosition = dpxClient then begin
    for I := 0 to DockVisibleList.Count - 1 do begin
      P := DockVisibleList[I];
      P.Width := Width;
    end;
  end;

  inherited;
end;

constructor TSpTBXMultiDock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := dpxTop;
  inherited OnRequestDock := DoCustomRequestDock;
end;

procedure TSpTBXMultiDock.DoCustomRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
var
  P: TPoint;
  R: Trect;
  Dock: TSpTBXMultiDock;
begin
  if (Sender = nil) or not (Sender is TSpTBXMultiDock) then begin
    if Assigned(FOnRequestDock) then FOnRequestDock(Sender, Bar, Accept);
    Exit;
  end;

  Dock := Sender as TSpTBXMultiDock;

  // When trying to dock on an empty MultiDock, and there's a client aligned
  // MultiDock right next to it, the client aligned MultiDock gets the priority.
  // To fix this, give a 1 pixel space for the empty MultiDock.
  // Republish the event OnRequestDock, TTBDock doesn't have DoRequestDock to override
  if Dock.Position = dpxClient then begin
    GetCursorPos(P);
    GetWindowRect(Dock.Handle, R);
    InflateRect(R, -1, -1);
    Accept := PtInRect(R, P);
  end;

  if Assigned(FOnRequestDock) then FOnRequestDock(Sender, Bar, Accept);
end;

function TSpTBXMultiDock.DoDockedResizing(Vertical: Boolean; var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnDockedResizing) then FOnDockedResizing(Self, Vertical, NewSize, rsResizing, Result);
end;

procedure TSpTBXMultiDock.SetPosition(Value: TSpTBXDockPosition);
begin
  if FPosition <> Value then begin
    if (ControlCount <> 0) then
      raise EInvalidOperation.Create(STBDockCannotChangePosition);
    FPosition := Value;
    case Value of
      dpxLeft:   inherited Position := dpLeft;
      dpxTop:    inherited Position := dpTop;
      dpxRight:  inherited Position := dpRight;
      dpxBottom: inherited Position := dpBottom;
      dpxClient:
        begin
          inherited Position := dpRight;
          Align := alClient;
        end;
    end;
    ArrangeToolbars;
  end;
end;

procedure TSpTBXMultiDock.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var
  C: TControl;
begin
  // When resizing a right or bottom aligned MultiDock and there's
  // a Splitter right next to it, the Splitter is moved to the right/bottom side
  // of the MultiDock.
  // To fix this, re align the Splitter after the MultiDock is resized.

  case Position of
    dpxRight:
      if (Width = 0) and (Message.WindowPos.cx > 0) then begin
        C := SpAdjacentSplitter(Self, 1);
        if Assigned(C) then begin
          inherited;  // Resize

          Parent.DisableAlign;
          try
            C.Left := Left - 5;
          finally
            Parent.EnableAlign;
          end;

          Exit;  // Exit after splitter realign
        end;
      end;
    dpxBottom:
      if (Height = 0) and (Message.WindowPos.cy > 0) then begin
        C := SpAdjacentSplitter(Self, 1);
        if Assigned(C) then begin
          inherited;  // Resize

          Parent.DisableAlign;
          try
            C.Top := Top - 1;
          finally
            Parent.EnableAlign;
          end;

          Exit;  // Exit after splitter realign
        end;
      end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanelButtonOptions }

constructor TSpTBXDockablePanelButtonOptions.Create(AParent: TWinControl);
begin
  FDockablePanel := AParent as TSpTBXDockablePanel;
  inherited;
  Maximize := False;
  Minimize := False;
  ButtonBorders := True;
  TitleBarSize := 15;
end;

procedure TSpTBXDockablePanelButtonOptions.CreateButtons;
begin
  FToolbar := FDockablePanel.FToolbar;
  inherited;
end;

procedure TSpTBXDockablePanelButtonOptions.ButtonsClick(Sender: TObject);
begin
  if Sender = MinimizeButton then FDockablePanel.SizeToggle(False)
  else if Sender = MaximizeButton then FDockablePanel.SizeToggle(True)
  else if Sender = CloseButton then FDockablePanel.Close;
end;

function TSpTBXDockablePanelButtonOptions.Restoring(B: TSpTBXCustomItem): Boolean;
begin
  Result := False;
  if Assigned(FDockablePanel) then
    if B = MinimizeButton then
      Result := FDockablePanel.Minimized
    else
      if B = MaximizeButton then
        Result := FDockablePanel.Maximized;
end;

procedure TSpTBXDockablePanelButtonOptions.ReorderButtons;
begin
  inherited;
  if not (csDesigning in FParentControl.ComponentState) then
    CaptionLabel := (FParentControl as TSpTBXDockablePanel).Caption;
end;

procedure TSpTBXDockablePanelButtonOptions.SetupButton(B: TSpTBXCustomItem);
begin
  inherited;
  TSpTBXCustomItemAccess(B).CustomWidth := 15;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanelToolbar }

constructor TSpTBXDockablePanelToolbar.Create(AOwner: TComponent);
begin
  inherited;
  CompoundToolbar := True;
end;

function TSpTBXDockablePanelToolbar.GetParentDockablePanel: TTBXDockablePanel;
var
  P: TWinControl;
begin
  Result := nil;
  P := Parent;
  while Assigned(P) do
    if P is TTBXDockablePanel then begin
      Result := P as TTBXDockablePanel;
      Break;
    end
    else
      P := P.Parent;
end;

function TSpTBXDockablePanelToolbar.GetRightAlignMargin: Integer;
begin
  Result := 4;
end;

function TSpTBXDockablePanelToolbar.CanItemClick(Item: TTBCustomItem;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  TransparentClick: Boolean;        
  DP: TTBXDockablePanel;
begin
  Result := True;

  // Move the DockablePanel if the toolbar client area or an item with
  // tbisClicksTransparent itemstyle is clicked (like a TBXLabelItem)
  if Button = mbLeft then begin
    DP := GetParentDockablePanel;
    if Assigned(DP) and DP.IsMovable then begin
      if Assigned(Item) then
        TransparentClick := tbisClicksTransparent in TTBCustomItemAccess(Item).ItemStyle
      else
        TransparentClick := True;
      if TransparentClick then
        if ssDouble in Shift then
          TTBXDockablePanelAccess(DP).DoubleClick
        else begin
          Result := False;
          SendMessage(DP.Handle, WM_NCLBUTTONDOWN, HT_TB2k_Border, 0);
        end;
    end;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDockablePanel }

constructor TSpTBXDockablePanel.Create(AOwner: TComponent);
begin
  inherited;
  FCaptionPanelBitmap := TBitmap.Create;

  FDockingState := TSpTBXDockingState.Create;
  FDockingState.RestoreSize := Point(Width, Height);
  FDockingState.FloatingRestoreSize := Point(Width, Height);

  FPrevDockingState := TSpTBXDockingState.Create;

  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Align := alTop;
  FPanel.BevelOuter := bvNone;

  FDock := TSpTBXDock.Create(Self);
  FDock.Parent := FPanel;
  FDock.OnRequestDock := DockRequestDock;
  FDock.OnDrawBackground := DockDrawBackground;
  FDock.OnResize := DockResize;

  FToolbar := TSpTBXDockablePanelToolbar.Create(Self);
  FToolbar.Parent := FDock;
  FToolbar.CurrentDock := FDock;
  FToolbar.BorderStyle := bsNone;
  FToolbar.DockMode := dmCannotFloatOrChangeDocks;
  FToolbar.DragHandleStyle := dhNone;
  FToolbar.Options := FToolbar.Options + [tboNoAutoHint];
  FToolbar.Stretch := True;
  FToolbar.ShrinkMode := tbsmNone;
  FToolbar.ShowCaption := False;

  MinClientHeight := 8;
  MinClientWidth := 8;
  FDefaultDockedWidth := 150;
  FDefaultDockedHeight := 150;

  // Hide the NC area of the FloatingWindow
  inherited ShowCaption := False;
  FShowCaption := True;
  // Hide the NC area of the DockablePanel
  inherited ShowCaptionWhenDocked := False;
  FShowCaptionWhenDocked := True;
  // CaptionRotation is not supported
  inherited CaptionRotation := dpcrAlwaysHorz;
  FCaptionRotation := dpcrAlwaysHorz;

  FOptions := TSpTBXDockablePanelButtonOptions.Create(Self);
  AddThemeNotification(Self);
end;

procedure TSpTBXDockablePanel.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, '');
end;

destructor TSpTBXDockablePanel.Destroy;
begin
  FOptions.Free;
  FToolbar.Free;
  FDock.Free;
  FPanel.Free;
  FreeAndNil(FCaptionPanelBitmap);
  FreeAndNil(FDockingState);
  FreeAndNil(FPrevDockingState);
  RemoveThemeNotification(Self);

  inherited;
end;

procedure TSpTBXDockablePanel.Loaded;
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
  FOptions.ReorderButtons;
  InvalidateBackground;
end;

procedure TSpTBXDockablePanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Don't let the streaming system store the WideStrings,
  // we need to store them manually
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

procedure TSpTBXDockablePanel.DockDrawBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault := False;

    // OnDrawBackground is triggered by the Dock and by the docked Toolbar.
    // The Toolbar triggers it only if Dock.ThemedBackground is true, which depends
    // on CurrentTheme.PaintDockBackground, this is done in
    // TTBXToolbar.WMEraseBkgnd.

    if Floating then begin
      if ARect.Top = 0 then
        DrawCaptionPanel(ACanvas, -1, -1)  // Dock
      else
        DrawCaptionPanel(ACanvas, -3, -3); // Toolbar
    end
    else
      DrawCaptionPanel(ACanvas, ARect.Left - 2, ARect.Top - 2);
  end;
end;

procedure TSpTBXDockablePanel.DockRequestDock(Sender: TObject;
  Bar: TTBCustomDockableWindow; var Accept: Boolean);
begin
  if Assigned(FToolbar) then
    Accept := Bar = FToolbar;
end;

procedure TSpTBXDockablePanel.DoDrawCaptionPanel(ACanvas: TCanvas;
  ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawCaptionPanel) then
    FOnDrawCaptionPanel(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

procedure TSpTBXDockablePanel.DrawCaptionPanel(ACanvas: TCanvas; X, Y: Integer);
var
  R: TRect;
  PaintDefault: Boolean;
  W, H: Integer;
  Wnd: THandle;
begin
  if not Assigned(Parent) or not Assigned(FCaptionPanelBitmap) then Exit;

  FCaptionPanelBitmap.Canvas.Lock;
  try
    W := CaptionPanelSize.X;
    H := CaptionPanelSize.Y;
    if Floating then begin
      Dec(W, 2);
      Dec(H, 3);
    end;
    R := Rect(0, 0, W, H);
    if (FCaptionPanelBitmap.Width <> W) or (FCaptionPanelBitmap.Height <> H) then begin
      FCaptionPanelBitmap.Width := W;
      FCaptionPanelBitmap.Height := H;
      if Floating then
        Wnd := Parent.Handle
      else
        Wnd := WindowHandle;

      SpDrawXPDockablePanelTitleBar(Wnd, FCaptionPanelBitmap.Canvas, 0, 0, FCaptionPanelBitmap.Width, FCaptionPanelBitmap.Height, True, Floating)
    end;

    PaintDefault := True;
    DoDrawCaptionPanel(FCaptionPanelBitmap.Canvas, R, pstPrePaint, PaintDefault);
    PaintDefault := True;
    DoDrawCaptionPanel(FCaptionPanelBitmap.Canvas, R, pstPostPaint, PaintDefault);

    ACanvas.Draw(X, Y, FCaptionPanelBitmap);
  finally
    FCaptionPanelBitmap.Canvas.UnLock;
  end;
end;

procedure TSpTBXDockablePanel.InvalidateBackground(InvalidateChildren: Boolean);
begin
  if HandleAllocated then begin
    if Assigned(FCaptionPanelBitmap) then begin
      FCaptionPanelBitmap.Width := 1;
      FCaptionPanelBitmap.Height := 1;
    end;
    if InvalidateChildren then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN)
    else
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE);
  end;
end;

procedure TSpTBXDockablePanel.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
var
  DC: HDC;
  R, CR: TRect;
  ACanvas: TCanvas;
  Sz: Integer;
  DockPanelInfo: TTBXDockPanelInfo;
begin
  if not Docked or not HandleAllocated then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;

  Assert(DC <> 0, 'TSpTBXDockablePanel.DrawNCArea Error');
  try
    GetDockPanelInfo(DockPanelInfo);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      CR := R;
      with DockPanelInfo.BorderSize, CR do
      begin
        InflateRect(CR, -X, -Y);
        if DockPanelInfo.ShowCaption then
        begin
          Sz := CaptionPanelSize.Y;
          if DockPanelInfo.IsVertical then Inc(Top, Sz)
          else Inc(Left, Sz);
        end;
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
    end;
    ACanvas := TCanvas.Create;
    try
      ACanvas.Handle := DC;
      ACanvas.Brush.Color := EffectiveColor;

      CurrentTheme.PaintDockPanelNCArea(ACanvas, R, DockPanelInfo);
      if (Floating and FShowCaption) or (not Floating and FShowCaptionWhenDocked) then begin
        ExcludeClipRect(ACanvas.Handle, 2, 2, CaptionPanelSize.X - 2, CaptionPanelSize.Y - 2);
        DrawCaptionPanel(ACanvas, 0, 0);
      end;
    finally
      ACanvas.Handle := 0;
      ACanvas.Free;
    end;
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

function TSpTBXDockablePanel.GetCaptionPanelSize: TPoint;
var
  P: TPoint;
begin
  if Floating then begin
    P := GetFloatingBorderSize;
    Result := Point(Width + 4, FToolbar.Height + 4);
  end
  else
    Result := Point(Width, FToolbar.Height + 2);
end;

procedure TSpTBXDockablePanel.GetChildren(Proc: TGetChildProc;
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

  TTBRootItemAccess(Items).GetChildren(Proc, Root);
  for I := 0 to Items.Count - 1 do
    if (Items[I] is TTBControlItem) then
      if Assigned(TTBControlItem(Items[I]).Control) then begin
        C := TTBControlItem(Items[I]).Control;
        if SpFindControl(Self, C) = -1 then Proc(C);
      end;
  inherited;
end;

procedure TSpTBXDockablePanel.GetDockPanelInfo(out DockPanelInfo: TTBXDockPanelInfo);
begin
  FillChar(DockPanelInfo, SizeOf(DockPanelInfo), 0);
  DockPanelInfo.WindowHandle := WindowHandle;
  DockPanelInfo.ViewType := GetViewType;
  DockPanelInfo.IsVertical := True;
  if Assigned(CurrentDock) then begin
    DockPanelInfo.IsVertical := not IsVertCaption;
    DockPanelInfo.AllowDrag := CurrentDock.AllowDrag;
  end;
  DockPanelInfo.BorderStyle := BorderStyle;
  CurrentTheme.GetViewBorder(DockPanelInfo.ViewType, DockPanelInfo.BorderSize);
  DockPanelInfo.ClientWidth := ClientWidth;
  DockPanelInfo.ClientHeight := ClientHeight;
  DockPanelInfo.EffectiveColor := EffectiveColor;
  // Don't show caption or close button
  DockPanelInfo.ShowCaption := False;
end;

function TSpTBXDockablePanel.GetDPToolbar: TSpTBXDockablePanelToolbar;
begin
  Result := FToolbar;
end;

function TSpTBXDockablePanel.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TSpTBXFloatingWindowParent;
end;

function TSpTBXDockablePanel.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;

function TSpTBXDockablePanel.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TSpTBXDockablePanel.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self);
end;

function TSpTBXDockablePanel.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self);
end;

function TSpTBXDockablePanel.GetImages: TCustomImageList;
begin
  if Assigned(FToolbar) then
    Result := FToolbar.Images
  else
    Result := nil;
end;

function TSpTBXDockablePanel.GetItems: TTBCustomItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXDockablePanel.GetRootItems: TTBRootItem;
begin
  Result := FToolbar.Items;
end;

function TSpTBXDockablePanel.GetView: TTBToolbarView;
begin
  Result := FToolbar.View;
end;

procedure TSpTBXDockablePanel.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;

procedure TSpTBXDockablePanel.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

procedure TSpTBXDockablePanel.SetCaptionRotation(const Value: TDPCaptionRotation);
begin
  // Do nothing
end;

procedure TSpTBXDockablePanel.SetDefaultDockedHeight(Value: Integer);
begin
  if Value < 8 then Value := 8;
  if FDefaultDockedHeight <> Value then
    FDefaultDockedHeight := Value;
end;

procedure TSpTBXDockablePanel.SetDefaultDockedWidth(Value: Integer);
begin
  if Value < 8 then Value := 8;
  if FDefaultDockedWidth <> Value then
    FDefaultDockedWidth := Value;
end;

procedure TSpTBXDockablePanel.SetImages(const Value: TCustomImageList);
begin
  if Assigned(FToolbar) then
    FToolbar.Images := Value;
end;

procedure TSpTBXDockablePanel.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then begin
    FShowCaption := Value;
    FPanel.Visible := (Floating and FShowCaption) or (not Floating and FShowCaptionWhenDocked);
  end;
end;

procedure TSpTBXDockablePanel.SetShowCaptionWhenDocked(const Value: Boolean);
begin
  if FShowCaptionWhenDocked <> Value then begin
    FShowCaptionWhenDocked := Value;
    FPanel.Visible := (Floating and FShowCaption) or (not Floating and FShowCaptionWhenDocked);

    // The panel can't be hidden at designtime, move it outside the client area
    if (csDesigning in ComponentState) then
      if FPanel.Visible then
        FPanel.Align := alTop
      else begin
        FPanel.Align := alNone;
        FPanel.Top := FPanel.Top - FPanel.Height - 30;
      end;
    
    // Invalidate NC area
    if not Floating and HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
  end;
end;

procedure TSpTBXDockablePanel.SetParent(AParent: TWinControl);
var
  PrevParent: TWinControl;
begin
  // When a DockablePanel is docked in a MultiDock it resizes the panels to
  // the largest docked panel.
  // We should avoid this behavior by adjusting the newly DockedPanel, this is
  // done by using the FChangingDock flag and changing the result value of
  // GetBaseSize, TTBXMultiDock.ArrangeToolbars uses GetBaseSize to resize
  // the docked panel (at line 1479: if K > CurRowSize then CurRowSize := K;).

  PrevParent := Parent;
  if (AParent <> PrevParent) and not (csDestroying in ComponentState) and Assigned(AParent) then
    FChangingDock := True;

  inherited;

  FChangingDock := False;

  if (AParent <> PrevParent) and not (csDestroying in ComponentState) and Assigned(Parent) and
    Assigned(FOptions) then
  begin
    FPanel.Visible := (Floating and FShowCaption) or (not Floating and FShowCaptionWhenDocked);
    // Reset the floating values
    if Parent is TTBFloatingWindowParent then begin
      if FloatingHeight < 18 then
        FloatingHeight := FDockingState.FloatingRestoreSize.Y;
      if FDockingState.FloatingMinimized or Minimized then begin
        Parent.Constraints.MinHeight := 0;
        Parent.Constraints.MaxHeight := 0;
      end;
      FDockingState.FloatingMinimized := False;
    end;

    SetupDocking;

    // Invalidate the buttons
    FOptions.SetupButtonIcon(FOptions.MaximizeButton);
    FOptions.SetupButtonIcon(FOptions.MinimizeButton);
  end;
end;

procedure TSpTBXDockablePanel.GetBaseSize(var ASize: TPoint);
var
  DockList: TList;
  Horizontal : Boolean;
  Empty: Boolean;
begin
  inherited GetBaseSize(ASize);

  // [DockablePanel-Rule]
  // When a DockablePanel is docked in a MultiDock it resizes the panels to
  // the largest docked panel, adjust the panel to a minimum size.
  if not (csDesigning in ComponentState) and not Floating and FChangingDock and Assigned(CurrentDock) then begin
    Horizontal :=  CurrentDock.Position in [dpTop, dpBottom];
    DockList := TTBDockAccess(CurrentDock).DockVisibleList;
    Empty := False;
    if Assigned(DockList) then
      if (DockList.Count = 0) or ((DockList.Count = 1) and (DockList[0] = Self)) then
        Empty := True;

    if not Empty then
      if Horizontal then ASize.Y := 10
      else ASize.X := 10
    else
      if Horizontal then begin
        if Minimized then
          ASize.Y := Height - CalcNCSizes.Y
        else
          ASize.Y := FDefaultDockedHeight;
      end
      else
        ASize.X := FDefaultDockedWidth;
  end;
end;

procedure TSpTBXDockablePanel.SetupDocking;
var
  SiblingsC, MinimizedSiblingsC: Integer;
  FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel;
  Horizontal: Boolean;
begin
  if not Floating and Assigned(CurrentDock) then begin
    // [DockablePanel-Rule]
    // Reset the state if we are mouse-docking and the panel is brought back
    // to the previous dock
    if CurrentDock = FPrevDockingState.Dock then begin
      FDockingState.Assign(FPrevDockingState);
      if Assigned(FPrevDockingState.MaximizedSibling) then
        FPrevDockingState.MaximizedSibling.Minimize;
      Exit;
    end;

    Horizontal := CurrentDock.Position in [dpTop, dpBottom];
    SpDkPanelGetSiblingsState(Self, SiblingsC, MinimizedSiblingsC, FirstMinimizedSibling, FirstMaximizedSibling);

    // [DockablePanel-Rule]
    // When moving a maximized panel restore it
    if Maximized then begin
      if Assigned(FirstMaximizedSibling) then
        // Minimize it when the MultiDock has a maximized panel
        Minimize
      else begin
        // Internal restore
        FDockingState.Maximized := False;
        SpDkPanelResize(Self, FDockingState.RestoreSize.Y, False);
      end;
      // [DockablePanel-Rule]
      // Maximize or restore the MaximizedSibling
      if Assigned(FPrevDockingState.MaximizedSibling) then
        if not FPrevDockingState.MaximizedSibling.Maximize then
          FPrevDockingState.MaximizedSibling.RestoreAllSiblings;
      Exit;
    end
    else begin
      // [DockablePanel-Rule]
      // Minimize it when the MultiDock has a maximized panel
      if Assigned(FirstMaximizedSibling) then begin
        Minimize;
        Exit;
      end;

      // [DockablePanel-Rule]
      // Maximize or restore the MaximizedSibling
      if Assigned(FPrevDockingState.MaximizedSibling) then
        if not FPrevDockingState.MaximizedSibling.Maximize then
          FPrevDockingState.MaximizedSibling.RestoreAllSiblings;
    end;

    if Horizontal then begin
      // [DockablePanel-Rule]
      // Costas rule #3
      // If a DP is moved to an empty horizontal multidock and the DP is
      // minimized it should stay minimized.
      // If a DP is moved to a non-empty horizontal multidock all of the DP
      // should get restored (e.g. you cannot minimize all docks of a horizontal
      // multidock, if there's more than DP docked)
      if SiblingsC = 0 then begin
        if FPrevDockingState.Minimized then Minimize;
      end
      else
        RestoreAllSiblings;
    end
    else begin
      // [DockablePanel-Rule]
      // Costas rule #1:
      // When a minimized DP is moved to an empty multidock it should get restored.
      if Minimized then
        if SiblingsC = 0 then Restore;
    end;
  end;
end;

function TSpTBXDockablePanel.DoDockedResizing(Vertical: Boolean;
  var NewSize: Integer): Boolean;
begin
  Result := inherited DoDockedResizing(Vertical, NewSize);
  if Assigned(CurrentDock) and (CurrentDock is TSpTBXMultiDock) then
    Result := TSpTBXMultiDock(CurrentDock).DoDockedResizing(Vertical, NewSize);
end;

procedure TSpTBXDockablePanel.ConstrainedResize(var MinWidth, MinHeight,
  MaxWidth, MaxHeight: Integer);
begin
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);

  if not FRestoring and not Floating then
    if FFixedDockedSize and not (csDesigning in ComponentState) then begin
      MinHeight := Height;
      MaxHeight := Height;
    end
    else
      if Minimized then begin
        MinHeight := CaptionPanelSize.Y;
        MaxHeight := CaptionPanelSize.Y;
      end
      else
        MinHeight := CaptionPanelSize.Y;
end;

procedure TSpTBXDockablePanel.Resize;
begin
  inherited;
  // Invalidate NC area
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TSpTBXDockablePanel.DockResize(Sender: TObject);
begin
  if FPanel.Height <> FDock.Height then
    FPanel.Height := FDock.Height;
end;

function TSpTBXDockablePanel.InternalMaximize(Restore: Boolean): Boolean;
// Resize the dockable panel to the maximum size, and minimize the rest of
// the panels
// Horizontal resizing is not supported.
var
  I, NewSize: Integer;
  DockList, ResizedList: TList;
  DkPanel: TSpTBXDockablePanel;
begin
  Result := False;
  if Assigned(CurrentDock) then DockList := TTBDockAccess(CurrentDock).DockVisibleList
  else DockList := nil;
  if not Assigned(DockList) or (DockList.Count < 1) or (Floating) or
    (CurrentDock.Position in [dpTop, dpBottom]) then Exit;

  ResizedList := TList.Create;
  try
    CurrentDock.BeginUpdate;
    try
      // Fix TBX bug, when the Dock is resized SplitHeight is not updated.
      for I := 0 to DockList.Count - 1 do begin
        DkPanel := DockList[I];
        DkPanel.SplitHeight := DkPanel.Height;
      end;

      for I := 0 to DockList.Count - 1 do begin
        if (DockList[I] <> Self) and (TControl(DockList[I]) is TSpTBXDockablePanel) then begin
          DkPanel := TControl(DockList[I]) as TSpTBXDockablePanel;
          if Restore then begin
            if DkPanel.Minimized then begin
              DkPanel.FRestoring := True;
              DkPanel.FDockingState.Minimized := False;
              DkPanel.SplitHeight := DkPanel.FDockingState.RestoreSize.Y;
              ResizedList.Add(DkPanel);
              DkPanel.Options.SetupButtonIcon(DkPanel.Options.MinimizeButton);
              DkPanel.Options.SetupButtonIcon(DkPanel.Options.MaximizeButton);
            end;
          end
          else begin
            if not DkPanel.Minimized then begin
              if DkPanel.Maximized then DkPanel.FDockingState.Maximized := False
              else DkPanel.FDockingState.RestoreSize.Y := DkPanel.Height;
              DkPanel.FRestoring := True;
              DkPanel.FDockingState.Minimized := True;
              DkPanel.SplitHeight := DkPanel.CaptionPanelSize.Y;
              ResizedList.Add(DkPanel);
              DkPanel.Options.SetupButtonIcon(DkPanel.Options.MinimizeButton);
              DkPanel.Options.SetupButtonIcon(DkPanel.Options.MaximizeButton);
            end;
          end;
        end;
      end;

      NewSize := CurrentDock.Height;
      // If the Form was resized then resize the DPs proportionally
      if Restore and (NewSize <> FDockingState.ParentSize.Y) then
        SpDkPanelResize(Self, NewSize div DockList.Count, True)
      else begin
        FDockingState.ParentSize := Point(CurrentDock.Width, CurrentDock.Height);
        for I := 0 to DockList.Count - 1 do begin
          if (DockList[I] <> Self) then
            dec(NewSize, TTBXDockablePanel(DockList[I]).SplitHeight);
        end;

        if NewSize <> Height then
          FDockingState.RestoreSize.Y := Height
        else
          NewSize := FDockingState.RestoreSize.Y;

        SplitHeight := NewSize;
      end;

      Result := True;
    finally
      CurrentDock.EndUpdate;
    end;

    // Clear the flag
    for I := 0 to ResizedList.Count - 1 do begin
      DkPanel := ResizedList[I];
      DkPanel.FRestoring := False;
    end;
  finally
    ResizedList.Free;
  end;
end;

function TSpTBXDockablePanel.Maximize: Boolean;
begin
  Result := False;
  if not Maximized then begin
    // [DockablePanel-Rule]
    // Do not maximize if it's the only DP on the dock
    if SiblingsCount > 0 then begin
      FRestoring := True;
      FDockingState.Maximized := True; // Maximized is needed by InternalMaximize
      try
        Result := InternalMaximize(False);
        FDockingState.Maximized := Result;
        if Result then
          FDockingState.Minimized := False;
        FOptions.SetupButtonIcon(FOptions.MinimizeButton);
        FOptions.SetupButtonIcon(FOptions.MaximizeButton);
      finally
        FRestoring := False;
      end;
    end;
  end;
end;

function TSpTBXDockablePanel.Minimize(ProportionalSizing: Boolean = False): Boolean;
var
  Horizontal, CanMinimize: Boolean;
  SiblingsC, MinimizedSiblingsC: Integer;
  FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel;
begin
  Result := False;
  if Floating and not FDockingState.FloatingMinimized then begin
    FRestoring := True;
    FDockingState.FloatingMinimized := True;
    try
      FDockingState.FloatingRestoreSize.Y := FloatingHeight;
      Result := SpDkPanelResize(Self, CaptionPanelSize.Y, ProportionalSizing);
      FDockingState.FloatingMinimized := Result;
      FOptions.SetupButtonIcon(FOptions.MinimizeButton);
      FOptions.SetupButtonIcon(FOptions.MaximizeButton);
      Parent.Constraints.MinHeight := CaptionPanelSize.Y;
      Parent.Constraints.MaxHeight := CaptionPanelSize.Y;
    finally
      FRestoring := False;
    end;
  end
  else
    if not Minimized then begin
      // [DockablePanel-Rule]
      // Only minimize if it's horizontal and is the only DP on the dock
      // Or if it's vertical and it's not the only DP on the dock and the rest of the siblings are not minimized
      Horizontal := Docked and Assigned(CurrentDock) and (CurrentDock.Position in [dpTop, dpBottom]);
      SpDkPanelGetSiblingsState(Self, SiblingsC, MinimizedSiblingsC, FirstMinimizedSibling, FirstMaximizedSibling);

      if Horizontal then
        CanMinimize := SiblingsC = 0
      else
        CanMinimize := (SiblingsC > 0) and (SiblingsC <> MinimizedSiblingsC);

      if CanMinimize then begin
        FRestoring := True;
        try
          if Height > CaptionPanelSize.Y then
            FDockingState.RestoreSize.Y := Height;
          Result := SpDkPanelResize(Self, CaptionPanelSize.Y, ProportionalSizing);
          FDockingState.Minimized := Result;
          if Result then
            FDockingState.Maximized := False;
          FOptions.SetupButtonIcon(FOptions.MinimizeButton);
          FOptions.SetupButtonIcon(FOptions.MaximizeButton);
        finally
          FRestoring := False;
        end;
      end;
    end;
end;

function TSpTBXDockablePanel.Restore(ProportionalSizing: Boolean = False): Boolean;
var
  I: Integer;
  DockList: TList;
  DkPanel: TSpTBXDockablePanel;
begin
  Result := False;
  FRestoring := True;
  try
    if Floating and FDockingState.FloatingMinimized then begin
      Parent.Constraints.MinHeight := 0;
      Parent.Constraints.MaxHeight := 0;
      Result := SpDkPanelResize(Self, FDockingState.FloatingRestoreSize.Y, False);
      if Result then
        FDockingState.FloatingMinimized := False;
    end
    else
      if Maximized then begin
        Result := InternalMaximize(True);
        if Result then
          FDockingState.Maximized := False;
      end
      else
        if Minimized then begin
          Result := SpDkPanelResize(Self, FDockingState.RestoreSize.Y, ProportionalSizing);
          if Result then begin
            FDockingState.Minimized := False;
            // If a sibling was Maximized restore it
            if Assigned(CurrentDock) then begin
              DockList := TTBDockAccess(CurrentDock).DockVisibleList;
              for I := 0 to DockList.Count - 1 do
                if (DockList[I] <> Self) and (TControl(DockList[I]) is TSpTBXDockablePanel) then begin
                  DkPanel := TControl(DockList[I]) as TSpTBXDockablePanel;
                  if DkPanel.Maximized then begin
                    DkPanel.FDockingState.Maximized := False;
                    if DkPanel.Options.Maximize then
                      DkPanel.Options.SetupButtonIcon(DkPanel.Options.MaximizeButton);
                    Break;
                  end;
                end;
            end;
          end;
        end;

    FOptions.SetupButtonIcon(FOptions.MinimizeButton);
    FOptions.SetupButtonIcon(FOptions.MaximizeButton);
  finally
    FRestoring := False;
  end;
end;

function TSpTBXDockablePanel.RestoreAllSiblings(ProportionalSizing: Boolean = False): Boolean;
var
  I: Integer;
  DockList: TList;
  DP: TSpTBXDockablePanel;
begin
  if not Floating and Assigned(CurrentDock) then begin
    DockList := TTBDockAccess(CurrentDock).DockVisibleList;
    for I := 0 to DockList.Count - 1 do
      if TControl(DockList[I]) is TSpTBXDockablePanel then begin
        DP := TControl(DockList[I]) as TSpTBXDockablePanel;
        DP.Restore(ProportionalSizing);
      end;
    Result := True;
  end
  else
    Result := False;
end;

function TSpTBXDockablePanel.SiblingsCount: Integer;
var
  MinimizedSiblingsCount: Integer;
  FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel;
begin
  SpDkPanelGetSiblingsState(Self, Result, MinimizedSiblingsCount, FirstMinimizedSibling, FirstMaximizedSibling);
end;

function TSpTBXDockablePanel.SizeToggle(ToMaximize: Boolean = False;
  ProportionalSizing: Boolean = False): Boolean;
begin
  if (Minimized and not ToMaximize) or (Maximized and ToMaximize) then
    Result := Restore(ProportionalSizing)
  else
    if ToMaximize then Result := Maximize
    else Result := Minimize(ProportionalSizing);
end;

function TSpTBXDockablePanel.Minimized: Boolean;
begin
  if Floating then
    Result := FDockingState.FloatingMinimized
  else
    Result := FDockingState.Minimized;
end;

function TSpTBXDockablePanel.Maximized: Boolean;
begin
  if Floating then
    Result := False
  else
    Result := FDockingState.Maximized;
end;

procedure TSpTBXDockablePanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FOptions) then
    FOptions.CaptionLabel := Caption;
end;

procedure TSpTBXDockablePanel.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
begin
  // TSpTBXDockablePanel is not draggable by the client area like the TBX
  // counterpart because the inherited ShowCaptionWhenDocked is False.
  // This is controlled by TTBXDockablePanel.WMNCHitTest.

  inherited;

  // When ShowCaptionWhenDocked is false and the client area of the
  // DockablePanel is clicked the main form doesn't get activated if it's inactive.
  // This is a TBX bug in TTBXCustomDockablePanel.WMNCHitTest, if
  // ShowCaptionWhenDocked is false the return value for the hit test is
  // HTNOWHERE, should be HTTRANSPARENT;
  if Message.Result = HTNOWHERE then begin
    P := SmallPointToPoint(Message.Pos);
    GetWindowRect(Handle, R);
    if PtInRect(R, P) then
      Message.Result := HTTRANSPARENT;
  end;
  
  // Deactivate the internal splitter if there's an adjacent splitter
  if Message.Result in [HTLEFT..HTBOTTOM] then begin
    if not Floating and Assigned(CurrentDock) then
      if SpAdjacentSplitter(CurrentDock) <> nil then
        Message.Result := HTTRANSPARENT;
  end;
end;

procedure TSpTBXDockablePanel.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  Dock: TTBDock;
  ScreenP, P: TPoint;
  C: TControl;
  SiblingsCount, MinimizedSiblingsCount: Integer;
  FirstMinimizedSibling, FirstMaximizedSibling: TSpTBXDockablePanel;
const
  HT_TBX_SPLITRESIZELEFT = 86;
  HT_TBX_SPLITRESIZERIGHT = 87;
  HT_TBX_SPLITRESIZETOP = 88;
  HT_TBX_SPLITRESIZEBOTTOM = 89;
begin
  Dock := CurrentDock;

  // A client aligned TSpTBXMultiDock can't be resized correctly with the
  // splitter if there's another MultiDock aligned at the right side of the
  // form.
  // To reproduce this move the mouse from the left to the right towards the
  // border between both docks very slowly and as soon as the cursor changes
  // into a resize cursor you begin to drag, you'll see that it won't resize.
  // It will flicker but not resize.
  // Now do the same but move the mouse from the right to the left and again
  // resize as soon as possible. Now the docks can be resized properly.
  // This happens because the client aligned TSpTBXMultiDock doesn't know how
  // to resize when the HitTest is HTRIGHT.

  if not Floating and Assigned(Dock) and (Dock is TSpTBXMultiDock) then
    case Message.HitTest of
      HTLEFT:
        // [DockablePanel-Rule]
        // Get the neighbour left-aligned MultiDock and pass the opposite HitTest
        if TSpTBXMultiDock(Dock).Position = dpxClient then begin
          P := Point(Dock.Left - 6, Dock.Top);
          ScreenP := Dock.Parent.ClientToScreen(P);
          C := Dock.Parent.ControlAtPos(P, False, True);
          if Assigned(C) and (C is TTBXMultiDock) and (TTBXMultiDock(C).Position = dpLeft) then begin
              P := TTBXMultiDock(C).ScreenToClient(ScreenP);
              C := TTBXMultiDock(C).ControlAtPos(P, False, True);
              if Assigned(C) and (C is TTBXDockablePanel)then begin
                TTBXDockablePanelAccess(C).BeginDockedSizing(HTRIGHT);
                Exit;
              end;
          end;
        end;
      HT_TB2k_Border:
        // [DockablePanel-Rule]
        // Keep track of the state in the previous dock, used by SetupDocking
        begin
          FPrevDockingState.Assign(FDockingState);
          FPrevDockingState.Dock := Dock;
          FPrevDockingState.MaximizedSibling := nil;
          // [DockablePanel-Rule]
          // Costas rule #4:
          // When moving a vertical maximized panel, maximize a sibling if
          // the rest is minimized
          if not (Dock.Position in [dpTop, dpBottom]) then begin
            SpDkPanelGetSiblingsState(Self, SiblingsCount, MinimizedSiblingsCount, FirstMinimizedSibling, FirstMaximizedSibling);
            if Assigned(FirstMinimizedSibling) and (SiblingsCount = MinimizedSiblingsCount) then
              FPrevDockingState.MaximizedSibling := FirstMinimizedSibling;
          end;
        end;
    end;

  inherited;
end;

procedure TSpTBXDockablePanel.WMCapturechanged(var Message: TMessage);
begin
  inherited;
  // [DockablePanel-Rule]
  // Clear the previous state
  FPrevDockingState.Dock := nil;
  FPrevDockingState.MaximizedSibling := nil;
end;

procedure TSpTBXDockablePanel.TBMThemeChange(var Message: TMessage);
begin
  if Message.WParam = TSC_AFTERVIEWCHANGE then
    InvalidateBackground;
  inherited;
end;

end.
