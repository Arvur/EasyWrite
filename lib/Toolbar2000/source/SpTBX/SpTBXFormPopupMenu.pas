unit SpTBXFormPopupMenu;

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
  -

Development notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.

History:
8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - Fixed TSpTBXFormPopupMenu resizing flicker.

27 August 2006 - version 1.8
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, TB2Common, TB2Acc, TB2Anim, TB2Item, TB2Dock, TB2Toolbar,
  TBX, TBXUtils, TBXThemes, SpTBXItem, Types;

const
  WM_SPTBX_POPUPROLLUP = WM_USER + 888;

type
  TSpTBXCustomPopupForm = class;

  TSpTBXPopupAnimationType = (
    patNone,
    patSlide,
    patFade
  );

  TSpTBXPopupBorderStyleType = (
    pbsFrame,
    pbsSizeable,
    pbsSizeableBottom,
    pbsSizeableRightBottom
  );

  TSpTBXRollUpEvent = procedure(Sender: TObject; Selected: Boolean) of object;

  TSpTBXPopupSizeGrip = class(TWinControl)
  private
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FPopupForm: TSpTBXCustomPopupForm;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetGripRect: TRect;
    function GetGripSizerRect: TRect;
    function IsScreenPointInGrip(P: TPoint): Boolean;
  published
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  end;

  TSpTBXCustomPopupForm = class(TCustomForm)
  private
    FHooksInstalled: Boolean;
    FOldAppOnMessage: TMessageEvent;
    FOldPopupControlWndProc: TWndMethod;
    FShowShadows: Boolean;
    FAnimation: TSpTBXPopupAnimationType;
    FAnimationDirection: TTBAnimationDirection;
    FBorderStyle: TSpTBXPopupBorderStyleType;
    FSizeGrip: TSpTBXPopupSizeGrip;
    FOnRollDown: TNotifyEvent;
    FOnRollUp: TSpTBXRollUpEvent;
    procedure SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
    procedure InstallHooks;
    procedure UninstallHooks;
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSpTBXPopupRollUp(var Message: TMessage); message WM_SPTBX_POPUPROLLUP;
  protected
    FPopupControl: TControl;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetNCSize: TPoint; virtual; abstract;
    procedure NCPaint(Wnd: HWND; DC: HDC); virtual; abstract;
    procedure CreateShadow; virtual; abstract;
    procedure DestroyShadow; virtual; abstract;
    procedure ResizeShadow; virtual; abstract;
    function IsShadowVisible: Boolean; virtual; abstract;
    function GetSysAnimation: TSpTBXPopupAnimationType;
    procedure AppOnMessageHook(var Msg: TMsg; var Handled: Boolean); virtual;
    procedure PopupControlWindowProc(var Message: TMessage); virtual;
    procedure DoRollDown; virtual;
    procedure DoRollUp(Selected: Boolean); virtual;
    property Animation: TSpTBXPopupAnimationType read FAnimation write FAnimation;
    property AnimationDirection: TTBAnimationDirection read FAnimationDirection write FAnimationDirection default [];
    property BorderStyle: TSpTBXPopupBorderStyleType read FBorderStyle write SetBorderStyle default pbsFrame;
    property ShowShadows: Boolean read FShowShadows write FShowShadows default True;
    property OnRollDown: TNotifyEvent read FOnRollDown write FOnRollDown;
    property OnRollUp: TSpTBXRollUpEvent read FOnRollUp write FOnRollUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFillColor: TColor; virtual;
    procedure RollDown(X, Y, AWidth, AHeight: Integer; FocusPopup: Boolean = True); overload; virtual;
    procedure RollDown(APopupControl: TControl; AWidth, AHeight: Integer; IsVertical: Boolean = False; FocusPopup: Boolean = True); overload; virtual;
    procedure RollUp(Selected: Boolean; FocusParentControl: Boolean = True); virtual;
  end;

  TSpTBXPopupForm = class(TSpTBXCustomPopupForm)
  private
    FShadows: TShadows;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMTB2kPopupShowing(var Message: TMessage); message WM_TB2K_POPUPSHOWING;
    procedure WMTB2kStepAnimation(var Message: TMessage); message WM_TB2K_STEPANIMATION;
    procedure WMTB2kAnimationEnded (var Message: TMessage); message WM_TB2K_ANIMATIONENDED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateShadow; override;
    procedure DestroyShadow; override;
    procedure ResizeShadow; override;
    function IsShadowVisible: Boolean; override;
    function GetNCSize: TPoint; override;
    procedure NCPaint(Wnd: HWND; DC: HDC); override;
  public
    destructor Destroy; override;
    function GetFillColor: TColor; override;
  published
    property Height;
    property Width;

    property BorderStyle;
    property ShowShadows;
    property OnRollDown;
    property OnRollUp;
  end;

  TSpTBXFormPopupMenu = class(TPopupMenu, ISpTBXPopupMenu)
  private
    FDefaultSize: TPoint;
    FItems: Boolean;
    FPopupFocus: Boolean;
    FOldPopupFormClose: TCloseEvent;
    procedure InternalPopupFormClose(Sender: TObject; var Action: TCloseAction);
    function GetBorderStyle: TSpTBXPopupBorderStyleType;
    function GetShowShadows: Boolean;
    procedure SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
    procedure SetShowShadows(const Value: Boolean);
    procedure SetPopupForm(const Value: TCustomForm);
    function GetOnRollUp: TSpTBXRollUpEvent;
    procedure SetOnRollUp(const Value: TSpTBXRollUpEvent);
  protected
    FForm: TSpTBXPopupForm;
    FPopupForm: TCustomForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
      PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X: Integer; Y: Integer); override;
    property PopupForm: TCustomForm read FPopupForm write SetPopupForm;
  published
    property Items: Boolean read FItems; // Hide the Items property
    property BorderStyle: TSpTBXPopupBorderStyleType read GetBorderStyle write SetBorderStyle default pbsFrame;
    property PopupFocus: Boolean read FPopupFocus write FPopupFocus default False;
    property ShowShadows: Boolean read GetShowShadows write SetShowShadows default True;
    property OnClosePopup: TSpTBXRollUpEvent read GetOnRollUp write SetOnRollUp;
  end;

implementation

type
  TCustomFormAccess = class(TCustomForm);
  TShadowsAccess = class(TShadows);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupSizeGrip }

constructor TSpTBXPopupSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TSpTBXCustomPopupForm) then
    FPopupForm := AOwner as TSpTBXCustomPopupForm;
  Align := alBottom;
  Height := 10;
end;

procedure TSpTBXPopupSizeGrip.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXPopupSizeGrip.GetGripRect: TRect;
begin
  // Return the rect of the area that is sizeable.
  // If BorderStyle = pbsSizeableRightBottom the sizeable area is the
  // bottom right corner, when BorderStyle = pbsSizeableRightBottom the
  // sizeable area is the client rect of the SizeGrip
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and Assigned(FPopupForm) and
    (FPopupForm.FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom]) then
  begin
    Result := ClientRect;
    if FPopupForm.BorderStyle = pbsSizeableRightBottom then
      Result := GetGripSizerRect
    else
      Result := ClientRect;
  end;
end;

function TSpTBXPopupSizeGrip.GetGripSizerRect: TRect;
begin
  // Return the rect of the grip sizer, the area that has the
  // dots on the sizer.
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and Assigned(FPopupForm) then begin
    case FPopupForm.BorderStyle of
      pbsSizeableBottom:
        begin
          Result := ClientRect;
          Result.Left := (Result.Right + Result.Left - 20) div 2;
          Result.Right := Result.Left + 20;
        end;
      pbsSizeableRightBottom:
        begin
          Result := ClientRect;
          Result.Left := Result.Right - 14;
        end;
    end;
  end;
end;

function TSpTBXPopupSizeGrip.IsScreenPointInGrip(P: TPoint): Boolean;
var
  GR: TRect;
begin
  Result := False;
  P := ScreenToClient(P);
  GR := GetGripRect;
  if not IsRectEmpty(GR) and PtInRect(GR, P) then
    Result := True;
end;

procedure TSpTBXPopupSizeGrip.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
const
  SC_SizeDown      = $F006;
  SC_SizeDownRight = $F008;
begin
  // Resize the StatusBar if the parent is TSpTBXTitleBar
  if not (csDesigning in ComponentState) and (Button = mbLeft) and Assigned(FPopupForm) then begin
    P := ClientToScreen(Point(X, Y));
    if IsScreenPointInGrip(P) then begin
      ReleaseCapture;
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          SendMessage(FPopupForm.Handle, WM_SYSCOMMAND, SC_SizeDown, 0);
        pbsSizeableRightBottom:
          SendMessage(FPopupForm.Handle, WM_SYSCOMMAND, SC_SizeDownRight, 0);
      end;
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXPopupSizeGrip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  ACanvas: TCanvas;
  PaintDefault: Boolean;
  R, GR, CellR: TRect;
begin
  Message.Result := 1;
  if (csDestroying in ComponentState) then Exit;

  ACanvas := TCanvas.Create;
  ACanvas.Handle := Message.DC;
  try
    R := ClientRect;

    // Draw the background
    PaintDefault := True;
    DoDrawBackground(ACanvas, R, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      GR := Rect(0, 0, 0, 0);
      SpDrawXPStatusBar(ACanvas, R, GR, thtTBX, False);
    end;

    // Draw the grip
    PaintDefault := True;
    DoDrawBackground(ACanvas, R, pstPostPaint, PaintDefault);
    if PaintDefault then begin
      // Grip cells are 4x3 pixels
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          begin
            GR := GetGripSizerRect;
            CellR := GR;
            CellR.Top := CellR.Top + 1;
            SpDrawXPGrip(ACanvas, CellR, True, False);
          end;
        pbsSizeableRightBottom:
          begin
            GR := GetGripSizerRect;
            CellR := GR;
            // Draw 2 cells at the bottom
            CellR.Left := GR.Right - 8;
            CellR.Top := CellR.Bottom - 3 - 1;
            SpDrawXPGrip(ACanvas, CellR, False, False);
            // Draw 1 cell at the top
            CellR.Bottom := CellR.Top - 1;
            CellR.Top := CellR.Bottom - 3;
            CellR.Left := CellR.Left + 4;
            SpDrawXPGrip(ACanvas, CellR, False, False);
          end;
      end;
    end;
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXPopupSizeGrip.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (Screen.Cursor = crDefault) and Assigned(FPopupForm) then
  begin
    GetCursorPos(P);
    if IsScreenPointInGrip(P) then begin
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          Windows.SetCursor(Screen.Cursors[-7]);
        pbsSizeableRightBottom:
          Windows.SetCursor(Screen.Cursors[-8]);
      end;
      Message.Result := 1;
      Exit;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpCustomPopupForm }

constructor TSpTBXCustomPopupForm.Create(AOwner: TComponent);
begin
  // Form doesn't have DFM info
  inherited CreateNew(AOwner);

  SetBounds(0, 0, 0, 0);
  Visible := False;
  Color := GetFillColor;
  FAnimation := GetSysAnimation;
  FAnimationDirection := [];
  FShowShadows := True;
  FSizeGrip := TSpTBXPopupSizeGrip.Create(Self);
  FSizeGrip.Parent := Self;
end;

procedure TSpTBXCustomPopupForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    with Params do begin
      Style := WS_POPUP or WS_CLIPCHILDREN;

      // Add the thickframe on all the BorderStyles
      // We should handle the NC HitTest
      Style := Style or WS_THICKFRAME;

      ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    end;
end;

destructor TSpTBXCustomPopupForm.Destroy;
begin
  DestroyShadow;
  FreeAndNil(FSizeGrip);
  inherited;
end;

procedure TSpTBXCustomPopupForm.DoRollDown;
begin
  if Assigned(FOnRollDown) then FOnRollDown(Self);
end;

procedure TSpTBXCustomPopupForm.DoRollUp(Selected: Boolean);
begin
  if Assigned(FOnRollUp) then FOnRollUp(Self, Selected);
end;

function TSpTBXCustomPopupForm.GetFillColor: TColor;
begin
  Result := clBtnFace;
end;

function TSpTBXCustomPopupForm.GetSysAnimation: TSpTBXPopupAnimationType;
const
  SPI_GETMENUFADE = $1012;
var
  Animate: BOOL;
begin
  Result := patNone;
  if SystemParametersInfo(SPI_GETMENUANIMATION, 0, @Animate, 0) and Animate then
    if SystemParametersInfo(SPI_GETMENUFADE, 0, @Animate, 0) and Animate then
      Result := patFade
    else
      Result := patSlide;
end;

procedure TSpTBXCustomPopupForm.RollDown(X, Y, AWidth,
  AHeight: Integer; FocusPopup: Boolean = True);
begin
  if not Visible then begin
    // Increase the size of the form if the size grip is visible
    if FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom] then begin
      FSizeGrip.Visible := True;
      AHeight := AHeight + FSizeGrip.Height;
    end
    else
      FSizeGrip.Visible := False;

    InstallHooks;
    SetBounds(X, Y, AWidth, AHeight);
    Visible := True;
    // Not sure why this is needed...
    SetWindowPos(WindowHandle, 0, X, Y, AWidth, AHeight, SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);

    if FocusPopup then SetFocus;
    DoRollDown;
  end;
end;

procedure TSpTBXCustomPopupForm.RollDown(APopupControl: TControl;
  AWidth, AHeight: Integer; IsVertical: Boolean = False;
  FocusPopup: Boolean = True);
var
  P, Size: TPoint;
begin
  if not Visible and Assigned(APopupControl) and Assigned(APopupControl.Parent) then begin
    FPopupControl := APopupControl;
    // Increase the size of the form is the size grip is visible
    Size.X := AWidth;
    Size.Y := AHeight;
    if FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom] then
      Size.Y := Size.Y + FSizeGrip.Height;

    P := SpCalcPopupPosition(0, 0, Size.X, Size.Y, APopupControl, IsVertical);
    RollDown(P.X, P.Y, AWidth, AHeight, FocusPopup);
  end
  else
    FPopupControl := nil;
end;

procedure TSpTBXCustomPopupForm.RollUp(Selected: Boolean; FocusParentControl: Boolean = True);
var
  W: TWinControl;
  Msg: TMessage;
begin
  if Visible then begin
    UninstallHooks;
    if Assigned(FPopupControl) and (FPopupControl is TWinControl) then begin
      W := FPopupControl as TWinControl;
      if FocusParentControl and W.CanFocus then
        W.SetFocus;
      // Send a message to the PopupControl and it's children controls
      // to inform that the Popup is closed.
      Msg.Msg := CM_SPPOPUPCLOSE;
      Msg.WParam := Integer(Self);
      Msg.LParam := 0;
      Msg.Result := 0;
      PostMessage(W.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
      W.Broadcast(Msg);
    end;
    DestroyShadow;
    Visible := False;
    DoRollUp(Selected);
  end;

  FPopupControl := nil;
end;

procedure TSpTBXCustomPopupForm.SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
begin
  if FBorderStyle <> Value then
    FBorderStyle := Value;
end;

procedure TSpTBXCustomPopupForm.InstallHooks;
begin
  if not FHooksInstalled then begin
    FHooksInstalled := True;

    // Needed to handle main form mouse clicks when the
    // popup is visible
    FOldAppOnMessage := Application.OnMessage;
    Application.OnMessage := AppOnMessageHook;

    if Assigned(FPopupControl) then begin
      // Needed to handle focus changes when the popup
      // is visible but the ParentControl has the focus,
      // like the Comboboxes
      FOldPopupControlWndProc := FPopupControl.WindowProc;
      FPopupControl.WindowProc := PopupControlWindowProc;
    end;
  end;
end;

procedure TSpTBXCustomPopupForm.UninstallHooks;
begin
  if FHooksInstalled then begin
    FHooksInstalled := False;

    Application.OnMessage := FOldAppOnMessage;
    FOldAppOnMessage := nil;

    if Assigned(FPopupControl) then begin
      FPopupControl.WindowProc := FOldPopupControlWndProc;
      FOldPopupControlWndProc := nil;
    end;
  end;
end;

procedure TSpTBXCustomPopupForm.AppOnMessageHook(var Msg: TMsg; var Handled: Boolean);
begin
  case Msg.message of
    CM_DEACTIVATE:
      begin
        // Rollup when the popup is deactivated
        // Instead of calling Rollup post a message so the
        // Application.OnMessage is processed before
        // the popup is closed, this is needed to handle
        // the mouse clicks on the main form
        PostMessage(Handle, WM_SPTBX_POPUPROLLUP, 0, 0);
      end;
    WM_LBUTTONDOWN..WM_MBUTTONDBLCLK:
      // If the click was not on the popup, rollup and Handle the message
      if GetCapture = 0 then begin
        if (Msg.hwnd <> Handle) and not Windows.IsChild(Handle, Msg.hwnd) then begin
          RollUp(False);
          Handled := True;
        end;
      end;
    WM_NCLBUTTONDOWN..WM_NCMBUTTONDBLCLK:
      // If the click was not on the popup, rollup and Handle the message
      if (Msg.hwnd <> Handle) and not Windows.IsChild(Handle, Msg.hwnd) then begin
        RollUp(False);
        Handled := True;
      end;
  end;
end;

procedure TSpTBXCustomPopupForm.PopupControlWindowProc(var Message: TMessage);
begin
  if Assigned(FOldPopupControlWndProc) then FOldPopupControlWndProc(Message);

  if Visible then
    case Message.Msg of
      CM_FOCUSCHANGED:
        RollUp(False);
      CM_CHILDKEY:
        if Message.WParam = VK_ESCAPE then
          RollUp(False);
      CM_CANCELMODE:
        RollUp(False);
    end;
end;

procedure PopupFormNCPaintProc(Wnd: HWND; DC: HDC; AppData: Longint);
begin
  // Used by TSpTBXCustomPopupForm.WMNCPaint and WMPrint
  TSpTBXCustomPopupForm(AppData).NCPaint(Wnd, DC);
end;

procedure TSpTBXCustomPopupForm.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  RollUp(False);
end;

procedure TSpTBXCustomPopupForm.CMChildKey(var Message: TCMChildKey);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    RollUp(False);
end;

procedure TSpTBXCustomPopupForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_INACTIVE then begin
    // Rollup when the popup is deactivated
    // Instead of calling Rollup post a message so the
    // Application.OnMessage is processed before
    // the popup is closed, this is needed to handle
    // the mouse clicks on the main form
    PostMessage(Handle, WM_SPTBX_POPUPROLLUP, 0, 0);
  end
  else
    // When the popup is activated redraw the caption bar of the Main Form
    SendMessage(Message.ActiveWindow, WM_NCACTIVATE, 1, 0);
end;

procedure TSpTBXCustomPopupForm.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  with GetNCSize do
    InflateRect(Message.CalcSize_Params^.rgrc[0], -X, -Y);
end;

procedure TSpTBXCustomPopupForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  GR: TRect;
  HitOnBorder: Boolean;
begin
  inherited;

  HitOnBorder := Message.Result in [HTLEFT, HTTOP, HTTOPLEFT, HTTOPRIGHT, HTRIGHT, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT];

  if HitOnBorder then
    case FBorderStyle of
      pbsFrame:
        Message.Result := HTNOWHERE;
      pbsSizeableBottom:
        begin
          // Make the NC area resizeable
          P := FSizeGrip.ScreenToClient(SmallPointToPoint(Message.Pos));
          GR := FSizeGrip.GetGripRect;
          if P.Y >= GR.Top then
            Message.Result := HTBOTTOM;
        end;
      pbsSizeableRightBottom:
        begin
          // Make the NC area resizeable
          P := FSizeGrip.ScreenToClient(SmallPointToPoint(Message.Pos));
          GR := FSizeGrip.GetGripRect;
          if P.Y >= GR.Top then
            if P.X >= GR.Left then
              Message.Result := HTBOTTOMRIGHT;
        end;
    end;
end;

procedure TSpTBXCustomPopupForm.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    SelectNCUpdateRgn(Handle, DC, HRGN(Message.WParam));
    PopupFormNCPaintProc(Handle, DC, Longint(Self));
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXCustomPopupForm.WMPrint(var Message: TMessage);
begin
  HandleWMPrint(Handle, Message, PopupFormNCPaintProc, Longint(Self));
end;

procedure TSpTBXCustomPopupForm.WMSpTBXPopupRollUp(var Message: TMessage);
begin
  RollUp(False);
end;

procedure TSpTBXCustomPopupForm.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  // When the form is moved recreate the shadows
  with Message.WindowPos^ do
    if (flags and SWP_SHOWWINDOW = 0) and (flags and SWP_HIDEWINDOW = 0) then
      if IsShadowVisible then
        ResizeShadow;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupForm }

destructor TSpTBXPopupForm.Destroy;
begin
  if HandleAllocated then begin
    TBEndAnimation(WindowHandle);
    { Cleanly destroy any timers before the window handle is destroyed }
    CallNotifyWinEvent(EVENT_SYSTEM_MENUPOPUPEND, WindowHandle, OBJID_CLIENT, CHILDID_SELF);
  end;
  inherited;
end;

procedure TSpTBXPopupForm.CreateShadow;
var
  ParentViewerRect: TRect;
begin
  if (CurrentTheme.GetPopupShadowType = PST_WINDOWS2K) and not
    ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5)) then Exit;

  ParentViewerRect := Rect(0, 0, 0, 0);

  {
  // When the popup type is PST_OFFICEXP, create a shadow
  // for the parent viewer (menu item)
  if CurrentTheme.GetPopupShadowType = PST_OFFICEXP then begin
    PR := BoundsRect;
    if ((VT and PVT_POPUPMENU) <> PVT_POPUPMENU) or ChevronParent then
    begin
      PR := ParentViewer.BoundsRect;
      PR.TopLeft := View.ParentView.Window.ClientToScreen(PR.TopLeft);
      PR.BottomRight := View.ParentView.Window.ClientToScreen(PR.BottomRight);
    end;
  end;
  }

  FShadows := TShadows.Create(ParentViewerRect, BoundsRect, 4, 61, TBXLoColor);
  FShadows.Show(Handle);
end;

procedure TSpTBXPopupForm.DestroyShadow;
var
  SaveShadows: TObject;
begin
  SaveShadows := FShadows;
  FShadows := nil;
  FreeAndNil(SaveShadows);
end;

procedure TSpTBXPopupForm.ResizeShadow;
const
  Size = 4;
begin
  { TODO : subclass TShadows to resize }
  if IsShadowVisible then
    if (TShadowsAccess(FShadows).V1 <> nil) and (TShadowsAccess(FShadows).H1 <> nil) then
      with BoundsRect do begin
        TShadowsAccess(FShadows).V1.BoundsRect := Rect(Right, Top + Size, Right + Size, Bottom);
        TShadowsAccess(FShadows).H1.BoundsRect := Rect(Left + Size, Bottom, Right + Size, Bottom + Size);
        TShadowsAccess(FShadows).V1.Render;
        TShadowsAccess(FShadows).H1.Render;
      end;
end;

function TSpTBXPopupForm.IsShadowVisible: Boolean;
begin
  Result := Assigned(FShadows);
end;

function TSpTBXPopupForm.GetFillColor: TColor;
begin
  Result := CurrentTheme.GetViewColor(PVT_POPUPMENU);
end;

function TSpTBXPopupForm.GetNCSize: TPoint;
begin
  Result.X := 2;
  Result.Y := 2;
  // Don't use the theme borders
  //  CurrentTheme.GetViewBorder(PVT_POPUPMENU, Result);
end;

procedure TSpTBXPopupForm.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
var
  Blend: Boolean;
begin
  { Must override TCustomForm/TForm's CM_SHOWINGCHANGED handler so that the
    form doesn't get activated when Visible is set to True. }

  { Handle animation }
  if Showing and not IsWindowVisible(WindowHandle) and (Animation <> patNone) then
  begin
    Blend := Animation = patFade;
    if Blend or (FAnimationDirection <> []) then begin
      if SendMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_ANIMSTART, 0) = 0 then
      begin
        { Start animation only if WM_TB2K_POPUPSHOWING returns zero (or not handled) }
        TBStartAnimation(WindowHandle, Blend, FAnimationDirection);
        Exit;
      end;
    end;
  end;

  { No animation... }
  if not Showing then begin
    { Call TBEndAnimation to ensure WS_EX_LAYERED style is removed before
      hiding, otherwise windows under the popup window aren't repainted
      properly. }
    TBEndAnimation(WindowHandle);
    DestroyShadow;
  end;
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
  if Showing then SendNotifyMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_NOANIM, 0);
end;

procedure TSpTBXPopupForm.NCPaint(Wnd: HWND; DC: HDC);
var
  R: TRect;
  Canvas: TCanvas;
  PopupInfo: TTBXPopupInfo;
begin
  Assert(DC <> 0, 'TBXPopupNCPaintProc');
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    FillChar(PopupInfo, SizeOf(PopupInfo), 0);
    PopupInfo.WindowHandle := Wnd;
    PopupInfo.ViewType := PVT_POPUPMENU;

    GetWindowRect(Wnd, R);
    OffsetRect(R, -R.Left, -R.Top);
    PopupInfo.BorderSize := GetNCSize;
    CurrentTheme.PaintPopupNCArea(Canvas, R, PopupInfo);
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TSpTBXPopupForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  C: TCanvas;
  R: TRect;
begin
  TBEndAnimation(WindowHandle);
  C := TCanvas.Create;
  try
    C.Handle := Message.DC;
    R := ClientRect;
    CurrentTheme.PaintBackgnd(C, R, R, R, GetFillColor, False, PVT_POPUPMENU);
  finally
    C.Handle := 0;
    C.Free;
  end;

  Message.Result := 1;
end;

procedure TSpTBXPopupForm.WMTB2kAnimationEnded(var Message: TMessage);
begin
  SendNotifyMessage(WindowHandle, WM_TB2K_POPUPSHOWING, TPS_ANIMFINISHED, 0);
end;

procedure TSpTBXPopupForm.WMTB2kPopupShowing(var Message: TMessage);
begin
  inherited;
  if not (csDestroying in ComponentState) and (Message.WParam in [TPS_ANIMFINISHED, TPS_NOANIM]) then
  begin
    if not (csDestroying in ComponentState) then
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);

    if FShowShadows and (CurrentTheme.GetPopupShadowType <> PST_NONE) then
      CreateShadow;
  end;
end;

procedure TSpTBXPopupForm.WMTB2kStepAnimation(var Message: TMessage);
begin
  TBStepAnimation(Message);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFormPopupMenu }

constructor TSpTBXFormPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FForm := TSpTBXPopupForm.Create(nil);
end;

destructor TSpTBXFormPopupMenu.Destroy;
begin
  FreeAndNil(FForm);
  if Assigned(FPopupForm) then
    TCustomFormAccess(FPopupForm).OnClose := FOldPopupFormClose;
  inherited;
end;

function TSpTBXFormPopupMenu.InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
  PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False): Boolean;
begin
  Result := False;
  ClickedItem := nil;

  {$IFDEF JR_D9}
  SetPopupPoint(Point(X, Y));
  {$ELSE}
  PPoint(@PopupPoint)^ := Point(X, Y);
  {$ENDIF}

  if Assigned(FPopupForm) then begin
    FPopupForm.Parent := FForm;
    FPopupForm.Align := alClient;
    FPopupForm.BorderStyle := bsNone;
    FPopupForm.Visible := True;
    FPopupForm.Color := CurrentTheme.GetViewColor(PVT_POPUPMENU);

    if Assigned(PopupControl) then
      FForm.RollDown(PopupControl, FDefaultSize.X, FDefaultSize.Y, False, FPopupFocus)
    else
      FForm.RollDown(X, Y, FDefaultSize.X, FDefaultSize.Y, FPopupFocus);
    Result := True;

    if Assigned(OnPopup) then OnPopup(Self);
  end;
end;

procedure TSpTBXFormPopupMenu.InternalPopupFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // The FPopupForm was closed at runtime because a selection
  // was made, we have to RollUp
  FForm.RollUp(True);
  if Assigned(FOldPopupFormClose) then FOldPopupFormClose(Sender, Action);
end;

procedure TSpTBXFormPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = PopupForm then begin
      // Weird Delphi IDE bug at design time.
      // When a Form is closed at design time it will fire the
      // FreeNotification, setting PopupForm to nil and not
      // saving it to the dfm

      SetPopupForm(nil);

      { TODO : Delphi IDE bug }
      // This line doesn't seem to fix it, it raises AVs in the IDE
      // PopupForm shouldn't be published.
      // if not (csDesigning in ComponentState) then
      //     SetPopupForm(nil);
    end;
end;

procedure TSpTBXFormPopupMenu.Popup(X, Y: Integer);
var
  Dummy: TTBCustomItem;
begin
  InternalPopup(X, Y, Dummy);
end;

function TSpTBXFormPopupMenu.GetBorderStyle: TSpTBXPopupBorderStyleType;
begin
  Result := FForm.BorderStyle;
end;

function TSpTBXFormPopupMenu.GetOnRollUp: TSpTBXRollUpEvent;
begin
  Result := FForm.OnRollUp;
end;

function TSpTBXFormPopupMenu.GetShowShadows: Boolean;
begin
  Result := FForm.ShowShadows;
end;

procedure TSpTBXFormPopupMenu.SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
begin
  FForm.BorderStyle := Value;
end;

procedure TSpTBXFormPopupMenu.SetOnRollUp(const Value: TSpTBXRollUpEvent);
begin
  FForm.OnRollUp := Value;
end;

procedure TSpTBXFormPopupMenu.SetPopupForm(const Value: TCustomForm);
begin
  if FPopupForm <> Value then begin
    if Assigned(FPopupForm) then begin
      FPopupForm.RemoveFreeNotification(Self);
      TCustomFormAccess(FPopupForm).OnClose := FOldPopupFormClose;
    end;

    FPopupForm := Value;
    if Assigned(FPopupForm) then begin
      FPopupForm.FreeNotification(Self);
      FDefaultSize := FPopupForm.ClientRect.BottomRight;

      // Handle the PopupForm's OnClose event
      FOldPopupFormClose := TCustomFormAccess(FPopupForm).OnClose;
      TCustomFormAccess(FPopupForm).OnClose := InternalPopupFormClose;
    end;
  end;
end;

procedure TSpTBXFormPopupMenu.SetShowShadows(const Value: Boolean);
begin
  FForm.ShowShadows := Value;
end;

end.
