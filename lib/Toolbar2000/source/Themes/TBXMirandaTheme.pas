unit TBXMirandaTheme;

// TBX Package
// Copyright 2001-2002 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// 'Zezio' TBX theme © 2004 Roy Magne Klever
// roymagne@rmklever.com
//
// Version for TBX version 2.1
// Last updated: 02.12.2004

interface

uses
  Windows, Messages, Graphics, TBXThemes, ImgList;

{$DEFINE ALTERNATIVE_DISABLED_STYLE}

type
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  TWinFrameState = (wfsActive, wfsInactive);

  TTBXMirandaTheme = class(TTBXTheme)
  private
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    { View/Window Colors }
    MenubarColor: TColor;
    ToolbarColor: TColor;
    PopupColor: TColor;
    DockPanelColor: TColor;
    BarSepColor: TColor;
    EditFrameColor: TColor;
    EditFrameDisColor: TColor;

    PopupFrameColor: TColor;
    WinFrameColors: array[TWinFrameState, TWinFramePart] of TColor;
    PnlFrameColors: array[TWinFrameState, TWinFramePart] of TColor;
    MenuItemColors: array[TMenuItemState, TItemPart] of TColor;
    BtnItemColors: array[TBtnItemState, TItemPart] of TColor;

    { Other Colors }
    DragHandleColor: TColor;
    PopupSeparatorColor: TColor;
    ToolbarSeparatorColor: TColor;
    IconShadowColor: TColor;
    StatusPanelFrameColor: TColor;
    DisabledColor: TColor;

    procedure SetupColorCache; virtual;
  protected
    { Internal Methods }
    function GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart):
      TColor;
    function GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart):
      TColor;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics access}
    function GetBooleanMetrics(Index: Integer): Boolean; override;
    function GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo;
      ImageList: TCustomImageList): TPoint; override;
    function GetIntegerMetrics(Index: Integer): Integer; override;
    function GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
      override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function GetViewColor(AViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins);
      override;

    { Painting routines }
    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect:
      TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer);
      override;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo:
      TTBXItemInfo); override;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo:
      TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated:
      Boolean);
      override;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo:
      TTBXItemInfo); override;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo:
      TTBXItemInfo); override;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect;
      DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const
      DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const
      ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo:
      TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(Canvas: TCanvas; const ARect: TRect; var ItemInfo:
      TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const
      WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo:
      TTBXItemInfo); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo:
      TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(Canvas: TCanvas; ARect: TRect; const ItemInfo:
      TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo:
      TTBXItemInfo); override;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const
      ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(Canvas: TCanvas; const ARect: TRect;
      ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo:
      TTBXPopupInfo); override;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo:
      TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo:
      TTBXToolbarInfo); override;
    procedure PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer;
      Params: Pointer); override;
    procedure PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);
      override;
  end;

var
  MenuButtons, BarLines, AltCaption,
    Aqua, DarkAqua, CaptionOutline, DottedGrip: boolean;
  SelGradient: integer;
  HotColor, BaseColor, BaseShade: TColor;

{$IFDEF DTM_Package}
function TBXThemeName: shortstring; stdcall;
procedure TBXRegisterTheme(RegisterTheme: boolean); stdcall;
{$ENDIF}

implementation

{.$R tbx_glyphs.res}

uses
  TBXUtils, TB2Common, TB2Item, Classes, Controls, Commctrl, Forms, rmkThemes,
  TBXUxThemes, TBX;

{$IFDEF DTM_Package}
exports
  TBXThemeName,
  TBXRegisterTheme;

const
  cThemeName = 'Zezio';

function TBXThemeName: shortstring; stdcall;
begin
  result := cThemeName;
end;

procedure TBXRegisterTheme(RegisterTheme: boolean); stdcall;
begin
  if RegisterTheme then
    RegisterTBXTheme(cThemeName, TTBXMirandaTheme)
  else
    UnregisterTBXTheme(cThemeName);
end;
{$ENDIF}

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  StockImgList: TImageList;
  CounterLock: Integer;

procedure InitializeStock;
begin
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0,
    clWhite);
end;

procedure FinalizeStock;
begin
  StockImgList.Free;
end;

{ TTBXMirandaTheme }

procedure RoundFrame(Canvas: TCanvas; R: TRect; RL, RR: Integer);
begin
  with Canvas, R do
  begin
    Dec(Right);
    Dec(Bottom);
    PolyLine([
      Point(Left + RL, Top),
        Point(Right - RR, Top),
        Point(Right, Top + RR),
        Point(Right, Bottom - RR),
        Point(Right - RR, Bottom),
        Point(Left + RL, Bottom),
        Point(Left, Bottom - RL),
        Point(Left, Top + RL),
        Point(Left + RL, Top)
        ]);
    Inc(Right);
    Inc(Bottom);
  end;
end;

function TTBXMirandaTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT: Result := False;
    TMB_EDITHEIGHTEVEN:         Result := False;
    TMB_PAINTDOCKBACKGROUND:    Result := True;
    TMB_SOLIDTOOLBARNCAREA:     Result := True;
    TMB_SOLIDTOOLBARCLIENTAREA: Result := True;
  else
    Result := False;
  end;
end;

function TTBXMirandaTheme.GetIntegerMetrics(Index: Integer): Integer;
const
  DEFAULT = -1;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:         Result := 12;

    TMI_DROPDOWN_ARROWWIDTH:         Result := 8;
    TMI_DROPDOWN_ARROWMARGIN:        Result := 3;

    TMI_MENU_IMGTEXTSPACE:           Result := 3;
    TMI_MENU_LCAPTIONMARGIN:         Result := 3;
    TMI_MENU_RCAPTIONMARGIN:         Result := 3;
    TMI_MENU_SEPARATORSIZE:          Result := 5;

    TMI_MENU_MDI_DW:                 Result := 2;
    TMI_MENU_MDI_DH:                 Result := 2;

    TMI_TLBR_SEPARATORSIZE:          Result := 5;

    TMI_EDIT_MENURIGHTINDENT:        Result := 1;
    TMI_EDIT_FRAMEWIDTH:             Result := 1;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 2;
    TMI_EDIT_BTNWIDTH:               Result := 14;
  else
    Result := -1;
  end;
end;

function TTBXMirandaTheme.GetViewColor(AViewType: Integer): TColor;
begin
  Result := clBtnFace;
  if (AViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (AViewType and TVT_MENUBAR) = TVT_MENUBAR then
      Result := MenubarColor
    else
      Result := ToolbarColor;
  end
  else if (AViewType and VT_POPUP) = VT_POPUP then
  begin
    if (AViewType and PVT_LISTBOX) = PVT_LISTBOX then
      Result := clWindow
    else
      Result := PopupColor;
  end
  else if (AViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
    Result := DockPanelColor;
end;

function TTBXMirandaTheme.GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart:
  TItemPart): TColor;
const
  BFlags1: array[Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array[Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array[Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  B: TBtnItemState;
  Embedded: Boolean;
begin
  with ItemInfo do
  begin
    Embedded := (ViewType and VT_TOOLBAR = VT_TOOLBAR) and
      (ViewType and TVT_EMBEDDED = TVT_EMBEDDED);
    if not Enabled then
      B := BFlags1[HoverKind = hkKeyboardHover]
    else if ItemInfo.IsPopupParent then
      B := bisPopupParent
    else if Pushed then
      B := bisPressed
    else if Selected then
      B := BFlags2[HoverKind <> hkNone]
    else
      B := BFlags3[HoverKind <> hkNone];
    Result := BtnItemColors[B, ItemPart];
    if Embedded then
    begin
      if (ItemPart = ipBody) and (Result = clNone) then
        Result := ToolbarColor;
      if ItemPart = ipFrame then
      begin
        if Selected then
          Result := clWindowFrame
        else if (Result = clNone) then
          Result := clBtnShadow;
      end;
    end;
  end;
end;

function TTBXMirandaTheme.GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart:
  TItemPart): TColor;
const
  MFlags1: array[Boolean] of TMenuItemState = (misDisabled, misDisabledHot);
  MFlags2: array[Boolean] of TMenuItemState = (misNormal, misHot);
  BFlags1: array[Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array[Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array[Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  IsMenuItem, Embedded: Boolean;
  M: TMenuItemState;
  B: TBtnItemState;
begin
  with ItemInfo do
  begin
    IsMenuItem := ((ViewType and PVT_POPUPMENU) = PVT_POPUPMENU) and
      ((ItemOptions and IO_TOOLBARSTYLE) = 0) and (ItemInfo.ComboPart = cpNone);
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
    if IsMenuItem then
    begin
      if not Enabled then
        M := MFlags1[HoverKind = hkKeyboardHover]
      else
        M := MFlags2[HoverKind <> hkNone];
      Result := MenuItemColors[M, ItemPart];
    end
    else
    begin
      if not Enabled then
        B := BFlags1[HoverKind = hkKeyboardHover]
      else if ItemInfo.IsPopupParent then
        B := bisPopupParent
      else if Pushed then
        B := bisPressed
      else if Selected then
        B := BFlags2[HoverKind <> hkNone]
      else
        B := BFlags3[HoverKind <> hkNone];
      Result := BtnItemColors[B, ItemPart];
      if Embedded and (Result = clNone) then
      begin
        if ItemPart = ipBody then
          Result := ToolbarColor;
        if ItemPart = ipFrame then
          Result := clBtnShadow;
      end;
    end;
  end;
end;

procedure DrawButtonBitmap(Canvas: TCanvas; R: TRect; Color: TColor);
const
{$IFNDEF SMALL_CLOSE_BUTTON}
  Pattern: array[0..15] of Byte =
  ($C3, 0, $66, 0, $3C, 0, $18, 0, $3C, 0, $66, 0, $C3, 0, 0, 0);
{$ELSE}
  Pattern: array[0..15] of Byte =
  (0, 0, $63, 0, $36, 0, $1C, 0, $1C, 0, $36, 0, $63, 0, 0, 0);
{$ENDIF}
var
  Bmp: TBitmap;
  W, H: Integer;
  Index: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := CreateBitmap(8, 8, 1, 1, @Pattern);
    Index := SaveDC(Canvas.Handle);
    Canvas.Brush.Color := Color;
    SetTextColor(Canvas.Handle, clBlack);
    SetBkColor(Canvas.Handle, clWhite);
    W := 8;
    H := 7;
    with R do
    begin
      BitBlt(Canvas.Handle, (Left + Right - W + 1) div 2, (Top + Bottom - H) div
        2, W, H,
        Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    RestoreDC(Canvas.Handle, Index);
  finally
    Bmp.Free;
  end;
end;

function TTBXMirandaTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then
    Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXMirandaTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
var
  InMenuBar, ToolbarStyle, ShowInactive: Boolean;
begin
  with ItemInfo do
  begin
    InMenuBar := (ItemInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    ShowInactive := InMenubar and not Boolean(ItemOptions and IO_APPACTIVE);

    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then
      Result := clBtnShadow
    else
    if not Enabled then
      Result := DisabledColor
    else
    if not ToolbarStyle or InMenuBar then
    begin
      if HoverKind <> hkNone then
        Result := clHighlightText
      else
      if ShowInactive then
        Result := clGrayText
      else
        Result := clMenuText
    end
    else
      Result := clBtnText;
  end;
end;

function TTBXMirandaTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo):
  TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then
    result := GetViewColor(ItemInfo.ViewType);
end;

procedure TTBXMirandaTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array[Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array[Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;

  procedure SetBorder(X, Y: Integer);
  begin
    Border.X := X;
    Border.Y := Y;
  end;

begin
  if ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) or ((ViewType and TVT_MENUBAR) =
    TVT_MENUBAR) then
  begin
    if (ViewType and TVT_FLOATING) = TVT_FLOATING then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) = TVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else
      SetBorder(2, 2);
  end
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
    if (ViewType and PVT_POPUPMENU) = PVT_POPUPMENU then
    begin
      Border.X := 2;
      Border.Y := 2;
    end
    else
    begin
      Border.X := 2;
      Border.Y := 2;
    end;
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if (ViewType and DPVT_FLOATING) = DPVT_FLOATING then
    begin
      Resizable := (ViewType and DPVT_RESIZABLE) = DPVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else
      SetBorder(2, 2);
  end
  else
    SetBorder(0, 0);
end;

procedure TTBXMirandaTheme.GetMargins(MarginID: Integer; out Margins:
  TTBXMargins);
begin
  with Margins do
    case MarginID of
      MID_TOOLBARITEM:
        begin
          LeftWidth := 2;
          RightWidth := 2;
          TopHeight := 2;
          BottomHeight := 2;
        end;
      MID_STATUSPANE:
        begin
          LeftWidth := 3;
          RightWidth := 3;
          TopHeight := 1;
          BottomHeight := 1;
        end;
      MID_MENUITEM:
        begin
          LeftWidth := 0;
          RightWidth := 0;
          TopHeight := 3;
          BottomHeight := 3;
        end;
    else
      LeftWidth := 0;
      RightWidth := 0;
      TopHeight := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXMirandaTheme.PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect,
  AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer);
var
  R: TRect;
  Brush: HBrush;
begin
   if not Transparent then
  begin
    Brush:= CreateSolidBrush(ColorToRGB(AColor));
    IntersectRect(R, ARect, AClipRect);
    FillRect(Canvas.Handle, R, Brush);
    DeleteObject(Brush);
  end;
end;

procedure TTBXMirandaTheme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  R: TRect;
  C: TColor;
  InMenuBar, ToolbarStyle: Boolean;

  procedure _Draw(Color: TColor);
  begin
    Canvas.Font.Color := Color;
    if not Rotated then Windows.DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), R, AFormat)
    else DrawRotatedText(Canvas.Handle, ACaption, R, AFormat);
  end;

begin
  with ItemInfo, Canvas do
  begin
    R := ARect;
    Brush.Style := bsClear;
    InMenuBar := (ViewType and TVT_MENUBAR) = TVT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);

    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then _Draw(C)
    else
    begin
      if Enabled then
        if ToolbarStyle and (Pushed or Selected) and not (InMenuBar and USE_FLATMENUS) then
        OffsetRect(R, 1, 1);
      C := Font.Color;
      if C = clNone then C := GetItemTextColor(ItemInfo);
      _Draw(C);
      Brush.Style := bsSolid;
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintCheckMark(Canvas: TCanvas; ARect: TRect; const
  ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
begin
  X := (ARect.Left + ARect.Right) div 2 - 2;
  Y := (ARect.Top + ARect.Bottom) div 2 + 1;
  if ItemInfo.Enabled and (Iteminfo.HoverKind <> hkNone) then
    Canvas.Pen.Color := clBlack
  else if not ItemInfo.Enabled then
    Canvas.Pen.Color := MenuItemColors[misDisabled, ipText]
  else
    Canvas.Pen.Color := clBlack;
  Canvas.Polyline([Point(X - 2, Y - 2), Point(X, Y), Point(X + 4, Y - 4),
    Point(X + 4, Y - 3), Point(X, Y + 1), Point(X - 2, Y - 1), Point(X - 2, Y -
      2)]);
end;

procedure TTBXMirandaTheme.PaintChevron(Canvas: TCanvas; ARect: TRect; const
  ItemInfo: TTBXItemInfo);
const
  Pattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  R2: TRect;
  Bmp: TBitmap;
begin
  R2 := ARect;
  PaintButton(Canvas, ARect, ItemInfo);
  if not ItemInfo.IsVertical then
  begin
    R2.Top := 4;
    R2.Bottom := R2.Top + 5;
    Inc(R2.Left, 2);
    R2.Right := R2.Left + 8;
  end
  else
  begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    Inc(R2.Top, 2);
    R2.Bottom := R2.Top + 8;
  end;
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := CreateBitmap(8, 8, 1, 1, @Pattern[ItemInfo.IsVertical]);
    Canvas.Brush.Color := GetPartColor(ItemInfo, ipText);
    SetTextColor(Canvas.Handle, clBlack);
    SetBkColor(Canvas.Handle, clWhite);
    with R2 do
      BitBlt(Canvas.Handle, Left, Top, Right - Left,
        Bottom - Top, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
  end;
end;

procedure TTBXMirandaTheme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
const
  ArrowColor: array[Boolean] of TColor = (clBtnText, clMenuText);
var
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  SaveItemInfoPushed: Boolean;

  procedure PaintEnabled(R: TRect; Pressed: Boolean);
  var
    C: TColor;
  begin
    if BtnDisabled then
      C := EditFrameDisColor
    else
      C := EditFrameColor;
    if Embedded then
    begin
      FillRectEx(Canvas.Handle, R, ToolBarColor);
      FrameRectEx(Canvas.Handle, R, C, True);
    end
    else
      FrameRectEx(Canvas.Handle, R, C, True);
  end;

begin
  with Canvas, ItemInfo do
  begin
    R := ARect;
    Inc(R.Left);
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);

    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;

      if not BtnDisabled then
      begin
        if BtnPressed then
        begin
          GradientFill(Canvas, R, Blend(clBtnHigHlight, clBtnFace, 60),
            Blend(clBtnFace, clBtnShadow, 60), TGTopBottom);
          InflateRect(R, 1, 1);
          Pen.Color := Blend(clBtnFace, clBtnShadow, 45);
          MoveTo(R.Left + 1, R.Top + 1);
          LineTo(R.Right - 1, R.Top + 1);
          Pen.Color := clBtnShadow;
        end
        else if BtnHot then
        begin
          GradientFill(Canvas, R, clBtnFace, clBtnHighLight, TGTopBottom);
          InflateRect(R, 1, 1);
          Pen.Color := EditFrameColor;
        end
        else
        begin
          PaintEnabled(R, BtnPressed);
        end;
      end;
      if BtnDisabled then
        FrameRectEx(Canvas.Handle, R, BtnItemColors[bisDisabled, ipFrame],
          True);
      if (not BtnPressed) and (not BtnHot) then
        Pen.Color := EditFrameColor;
      if not BtnDisabled then
      begin
        MoveTo(R.Left, R.Top);
        LineTo(R.Left, R.Bottom);
      end;
      PaintDropDownArrow(Canvas, R, ItemInfo);
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;

      // Upper button
      Pen.Color := clBtnShadow;
      BR := R;
      BR.Bottom := (R.Top + R.Bottom + 1) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
      SaveItemInfoPushed := ItemInfo.Pushed;
      ItemInfo.Pushed := BtnPressed;

      if not BtnDisabled then
      begin
        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_UP) <> 0);
        if BtnPressed or BtnHot or Embedded then
        begin
          // BR = Button Rectangle
          BR.Bottom := BR.Bottom - 1;
          if BtnPressed then
          begin
            GradientFill(Canvas, BR, Blend(clBtnHigHlight, clBtnFace, 60),
              Blend(clBtnFace, clBtnShadow, 60), TGTopBottom);
            InflateRect(BR, 1, 1);
            Pen.Color := Blend(clBtnFace, clBtnShadow, 45);
            MoveTo(BR.Left + 1, BR.Top + 1);
            LineTo(BR.Right - 1, BR.Top + 1);
            Pen.Color := clBtnShadow;
          end
          else if BtnHot then
          begin
            GradientFill(Canvas, BR, clBtnFace, clBtnHighLight, TGTopBottom);
            InflateRect(BR, 1, 1);
            Pen.Color := EditFrameColor;
          end;
          BR.Bottom := BR.Bottom + 1;
        end;
        if (not BtnPressed) and (not BtnHot) then
          Pen.Color := EditFrameColor;
        MoveTo(BR.Left, BR.Top);
        LineTo(BR.Left, BR.Bottom);
      end;

      X := (BR.Left + BR.Right) div 2     + Ord(Pushed or Selected);
      Y := (BR.Top + BR.Bottom - 1) div 2 + Ord(Pushed or Selected);
      if not BtnDisabled then
        Pen.Color := ArrowColor[not Boolean(ItemOptions and IO_TOOLBARSTYLE)]
      else
        Pen.Color := DisabledColor;
      Brush.Color := Pen.Color;
      Polygon([Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)]);

      // Lower button...
      Pen.Color := BaseShade;
      BR := R;
      BR.Top := (R.Top + R.Bottom) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
      if btnHot or btnPressed then
        BR.Top := Br.Top + 1;

      ItemInfo.Pushed := BtnPressed;
      if not BtnDisabled then
      begin
        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_UP) <> 0);
        if BtnPressed or BtnHot or Embedded then
        begin
          if BtnPressed then
          begin
            GradientFill(Canvas, BR, Blend(clBtnHigHlight, clBtnFace, 60),
              Blend(clBtnFace, clBtnShadow, 60), TGTopBottom);
            InflateRect(BR, 1, 1);
            Pen.Color := Blend(clBtnFace, clBtnShadow, 45);
            MoveTo(BR.Left + 1, BR.Top + 1);
            LineTo(BR.Right - 1, BR.Top + 1);
            Pen.Color := clBtnShadow;
          end
          else if BtnHot then
          begin
            GradientFill(Canvas, BR, clBtnFace, clBtnHighLight, TGTopBottom);
            InflateRect(BR, 1, 1);
            Pen.Color := EditFrameColor;
          end
        end;
        if (not BtnPressed) and (not BtnHot) then
          Pen.Color := EditFrameColor;
        MoveTo(BR.Left, BR.Bottom - 1);
        LineTo(BR.Left, BR.Top);
        LineTo(BR.Right, BR.Top);
      end;

      X := (BR.Left + BR.Right) div 2 + Ord(Pushed or Selected);
      Y := (BR.Top + BR.Bottom) div 2 + Ord(Pushed or Selected);
      if not BtnDisabled then
        Pen.Color := ArrowColor[not Boolean(ItemOptions and IO_TOOLBARSTYLE)]
      else
        Pen.Color := DisabledColor;
      Brush.Color := Pen.Color;
      Polygon([Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)]);
      ItemInfo.Pushed := SaveItemInfoPushed;
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintEditFrame(Canvas: TCanvas;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
begin
  R := ARect;
  PaintFrame(Canvas, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);

  with EditInfo do
    if RightBtnWidth > 0 then
      Dec(R.Right, RightBtnWidth - 2);

  if ItemInfo.Enabled then
  begin
    if ItemInfo.HoverKind = hkNone then
      Canvas.Brush.Color := EditFrameColor // Should be central...
    else
      Canvas.Brush.Color := EditFrameDisColor;
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWindow;
  end;

  if not ItemInfo.Enabled then
  begin
    if ItemInfo.HoverKind = hkNone then
      Canvas.Brush.Color := BtnItemColors[bisDisabled, ipFrame]
    else
      Canvas.Brush.Color := clWindow;
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWindow;
  end;

  InflateRect(R, -1, -1);
  if ItemInfo.Enabled then
    Canvas.FillRect(R);

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXMirandaTheme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ArrowColor: array[Boolean] of TColor = (clBtnText, clMenuText);
var
  X, Y: Integer;
  C: TColor;
begin
  if ItemInfo.Enabled then
    C := ArrowColor[not Boolean(ItemInfo.ItemOptions and IO_TOOLBARSTYLE)]
  else
    C := DisabledColor;
  with ItemInfo, ARect, Canvas do
    if (ItemInfo.ComboPart = cpNone) or (ItemInfo.ComboPart = cpCombo) then
    begin
      if ItemInfo.IsVertical then
      begin
        X := (Left + Right) div 2 - 1 + Ord(Pushed or Selected);
        Y := (Top + Bottom) div 2     + Ord(Pushed or Selected);
      end
      else
      begin
        X := (Left + Right) div 2     + Ord(Pushed or Selected);
        Y := (Top + Bottom) div 2 + 1 + Ord(Pushed or Selected);
      end;
      Pen.Color := C;
      Brush.Color := Pen.Color;
      if ItemInfo.IsVertical then
        Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
      else
        Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);

      if ItemInfo.IsVertical then
      begin
        X := (Left + Right) div 2 + 2 + Ord(Pushed or Selected);
        Y := (Top + Bottom) div 2     + Ord(Pushed or Selected);
      end
      else
      begin
        X := (Left + Right) div 2     + Ord(Pushed or Selected);
        Y := (Top + Bottom) div 2 - 2 + Ord(Pushed or Selected);
      end;
      Pen.Color := C;
      Brush.Color := Pen.Color;
      if ItemInfo.IsVertical then
        Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X + 2, Y)])
      else
        Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y - 2)]);
    end
    else
    begin
      X := (Left + Right) div 2     + Ord(Pushed or Selected);
      Y := (Top + Bottom) div 2 - 1 + Ord(Pushed or Selected);
      Pen.Color := C;
      Brush.Color := Pen.Color;
      if ItemInfo.IsVertical then
        Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
      else
        Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
    end;
end;

procedure TTBXMirandaTheme.PaintButton(Canvas: TCanvas; const ARect: TRect; const
  ItemInfo: TTBXItemInfo);
var
  R: TRect;
  RL, RR: Integer;
  ShowHover, Embedded: Boolean;
begin
  R := ARect;
  with ItemInfo, Canvas do
  begin
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    Embedded := (ViewType and VT_TOOLBAR = VT_TOOLBAR) and
      (ViewType and TVT_EMBEDDED = TVT_EMBEDDED);
    RL := 2;
    RR := 2;

    if ComboPart = cpSplitRight then
    begin
      Dec(R.Left);
      Dec(RL);
    end;
    if ComboPart = cpSplitLeft then
      Dec(RR);
    if (ItemInfo.ItemOptions and IO_TOOLBARSTYLE) = 0 then
    begin
      RR := 1;
      RL := 1;
    end;

    if Embedded and not ShowHover then
    begin
      if Enabled then
      begin
        InflateRect(R, -1, -1);
        FillRectEx(Canvas.Handle, R, ToolBarColor);
        InflateRect(R, 1, 1);
        Pen.Color := clBtnShadow;
      end
      else
        Pen.Color := clBtnFace;
      SmartFrame(Canvas, R, RL, RR, Pen.Color, clNone);
    end;

    if ((ViewType and TVT_MENUBAR) = TVT_MENUBAR) then
    begin
      if ((Pushed or Selected) and Enabled) or ShowHover then
        PaintBackgnd(Canvas, ZERO_RECT, ARect, ARect, MenuItemColors[misHot,
          ipBody], False, VT_UNKNOWN);
      Exit;
    end;

    if (Pushed or Selected) and Enabled then
    begin
      InflateRect(R, -1, -1);
      if not Pushed then
      begin
        if HoverKind = hkNone then
        begin
          GradientFill(Canvas, R, clBtnHighLight, clBtnFace, TGTopBottom);
          InflateRect(R, 1, 1);
          SmartFrame(Canvas, R, RL, RR, clBtnShadow, Blend(clBtnShadow,
            clBtnFace, 50));
          Pen.Color := Blend(clBtnShadow, clBtnFace, 25);
          MoveTo(R.Left + RL, R.Top + 1);
          LineTo(R.Right - RR, R.Top + 1);
        end
        else
        begin
          GradientFill(Canvas, R, clBtnFace, clBtnHighLight, TGTopBottom);
          InflateRect(R, 1, 1);
          SmartFrame(Canvas, R, RL, RR, GetBtnColor(ItemInfo, ipFrame), clNone);
        end;
      end
      else
      begin
        GradientFill(Canvas, R, Blend(clBtnHigHlight, clBtnFace, 60),
          Blend(clBtnFace, clBtnShadow, 60), TGTopBottom);
        InflateRect(R, 1, 1);
        SmartFrame(Canvas, R, RL, RR, clBtnShadow, Blend(clBtnShadow, clBtnFace,
          50));
        Pen.Color := Blend(clBtnFace, clBtnShadow, 45);
        MoveTo(R.Left + RL, R.Top + 1);
        LineTo(R.Right - RR, R.Top + 1);
      end;
    end
    else if ShowHover or ((ItemOptions and IO_DESIGNING) <> 0) then
    begin
      InflateRect(R, -1, -1);
      GradientFill(Canvas, R, clBtnFace, clBtnHighLight, TGTopBottom);
      InflateRect(R, 1, 1);
      SmartFrame(Canvas, R, RL, RR, GetBtnColor(ItemInfo, ipFrame), clNone);
    end;

    if ComboPart = cpSplitRight then
      PaintDropDownArrow(Canvas, R, ItemInfo);
  end;
end;

procedure TTBXMirandaTheme.PaintFloatingBorder(Canvas: TCanvas; const ARect:
  TRect;
  const WindowInfo: TTBXWindowInfo);
const
  SPI_GETGRADIENTCAPTIONS = $1008;
  DC_GRADIENT = $20;
  ActiveCaptionFlags: array[Boolean] of Integer = (0, DC_ACTIVE);
  GradientCaptionFlags: array[Boolean] of Integer = (0, DC_GRADIENT);
  CaptionBkColors: array[Boolean, Boolean] of Integer =
  ((COLOR_INACTIVECAPTION, COLOR_ACTIVECAPTION),
    (COLOR_GRADIENTINACTIVECAPTION, COLOR_GRADIENTACTIVECAPTION));
  ButtonStateFlags: array[Boolean] of Integer = (0, DFCS_PUSHED);
var
  R, R2: TRect;
  DC: HDC;
  Gradient, ShowCloseBtn: Boolean;
  B: BOOL;
begin
  DC := Canvas.Handle;

  with WindowInfo do
  begin
    R := ARect;
    if (WRP_BORDER and RedrawPart) <> 0 then
    begin
      R2 := R;
      with FloatingBorderSize do
        InflateRect(R2, -X, -Y);
      SaveDC(DC);
      with R2 do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      Windows.DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE);
      RestoreDC(DC, -1);
    end;

    if not WindowInfo.ShowCaption then
      Exit;
    Gradient := SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @B, 0) and B;
    ShowCloseBtn := (CDBS_VISIBLE and CloseButtonState) <> 0;
    R := GetTBXCloseButtonRect(WindowInfo, True);

    if (WRP_CAPTION and RedrawPart) <> 0 then
    begin
      if ShowCloseBtn then
      begin
        SaveDC(DC);
        with R do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
      R2 := GetTBXCaptionRect(WindowInfo, True, ShowCloseBtn);
      DrawCaption(ParentHandle, DC, R2, DC_TEXT or DC_SMALLCAP or
        ActiveCaptionFlags[Active] or GradientCaptionFlags[Gradient]);
      if ShowCloseBtn then
        RestoreDC(DC, -1);
      R2 := GetTBXCaptionRect(WindowInfo, True, False);
      R2.Top := R2.Bottom;
      Inc(R2.Bottom);
      FillRect(DC, R2, GetSysColorBrush(COLOR_BTNFACE));
    end;

    if ShowCloseBtn then
    begin
      R2 := R;
      InflateRect(R2, -2, -2);
      if (WRP_CAPTION and RedrawPart) <> 0 then
      begin
        SaveDC(DC);
        with R2 do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
        FillRect(DC, R, GetSysColorBrush(CaptionBkColors[Gradient,
          WindowInfo.Active]));
        RestoreDC(DC, -1);
      end;
      if (WRP_CLOSEBTN and RedrawPart) <> 0 then
        DrawFrameControl(DC, R2, DFC_CAPTION, DFCS_CAPTIONCLOSE or
          ButtonStateFlags[(CDBS_PRESSED and CloseButtonState) <> 0]);
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintFrame(Canvas: TCanvas; const ARect: TRect; const
  ItemInfo: TTBXItemInfo);
var
  R: TRect;
  E: Boolean;
  C: TColor;
begin
  R := ARect;
  with ItemInfo do
  begin
    E := Enabled or (not Enabled and (HoverKind = hkKeyboardHover));
    if not E then
    begin
      InflateRect(R, -1, -1);
      FrameRectEx(Canvas.Handle, R, ToolBarColor, True);
    end
    else if Pushed or Selected or (HoverKind <> hkNone) or ((ItemOptions and
      IO_DESIGNING) <> 0) then
    begin
      if Pushed then
        C := BtnItemColors[bisSelected, ipFrame]
      else
        C := GetPartColor(ItemInfo, ipFrame);
      SmartFrame(Canvas, R, 1, 1, c, clNone);
    end;
  end;
end;

function TTBXMirandaTheme.GetImageOffset(Canvas: TCanvas;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint;
begin
  Result.X := 0;
  if not (ImageList is TTBCustomImageList) then
    with ItemInfo do
      if Enabled and (HoverKind <> hkNone) and
        not (Selected or Pushed and not IsPopupParent) then
        Result.X := -1;
  Result.Y := Result.X
end;

procedure TTBXMirandaTheme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex:
  Integer);
var
  HiContrast: Boolean;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      TTBCustomImageList(ImageList).DrawState(Canvas, ARect.Left, ARect.Top,
        ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
      Exit;
    end;

    HiContrast := ColorIntensity(GetItemImageBackground(ItemInfo)) < 80;
    if not Enabled then
    begin
      if not HiContrast then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else
        DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex,
          clBtnShadow);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if Selected or Pushed then
      begin
        {
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, -2, -2);
        }
      end
      else
      begin
        {
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, -2, -2);
        }
      end;
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, False);
    end
    else if HiContrast or TBXHiContrast or TBXLoColor then
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
    else
      HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 255);
  end;
end;

procedure TTBXMirandaTheme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
  X, Y: Integer;
  Bmp: TBitmap;
begin
  PaintButton(Canvas, ARect, ItemInfo);
  with ARect do
  begin
    X := (Left + Right - StockImgList.Width) div 2;
    Y := (Top + Bottom - StockImgList.Height) div 2;
  end;
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  Bmp := TBitmap.Create;
  Bmp.Monochrome := True;
  StockImgList.GetBitmap(Index, Bmp);
  Canvas.Brush.Color := GetPartColor(ItemInfo, ipText);
  SetTextColor(Canvas.Handle, clBlack);
  SetBkColor(Canvas.Handle, clWhite);
  BitBlt(Canvas.Handle, X, Y, StockImgList.Width, StockImgList.Height,
    Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  Bmp.Free;
end;

procedure TTBXMirandaTheme.PaintMenuItemFrame(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
begin
  with ItemInfo do
    if (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover)) then
    begin
      PaintBackgnd(Canvas, ZERO_RECT, ARect, ARect, MenuItemColors[misHot,
        ipBody], False, VT_UNKNOWN);
    end;
end;

procedure TTBXMirandaTheme.PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var
  ItemInfo: TTBXItemInfo);
var
  DC: HDC;
  R: TRect;
  ShowImageOrCheck: Boolean;
  IsCombo: Boolean;
  X, Y: Integer;
  ArrowWidth: Integer;

  procedure DrawArrow(AColor: TColor);
  begin
    Canvas.Pen.Color := AColor;
    Canvas.Brush.Color := AColor;
    Canvas.Polygon([Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)]);
  end;

begin
  DC := Canvas.Handle;
  with ItemInfo do
  begin
    ShowImageOrCheck := (ImageWidth > 0) or Selected;
    IsCombo := ((ItemOptions and IO_COMBO) <> 0);
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);

    R := ARect;
    if ShowImageOrCheck then Inc(R.Left, ItemInfo.PopupMargin + MenuImageTextSpace);
    if IsCombo and Enabled then Dec(R.Right, ArrowWidth + 1);

    PaintMenuItemFrame(Canvas, R, ItemInfo);

    if IsCombo then
    begin
      R.Left := ARect.Right - ArrowWidth;
      R.Right := ARect.Right;
      if Enabled and (HoverKind <> hkNone) then
        PaintBackgnd(Canvas, ZERO_RECT, R, R, clHighlight, False, PVT_POPUPMENU)
      else
      begin
        Dec(R.Left);
        DrawLineEx(DC, R.Left, R.Top, R.Left, R.Bottom, DisabledColor);
      end;
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3;
      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then DrawArrow(clBtnShadow)
        else DrawArrow(DisabledColor);
      end
      else if (HoverKind <> hkNone) then DrawArrow(clHighlightText)
      else DrawArrow(clMenuText);
    end;

    if ShowImageOrCheck and ((HoverKind <> hkNone) or Selected) then
    begin
      R.Left := ARect.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      PaintButton(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintPopupNCArea(Canvas: TCanvas; R: TRect; const
  PopupInfo: TTBXPopupInfo);
var
  PR: TRect;
begin
  with Canvas do
  begin
    Brush.Color := PopupFrameColor;
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := PopupColor;
    FillRect(R);

    if not IsRectEmpty(PopupInfo.ParentRect) then
    begin
      PR := PopupInfo.ParentRect;
      if not IsRectEmpty(PR) then
        with PR do
        begin
          Pen.Color := ToolbarColor;
          if Bottom = R.Top then
          begin
            if Left <= R.Left then
              Left := R.Left - 1;
            if Right >= R.Right then
              Right := R.Right + 1;
            MoveTo(Left + 1, Bottom - 1);
            LineTo(Right - 1, Bottom - 1);
          end
          else if Top = R.Bottom then
          begin
            if Left <= R.Left then
              Left := R.Left - 1;
            if Right >= R.Right then
              Right := R.Right + 1;
            MoveTo(Left + 1, Top);
            LineTo(Right - 1, Top);
          end;
          if Right = R.Left then
          begin
            if Top <= R.Top then
              Top := R.Top - 1;
            if Bottom >= R.Bottom then
              Bottom := R.Bottom + 1;
            MoveTo(Right - 1, Top + 1);
            LineTo(Right - 1, Bottom - 1);
          end
          else if Left = R.Right then
          begin
            if Top <= R.Top then
              Top := R.Top - 1;
            if Bottom >= R.Bottom then
              Bottom := R.Bottom + 1;
            MoveTo(Left, Top + 1);
            LineTo(Left, Bottom - 1);
          end;
        end;
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
var
  IsToolbox         : Boolean;
  R                 : TRect;
  i                 : integer;
begin
  with ItemInfo, ARect, Canvas do
  begin
    if Not Enabled then exit;

    if Horizontal then
    begin
      IsToolbox := (ViewType and PVT_TOOLBOX) = PVT_TOOLBOX;
      if ((ItemOptions and IO_TOOLBARSTYLE) = 0) and not IsToolBox then
      begin
        R := ARect;
        R.Right := ItemInfo.PopupMargin + 2;
        Brush.Color := DisabledColor;
        Inc(Left, ItemInfo.PopupMargin + 9);
        Pen.Color := DisabledColor;
      end
      else
        Pen.Color := DisabledColor;

      if ((ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR) and Enabled then
      begin
        Top := Bottom div 2;
        DrawLineEx(Canvas.Handle, Left + 2, Top, Right - 2, Top, DisabledColor)
      end else
      begin
        Top := Bottom div 2;
        i := 14;
        while i < Right - 14 do
        begin
          Canvas.Pixels[i, Top] := clBtntext;
          i := i + 3;
        end;
      end;
    end
    else
    if enabled then
    begin
      Left := Right div 2;
      DrawLineEx(Canvas.Handle, Left, Top + 2, Left, Bottom - 2, DisabledColor);
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect;
  const ToolbarInfo: TTBXToolbarInfo);
const
  Pattern: array[0..15] of Byte = (0, 0, $CC, 0, $78, 0, $30, 0, $78, 0, $CC, 0,
    0, 0, 0, 0);
  DragHandleOffsets: array[Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0,
    1), (5, 0, 5));

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then
      Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then
      Result := bisHot
    else
      Result := bisNormal;
  end;

var
  Sz, Z: Integer;
  R2: TRect;
  j: Integer;
  BtnVisible, Horz, CloseButtondown, CloseButtonHover: Boolean;
begin
  with Canvas do
  begin
    if not ToolbarInfo.IsVertical then
      j := -1
    else
      j := +1;
    R.Top := R.Top + j;
    if (ToolbarInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR then
      PaintBackgnd(Canvas, ZERO_RECT, R, R, MenubarColor, false, VT_UNKNOWN)
    else
      PaintBackgnd(Canvas, ZERO_RECT, R, R, ToolbarColor, false, VT_UNKNOWN);
    R.Top := R.Top - j;
    if ((ToolbarInfo.ViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR)
      or (((ToolbarInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR))
      or ((ToolbarInfo.ViewType and TVT_TOOLWINDOW) = TVT_TOOLWINDOW) then
      if (ToolbarInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR then
      begin
        DrawLineEx(Canvas.Handle, R.Left, R.Top, R.Right - 1, R.Top,
          clBtnHighLight); // Top
        DrawLineEx(Canvas.Handle, R.Left, R.Top, R.Left, R.Bottom - 1,
          clBtnHighLight); // Left
        DrawLineEx(Canvas.Handle, R.Left, R.Bottom - 1, R.Right, R.Bottom - 1,
          clBtnShadow);
        DrawLineEx(Canvas.Handle, R.Right - 1, R.Top + 1, R.Right - 1, R.Bottom
          - 1, clBtnShadow);
      end
      else
      begin
        DrawLineEx(Canvas.Handle, R.Left, R.Top, R.Right - 1, R.Top,
          clBtnHighLight); // Top
        DrawLineEx(Canvas.Handle, R.Left, R.Top, R.Left, R.Bottom - 1,
          clBtnHighLight); // Left
        DrawLineEx(Canvas.Handle, R.Left + 1, R.Bottom - 1, R.Right, R.Bottom -
          1, clBtnShadow); // Bottom
        DrawLineEx(Canvas.Handle, R.Right - 1, R.Top + 1, R.Right - 1, R.Bottom
          - 1, clBtnShadow); // Right
      end;
    InflateRect(R, -2, -2);

    if not ToolbarInfo.AllowDrag then
      Exit;

    BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
    Sz := GetTBXDragHandleSize(ToolbarInfo);
    Horz := not ToolbarInfo.IsVertical;
    if Horz then
      R.Right := R.Left + Sz
    else
      R.Bottom := R.Top + Sz;

    { Drag Handle }
    if ToolbarInfo.DragHandleStyle <> DHS_NONE then
    begin
      R2 := R;
      if Horz then
      begin
        R2.Top := R2.Top + 2;
        R2.Bottom := R2.Bottom - 2;
        R2.Right := R2.Right - 1;
        if BtnVisible then
        begin
          Inc(R2.Top, Sz - 2);
          Inc(R2.Left, 4);
          Dec(R2.Right, 4);
        end;
        GradientFill(Canvas, R2, clBtnFace, clBtnShadow, TGLeftRight);
        DrawLineEx(Canvas.Handle, R2.Left, R2.Top + 1, R2.Left, R2.Bottom - 1,
          Blend(clBtnFace, clBtnHighLight, 50));
      end
      else
      begin
        R2.Left := R2.Left + 2;
        R2.Right := R2.Right - 2;
        if BtnVisible then
        begin
          Dec(R2.Right, Sz - 2);
          Inc(R2.Top, 4);
          Dec(R2.Bottom, 4);
        end;
        R2.Bottom := R2.Bottom - 1;
        GradientFill(Canvas, R2, clBtnFace, clBtnShadow, TGTopBottom);
        DrawLineEx(Canvas.Handle, R2.Left + 1, R2.Top, R2.Right - 1, R2.Top,
          Blend(clBtnFace, clBtnHighLight, 50));
      end;
    end;

    { Close Button }
    if BtnVisible then
    begin
      CloseButtonDown := (ToolbarInfo.CloseButtonState and CDBS_PRESSED) <> 0;
      CloseButtonHover := (ToolbarInfo.CloseButtonState and CDBS_HOT) <> 0;
      R2 := GetTBXDockedCloseButtonRect(ToolbarInfo);
      Z := 2;
      if Horz then
      begin
        Dec(R2.Bottom, Z);
        Dec(R2.Right, Z);
      end
      else
      begin
        Dec(R2.Bottom, Z);
        Inc(R2.Left, Z);
      end;

      if CloseButtonDown then
      begin
        Windows.DrawEdge(Canvas.Handle, R2, BDR_SUNKENOUTER, BF_RECT);
        OffsetRect(R2, 1, 1);
      end
      else if CloseButtonHover then
        Windows.DrawEdge(Canvas.Handle, R2, BDR_RAISEDINNER, BF_RECT);
      DrawGlyph(Canvas.Handle, R2, 7, 7, Pattern[0], clBtnText);
    end;
  end;
end;

procedure TTBXMirandaTheme.PaintDock(Canvas: TCanvas; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
var
  R: TRect;
begin
  Canvas.Pen.Width := 0;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;//blend(clBtnShadow, clBtnFace, 25);
  R := DockRect;
  InFlateRect(R, 1, 1);
  Canvas.Rectangle(R);
end;

procedure TTBXMirandaTheme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const
  DockPanelInfo: TTBXDockPanelInfo);
var
  DC: HDC;
  Sz: Integer;
  R2: TRect;
  Flags: Integer;
  CloseButtonDown, CloseButtonHover: Boolean;

  procedure CaptionFill(R: TRect);
  const
    GRAD: array[Boolean] of TGradientKind = (gkHorz, gkVert);
  begin
    if USE_THEMES then
      GradFill(DC, R, Lighten(ToolbarColor, 12), Lighten(ToolbarColor, -12),
        GRAD[DockPanelInfo.IsVertical])
    else
      FillRectEx(DC, R, ToolbarColor);
  end;

begin
  DC := Canvas.Handle;
  with Canvas, DockPanelInfo do
  begin
    Sz := GetSystemMetrics(SM_CYSMCAPTION);

    { Border }
    FrameRectEx(DC, R, ToolbarColor, True);
    R2 := R;
    if ShowCaption then
      if IsVertical then
        Inc(R2.Top, Sz)
      else
        Inc(R2.Left, Sz);
    FrameRectEx(DC, R2, clWindow, False);

    if not ShowCaption then
      Exit;

    { Caption area }
    if IsVertical then
      R.Bottom := R.Top + Sz
    else
      R.Right := R.Left + Sz;
    Windows.DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);

    { Close button }
    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      CloseButtonDown := (CloseButtonState and CDBS_PRESSED) <> 0;
      CloseButtonHover := (CloseButtonState and CDBS_HOT) <> 0;
      R2 := R;
      Brush.Color := ToolbarColor;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz;
        R.Right := R2.Left;
        CaptionFill(R2);
        InflateRect(R2, -1, -1);
        Inc(R2.Left);
      end
      else
      begin
        R2.Top := R2.Bottom - Sz;
        R.Bottom := R2.Top;
        CaptionFill(R2);
        InflateRect(R2, -1, -1);
        Dec(R2.Bottom);
      end;
      if CloseButtonDown then
      begin
        Windows.DrawEdge(DC, R2, BDR_SUNKENOUTER, BF_RECT);
        OffsetRect(R2, 1, 1);
      end
      else if CloseButtonHover then
        Windows.DrawEdge(DC, R2, BDR_RAISEDINNER, BF_RECT);
      InflateRect(R2, -2, -2);
      DrawButtonBitmap(Canvas, R2, clBtnText);
    end;

    { Caption }
    CaptionFill(R);
    if IsVertical then
      InflateRect(R, -2, 0)
    else
      Inflaterect(R, 0, -2);
    Font.Assign(SmCaptionFont);
    Font.Color := clBtnText;
    Brush.Style := bsClear;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then
      DrawText(Canvas.Handle, Caption, -1, R, Flags)
    else
      DrawRotatedText(Canvas.Handle, string(Caption), R, Flags);
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXMirandaTheme.SetupColorCache;
var
  DC: HDC;
  HotBtnFace, DisabledText: TColor;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then
      C := GetNearestColor(DC, ColorToRGB(C));
  end;

begin
  DC := StockCompatibleBitmap.Canvas.Handle;
  MenubarColor := clBtnFace;
  ToolbarColor := clBtnFace;

  PopupColor := clWindow;
  DockPanelColor := PopupColor;
  PopupFrameColor := clBtnshadow;

  EditFrameColor := clBtnShadow;
  EditFrameDisColor := Blend(clBtnFace, clWindow, 75);
  ;

  BaseColor := clHighLight;
  HotBtnFace := clBtnHighlight;
  DisabledText := Blend(clBtnshadow, clWindow, 90);
  DisabledColor := DisabledText;

  WinFrameColors[wfsActive, wfpBorder] := Blend(clBtnShadow, clWhite, 80);
  WinFrameColors[wfsActive, wfpCaption] := clBtnFace;
  WinFrameColors[wfsActive, wfpCaptionText] := clCaptionText;
  WinFrameColors[wfsInactive, wfpBorder] := WinFrameColors[wfsActive,
    wfpBorder];
  WinFrameColors[wfsInactive, wfpCaption] := BaseColor;
  WinFrameColors[wfsInactive, wfpCaptionText] := clSilver;

  PnlFrameColors[wfsActive, wfpBorder] := WinFrameColors[wfsActive, wfpBorder];
  PnlFrameColors[wfsActive, wfpCaption] := clBtnFace;
  PnlFrameColors[wfsActive, wfpCaptionText] := clCaptionText;

  PnlFrameColors[wfsInactive, wfpBorder] := clBtnFace;
  PnlFrameColors[wfsInactive, wfpCaption] := clBtnFace;
  PnlFrameColors[wfsInactive, wfpCaptionText] := clBtnFace;

  BtnItemColors[bisNormal, ipBody] := clNone;
  BtnItemColors[bisNormal, ipText] := clBtnText;
  SetContrast(BtnItemColors[bisNormal, ipText], ToolbarColor, 180);
  BtnItemColors[bisNormal, ipFrame] := clNone;

  BtnItemColors[bisDisabled, ipBody] := clNone;
  BtnItemColors[bisDisabled, ipText] := Blend(clGrayText, clWindow, 70);
  SetContrast(BtnItemColors[bisDisabled, ipText], ToolbarColor, 80);
  BtnItemColors[bisDisabled, ipFrame] := Blend(clBtnFace, clWindow, 50);

  BtnItemColors[bisSelected, ipBody] := clNone;
  SetContrast(BtnItemColors[bisSelected, ipBody], ToolbarColor, 5);
  BtnItemColors[bisSelected, ipText] := BtnItemColors[bisNormal, ipText];
  BtnItemColors[bisSelected, ipFrame] := clBtnShadow;

  BtnItemColors[bisPressed, ipBody] := Blend(BaseColor, clWhite, 50);
  BtnItemColors[bisPressed, ipText] := clBtnText;
  BtnItemColors[bisPressed, ipFrame] := BaseColor;

  BtnItemColors[bisHot, ipBody] := Blend(clHighlight, clWindow, 20);
  BtnItemColors[bisHot, ipText] := clBtnText;
  BtnItemColors[bisHot, ipFrame] := clBtnShadow;

  BtnItemColors[bisDisabledHot, ipBody] := HotBtnFace;
  BtnItemColors[bisDisabledHot, ipText] := DisabledText;
  BtnItemColors[bisDisabledHot, ipFrame] := clNone;

  BtnItemColors[bisSelectedHot, ipBody] := Blend(BaseColor, clWhite, 25);
  SetContrast(BtnItemColors[bisSelectedHot, ipBody], ToolbarColor, 30);
  BtnItemColors[bisSelectedHot, ipText] := clBtnText;
  BtnItemColors[bisSelectedHot, ipFrame] := clBtnShadow;

  BtnItemColors[bisPopupParent, ipBody] := Blend(BaseColor, clWhite, 80);
  BtnItemColors[bisPopupParent, ipText] := clBtnText;
  BtnItemColors[bisPopupParent, ipFrame] := clBtnShadow;

  MenuItemColors[misNormal, ipBody] := clNone;
  MenuItemColors[misNormal, ipText] := clBtnText;
  SetContrast(MenuItemColors[misNormal, ipText], PopupColor, 180);
  MenuItemColors[misNormal, ipFrame] := clNone;

  MenuItemColors[misDisabled, ipBody] := clNone;
  MenuItemColors[misDisabled, ipText] := Blend(clGrayText, clWindow, 70);
  SetContrast(MenuItemColors[misDisabled, ipText], PopupColor, 80);
  MenuItemColors[misDisabled, ipFrame] := clNone;

  MenuItemColors[misHot, ipBody] := clHighLight;
  MenuItemColors[misHot, ipText] := clHighLightText;
  MenuItemColors[misHot, ipFrame] := BtnItemColors[bisHot, ipFrame];

  MenuItemColors[misDisabledHot, ipBody] := PopupColor;
  MenuItemColors[misDisabledHot, ipText] := Blend(clGrayText, clWindow, 50);
  MenuItemColors[misDisabledHot, ipFrame] := clNone;

  DragHandleColor := Blend(clGrayText, clWindow, 75);
  SetContrast(DragHandleColor, ToolbarColor, 85);
  IconShadowColor := Blend(clBlack, HotBtnFace, 25);

  ToolbarSeparatorColor := clBtnText;
  PopupSeparatorColor := ToolbarSeparatorColor;

  Undither(MenubarColor);
  Undither(ToolbarColor);
  Undither(PopupColor);
  Undither(DockPanelColor);
  Undither(PopupFrameColor);
  Undither(WinFrameColors[wfsActive, wfpBorder]);
  Undither(WinFrameColors[wfsActive, wfpCaption]);
  Undither(WinFrameColors[wfsActive, wfpCaptionText]);
  Undither(WinFrameColors[wfsInactive, wfpBorder]);
  Undither(WinFrameColors[wfsInactive, wfpCaption]);
  Undither(WinFrameColors[wfsInactive, wfpCaptionText]);
  Undither(PnlFrameColors[wfsActive, wfpBorder]);
  Undither(PnlFrameColors[wfsActive, wfpCaption]);
  Undither(PnlFrameColors[wfsActive, wfpCaptionText]);
  Undither(PnlFrameColors[wfsInactive, wfpBorder]);
  Undither(PnlFrameColors[wfsInactive, wfpCaption]);
  Undither(PnlFrameColors[wfsInactive, wfpCaptionText]);
  Undither(BtnItemColors[bisNormal, ipBody]);
  Undither(BtnItemColors[bisNormal, ipText]);
  Undither(BtnItemColors[bisNormal, ipFrame]);
  Undither(BtnItemColors[bisDisabled, ipBody]);
  Undither(BtnItemColors[bisDisabled, ipText]);
  Undither(BtnItemColors[bisDisabled, ipFrame]);
  Undither(BtnItemColors[bisSelected, ipBody]);
  Undither(BtnItemColors[bisSelected, ipText]);
  Undither(BtnItemColors[bisSelected, ipFrame]);
  Undither(BtnItemColors[bisPressed, ipBody]);
  Undither(BtnItemColors[bisPressed, ipText]);
  Undither(BtnItemColors[bisPressed, ipFrame]);
  Undither(BtnItemColors[bisHot, ipBody]);
  Undither(BtnItemColors[bisHot, ipText]);
  Undither(BtnItemColors[bisHot, ipFrame]);
  Undither(BtnItemColors[bisDisabledHot, ipBody]);
  Undither(BtnItemColors[bisDisabledHot, ipText]);
  Undither(BtnItemColors[bisDisabledHot, ipFrame]);
  Undither(BtnItemColors[bisSelectedHot, ipBody]);
  Undither(BtnItemColors[bisSelectedHot, ipText]);
  Undither(BtnItemColors[bisSelectedHot, ipFrame]);
  Undither(BtnItemColors[bisPopupParent, ipBody]);
  Undither(BtnItemColors[bisPopupParent, ipText]);
  Undither(BtnItemColors[bisPopupParent, ipFrame]);
  Undither(MenuItemColors[misNormal, ipBody]);
  Undither(MenuItemColors[misNormal, ipText]);
  Undither(MenuItemColors[misNormal, ipFrame]);
  Undither(MenuItemColors[misDisabled, ipBody]);
  Undither(MenuItemColors[misDisabled, ipText]);
  Undither(MenuItemColors[misDisabled, ipFrame]);
  Undither(MenuItemColors[misHot, ipBody]);
  Undither(MenuItemColors[misHot, ipText]);
  Undither(MenuItemColors[misHot, ipFrame]);
  Undither(MenuItemColors[misDisabledHot, ipBody]);
  Undither(MenuItemColors[misDisabledHot, ipText]);
  Undither(MenuItemColors[misDisabledHot, ipFrame]);
  Undither(DragHandleColor);
  Undither(IconShadowColor);
  Undither(ToolbarSeparatorColor);
  Undither(PopupSeparatorColor);
  Undither(StatusPanelFrameColor);
end;

function TTBXMirandaTheme.GetPopupShadowType: Integer;
begin
  Result := PST_WINDOWS2K;
end;

constructor TTBXMirandaTheme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then
    InitializeStock;
  Inc(CounterLock);
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXMirandaTheme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  Dec(CounterLock);
  if CounterLock = 0 then
    FinalizeStock;
  inherited;
end;

procedure TTBXMirandaTheme.GetViewMargins(ViewType: Integer;
  out Margins: TTBXMargins);
begin
  Margins.LeftWidth := 0;
  Margins.TopHeight := 0;
  Margins.RightWidth := 0;
  Margins.BottomHeight := 0;
end;

procedure TTBXMirandaTheme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  Flags: Integer;
begin
  R := ARect;
  if Hot then
    Flags := DFCS_FLAT
  else
    Flags := 0;
  case ButtonType of
    PSBT_UP: Flags := Flags or DFCS_SCROLLUP;
    PSBT_DOWN: Flags := Flags or DFCS_SCROLLDOWN;
    PSBT_LEFT: Flags := Flags or DFCS_SCROLLLEFT;
    PSBT_RIGHT: Flags := Flags or DFCS_SCROLLRIGHT;
  end;
  Windows.DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
end;

procedure TTBXMirandaTheme.PaintFrameControl(Canvas: TCanvas; R: TRect; Kind,
  State: Integer; Params: Pointer);
const
  Offs: array[0..10] of integer = (3, 1, 1, 0, 0, 0, 0, 0, 1, 1, 3);
var
  X, Y: Integer;
  rc1, gc1, bc1,
    rc2, gc2, bc2,
    rc3, gc3, bc3,
    y1, i, j, GSize: Integer;
  GradCol: TRGBTriple;
  Brush: HBrush;

  procedure RadioGradient(const Canvas: TCanvas; const ARect: TRect;
    const StartColor, EndColor: TColor; Shadow: TColor);
  var
    i: integer;
  begin
    rc1 := GetRValue(ColorToRGB(StartColor));
    gc1 := GetGValue(ColorToRGB(StartColor));
    bc1 := GetBValue(ColorToRGB(StartColor));
    rc2 := GetRValue(ColorToRGB(EndColor));
    gc2 := GetGValue(ColorToRGB(EndColor));
    bc2 := GetBValue(ColorToRGB(EndColor));

    rc3 := rc2 + (((rc1 - rc2) * 15) div 9);
    gc3 := gc2 + (((gc1 - gc2) * 15) div 9);
    bc3 := bc2 + (((bc1 - bc2) * 15) div 9);

    if rc3 < 0 then
      rc3 := 0
    else if rc3 > 255 then
      rc3 := 255;
    if gc3 < 0 then
      gc3 := 0
    else if gc3 > 255 then
      gc3 := 255;
    if bc3 < 0 then
      bc3 := 0
    else if bc3 > 255 then
      bc3 := 255;

    j := 0;
    GSize := (ARect.Bottom - ARect.Top) - 1;
    if Shadow <> clNone then
    begin
      i := GSize;
      Brush := CreateSolidBrush(Shadow);
      Windows.FillRect(Canvas.Handle, Rect(ARect.Left + Offs[i], ARect.Bottom
        - i - 1, ARect.Right - Offs[i], ARect.Bottom - i), Brush);
      DeleteObject(Brush);
      j := 1;
    end;

    for i := 0 to GSize - j do
    begin
      Brush := CreateSolidBrush(
        RGB(Byte(rc1 + (((rc2 - rc1) * i) div GSize)),
        Byte(gc1 + (((gc2 - gc1) * i) div GSize)),
        Byte(bc1 + (((bc2 - bc1) * i) div GSize))));
      Windows.FillRect(Canvas.Handle, Rect(ARect.Left + Offs[i], ARect.Bottom
        - i - 1, ARect.Right - Offs[i], ARect.Bottom - i), Brush);
      DeleteObject(Brush);
    end;
  end;

  function TextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then
      Result := BtnItemColors[bisDisabled, ipText]
    else if Boolean(State and PFS_PUSHED) then
      Result := BtnItemColors[bisPressed, ipText]
    else if Boolean(State and PFS_MIXED) then
      Result := clBtnShadow
    else if Boolean(State and PFS_HOT) then
      Result := BtnItemColors[bisHot, ipText]
    else
      Result := BtnItemColors[bisNormal, ipText];
  end;

begin
  with Canvas do
    case Kind of
      PFC_CHECKBOX:
        begin
          if Boolean(State and PFS_HOT) then
          begin
            InflateRect(R, -1, -1);
            if Boolean(State and PFS_PUSHED) then
            begin
              GradientFill(Canvas, R, Blend(clBtnHigHlight, clBtnFace, 60),
                Blend(clBtnFace, clBtnShadow, 60), TGTopBottom);
              Pen.Color := Blend(clBtnShadow, clBtnFace, 25);
              MoveTo(R.Left, R.Top);
              LineTo(R.Right, R.Top);
              Pen.Color := clBtnShadow;
            end
            else if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              GradientFill(Canvas, R, clBtnFace, clBtnHighLight, TGTopBottom);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end
            else
            begin
              GradientFill(Canvas, R, clBtnFace, clBtnHighLight, TGTopBottom);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end;
            InflateRect(R, 1, 1);
            RoundFrame(Canvas, R, 0, 0);
          end
          else
          begin
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              InflateRect(R, -1, -1);
              GradientFill(Canvas, R, clBtnHighLight, clBtnFace, TGTopBottom);
              Pen.Color := Blend(clBtnShadow, clBtnFace, 25);
              MoveTo(R.Left, R.Top);
              LineTo(R.Right, R.Top);
              InflateRect(R, 1, 1);
              Pen.Color := clBtnShadow;
            end
            else
              Pen.Color := clBtnShadow;
            RoundFrame(Canvas, R, 0, 0);
            Pen.Style := psSolid;
            Brush.Style := bsSolid;
          end;

          if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
          begin
            X := (R.Left + R.Right) div 2 - 1;
            Y := (R.Top + R.Bottom) div 2 + 1;
            Pen.Color := TextColor;
            Brush.Color := Pen.Color;
            Polygon([Point(X - 2, Y), Point(X, Y + 2), Point(X + 4, Y - 2),
              Point(X + 4, Y - 4), Point(X, Y), Point(X - 2, Y - 2), Point(X -
                2, Y)]);
          end;
        end;

      PFC_RADIOBUTTON:
        begin
          Brush.Style := bsClear;
          if Boolean(State and PFS_HOT) then
          begin
            InflateRect(R, -1, -1);
            if Boolean(State and PFS_PUSHED) then
            begin
              RadioGradient(Canvas, R, Blend(clBtnHigHlight, clBtnFace, 60),
                Blend(clBtnFace, clBtnShadow, 60), Blend(clBtnShadow, clBtnFace,
                25));
              Pen.Color := clBtnShadow;
            end
            else if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              RadioGradient(Canvas, R, clBtnFace, clBtnHighLight, clNone);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end
            else
            begin
              RadioGradient(Canvas, R, clBtnFace, clBtnHighLight, clNone);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end;
            InflateRect(R, 1, 1);
            X := R.Left;
            Y := R.Top;
            Polygon([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
              Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
                Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
                Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y +
                3),
                Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y +
                9),
                Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y +
                11),
                Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y +
                11),
                Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y +
                9)]);
          end
          else
          begin
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              InflateRect(R, -1, -1);
              RadioGradient(Canvas, R, clBtnHighLight, clBtnFace,
                Blend(clBtnShadow, clBtnFace, 25));
              Pen.Color := clBtnShadow;
              InflateRect(R, 1, 1);
            end
            else
              Pen.Color := clBtnShadow;

            Brush.Style := bsClear;
            X := R.Left;
            Y := R.Top;
            Polygon([Point(X, Y + 8), Point(X, Y + 4), Point(X + 1, Y + 3),
              Point(X + 1, Y + 2), Point(X + 2, Y + 1), Point(X + 3, Y + 1),
                Point(X + 4, Y), Point(X + 8, Y), Point(X + 9, Y + 1),
                Point(X + 10, Y + 1), Point(X + 11, Y + 2), Point(X + 11, Y +
                3),
                Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y +
                9),
                Point(X + 11, Y + 10), Point(X + 10, Y + 11), Point(X + 9, Y +
                11),
                Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y +
                11),
                Point(X + 2, Y + 11), Point(X + 1, Y + 10), Point(X + 1, Y +
                9)]);
          end;
          InflateRect(R, -1, -1);
          Pen.Style := psSolid;
          Brush.Style := bsClear;
          if Boolean(State and PFS_CHECKED) then
          begin
            InflateRect(R, -3, -3);
            Pen.Color := TextColor;
            Brush.Color := Pen.Color;
            with R do
              Ellipse(Left, Top, Right, Bottom);
          end;
        end;
    end;
end;

procedure TTBXMirandaTheme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part:
  Integer);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  Color, Hi, Lo, Lo1, Hi1, Lo2, Hi2, Lo3, Hi3, Hi4: TColor;
  D, Sz, i: Integer;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(Canvas.Handle, Right - 1 - D, Bottom - 1, Right, Bottom - D -
        2, C);
    Inc(D);
  end;

begin
  with Canvas do
    case Part of
      SBP_BODY:
        begin
          FillRectEx(Canvas.Handle, R, clBtnFace);
          DrawLineEx(Canvas.Handle, R.Left, R.Top + 0, R.Right, R.Top + 0,
            Blend(clBtnShadow, cl3DDkShadow, 70));
          DrawLineEx(Canvas.Handle, R.Left, R.Top + 1, R.Right, R.Top + 1,
            NearestMixedColor(clBtnShadow, clBtnFace, 128));
          DrawLineEx(Canvas.Handle, R.Left, R.Top + 2, R.Right, R.Top + 2,
            NearestMixedColor(clBtnShadow, clBtnFace, 64));
          DrawLineEx(Canvas.Handle, R.Left, R.Top + 3, R.Right, R.Top + 3,
            NearestMixedColor(clBtnShadow, clBtnFace, 32));
          DrawLineEx(Canvas.Handle, R.Left, R.Top + 4, R.Right, R.Top + 4,
            NearestMixedColor(clBtnShadow, clBtnFace, 12));

          DrawLineEx(Canvas.Handle, R.Left, R.Bottom - 3, R.Right, R.Bottom - 3,
            NearestMixedColor(clBtnShadow, clBtnFace, 8));
          DrawLineEx(Canvas.Handle, R.Left, R.Bottom - 2, R.Right, R.Bottom - 2,
            NearestMixedColor(clBtnShadow, clBtnFace, 16));
          DrawLineEx(Canvas.Handle, R.Left, R.Bottom - 1, R.Right, R.Bottom - 1,
            NearestMixedColor(clBtnShadow, clBtnFace, 24));
        end;
      SBP_PANE, SBP_LASTPANE:
        begin
          if Part = SBP_PANE then Dec(R.Right, 3);
          DrawLineEx(Canvas.Handle, R.Right, R.Top + 4, R.Right, R.Bottom - 3, DisabledColor);
        end;
      SBP_GRIPPER:
        if DottedGrip then
        begin
          Color := clBtnFace;
          Hi1 := GetNearestColor(Handle, MixColors(Color, clBtnShadow, 64));
          Lo1 := GetNearestColor(Handle, MixColors(Color, clBtnShadow, 48));
          Hi2 := GetNearestColor(Handle, MixColors(Color, clBtnShadow, 32));
          Lo2 := GetNearestColor(Handle, MixColors(Color, clBtnShadow, 16));
          Hi3 := GetNearestColor(Handle, MixColors(Color, clBtnHighlight, 128));
          Lo3 := GetNearestColor(Handle, MixColors(Color, clBtnHighlight, 96));
          Hi4 := GetNearestColor(Handle, MixColors(Color, clBtnHighlight, 72));

          with R do
          begin
            Pixels[Right - 12, Bottom - 4] := Lo2;
            Pixels[Right - 12, Bottom - 3] := Hi2;
            Pixels[Right - 11, Bottom - 4] := Lo1;
            Pixels[Right - 11, Bottom - 3] := Hi1;
            Pixels[Right - 11, Bottom - 2] := Hi4;
            Pixels[Right - 10, Bottom - 2] := Hi3;
            Pixels[Right - 10, Bottom - 3] := Lo3;

            Pixels[Right - 8, Bottom - 4] := Lo2;
            Pixels[Right - 8, Bottom - 3] := Hi2;
            Pixels[Right - 7, Bottom - 4] := Lo1;
            Pixels[Right - 7, Bottom - 3] := Hi1;
            Pixels[Right - 7, Bottom - 2] := Hi4;
            Pixels[Right - 6, Bottom - 2] := Hi3;
            Pixels[Right - 6, Bottom - 3] := Lo3;

            Pixels[Right - 4, Bottom - 4] := Lo2;
            Pixels[Right - 4, Bottom - 3] := Hi2;
            Pixels[Right - 3, Bottom - 4] := Lo1;
            Pixels[Right - 3, Bottom - 3] := Hi1;
            Pixels[Right - 3, Bottom - 2] := Hi4;
            Pixels[Right - 2, Bottom - 2] := Hi3;
            Pixels[Right - 2, Bottom - 3] := Lo3;

            Pixels[Right - 8, Bottom - 8] := Lo2;
            Pixels[Right - 8, Bottom - 7] := Hi2;
            Pixels[Right - 7, Bottom - 8] := Lo1;
            Pixels[Right - 7, Bottom - 7] := Hi1;
            Pixels[Right - 7, Bottom - 6] := Hi4;
            Pixels[Right - 6, Bottom - 6] := Hi3;
            Pixels[Right - 6, Bottom - 7] := Lo3;

            Pixels[Right - 4, Bottom - 8] := Lo2;
            Pixels[Right - 4, Bottom - 7] := Hi2;
            Pixels[Right - 3, Bottom - 8] := Lo1;
            Pixels[Right - 3, Bottom - 7] := Hi1;
            Pixels[Right - 3, Bottom - 6] := Hi4;
            Pixels[Right - 2, Bottom - 6] := Hi3;
            Pixels[Right - 2, Bottom - 7] := Lo3;

            Pixels[Right - 4, Bottom - 12] := Lo2;
            Pixels[Right - 4, Bottom - 11] := Hi2;
            Pixels[Right - 3, Bottom - 12] := Lo1;
            Pixels[Right - 3, Bottom - 11] := Hi1;
            Pixels[Right - 3, Bottom - 10] := Hi4;
            Pixels[Right - 2, Bottom - 10] := Hi3;
            Pixels[Right - 2, Bottom - 11] := Lo3;
          end;
        end
        else
        begin
          Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
          Lo := NearestMixedColor(clBtnFace, clBtnHighlight, 64);
          Hi := NearestMixedColor(clBtnFace, clBtnShadow, 64);

          D := 2;
          for I := 1 to 3 do
          begin
            case Sz of
              0..8:
                begin
                  DiagLine(Lo);
                  DiagLine(Hi);
                end;
              9..12:
                begin
                  DiagLine(Lo);
                  DiagLine(Hi);
                  Inc(D);
                end;
            else
              DiagLine(Lo);
              Inc(D, 1);
              DiagLine(Hi);
              Inc(D, 1);
            end;
          end;
        end;
    end;
end;

procedure TTBXMirandaTheme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then
    SetupColorCache;
end;

{$IFNDEF DTM_Package}
initialization
  RegisterTBXTheme('Miranda', TTBXMirandaTheme);
{$ENDIF}
end.

