unit TBXXitoTheme;

// TBX Package
// Copyright 2001-2002 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
//
// Xito theme by Roy Magne Klever
//
// 18.10.2003 - Roy Magne Klever roy.magne@os.ino.no
//              First version...
//              �2003 Roy Magne Klever

interface

uses
  Windows, Messages, Graphics, TBXThemes, ImgList;

{$DEFINE ALTERNATIVE_DISABLED_STYLE} // remove the asterisk to change appearance of disabled images
{*$DEFINE SMALL_CLOSE_BUTTON}// remove the asterisk for smaller close button size

type
  TGradDir = (tGLeftRight, tGTopBottom);
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  TWinFrameState = (wfsActive, wfsInactive);

  TTBXXitoTheme = class(TTBXTheme)
  private
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    { View/Window Colors }
    MenubarColor: TColor;
    ToolbarColor: TColor;
    PopupColor: TColor;
    DockPanelColor: TColor;

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


    procedure SetupColorCache; virtual;
  protected
    { Internal Methods }
    function GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
    function GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics access}
    function GetBooleanMetrics(Index: Integer): Boolean; override;
    function GetImageOffset(Canvas: TCanvas; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function GetIntegerMetrics(Index: Integer): Integer; override;
    function GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function GetViewColor(AViewType: Integer): TColor; override;
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

    { Painting routines }
    procedure PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; Transparent: Boolean; AViewType: Integer); override;
    procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string; AFormat: Cardinal; Rotated: Boolean); override;
    procedure PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintDock(Canvas: TCanvas; const ClientRect, DockRect: TRect; DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintImage(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(Canvas: TCanvas; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintSeparator(Canvas: TCanvas; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(Canvas: TCanvas; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer); override;
    procedure PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer); override;
  end;

{$IFDEF DTM_Package}
function TBXThemeName: shortstring; stdcall;
procedure TBXRegisterTheme(RegisterTheme: boolean); stdcall;
{$ENDIF}

implementation

{.$R tbx_glyphs.res}

uses
  TBXUtils, TB2Common, TB2Item, Classes, Controls, Commctrl, Forms;

{$IFDEF DTM_Package}
exports
  TBXThemeName,
  TBXRegisterTheme;

const
  cThemeName = 'Xito';

function TBXThemeName: shortstring; stdcall;
begin
  result := cThemeName;
end;

procedure TBXRegisterTheme(RegisterTheme: boolean); stdcall;
begin
  if RegisterTheme then
    RegisterTBXTheme(cThemeName, TTBXZezioTheme)
  else
    UnregisterTBXTheme(cThemeName);
end;
{$ENDIF}

var
  StockImgList: TImageList;
  CounterLock: Integer;
  GradientBmp: TBitmap;
  gradCol1, gradCol2,
    gradHandle1, gradHandle2, gradHandle3,
    gradBL, gradTL: TColor;

procedure InitializeStock;
begin
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
  GradientBmp := TBitmap.Create;
  GradientBmp.PixelFormat := pf24bit;
end;

procedure FinalizeStock;
begin
  GradientBmp.Free;
  StockImgList.Free;
end;

{ TTBXXitoTheme }

procedure RoundFrame(Canvas: TCanvas; R: TRect; RL, RR: Integer);
begin
  with Canvas, R do
  begin
    Dec(Right); Dec(Bottom);
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
    Inc(Right); Inc(Bottom);
  end;
end;

function TTBXXitoTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT:    Result:= False;
    TMB_EDITHEIGHTEVEN:            Result:= False;
    TMB_PAINTDOCKBACKGROUND:       Result:= True;
    TMB_SOLIDTOOLBARNCAREA:        Result:= True;
    TMB_SOLIDTOOLBARCLIENTAREA:    Result:= True;
  else
    Result := False;
  end;
end;

function TTBXXitoTheme.GetIntegerMetrics(Index: Integer): Integer;
const
  DEFAULT = -1;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH: Result := 12;

    TMI_DROPDOWN_ARROWWIDTH: Result := 8;
    TMI_DROPDOWN_ARROWMARGIN: Result := 3;

    TMI_MENU_IMGTEXTSPACE: Result := 0;
    TMI_MENU_LCAPTIONMARGIN: Result := 8;
    TMI_MENU_RCAPTIONMARGIN: Result := 3;
    TMI_MENU_SEPARATORSIZE: Result := 7;
    TMI_MENU_MDI_DW: Result := 2;
    TMI_MENU_MDI_DH: Result := 2;

    TMI_TLBR_SEPARATORSIZE: Result := 5;
    TMI_EDIT_MENURIGHTINDENT: Result := 1;
    TMI_EDIT_FRAMEWIDTH: Result := 1;
    TMI_EDIT_TEXTMARGINHORZ: Result := 2;
    TMI_EDIT_TEXTMARGINVERT: Result := 2;
    TMI_EDIT_BTNWIDTH: Result := 14;
  else
    Result := DEFAULT;
  end;
end;

function TTBXXitoTheme.GetViewColor(AViewType: Integer): TColor;
begin
  Result := clBtnFace;
  if (AViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (AViewType and TVT_MENUBAR) = TVT_MENUBAR then Result := MenubarColor
    else Result := ToolbarColor;
  end
  else if (AViewType and VT_POPUP) = VT_POPUP then
  begin
    if (AViewType and PVT_LISTBOX) = PVT_LISTBOX then Result := clWindow
    else Result := PopupColor;
  end
  else if (AViewType and VT_DOCKPANEL) = VT_DOCKPANEL then Result := DockPanelColor;
end;

function TTBXXitoTheme.GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
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
    if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
    else if ItemInfo.IsPopupParent then
      B := bisPopupParent
    else if Pushed then B := bisPressed
    else if Selected then B := BFlags2[HoverKind <> hkNone]
    else B := BFlags3[HoverKind <> hkNone];
    Result := BtnItemColors[B, ItemPart];
    if Embedded then
    begin
      if (ItemPart = ipBody) and (Result = clNone) then Result := ToolbarColor;
      if ItemPart = ipFrame then
      begin
        if Selected then Result := clWindowFrame
        else if (Result = clNone) then Result := clBtnShadow;
      end;
    end;
  end;
end;

function TTBXXitoTheme.GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
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
      ((ItemOptions and IO_TOOLBARSTYLE) = 0);
    Embedded := ((ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
      ((ViewType and TVT_EMBEDDED) = TVT_EMBEDDED);
    if IsMenuItem then
    begin
      if not Enabled then M := MFlags1[HoverKind = hkKeyboardHover]
      else M := MFlags2[HoverKind <> hkNone];
      Result := MenuItemColors[M, ItemPart];
    end
    else
    begin
      if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
      else if ItemInfo.IsPopupParent then B := bisPopupParent
      else if Pushed then B := bisPressed
      else if Selected then B := BFlags2[HoverKind <> hkNone]
      else B := BFlags3[HoverKind <> hkNone];
      Result := BtnItemColors[B, ItemPart];
      if Embedded and (Result = clNone) then
      begin
      if ItemPart = ipBody then Result := ToolbarColor;
      if ItemPart = ipFrame then Result := clBtnShadow;
      end;
    end;
  end;
end;

procedure FillGradient(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor;
  const Direction: TGradDir);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;
  TGradientColors = array[0..255] of TRGBTriple;
var
  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3,
    y1, i, GSize: Integer;

  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
begin
  rc2 := GetRValue(ColorToRGB(StartColor));
  gc2 := GetGValue(ColorToRGB(StartColor));
  bc2 := GetBValue(ColorToRGB(StartColor));
  rc1 := GetRValue(ColorToRGB(EndColor));
  gc1 := GetGValue(ColorToRGB(EndColor));
  bc1 := GetBValue(ColorToRGB(EndColor));

  rc3 := rc1 + (((rc2 - rc1) * 15) div 9);
  gc3 := gc1 + (((gc2 - gc1) * 15) div 9);
  bc3 := bc1 + (((bc2 - bc1) * 15) div 9);

  if rc3 < 0 then rc3 := 0 else if rc3 > 255 then rc3 := 255;
  if gc3 < 0 then gc3 := 0 else if gc3 > 255 then gc3 := 255;
  if bc3 < 0 then bc3 := 0 else if bc3 > 255 then bc3 := 255;

  if Direction = tGTopBottom then
  begin
    GradientBmp.Width := 1;
    GradientBmp.Height := (ARect.Bottom - ARect.Top) - 1;
    GSize := GradientBmp.Height;

    y1 := GSize div 2;
    for i := 0 to y1 - 1 do
    begin
      Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
      GradCol.rgbtRed := Byte(rc1 + (((rc2 - rc1) * (i)) div y1));
      GradCol.rgbtGreen := Byte(gc1 + (((gc2 - gc1) * (i)) div y1));
      GradCol.rgbtBlue := Byte(bc1 + (((bc2 - bc1) * (i)) div y1));
      Row[0] := GradCol;
    end;
    if rc2 > rc1 then
    begin
      rc3 := rc2;
      gc3 := gc2;
      bc3 := bc2;
    end;
    for i := y1 to GSize - 1 do
    begin
      Row := PRGBTripleArray(GradientBmp.ScanLine[i]);
      GradCol.rgbtRed := Byte(rc3 + (((rc2 - rc3) * (i)) div GSize));
      GradCol.rgbtGreen := Byte(gc3 + (((gc2 - gc3) * (i)) div GSize));
      GradCol.rgbtBlue := Byte(bc3 + (((bc2 - bc3) * (i)) div GSize));
      Row[0] := GradCol;
    end;
    Canvas.StretchDraw(ARect, GradientBmp);
  end else
  begin
    GradientBmp.Width := (ARect.Right - ARect.Left) - 1;
    GradientBmp.Height := 1;
    GSize := GradientBmp.Width;

    y1 := GSize div 2;
    Row := PRGBTripleArray(GradientBmp.ScanLine[0]);
    for i := 0 to y1 - 1 do
    begin
      GradCol.rgbtRed := Byte(rc1 + (((rc2 - rc1) * (i)) div y1));
      GradCol.rgbtGreen := Byte(gc1 + (((gc2 - gc1) * (i)) div y1));
      GradCol.rgbtBlue := Byte(bc1 + (((bc2 - bc1) * (i)) div y1));
      Row[i] := GradCol;
    end;

    if rc2 > rc1 then
    begin
      rc3 := rc2;
      gc3 := gc2;
      bc3 := bc2;
    end;

    for i := y1 to GSize - 1 do
    begin
      GradCol.rgbtRed := Byte(rc2 + (((rc3 - rc2) * (i)) div GSize));
      GradCol.rgbtGreen := Byte(gc2 + (((gc3 - gc2) * (i)) div GSize));
      GradCol.rgbtBlue := Byte(bc2 + (((bc3 - bc2) * (i)) div GSize));
      Row[i] := GradCol;
    end;

    Canvas.StretchDraw(ARect, GradientBmp);
  end;
end;

procedure GradientFill(const Canvas: TCanvas; const ARect: TRect;
  const StartColor, EndColor: TColor;
  const Direction: TGradDir);
var
  rc1, rc2, gc1, gc2, bc1, bc2, Counter, GSize: Integer;
  Brush: HBrush;
begin
  rc1 := GetRValue(ColorToRGB(StartColor));
  gc1 := GetGValue(ColorToRGB(StartColor));
  bc1 := GetBValue(ColorToRGB(StartColor));
  rc2 := GetRValue(ColorToRGB(EndColor));
  gc2 := GetGValue(ColorToRGB(EndColor));
  bc2 := GetBValue(ColorToRGB(EndColor));

  if Direction = tGTopBottom then begin
    GSize := (ARect.Bottom - ARect.Top) - 1;
    for Counter := 0 to GSize do
    begin
      Brush := CreateSolidBrush(
        RGB(Byte(rc1 + (((rc2 - rc1) * (Counter)) div GSize)),
        Byte(gc1 + (((gc2 - gc1) * (Counter)) div GSize)),
        Byte(bc1 + (((bc2 - bc1) * (Counter)) div GSize))));
      Windows.FillRect(Canvas.Handle, Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom - Counter), Brush);
      DeleteObject(Brush);
    end;
  end else begin
    GSize := (ARect.Right - ARect.Left) - 1;
    for Counter := 0 to GSize do
    begin
      Brush := CreateSolidBrush(
        RGB(Byte(rc1 + (((rc2 - rc1) * (Counter)) div GSize)),
        Byte(gc1 + (((gc2 - gc1) * (Counter)) div GSize)),
        Byte(bc1 + (((bc2 - bc1) * (Counter)) div GSize))));
      Windows.FillRect(Canvas.Handle, Rect(ARect.Left, ARect.Top, ARect.Right - Counter, ARect.Bottom), Brush);
      DeleteObject(Brush);
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
      BitBlt(Canvas.Handle, (Left + Right - W + 1) div 2, (Top + Bottom - H) div 2, W, H,
        Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    RestoreDC(Canvas.Handle, Index);
  finally
    Bmp.Free;
  end;
end;

function TTBXXitoTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXXitoTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipText);
end;

function TTBXXitoTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then result := GetViewColor(ItemInfo.ViewType);
end;

procedure TTBXXitoTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
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
  if (ViewType and VT_TOOLBAR) = VT_TOOLBAR then
  begin
    if (ViewType and TVT_FLOATING) = TVT_FLOATING then
    begin
      Resizable := (ViewType and TVT_RESIZABLE) = TVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else SetBorder(2, 2);
  end
  else if (ViewType and VT_POPUP) = VT_POPUP then
  begin
    if (ViewType and PVT_POPUPMENU) = PVT_POPUPMENU then Border.X := 1
    else Border.X := 2;
    Border.Y := 2;
  end
  else if (ViewType and VT_DOCKPANEL) = VT_DOCKPANEL then
  begin
    if (ViewType and DPVT_FLOATING) = DPVT_FLOATING then
    begin
      Resizable := (ViewType and DPVT_RESIZABLE) = DPVT_RESIZABLE;
      Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
      Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
    end
    else SetBorder(2, 2);
  end
  else SetBorder(0, 0);
end;

procedure TTBXXitoTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
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
          LeftWidth := 1;
          RightWidth := 1;
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

procedure TTBXXitoTheme.PaintBackgnd(Canvas: TCanvas; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; Transparent: Boolean; AViewType: Integer);
var
  Brush: HBrush;
  R: TRect;
  IsHoriz: boolean;
begin
  if not Transparent then
  begin
    if ((AViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR)
      and (not (AViewType and TVT_EMBEDDED = TVT_EMBEDDED))
      or ((AViewType and TVT_TOOLWINDOW) = TVT_TOOLWINDOW)
      or ((AViewType and TVT_MENUBAR) = TVT_MENUBAR) then
    begin
      IntersectRect(R, ARect, AClipRect);
      if (ADockRect.Top = 0) and
        (ADockRect.Left = 0) and
        (ADockRect.Right = 0) and
        (ADockRect.Bottom = 0) then
        IsHoriz := (ARect.Right > ARect.Bottom)
      else
        IsHoriz := Abs(R.Right - R.Left) > Abs(R.Bottom - R.Top);

      if IsHoriz then
        FillGradient(Canvas, R, $00f0f0f0, clWhite, TGTopBottom)
      else
        FillGradient(Canvas, R, $00f0f0f0, clWhite, TGLeftRight);
    end else
    begin
      Brush := CreateSolidBrush(ColorToRGB(AColor));
      IntersectRect(R, ARect, AClipRect);
      FillRect(Canvas.Handle, R, Brush);
      DeleteObject(Brush);
    end;
  end;
end;

procedure TTBXXitoTheme.PaintCaption(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo; const ACaption: string;
  AFormat: Cardinal; Rotated: Boolean);
var
  R: TRect;
begin
  with ItemInfo, Canvas do
  begin
    R := ARect;
    Brush.Style := bsClear;
    if Font.Color = clNone then Font.Color := GetPartColor(ItemInfo, ipText);
    if not Rotated then Windows.DrawText(Handle, PChar(ACaption), Length(ACaption), R, AFormat)
    else DrawRotatedText(Handle, ACaption, R, AFormat);
    Brush.Style := bsSolid;
  end;
end;

procedure TTBXXitoTheme.PaintCheckMark(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
begin
  X := (ARect.Left + ARect.Right) div 2 - 2;
  Y := (ARect.Top + ARect.Bottom) div 2 + 1;
  Canvas.Pen.Color := GetBtnColor(ItemInfo, ipText);
  Canvas.Polyline([Point(X - 2, Y - 2), Point(X, Y), Point(X + 4, Y - 4),
    Point(X + 4, Y - 3), Point(X, Y + 1), Point(X - 2, Y - 1), Point(X - 2, Y - 2)]);
end;

procedure TTBXXitoTheme.PaintChevron(Canvas: TCanvas; ARect: TRect; const ItemInfo: TTBXItemInfo);
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
    with R2 do BitBlt(Canvas.Handle, Left, Top, Right - Left,
        Bottom - Top, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
  end;
end;

procedure TTBXXitoTheme.PaintEditButton(Canvas: TCanvas; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);

const
  ArrowColor: array[Boolean] of TColor = (clBtnText, clMenuText);
var
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  SaveItemInfoPushed: Boolean;

  procedure PaintEnabled(R: TRect; Pressed: Boolean);
  begin
    if Pressed then
    else
      if BtnHot then
      else
        if Embedded then
        begin
          FillRectEx(Canvas.Handle, R, ToolBarColor);
          FrameRectEx(Canvas.Handle, R, $00d0d0d0, True);
        end
        else FrameRectEx(Canvas.Handle, R, $00d0d0d0, True);
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
        if BtnPressed or BtnHot then
        begin
          if BtnPressed then
          begin
            FillRectEx(Canvas.Handle, R, $00c0c0c0);
            Pen.Color := $00909090;
            DrawLineEx(Canvas.Handle, R.Left - 1, R.Top, R.Left - 1, R.Bottom, $00909090);
          end else
          begin
            FillRectEx(Canvas.Handle, R, $00e0e0e0);
            Pen.Color := $00909090;
            DrawLineEx(Canvas.Handle, R.Left - 1, R.Top, R.Left - 1, R.Bottom, $00909090);
          end;
        end else
        begin
          DrawLineEx(Canvas.Handle, R.Left - 1, R.Top + 1, R.Left - 1, R.Bottom - 1, $00d0d0d0);
          DrawLineEx(Canvas.Handle, R.Left - 1, R.Top + 1, R.Left - 1, R.Bottom - 1, clWhite)
        end;
        PaintEnabled(R, BtnPressed);
      end;
      if BtnDisabled then FrameRectEx(Canvas.Handle, R, BtnItemColors[bisDisabled, ipFrame], True);
      PaintDropDownArrow(Canvas, R, ItemInfo);
    end

    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;

      { Upper button }

      BR := R;
      BR.Bottom := (R.Top + R.Bottom + 1) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
      SaveItemInfoPushed := ItemInfo.Pushed;
      ItemInfo.Pushed := BtnPressed;

      if not BtnDisabled then
      begin
        if BtnPressed or BtnHot or Embedded then
        begin
          // BR = Button Rectangle
          if BtnPressed then
            GradientFill(Canvas, BR, clWhite, clSilver, TGTopBottom)
          else
            if BtnHot then
            begin
              FillRectEx(Canvas.Handle, BR, $00d0d0d0);
              Pen.Color := $00808080;
            end;
        end
        else if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
        begin
          if not Embedded then
          begin
            Pen.Color := clWhite;
          end
          else Pen.Color := GetBtnColor(ItemInfo, ipFrame);
        end;
        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_UP) <> 0);
      end;
      X := (BR.Left + BR.Right) div 2;
      Y := (BR.Top + BR.Bottom - 1) div 2;
      Pen.Color := GetPartColor(ItemInfo, ipText);
      Brush.Color := Pen.Color;
      Polygon([Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)]);

      BR := R;
      BR.Top := (R.Top + R.Bottom) div 2;
      BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
      ItemInfo.Pushed := BtnPressed;
      if not BtnDisabled then
      begin
        if BtnPressed or BtnHot or Embedded then
        begin
          if BtnPressed then
            GradientFill(Canvas, BR, clWhite, clSilver, TGTopBottom)
          else
            if BtnHot then
              begin
                FillRectEx(Canvas.Handle, BR, $00d0d0d0);
                Pen.Color := $00808080;
              end;
        end
        else if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
        begin
          if not Embedded then
          begin
            Pen.Color := clWindow;
          end else
            Pen.Color := GetBtnColor(ItemInfo, ipFrame);
        end;
        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_UP) <> 0);
      end;

      X := (BR.Left + BR.Right) div 2;
      Y := (BR.Top + BR.Bottom) div 2;
      Pen.Color := GetPartColor(ItemInfo, ipText);
      Brush.Color := Pen.Color;
      Polygon([Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)]);

      ItemInfo.Pushed := SaveItemInfoPushed;
    end;
  end;
end;

procedure TTBXXitoTheme.PaintEditFrame(Canvas: TCanvas;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
begin
  R := ARect;
  PaintFrame(Canvas, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);

  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);

  if ItemInfo.Enabled then
  begin
    if ItemInfo.HoverKind = hkNone then
      Canvas.Brush.Color := $00d0d0d0 // Should be central...
    else
      Canvas.Brush.Color := clWhite;
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWhite;
  end;

  if not ItemInfo.Enabled then
  begin
    if ItemInfo.HoverKind = hkNone then
      Canvas.Brush.Color := BtnItemColors[bisDisabled, ipFrame]
    else
      Canvas.Brush.Color := clWhite;
    Canvas.FrameRect(R);
    Canvas.Brush.Color := clWhite;
  end;

  InflateRect(R, -1, -1);
  if ItemInfo.Enabled then Canvas.FillRect(R);

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(Canvas, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXXitoTheme.PaintDropDownArrow(Canvas: TCanvas;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
begin
  with ARect, Canvas do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;
    Pen.Color := GetPartColor(ItemInfo, ipText);
    Brush.Color := Pen.Color;
    if ItemInfo.IsVertical then Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)])
    else Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
  end;
end;

procedure TTBXXitoTheme.PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
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
    if ComboPart = cpSplitLeft then Dec(RR,2); // 1
    if (ItemInfo.ItemOptions and IO_TOOLBARSTYLE) = 0 then
    begin
      RR := 1; RL := 1;
    end;

    if Embedded and not ShowHover then
    begin
      if Enabled then
      begin
        InflateRect(R, -1, -1);
        FillRectEx(Canvas.Handle, R, ToolBarColor);
        InflateRect(R, 1, 1);
        Pen.Color := Blend(clHighLight, clWindow, 20);
      end
      else
        Pen.Color := Blend(clHighLight, clWindow, 15);
      RoundFrame(Canvas, R, RL, RR);
    end;

    if ((ViewType and TVT_MENUBAR) = TVT_MENUBAR) then
    begin
      if ((Pushed or Selected) and Enabled) or ShowHover then
        if Pushed then
        begin

          Pen.Color := $00d0d0d0;
          RoundFrame(Canvas, R, 1, 1);

          InflateRect(R, -1, -1);
          FillRectEx(Canvas.Handle, R, $00d0d0d0);
          Pen.Color := $00808080;
          InflateRect(R, 1, 1);
          RoundFrame(Canvas, R, 1, 1);
        end else
        begin
          Pen.Color := BtnItemColors[bisHot, ipFrame];
          RoundFrame(Canvas, R, 1, 1);
        end;
      Exit;
    end;

    if (Pushed or Selected) and Enabled then
    begin
      InflateRect(R, -1, -1);

      if not Pushed and (HoverKind = hkNone) then
      begin
        FillRectEx(Canvas.Handle, R, $00d8d8d8);
        Pen.Color := $00b8b8b8;
        MoveTo(R.Left, R.Bottom-2); LineTo(R.Left, R.Top); LineTo(R.Right, R.Top);
        Pen.Color := $00f8f8f8;
        InflateRect(R, 1, 1);
        RoundFrame(Canvas, R, RL, RR);
      end else
      begin
        InflateRect(R, 1, 1);
        Pen.Color := $00d0d0d0;
        RoundFrame(Canvas, R, 1, 1);
        InflateRect(R, -1, -1);
        FillRectEx(Canvas.Handle, R, $00c0c0c0);
        Pen.Color := $00909090;
        InflateRect(R, 1, 1);
        RoundFrame(Canvas, R, RL, RR);
      end;

      if Selected and (not Pushed) and (HoverKind <> hkNone) then
      begin
        Pen.Color := $00d0d0d0;
        RoundFrame(Canvas, R, 1, 1);

        InflateRect(R, -1, -1);
        FillRectEx(Canvas.Handle, R, $00d0d0d0);
        Pen.Color := $00808080;
        InflateRect(R, 1, 1);
        RoundFrame(Canvas, R, RL, RR);
      end;
    end
    else
      if ShowHover or ((ItemOptions and IO_DESIGNING) <> 0) then
      begin
        Pen.Color := $00e0e0e0;
        RoundFrame(Canvas, R, 1, 1);

        InflateRect(R, -1, -1);
        FillRectEx(Canvas.Handle, R, $00e0e0e0);
        InflateRect(R, 1, 1);
        Pen.Color := GetBtnColor(ItemInfo, ipFrame);
        RoundFrame(Canvas, R, RL, RR);
      end;

    if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
  end;
end;

procedure TTBXXitoTheme.PaintFloatingBorder(Canvas: TCanvas; const ARect: TRect;
  const WindowInfo: TTBXWindowInfo);
const
  WinStates: array[Boolean] of TWinFramestate = (wfsInactive, wfsActive);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if not WindowInfo.Active then Result := bisDisabled
    else if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  WinState: TWinFrameState;
  BtnItemState: TBtnItemState;
  SaveIndex, X, Y: Integer;
  Sz: TPoint;
  R: TRect;
  BodyColor, CaptionColor, CaptionText: TColor;
  IsDockPanel: Boolean;

  procedure DrawGlyph(C: TColor);
  var
    Bmp: TBitmap;
    DC: HDC;
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Monochrome := True;
      StockImgList.GetBitmap(0, Bmp);
      Canvas.Brush.Color := C;
      DC := Canvas.Handle;
      SetTextColor(DC, clBlack);
      SetBkColor(DC, clWhite);
      BitBlt(DC, X, Y, StockImgList.Width, StockImgList.Height,
        Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    finally
      Bmp.Free;
    end;
  end;

begin
  with Canvas do
  begin
    WinState := WinStates[WindowInfo.Active];
    IsDockPanel := (WindowInfo.ViewType and VT_DOCKPANEL) = VT_DOCKPANEL;
    BodyColor := Brush.Color;

    if (WRP_BORDER and WindowInfo.RedrawPart) <> 0 then
    begin
      R := ARect;

      if not IsDockPanel then
        Brush.Color := WinFrameColors[WinState, wfpBorder]
      else
        Brush.Color := PnlFrameColors[WinState, wfpBorder];
      SaveIndex := SaveDC(Canvas.Handle);
      Sz := WindowInfo.FloatingBorderSize;
      with R, Sz do ExcludeClipRect(Canvas.Handle, Left + X, Top + Y, Right - X, Bottom - Y);
      FillRect(R);
      RestoreDC(Canvas.Handle, SaveIndex);
      InflateRect(R, -Sz.X, -Sz.Y);
      Pen.Color := BodyColor;
      with R do
        if not IsDockPanel then
          Canvas.Polyline([
            Point(Left, Top - 1), Point(Right - 1, Top - 1),
              Point(Right, Top), Point(Right, Bottom - 1),
              Point(Right - 1, Bottom),
              Point(Left, Bottom), Point(Left - 1, Bottom - 1),
              Point(Left - 1, Top), Point(Left, Top - 1)
              ])
        else
          Canvas.Polyline([
            Point(Left, Top - 1), Point(Right - 1, Top - 1),
              Point(Right, Top), Point(Right, Bottom),
              Point(Left - 1, Bottom),
              Point(Left - 1, Top), Point(Left, Top - 1)
              ]);
    end;

    if not WindowInfo.ShowCaption then Exit;

    if (WindowInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
    begin
      CaptionColor := WinFrameColors[WinState, wfpCaption];
      CaptionText := WinFrameColors[WinState, wfpCaptionText];
    end
    else
    begin
      CaptionColor := PnlFrameColors[WinState, wfpCaption];
      CaptionText := PnlFrameColors[WinState, wfpCaptionText];
    end;

    { Caption }
    if (WRP_CAPTION and WindowInfo.RedrawPart) <> 0 then
    begin
      R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
      with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
      DrawLineEx(Canvas.Handle, R.Left, R.Bottom, R.Right, R.Bottom, BodyColor);

      if ((CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0) and
        ((WRP_CLOSEBTN and WindowInfo.RedrawPart) <> 0) then
        Dec(R.Right, GetSystemMetrics(SM_CYSMCAPTION) - 1);

      Brush.Color := CaptionColor;
      FillGradient(Canvas, R, $00f0f0f0, gradCol2, TGTopBottom);

      InflateRect(R, -2, 0);
      Font.Assign(SmCaptionFont);
      Font.Color := CaptionText;
      Brush.Style := bsClear;
      DrawText(Canvas.Handle, WindowInfo.Caption, -1, R,
        DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX);
    end;

    { Close button }
    if (CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0 then
    begin
      R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
      with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
      R.Left := R.Right - (R.Bottom - R.Top);
      DrawLineEx(Canvas.Handle, R.Left - 1, R.Bottom, R.Right, R.Bottom, BodyColor);
      Brush.Color := CaptionColor;

      FillGradient(Canvas, R, $00f0f0f0, gradCol2, TGTopBottom);

      BtnItemState := GetBtnItemState(WindowInfo.CloseButtonState);

      if BtnItemState = bisHot then
      begin
        FrameRectEx(Canvas.Handle, R, clGray, True);
        FillRectEx(Canvas.Handle, R, clWhite);
        DrawButtonBitmap(Canvas, R, clBlack);
      end else
      if BtnItemState = bisPressed then
      begin
        FrameRectEx(Canvas.Handle, R, clGray, True);
        FillRectEx(Canvas.Handle, R, clSilver);
        DrawButtonBitmap(Canvas, R, clWhite);
      end else
      begin
        FrameRectEx(Canvas.Handle, R, BtnItemColors[BtnItemState, ipFrame], True);
        FillRectEx(Canvas.Handle, R, BtnItemColors[BtnItemState, ipBody]);
        DrawButtonBitmap(Canvas, R, clBlack);
      end;
    end;
  end;
end;

procedure TTBXXitoTheme.PaintFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  E: Boolean;
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
    else if E and (HoverKind <> hkNone) or
      ((ItemOptions and IO_DESIGNING) <> 0) then
    begin
      InflateRect(R, -1, -1);
      FillRectEx(Canvas.Handle, R, $00d0d0d0);
      if Pushed then
        Canvas.Pen.Color := $00909090
      else
        Canvas.Pen.Color := GetPartColor(ItemInfo, ipFrame);
      InflateRect(R, 1, 1);
      RoundFrame(Canvas, R, 1, 1);
    end;
  end;
end;

function TTBXXitoTheme.GetImageOffset(Canvas: TCanvas;
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

procedure TTBXXitoTheme.PaintImage(Canvas: TCanvas; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
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

{$IFNDEF ALTERNATIVE_DISABLED_STYLE}
    HiContrast := IsDarkColor(GetItemImageBackground(ItemInfo), 64);

    if not Enabled then
    begin
      DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex,
        BtnItemColors[bisDisabled, ipText]);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if not (Selected or Pushed and not IsPopupParent) then
      begin
        OffsetRect(ARect, 1, 1);
        DrawTBXIconFullShadow(Canvas, ARect, ImageList, ImageIndex, IconShadowColor);
        OffsetRect(ARect, -2, -2);
      end;
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast or TBXLoColor then
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
    else
      HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, 178);
{$ELSE}
    HiContrast := ColorIntensity(GetItemImageBackground(ItemInfo)) < 80;
    if not Enabled then
    begin
      if not HiContrast then
        DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 0)
      else
        DrawTBXIconFlatShadow(Canvas, ARect, ImageList, ImageIndex, clBtnShadow);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if not (Selected or Pushed and not IsPopupParent) then
      begin // rmk Removed the shados under the glyphs
        // aaa
        //DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        //OffsetRect(ARect, 1, 1);
        //DrawTBXIconShadow(Canvas, ARect, ImageList, ImageIndex, 1);
        //OffsetRect(ARect, -2, -2);
      end;
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast or TBXLoColor then
      DrawTBXIcon(Canvas, ARect, ImageList, ImageIndex, HiContrast)
    else // rmk 17.08.2003 Removed washed out glyphs
      HighlightTBXIcon(Canvas, ARect, ImageList, ImageIndex, clWindow, {178} 255);
{$ENDIF}
  end;
end;

procedure TTBXXitoTheme.PaintMDIButton(Canvas: TCanvas; ARect: TRect;
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

procedure TTBXXitoTheme.PaintMenuItemFrame(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  R := ARect;
  if (ItemInfo.ViewType and PVT_TOOLBOX) <> PVT_TOOLBOX then with Canvas do
  begin
    R.Right := R.Left + ItemInfo.PopupMargin + 2;
    Brush.Color := clWhite;
    FillRect(R);
    Inc(R.Left);
    R.Right := ARect.Right - 1;
  end;
  PaintFrame(Canvas, R, ItemInfo);
end;

procedure TTBXXitoTheme.PaintMenuItem(Canvas: TCanvas; const ARect: TRect; var ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  R, R2: TRect;
  X, Y: Integer;
  ArrowWidth: Integer;
  ClrText: TColor;

  procedure DrawArrow(AColor: TColor);
  begin
    Canvas.Pen.Color := AColor;
    Canvas.Brush.Color := AColor;
    Canvas.Polygon([Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)]);
  end;

  procedure PaintButton(Canvas: TCanvas; const ARect: TRect; const ItemInfo: TTBXItemInfo);
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
      if ComboPart = cpSplitLeft then Dec(RR);
      if (ItemInfo.ItemOptions and IO_TOOLBARSTYLE) = 0 then
      begin
        RR := 1; RL := 1;
      end;

      if Embedded and not ShowHover then
      begin
        if Enabled then
        begin
          InflateRect(R, -1, -1);
          FillRectEx(Canvas.Handle, R, ToolBarColor);
          InflateRect(R, 1, 1);
          Pen.Color := Blend(clHighLight, clWindow, 20);
        end
        else
          Pen.Color := Blend(clHighLight, clWindow, 15);
        RoundFrame(Canvas, R, RL, RR);
      end;

      if (Pushed or Selected) and Enabled then
      begin
        if not Pushed and (HoverKind = hkNone) then
        begin
          FillRectEx(Canvas.Handle, R, $00d8d8d8);
          Pen.Color := $00b8b8b8;
          MoveTo(R.Left, R.Bottom-2); LineTo(R.Left, R.Top); LineTo(R.Right, R.Top);
          Pen.Color := $00c4c4c4;
          MoveTo(R.Left, R.Bottom-1); LineTo(R.Right-1, R.Bottom-1); LineTo(R.Right-1, R.Top);

          Pen.Color := clWhite;
          InflateRect(R, 1, 1);
          RoundFrame(Canvas, R, 2, 2);
        end else
        begin
          FillRectEx(Canvas.Handle, R, $00d0d0d0);
          Pen.Color := $00909090;
          InflateRect(R, 1, 1);
          RoundFrame(Canvas, R, 2, 2);
        end;
        InflateRect(R, 1, 1);
      end
      else if ShowHover or ((ItemOptions and IO_DESIGNING) <> 0) then
      begin
        Canvas.Pen.Color := Blend(clred, clWindow, 40);
        RoundFrame(Canvas, R, RL, RR);
      end;
      if ComboPart = cpSplitRight then PaintDropDownArrow(Canvas, R, ItemInfo);
    end;
  end;

begin
  with Canvas, ItemInfo do
  begin
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);
    PaintMenuItemFrame(Canvas, ARect, ItemInfo);
    ClrText := GetPartColor(ItemInfo, ipText);
    R := ARect;

    if (ItemOptions and IO_COMBO) <> 0 then
    begin
      X := R.Right - ArrowWidth - 1;
      if not ItemInfo.Enabled then Pen.Color := ClrText
      else if HoverKind = hkMouseHover then Pen.Color := clWhite
      else Pen.Color := PopupSeparatorColor;
      MoveTo(X, R.Top + 1);
      LineTo(X, R.Bottom - 1);
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3 - 1;
      DrawArrow(ClrText);
    end;

    R2:= Arect;
    InflateRect(R2, -1, -1);

    if Selected and Enabled then
    begin
      R.Left := R2.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      InflateRect(R, -1, -1);
      PaintButton(Canvas, R, ItemInfo);
    end;
  end;
end;

procedure TTBXXitoTheme.PaintPopupNCArea(Canvas: TCanvas; R: TRect; const PopupInfo: TTBXPopupInfo);
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
    Pen.Color:= PopupFrameColor;
    Pixels[1,1]:= PopupFrameColor;
    Pixels[R.Right - 1, 1]:= PopupFrameColor;
    Pixels[1, R.Bottom - 1]:= PopupFrameColor;
    Pixels[R.Right - 1, R.Bottom - 1]:= PopupFrameColor;

    if not IsRectEmpty(PopupInfo.ParentRect) then
    begin
      PR := PopupInfo.ParentRect;
      if not IsRectEmpty(PR) then with PR do
        begin
          Pen.Color := ToolbarColor;
          if Bottom = R.Top then
          begin
            if Left <= R.Left then Left := R.Left - 1;
            if Right >= R.Right then Right := R.Right + 1;
            MoveTo(Left + 1, Bottom - 1);
            LineTo(Right - 1, Bottom - 1);
          end
          else if Top = R.Bottom then
          begin
            if Left <= R.Left then Left := R.Left - 1;
            if Right >= R.Right then Right := R.Right + 1;
            MoveTo(Left + 1, Top);
            LineTo(Right - 1, Top);
          end;
          if Right = R.Left then
          begin
            if Top <= R.Top then Top := R.Top - 1;
            if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
            MoveTo(Right - 1, Top + 1);
            LineTo(Right - 1, Bottom - 1);
          end
          else if Left = R.Right then
          begin
            if Top <= R.Top then Top := R.Top - 1;
            if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
            MoveTo(Left, Top + 1);
            LineTo(Left, Bottom - 1);
          end;
        end;
    end;
  end;
end;

procedure TTBXXitoTheme.PaintSeparator(Canvas: TCanvas; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
var
  IsToolbox: Boolean;
  R: TRect;
  i: integer;
begin
  with ItemInfo, ARect, Canvas do
  begin
    if Horizontal then
    begin
      IsToolbox := (ViewType and PVT_TOOLBOX) = PVT_TOOLBOX;
      if ((ItemOptions and IO_TOOLBARSTYLE) = 0) and not IsToolBox then
      begin
        R := ARect;
        R.Right := ItemInfo.PopupMargin + 2;
        Brush.Color := ToolbarColor;
        Inc(Left, ItemInfo.PopupMargin + 9);
        Pen.Color := PopupSeparatorColor;
      end
      else
        Pen.Color := ToolbarSeparatorColor;

      if (ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR then
      begin
        Top := Bottom div 2 +1;
        Left := Left + 4;
        Right := Right - 2;
        Bottom := Top + 1;
        DrawLineEx(Canvas.Handle, Left, Top, Right, Top, clWhite);
        Top := Top - 1;
        Left := Left - 1;
        Right := Right - 1;
        DrawLineEx(Canvas.Handle, Left, Top, Right, Top, gradCol1);
      end else
      begin
        Top := Bottom div 2;
        i:= 14;
        while i < Right - 14 do
        begin
          Canvas.Pixels[i, Top] := $00909090;
          i := i + 3;
        end;
      end;
    end else if enabled then
    begin
      Top := Top + 4;
      Bottom := Bottom - 2;
      Left := Right div 2;
      DrawLineEx(Canvas.Handle, Left, Top, Left, Bottom, clWhite);
      Top := Top - 1;
      Left := Left - 1;
      Bottom := Bottom - 1;
      DrawLineEx(Canvas.Handle, Left, Top, Left, Bottom, gradCol1);
    end;
  end;
end;

procedure TTBXXitoTheme.PaintToolbarNCArea(Canvas: TCanvas; R: TRect;
  const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleOffsets: array[Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0, 1), (5, 0, 5));

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  Sz: Integer;
  R2: TRect;
  I: Integer;
  BtnVisible, Horz: Boolean;
  BtnItemState: TBtnItemState;
begin
  with Canvas do
  begin
    if ((ToolbarInfo.ViewType and TVT_NORMALTOOLBAR) = TVT_NORMALTOOLBAR)
      or ((ToolbarInfo.ViewType and TVT_TOOLWINDOW) = TVT_TOOLWINDOW)
      or ((ToolbarInfo.ViewType and TVT_MENUBAR) = TVT_MENUBAR)
      then
    begin

      with R do begin
        if (R.Right > R.Bottom) then
        begin
          R2 := r;
          R2.Top := R2.Top + 2;
          R2.Bottom := R.Bottom - 2;
          FillGradient(Canvas, R, $00f0f0f0, clWhite, TGTopBottom);
          FillGradient(Canvas, R2, $00f0f0f0, clWhite, TGTopBottom);
        end else
          FillGradient(Canvas, R, $00f0f0f0, clWhite, TGLeftRight);
        R2 := R;
        R2.Left := R2.Right - 1;

        R2.Top := R2.Top + 1;

        Pen.Color := gradBL;
        MoveTo(Left + 2, Bottom - 1);
        LineTo(Right - 3, Bottom - 1);
        LineTo(Right - 1, Bottom - 3);
        LineTo(Right - 1, Top);

        Pen.Color := clWhite;
        MoveTo(Left + 1, Bottom - 2);
        LineTo(Left, Bottom - 3);
        LineTo(Left, Top + 2);
        LineTo(Left + 2, Top);
        LineTo(Right - 3, Top);
        LineTo(Right - 1, Top + 2);

        Pen.Color := $00e0e0e0;
        MoveTo(Left, Top);
        LineTo(Left, Top + 1);
        LineTo(Left + 2, Top - 1);

        MoveTo(Right - 1, Top);
        LineTo(Right - 2, Top);
        LineTo(Right, Top + 2);

        MoveTo(Left, Bottom - 1);
        LineTo(Left, Bottom - 2);
        LineTo(Left + 2, Bottom);

        MoveTo(Right - 1, Bottom - 2);
        LineTo(Right - 2, Bottom - 1);
        LineTo(Right, Bottom - 1);
      end;
      InflateRect(R, -2, -2);
    end else
      InflateRect(R, -2, -2);

    if not ToolbarInfo.AllowDrag then Exit;

    BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
    Sz := GetTBXDragHandleSize(ToolbarInfo);
    Horz := not ToolbarInfo.IsVertical;
    if Horz then R.Right := R.Left + Sz
    else R.Bottom := R.Top + Sz;

    { Drag Handle }
    if ToolbarInfo.DragHandleStyle <> DHS_NONE then
    begin
      R2 := R;
      if Horz then
      begin
        Inc(R2.Left, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Inc(R2.Top, Sz - 2);
        R2.Right := R2.Left + 3;
      end
      else
      begin
        Inc(R2.Top, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
        if BtnVisible then Dec(R2.Right, Sz - 2);
        R2.Bottom := R2.Top + 3;
      end;

      Pen.Color := DragHandleColor;
      if Horz then
      begin
        I := R2.Top + 3;
        while I < R2.Bottom - 3 do
        begin
          Pen.Color := $00A0A0A0;
          MoveTo(R2.Left, I); LineTo(R2.Right, I);
          Pen.Color := clWhite;
          MoveTo(R2.Left, I + 1); LineTo(R2.Right, I + 1);
          Inc(I, 2);
        end;
      end
      else
      begin
        I := R2.Left + 3;
        while I < R2.Right - 3 do
        begin
          Pen.Color := $00a0a0a0;
          MoveTo(I, R2.Top); LineTo(I, R2.Bottom);
          Pen.Color := clWhite;
          MoveTo(I + 1, R2.Top); LineTo(I + 1, R2.Bottom);
          Inc(I, 2);
        end;
      end;
    end;

    { Close button }
    if BtnVisible then
    begin
      R2 := R;
      if Horz then
      begin
        Dec(R2.Right);
        R2.Bottom := R2.Top + R2.Right - R2.Left;
      end
      else
      begin
        Dec(R2.Bottom);
        R2.Left := R2.Right - R2.Bottom + R2.Top;
      end;

      BtnItemState := GetBtnItemState(ToolbarInfo.CloseButtonState);
      FrameRectEx(Canvas.Handle, R2, BtnItemColors[BtnItemState, ipFrame], True);
      FillRectEx(Canvas.Handle, R2, BtnItemColors[BtnItemState, ipBody]);
      DrawButtonBitmap(Canvas, R2, BtnItemColors[BtnItemState, ipText]);
    end;
  end;
end;

procedure TTBXXitoTheme.PaintDock(Canvas: TCanvas; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
var R: TRect;
begin
  Canvas.Pen.Width:= 0;
  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= $00e0e0e0;
  R:= DockRect;
  InFlateRect(R, 1,1);
  Canvas.Rectangle(R);
end;

procedure TTBXXitoTheme.PaintDockPanelNCArea(Canvas: TCanvas; R: TRect; const DockPanelInfo: TTBXDockPanelInfo);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  C, HeaderColor: TColor;
  I, Sz, Flags: Integer;
  R2: TRect;
  BtnItemState: TBtnItemState;
begin
  with Canvas, DockPanelInfo do
  begin
    C := Brush.Color; // Dock panel passes its body color in Canvas.Brush
    I := ColorIntensity(ColorToRGB(clBtnFace));
    R2 := R;
    if not TBXLoColor and (I in [64..250]) then
    begin
      FrameRectEx(Canvas.Handle, R, clBtnFace, True);
      FrameRectEx(Canvas.Handle, R, C, True);
      with R do
      begin
        Pixels[Left, Top] := clBtnFace;
        if IsVertical then Pixels[Right - 1, Top] := clBtnFace
        else Pixels[Left, Bottom - 1] := clBtnFace;
      end;
    end
    else
    begin
      FrameRectEx(Canvas.Handle, R, C, True);
      if I < 64 then Brush.Bitmap := AllocPatternBitmap(C, clWhite)
      else Brush.Bitmap := AllocPatternBitmap(C, clBtnShadow);
      FrameRect(R);
      with R do
      begin
        Pixels[Left, Top] := C;
        if IsVertical then Pixels[Right - 1, Top] := C
        else Pixels[Left, Bottom - 1] := C;
      end;
      InflateRect(R, -1, -1);
      FrameRectEx(Canvas.Handle, R, C, True);
    end;
    R := R2;
    InflateRect(R, -BorderSize.X, -BorderSize.Y);
    Sz := GetSystemMetrics(SM_CYSMCAPTION);
    if IsVertical then
    begin
      R.Bottom := R.Top + Sz - 1;
      DrawLineEx(Canvas.Handle, R.Left, R.Bottom, R.Right, R.Bottom, C);
      HeaderColor := clBtnFace;
      R.Bottom := R.Bottom + 1;
      FillGradient(Canvas, R, $00f0f0f0, gradCol2, TGTopBottom);
      DrawLineEx(Canvas.Handle, R.Left, R.Bottom - 1, R.Right, R.Bottom - 1, clSilver);
      R.Bottom := R.Bottom - 1;
    end
    else
    begin
      R.Right := R.Left + Sz - 1;
      DrawLineEx(Canvas.Handle, R.Right, R.Top, R.Right, R.Bottom, C);
      HeaderColor := clBtnFace;
      R.Right := R.Right + 1;
      FillGradient(Canvas, R, $00f0f0f0, gradCol2, TGLeftRight);
      DrawLineEx(Canvas.Handle, R.Right - 1, R.Top, R.Right - 1, R.Bottom, clSilver);
      R.Right := R.Right - 1;
    end;

    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      R2 := R;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz + 1;
        R.Right := R2.Left;
      end
      else
      begin
        R2.Top := R2.Bottom - Sz + 1;
        R.Bottom := R2.Top;
      end;

      BtnItemState := GetBtnItemState(CloseButtonState);

      if BtnItemState = bisHot then
      begin
        FrameRectEx(Canvas.Handle, R2, clGray, True);
        FillRectEx(Canvas.Handle, R2, clWhite);
        DrawButtonBitmap(Canvas, R2, clBlack);
      end else
        if BtnItemState = bisPressed then
        begin
          FrameRectEx(Canvas.Handle, R2, clGray, True);
          FillRectEx(Canvas.Handle, R2, clSilver);
          DrawButtonBitmap(Canvas, R2, clWhite);
        end else
        begin
          FrameRectEx(Canvas.Handle, R2, BtnItemColors[BtnItemState, ipFrame], True);
          FillRectEx(Canvas.Handle, R2, BtnItemColors[BtnItemState, ipBody]);
          DrawButtonBitmap(Canvas, R2, clBlack);
        end;
    end;

    if IsVertical then InflateRect(R, -4, 0)
    else InflateRect(R, 0, -4);
    Font.Assign(SmCaptionFont);
    Font.Color := clBlack;
    Brush.Color := HeaderColor;
    Brush.Style := bsClear;
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX;

    if IsVertical then DrawText(Canvas.Handle, Caption, -1, R, Flags)
    else DrawRotatedText(Canvas.Handle, string(Caption), R, Flags);
  end;
end;

procedure TTBXXitoTheme.SetupColorCache;
var
  DC: HDC;
  HotBtnFace, DisabledText: TColor;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then C := GetNearestColor(DC, ColorToRGB(C));
  end;

begin
  DC := StockCompatibleBitmap.Canvas.Handle;

  gradCol1 := NearestMixedColor(clBtnShadow, clBtnFace, 48);
  gradCol2 := clWhite;
  gradHandle1 := clBtnFace;
  gradHandle2 := clGray;
  gradHandle3 := clWhite;
  gradBL := $00b0b0b0;
  gradTL := Blend(gradCol1, gradCol2, 50);

  { View/Window Colors }
  MenubarColor := clWhite;
  ToolbarColor := gradCol2;
  PopupColor := clWhite;
  DockPanelColor := PopupColor;
  PopupFrameColor := $00C0C0C0;

  HotBtnFace := Blend(clHighlight, clWindow, 20);
  SetContrast(HotBtnFace, ToolbarColor, 50);
  DisabledText := Blend(clBtnshadow, clWindow, 90);

  WinFrameColors[wfsActive, wfpBorder] := $00909090;
  WinFrameColors[wfsActive, wfpCaption] := clWhite;
  WinFrameColors[wfsActive, wfpCaptionText] := clBlack;
  SetContrast(WinFrameColors[wfsActive, wfpCaptionText], clBtnShadow, 180);
  WinFrameColors[wfsInactive, wfpBorder] := WinFrameColors[wfsActive, wfpBorder];
  WinFrameColors[wfsInactive, wfpCaption] := clBtnFace;
  WinFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
  SetContrast(WinFrameColors[wfsInactive, wfpCaptionText], clBtnFace, 120);

  PnlFrameColors[wfsActive, wfpBorder] := $00909090;
  PnlFrameColors[wfsActive, wfpCaption] := clWhite;
  PnlFrameColors[wfsActive, wfpCaptionText] := clBlack;
  PnlFrameColors[wfsInactive, wfpBorder] := clBtnShadow;
  PnlFrameColors[wfsInactive, wfpCaption] := clBtnFace;
  PnlFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
  SetContrast(PnlFrameColors[wfsInactive, wfpCaptionText], clBtnFace, 120);

  BtnItemColors[bisNormal, ipBody] := clNone;
  BtnItemColors[bisNormal, ipText] := clBtnText;
  SetContrast(BtnItemColors[bisNormal, ipText], ToolbarColor, 180);
  BtnItemColors[bisNormal, ipFrame] := clNone;

  BtnItemColors[bisDisabled, ipBody] := clNone;
  BtnItemColors[bisDisabled, ipText] := DisabledText;
  SetContrast(BtnItemColors[bisDisabled, ipText], ToolbarColor, 80);
  BtnItemColors[bisDisabled, ipFrame] := $00D0D0D0;

  BtnItemColors[bisSelected, ipBody] := Blend(clHighlight, Blend(clBtnFace, clWindow, 50), 10);
  SetContrast(BtnItemColors[bisSelected, ipBody], ToolbarColor, 5);
  BtnItemColors[bisSelected, ipText] := BtnItemColors[bisNormal, ipText];
  BtnItemColors[bisSelected, ipFrame] := clSilver;

  BtnItemColors[bisPressed, ipBody] := Blend(clHighlight, clWindow, 30);
  BtnItemColors[bisPressed, ipText] := clBlack;
  BtnItemColors[bisPressed, ipFrame] := clRed;

  BtnItemColors[bisHot, ipBody] := HotBtnFace;
  BtnItemColors[bisHot, ipText] := clMenuText;
  SetContrast(BtnItemColors[bisHot, ipText], BtnItemColors[bisHot, ipBody], 180);
  BtnItemColors[bisHot, ipFrame] := $00a0a0a0;

  BtnItemColors[bisDisabledHot, ipBody] := HotBtnFace;
  BtnItemColors[bisDisabledHot, ipText] := DisabledText;
  BtnItemColors[bisDisabledHot, ipFrame] := clHighlight;

  BtnItemColors[bisSelectedHot, ipBody] := Blend(clHighlight, clWindow, 50);
  SetContrast(BtnItemColors[bisSelectedHot, ipBody], ToolbarColor, 30);
  BtnItemColors[bisSelectedHot, ipText] := clBlack;
  SetContrast(BtnItemColors[bisSelectedHot, ipText], BtnItemColors[bisSelectedHot, ipBody], 180);
  BtnItemColors[bisSelectedHot, ipFrame] := clHighlight;
  SetContrast(BtnItemColors[bisSelectedHot, ipFrame], BtnItemColors[bisSelectedHot, ipBody], 100);

  BtnItemColors[bisPopupParent, ipBody] := ToolbarColor;
  BtnItemColors[bisPopupParent, ipText] := BtnItemColors[bisNormal, ipText];
  BtnItemColors[bisPopupParent, ipFrame] := BtnItemColors[bisHot, ipFrame];

  MenuItemColors[misNormal, ipBody] := clNone;
  MenuItemColors[misNormal, ipText] := clWindowText;
  SetContrast(MenuItemColors[misNormal, ipText], PopupColor, 180);
  MenuItemColors[misNormal, ipFrame] := clNone;
  MenuItemColors[misDisabled, ipBody] := clNone;
  MenuItemColors[misDisabled, ipText] := Blend(clGrayText, clWindow, 70);
  SetContrast(MenuItemColors[misDisabled, ipText], PopupColor, 80);
  MenuItemColors[misDisabled, ipFrame] := clNone;

  MenuItemColors[misHot, ipBody] := BtnItemColors[bisHot, ipBody];
  MenuItemColors[misHot, ipText] := BtnItemColors[bisHot, ipText];
  MenuItemColors[misHot, ipFrame] := clSilver;

  MenuItemColors[misDisabledHot, ipBody] := PopupColor;
  MenuItemColors[misDisabledHot, ipText] := Blend(clGrayText, clWindow, 70);
  MenuItemColors[misDisabledHot, ipFrame] := clHighlight;

  DragHandleColor := Blend(clBtnShadow, clWindow, 75);
  SetContrast(DragHandleColor, ToolbarColor, 85);
  IconShadowColor := Blend(clBlack, HotBtnFace, 25);
  ToolbarSeparatorColor := Blend(clBtnShadow, clWindow, 70);
  SetContrast(ToolbarSeparatorColor, ToolbarColor, 50);
  PopupSeparatorColor := $00909090;
  StatusPanelFrameColor := Blend(clWindow, clBtnFace, 30);
  SetContrast(StatusPanelFrameColor, clBtnFace, 30);

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

function TTBXXitoTheme.GetPopupShadowType: Integer;
begin
  Result := PST_WINDOWS2K;
end;

constructor TTBXXitoTheme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then InitializeStock;
  Inc(CounterLock);
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXXitoTheme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  Dec(CounterLock);
  if CounterLock = 0 then FinalizeStock;
  inherited;
end;

procedure TTBXXitoTheme.GetViewMargins(ViewType: Integer;
  out Margins: TTBXMargins);
begin
  Margins.LeftWidth := 0;
  Margins.TopHeight := 0;
  Margins.RightWidth := 0;
  Margins.BottomHeight := 0;
end;

procedure TTBXXitoTheme.PaintPageScrollButton(Canvas: TCanvas;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  X, Y, Sz: Integer;
begin
  R := ARect;
  InflateRect(R, -1, -1);
  if Hot then
  begin
    FillGradient(Canvas, R, $00f0f0f0, clWhite, TGTopBottom);
    Canvas.Pen.Color := BtnItemColors[bisHot, ipFrame];
  end
  else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(R);
    Canvas.Pen.Color := $00D0D0D0;
  end;
  InflateRect(R, 1, 1);

  RoundFrame(Canvas, R, 1, 1);
  Canvas.Pen.Color := clBtnFace;
  with R do
  begin
    Canvas.Pixels[Left, Top] := clBtnFace;
    Canvas.Pixels[Right + 1, Top] := clBtnFace;
    Canvas.Pixels[Right + 1, Bottom + 1] := clBtnFace;
    Canvas.Pixels[Left, Bottom + 1] := clBtnFace;
  end;

  { Arrow }
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
  Canvas.Pen.Color := clBtnText;
  Canvas.Brush.Color := Canvas.Pen.Color;
  case ButtonType of
    PSBT_UP:
      begin
        Inc(Y, Sz div 2);
        Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
      end;
    PSBT_DOWN:
      begin
        Y := (R.Top + R.Bottom - 1) div 2;
        Dec(Y, Sz div 2);
        Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
      end;
    PSBT_LEFT:
      begin
        Inc(X, Sz div 2);
        Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
      end;
    PSBT_RIGHT:
      begin
        X := (R.Left + R.Right - 1) div 2;
        Dec(X, Sz div 2);
        Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
      end;
  end;
end;

procedure TTBXXitoTheme.PaintFrameControl(Canvas: TCanvas; R: TRect; Kind, State: Integer; Params: Pointer);
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..1024] of TRGBTriple;
  TGradientColors = array[0..255] of TRGBTriple;
const
  Offs: array[0..10] of integer = (3, 1, 1, 0, 0, 0, 0, 0, 1, 1, 3);
var
  X, Y: Integer;

  rc1, gc1, bc1, rc2, gc2, bc2, rc3, gc3, bc3,
  y1, i, GSize: Integer;
  Row: PRGBTripleArray;
  GradCol: TRGBTriple;
  Brush: HBrush;

  procedure RadioGradient(const Canvas: TCanvas; const ARect: TRect;
    const StartColor, EndColor: TColor;
    const NewG: boolean);
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

    if rc3 < 0 then rc3 := 0 else if rc3 > 255 then rc3 := 255;
    if gc3 < 0 then gc3 := 0 else if gc3 > 255 then gc3 := 255;
    if bc3 < 0 then bc3 := 0 else if bc3 > 255 then bc3 := 255;

    if NewG then
    begin
      GradientBmp.Width := 1;
      GradientBmp.Height := (ARect.Bottom - ARect.Top) - 1;
      GSize := GradientBmp.Height;

      y1 := GSize div 2;
      for i := 0 to y1 - 1 do
      begin
        Brush := CreateSolidBrush(
        RGB(Byte(rc2 + (((rc1 - rc2) * i) div y1)),
            Byte(gc2 + (((gc1 - gc2) * i) div y1)),
            Byte(bc2 + (((bc1 - bc2) * i) div y1))));
        Windows.FillRect(Canvas.Handle, Rect(ARect.Left + Offs[i],  ARect.Top + i, ARect.Right - Offs[i], ARect.Top + i + 1), Brush);
        DeleteObject(Brush);
      end;
      if rc1 > rc2 then
      begin
        rc3 := rc1;
        gc3 := gc1;
        bc3 := bc1;
      end;
      for i := y1 to GSize do
      begin
        Brush := CreateSolidBrush(
        RGB(Byte(rc3 + (((rc1 - rc3) * i) div GSize)),
            Byte(gc3 + (((gc1 - gc3) * i) div GSize)),
            Byte(bc3 + (((bc1 - bc3) * i) div GSize))));
        Windows.FillRect(Canvas.Handle, Rect(ARect.Left + Offs[i], ARect.Top + i, ARect.Right - Offs[i], ARect.Top + i + 1), Brush);
        DeleteObject(Brush);
      end;
    end else
    begin
      GSize := (ARect.Bottom - ARect.Top) - 1;
      for i := 0 to GSize do
      begin
        Brush := CreateSolidBrush(
        RGB(Byte(rc1 + (((rc2 - rc1) * i) div GSize)),
            Byte(gc1 + (((gc2 - gc1) * i) div GSize)),
            Byte(bc1 + (((bc2 - bc1) * i) div GSize))));
        Windows.FillRect(Canvas.Handle, Rect(ARect.Left + Offs[i], ARect.Bottom - i - 1, ARect.Right - Offs[i], ARect.Bottom - i), Brush);
        DeleteObject(Brush);
      end;
    end;
  end;

  function TextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := BtnItemColors[bisDisabled, ipText]
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipText]
    else if Boolean(State and PFS_MIXED) then Result := clBtnShadow
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipText]
    else Result := BtnItemColors[bisNormal, ipText];
  end;

  procedure DiagLine(C: TColor);
  begin
    with Canvas, R do
    begin
      Pen.Color := C;
      MoveTo(Right - 1 - X, Bottom - 1); LineTo(Right, Bottom - X - 2);
      Inc(X);
    end;
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
              if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
                FillGradient(Canvas, R, clWhite, clSilver, TGTopBottom)
              else
                FillGradient(Canvas, R, clWhite, clSilver, TGTopBottom);
              Pen.Color := clSilver;
            end else
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              FillGradient(Canvas, R, clSilver, clWhite, TGTopBottom);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end else
            begin
              FillGradient(Canvas, R, $00f0f0f0, clWhite, TGTopBottom);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end;
            InflateRect(R, 1, 1);
            RoundFrame(Canvas, R, 1, 1);
          end else
          begin
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              InflateRect(R, -1, -1);
              FillGradient(Canvas, R, $00d0d0d0, clWhite, TGTopBottom);
              Pen.Color := clSilver;
              InflateRect(R, 1, 1);
            end else
              Pen.Color := clSilver;
            RoundFrame(Canvas, R, 1, 1);
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
              Point(X + 4, Y - 4), Point(X, Y), Point(X - 2, Y - 2), Point(X - 2, Y)]);
          end;
        end;

      PFC_RADIOBUTTON:
        begin
          if Boolean(State and PFS_HOT) then
          begin
            InflateRect(R, -1, -1);
            if Boolean(State and PFS_PUSHED) then
            begin
              if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
                RadioGradient(Canvas, R, clWhite, clSilver, false)
              else
                RadioGradient(Canvas, R, clWhite, clSilver, true);
              Pen.Color := clSilver;
            end else
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              RadioGradient(Canvas, R, clSilver, clWhite, true);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end else
            begin
              RadioGradient(Canvas, R, $00f0f0f0, clWhite, true);
              Pen.Color := BtnItemColors[bisHot, ipFrame];
            end;
            InflateRect(R, 1, 1);
            Brush.Style:= bsClear;
            X:= R.Left;
            Y:= R.Top;
            Polygon([Point(X, Y + 8),      Point(X, Y + 4),      Point(X + 1, Y + 3),
                     Point(X + 1, Y + 2),  Point(X + 2, Y + 1),  Point(X + 3, Y + 1),
                     Point(X + 4, Y),      Point(X + 8, Y),      Point(X + 9, Y + 1),
                     Point(X + 10, Y + 1),  Point(X + 11, Y + 2),  Point(X + 11, Y + 3),
                     Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
                     Point(X + 11, Y + 10),  Point(X + 10, Y + 11),  Point(X + 9, Y + 11),
                     Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
                     Point(X + 2, Y + 11),  Point(X + 1, Y + 10),  Point(X + 1, Y + 9)]);
          end else
          begin
            if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
            begin
              InflateRect(R, -1, -1);
              RadioGradient(Canvas, R, $00d0d0d0, clWhite, true);
              Pen.Color := clSilver;
              InflateRect(R, 1, 1);
            end else
              Pen.Color := clSilver;

            Brush.Style:= bsClear;
            X:= R.Left;
            Y:= R.Top;
            Polygon([Point(X, Y + 8),      Point(X, Y + 4),      Point(X + 1, Y + 3),
                     Point(X + 1, Y + 2),  Point(X + 2, Y + 1),  Point(X + 3, Y + 1),
                     Point(X + 4, Y),      Point(X + 8, Y),      Point(X + 9, Y + 1),
                     Point(X + 10, Y + 1),  Point(X + 11, Y + 2),  Point(X + 11, Y + 3),
                     Point(X + 12, Y + 4), Point(X + 12, Y + 8), Point(X + 11, Y + 9),
                     Point(X + 11, Y + 10),  Point(X + 10, Y + 11),  Point(X + 9, Y + 11),
                     Point(X + 8, Y + 12), Point(X + 4, Y + 12), Point(X + 3, Y + 11),
                     Point(X + 2, Y + 11),  Point(X + 1, Y + 10),  Point(X + 1, Y + 9)]);
          end;
          InflateRect(R, -1, -1);
          Pen.Style := psSolid;
          Brush.Style := bsClear;
          if Boolean(State and PFS_CHECKED) then
          begin
            InflateRect(R, -3, -3);
            Pen.Color := TextColor;
            Brush.Color := Pen.Color;
            with R do Ellipse(Left, Top, Right, Bottom);
          end;
        end;
    end;
end;

procedure TTBXXitoTheme.PaintStatusBar(Canvas: TCanvas; R: TRect; Part: Integer);
var
  Hi, Lo: TColor;
  D, Sz, i: Integer;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(Canvas.Handle, Right - 1 - D, Bottom - 1, Right, Bottom - D - 2, C);
    Inc(D);
  end;

begin
  with Canvas do
    case Part of
      SBP_BODY:
        begin
          FillGradient(Canvas, R, $00f0f0f0, clWhite, TGTopBottom);
        end;
      SBP_PANE, SBP_LASTPANE:
        begin
          if Part = SBP_PANE then Dec(R.Right, 3);
          DrawLineEx(Canvas.Handle, R.Right, R.Top + 4, R.Right, R.Bottom - 3, clBtnShadow);
          DrawLineEx(Canvas.Handle, R.Right + 1, R.Top + 5, R.Right + 1, R.Bottom - 2, clBtnHighLight);
        end;
      SBP_GRIPPER:
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

procedure TTBXXitoTheme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

{$IFNDEF DTM_Package}
initialization
  RegisterTBXTheme('Xito', TTBXXitoTheme);
{$ENDIF}
end.

