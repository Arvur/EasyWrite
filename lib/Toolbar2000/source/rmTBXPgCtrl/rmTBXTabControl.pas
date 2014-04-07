unit rmTBXTabControl;

interface

{$I Compilerdefines.inc}

uses Messages, windows, sysutils, Controls, classes,
	{$ifdef d6_OR_HIGHER}Types,{$endif}
	Graphics, ComCtrls, commctrl, extctrls, TBX, TBXThemes, rmTBXStdFunc;

type
  TrmTBXTabControl = class(TTabControl)
  private
    fEdgeLight: TColor;
    fEdge3dLight: TColor;
    fEdgeDark: TColor;
    fEdge3dDark: TColor;
    fBufferBMP : TBitmap;
    fGradientTabColor: TColor;
    fUseFancyTabs: boolean;

    procedure wmEraseBkgrnd(var Message:TWMEraseBkgnd); message wm_erasebkgnd;
    procedure wmPaint(var message : TWMPaint); message wm_Paint;

    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;

    procedure SetEdge3DDark(const Value: TColor);
    procedure SetEdge3DLight(const Value: TColor);
    procedure SetEdgeDark(const Value: TColor);
    procedure SetEdgeLight(const Value: TColor);
    procedure SetGradientTabColor(const Value: TColor);
    procedure SetUseFancyTabs(const Value: boolean);

  protected
    function GetTabRect(index: integer): TRect;
    function GetFocusedTab:integer;
    procedure CustomPaint; virtual;
    Procedure DrawStandardTopTabs(wC:TCanvas); virtual;
    procedure DrawStandardBottomTabs(wc:TCanvas); virtual;
    procedure DrawStandardLeftTabs(wC: TCanvas); virtual;
    procedure DrawStandardRightTabs(wC: TCanvas); virtual;

    procedure DrawStandardButtons(wC:TCanvas); virtual;
    procedure DrawFlatButtons(wC:TCanvas); virtual;

    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property OwnerDraw default True;
    property EdgeColor3DLight:TColor read fEdge3dLight write SetEdge3DLight default cl3dLight;
    property EdgeColor3dDark:TColor read fEdge3dDark write SetEdge3DDark default cl3dDkShadow;
    property EdgeColorLight:TColor read fEdgeLight write SetEdgeLight default clBtnHighlight;
    property EdgeColorDark:TColor read fEdgeDark write SetEdgeDark default clBtnShadow;
    property UseFancyTabs : boolean read fUseFancyTabs write SetUseFancyTabs default false;
    property GradientTabColor : TColor read fGradientTabColor write SetGradientTabColor default clNone;
  end;

implementation

function TrmTBXTabControl.GetTabRect(index:integer):TRect;
var
   wRect : TRect;
begin
   if sendmessage(self.handle, TCM_GetItemRect, index, integer(@wRect)) > 0 then
      result := wRect
   else
      wRect := Rect(0,0,0,0);
end;

procedure TrmTBXTabControl.DrawStandardLeftTabs(wC:TCanvas);
var
  wRect : TRect;
  loop : integer;
  wStr : String;
  wColor : TColor;
  wItemInfo : TTBXItemInfo;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     fillchar(wItemInfo, sizeof(wItemInfo), 0);
     wItemInfo.ViewType := VT_TOOLBAR;
     wItemInfo.ItemOptions := IO_TOOLBARSTYLE;
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];

            if fUseFancyTabs then
            begin
               if (fGradientTabColor <> clNone) then
                  wColor := fGradientTabColor
               else
                  wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
               inflaterect(wRect, -1, -1);
               GradientFill(wc, darkencolor(wColor, 20), LightenColor(wColor, 20), wRect, gfLinear);
               inflaterect(wRect, 1, 1);
            end;

            if ptinrect(wRect, screentoclient(Mouse.CursorPos)) and HotTrack then
            begin
               wc.brush.color := clHotlight;
               wc.fillrect(Rect(wrect.Left+1, wrect.Top+1, wrect.Right-1, wrect.Top+4));
            end;

            Inflaterect(wRect, 2, 0);
            wRect.Left := wRect.Left+2;
            wRect.Bottom := wRect.Bottom-1;
            TabsDrawLeftTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
            wRect.Bottom := wRect.Bottom+1;
            wRect.Left := wRect.Left-2;
            Inflaterect(wRect, -2, 0);

            offsetrect(wRect, 0, -1);
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, true, loop, Images);
            offsetrect(wRect, 0, 1);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];

        if fUseFancyTabs then
        begin
           if (fGradientTabColor <> clNone) then
              wColor := fGradientTabColor
           else
              wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
           GradientFill(wc, lightencolor(wColor, 20), wColor, wRect, gfLinear);
        end;

        Inflaterect(wRect, 2, 2);
        wRect.Bottom := wRect.Bottom-1;
        TabsDrawLeftTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
        wRect.Bottom := wRect.Bottom+1;
        Inflaterect(wRect, -2, 0);

        offsetrect(wRect, -2, -1);
        wc.Font.assign(Font);
        TabsDrawText(wc, wRect, wstr, False, true, True, TabIndex, Images);
        offsetrect(wRect, 2, 1);

        Inflaterect(wRect, 0, -2);
        TabsDrawLeftBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end
     else
     begin
        wRect.left := 0;
        wRect.Right := 0;
        TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end;
  end
  else
  begin
     wRect := rect(0,0,0,0);
     TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
  end;
end;

procedure TrmTBXTabControl.DrawStandardTopTabs(wC:TCanvas);
var
  wRect : TRect;
  loop : integer;
  wStr : String;
  wColor : TColor;
  wItemInfo : TTBXItemInfo;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     fillchar(wItemInfo, sizeof(wItemInfo), 0);
     wItemInfo.ViewType := VT_TOOLBAR;
     wItemInfo.ItemOptions := IO_TOOLBARSTYLE;
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];

            if fUseFancyTabs then
            begin
               if (fGradientTabColor <> clNone) then
                  wColor := fGradientTabColor
               else
                  wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
               inflaterect(wRect, -1, -1);
               GradientFill(wc, darkencolor(wColor, 20), LightenColor(wColor, 20), wRect, gfHLinear);
               inflaterect(wRect, 1, 1);
            end;

            if ptinrect(wRect, screentoclient(Mouse.CursorPos)) and HotTrack then
            begin
               wc.brush.color := clHotlight;
               wc.fillrect(Rect(wrect.Left+1, wrect.Top+1, wrect.Right-1, wrect.Top+4));
            end;

            Inflaterect(wRect, 0, 2);
            wRect.Top := wRect.Top+2;
            wRect.Right := wRect.Right-1;
            TabsDrawTopTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
            wRect.Right := wRect.Right+1;
            wRect.Top := wRect.Top-2;
            Inflaterect(wRect, 0, -2);

            offsetrect(wRect, -1, 0);
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, false, loop, Images);
            offsetrect(wRect, 1, 0);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];

        if fUseFancyTabs then
        begin
           if (fGradientTabColor <> clNone) then
              wColor := fGradientTabColor
           else
              wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
           GradientFill(wc, lightencolor(wColor, 20), wColor, wRect, gfHLinear);
        end;

        Inflaterect(wRect, 2, 2);
        wRect.Right := wRect.Right-1;
        TabsDrawTopTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
        wRect.Right := wRect.Right+1;
        Inflaterect(wRect, 0, -2);

        offsetrect(wRect, -1, -2);
        wc.Font.assign(Font);
        TabsDrawText(wc, wRect, wstr, False, true, false, TabIndex, Images);
        offsetrect(wRect, 1, 2);

        Inflaterect(wRect, -2, 0);
        TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end
     else
     begin
        wRect.left := 0;
        wRect.Right := 0;
        TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end;
  end
  else
  begin
     wRect := rect(0,0,0,0);
     TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
  end;
end;

procedure TrmTBXTabControl.CustomPaint;
var
   wRect1 : TRect;
   ItemInfo : TTBXItemInfo;
begin
   fBufferBMP.Height := Height;
   fBufferBMP.Width := Width;
   FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
   with ItemInfo do
   begin
     ViewType    := TVT_NORMALTOOLBAR;
     ItemOptions := IO_APPACTIVE or IO_TOOLBARSTYLE or IO_DESIGNING;
   end;

   wRect1 := rect(0, 0, width, height);
   CurrentTheme.PaintBackgnd(fBufferBMP.Canvas, wRect1, wRect1, wRect1, CurrentTheme.GetViewColor(VT_TOOLBAR), False, TVT_NORMALTOOLBAR);

   case Style of
     tsTabs:
       begin
          case TabPosition of
            tpTop : DrawStandardTopTabs(fBufferBMP.Canvas);
            tpLeft : DrawStandardLeftTabs(fBufferBMP.Canvas);
            tpBottom : DrawStandardBottomTabs(fBufferBMP.Canvas);
            tpRight : DrawStandardRightTabs(fBufferBMP.Canvas);
          else
            raise Exception.Create('Tab position is not supported');
          end;
       end;
     tsButtons: DrawStandardButtons(fBufferBMP.Canvas);
     tsFlatButtons: DrawFlatButtons(fBufferBMP.Canvas);
   else
      raise Exception.Create('Tab style is not supported');
   end;

   if focused and (GetFocusedTab > -1) then
   begin
     wRect1 := GetTabRect(GetFocusedTab);
     InflateRect(wRect1, -2, -1);
     fBufferBMP.Canvas.DrawFocusRect(wRect1);
   end;

   canvas.Draw(0,0, fBufferBMP);
end;

procedure TrmTBXTabControl.wmEraseBkgrnd(var Message: TWMEraseBkgnd);
begin
   Message.Result := 1;
end;

procedure TrmTBXTabControl.wmPaint(var message: TWMPaint);
begin
   inherited;
   CustomPaint;
   message.Result := 1;
end;

procedure TrmTBXTabControl.SetEdge3DDark(const Value: TColor);
begin
  if fEdge3dDark <> Value then
  begin
     fEdge3dDark := Value;
     invalidate;
  end;
end;

procedure TrmTBXTabControl.SetEdge3DLight(const Value: TColor);
begin
  if fEdge3dLight <> Value then
  begin
     fEdge3dLight := Value;
     invalidate;
  end;
end;

procedure TrmTBXTabControl.SetEdgeDark(const Value: TColor);
begin
  if fEdgeDark <> Value then
  begin
     fEdgeDark := Value;
     invalidate;
  end;
end;

procedure TrmTBXTabControl.SetEdgeLight(const Value: TColor);
begin
  if fEdgeLight <> Value then
  begin
     fEdgeLight := Value;
     invalidate;
  end;
end;

constructor TrmTBXTabControl.Create(AOwner: TComponent);
begin
  inherited;
  OwnerDraw := true; //Helps to reduce the redrawing flicker on XP and Win2k
  fBufferBMP := TBitmap.create;
  fEdgeLight := clBtnHighlight;
  fEdge3dLight := cl3DLight;
  fEdgeDark := clBtnShadow;
  fEdge3dDark := cl3DDkShadow;
  fGradientTabColor := clNone;
  fUseFancyTabs := false;
  Color := clBtnFace;

  //TBX Stuff
  AddThemeNotification(Self);
end;

destructor TrmTBXTabControl.Destroy;
begin
  RemoveThemeNotification(Self);
  fBufferBMP.Free;
  inherited;
end;

procedure TrmTBXTabControl.DrawStandardButtons;
var
  wRect : TRect;
  loop : integer;
  wStr : String;

  procedure FillBackground(wRect:TRect; Pushed:boolean);
  var
     wColor : TColor;
     wItemInfo : TTBXItemInfo;
  begin
     if UseFancyTabs then
     begin

        fillchar(wItemInfo, sizeof(wItemInfo), 0);
        wItemInfo.ViewType := VT_TOOLBAR;
        wItemInfo.ItemOptions := IO_TOOLBARSTYLE;

        if GradientTabColor <> clNone then
           wColor := GradientTabColor
        else
           wColor := CurrentTheme.GetItemColor(wITemInfo);

        if Pushed then
           GradientFill(wc, wColor, LightenColor(wColor, 20), wRect, gfRectangle);
     end;
  end;

  procedure DrawUnselected(Rect: TRect);
  var
     wRect : TRect;
  begin
     wRect := Rect;
     wc.Pen.Width := 1;

     if fEdge3dLight <> clNone then
     begin
        wc.Pen.Color := DarkenColor(fEdge3dLight, 20);
        wc.PolyLine([point(rect.Left, rect.Bottom-2),
                     point(rect.Left, rect.Top+2),
                     point(rect.Left+2, rect.top),
                     point(rect.Right-2, rect.Top),
                     point(rect.right, rect.top+2)]);
     end;

     if fEdgeLight <> clNone then
     begin
        wc.Pen.Color := fEdgeLight;
        wc.PolyLine([point(rect.Left+1, rect.Bottom-2),
                     point(rect.Left+1, rect.Top+2),
                     point(rect.Left+2, rect.top+1),
                     point(rect.Right-2, rect.Top+1),
                     point(rect.right-1, rect.top+2)]);
     end;

     if fEdge3dDark <> clNone then
     begin
        wc.Pen.color := fEdge3dDark;
        wc.PolyLine([point(rect.Left+1, rect.Bottom-1),
                     point(rect.Left+2, rect.Bottom),
                     point(rect.Right-2, rect.bottom),
                     point(rect.right, rect.bottom-2),
                     point(rect.Right, rect.top+1)]);
     end;

     if fEdgeDark <> clNone then
     begin
        wc.Pen.Color := fEdgeDark;
        wc.PolyLine([point(rect.Left+2, rect.Bottom-1),
                     point(rect.Right-2, rect.bottom-1),
                     point(rect.right-1, rect.bottom-2),
                     point(rect.Right-1, rect.top+1)]);
     end;
  end;

  procedure DrawSelected(Rect: TRect);
  var
     wRect : TRect;
  begin
     wRect := Rect;
     wc.Pen.Width := 1;

     if fEdgeDark <> clNone then
     begin
        wc.Pen.Color := fEdgeDark;
        wc.PolyLine([point(rect.Left, rect.Bottom-2),
                     point(rect.Left, rect.Top+2),
                     point(rect.Left+2, rect.top),
                     point(rect.Right-2, rect.Top),
                     point(rect.right, rect.top+2)]);
     end;

     if fEdge3dDark <> clNone then
     begin
        wc.Pen.color := fEdge3dDark;
        wc.PolyLine([point(rect.Left+1, rect.Bottom-2),
                     point(rect.Left+1, rect.Top+2),
                     point(rect.Left+2, rect.top+1),
                     point(rect.Right-2, rect.Top+1),
                     point(rect.right-1, rect.top+2)]);
     end;

     if fEdgeLight <> clNone then
     begin
        wc.Pen.Color := fEdgeLight;
        wc.PolyLine([point(rect.Left+1, rect.Bottom-1),
                     point(rect.Left+2, rect.Bottom),
                     point(rect.Right-2, rect.bottom),
                     point(rect.right, rect.bottom-2),
                     point(rect.Right, rect.top+1)]);
     end;

     if fEdge3dLight <> clNone then
     begin
        wc.Pen.Color := DarkenColor(fEdge3dLight, 20);
        wc.PolyLine([point(rect.Left+2, rect.Bottom-1),
                     point(rect.Right-2, rect.bottom-1),
                     point(rect.right-1, rect.bottom-2),
                     point(rect.Right-1, rect.top+1)]);
     end;
  end;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     wRect := GetTabRect(0);
     wC.Brush.style := bsClear;
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];

            FillBackground(wRect, false);

            DrawUnselected(wRect);
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, false, loop, Images, false);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];
        FillBackground(wRect, true);
        DrawSelected(wRect);
        wc.Font.assign(Font);
        offsetrect(wrect, 1, 1);
        TabsDrawText(wc, wRect, wstr, true, true, false, TabIndex, Images, false);
     end;
  end;
end;

procedure TrmTBXTabControl.DrawFlatButtons;
var
  wRect : TRect;
  loop : integer;
  wStr : String;

  procedure DrawSeperator(Rect: TRect);
  begin
     wc.pen.Color := fEdgeDark;
     wc.Polyline([point(rect.Right + 3, rect.top),
                  point(rect.Right +3, rect.Bottom)]);

     wc.pen.Color := fEdgeLight;
     wc.Polyline([point(rect.Right + 4, rect.top),
                  point(rect.Right +4, rect.Bottom)]);
  end;

  procedure DrawUnselected(Rect: TRect; IsFirst, PrevUnSelected:boolean);
  begin
     //Do nothing...

     DrawSeperator(rect);
  end;

  procedure DrawSelected(Rect: TRect);
  var
     wRect : TRect;
  begin
     wRect := Rect;
     frame3d(wc, wrect, fEdgeDark, fEdgeLight, 1);
     frame3d(wc, wrect, fEdge3DDark, fEdge3dLight, 1);

     DrawSeperator(rect);
  end;

  procedure FillBackground(wRect:TRect; Pushed:boolean);
  var
     wColor : TColor;
     wItemInfo : TTBXItemInfo;
  begin
     if UseFancyTabs then
     begin

        fillchar(wItemInfo, sizeof(wItemInfo), 0);
        wItemInfo.ViewType := VT_TOOLBAR;
        wItemInfo.ItemOptions := IO_TOOLBARSTYLE;

        if GradientTabColor <> clNone then
           wColor := GradientTabColor
        else
           wColor := CurrentTheme.GetItemColor(wITemInfo);

        GradientFill(wc, wColor, LightenColor(wColor, 20), wRect, gfRectangle)
     end;
  end;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     wRect := GetTabRect(0);
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];
            DrawUnselected(wRect, (loop = 0), (loop-1 <> tabindex));
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, false, loop, Images, false);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];
        FillBackground(wRect, true);
        DrawSelected(wRect);
        wc.Font.assign(Font);
        offsetrect(wrect, 1, 1);
        TabsDrawText(wc, wRect, wstr, true, true, false, TabIndex, Images, false);
     end;
  end;
end;

procedure TrmTBXTabControl.TBMThemeChange(var Message: TMessage);
begin
   if Message.WParam = TSC_VIEWCHANGE then
   begin
      Color := CurrentTheme.GetViewColor(VT_TOOLBAR);
      invalidate;
   end;
end;

procedure TrmTBXTabControl.SetGradientTabColor(const Value: TColor);
begin
  if fGradientTabColor <> Value then
  begin
     fGradientTabColor := Value;
     invalidate;
  end;
end;

procedure TrmTBXTabControl.SetUseFancyTabs(const Value: boolean);
begin
  if fUseFancyTabs <> Value then
  begin
     fUseFancyTabs := Value;
     invalidate;
  end;
end;

function TrmTBXTabControl.GetFocusedTab: integer;
begin
   result := sendmessage(self.handle, TCM_GetCurFocus, 0, 0);
end;

procedure TrmTBXTabControl.CMMouseleave(var Message: TMessage);
begin
   inherited;
   if HotTrack then
      invalidate;
end;


procedure TrmTBXTabControl.DrawStandardBottomTabs(wc: TCanvas);
var
  wRect : TRect;
  loop : integer;
  wStr : String;
  wColor : TColor;
  wItemInfo : TTBXItemInfo;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     fillchar(wItemInfo, sizeof(wItemInfo), 0);
     wItemInfo.ViewType := VT_TOOLBAR;
     wItemInfo.ItemOptions := IO_TOOLBARSTYLE;
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];

            if fUseFancyTabs then
            begin
               if (fGradientTabColor <> clNone) then
                  wColor := fGradientTabColor
               else
                  wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
               inflaterect(wRect, -1, 0);
               GradientFill(wc, DarkenColor(wColor, 20), LightenColor(wColor, 20), wRect, gfHLinear);
               inflaterect(wRect, 1, 0);
            end;

            if HotTrack and ptinrect(wRect, screentoclient(Mouse.CursorPos)) then
            begin
               wc.brush.color := clHotlight;
               wc.fillrect(Rect(wrect.Left+1, wrect.Bottom-4, wrect.Right-1, wrect.bottom-1));
            end;

            Inflaterect(wRect, 0, 3);
            wRect.bottom := wRect.bottom-3;
            wRect.Right := wRect.Right-1;
            TabsDrawBottomTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
            wRect.Right := wRect.Right+1;
            wRect.bottom := wRect.bottom+3;
            Inflaterect(wRect, 0, -3);

            offsetrect(wRect, -1, -1);
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, false, loop, Images);
            offsetrect(wRect, 1, 1);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];

        if fUseFancyTabs then
        begin
           if (fGradientTabColor <> clNone) then
              wColor := fGradientTabColor
           else
              wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
           GradientFill(wc, wColor, DarkenColor(wColor, 20), wRect, gfHLinear);
        end;

        Inflaterect(wRect, 2, 3);
        wRect.Right := wRect.Right-1;
        wRect.Bottom := wRect.Bottom-1;
        TabsDrawBottomTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
        wRect.Right := wRect.Right+1;
        wRect.Bottom := wRect.Bottom+1;
        Inflaterect(wRect, 0, -3);

        offsetrect(wRect, -1, 1);
        wc.Font.assign(Font);
        TabsDrawText(wc, wRect, wstr, False, true, false, TabIndex, Images);
        offsetrect(wRect, 1, -1);

        Inflaterect(wRect, -2, 1);
        TabsDrawBottomBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end
     else
     begin
        wRect := GetTabRect(TabIndex);
        wRect.left := 0;
        wRect.Right := 0;
        TabsDrawbottomBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end;
  end
  else
  begin
     wRect := rect(0,0,0,0);
     TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
  end;
end;

procedure TrmTBXTabControl.DrawStandardRightTabs(wC: TCanvas);
var
  wRect : TRect;
  loop : integer;
  wStr : String;
  wColor : TColor;
  wItemInfo : TTBXItemInfo;

begin
  if not HandleAllocated then Exit;

  if self.Tabs.Count > 0 then
  begin
     fillchar(wItemInfo, sizeof(wItemInfo), 0);
     wItemInfo.ViewType := VT_TOOLBAR;
     wItemInfo.ItemOptions := IO_TOOLBARSTYLE;
     for loop := 0 to Tabs.Count-1 do
     begin
         if not (loop = TabIndex) then
         begin
            wRect := GetTabRect(loop);
            wStr := Tabs[loop];

            if fUseFancyTabs then
            begin
               if (fGradientTabColor <> clNone) then
                  wColor := fGradientTabColor
               else
                  wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
               inflaterect(wRect, 0, -1);
               wRect.top := wRect.top + 1;
               GradientFill(wc, darkencolor(wColor, 20), LightenColor(wColor, 20), wRect, gfLinear);
               wRect.top := wRect.top - 1;
               inflaterect(wRect, 0, 1);
            end;

            if ptinrect(wRect, screentoclient(Mouse.CursorPos)) and HotTrack then
            begin
               wc.brush.color := clHotlight;
               wc.fillrect(Rect(wrect.Left+1, wrect.Top+1, wrect.Right-1, wrect.Top+4));
            end;

            wRect.Right := wRect.Right-1;
            TabsDrawRightTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
            wRect.Right := wRect.Right+1;

            offsetrect(wRect, -1, -1);
            wc.Font.assign(Font);
            TabsDrawText(wc, wRect, wstr, False, true, true, loop, Images);
            offsetrect(wRect, 1, 1);
         end;
     end;
     if TabIndex > -1 then
     begin
        wRect := GetTabRect(TabIndex);
        wStr := Tabs[TabIndex];

        if fUseFancyTabs then
        begin
           if (fGradientTabColor <> clNone) then
              wColor := fGradientTabColor
           else
              wColor := CurrentTheme.GetViewColor(VT_TOOLBAR);
           GradientFill(wc, wColor, DarkenColor(wColor, 20), wRect, gfLinear);
        end;

        Inflaterect(wRect, 2, 2);
        wRect.Right := wRect.Right-1;
        wRect.bottom := wRect.bottom-1;
        TabsDrawRightTabBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark);
        wRect.Right := wRect.Right+1;
        wRect.bottom := wRect.bottom+1;
        Inflaterect(wRect, 0, -2);

        offsetrect(wRect, 1, -1);
        wc.Font.assign(Font);
        TabsDrawText(wc, wRect, wstr, False, true, True, TabIndex, Images);
        offsetrect(wRect, -1, 1);

        wRect.Left := wRect.Left+2;
        wRect.top := wRect.top-2;
        TabsDrawRightBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end
     else
     begin
        wRect.left := 0;
        wRect.Right := 0;
        TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
     end;
  end
  else
  begin
     wRect := rect(0,0,0,0);
     TabsDrawTopBorder(wc, wRect, fEdge3dLight, fEdgeLight, fEdge3dDark, fEdgeDark, width, height);
  end;
end;

end.
