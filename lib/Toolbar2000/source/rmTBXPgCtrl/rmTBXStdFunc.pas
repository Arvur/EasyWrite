unit rmTBXStdFunc;

interface

{$I compilerdefines.inc}

uses {$ifdef d6_or_higher}
       Types
     {$else}
       classes
     {$endif},
     Windows, Graphics, ImgList, Controls;

type
   TGradientFill = (gfLinear, gfRectangle, gfRoundRect, gfOval, gfRadial, gfHLinear);

function RectWidth(Rect:TRect):integer;
function RectHeight(Rect:TRect):integer;

function LightenColor(Color:TColor; Percentage:integer):TColor;
function DarkenColor(Color:TColor; Percentage:integer):TColor;
procedure GradientFill(Canvas: TCanvas; FBeginColor, FEndColor: TColor; R: TRect; FillStyle:TGradientFill = gfLinear);

procedure TabsDrawRightBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
procedure TabsDrawLeftBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
procedure TabsDrawTopBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
procedure TabsDrawBottomBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);

procedure TabsDrawRightTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
procedure TabsDrawLeftTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
procedure TabsDrawTopTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
procedure TabsDrawBottomTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);

procedure TabsDrawText(aCanvas:TCanvas; aRect:TRect; aData:String; aPushed, aEnabled:boolean; Vertical:boolean; ImageIndex:integer; ImageList:TCustomImageList; aTabs:boolean = true);

implementation

uses TBX, TBXThemes;

function RectWidth(Rect:TRect):integer;
begin
   result := rect.Right-rect.Left;
end;

function RectHeight(Rect:TRect):Integer;
begin
   result := rect.bottom-rect.top;
end;

function LessThanInt(x1, x2: integer) : integer;
begin
   if x1 < x2 then
      result := x1
   else
      result := x2;
end;

function LightenColor(Color:TColor; Percentage:integer):TColor;
var
   wRGB, wR, wG, wB : longint;
begin
   wRGB := ColorToRGB(Color);
   wR := LessThanInt(round(GetRValue(wRGB) * (1+(percentage / 100))), 255);
   wG := LessThanInt(round(GetGValue(wRGB) * (1+(percentage / 100))), 255);
   wB := LessThanInt(round(GetBValue(wRGB) * (1+(percentage / 100))), 255);
   result := RGB(wR, wG, wB);
end;

function DarkenColor(Color:TColor; Percentage:integer):TColor;
var
   wRGB, wR, wG, wB : longint;
begin
   wRGB := ColorToRGB(Color);
   wR := round(GetRValue(wRGB) / (1+(percentage / 100)));
   wG := round(GetGValue(wRGB) / (1+(percentage / 100)));
   wB := round(GetBValue(wRGB) / (1+(percentage / 100)));
   result := RGB(wR, wG, wB);
end;

procedure GradientFill(Canvas: TCanvas; FBeginColor, FEndColor: TColor; R: TRect; FillStyle:TGradientFill = gfLinear);
const
   FNumColors = $FF; //8-bit

type
   pTRGBArray = ^TRGBArray;
   TRGBArray = array[0..0] of TRGBTriple; {This syntax is as bad as C}

var
  { Set up working variables }
   wBeginRGBValue: array[0..2] of Byte; { Begin RGB values }
   wRGBDifference: array[0..2] of integer; { Difference between begin and end }
                                           { RGB values                       }
   wColorBand: TRect; { Color band rectangular coordinates }
   wIndex: Integer; { Color band index }
   wRed: Byte; { Color band Red value }
   wGreen: Byte; { Color band Green value }
   wBlue: Byte; { Color band Blue value }
   wBrush, wOldBrush: HBrush;
   wPen, wOldPen: HPen;
   wWidth, wHeight : integer;
   wSteps : integer;
   wAngle : Double;
   wLastx, wLasty : integer;
   wIncAngle : double;
begin
  { Extract the begin RGB values }
  { Set the Red, Green and Blue colors }
   wBeginRGBValue[0] := GetRValue(ColorToRGB(FBeginColor) ) ;
   wBeginRGBValue[1] := GetGValue(ColorToRGB(FBeginColor) ) ;
   wBeginRGBValue[2] := GetBValue(ColorToRGB(FBeginColor) ) ;
  { Calculate the difference between begin and end RGB values }
   wRGBDifference[0] := GetRValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[0];
   wRGBDifference[1] := GetGValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[1];
   wRGBDifference[2] := GetBValue(ColorToRGB(FEndColor) ) - wBeginRGBValue[2];

  wSteps := 0;
  wHeight := 0;
  wWidth := 0;
  wLastx := 0;
  wLasty := 0;

  case FillStyle of
    gfLinear :
       begin
          { Calculate the color band's top and bottom coordinates }
          { for Left To Right fills }
           wColorBand.Top := R.Top;
           wColorBand.Bottom := R.Bottom;
           wColorBand.Left := R.Left;
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wSteps := fNumColors;
       end;
    gfHLinear :
       begin
          { Calculate the color band's top and bottom coordinates }
          { for Left To Right fills }
           wColorBand.Top := R.Top;
           wColorBand.Right := R.Right;
           wColorBand.Left := R.Left;
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wSteps := fNumColors;
       end;

    gfRectangle,
    gfOval,
    gfRoundRect :
       begin
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wSteps := lessthanint(wHeight div 2, wWidth div 2);
           wColorBand := R;
       end;
    gfRadial :
       begin
           wSteps := 360;
           wHeight := RectHeight(R);
           wWidth := RectWidth(R);
           wColorBand := R;
           wLastx := (wWidth shr 1);
           wLasty := (wheight shr 1);
       end;
  end;

  { Perform the fill }
   for wIndex := 0 to wSteps - 1 do
   begin { iterate through the color bands }
    { Calculate the color band's color }
      if FillStyle <> gfRadial then
      begin
         wRed := wBeginRGBValue[0] + MulDiv(wIndex, wRGBDifference[0], wSteps - 1) ;
         wGreen := wBeginRGBValue[1] + MulDiv(wIndex, wRGBDifference[1], wSteps - 1) ;
         wBlue := wBeginRGBValue[2] + MulDiv(wIndex, wRGBDifference[2], wSteps - 1) ;
      end
      else
      begin
         if wIndex < (wSteps shr 1)-1 then
         begin
            wRed := wBeginRGBValue[0] + MulDiv(wIndex, wRGBDifference[0], (wSteps shr 1) - 1) ;
            wGreen := wBeginRGBValue[1] + MulDiv(wIndex, wRGBDifference[1], (wSteps shr 1) - 1) ;
            wBlue := wBeginRGBValue[2] + MulDiv(wIndex, wRGBDifference[2], (wSteps shr 1) - 1) ;
         end
         else
         begin
            wRed := wBeginRGBValue[0] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[0], (wSteps shr 1)) ;
            wGreen := wBeginRGBValue[1] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[1], (wSteps shr 1)) ;
            wBlue := wBeginRGBValue[2] + MulDiv(((wSteps-1) - wIndex), wRGBDifference[2], (wSteps shr 1)) ;
         end;
      end;

      wBrush := CreateSolidBrush(RGB(wRed, wGreen, wBlue) ) ;
      wOldBrush := SelectObject(Canvas.handle, wBrush) ;
      try
         case FillStyle of
           gfLinear :
              begin
                 wColorBand.Right := R.Left + MulDiv(wIndex + 1, wWidth, wSteps) ;
                 PatBlt(Canvas.handle, wColorBand.Left, wColorBand.Top, wColorBand.Right - wColorBand.Left, wHeight, PATCOPY) ;
                 wColorBand.Left := wColorBand.Right;
              end;
           gfHLinear :
              begin
                 wColorBand.bottom := R.top + MulDiv(wIndex + 1, wHeight, wSteps) ;
                 PatBlt(Canvas.handle, wColorBand.Left, wColorBand.Top, wWidth, wColorBand.bottom - wColorBand.top, PATCOPY) ;
                 wColorBand.top := wColorBand.bottom;
              end;
           gfRectangle :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    Rectangle(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfRoundRect :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    RoundRect(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom, 30, 30);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfOval :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    Ellipse(Canvas.Handle, wColorBand.left, wColorBand.Top, wColorBand.right, wColorBand.Bottom);
                    InflateRect(wColorBand, -1, -1);
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
           gfRadial :
              begin
                 wPen := CreatePen(ps_Solid, 1, RGB(wRed, wGreen, wBlue)) ;
                 wOldPen := SelectObject(Canvas.handle, wPen) ;
                 try
                    if wIndex < wSteps then
                      wincangle := 0.027
                    else
                      wincangle := 0;
                    wAngle := (2 * Pi * ((wIndex+1) / wSteps));
                    pie(Canvas.handle, 0, 0, wWidth, wHeight, Round(((wWidth shr 1)) * (1-Cos(wAngle+wIncAngle))), Round(((wheight shr 1)) * (1-Sin(wAngle+wIncAngle))), wlastx, wlasty);
                    wlastx := Round(((wWidth shr 1)) * (1-Cos(wAngle)));
                    wlasty := Round(((wheight shr 1)) * (1-Sin(wAngle)));
                 finally
                    SelectObject(Canvas.handle, wOldPen) ;
                    DeleteObject(wPen) ;
                 end;
              end;
         end;
      finally
         SelectObject(Canvas.handle, wOldBrush) ;
         DeleteObject(wBrush) ;
      end;
   end; { iterate through the color bands }
end; { GradientFill }

procedure TabsDrawRightBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
begin
   if Edge3dLight <> clNone then
   begin
      wc.Pen.Color := DarkenColor(Edge3dLight, 20);
      wc.Polyline([point(0, height-1),
                   point(0, 0),
                   point(rect.left, 0)]);

   end;

   if EdgeLight <> clNone then
   begin
      wc.Pen.Color := EdgeLight;
      wc.Polyline([point(1, height-2),
                   point(1, 1),
                   point(rect.left-1, 1)]);
   end;

   if Edge3dDark <> clNone then
   begin
      wc.Pen.color := Edge3dDark;
      wc.Polyline([point(rect.left, 0),
                   point(rect.left, rect.top)]);

      wc.Polyline([point(1, height-1),
                   point(rect.left, height-1),
                   point(rect.Left, rect.Bottom)]);
   end;

   if EdgeDark <> clNone then
   begin
      wc.Pen.color := EdgeDark;
      wc.Polyline([point(rect.left-1, 1),
                   point(rect.left-1, rect.top+1)]);

      wc.Polyline([point(2, height-2),
                   point(rect.left-1, height-2),
                   point(rect.Left-1, rect.Bottom-1)]);
   end;

end;

procedure TabsDrawLeftBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
begin
   if Edge3dLight <> clNone then
   begin
      wc.Pen.Color := DarkenColor(Edge3dLight, 20);
      wc.Polyline([point(rect.right-1, rect.top-2),
                   point(rect.right-1, 0),
                   point(width, 0)]);

      wc.Polyline([point(rect.right-1, height-1),
                   point(rect.right-1, rect.bottom)]);
   end;

   if EdgeLight <> clNone then
   begin
      wc.Pen.Color := EdgeLight;
      wc.Polyline([point(rect.right, rect.top-1),
                   point(rect.right, 1),
                   point(width-1, 1)]);

      wc.Polyline([point(rect.right, height-2),
                   point(rect.right, rect.bottom-1)]);
   end;


   if Edge3dDark <> clNone then
   begin
      wc.Pen.color := Edge3dDark;
      wc.Polyline([point(width-1,1),
                   point(width-1, height-1),
                   point(rect.right-1, height-1)]);
   end;

   if EdgeDark <> clNone then
   begin

      wc.Pen.color := EdgeDark;
      wc.Polyline([point(width-2, 2),
                   point(width-2, height-2),
                   point(rect.right, height-2)]);
   end;
end;

procedure TabsDrawTopBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
begin
   if Edge3dLight <> clNone then
   begin
      wc.Pen.Color := DarkenColor(Edge3dLight, 20);
      wc.Polyline([point(rect.left-2, rect.bottom-1),
                   point(0, rect.bottom-1),
                   point(0, height)]);
      wc.Polyline([point(width-1, rect.bottom-1),
                   point(rect.right+1, rect.bottom-1)]);
   end;

   if EdgeLight <> clNone then
   begin
      wc.Pen.Color := EdgeLight;
      wc.Polyline([point(rect.left-1, rect.bottom),
                   point(1, rect.bottom),
                   point(1, height-1)]);
      wc.Polyline([point(width-2, rect.bottom),
                   point(rect.right, rect.bottom)]);
   end;

   if Edge3dDark <> clNone then
   begin
      wc.Pen.color := Edge3dDark;
      wc.Polyline([point(width-1, rect.bottom-1),
                   point(width-1, height-1),
                   point(0, height-1)]);
   end;

   if EdgeDark <> clNone then
   begin

      wc.Pen.color := EdgeDark;
      wc.Polyline([point(width-2, rect.bottom),
                   point(width-2, height-2),
                   point(1, height-2)]);
   end;
end;

procedure TabsDrawBottomBorder(wC:TCanvas; Rect:TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor; Width, Height:integer);
begin
   if Edge3dLight <> clNone then
   begin
      wc.Pen.Color := DarkenColor(Edge3dLight, 20);
      wc.Polyline([point(0, rect.top),
                   point(0, 0),
                   point(width-1, 0)]);

   end;

   if EdgeLight <> clNone then
   begin
      wc.Pen.Color := EdgeLight;
      wc.Polyline([point(1, rect.top-1),
                   point(1, 1),
                   point(width-2, 1)]);
   end;

   if Edge3dDark <> clNone then
   begin
      wc.Pen.color := Edge3dDark;
      wc.Polyline([point(width-1, 0),
                   point(width-1, rect.top),
                   point(rect.right, rect.top)]);

      wc.Polyline([point(rect.left-2, rect.top),
                   point(0, rect.top)]);
   end;

   if EdgeDark <> clNone then
   begin
      wc.Pen.color := EdgeDark;
      wc.Polyline([point(width-2, 1),
                   point(width-2, rect.top-1),
                   point(rect.right-1, rect.top-1)]);

      wc.Polyline([point(rect.left-1, rect.top-1),
                   point(1, rect.top-1)]);
   end;
end;

procedure TabsDrawRightTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
begin
  wc.Pen.Width := 1;

  if Edge3dDark <> clNone then
  begin
    wc.Pen.color := Edge3dDark;
     wc.Polyline([point(rect.right-2, rect.top),
                  point(rect.right, rect.top+2),
                  point(rect.right, rect.bottom-2),
                  point(rect.Right-2, rect.bottom),
                  point(rect.Left, rect.bottom)]);
  end;

  if EdgeDark <> clNone then
  begin
     wc.Pen.Color := EdgeDark;
     wc.Polyline([point(rect.right-3, rect.top),
                  point(rect.right-1, rect.top+2),
                  point(rect.right-1, rect.bottom-2),
                  point(rect.Right-2, rect.bottom-1),
                  point(rect.Left, rect.bottom-1)]);
  end;

  if Edge3dLight <> clNone then
  begin
     wc.Pen.Color := DarkenColor(Edge3dLight, 20);
     wc.PolyLine([point(rect.Left+1, rect.top),
                  point(rect.Right-2, rect.Top)]);
  end;

  if EdgeLight <> clNone then
  begin
     wc.Pen.Color := EdgeLight;
     wc.PolyLine([point(rect.Left+1, rect.top+1),
                  point(rect.right-1, rect.top+1)]);
  end;

end;

procedure TabsDrawLeftTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
begin
  wc.Pen.Width := 1;

  if Edge3dLight <> clNone then
  begin
     wc.Pen.Color := DarkenColor(Edge3dLight, 20);
     wc.PolyLine([point(rect.Left+2, rect.Bottom),
                  point(rect.Left, rect.Bottom-2),
                  point(rect.Left, rect.top+2),
                  point(rect.Left+2, rect.Top),
                  point(rect.right-1, rect.top)]);
  end;

  if EdgeLight <> clNone then
  begin
     wc.Pen.Color := EdgeLight;
     wc.PolyLine([point(rect.Left+3, rect.Bottom),
                  point(rect.Left+1, rect.Bottom-2),
                  point(rect.Left+1, rect.top+2),
                  point(rect.Left+3, rect.Top+1),
                  point(rect.right-1, rect.top+1)]);
  end;

  if Edge3dDark <> clNone then
  begin
     wc.Pen.color := Edge3dDark;
     wc.Polyline([point(rect.Left+2, rect.Bottom),
                  point(rect.right-1, rect.Bottom)]);
  end;

  if EdgeDark <> clNone then
  begin
     wc.Pen.Color := EdgeDark;
     wc.Polyline([point(rect.left+3, rect.Bottom-1),
                  point(rect.right-1, rect.Bottom-1)]);
  end;
end;

procedure TabsDrawTopTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
begin
  wc.Pen.Width := 1;

  if Edge3dLight <> clNone then
  begin
     wc.Pen.Color := DarkenColor(Edge3dLight, 20);
     wc.PolyLine([point(rect.Left, rect.Bottom-2),
                  point(rect.Left, rect.Top+2),
                  point(rect.Left+2, rect.top),
                  point(rect.Right-2, rect.Top),
                  point(rect.right, rect.top+2)]);
  end;

  if EdgeLight <> clNone then
  begin
     wc.Pen.Color := EdgeLight;
     wc.PolyLine([point(rect.Left+1, rect.Bottom-2),
                  point(rect.Left+1, rect.Top+2),
                  point(rect.Left+3, rect.top+1),
                  point(rect.Right-2, rect.Top+1),
                  point(rect.right, rect.top+3)]);
  end;

  if Edge3dDark <> clNone then
  begin
     wc.Pen.color := Edge3dDark;
     wc.Polyline([point(rect.right, rect.Top+2),
                  point(rect.right, rect.Bottom-2)]);
  end;

  if EdgeDark <> clNone then
  begin
     wc.Pen.Color := EdgeDark;
     wc.Polyline([point(rect.right-1, rect.Top+3),
                  point(rect.right-1, rect.Bottom-1)]);
  end;
end;

procedure TabsDrawBottomTabBorder(wc:TCanvas; Rect: TRect; Edge3dLight, EdgeLight, Edge3dDark, EdgeDark: TColor);
begin
  wc.Pen.Width := 1;

  if Edge3dLight <> clNone then
  begin
     wc.Pen.Color := DarkenColor(Edge3dLight, 20);
     wc.PolyLine([point(rect.Left, rect.top+2),
                  point(rect.Left, rect.bottom-2)]);
  end;

  if EdgeLight <> clNone then
  begin
     wc.Pen.Color := EdgeLight;
     wc.PolyLine([point(rect.Left+1, rect.top+2),
                  point(rect.Left+1, rect.bottom-3)]);
  end;

  if Edge3dDark <> clNone then
  begin
     wc.Pen.color := Edge3dDark;
     wc.Polyline([point(rect.right, rect.top+2),
                  point(rect.right, rect.bottom-3),
                  point(rect.Right-2, rect.bottom-1),
                  point(rect.Left+2, rect.bottom-1),
                  point(rect.Left+1, rect.bottom-3)]);
  end;

  if EdgeDark <> clNone then
  begin
     wc.Pen.Color := EdgeDark;
     wc.Polyline([point(rect.right-1, rect.top+2),
                  point(rect.right-1, rect.bottom-3),
                  point(rect.Right-2, rect.bottom-2),
                  point(rect.Left+2, rect.bottom-2),
                  point(rect.Left+1, rect.bottom-4)]);
  end;
end;

procedure TabsDrawText(aCanvas:TCanvas; aRect:TRect; aData:String; aPushed, aEnabled:boolean; Vertical:boolean; ImageIndex:integer; ImageList:TCustomImageList; aTabs:boolean = true);
var
  ItemInfo: TTBXItemInfo;
  wRect : TRect;
  xpos : integer;
  ypos : integer;
begin
   FillChar(ItemInfo, SizeOf(TTBXItemInfo), 0);
   with ItemInfo do
   begin
     ViewType    := VT_TOOLBAR;
     ItemOptions := IO_TOOLBARSTYLE;
     Enabled     := aEnabled;
     Pushed      := aPushed;
     Selected    := true;
     ImageShown  := (imagelist <> nil) and (ImageIndex > -1) and (imageindex < Imagelist.Count);
     if Imagelist <> nil then
     begin
        ImageHeight := imagelist.Height;
        ImageWidth  := ImageList.Width;
     end
     else
     begin
        ImageHeight := 0;
        ImageWidth  := 0;
     end;
     IsVertical  := vertical;
   end;

   if (imageList = nil) then
      CurrentTheme.PaintCaption(aCanvas, aRect, ItemInfo, aData, DT_CENTER or DT_VCENTER or DT_SINGLELINE, Vertical)
   else
   begin
      if (ImageIndex = -1) then
        CurrentTheme.PaintCaption(aCanvas, aRect, ItemInfo, aData, DT_CENTER or DT_VCENTER or DT_SINGLELINE, Vertical)
      else
      begin
         if (ImageIndex < ImageList.Count) then //Empty Image....
         begin
            wRect := aRect;
            wRect.Left := wRect.Left + (ImageList.Width div 3);
            wRect.Right := wRect.Left+ImageList.Width;

            if apushed and aTabs then
               xpos := 2
            else
               xpos := 0;

            if aTabs then
            begin
               if CurrentTheme.Name = 'Default' then
                  ypos := 0
               else
                  ypos := 1;
            end
            else
            ypos := 1;

            ypos := ypos + ((rectheight(wRect) div 2) - (Imagelist.height div 2));

            offsetRect(wRect, xpos, ypos);
            CurrentTheme.PaintImage(aCanvas, wRect, ItemInfo, ImageList, ImageIndex);
         end;

         wRect := aRect;
         wRect.Left := (aRect.Left+ImageList.width+(ImageList.Width div 3));
         if not aTabs then
            offsetrect(wRect, 0, -1);
         CurrentTheme.PaintCaption(aCanvas, wRect, ItemInfo, aData, DT_Center or DT_VCENTER or DT_SINGLELINE, Vertical);
      end;
   end;
end;

end.
