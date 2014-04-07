unit TBXSizer;
// TBX Sizer panel
// Copyright 2005 Konstantin Rudenko. All Rights Reserved

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Classes, Graphics, TB2Item, TBX, TBXThemes;

type
  TTBXSizerItem = class(TTBXCustomItem)
  public
    constructor Create(AOwner: TComponent); override;
    function    GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  published
    property  Visible;
  end;

  TTBXSizerItemViewer = class(TTBSeparatorItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;IsHoverItem, IsPushed, UseDisabledShadow: Boolean); override;
  public
  end;

implementation

uses Controls, Forms, TB2Common, TB2Dock, Types;

{ TTBXSizerItem }

constructor TTBXSizerItem.Create(AOwner: TComponent);
begin
 inherited;
 ImageIndex:=-1;
 ItemStyle:=ItemStyle-[tbisSelectable,tbisRedrawOnSelChange,tbisRedrawOnMouseOverChange]+[tbisClicksTransparent];
end;

function TTBXSizerItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
 Result:=TTBXSizerItemViewer;
end;

{ TTBXSizerItemViewer }

procedure TTBXSizerItemViewer.CalcSize(const Canvas: TCanvas; var AWidth,AHeight: Integer);
var
 VT : integer;
 i  : integer;
 mw,mh : integer;
begin
 AWidth := 0;
 AHeight := 0;
 if not TTBXSizerItem(Self.Item).Visible then exit;
 if Assigned(View) then
  if ((GetWinViewType(TTBXToolbar(View.Owner)) and VT_TOOLBAR) = VT_TOOLBAR) and Assigned(TTBCustomDockableWindow(View.Owner).CurrentDock) then
   begin
    VT := GetViewType(View);
    if not ((((VT and VT_POPUP) = VT_POPUP) or ((VT and PVT_CHEVRONMENU) = PVT_CHEVRONMENU))) then
     if (TTBXToolbar(View.Owner).FullSize or TTBXToolbar(View.Owner).Stretch) and (View.Orientation <> tbvoFloating) then
      case View.Orientation of
       tbvoHorizontal:
        begin
         if View.ViewerCount>0 then
          begin
           mh:=0; mw:=0;
           for i:=0 to View.ViewerCount-1 do
            if (View.Viewers[i]<>Self) and (View.Viewers[i].IsAccessible) then
             begin
              mw:=mw+(View.Viewers[i].BoundsRect.Right-View.Viewers[i].BoundsRect.Left);
              if mh<(View.Viewers[i].BoundsRect.Bottom-View.Viewers[i].BoundsRect.Top)
               then mh:=(View.Viewers[i].BoundsRect.Bottom-View.Viewers[i].BoundsRect.Top);
             end;
           AWidth:=TTBCustomDockableWindow(View.Owner).CurrentDock.Width-TTBCustomDockableWindow(View.Owner).NonClientWidth-mw;
           if AWidth<0 then AWidth:=0;
           AHeight:=mh;
          end;
        end;
       tbvoVertical:
        begin
         if View.ViewerCount>0 then
          begin
           mh:=0; mw:=0;
           for i:=0 to View.ViewerCount-1 do
            if (View.Viewers[i]<>Self) and (View.Viewers[i].IsAccessible) then
             begin
              mh:=mh+(View.Viewers[i].BoundsRect.Bottom-View.Viewers[i].BoundsRect.Top);
              if mw<(View.Viewers[i].BoundsRect.Right-View.Viewers[i].BoundsRect.Left)
               then mw:=(View.Viewers[i].BoundsRect.Right-View.Viewers[i].BoundsRect.Left);
             end;
           AHeight:=TTBCustomDockableWindow(View.Owner).CurrentDock.Height-TTBCustomDockableWindow(View.Owner).NonClientHeight-mh;
           if AHeight<0 then AHeight:=0;
           AWidth:=mw;
          end;
        end;
      end;
   end;
end;

procedure TTBXSizerItemViewer.Paint(const Canvas: TCanvas;const ClientAreaRect: TRect; IsHoverItem,IsPushed,UseDisabledShadow: Boolean);
var
  Item: TTBXSizerItem;
  R: TRect;
begin
  Item := TTBXSizerItem(Self.Item);
  if not (csDesigning in Item.ComponentState) then exit;
  if not Item.Visible then exit;
  R := ClientAreaRect;
  Canvas.Pen.Style:=psDot;
  Canvas.Pen.Mode:=pmBlack;
  InflateRect(R,-1,0);
  Canvas.Rectangle(R);
end;

end.
