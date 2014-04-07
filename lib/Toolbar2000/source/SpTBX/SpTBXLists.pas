unit SpTBXLists;

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
  - It's impossible to enable unicode support on a TTBXUndoList descendant, to
    facilitate this TTBXCustomListViewer's FOffset and FVisibleItems members
    should be promoted to the public section.

Development notes:
  - All the TBX theme changes and adjustments are marked with '[TBXTheme-Change]'.

To Do:
  - Rotated caption painting.

Known Issues:
  -

History:
8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - No changes.

27 August 2006 - version 1.8
  - New component added, TSpTBXUndoList, thanks to Piotr Janus
    for his code donation.

15 June 2006 - version 1.7
  - No changes.

4 May 2006 - version 1.6
  - No changes.

12 April 2006 - version 1.5
  - No changes.

27 February 2006 - version 1.4
  - No changes.

10 February 2006 - version 1.3
  - No changes.

18 October 2005 - version 1.1
  - No changes.

18 August 2005 1.0
  - No changes.

10 June 2005 - version 0.9
  - SpTBXLib may now alternatively, at your option, be used and/or
    distributed under the terms of the SpTBXLib License.
    Please see the updated LICENSE.TXT file for more information.

20 May 2005 - version 0.8
  - No changes.

16 February 2005 - version 0.7
  - No changes.

23 December 2004 - version 0.6
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  TB2Item, TBX, TBXThemes, TBXLists, SpTBXItem, TntClasses;

type
  { TSpTBXCustomList }

  TSpTBXCustomList = class(TTBXCustomList)
  protected
    procedure DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer; const ARect: TRect; AIndex, AHoverIndex: Integer); override;
    function GetItemText(Index: Integer): string; override;
    function GetItemTextW(Index: Integer): WideString; virtual; abstract;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TSpTBXCustomListViewer = class(TTBXCustomListViewer)
  protected
    function GetItemHeight(ACanvas: TCanvas): Integer; override;
    function GetItemWidth(ACanvas: TCanvas; Index: Integer): Integer; override;
  end;

  { TSpTBXStringList }

  TSpTBXStringList = class(TSpTBXCustomList)
  private
    FStrings: TTntStrings;
    procedure SetStrings(Value: TTntStrings);
  protected
    function GetItemTextW(Index: Integer): WideString; override;
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ItemIndex;
    property MaxVisibleItems;
    property MaxWidth;
    property MinWidth;
    property Strings: TTntStrings read FStrings write SetStrings;
    property OnAdjustImageIndex;
    property OnChange;
    property OnClearItem;
    property OnClick;
    property OnDrawItem;
    property OnMeasureHeight;
    property OnMeasureWidth;
  end;

  TSpTBXStringListClass = class of TSpTBXStringList;

  { TSpTBXUndoList }

  TSpTBXUndoList = class(TSpTBXStringList)
  protected
    procedure DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer; const ARect: TRect; AIndex, AHoverIndex: Integer); override;
    function  GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    procedure HandleHover(AIndex: Integer); override;
  end;

  TSpTBXUndoListViewer = class(TSpTBXCustomListViewer)
  protected
    procedure AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer); override;
    procedure HandleAutoScroll(var Direction, Interval: Integer); override;
  end;

implementation

type
  TTBXItemViewerAccess = class(TTBXItemViewer);

const
  CImageSpacing = 4;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomList }

procedure TSpTBXCustomList.DrawItem(ACanvas: TCanvas;
  AViewer: TTBXCustomListViewer; const ARect: TRect; AIndex,
  AHoverIndex: Integer);
const
  FillColors: array [Boolean] of TColor = (clWindow, clHighlight);
  TextColors: array [Boolean] of TColor = (clWindowText, clHighlightText);
var
  S: WideString;
  R, R2: TRect;
  ImgList: TCustomImageList;
begin
  ACanvas.Brush.Color := FillColors[AIndex = AHoverIndex];
  if DoClearItem(ACanvas, ARect, AIndex, AHoverIndex) then ACanvas.FillRect(ARect);

  ACanvas.Font.Color := TextColors[AIndex = AHoverIndex];
  if DoDrawItem(ACanvas, ARect, AIndex, AHoverIndex) then
  begin
    R := ARect;
    InflateRect(R, -4, 1);
    ImgList := TTBXItemViewerAccess(AViewer).GetImageList;
    if ShowImages and (ImgList <> nil) then
    begin
      R2.Left := R.Left;
      R2.Top := (R.Top + R.Bottom - ImgList.Height) div 2;
      R2.Right := R2.Left + ImgList.Width;
      R2.Bottom := R2.Top + ImgList.Height;
      if Enabled then ImgList.Draw(ACanvas, R2.Left, R2.Top, GetImageIndex(AIndex))
      else DrawTBXImage(ACanvas, R2, ImgList, GetImageIndex(AIndex), ISF_DISABLED);
      Inc(R.Left, ImgList.Width + CImageSpacing);
    end;

    S := GetItemTextW(AIndex);
    if Length(S) > 0 then
    begin
      ACanvas.Brush.Style := bsClear;
      SpDrawXPText(ACanvas, S, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      ACanvas.Brush.Style := bsSolid;
    end;
  end;
end;

function TSpTBXCustomList.GetItemText(Index: Integer): string;
begin
  Result := '';
end;

function TSpTBXCustomList.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXCustomListViewer;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomListViewer }

function TSpTBXCustomListViewer.GetItemHeight(ACanvas: TCanvas): Integer;
var
  ImgList: TCustomImageList;
begin
  Result := SpGetTextSize(ACanvas.Handle, 'Q', False).cy + 2;
  with TSpTBXStringList(Item) do
  begin
    ImgList := GetImageList;
    if ShowImages and (ImgList <> nil) and (Result < ImgList.Height + 2) then
      Result := ImgList.Height + 2;
    DoMeasureHeight(ACanvas, Result);
  end;
end;

function TSpTBXCustomListViewer.GetItemWidth(ACanvas: TCanvas;
  Index: Integer): Integer;
var
  S: WideString;
  ImgList: TCustomImageList;
begin
  with TSpTBXStringList(Item) do
  begin
    S := GetItemTextW(Index);
    Result := SpGetTextSize(ACanvas.Handle, S, False).cx;
    if ShowImages then
    begin
      ImgList := GetImageList;
      if ImgList <> nil then
      begin
        Inc(Result, ImgList.Width);
        if Length(S) > 0 then Inc(Result, CImageSpacing);
      end;
    end;
    Inc(Result, 8);
    DoMeasureWidth(ACanvas, Index, Result)
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXStringList }

constructor TSpTBXStringList.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TTntStringList.Create;
end;

destructor TSpTBXStringList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TSpTBXStringList.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TSpTBXStringList.GetItemTextW(Index: Integer): WideString;
begin
  Result := FStrings[Index];
end;

procedure TSpTBXStringList.SetStrings(Value: TTntStrings);
begin
  FStrings.Assign(Value);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXUndoList }

procedure TSpTBXUndoList.DrawItem(ACanvas: TCanvas; AViewer: TTBXCustomListViewer;
  const ARect: TRect; AIndex, AHoverIndex: Integer);
const
  FillColors: array [Boolean] of TColor = (clWindow, clHighlight);
  TextColors: array [Boolean] of TColor = (clWindowText, clHighlightText);
var
  S: WideString;
  R: TRect;
begin
  ACanvas.Brush.Color := FillColors[AIndex <= AHoverIndex];
  ACanvas.FillRect(ARect);
  S := Strings[AIndex];
  if Length(S) > 0 then
  begin
    R := ARect;
    InflateRect(R, -4, 1);
    ACanvas.Font.Color := TextColors[AIndex <= AHoverIndex];
    ACanvas.Brush.Style := bsClear;
    DrawTextW(ACanvas.Handle, PWideChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

function TSpTBXUndoList.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TSpTBXUndoListViewer;
end;

procedure TSpTBXUndoList.HandleHover(AIndex: Integer);
begin
  ItemIndex := AIndex;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TTBXUndoListViewer }

procedure TSpTBXUndoListViewer.AdjustAutoScrollHover(var AIndex: Integer; Direction: Integer);
begin
  if Direction < 0 then AIndex := Offset
  else if Direction > 0 then AIndex := Offset + VisibleItems - 1;
end;

procedure TSpTBXUndoListViewer.HandleAutoScroll(var Direction, Interval: Integer);
begin
  inherited;
  if Direction < 0 then HoverIndex := Offset
  else if Direction > 0 then HoverIndex := Offset + VisibleItems - 1
  else Exit;
  TSpTBXCustomList(Item).HandleHover(HoverIndex);
  UpdateItems;
end;

end.
