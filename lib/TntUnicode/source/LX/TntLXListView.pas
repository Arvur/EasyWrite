
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXListView;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, ComCtrls, TntComCtrls, Graphics, Controls, TntMenus, Windows;

type
  TTntListViewItemLX = class(TTntListItem)
  private
    FItemID: Integer;
    FIsCheckBoxHidden: Boolean;
    procedure SetFontStyles(const Value: TFontStyles);
    procedure SetIsCheckBoxHidden(const Value: Boolean);
    procedure HideCheckBox;
    procedure SetBrushColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetLastItemBeforeDivider(const Value: Boolean);
  protected
    FLastItemBeforeDivider: Boolean;
    FFontStyles: TFontStyles;
    FFontColor: TColor;
    FBrushColor: TColor;
  public
    constructor Create(AOwner: TListItems{TNT-ALLOW TListItems}); override;
    property LastItemBeforeDivider: Boolean read FLastItemBeforeDivider write SetLastItemBeforeDivider;
    property ItemID: Integer read FItemID;
    property IsCheckBoxHidden: Boolean read FIsCheckBoxHidden write SetIsCheckBoxHidden;
    property FontStyles: TFontStyles read FFontStyles write SetFontStyles;
    property FontColor: TColor read FFontColor write SetFontColor;
    property BrushColor: TColor read FBrushColor write SetBrushColor;
  end;

  TTntListViewLXClass = class of TTntListViewItemLX;

  TTntCustomListViewLX = class(TTntCustomListView)
  private
    FIgnoreChange: Boolean;
    FCreateItemClass: TTntListViewLXClass;
    procedure GetItemClass(Sender: TCustomListView{TNT-ALLOW TCustomListView}; var ItemClass: TListItemClass);
    function GetColumns: TTntListColumns;
    function GetItems: TTntListItems;
  protected
    FColumnPopup: TTntPopupMenu;
    function AddLXItem(ItemClass: TTntListViewLXClass; ItemID: Integer): TTntListViewItemLX; virtual;
    procedure ColumnPopupPopup(Sender: TObject); virtual;
  protected
    procedure Change(Item: TListItem{TNT-ALLOW TListItem}; Change: Integer); override;
    function IsCustomDrawn(Target: TCustomDrawTarget;
      Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Item: TListItem{TNT-ALLOW TListItem}; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    procedure ColRightClick(Column: TListColumn{TNT-ALLOW TListColumn}; Point: TPoint); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoSizeAllColumns;
    procedure InvertCheckedItems;
    procedure CheckAllItems;
  published
    property Columns: TTntListColumns read GetColumns;
    property Items: TTntListItems read GetItems;
  end;

  TTntListViewLX = class(TTntCustomListViewLX)
  private
    function GetColumns: TTntListColumns;
    procedure SetColumns(const Value: TTntListColumns);
    function GetItems: TTntListItems;
    procedure SetItems(const Value: TTntListItems);
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property Columns: TTntListColumns read GetColumns write SetColumns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items: TTntListItems read GetItems write SetItems;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure AutoSizeAllColumns(Listview: TTntCustomListView);

implementation

uses
  Math, CommCtrl, SysUtils, TntGraphics;

type
  TAccessCustomListView = class(TTntCustomListView);

function CalculateColumnWidth(Listview: TTntCustomListView; ColumnIndex, MaxRows: Integer): Integer;
var
  AText: WideString;
  i: integer;
  TotalWidths: Integer;
  LastWidth: Integer;
begin
  Result := 0;
  with TAccessCustomListView(Listview) do begin
    // calculate width
    for i := 0 to Min(MaxRows - 1, Items.Count - 1) do begin
      // get text
      if ColumnIndex = 0 then
        AText := Items[i].Caption
      else if ColumnIndex - 1 < Items[i].SubItems.Count then
        AText := Items[i].SubItems[ColumnIndex - 1]
      else
        AText := '';
      // calculate min width
      Result := Max(Result, WideCanvasTextWidth(Canvas, AText))
    end;
    if ColumnIndex > 0 then
      Inc(Result, 12)
    else begin
      Inc(Result, 8);
      if SmallImages <> nil then
        Inc(Result, SmallImages.Width);
      if Checkboxes then
        Inc(Result, GetSystemMetrics(SM_CXMENUCHECK));
    end;

    // use header option
    if (ColumnIndex < Columns.Count) then
      Result := Max(Result, WideCanvasTextWidth(Canvas, Columns[ColumnIndex].Caption));

    // auto fill last column
    if (ColumnIndex = Columns.Count - 1) then begin
      TotalWidths := 0;
      for i := 0 to Columns.Count - 2 do begin
        Inc(TotalWidths, Columns[i].Width);
        if GridLines then
          Inc(TotalWidths, 1);
      end;
      LastWidth := ClientWidth - TotalWidths - GetSystemMetrics(SM_CXHTHUMB);
      if LastWidth > Result then
        Result := LastWidth;
    end;

  end;
end;

procedure AutoSizeAllColumns(Listview: TTntCustomListView);
var
  i: integer;
begin
  with TAccessCustomListView(Listview) do begin
    Items.BeginUpdate;
    try
      for i := 0 to Columns.Count - 1 do begin
        if OwnerData then begin
          Columns[i].Width := CalculateColumnWidth(Listview, i, 100);
        end else begin
          ListView_SetColumnWidth(Handle, i, LVSCW_AUTOSIZE_USEHEADER);
          Columns[i].Width := ListView_GetColumnWidth(Handle, i);
        end;
      end;
    finally
      Items.EndUpdate;
    end;
  end
end;

{ TTntListViewItemLX }

constructor TTntListViewItemLX.Create(AOwner: TListItems{TNT-ALLOW TListItems});
begin
  inherited;
  FFontColor := clDefault;
  FBrushColor := clDefault;
end;

procedure TTntListViewItemLX.HideCheckBox;
begin
  (Owner.Owner as TTntCustomListViewLX).FIgnoreChange := True;
  try
    ListView_SetItemState(Handle, Index, IndexToStateImageMask(0), LVIS_STATEIMAGEMASK);
  finally
    (Owner.Owner as TTntCustomListViewLX).FIgnoreChange := False;
  end;
  (Owner.Owner as TTntCustomListViewLX).UpdateItems(Index, Index);
end;

procedure TTntListViewItemLX.SetBrushColor(const Value: TColor);
begin
  FBrushColor := Value;
  Owner.Owner.Invalidate;
end;

procedure TTntListViewItemLX.SetFontStyles(const Value: TFontStyles);
begin
  FFontStyles := Value;
  Owner.Owner.Invalidate;
end;

procedure TTntListViewItemLX.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
  Owner.Owner.Invalidate;
end;

procedure TTntListViewItemLX.SetIsCheckBoxHidden(const Value: Boolean);
begin
  if FIsCheckBoxHidden <> Value then begin
    FIsCheckBoxHidden := Value;
    if Value then
      HideCheckBox
    else begin
      StateIndex := 0;
      StateIndex := -1;
    end;
  end;
end;

procedure TTntListViewItemLX.SetLastItemBeforeDivider(const Value: Boolean);
begin
  FLastItemBeforeDivider := Value;
  Owner.Owner.Invalidate;
end;

{ TTntCustomListViewLX }

constructor TTntCustomListViewLX.Create(AOwner: TComponent);
begin
  inherited;
  OnCreateItemClass := GetItemClass;
  FCreateItemClass := TTntListViewItemLX;
  FColumnPopup := TTntPopupMenu.Create(Self);
  FColumnPopup.OnPopup := ColumnPopupPopup;
end;

function TTntCustomListViewLX.GetColumns: TTntListColumns;
begin
  Result := inherited Columns;
end;

function TTntCustomListViewLX.GetItems: TTntListItems;
begin
  Result := inherited Items;
end;

procedure TTntCustomListViewLX.GetItemClass(Sender: TCustomListView{TNT-ALLOW TCustomListView}; var ItemClass: TListItemClass);
begin
  ItemClass := FCreateItemClass;
end;

function TTntCustomListViewLX.AddLXItem(ItemClass: TTntListViewLXClass; ItemID: Integer): TTntListViewItemLX;
begin
  FCreateItemClass := ItemClass;
  Result := Items.Add as TTntListViewItemLX;
  Result.FItemID := ItemID;
end;

procedure TTntCustomListViewLX.Change(Item: TListItem{TNT-ALLOW TListItem}; Change: Integer);
begin
  if not FIgnoreChange then begin
    if  (Change = LVIF_STATE)
    and (Item is TTntListViewItemLX)
    and (TTntListViewItemLX(Item).IsCheckBoxHidden) then begin
      TTntListViewItemLX(Item).HideCheckBox;
    end;
    inherited;
  end;
end;

function TTntCustomListViewLX.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
  Result := Target = dtItem;
end;

function TTntCustomListViewLX.CustomDrawItem(Item: TListItem{TNT-ALLOW TListItem}; State: TCustomDrawState;
  Stage: TCustomDrawStage): Boolean;
var
  LXListItem: TTntListViewItemLX;
  Rect: TRect;
begin
  LXListItem := Item as TTntListViewItemLX;

  Canvas.Font.Style := LXListItem.FFontStyles;

  Canvas.Font.Color := clWhite;
  Canvas.Font.Color := clBlack; // toggle font to make sure it gets set
  if LXListItem.FFontColor <> clDefault then
    Canvas.Font.Color := LXListItem.FFontColor
  else
    Canvas.Font.Color := Font.Color;

  if LXListItem.FBrushColor <> clDefault then
    Canvas.Brush.Color := LXListItem.FBrushColor
  else
    Canvas.Brush.Color := Color;

  Result := inherited CustomDrawItem(Item, State, Stage);

  if LXListItem.FLastItemBeforeDivider then
  begin
    Rect := Item.DisplayRect(drBounds);
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 2;
    Canvas.Pen.Mode := pmBlack;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
  end;
end;

procedure TTntCustomListViewLX.ColRightClick(Column: TListColumn{TNT-ALLOW TListColumn}; Point: TPoint);
begin
  inherited;
  Point := ClientToScreen(Point);
  FColumnPopup.Popup(Point.X, Point.Y);
end;

procedure TTntCustomListViewLX.ColumnPopupPopup(Sender: TObject);
begin
  //
end;

procedure TTntCustomListViewLX.InvertCheckedItems;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if not (Items[i] as TTntListViewItemLX).IsCheckBoxHidden then
      Items[i].Checked := not Items[i].Checked;
end;

procedure TTntCustomListViewLX.CheckAllItems;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    if not (Items[i] as TTntListViewItemLX).IsCheckBoxHidden then
      Items[i].Checked := True;
end;

procedure TTntCustomListViewLX.AutoSizeAllColumns;
begin
  TntLXListView.AutoSizeAllColumns(Self);
end;

{ TTntListViewLX }

function TTntListViewLX.GetColumns: TTntListColumns;
begin
  Result := inherited Columns;
end;

procedure TTntListViewLX.SetColumns(const Value: TTntListColumns);
begin
  (inherited Columns).Assign(Value);
end;

function TTntListViewLX.GetItems: TTntListItems;
begin
  Result := inherited Items;
end;

procedure TTntListViewLX.SetItems(const Value: TTntListItems);
begin
  (inherited Items).Assign(Value);
end;

end.
