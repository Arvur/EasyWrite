
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXDBGrids;

{$INCLUDE TntCompilers.inc}

interface

uses
  Types, Classes, Controls, Grids, Windows, Graphics, Messages, Forms, DB, DBGrids,
  TntDBGrids, TntLXLookupCtrls, TntExtCtrls;

type
  TTntDbGridLX = class;

  TTntDBGridInplaceEditLX = class(TTntDBGridInplaceEdit)
  private
    FActiveControl: TWinControl;
    FComboBox: TTntDBLookupComboBoxLX;
    FCheckBoxPanel: TTntPanel;
    function ActiveControlIsFocused: Boolean;
    procedure SetActiveControl(Control: TWinControl);
  protected
    function Grid: TTntDbGridLX;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure Resize; override;
    procedure UpdateContents; override;
    function GetText: WideString; override;
    procedure SetText(const Value: WideString); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TOnCheckLookupFields = procedure(ComboBox: TTntDBLookupComboBoxLX; Field: TField) of object;

  TTntColumnTabStopMode = (tsmAlways, tsmAuto, tsmNever);

  TTntColumnLX = class(TTntColumn)
  private
    FTabStopMode: TTntColumnTabStopMode;
  public
    constructor Create(Collection: TCollection); override;
  published
    property TabStopMode: TTntColumnTabStopMode read FTabStopMode write FTabStopMode default tsmAuto;
  end;

  TTntDbGridLX = class(TTntDBGrid)
  private
    FOnCheckLookupFields: TOnCheckLookupFields;
    FCheckedBMP: TBitmap;
    FUncheckedBMP: TBitmap;
    FInMouseDown: Boolean;
    FLastMouseDownPt: TPoint;
    FOnDrawTitleCell: TDrawColumnCellEvent;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure DrawTitleCell(const Rect: TRect; DataCol: Integer; Column: TColumn{TNT-ALLOW TColumn};
      State: TGridDrawState);
  protected
    function Editor: TTntDBGridInplaceEditLX;
    function CreateColumns: TDBGridColumns{TNT-ALLOW TDBGridColumns}; override;
    procedure SetColumnAttributes; override;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure WndProc(var Msg: TMessage); override;
    function CanEditAcceptKey(Key: Char{TNT-ALLOW Char}): Boolean; override;
    function CanEditShow: Boolean; override;
    function CanEditModify: Boolean; override;
    function CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit}; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ShowEditorChar(Ch: WideChar); override;
  public
    destructor Destroy; override;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TTntColumn; State: TGridDrawState); override;
  published
    property OnCheckLookupFields: TOnCheckLookupFields
      read FOnCheckLookupFields write FOnCheckLookupFields;
    property OnDrawTitleCell: TDrawColumnCellEvent read FOnDrawTitleCell
      write FOnDrawTitleCell;
  end;

implementation

uses
  StdCtrls, SysUtils,
  TntControls, TntClasses, TntForms, TntStdCtrls, TntDBCtrls, TntSysUtils;

var
  MySetFocus: Cardinal;

type
  TAccessCustomGrid = class(TCustomGrid);

procedure RemoveMessageFromQueue(Handle: THandle; MessageID: Integer);
var
  Msg: TMsg;
begin
  EnableManualPeekMessageWithRemove;
  try
    while PeekMessage{TNT-ALLOW PeekMessage}(Msg, Handle, MessageID, MessageID, PM_REMOVE) do ;
  finally
    DisableManualPeekMessageWithRemove;
  end;
end;

procedure PostMyFocusMessage(Handle: THandle);
begin
  RemoveMessageFromQueue(0, MySetFocus);
  PostMessage(Handle, MySetFocus, 0, 0);
end;

function HandleBelongsToParent(Control: TWinControl; h: Cardinal; MaxParentClass: TClass): Boolean;
var
  Parent: TWinControl;
begin
  Result := False;
  Parent := Control.Parent;
  While Parent <> nil do begin
    if h = Parent.Handle then begin
      Result := True;
      break;
    end;
    if (Control is MaxParentClass)
    or (Parent is MaxParentClass) then
      break;
    Parent := Parent.Parent;
  end;
end;

procedure InplaceControl_AfterInherited_WndProc(Control: TWinControl; var Msg: TMessage; Grid: TTntDbGridLX; FocusControl: TWinControl; SnapFocusControl: TWinControl);
var
  SaveLeft, SaveWidth: Integer;
begin
  if (Msg.Msg = WM_SETFOCUS) then begin
    // WM_SETFOCUS
    if (not HandleBelongsToParent(Control, TWMSetFocus(Msg).FocusedWnd, TTntDBGridInplaceEditLX)) then
      PostMyFocusMessage(Grid.Handle) {focus didn't come from parent}
    else if FocusControl <> nil then
      PostMyFocusMessage(FocusControl.Handle);
  end else if (Msg.Msg = MySetFocus) then begin
    // MySetFocus
    if FocusControl <> nil then
      PostMyFocusMessage(FocusControl.Handle)
    else if (not Control.Focused) then begin
      SaveLeft := SnapFocusControl.Left;
      SaveWidth := SnapFocusControl.Width;
      try
        SnapFocusControl.Left := 0; { keep form.horzscrollbar from moving }
        if SnapFocusControl = Control then
          SnapFocusControl.Width := 0
        else
          SnapFocusControl.Width := Control.Width;
        Control.SetFocus;
      finally
        SnapFocusControl.Left := SaveLeft;
        SnapFocusControl.Width := SaveWidth;
      end;
    end
  end;
end;

{ TTntDBGridLXCheckBox }

type
  TTntDBGridLXCheckBox = class(TTntDBCheckBox)
  private
    function Edit: TTntDBGridInplaceEditLX;
    function Grid: TTntDbGridLX;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
  end;

function TTntDBGridLXCheckBox.Edit: TTntDBGridInplaceEditLX;
begin
  if Parent = nil then
    Result := nil
  else
    Result := Parent.Parent as TTntDBGridInplaceEditLX;
end;

function TTntDBGridLXCheckBox.Grid: TTntDbGridLX;
begin
  if Edit = nil then
    Result := nil
  else
    Result := Edit.Grid;
end;

procedure TTntDBGridLXCheckBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
  if goTabs in TAccessCustomGrid(Grid).Options then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TTntDBGridLXCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not (Key in
    [VK_ESCAPE, VK_RETURN, VK_TAB, VK_INSERT, VK_F2, VK_DELETE,
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT,
    VK_LEFT, VK_RIGHT, VK_HOME, VK_END] )
  then
    inherited
  else begin
    if Key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END] then begin
      if (goAlwaysShowEditor in TAccessCustomGrid(Grid).Options) then begin
        Grid.KeyDown(Key, Shift);
        Key := 0;
        exit;
      end;
    end;
    Edit.KeyDown(Key, Shift);
    if Key <> 0 then
      inherited KeyDown(Key, Shift);
  end;
end;

procedure TTntDBGridLXCheckBox.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  if Key in [#27, #13, #9] then
    Edit.KeyPress(Key);
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TTntDBGridLXCheckBox.WndProc(var Msg: TMessage);
begin
  inherited;
  InplaceControl_AfterInherited_WndProc(Self, Msg, Grid, nil, Parent);
end;

{ TTntDBGridLXCheckBoxPanel }

type
  TTntDBGridLXCheckBoxPanel = class(TTntPanel)
  private
    CheckBox: TTntDBCheckBox;
    function Edit: TTntDBGridInplaceEditLX;
    function Grid: TTntDbGridLX;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TTntDBGridLXCheckBoxPanel.Create(AOwner: TComponent);
begin
  inherited;
  CheckBox := TTntDBGridLXCheckBox.Create(Self);
  CheckBox.Parent := Self;
  CheckBox.Height := CheckBox.Height - 4;
  CheckBox.Width := CheckBox.Height;
end;

procedure TTntDBGridLXCheckBoxPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  { This keeps check box from being painted over by the inplaceedit. }
  Params.Style := Params.Style and (not WS_CLIPCHILDREN);
end;

function TTntDBGridLXCheckBoxPanel.Edit: TTntDBGridInplaceEditLX;
begin
  result := Parent as TTntDBGridInplaceEditLX;
end;

function TTntDBGridLXCheckBoxPanel.Grid: TTntDbGridLX;
begin
  if Edit = nil then
    Result := nil
  else
    Result := Edit.Grid;
end;

procedure TTntDBGridLXCheckBoxPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TTntDBGridLXCheckBox(CheckBox).Toggle;
end;

procedure TTntDBGridLXCheckBoxPanel.WndProc(var Msg: TMessage);
begin
  inherited;
  InplaceControl_AfterInherited_WndProc(Self, Msg, Grid, CheckBox, Self);
  if (Msg.Msg = WM_CHAR) and (Msg.WParam = VK_SPACE) then
    TTntDBGridLXCheckBox(CheckBox).Toggle;
end;

procedure TTntDBGridLXCheckBoxPanel.Resize;
begin
  inherited;
  CheckBox.Top := (ClientHeight - CheckBox.Height) div 2;
  CheckBox.Left := (ClientWidth - CheckBox.Width) div 2;
end;

procedure TTntDBGridLXCheckBoxPanel.WMPaint(var Message: TWMPaint);
var
  Rect: TRect;
begin
  inherited;
  Rect := ClientRect;
  Canvas.Brush.Color := Color;
  InflateRect(Rect, -1, -1);
  Dec(Rect.Right);
  Canvas.DrawFocusRect(Rect);
end;

{ TTntColumnLX }

constructor TTntColumnLX.Create(Collection: TCollection);
begin
  inherited;
  FTabStopMode := tsmAuto;
end;

{ TTntDBGridComboBoxLX }

type
  TTntDBGridComboBoxLX = class(TTntDBLookupComboBoxLX)
  private
    function Edit: TTntDBGridInplaceEditLX;
    function Grid: TTntDbGridLX;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
    procedure Change; override;
    procedure Select; override;
    property Style;
  end;

procedure TTntDBGridComboBoxLX.WndProc(var Msg: TMessage);
begin
  inherited;
  InplaceControl_AfterInherited_WndProc(Self, Msg, Grid, nil, Self);
end;

function TTntDBGridComboBoxLX.Edit: TTntDBGridInplaceEditLX;
begin
  result := Parent as TTntDBGridInplaceEditLX;
end;

function TTntDBGridComboBoxLX.Grid: TTntDbGridLX;
begin
  if Edit = nil then
    Result := nil
  else
    Result := Edit.Grid
end;

procedure TTntDBGridComboBoxLX.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if goTabs in TAccessCustomGrid(Grid).Options then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TTntDBGridComboBoxLX.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ((Shift = [ssAlt]) and (Key = VK_DOWN))
  or (DroppedDown and (Style <> csSimple)) then
    inherited
  else begin
    Edit.KeyDown(Key, Shift);
    if Key <> 0 then
      inherited KeyDown(Key, Shift);
  end;
end;

procedure TTntDBGridComboBoxLX.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  if (not DroppedDown) or (Style = csSimple) then
    Edit.KeyPress(Key);
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TTntDBGridComboBoxLX.Change;
var
  Message: TWMCommand;
begin
  inherited;
  if (not Edit.UseDataList) then begin
    Message.Msg := WM_COMMAND;
    Message.ItemID := 0;
    Message.NotifyCode := EN_CHANGE;
    Message.Ctl := Edit.Handle;
    Message.Result := 0;
    Grid.Dispatch(Message);
  end;
end;

procedure TTntDBGridComboBoxLX.Select;
begin
  if TAccessCustomGrid(Grid).CanEditModify and (not Edit.UseDataList) then
    Change;
  inherited;
end;

procedure TTntDBGridComboBoxLX.WMPaint(var Message: TWMPaint);
begin
  inherited;
  Canvas.Pen.Color := Self.Color;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(1, 1, ClientWidth, ClientHeight);
end;

{ TTntDBGridInplaceEditLX }

constructor TTntDBGridInplaceEditLX.Create(AOwner: TComponent);
begin
  inherited;
  // create combo box
  FComboBox := TTntDBGridComboBoxLX.Create(Self);
  (FComboBox as TTntDBGridComboBoxLX).FDeferChangesTilCloseup := True;
  FComboBox.Parent := Self;
  FComboBox.Top := -1;
  FComboBox.Left := -1;
  FComboBox.Visible := False;
  FComboBox.ParentColor := True;
  // create check box panel
  FCheckBoxPanel := TTntDBGridLXCheckBoxPanel.Create(Self);
  FCheckBoxPanel.Parent := Self;
  FCheckBoxPanel.Top := -1;
  FCheckBoxPanel.Left := -1;
  FCheckBoxPanel.Visible := False;
  FCheckBoxPanel.ParentColor := True;
end;

function TTntDBGridInplaceEditLX.Grid: TTntDbGridLX;
begin
  Result := inherited Grid as TTntDbGridLX;
end;

procedure TTntDBGridInplaceEditLX.WndProc(var Msg: TMessage);

  function ComboEditHandle: HWND;
  begin
    result := (FComboBox as TTntDBGridComboBoxLX).EditHandle;
  end;

begin
  if (   (Msg.Msg = WM_GETTEXTLENGTH)
      or (Msg.Msg = WM_GETTEXT)
      or (Msg.Msg = WM_CHAR)
      or (Msg.Msg = EM_GETSEL)
      or (Msg.Msg = EM_SETSEL)
      or (Msg.Msg = EM_CHARFROMPOS)
     ) and (ComboEditHandle <> 0) then
  begin
    if IsWindowUnicode(ComboEditHandle) then
      Msg.Result := SendMessageW(ComboEditHandle, Msg.Msg, Msg.WParam, Msg.LParam)
    else
      Msg.Result := SendMessageA(ComboEditHandle, Msg.Msg, Msg.WParam, Msg.LParam)
  end else begin
    inherited;
    InplaceControl_AfterInherited_WndProc(Self, Msg, Grid, ActiveControl, Self);
  end;
end;

function TTntDBGridInplaceEditLX.ActiveControlIsFocused: Boolean;
begin
  Result := ActiveControl.Focused;
  if (not Result) and (ActiveControl = FCheckBoxPanel) then
    Result := TTntDBGridLXCheckBoxPanel(FCheckBoxPanel).CheckBox.Focused;
end;

procedure TTntDBGridInplaceEditLX.Resize;
begin
  inherited;
  if (ActiveControl <> nil) then begin
    if TAccessCustomGrid(Grid).CanEditShow then begin
      // width
      if Grid.GetEditStyle(0, 0) = esEllipsis then
        ActiveControl.Width := ClientWidth + 1 - ButtonWidth
      else
        ActiveControl.Width := ClientWidth + 1 + 2;
      // height
      ActiveControl.Height := ClientHeight + 2;
    end else begin
      // hidden, move focus back to grid
      if ActiveControlIsFocused then
        PostMyFocusMessage(Grid.Handle);
    end;
  end;
end;

procedure TTntDBGridInplaceEditLX.UpdateContents;
var
  Column: TTntColumn;
begin
  { Figure Column }
  with Grid do
    Column := Grid.Columns[SelectedIndex] as TTntColumn;
  { Setup Combo Box }
  if (Grid.GetEditStyle(0, 0) <> esPickList) then begin
    // Simple Edit
    (FComboBox as TTntDBGridComboBoxLX).Style := csSimple;
    FComboBox.DataSource := nil;
    FCombobox.DataField := '';
    FComboBox.Clear;
  end else if UseDataList then begin
    // Lookup List
    (FComboBox as TTntDBGridComboBoxLX).Style := csDropDown;
    if FComboBox.DataSource <> Grid.DataSource then begin
      FComboBox.Items.Clear;
      FComboBox.DataSource := Grid.DataSource;
    end;
    if FCombobox.DataField <> Column.FieldName then begin
      FComboBox.Text := '';
      FComboBox.ListField := '';
      FCombobox.DataField := Column.FieldName;
      if Assigned(Grid.OnCheckLookupFields) then
        Grid.OnCheckLookupFields(FComboBox, Column.Field);
    end;
  end else begin
    // Pick List
    (FComboBox as TTntDBGridComboBoxLX).Style := csDropDown;
    FComboBox.DataSource := nil;
    FCombobox.DataField := '';
    FComboBox.Items.Assign(Column.PickList{TNT-ALLOW PickList});
  end;
  { Select Active Control }
  if (Column.Field <> nil) and (Column.Field is TBooleanField) then begin
    // check box panel
    SetActiveControl(FCheckBoxPanel);
    with TTntDBGridLXCheckBoxPanel(FCheckBoxPanel) do begin
      CheckBox.DataField := '';
      CheckBox.DataSource := Grid.DataSource;
      CheckBox.DataField := Column.FieldName;
      CheckBox.ReadOnly := Column.ReadOnly;
    end;
  end else begin
    // combo box
    SetActiveControl(FComboBox);
    FComboBox.DropDownRows := Column.DropDownRows;
  end;

  inherited;

  { Toggle CheckBox if clicked on cell }
  if ActiveControl = FCheckBoxPanel then begin
    with Grid do begin
      if FInMouseDown then begin
        FInMouseDown := False;
        if PtInRect(CellRect(Col, Row), FLastMouseDownPt) then begin
          TTntDBGridLXCheckBox(TTntDBGridLXCheckBoxPanel(FCheckBoxPanel).CheckBox).Toggle;
        end;
      end;
    end;
  end;
end;

function TTntDBGridInplaceEditLX.GetText: WideString;
begin
  result := FComboBox.Text;
end;

procedure TTntDBGridInplaceEditLX.SetText(const Value: WideString);
begin
  if (not UseDataList) then
    FComboBox.Text := Value;
end;

procedure TTntDBGridInplaceEditLX.SetActiveControl(Control: TWinControl);
begin
  if ActiveControl <> Control then begin
    if ActiveControl <> nil then
      ActiveControl.Visible := False; { hide old control }
    FActiveControl := Control;
    if ActiveControl <> nil then begin
      ActiveControl.Visible := True; { show new control }
      PostMyFocusMessage(ActiveControl.Handle);
    end;
  end;
end;

{ TTntDbGridLX }

destructor TTntDbGridLX.Destroy;
begin
  inherited;
  FCheckedBMP.Free;
  FUncheckedBMP.Free;
end;

procedure TTntDbGridLX.DrawTitleCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn{TNT-ALLOW TColumn}; State: TGridDrawState);
begin
  if Assigned(OnDrawTitleCell) then
    OnDrawTitleCell(Self, Rect, DataCol, Column, State);
end;

procedure TTntDbGridLX.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
begin
  inherited;
  Dec(ARow, FixedRows);
  ACol := RawToDataColumn(ACol);
  if (ARow < 0) and (ACol >= 0) then
    DrawTitleCell(ARect, ACol, Columns[ACol], AState);
end;

function TTntDbGridLX.CreateColumns: TDBGridColumns{TNT-ALLOW TDBGridColumns};
begin
  Result := TTntDBGridColumns.Create(Self, TTntColumnLX);
end;

function TTntDbGridLX.Editor: TTntDBGridInplaceEditLX;
begin
  Result := InplaceEditor as TTntDBGridInplaceEditLX;
end;

procedure TTntDbGridLX.WndProc(var Msg: TMessage);
begin
  inherited;
  if (Msg.Msg = MySetFocus) and (not Focused) then
    Self.SetFocus;
end;

function TTntDbGridLX.CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit};
begin
  result := TTntDBGridInplaceEditLX.Create(Self);
end;

function TTntDbGridLX.CanEditAcceptKey(Key: Char{TNT-ALLOW Char}): Boolean;
begin
  result := ((Editor <> nil) and Editor.UseDataList) or (inherited CanEditAcceptKey(Key));
end;

function TTntDbGridLX.CanEditModify: Boolean;
begin
  if ((Editor <> nil) and Editor.UseDataList) then
    result := (not ReadOnly) and Datalink.Active and (not Datalink.Readonly)
  else
    result := inherited CanEditModify;
end;

procedure TTntDbGridLX.WMVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollCode = SB_THUMBTRACK then
    Message.ScrollCode := SB_THUMBPOSITION;
  inherited;
  Update;
end;

function TTntDbGridLX.CanEditShow: Boolean;
begin
  if (LayoutLock <> 0)
  or ([goRowSelect, goEditing] * TAccessCustomGrid(Self).Options <> [goEditing])
  or (not EditorMode)
  or (csDesigning in ComponentState)
  or (not HandleAllocated)
  or (goAlwaysShowEditor in TAccessCustomGrid(Self).Options)
  or (GetParentForm(Self) = nil) then
    Result := inherited CanEditshow
  else
    Result := (GetParentForm(Self).ActiveControl = Self)
           or (Editor.ActiveControlIsFocused);
end;

procedure TTntDbGridLX.SetColumnAttributes;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Columns.Count - 1 do begin
    with Columns[I] as TTntColumnLX do begin
      if (TabStopMode = tsmAuto) then
        // Do nothing...
      else if (TabStopMode = tsmAlways) then
        TabStops[I + IndicatorOffset] := True
      else if (TabStopMode = tsmNever) then
        TabStops[I + IndicatorOffset] := False
      else
        raise ETntInternalError.CreateFmt('Unexpected TabStopMode (%d)', [Integer(TabStopMode)]);
    end;
  end;
end;

procedure SaveControlAsBmp(Control: TWinControl; BMP: TBitmap);
var
  DC: HDC;
begin
  DC := GetDC(Control.Handle);
  try
    BMP.Handle := CreateCompatibleBitmap(DC, Control.Width, Control.Height);
  finally
    ReleaseDC(Control.Handle, DC);
  end;
  BMP.Canvas.Lock;
  try
    Control.PaintTo(BMP.Canvas.Handle, 0, 0);
  finally
    BMP.Canvas.Unlock;
  end;
end;

procedure TTntDbGridLX.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TTntColumn; State: TGridDrawState);
var
  bmp: TBitmap;
  X, Y: Integer;
  CB: TTntCheckBox;
  Panel: TTntPanel;
begin
  if (Column.Field = nil) or (not (Column.Field is TBooleanField)) then
    inherited DefaultDrawColumnCell(Rect, DataCol, Column, State)
  else begin
    Canvas.FillRect(Rect);
    { create bitmaps if first paint }
    if (FCheckedBMP = nil) then begin
      // create bitmaps
      FCheckedBMP := TBitmap.Create;
      FUncheckedBMP := TBitmap.Create;
      // create panel to provide a clean background for check box
      Panel := TTntPanel.Create(nil);
      try
        Panel.Left := -100;
        Panel.Top := -100;
        Panel.ParentColor := True;
        {$IFDEF COMPILER_7_UP}
        Panel.ParentBackground := False;
        {$ENDIF}
        Panel.BevelInner := bvNone;
        Panel.BevelOuter := bvNone;
        Panel.Parent := Self;
        // create checkbox
        CB := TTntCheckBox.Create(nil);
        try
          CB.Left := 0;
          CB.Top := 0;
          CB.Height := CB.Height - 4;
          CB.Width := CB.Height;
          CB.Visible := True;
          CB.Parent := Panel;
          // draw checked bmp
          CB.Checked := True;
          SaveControlAsBmp(CB, FCheckedBMP);
          // draw unchecked bmp
          CB.Checked := False;
          SaveControlAsBmp(CB, FUnCheckedBMP);
        finally
          CB.Free;
        end;
      finally
        Panel.Free;
      end;
    end;
    { draw check box }
    if Column.Field.AsBoolean then
      bmp := FCheckedBMP
    else
      bmp := FUncheckedBMP;
    if bmp <> nil then begin
      X := Rect.Left + (((Rect.Right - Rect.Left) - bmp.Width) div 2);
      Y := Rect.Top + (((Rect.Bottom - Rect.Top) - bmp.Height) div 2);
      Canvas.Draw(X, Y, bmp);
    end;
  end;
end;

procedure TTntDbGridLX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (Shift = [ssLeft]) then
    FInMouseDown := True;
  try
    FLastMouseDownPt := Point(X, Y);
    inherited;
  finally
    FInMouseDown := False;
  end;
end;

procedure TTntDbGridLX.ShowEditorChar(Ch: WideChar);
begin
  ShowEditor;
  if (Editor <> nil) and (Editor.ActiveControl <> nil) then begin
    if Win32PlatformIsUnicode then
      PostMessageW(Editor.ActiveControl.Handle, WM_CHAR, Word(Ch), 0)
    else
      PostMessageA(Editor.ActiveControl.Handle, WM_CHAR, Word(Ch), 0);
  end;
end;

initialization
  MySetFocus := RegisterWindowMessage('TTntDbGridLX.MySetFocus');

end.
