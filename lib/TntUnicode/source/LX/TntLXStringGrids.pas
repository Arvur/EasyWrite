
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXStringGrids;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, Classes, Controls, Grids, SysUtils, TntGrids, TntStdCtrls, TntComCtrls,
  TntLXCombos, TntLXClasses, TntLXUtils;

const
  DEFAULT_EDITOR_OFFSET = -1;

  WM_RESET_EDITOR_FOCUS = WM_USER + 100;

type
  TGetEditorMarginEvent = procedure(Side: TAnchorKind; var Margin: Integer) of object;

  TTntStringGridLX = class(TTntStringGrid)
  private
    FEditor: TWinControl;
    FEditorOffset: Integer;
    FOnUpdateEditorFromGrid: TTntSetEditEvent;
    FGetEditorMargin: TGetEditorMarginEvent;
    procedure SetEditor(const Value: TWinControl);
    procedure WmResetEditorFocus(var Msg: TMessage); message WM_RESET_EDITOR_FOCUS;
  protected
    function CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit}; override;
    function GetEditLimit: Integer; override;
    function GetEditorMargin(Side: TAnchorKind): Integer;
    procedure DoUpdateEditorFromGrid;
    procedure UpdateEditorBounds;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGridFromEditor(Value: WideString);
    procedure DefaultDrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
    procedure DefaultDrawCellText(ARect: TRect; AState: TGridDrawState; const Text: WideString);
    procedure DefaultDrawCellTextEx(ARect: TRect; AState: TGridDrawState; const Text: WideString;
      DX, DY: Integer; Alignment: TAlignment; ARightToLeft: Boolean);
  published
    property Editor: TWinControl read FEditor write SetEditor;
    property EditorOffset: Integer read FEditorOffset write FEditorOffset default DEFAULT_EDITOR_OFFSET;
    property OnUpdateEditorFromGrid: TTntSetEditEvent read FOnUpdateEditorFromGrid write FOnUpdateEditorFromGrid;
    property OnGetEditorMargin: TGetEditorMarginEvent read FGetEditorMargin write FGetEditorMargin;
  end;

  ITntStringGridLXEditor = interface
    ['{80604654-E52E-47FC-B2AD-088073C2C9D0}']
    procedure SetGrid(Grid: TTntStringGridLX);
    procedure UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
    function GetMargin(Side: TAnchorKind): Integer;
  end;

  TTntStringGridLXEdit = class(TTntEdit, ITntStringGridLXEditor)
  private
    FGrid: TTntStringGridLX;
    procedure SetGrid(Grid: TTntStringGridLX);
    procedure UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
    function GetMargin(Side: TAnchorKind): Integer;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
  end;

  TTntStringGridLXComboBox = class(TTntComboBoxLX_Action, ITntStringGridLXEditor)
  private
    FGrid: TTntStringGridLX;
    procedure SetGrid(Grid: TTntStringGridLX);
    procedure UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
    function GetMargin(Side: TAnchorKind): Integer;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure Change; override;
    procedure Select; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTntStringGridLXDatePicker = class(TTntDateTimePicker, ITntStringGridLXEditor)
  private
    FGrid: TTntStringGridLX;
    procedure SetGrid(Grid: TTntStringGridLX);
    procedure UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
    function GetMargin(Side: TAnchorKind): Integer;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  private
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
  protected
    function MsgSetDateTime(Value: TSystemTime): Boolean; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char{TNT-ALLOW Char}); override;
  end;

procedure FitGridColumn(Grid: TCustomGrid; Col: Integer);

implementation

uses
  TntGraphics, Graphics, StdCtrls, Tntclasses, CommCtrl, TntMenus, Menus,
  TntSysUtils;

type
  THackCustomGrid = class(TCustomGrid);

procedure FitGridColumn(Grid: TCustomGrid; Col: Integer);
var
  SumOtherColWidths: Integer;
  i: integer;
  HackGrid: THackCustomGrid;
begin
  HackGrid := THackCustomGrid(Grid);
  SumOtherColWidths := 0;
  for i := 0 to HackGrid.ColCount - 1 do begin
    if i <> Col then
      Inc(SumOtherColWidths, HackGrid.ColWidths[i]);
    if (HackGrid.Options * [goVertLine, goFixedVertLine] <> []) then
      Inc(SumOtherColWidths, 1);
  end;
  HackGrid.ColWidths[Col] := HackGrid.ClientWidth - SumOtherColWidths;
end;

type
  TCoolGridInplaceEdit = class(TTntInplaceEdit)
  private
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; override;
    procedure UpdateContents; override;
    function CoolGrid: TTntStringGridLX;
  end;

{ TTntStringGridLX }

constructor TTntStringGridLX.Create(AOwner: TComponent);
begin
  inherited;
  FEditorOffset := DEFAULT_EDITOR_OFFSET;
end;

destructor TTntStringGridLX.Destroy;
begin
  Destroying;
  Editor := nil;
  inherited;
end;

function TTntStringGridLX.CreateEditor: TInplaceEdit{TNT-ALLOW TInplaceEdit};
begin
  result := TCoolGridInplaceEdit.Create(Self);
end;

procedure TTntStringGridLX.DoUpdateEditorFromGrid;
var
  CoolEditor: ITntStringGridLXEditor;
begin
  if (FEditor <> nil) and FEditor.GetInterface(ITntStringGridLXEditor, CoolEditor) then
    CoolEditor.UpdateEditorFromGrid(Col, Row, Cells[Col, Row]);
  if Assigned(OnUpdateEditorFromGrid) then
    OnUpdateEditorFromGrid(Self, Col, Row, Cells[Col, Row])
end;

function TTntStringGridLX.GetEditLimit: Integer;
begin
  if Editor <> nil then
    result := -1 { can't type into editor }
  else
    result := inherited GetEditLimit;
end;

procedure TTntStringGridLX.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then
    FEditor := nil;
end;

procedure TTntStringGridLX.SetEditor(const Value: TWinControl);
var
  CoolEditor: ITntStringGridLXEditor;
begin
  if (csDesigning in ComponentState) then
    FEditor := Value
  else begin
    if FEditor <> nil then begin
      FEditor.RemoveFreeNotification(Self);
      FEditor.Hide; // hide previous editor
      if FEditor.GetInterface(ITntStringGridLXEditor, CoolEditor) then
        CoolEditor.SetGrid(nil);
    end;
    FEditor := Value;
    if (FEditor <> nil) then begin
      FEditor.FreeNotification(Self);
      if FEditor.GetInterface(ITntStringGridLXEditor, CoolEditor) then
        CoolEditor.SetGrid(Self);
      if EditorMode then
        DoUpdateEditorFromGrid;
    end;
    if not (csDestroying in ComponentState) then
      EditorMode := EditorMode; { refresh }
  end;
end;

procedure TTntStringGridLX.UpdateEditorBounds;
begin
  if (Editor <> nil)
  and (InplaceEditor <> nil) then begin
    Editor.Parent := InplaceEditor;
    Editor.Width  := InplaceEditor.Width  - (GetEditorMargin(akLeft) + GetEditorMargin(akRight));
    Editor.Height := InplaceEditor.Height - (GetEditorMargin(akTop)  + GetEditorMargin(akBottom));
    Editor.Left   := GetEditorMargin(akLeft);
    Editor.Top    := GetEditorMargin(akTop);
    if not Editor.Visible then
      Editor.Show;
    Editor.BringToFront;
  end;
end;

procedure TTntStringGridLX.UpdateGridFromEditor(Value: WideString);
begin
  if InplaceEditor <> nil then begin
    (InplaceEditor as TTntInplaceEdit).Text := Value;
    SetEditText(Col, Row, Value);
  end;
end;

procedure TTntStringGridLX.DefaultDrawCellTextEx(ARect: TRect; AState: TGridDrawState; const Text: WideString;
  DX, DY: Integer; Alignment: TAlignment; ARightToLeft: Boolean);
var
  Left: Integer;
begin
  if (Canvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
    ChangeBiDiModeAlignment(Alignment);
  case Alignment of
    taLeftJustify:
      Left := ARect.Left + DX;
    taRightJustify:
      Left := ARect.Right - WideCanvasTextWidth(Canvas, Text) - DX - 1;
  else { taCenter }
    Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
      - (WideCanvasTextWidth(Canvas, Text) shr 1);
  end;
  WideCanvasTextRect(Canvas, ARect, Left, ARect.Top + DY, Text);
end;

procedure TTntStringGridLX.DefaultDrawCellText(ARect: TRect; AState: TGridDrawState; const Text: WideString);
begin
  DefaultDrawCellTextEx(ARect, AState, Text, 2, 2, taLeftJustify, False);
end;

procedure TTntStringGridLX.DefaultDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  DefaultDrawCellText(ARect, AState, Cells[ACol, ARow]);
end;

procedure SafeSetFocus(Control: TWinControl);
begin
  if (Control <> nil)
  and (Control.CanFocus) then
    Control.SetFocus;
end;

procedure TTntStringGridLX.WmResetEditorFocus(var Msg: TMessage);
begin
  EditorMode := EditorMode;
  if (Editor <> nil) then
    SafeSetFocus(Editor)
  else
    SafeSetFocus(InplaceEditor);
end;

function TTntStringGridLX.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  PostMessage(Handle, WM_RESET_EDITOR_FOCUS, 0, 0);
end;

function TTntStringGridLX.GetEditorMargin(Side: TAnchorKind): Integer;
var
  CoolEditor: ITntStringGridLXEditor;
begin
  if (Editor <> nil) and Editor.GetInterface(ITntStringGridLXEditor, CoolEditor) then
    Result := CoolEditor.GetMargin(Side)
  else begin
    Result := EditorOffset;
    if Assigned(OnGetEditorMargin) then
      OnGetEditorMargin(Side, Result);
  end;
end;

{ TCoolGridInplaceEdit }

function TCoolGridInplaceEdit.CoolGrid: TTntStringGridLX;
begin
  if Grid = nil then
    raise ETntInternalError.Create('Internal Error: Grid not assigned.');
  result := Grid as TTntStringGridLX;
end;

procedure TCoolGridInplaceEdit.BoundsChanged;
begin
  inherited;
  CoolGrid.UpdateEditorBounds;
end;

procedure TCoolGridInplaceEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  PostMessage(Grid.Handle, WM_RESET_EDITOR_FOCUS, 0, 0);
end;

procedure TCoolGridInplaceEdit.UpdateContents;
begin
  inherited;
  CoolGrid.DoUpdateEditorFromGrid;
end;

procedure TCoolGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    (Grid as TTntStringGridLX).KeyDown(Key, Shift);
  inherited;
end;

// -----

function Handle_Cool_KeyDown(Grid: TTntStringGridLX; var Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  if Grid <> nil then begin
    if (Key in [VK_TAB, VK_UP, VK_DOWN, VK_RETURN, VK_ESCAPE]) then
      Grid.KeyDown(Key, Shift);
  end;
end;

function Handle_Cool_KeyPress(Grid: TTntStringGridLX; var Key: Char{TNT-ALLOW Char}): Boolean;
begin
  Result := False;
  if Key in [AnsiChar(VK_TAB), AnsiChar(VK_RETURN)] then begin
    Result := True;
    Key := #0
  end;
end;

{ TTntStringGridLXEdit }

procedure TTntStringGridLXEdit.SetGrid(Grid: TTntStringGridLX);
begin
  FGrid := Grid;
end;

procedure TTntStringGridLXEdit.Change;
begin
  if FGrid <> nil then
    FGrid.UpdateGridFromEditor(Text);
  inherited;
end;

procedure TTntStringGridLXEdit.UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
begin
  Text := Value;
end;

procedure TTntStringGridLXEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TTntStringGridLXEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Handle_Cool_KeyDown(FGrid, Key, Shift) then
    inherited;
end;

procedure TTntStringGridLXEdit.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  if not Handle_Cool_KeyPress(FGrid, Key) then
    inherited;
end;

function TTntStringGridLXEdit.GetMargin(Side: TAnchorKind): Integer;
begin
  Result := -1;
end;

{ TTntStringGridLXComboBox }

constructor TTntStringGridLXComboBox.Create(AOwner: TComponent);
begin
  inherited;
  BorderWidth := 0;
end;

procedure TTntStringGridLXComboBox.UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
begin
  if Style in [csSimple, csDropDown] then
    Text := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

procedure TTntStringGridLXComboBox.SetGrid(Grid: TTntStringGridLX);
begin
  FGrid := Grid;
end;

procedure TTntStringGridLXComboBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

function TTntStringGridLXComboBox.GetMargin(Side: TAnchorKind): Integer;
begin
  Result := -1;
end;

procedure TTntStringGridLXComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (DroppedDown and (Key in [VK_RETURN, VK_UP, VK_DOWN, VK_ESCAPE]))
  or ((not DroppedDown) and (Key = VK_DOWN) and (Shift = [ssAlt])) then
    inherited
  else if not Handle_Cool_KeyDown(FGrid, Key, Shift) then
    inherited;
end;

procedure TTntStringGridLXComboBox.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  if DroppedDown and (Word(Key) in [VK_RETURN]) then
    inherited
  else if not Handle_Cool_KeyPress(FGrid, Key) then
    inherited;
end;

procedure TTntStringGridLXComboBox.Change;
begin
  if FGrid <> nil then
    FGrid.UpdateGridFromEditor(Text);
  inherited;
end;

procedure TTntStringGridLXComboBox.Select;
begin
  if FGrid <> nil then
    FGrid.UpdateGridFromEditor(Text);
  inherited;
end;

{ TTntStringGridLXDatePicker }

function TTntStringGridLXDatePicker.MsgSetDateTime(Value: TSystemTime): Boolean;
begin
  Result := inherited MsgSetDateTime(Value);
  if Win32PlatformIsVista then
    Result := True; // it reports failure when it shouldn't
end;

procedure TTntStringGridLXDatePicker.SetGrid(Grid: TTntStringGridLX);
begin
  FGrid := Grid;
end;

procedure TTntStringGridLXDatePicker.Change;
begin
  if FGrid <> nil then
    FGrid.UpdateGridFromEditor(DateToStr(Date));
  inherited;
end;

procedure TTntStringGridLXDatePicker.UpdateEditorFromGrid(ACol, ARow: Integer; const Value: WideString);
var
  NewDate: TTntDate;
begin
  if ValidDateStr(Value) then begin
    NewDate := GetTntDate(TntStrToDate(Value));
    if  ((MinDate = 0) or (NewDate >= MinDate))
    and ((MaxDate = 0) or (NewDate <= MaxDate)) then
      Self.Date := NewDate
  end;
end;

procedure TTntStringGridLXDatePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TTntStringGridLXDatePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Handle_Cool_KeyDown(FGrid, Key, Shift) then
    inherited;
end;

procedure TTntStringGridLXDatePicker.KeyPress(var Key: Char{TNT-ALLOW Char});
begin
  if not Handle_Cool_KeyPress(FGrid, Key) then
    inherited;
end;

function TTntStringGridLXDatePicker.GetMargin(Side: TAnchorKind): Integer;
begin
  Result := -1;
end;

procedure TTntStringGridLXDatePicker.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_SUBTRACT:
      if (DateSeparator <> '-') then
        Message.CharCode := 0; // date is decremented w/ WM_CHAR '-'
    VK_ADD:
      if (DateSeparator <> '+') then
        Message.CharCode := 0; // date is incremented w/ WM_CHAR '+'
  end;
  inherited;
end;

procedure TTntStringGridLXDatePicker.WMChar(var Message: TWMChar);
begin
  if Message.CharCode <> Word(DateSeparator) then begin
    // safe to hook into
    case AnsiChar(Message.CharCode) of
      '-':
        begin
          if (MinDate = 0) or (Date >= MinDate + 1) then
            Date := Date - 1
          else
            DateTime := MinDate;
          Change;
          Message.CharCode := 0;
        end;
      '=', '+':
        begin
          if (MaxDate = 0) or (Date <= MaxDate - 1) then
            Date := Date + 1
          else
            DateTime := MaxDate;
          Change;
          Message.CharCode := 0;
        end;
    end;
  end;
  inherited;
end;

end.
