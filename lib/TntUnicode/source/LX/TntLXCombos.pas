
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXCombos;

{$INCLUDE TntCompilers.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Contnrs, DB, Controls, Forms,
  ComCtrls, StdCtrls, DBctrls, TntStdCtrls, TntDBCtrls, TntClasses, TntLXUtils;

type
  TTntComboLXItem = class;
  ITntComboBoxLx = interface;

  TTntComboLXItems = class(TOwnedCollection)
  private
    FIgnoreNotify: Integer;
    function Combo: ITntComboBoxLx;
    function GetItem(Index: Integer): TTntComboLXItem;
    procedure SetItem(Index: Integer; const Value: TTntComboLXItem);
    function GetStrings: TTntStrings;
    procedure SetStrings(const Value: TTntStrings);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function Add: TTntComboLXItem; overload;
    function Add(const Caption: WideString): TTntComboLXItem; overload;
    function Add(const Caption: WideString;
      _DisplayValues: array of WideString): TTntComboLXItem; overload;
    function Insert(Index: Integer): TTntComboLXItem;
    property Items[Index: Integer]: TTntComboLXItem read GetItem write SetItem; default;
    property Strings: TTntStrings read GetStrings write SetStrings;
  end;

  TTntComboLXItem = class(TCollectionItem)
  private
    FObject: TObject;
    FDisplayValues: TTntStrings;
    FDestroying: Boolean;
    FLineAbove: Boolean;
    FLineBelow: Boolean;
    FIsActionItem: Boolean;
    FActionCaption: WideString;
    FActionShortCut: TShortCut;
    function CollectionLx: TTntComboLXItems;
    function Combo: ITntComboBoxLx;
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    procedure SetDisplayValues(const Value: TTntStrings);
    procedure DisplayValuesChanged(Sender: TObject);
    procedure SetLineAbove(const Value: Boolean);
    procedure SetLineBelow(const Value: Boolean);
    procedure SetIsActionItem(const Value: Boolean);
    procedure SetActionCaption(const Value: WideString);
    procedure SetActionShortCut(const Value: TShortCut);
  protected
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure MakeAction(const ACaption: WideString; AShortCut: TShortCut = scNone);
    property AObject: TObject read FObject write FObject;
  published
    property IsActionItem: Boolean read FIsActionItem write SetIsActionItem;
    property ActionCaption: WideString read FActionCaption write SetActionCaption;
    property ActionShortCut: TShortCut read FActionShortCut write SetActionShortCut;
    property Caption: WideString read GetCaption write SetCaption;
    property DisplayValues: TTntStrings read FDisplayValues write SetDisplayValues;
    property LineAbove: Boolean read FLineAbove write SetLineAbove;
    property LineBelow: Boolean read FLineBelow write SetLineBelow;
  end;

  ITntComboBoxLx = interface
    ['{FA4A1883-D7D5-4FD7-9679-00700297D41A}']
    procedure BeginGetOrSetLxObject;
    procedure EndGetOrSetLxObject;
    function InGetOrSetLxObject: Boolean;
    procedure InheritedWndProc(var Message: TMessage);
    procedure RefreshTabWidths;
    function GetItems: TTntStrings;
    function GetItemsLX: TTntComboLXItems;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetStyle: TComboBoxStyle;
    procedure SetStyle(Value: TComboBoxStyle);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetHasPendingRefreshTabWidths: Boolean;
    procedure SetHasPendingRefreshTabWidths(const Value: Boolean);
    //--
    property Items: TTntStrings read GetItems;
    property ItemsLX: TTntComboLXItems read GetItemsLX;
    property Text: WideString read GetText write SetText;
    property Style: TComboBoxStyle read GetStyle write SetStyle;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property HasPendingRefreshTabWidths: Boolean read GetHasPendingRefreshTabWidths write SetHasPendingRefreshTabWidths;
  end;

  TTntComboBoxLX = class(TTntComboBox, ITntComboBoxLx, ITntComboFindString)
  private
    FGetOrSetLxObject: Integer;
    procedure BeginGetOrSetLxObject;
    procedure EndGetOrSetLxObject;
    function InGetOrSetLxObject: Boolean;
  private
    FItemsLX: TTntComboLXItems;
    FHasPendingRefreshTabWidths: Boolean;
    function GetItems: TTntStrings;
    function GetItemsLX: TTntComboLXItems;
    procedure SetItemsLX(const Value: TTntComboLXItems);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetStyle: TComboBoxStyle;
    function GetHasPendingRefreshTabWidths: Boolean;
    procedure SetHasPendingRefreshTabWidths(const Value: Boolean);
  private
    FAutoCompleteUniqueOnly: Boolean;
    FTabWidths: TIntegerDynArray;
  private
    procedure InheritedWndProc(var Message: TMessage);
    procedure RefreshTabWidths;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    function FindString(const Value: WideString; StartPos: Integer): Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditCharMsg(var Message: TWMChar); override;
    function GetAutoComplete_UniqueMatchOnly: Boolean; override;
    function GetAutoComplete_PreserveDataEntryCase: Boolean; override;
    function GetItemHt: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
  published
    property BorderWidth default 0;
    property Items stored False;
    property ItemHeight default 13;
    property AutoCompleteUniqueOnly: Boolean read FAutoCompleteUniqueOnly write FAutoCompleteUniqueOnly default False;
    property ItemsLX: TTntComboLXItems read GetItemsLX write SetItemsLX;
  end;

  TTntBaseDBComboBoxLX = class(TTntDBComboBox)
  private
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
  private
    FDataLink: TFieldDataLink;
    FInheritedEditingChange: TNotifyEvent;
    procedure EditingChange(Sender: TObject);
    procedure FixEditReadOnly;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTntDBComboBoxLX = class(TTntBaseDBComboBoxLX, ITntComboBoxLx, ITntComboFindString)
  private
    FGetOrSetLxObject: Integer;
    procedure BeginGetOrSetLxObject;
    procedure EndGetOrSetLxObject;
    function InGetOrSetLxObject: Boolean;
  private
    FItemsLX: TTntComboLXItems;
    FHasPendingRefreshTabWidths: Boolean;
    function GetItems: TTntStrings;
    function GetItemsLX: TTntComboLXItems;
    procedure SetItemsLX(const Value: TTntComboLXItems);
    function GetText: WideString;
    procedure SetText(const Value: WideString);
    function GetStyle: TComboBoxStyle;
    function GetHasPendingRefreshTabWidths: Boolean;
    procedure SetHasPendingRefreshTabWidths(const Value: Boolean);
  private
    FAutoCompleteUniqueOnly: Boolean;
    FTabWidths: TIntegerDynArray;
  private
    procedure InheritedWndProc(var Message: TMessage);
    procedure RefreshTabWidths;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    function FindString(const Value: WideString; StartPos: Integer): Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditCharMsg(var Message: TWMChar); override;
    function GetAutoComplete_UniqueMatchOnly: Boolean; override;
    function GetAutoComplete_PreserveDataEntryCase: Boolean; override;
    function GetItemHt: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
    destructor Destroy; override;
  published
    property BorderWidth default 0;
    property Items stored False;
    property ItemHeight default 13;
    property AutoCompleteUniqueOnly: Boolean read FAutoCompleteUniqueOnly write FAutoCompleteUniqueOnly default False;
    property ItemsLX: TTntComboLXItems read GetItemsLX write SetItemsLX;
  end;

procedure ComboLX_AfterInherited_CreateParams(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var Params: TCreateParams);
procedure ComboLX_AfterInherited_CNMeasureItem(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var Message: TWMMeasureItem);
procedure UpdateTabWidthArray(var FTabWidths: TIntegerDynArray; Control: TWinControl; ListValues: TTntStrings);
procedure UpdateDroppedWidth(Control: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const FTabWidths: TIntegerDynArray);
procedure DefaultDrawLookupItem_Simple(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState; Value: WideString);
procedure DefaultDrawLookupItem(Canvas: TCanvas; FTabWidths: TIntegerDynArray; Rect: TRect;
  State: TOwnerDrawState; ListValues: TTntStrings);
function ComboDrawLookupItem(Canvas: TCanvas; FTabWidths: TIntegerDynArray; OnDrawItem: TDrawItemEvent;
  Index: Integer; Rect: TRect; State: TOwnerDrawState; ListValues: TTntStrings): Boolean;
procedure CheckForUnderlineAfterDrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  ItemsLX: TtntComboLXItems);
procedure CoolPopulateLookupComboBox(ComboBox: TTntComboBoxLX;
  ListSource: TDataSet; const ListFields: WideString; ListFieldIndex: Integer = 0);
function ComboLx_GetItemHt(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; InheritedGetItemHt: Integer): Integer;
function ComboLx_GetItemHtForDefaultFont: Integer;
function ComboLx_GetItemHtForFont(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;

  //===================================================================
  //  BEGIN:  Action Combo Box LX
  //
  //  TODO:   1. Refactor this into TTntComboBoxLX / TTntDBComboBoxLX.
  //          2. Support TBasicAction to get Caption, Shortcut, Enabled, etc.
  //          3. Change OnActionItem to have a TTntComboBoxLXItem parameter.
  //
  //===================================================================

type
  TTntComboBoxLX_Action = class(TTntComboBoxLX)
  private
    UseSavedText: Boolean;
    SavedText: WideString;
    FOnActionItem: TIntegerEvent;
    FLastNonActionIndex: Integer;
    procedure UseLastNonActionIndex;
    function IndexOfActionShortCut(ShortCut: TShortCut): Integer;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure CloseUp; override;
  protected
    function IndexIsForAction(Index: Integer): Boolean;
    procedure ExecuteActionItem(Index: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnActionItem: TIntegerEvent read FOnActionItem write FOnActionItem;
  end;

  //===================================================================
  //  END:  Action Combo Box LX
  //===================================================================

implementation

uses
  Math, TntControls, TntGraphics, TntDB, TntSysUtils, TntMenus, TntWindows, Dialogs;

type
  TAccessCustomComboBox = class(TCustomComboBox{TNT-ALLOW TCustomComboBox});

//-------------------------

function UnCastLxObject(Obj: Integer): TTntComboLXItem;
begin
  Result := nil;
  if Obj <> CB_ERR then
    Result := TTntComboLXItem(Obj);
  Assert(Result <> nil, 'TNT Internal Error: UnCastLxObject returned nil.');
end;

function GetLxObject(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; Index: Integer): TTntComboLXItem;
var
  Obj: Integer;
  ComboLx: ITntComboBoxLx;
begin
  ForceGetInterface(Combo, 'Combo', ITntComboBoxLx, 'ITntComboBoxLx', ComboLX);
  ComboLx.BeginGetOrSetLxObject;
  try
    Obj := SendMessage(Combo.Handle, CB_GETITEMDATA, Index, 0);
    Result := UnCastLxObject(Obj);
  finally
    ComboLx.EndGetOrSetLxObject;
  end;
end;

var
  WM_TntCalcTabWidths: Cardinal;

procedure ComboLX_WndProc(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  var Message: TMessage);
var
  LxObj: TTntComboLXItem;
  DelRec: PDELETEITEMSTRUCT;
  ComboLx: ITntComboBoxLx;
begin
  ForceGetInterface(Combo, 'Combo', ITntComboBoxLx, 'ITntComboBoxLx', ComboLX);

  // when font changes, auto change ItemHeight...
  if (Message.Msg = CM_FONTCHANGED)
  and (not (TAccessCustomComboBox(Combo).Style in [csOwnerDrawFixed, csOwnerDrawVariable]))
  and (not (csLoading in Combo.ComponentState)) then
    TAccessCustomComboBox(Combo).ItemHeight := ComboLx_GetItemHtForFont(Combo);

  Inc(ComboLx.ItemsLX.FIgnoreNotify);
  try
    // deal with setting an object
    if (Message.Msg = CB_SETITEMDATA) then begin
      if (not ComboLx.InGetOrSetLxObject) then begin
        // set object (translate)
        LxObj := GetLxObject(Combo, Message.WParam);
        LxObj.FObject := TObject(Message.LParam);
        Message.LParam := Integer(LxObj);
      end else begin
        // update index
        LxObj := UnCastLxObject(Message.LParam);
        LxObj.Index := Message.WParam;
      end;
    end;
    // delete string
    if (Message.Msg = WM_DELETEITEM) and (not ComboLx.InGetOrSetLxObject) then begin
      DelRec := PDELETEITEMSTRUCT(Message.LParam);
      LxObj := GetLxObject(Combo, DelRec.itemID); { UnCastLxObject(DelRec.itemData) doesn't work NT4! }
      if not LxObj.FDestroying then begin
        LxObj.Free;
      end;
    end;
  finally
    Dec(ComboLx.ItemsLX.FIgnoreNotify);
  end;

  if (Message.Msg = WM_PAINT)
  or (Message.Msg = WM_DRAWITEM)
  or (Message.Msg = CN_DRAWITEM) then begin
    if ComboLX.HasPendingRefreshTabWidths then
      ComboLx.RefreshTabWidths
  end;

  ComboLx.InheritedWndProc(Message);

  Inc(ComboLx.ItemsLX.FIgnoreNotify);
  try
    // Add / Insert string
    if ((Message.Msg = CB_ADDSTRING) or (Message.Msg = CB_INSERTSTRING))
    and (Message.Result <> CB_ERR) and (not ComboLx.InGetOrSetLxObject) then begin
      // add LX object
      ComboLx.BeginGetOrSetLxObject;
      try
        LxObj := ComboLx.ItemsLX.Add;
        LxObj.Index := Message.Result;
        SendMessage(Combo.Handle, CB_SETITEMDATA, Message.Result, Integer(LxObj));
      finally
        ComboLx.EndGetOrSetLxObject;
      end;
    end;

    if (not ComboLx.InGetOrSetLxObject) then begin
      // get object (translate)
      if (Message.Msg = CB_GETITEMDATA)
      and (Message.Result <> CB_ERR) then begin
        LxObj := UnCastLxObject(Message.Result);
        Message.Result := Integer(LxObj.FObject);
      end;
    end;

    // calc tab widths
    if (Message.Msg = WM_TntCalcTabWidths) and (ComboLx.HasPendingRefreshTabWidths) then
    begin
      ComboLx.RefreshTabWidths;
    end;
  finally
    Dec(ComboLx.ItemsLX.FIgnoreNotify);
  end;
end;

procedure ComboLX_AfterInherited_CNMeasureItem(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    if (Integer(itemID) = -1)
    and (TAccessCustomComboBox(Combo).Style <> csOwnerDrawVariable) then begin
      if Is_WINE then
        itemHeight := itemHeight - 2// + (TAccessCustomComboBox(Combo).BorderWidth * 2); { makes combo box a little taller to accomodate border }
      else
        itemHeight := itemHeight + 2;// + (TAccessCustomComboBox(Combo).BorderWidth * 2); { makes combo box a little taller to accomodate border }
    end;
  end;
end;

var
  LastCombo: TCustomComboBox{TNT-ALLOW TCustomComboBox} = nil;
  LastTypeTime: Cardinal;
  TempAllowCAPSChange: Boolean = False;

procedure HandleKeyDownForCustomComboBox(CB: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) or (Key = VK_BACK) then
    // delete or backspace
    TempAllowCAPSChange := True
  else if TempAllowCAPSChange then
    // extend this mode only if same combo, and it hasn't been 7.5 sec since last key down
    TempAllowCAPSChange := (LastCombo = CB) and (GetTickCount - LastTypeTime < 7500);

  LastCombo := CB;
  LastTypeTime := GetTickCount;
end;

function TntComboLX_FindString(Combo: ITntComboBoxLx; const Value: WideString; StartPos: Integer): Integer;

  function WordMatches(Idx: Integer): Boolean;
  var
    ThisWord: WideString;
  begin
    with Combo do begin
      if (ItemsLX[Idx].DisplayValues.Count = 0) then
        ThisWord := ItemsLX[Idx].Caption
      else
        ThisWord := ItemsLX[Idx].DisplayValues[0];
    end;
    Result := (WideTextPos(Value, ThisWord) = 1);
  end;

var
  i: integer;
begin
  Result := CB_ERR;
  with Combo do begin
    if ItemsLX.Count > 0 then begin
      StartPos := Min(StartPos, ItemsLX.Count - 1);
      StartPos := Max(StartPos, -1);
      for i := StartPos + 1 to ItemsLX.Count - 1 do begin
        if WordMatches(i) then begin
          Result := i;
          break;
        end;
      end;
      if Result = CB_ERR then begin
        for i := 0 to StartPos do begin
          if WordMatches(i) then begin
            Result := i;
            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure HandleWmSetFocus;
begin
  TempAllowCAPSChange := False;
end;

type
  TAccessWinControl = class(TWinControl);

procedure PreHandleKeyPressForCustomComboBox(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox};
  var Message: TWMChar);
var
  CB: ITntComboBoxLx;
  Key: WideChar;
begin
  if (TAccessCustomComboBox(Combo).Style = csSimple)
  and (Message.CharCode = VK_ESCAPE) then begin
    // for some reason, the escape key just seems to die with Style = csSimple.
    PostMessage(Combo.Parent.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
    PostMessage(Combo.Parent.Handle, WM_KEYUP, VK_ESCAPE, 0);
  end;
  ForceGetInterface(Combo, 'Combo', ITntComboBoxLx, 'ITntComboBoxLx', CB);
  if Combo.AutoComplete then begin
    Key := GetWideCharFromWMCharMsg(Message);
    if (Key in [WideChar('-'), WideChar(':'), WideChar('.')]) then begin
      // special char shortcuts
      if (Pos(Key, TntCombo_GetSelText(Combo)) <> 0) then begin
        TntCombo_SetSelStart(Combo, TntCombo_GetSelStart(Combo) + Pos(Key, TntCombo_GetSelText(Combo)) - 1);
        TntCombo_SetSelLength(Combo, Length(TntControl_GetText(Combo)) - TntCombo_GetSelStart(Combo));
      end;
    end;
  end;
end;

procedure UpdateTabWidthArray(var FTabWidths: TIntegerDynArray; Control: TWinControl; ListValues: TTntStrings);
var
  hWnd: THandle;
  hDC: THandle;

  procedure UpdateTabWidthCount(Count: Integer);
  var
    OldCount: Integer;
    NewCount: Integer;
    i: integer;
  begin
    OldCount := Length(FTabWidths);
    NewCount := Max(1, Count);
    if OldCount < NewCount then begin
      SetLength(FTabWidths, NewCount);
      for i := OldCount to NewCount - 1 do
        FTabWidths[i] := 0;
    end;
  end;

  function MyTextWidth(const Text: WideString): Integer;
  begin
    Result := WideDCTextExtent(hDC, Text).cx + 6 {margin};
  end;

var
  i: integer;
  NewValue: Integer;
  Changed: Boolean;
begin
  if Control <> nil then
    hWnd := Control.Handle
  else
    hWnd := 0;
  hDC := GetDC(hWnd);
  try
    SelectObject(hDC, TAccessWinControl(Control).Font.Handle);
    UpdateTabWidthCount(ListValues.Count);
    Changed := False;
    for i := 0 to ListValues.Count - 1 do begin
      NewValue := Max(30, Max(FTabWidths[i], MyTextWidth(ListValues[i])));
      if FTabWidths[i] <> NewValue then begin
        FTabWidths[i] := NewValue;
        Changed := True;
      end;
    end;
  finally
    ReleaseDC(hWnd, hDC);
  end;
  if Changed then
    Control.Invalidate;
end;

procedure UpdateDroppedWidth(Control: TCustomComboBox{TNT-ALLOW TCustomComboBox}; const FTabWidths: TIntegerDynArray);
var
  DroppedWidth: Integer;
  i: integer;
  ParentForm: TCustomForm;
  MaxWidth: Integer;

  function ScreenLeft(Control: TControl): Integer;
  begin
    Result := Control.ClientToScreen(Point(0, 0)).X;
  end;

begin
  DroppedWidth := 0;
  for i := 0 to Length(FTabWidths) - 1 do begin
    Inc(DroppedWidth, FTabWidths[i]);
  end;
  if TAccessCustomComboBox(Control).ItemCount > TAccessCustomComboBox(Control).DropDownCount then
    Inc(DroppedWidth, WinCheckH(GetSystemMetrics(SM_CXVSCROLL)));
  // determine max width
  ParentForm := GetParentForm(Control);
  if ParentForm = nil then
    MaxWidth := Control.ClientWidth
  else begin
    MaxWidth := ParentForm.ClientWidth - (ScreenLeft(Control) - ScreenLeft(ParentForm));
    if ScreenLeft(Control) + MaxWidth > Screen.Width then
      MaxWidth := Screen.Width - ScreenLeft(Control);
  end;
  // check bounds
  DroppedWidth := Min(DroppedWidth, MaxWidth); { not too wide }
  DroppedWidth := Max(DroppedWidth, Control.ClientWidth); {not too narrow }
  // set bounds
  if SendMessage(Control.Handle, CB_GETDROPPEDWIDTH, 0, 0) <> DroppedWidth then begin
    SendMessage(Control.Handle, CB_SETDROPPEDWIDTH, DroppedWidth, 0);
    Control.Invalidate;
  end;
end;

procedure DefaultDrawLookupItem_PrepareCanvas(Canvas: TCanvas; var Rect: TRect; State: TOwnerDrawState);
begin
  (Canvas as TControlCanvas).UpdateTextFlags;
  Canvas.FillRect(Rect);
  if odDisabled in State then
    Canvas.Font.Color := clGrayText;
  Inc(Rect.Left, 2);
  if odComboBoxEdit in State then begin
    Inc(Rect.Top, 1);
    Dec(Rect.Left, 1);
  end;
end;

procedure DefaultDrawLookupItem_Simple(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState; Value: WideString);
begin
  DefaultDrawLookupItem_PrepareCanvas(Canvas, Rect, State);
  WideCanvasTextOut(Canvas, Rect.Left, Rect.Top, Value);
end;

procedure DefaultDrawLookupItem(Canvas: TCanvas; FTabWidths: TIntegerDynArray; Rect: TRect;
  State: TOwnerDrawState; ListValues: TTntStrings);
var
  i: integer;
begin
  DefaultDrawLookupItem_PrepareCanvas(Canvas, Rect, State);
  Assert(Length(FTabWidths) >= ListValues.Count, 'TNT Internal Error: TabWidths not updated.');
  // draw fields
  for i := 0 to ListValues.Count - 1 do begin
    WideCanvasTextOut(Canvas, Rect.Left, Rect.Top, ListValues[i]);
    if i > 0 then begin
      Canvas.Pen.Color := clGray;
      Canvas.MoveTo(Rect.Left - 3, Rect.Top);
      Canvas.LineTo(Rect.Left - 3, Rect.Bottom);
    end;
    Inc(Rect.Left, FTabWidths[i]);
  end;
end;

function ComboDrawLookupItem(Canvas: TCanvas; FTabWidths: TIntegerDynArray; OnDrawItem: TDrawItemEvent;
  Index: Integer; Rect: TRect; State: TOwnerDrawState; ListValues: TTntStrings): Boolean;
begin
  if (Assigned(OnDrawItem))
  or (Index < 0)
  or (Length(FTabWidths) = 0) then
    Result := False
  else begin
    Result := True;
    DefaultDrawLookupItem(Canvas, FTabWidths, Rect, State, ListValues);
  end;
end;

procedure GetListValues(ListValues: TTntStrings; ItemsLX: TTntComboLXItems;
  Index: Integer; const FTabWidths: TIntegerDynArray);
begin
  if Index >= 0 then begin
    ListValues.Clear;
    if ItemsLX[Index].DisplayValues.Count > 0 then
      ListValues.AddStrings(ItemsLX[Index].DisplayValues)
    else
      ListValues.Add(ItemsLX[Index].Caption);
    // make sure there are enough items for tabs
    while ListValues.Count < Length(FTabWidths) do
      ListValues.Add('');
  end;
end;

procedure CalcTabWidths(Control: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var FTabWidths: TIntegerDynArray; ItemsLX: TTntComboLXItems);
var
  i: integer;
  ListValues: TTntStringList;
  ComboLX: ITntComboBoxLx;
begin
  SetLength(FTabWidths, 0);
  ListValues := TTntStringList.Create;
  try
    for i := 0 to ItemsLX.Count - 1 do begin
      GetListValues(ListValues, ItemsLX, i, FTabWidths);
      UpdateTabWidthArray(FTabWidths, Control, ListValues);
    end;
  finally
    ListValues.Free;
  end;
  ForceGetInterface(Control, 'Control', ITntComboBoxLx, 'ITntComboBoxLx', ComboLX);
  ComboLX.HasPendingRefreshTabWidths := False;
end;

procedure CheckForUnderlineAfterDrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  ItemsLX: TtntComboLXItems);
begin
  if (Index >= 0) and (Index < ItemsLX.Count - 1) then begin
    // not the very last item
    if ItemsLX[Index].LineBelow
    or ItemsLX[Index + 1].LineAbove then begin
      // underlined index
      Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
      Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
    end;
  end;
end;

function CoolComboDrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
  State: TOwnerDrawState; ItemsLX: TTntComboLXItems;
    const FTabWidths: TIntegerDynArray; OnDrawItem: TDrawItemEvent): Boolean;
var
  ListValues: TTntStringList;
begin
  ListValues := TTntStringList.Create;
  try
    if (Index >= 0) and (ItemsLX.Count > 0) then begin
      if (not ItemsLX[Index].IsActionItem) then
        GetListValues(ListValues, ItemsLX, Index, FTabWidths)
      else begin
        // is action item
        if ItemsLX[Index].ActionShortCut = scNone then
          ListValues.Add(ItemsLX[Index].ActionCaption)
        else
          ListValues.Add(WideFormat('%s (%s)',
            [ItemsLX[Index].ActionCaption, WideShortCutToText(ItemsLX[Index].ActionShortCut)]));
      end;
    end;
    Result := ComboDrawLookupItem(Canvas, FTabWidths, OnDrawItem, Index, Rect, State, ListValues);
    if not (odComboBoxEdit in State) then
      CheckForUnderlineAfterDrawItem(Canvas, Index, Rect, ItemsLX);
  finally
    ListValues.Free;
  end;
end;

procedure PopulateLookupComboBox(ItemsLX: TTntComboLXItems;
  ListSource: TDataSet; const ListFields: WideString; ListFieldIndex: Integer = 0);
var
  BM: TBookmarkStr;
  Fields: TList;
  Field: TField;
  i: integer;
begin
  Fields := TList.Create;
  try
    ListSource.GetFieldList(Fields, ListFields);
    if Fields.Count = 0 then
      raise ETntInternalError.CreateFmt('Internal Error: Invalid ListFields (%s).', [ListFields]);
    if (ListFieldIndex < 0) or (ListFieldIndex > Fields.Count - 1) then
      raise ETntInternalError.CreateFmt('Internal Error: Invalid ListFieldIndex (%d).', [ListFieldIndex]);
    ItemsLx.BeginUpdate;
    try
      ItemsLx.Clear;
      ListSource.DisableControls;
      try
        BM := ListSource.Bookmark;
        try
          ListSource.First;
          While not ListSource.Eof do begin
            with ItemsLX.Add do begin
              Caption := GetAsWideString(TField(Fields[ListFieldIndex]));
              DisplayValues.Clear;
              for i := 0 to Fields.Count - 1 do begin
                Field := TField(Fields[i]);
                DisplayValues.Add(GetAsWideString(Field));
              end;
            end;
            ListSource.Next;
          end;
        finally
          ListSource.Bookmark := BM;
        end;
      finally
        ListSource.EnableControls;
      end;
    finally
      ItemsLx.EndUpdate;
    end;
  finally
    Fields.Free;
  end;
end;

procedure CoolPopulateLookupComboBox(ComboBox: TTntComboBoxLX;
  ListSource: TDataSet; const ListFields: WideString; ListFieldIndex: Integer = 0);
var
  WasSorted: Boolean;
begin
  with ComboBox do begin
    WasSorted := Sorted;
    try
      Sorted := False; { ItemsLX.Add.Caption will not add a blank item and will then update the caption and thus will cause list to lose sort. }
      PopulateLookupComboBox(ItemsLX, ListSource, ListFields, ListFieldIndex);
    finally
      Sorted := WasSorted; { By toggling sorted from false to true, the list will be re-sorted. }
    end;
  end;
end;

procedure ComboLX_AfterInherited_CreateParams(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; var Params: TCreateParams);
begin
  { Ensure that the list portion is owner-drawn. }
  if ((Params.Style and CBS_OWNERDRAWVARIABLE) = 0) then
    Params.Style := Params.Style or CBS_OWNERDRAWFIXED;
end;

function ComboLx_GetItemHt(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}; InheritedGetItemHt: Integer): Integer;
var
  Message: TWMMeasureItem;
  Struct: TMeasureItemStruct;
begin
  if TAccessCustomComboBox(Combo).Style in [csOwnerDrawFixed, csOwnerDrawVariable] then
    Result := InheritedGetItemHt
  else begin
    // convoluted way to read FItemHeight...
    ZeroMemory(@Struct, SizeOf(Struct));
    Message.Msg := CN_MEASUREITEM;
    Message.IDCtl := Combo.Handle;
    Message.MeasureItemStruct := @Struct;
    Combo.Dispatch(Message);
    Result := Struct.itemHeight;
  end;
end;

function _ComboLx_GetItemHtForFontHandle(FontHandle: THandle): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, FontHandle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
  if Screen.PixelsPerInch > 96 then
    Inc(Result); // makes it work for default font of "Tahoma" and Large Fonts
end;

function ComboLx_GetItemHtForFont(Combo: TCustomComboBox{TNT-ALLOW TCustomComboBox}): Integer;
begin
  Result := _ComboLx_GetItemHtForFontHandle(TAccessCustomComboBox(Combo).Font.Handle);
end;

function ComboLx_GetItemHtForDefaultFont: Integer;
var
  TempFont: TFont;
begin
  TempFont := TFont.Create;
  try
    Result := _ComboLx_GetItemHtForFontHandle(TempFont.Handle);
  finally
    FreeAndNil(TempFont);
  end;
end;

{ TTntComboLXItems }

function TTntComboLXItems.Combo: ITntComboBoxLx;
begin
  ForceGetInterface(Owner, 'Owner', ITntComboBoxLx, 'ITntComboBoxLx', Result);
end;

function TTntComboLXItems.GetItem(Index: Integer): TTntComboLXItem;
begin
  Result := (inherited Items[Index]) as TTntComboLXItem;
end;

procedure TTntComboLXItems.SetItem(Index: Integer; const Value: TTntComboLXItem);
begin
  inherited Items[Index] := VAlue;
end;

procedure TTntComboLXItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  ComboLX: ITntComboBoxLx;
begin
  inherited;
  if (FIgnoreNotify = 0)
  and (not (csDestroying in (Owner as TComponent).ComponentState)) then begin
    // Added
    if Action = cnAdded then begin
      ForceGetInterface(Owner, 'Owner', ITntComboBoxLx, 'ITntComboBoxLx', ComboLX);
      ComboLX.BeginGetOrSetLxObject;
      try
        Combo.Items.InsertObject(Item.Index, '', Item);
      finally
        ComboLx.EndGetOrSetLxObject;
      end;
    end;
    // Extracting
    if Action = cnExtracting then begin
      Combo.Items.Delete(Item.Index); { remove assoc. string }
    end;
  end;
end;

procedure TTntComboLXItems.Update(Item: TCollectionItem);
var
  WinOwner: TWinControl;
  ComboLX: ITntComboBoxLx;
begin
  inherited;
  WinOwner := Owner as TWinControl;
  ForceGetInterface(Owner, 'Owner', ITntComboBoxLx, 'ITntComboBoxLx', ComboLX);
  if (not (csDestroying in WinOwner.ComponentState))
  and (WinOwner.HandleAllocated) then begin
    if not ComboLX.HasPendingRefreshTabWidths then
    begin
      PostMessage(WinOwner.Handle, WM_TntCalcTabWidths, 0, 0);
      ComboLX.HasPendingRefreshTabWidths := True;
    end;
  end;
end;

procedure TTntComboLXItems.BeginUpdate;
begin
  if not (csDestroying in (Owner as TComponent).ComponentState) then
    Combo.Items.BeginUpdate;
  inherited;
end;

procedure TTntComboLXItems.EndUpdate;
begin
  inherited;
  if not (csDestroying in (Owner as TComponent).ComponentState) then
    Combo.Items.EndUpdate;
end;

function TTntComboLXItems.Add: TTntComboLXItem;
begin
  Result := inherited Add as TTntComboLXItem;
end;

function TTntComboLXItems.Add(const Caption: WideString): TTntComboLXItem;
begin
  Result := Add;
  Result.Caption := Caption;
end;

function TTntComboLXItems.Add(const Caption: WideString; _DisplayValues: array of WideString): TTntComboLXItem;
var
  i: integer;
begin
  Result := Add(Caption);
  with Result do begin
    DisplayValues.Clear;
    for i := Low(_DisplayValues) to High(_DisplayValues) do
      DisplayValues.Add(_DisplayValues[i]);
  end;
end;

function TTntComboLXItems.Insert(Index: Integer): TTntComboLXItem;
begin
  Result := inherited Insert(Index) as TTntComboLXItem;
end;

function TTntComboLXItems.GetStrings: TTntStrings;
begin
  Result := Combo.Items;
end;

procedure TTntComboLXItems.SetStrings(const Value: TTntStrings);
begin
  Combo.Items.Assign(Value);
end;

{ TTntComboLXItem }

constructor TTntComboLXItem.Create(Collection: TCollection);
begin
  FDisplayValues := TTntStringList.Create;
  TTntStringList(FDisplayValues).OnChange := DisplayValuesChanged;
  inherited;
end;

destructor TTntComboLXItem.Destroy;
begin
  FDestroying := True;
  inherited;
  FreeAndNil(FDisplayValues);
end;

function TTntComboLXItem.CollectionLx: TTntComboLXItems;
begin
  Result := Collection as TTntComboLXItems;
end;

function TTntComboLXItem.Combo: ITntComboBoxLx;
begin
  Result := CollectionLx.Combo;
end;

function TTntComboLXItem.GetCaption: WideString;
begin
  Result := Combo.Items[Index];
end;

procedure TTntComboLXItem.SetCaption(const Value: WideString);
begin
  (Combo as ITntComboBoxLx).BeginGetOrSetLxObject;
  try
    Combo.Items[Index] := Value;
  finally
    (Combo as ITntComboBoxLx).EndGetOrSetLxObject;
  end;
end;

procedure TTntComboLXItem.SetDisplayValues(const Value: TTntStrings);
begin
  FDisplayValues.Assign(Value);
end;

procedure TTntComboLXItem.DisplayValuesChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TTntComboLXItem.SetIndex(Value: Integer);
var
  OldIndex: Integer;
begin
  if Collection.Items[Value] <> Self then begin
    OldIndex := Index;
    inherited;
    if (Collection as TTntComboLXItems).FIgnoreNotify = 0 then
      Combo.Items.Move(OldIndex, Index);
  end;
end;

procedure TTntComboLXItem.SetIsActionItem(const Value: Boolean);
begin
  if IsActionItem <> Value then begin
    FIsActionItem := Value;
    Changed(False);
  end;
end;

procedure TTntComboLXItem.SetActionCaption(const Value: WideString);
begin
  if ActionCaption <> Value then begin
    FActionCaption := Value;
    Changed(False);
  end;
end;

procedure TTntComboLXItem.SetActionShortCut(const Value: TShortCut);
begin
  if ActionShortCut <> Value then begin
    FActionShortCut := Value;
    Changed(False);
  end;
end;

procedure TTntComboLXItem.SetLineAbove(const Value: Boolean);
begin
  if LineAbove <> Value then begin
    FLineAbove := Value;
    Changed(False);
  end;
end;

procedure TTntComboLXItem.SetLineBelow(const Value: Boolean);
begin
  if LineBelow <> Value then begin
    FLineBelow := Value;
    Changed(False);
  end;
end;

procedure TTntComboLXItem.MakeAction(const ACaption: WideString; AShortCut: TShortCut);
begin
  IsActionItem := True;
  ActionCaption := ACaption;
  ActionShortCut := AShortCut;
end;

{ TTntComboBoxLX }

constructor TTntComboBoxLX.Create(AOwner: TComponent);
begin
  FItemsLX := TTntComboLXItems.Create(Self, TTntComboLXItem);
  inherited;
  BorderWidth := 0;
  ItemHeight := ComboLx_GetItemHtForDefaultFont;
end;

destructor TTntComboBoxLX.Destroy;
begin
  inherited;
  FreeAndNil(FItemsLX);
end;

procedure TTntComboBoxLX.BeginGetOrSetLxObject;
begin
  Inc(FGetOrSetLxObject);
end;

procedure TTntComboBoxLX.EndGetOrSetLxObject;
begin
  Dec(FGetOrSetLxObject);
end;

function TTntComboBoxLX.InGetOrSetLxObject: Boolean;
begin
  Result := FGetOrSetLxObject > 0;
end;

function TTntComboBoxLX.GetItemHt: Integer;
begin
  Result := ComboLx_GetItemHt(Self, inherited GetItemHt);
end;

function TTntComboBoxLX.GetItems: TTntStrings;
begin
  Result := Items;
end;

function TTntComboBoxLX.GetItemsLX: TTntComboLXItems;
begin
  Result := FItemsLX;
end;

procedure TTntComboBoxLX.SetItemsLX(const Value: TTntComboLXItems);
begin
  FItemsLX.Assign(Value);
end;

procedure TTntComboBoxLX.InheritedWndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
end;

procedure TTntComboBoxLX.RefreshTabWidths;
begin
  CalcTabWidths(Self, FTabWidths, ItemsLX);
end;

procedure TTntComboBoxLX.CNMeasureItem(var Message: TWMMeasureItem);
begin
  inherited;
  ComboLX_AfterInherited_CNMeasureItem(Self, Message);
end;

procedure TTntComboBoxLX.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  HandleWmSetFocus;
end;

procedure TTntComboBoxLX.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ComboLX_AfterInherited_CreateParams(Self, Params);
end;

procedure TTntComboBoxLX.CreateWnd;
begin
  BeginGetOrSetLxObject;
  try
    inherited;
    ItemsLX.Update(nil);
  finally
    EndGetOrSetLxObject;
  end;
end;

procedure TTntComboBoxLX.DestroyWnd;
begin
  BeginGetOrSetLxObject;
  try
    inherited;
  finally
    EndGetOrSetLxObject;
  end;
end;

procedure TTntComboBoxLX.WndProc(var Message: TMessage);
begin
  ComboLX_WndProc(Self, Message);
end;

procedure TTntComboBoxLX.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not CoolComboDrawItem(Canvas, Index, Rect, State, ItemsLX, FTabWidths, OnDrawItem) then
    inherited;
end;

procedure TTntComboBoxLX.DropDown;
begin
  RefreshTabWidths;
  UpdateDroppedWidth(Self, FTabWidths);
  inherited;
end;

procedure TTntComboBoxLX.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleKeyDownForCustomComboBox(Self, Key, Shift);
  inherited;
end;

procedure TTntComboBoxLX.DoEditCharMsg(var Message: TWMChar);
begin
  PreHandleKeyPressForCustomComboBox(Self, Message);
  inherited;
end;

function TTntComboBoxLX.GetAutoComplete_UniqueMatchOnly: Boolean;
begin
  Result := AutoCompleteUniqueOnly;
end;

function TTntComboBoxLX.GetAutoComplete_PreserveDataEntryCase: Boolean;
begin
  Result := TempAllowCAPSChange;
end;

procedure TTntComboBoxLX.DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  TntCombo_DefaultDrawItem(Canvas, Index, Rect, State, Items);
end;

function TTntComboBoxLX.FindString(const Value: WideString; StartPos: Integer): Integer;
begin
  Result := TntComboLX_FindString(Self, Value, StartPos);
end;

function TTntComboBoxLX.GetText: WideString;
begin
  Result := Text;
end;

procedure TTntComboBoxLX.SetText(const Value: WideString);
begin
  Text := Value;
end;

function TTntComboBoxLX.GetStyle: TComboBoxStyle;
begin
  Result := Style;
end;

function TTntComboBoxLX.GetHasPendingRefreshTabWidths: Boolean;
begin
  Result := FHasPendingRefreshTabWidths;
end;

procedure TTntComboBoxLX.SetHasPendingRefreshTabWidths(const Value: Boolean);
begin
  FHasPendingRefreshTabWidths := Value;
end;

{ TTntBaseDBComboBoxLX }

constructor TTntBaseDBComboBoxLX.Create(AOwner: TComponent);
begin
  inherited;
  // wire-up event
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FInheritedEditingChange := FDataLink.OnEditingChange;
  FDataLink.OnEditingChange := EditingChange;
end;

procedure TTntBaseDBComboBoxLX.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntBaseDBComboBoxLX.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntBaseDBComboBoxLX.WMUndo(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TTntBaseDBComboBoxLX.FixEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.CanModify), 0);
end;

procedure TTntBaseDBComboBoxLX.CreateWnd;
begin
  inherited;
  // fix
  FixEditReadOnly;
end;

procedure TTntBaseDBComboBoxLX.EditingChange(Sender: TObject);
begin
  if Assigned(FInheritedEditingChange) then
    FInheritedEditingChange(Sender);
  // fix
  FixEditReadOnly;
end;

{ TTntDBComboBoxLX }

constructor TTntDBComboBoxLX.Create(AOwner: TComponent);
begin
  FItemsLX := TTntComboLXItems.Create(Self, TTntComboLXItem);
  inherited;
  BorderWidth := 0;
  ItemHeight := ComboLx_GetItemHtForDefaultFont;
end;

destructor TTntDBComboBoxLX.Destroy;
begin
  inherited;
  FreeAndNil(FItemsLX);
end;

procedure TTntDBComboBoxLX.BeginGetOrSetLxObject;
begin
  Inc(FGetOrSetLxObject);
end;

procedure TTntDBComboBoxLX.EndGetOrSetLxObject;
begin
  Dec(FGetOrSetLxObject);
end;

function TTntDBComboBoxLX.InGetOrSetLxObject: Boolean;
begin
  Result := FGetOrSetLxObject > 0;
end;

function TTntDBComboBoxLX.GetItemHt: Integer;
begin
  Result := ComboLx_GetItemHt(Self, inherited GetItemHt);
end;

function TTntDBComboBoxLX.GetItems: TTntStrings;
begin
  Result := Items;
end;

function TTntDBComboBoxLX.GetItemsLX: TTntComboLXItems;
begin
  Result := FItemsLX;
end;

procedure TTntDBComboBoxLX.SetItemsLX(const Value: TTntComboLXItems);
begin
  FItemsLX.Assign(Value);
end;

procedure TTntDBComboBoxLX.InheritedWndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
end;

procedure TTntDBComboBoxLX.RefreshTabWidths;
begin
  CalcTabWidths(Self, FTabWidths, ItemsLX);
end;

procedure TTntDBComboBoxLX.CNMeasureItem(var Message: TWMMeasureItem);
begin
  inherited;
  ComboLX_AfterInherited_CNMeasureItem(Self, Message);
end;

procedure TTntDBComboBoxLX.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  HandleWmSetFocus;
end;

procedure TTntDBComboBoxLX.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ComboLX_AfterInherited_CreateParams(Self, Params);
end;

procedure TTntDBComboBoxLX.CreateWnd;
begin
  BeginGetOrSetLxObject;
  try
    inherited;
    ItemsLX.Update(nil);
  finally
    EndGetOrSetLxObject;
  end;
end;

procedure TTntDBComboBoxLX.DestroyWnd;
begin
  BeginGetOrSetLxObject;
  try
    inherited;
  finally
    EndGetOrSetLxObject;
  end;
end;

procedure TTntDBComboBoxLX.WndProc(var Message: TMessage);
begin
  ComboLX_WndProc(Self, Message);
end;

procedure TTntDBComboBoxLX.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not CoolComboDrawItem(Canvas, Index, Rect, State, ItemsLX, FTabWidths, OnDrawItem) then
    inherited;
end;

procedure TTntDBComboBoxLX.DropDown;
begin
  RefreshTabWidths;
  UpdateDroppedWidth(Self, FTabWidths);
  inherited;
end;

procedure TTntDBComboBoxLX.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleKeyDownForCustomComboBox(Self, Key, Shift);
  inherited;
end;

procedure TTntDBComboBoxLX.DoEditCharMsg(var Message: TWMChar);
begin
  PreHandleKeyPressForCustomComboBox(Self, Message);
  inherited;
end;

function TTntDBComboBoxLX.GetAutoComplete_UniqueMatchOnly: Boolean;
begin
  Result := AutoCompleteUniqueOnly;
end;

function TTntDBComboBoxLX.GetAutoComplete_PreserveDataEntryCase: Boolean;
begin
  Result := TempAllowCAPSChange;
end;

procedure TTntDBComboBoxLX.DefaultDrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  TntCombo_DefaultDrawItem(Canvas, Index, Rect, State, Items);
end;

function TTntDBComboBoxLX.FindString(const Value: WideString; StartPos: Integer): Integer;
begin
  Result := TntComboLX_FindString(Self, Value, StartPos);
end;

function TTntDBComboBoxLX.GetText: WideString;
begin
  Result := Text;
end;

procedure TTntDBComboBoxLX.SetText(const Value: WideString);
begin
  Text := Value;
end;

function TTntDBComboBoxLX.GetStyle: TComboBoxStyle;
begin
  Result := Style;
end;

function TTntDBComboBoxLX.GetHasPendingRefreshTabWidths: Boolean;
begin
  Result := FHasPendingRefreshTabWidths;
end;

procedure TTntDBComboBoxLX.SetHasPendingRefreshTabWidths(const Value: Boolean);
begin
  FHasPendingRefreshTabWidths := Value;
end;

{ TTntComboBoxLX_Action }

constructor TTntComboBoxLX_Action.Create(AOwner: TComponent);
begin
  inherited;
  FLastNonActionIndex := -1;
end;

function TTntComboBoxLX_Action.IndexIsForAction(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= ItemsLX.Count - 1) and ItemsLX[Index].IsActionItem;
end;

procedure TTntComboBoxLX_Action.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = CN_COMMAND)
  and (TWMCommand(Msg).NotifyCode = CBN_SELCHANGE) then begin
    if (IndexIsForAction(ItemIndex)) then begin
      if Style in [csDropDown, csSimple] then begin
        SavedText := Text;
        UseSavedText := True;
      end else
        UseLastNonActionIndex;
    end else begin
      FLastNonActionIndex := ItemIndex;
      inherited;
    end;
  end else if Msg.Msg = CB_SETCURSEL then begin
    inherited;
    if not (IndexIsForAction(ItemIndex)) then
      FLastNonActionIndex := ItemIndex;
  end else if (Msg.Msg = WM_KEYDOWN)
  and (IndexOfActionShortCut(MessageToShortCut(TWMKeyDown(Msg))) <> -1) then
    ExecuteActionItem(IndexOfActionShortCut(MessageToShortCut(TWMKeyDown(Msg))))
  else
    inherited;
end;

function TTntComboBoxLX_Action.IndexOfActionShortCut(ShortCut: TShortCut): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ItemsLX.Count - 1 do begin
    if  (ItemsLX[i].IsActionItem)
    and (ItemsLX[i].ActionShortCut <> scNone)
    and (ItemsLX[i].ActionShortCut = ShortCut) then begin
      Result := i;
      break;
    end;
  end;
end;

procedure TTntComboBoxLX_Action.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if (Message.Msg = WM_SETTEXT)
  and (Style in [csDropDown, csSimple])
  and (UseSavedText) then begin
    UseSavedText := False;
    Tnt_SetWindowTextW(ComboWnd, PWideChar(SavedText))
  end else if (Message.Msg = WM_KEYDOWN)
  and (IndexOfActionShortCut(MessageToShortCut(TWMKeyDown(Message))) <> -1) then
    ExecuteActionItem(IndexOfActionShortCut(MessageToShortCut(TWMKeyDown(Message))))
  else
    inherited;
end;

procedure TTntComboBoxLX_Action.UseLastNonActionIndex;
var
  i: integer;
begin
  if not (Style in [csDropDown, csSimple]) then begin
    if FLastNonActionIndex > Items.Count - 1 then
      FLastNonActionIndex := Items.Count - 1;
    if FLastNonActionIndex = -1 then
      ItemIndex := FLastNonActionIndex
    else begin
      // found first non-action item on or before FLastNonActionIndex
      for i := FLastNonActionIndex downto 0 do begin
        if not IndexIsForAction(i) then begin
          ItemIndex := i;
          exit; // found one!
        end;
      end;
      // find first non-action item after FLastNonActionIndex
      for i := FLastNonActionIndex + 1 to Items.Count - 1 do begin
        if not IndexIsForAction(i) then begin
          ItemIndex := i;
          exit; // found one!
        end;
      end;
    end;
  end;
end;

procedure TTntComboBoxLX_Action.CloseUp;
var
  ActionIndex: Integer;
begin
  inherited;
  if IndexIsForAction(ItemIndex) then begin
    ActionIndex := ItemIndex;
    UseLastNonActionIndex;
    ExecuteActionItem(ActionIndex);
  end;
end;

procedure TTntComboBoxLX_Action.ExecuteActionItem(Index: Integer);
begin
  if DroppedDown and (ItemIndex = Index) then
    DroppedDown := False
  else if Assigned(FOnActionItem) then
    FOnActionItem(Index);
end;

initialization
  WM_TntCalcTabWidths := RegisterWindowMessage('Tnt.ComboBoxLX.CalcTabWidths');

end.
