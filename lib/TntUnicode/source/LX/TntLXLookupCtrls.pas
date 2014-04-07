
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXLookupCtrls;

{$INCLUDE TntCompilers.inc}

interface

uses
  Types, Windows, Classes, DBCtrls, DB, TntStdCtrls, Messages, Controls, Graphics, TntClasses;

type
  TBookMarkStrList = class(TStringList{TNT-ALLOW TStringList})
  protected
    function CompareStrings(const S1, S2: string{TNT-ALLOW string}): Integer; override;
  end;

  TSelectItemEvent = procedure(const Item: WideString; Alignment: TAlignment) of object;

  TTntDBLookupControlLX = class(TDBLookupControl)
  private
    FWinOwner: TWinControl;
    FOldTabWidths: array of Integer;
    FTabWidths: TIntegerDynArray;
    FOnSelectItem: TSelectItemEvent;
    FBookMarks: TBookMarkStrList;
    FOnUpdateTabWidths: TNotifyEvent;
    procedure UpdateTabWidths;
    function CurrentListString: WideString;
    function GotoListIndex(ListIndex: Integer): Boolean;
    procedure DoOnUpdateTabWidths;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;    
  protected
    FListItems: TTntStrings;
    procedure UpdateListFields; override;
    procedure ListLinkDataChanged; override;
    procedure KeyValueChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectItem(const Item: WideString; ItemIndex: Integer = -1);
    property OnSelectItem: TSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnUpdateTabWidths: TNotifyEvent read FOnUpdateTabWidths write FOnUpdateTabWidths;
  end;

  TTntCustomDBLookupListBoxLX = class(TTntCustomListBox)
  private
    function GetKeyValue: Variant;
    procedure SetKeyValue(const Value: Variant);
    function GetField: TField;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetDataField: AnsiString;
    procedure SetDataField(const Value: AnsiString);
    function GetListSource: TDataSource;
    procedure SetListSource(const Value: TDataSource);
    function GetKeyField: AnsiString;
    procedure SetKeyField(const Value: AnsiString);
    function GetListField: AnsiString;
    procedure SetListField(const Value: AnsiString);
    function GetListFieldIndex: Integer;
    procedure SetListFieldIndex(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
    function GetRowCount: Integer;
    procedure SetRowCount(const Value: Integer);
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    function GetNullValueKey: TShortCut;
    procedure SetNullValueKey(const Value: TShortCut);
    function GetSelectedItem: WideString;
  protected
    FLookupControl: TTntDBLookupControlLX;
    procedure UpdateTabWidths(Sender: TObject);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure DoSelectItem(const Item: WideString; Alignment: TAlignment);
  protected
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: AnsiString read GetDataField write SetDataField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: AnsiString read GetKeyField write SetKeyField;
    property ListField: AnsiString read GetListField write SetListField;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property RowCount: Integer read GetRowCount write SetRowCount stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedItem: WideString read GetSelectedItem;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property Field: TField read GetField;
  end;

  TTntDBLookupListBoxLX = class(TTntCustomDBLookupListBoxLX)
  published
    property Style;
    property AutoComplete;
    //--
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
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
    property OnStartDock;
    property OnStartDrag;
  published
    property DataField;
    property DataSource;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property NullValueKey;
    property ReadOnly;
    property RowCount;
  end;

  _TTntInternalCustomDBLookupComboBoxLX = class(TTntCustomComboBox)
  protected
    procedure DoDropDown; virtual;
    procedure DropDown; override;
  end;

  TTntCustomDBLookupComboBoxLX = class(_TTntInternalCustomDBLookupComboBoxLX)
  private
    FProcessChangeOnCloseUp: Boolean;
    function GetKeyValue: Variant;
    procedure SetKeyValue(const Value: Variant);
    function GetField: TField;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetDataField: AnsiString;
    procedure SetDataField(const Value: AnsiString);
    function GetListSource: TDataSource;
    procedure SetListSource(const Value: TDataSource);
    function GetKeyField: AnsiString;
    procedure SetKeyField(const Value: AnsiString);
    function GetListField: AnsiString;
    procedure SetListField(const Value: AnsiString);
    function GetListFieldIndex: Integer;
    procedure SetListFieldIndex(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    function GetNullValueKey: TShortCut;
    procedure SetNullValueKey(const Value: TShortCut);
    function GetListVisible: Boolean;
    function GetDropDownRows: Integer;
    procedure SetDropDownRows(const Value: Integer);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    FLookupControl: TTntDBLookupControlLX;
    FLastSelectedItemIndex: Integer;
    FDeferChangesTilCloseup: Boolean;
    procedure UpdateTabWidths(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDropDown; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure Change; override;
    procedure DoExit; override;
    function ResetContents(AllowNull: Boolean): Boolean;
    procedure DoSelectItem(const Item: WideString; Alignment: TAlignment);
    procedure HandleChangedItem;
    procedure CloseUp; override;
    function GetItemHt: Integer; override;
  protected
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: AnsiString read GetDataField write SetDataField;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: AnsiString read GetKeyField write SetKeyField;
    property ListField: AnsiString read GetListField write SetListField;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex;
    property NullValueKey: TShortCut read GetNullValueKey write SetNullValueKey;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property DropDownRows: Integer read GetDropDownRows write SetDropDownRows stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown; reintroduce; virtual;
    property Field: TField read GetField;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property ListVisible: Boolean read GetListVisible;
  end;

  TTntDBLookupComboBoxLX = class(TTntCustomDBLookupComboBoxLX)
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    property Align;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight default 13;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
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
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  published
    property DataField;
    property DataSource;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property NullValueKey;
    property ReadOnly;
    property DropDownRows;
  end;

implementation

uses
  TntDB, SysUtils, TypInfo, StdCtrls, Math, TntGraphics, Forms, TntLXCombos, TntLXUtils,
  Variants, TntMenus;

{ TBookMarkStrList }

function TBookMarkStrList.CompareStrings(const S1, S2: string{TNT-ALLOW string}): Integer;
begin
  // must compare strings via binary (may contain null chars in the middle)
  if S1 = S2 then
    result := 0
  else if S1 < S2 then
    result := -1
  else
    result := 1;
end;

{ TTntDBLookupControlLX }

constructor TTntDBLookupControlLX.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TWinControl then
    FWinOwner := TWinControl(AOwner);
  FBookMarks := TBookMarkStrList.Create;
end;

destructor TTntDBLookupControlLX.Destroy;
begin
  FreeAndNil(FBookMarks);
  inherited;
end;

procedure TTntDBLookupControlLX.UpdateTabWidths;
var
  i: integer;
  ListValues: TTntStringList;
begin
  ListValues := TTntStringList.Create;
  try
    for i := 0 to ListFields.Count - 1 do
      ListValues.Add(GetWideDisplayText(ListFields[i]));
    UpdateTabWidthArray(FTabWidths, FWinOwner, ListValues);
  finally
    ListValues.Free;
  end;
end;

function TTntDBLookupControlLX.CurrentListString: WideString;
var
  f: TField;
begin
  f := nil;
  Result := '';
  // get field
  if (ListFieldIndex >= 0) and (ListFieldIndex < ListFields.Count) then
    f := ListFields[ListFieldIndex]
  else if ListFields.Count > 0 then
    f := ListFields[0];
  // get display text
  if f <> nil then
    Result := GetWideDisplayText(f);
end;

var
  GEnablingListLink: Boolean = False; // this is used to avoid unecessary refreshes

procedure TTntDBLookupControlLX.UpdateListFields;
var
  ListBookMark: TBookmarkStr;
begin
  inherited;
  if (FListItems <> nil) then begin
    FListItems.BeginUpdate;
    try
      FListItems.Clear;
      SetLength(FTabWidths, 0);
      FBookMarks.Clear;
      if ListActive then begin
        ListLink.DataSet.DisableControls;
        try
          ListBookMark := ListLink.DataSet.Bookmark;
          try
            ListLink.DataSet.First;
            while not ListLink.DataSet.Eof do begin
              // add reference to bookmark
              FBookMarks.Add(ListLink.DataSet.Bookmark);
              UpdateTabWidths;
              // add list item
              FListItems.AddObject(CurrentListString, TObject(FBookMarks.Count));
              ListLink.DataSet.Next;
            end;
          finally
            ListLink.DataSet.Bookmark := ListBookMark;
          end;
          KeyValueChanged;
        finally
          GEnablingListLink := True;
          try
            ListLink.DataSet.EnableControls;
          finally
            GEnablingListLink := False;
          end;
        end;
      end;
      DoOnUpdateTabWidths;
    finally
      FListItems.EndUpdate;
    end;
  end;
end;

function TTntDBLookupControlLX.GotoListIndex(ListIndex: Integer): Boolean;
var
  BMIndex: Integer;
begin
  Result := False;
  if (ListIndex >= 0) and (ListIndex <= FListItems.Count - 1) then begin
    BMIndex := Integer(FListItems.Objects[ListIndex]) - 1;
    if (BMIndex >= 0) and (BMIndex <= FBookMarks.Count - 1) and (ListActive) then begin
      if ListLink.DataSet.BookmarkValid(Pointer(FBookMarks[BMIndex])) then begin
        ListLink.DataSet.BookMark := FBookMarks[BMIndex];
        Result := True;
      end;
    end;
  end;
end;

procedure TTntDBLookupControlLX.ListLinkDataChanged;
var
  BMIndex: Integer;
  ListIndex: Integer;
  s: WideString;
begin
  if (not GEnablingListLink) then begin
    inherited;
    if ListActive and (FListItems <> nil) and (ListLink.DataSet.State = dsBrowse) then begin
      if (ListLink.DataSet.RecordCount <> FBookMarks.Count) then
        UpdateListFields {insert or delete}
      else begin
        // update current record
        BMIndex := FBookMarks.IndexOf(ListLink.DataSet.Bookmark);
        if (BMIndex = -1) then
          UpdateListFields {can't find this record}
        else begin
          ListIndex := FListItems.IndexOfObject(TObject(BMIndex + 1));
          if (ListIndex = -1) then
            UpdateListFields {can't find this item}
          else begin
            // update just this item
            UpdateTabWidths;
            s := CurrentListString;
            if FListItems[ListIndex] <> s then begin
              FListItems[ListIndex] := s;
              KeyValueChanged;
            end;
            DoOnUpdateTabWidths;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTntDBLookupControlLX.KeyValueChanged;
var
  FText: WideString;
  FAlignment: TAlignment;
  FListField: TField;
begin
  inherited;

  if (ListActive) and (Field <> nil) and (Field.FieldKind = fkLookup) then begin
    FListField := GetFieldProperty(ListLink.DataSet, Self, Field.LookupResultField)
  end else if (ListFieldIndex >= 0) and (ListFieldIndex < ListFields.Count) then
    FListField := ListFields[ListFieldIndex]
  else if ListFields.Count > 0 then
    FListField := ListFields[0]
  else
    FListField := nil;

  if (FListField <> nil) and ListActive and LocateKey then
  begin
    FText := CurrentListString;
    FAlignment := FListField.Alignment;
  end else begin
    FText := '';
    FAlignment := taLeftJustify;
  end;

  if Assigned(OnSelectItem) then
    OnSelectItem(FText, FAlignment);
end;

procedure TTntDBLookupControlLX.SelectItem(const Item: WideString; ItemIndex: Integer = -1);
var
  ListIndex: Integer;
  FKeyField: TField;
  FKeyFieldName: AnsiString;
begin
  if (DataLink.Dataset <> nil) and (Field <> nil)
  and ((not DataLink.Dataset.CanModify) or Field.ReadOnly or DataLink.ReadOnly) then
    KeyValueChanged //restore old value
  else begin
    if ListActive and (FListItems <> nil) then begin
      if (ItemIndex <> -1) and WideSameText(FListItems[ItemIndex], Item) then
        ListIndex := ItemIndex
      else
        ListIndex := FListItems.IndexOf(Item);
      if GotoListIndex(ListIndex) then begin
        if (Field <> nil) and (Field.FieldKind = fkLookup) then
          FKeyFieldName := Field.LookupKeyFields
        else
          FKeyFieldName := KeyField;
        FKeyField := GetFieldProperty(ListLink.DataSet, Self, FKeyFieldName);
        SelectKeyValue(FKeyField.Value);
      end else
        SelectKeyValue(Null);
    end;
  end
end;

function IntArraysDiffer(A, B: array of Integer): Boolean;
var
  i: integer;
begin
  if Length(A) <> Length(B) then
    Result := True
  else begin
    Result := False; {assume no difference}
    for i := 0 to Length(A) - 1 do begin
      if A[i] <> B[i] then begin
        Result := True;
        exit; {found a difference}
      end;
    end;
  end;
end;

procedure TTntDBLookupControlLX.DoOnUpdateTabWidths;
var
  i: integer;
begin
  if Assigned(OnUpdateTabWidths) then begin
    if IntArraysDiffer(FTabWidths, FOldTabWidths) then
      // only report if different
      OnUpdateTabWidths(Self);
    // remember reported tab widths
    SetLength(FOldTabWidths, Length(FTabWidths));
    for i := 0 to Length(FTabWidths) - 1 do
      FOldTabWidths[i] := FTabWidths[i];
  end;
end;

function DrawLookupItem(Canvas: TCanvas; FLookupControl: TTntDBLookupControlLX;
  OnDrawItem: TDrawItemEvent; Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean;
var
  i: integer;
  ListValues: TTntStrings;
begin
  if (FLookupControl.ListFields.Count in [0, 1])
  or (not FLookupControl.GotoListIndex(Index)) then
    Result := False
  else begin
    ListValues := TTntStringList.Create;
    try
      for i := 0 to FLookupControl.ListFields.Count - 1 do
        ListValues.Add(GetWideDisplayText(FLookupControl.ListFields[i]));
      Result := ComboDrawLookupItem(Canvas, FLookupControl.FTabWidths, OnDrawItem, Index, Rect, State, ListValues);
    finally
      ListValues.Free;
    end;
  end;
end;

procedure TTntDBLookupControlLX.WMKeyDown(var Message: TWMKeyDown);
begin
  if (NullValueKey <> 0) and CanModify and (NullValueKey = MessageToShortCut(Message)) then
  begin
    SelectKeyValue(Null);
    Message.CharCode := 0;
  end;
end;

{ TTntCustomDBLookupListBoxLX }

constructor TTntCustomDBLookupListBoxLX.Create(AOwner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawFixed;
  FLookupControl := TTntDBLookupControlLX.Create(Self);
  FLookupControl.OnSelectItem := DoSelectItem;
  FLookupControl.OnUpdateTabWidths := UpdateTabWidths;
  FLookupControl.FListItems := Items;
  TabWidth := 1;
end;

destructor TTntCustomDBLookupListBoxLX.Destroy;
begin
  FLookupControl.FListItems := nil;
  FLookupControl.OnSelectItem := nil;
  inherited;
end;

function TTntCustomDBLookupListBoxLX.GetField: TField;
begin
  result := FLookupControl.Field;
end;

function TTntCustomDBLookupListBoxLX.GetKeyValue: Variant;
begin
  result := FLookupControl.KeyValue;
end;

procedure TTntCustomDBLookupListBoxLX.SetKeyValue(const Value: Variant);
begin
  FLookupControl.KeyValue := Value;
end;

function TTntCustomDBLookupListBoxLX.GetDataField: AnsiString;
begin
  result := FLookupControl.DataField;
end;

procedure TTntCustomDBLookupListBoxLX.SetDataField(const Value: AnsiString);
begin
  if Value <> FLookupControl.DataField then begin
    FLookupControl.DataField := ''; {required to properly refresh lookup fields}
    FLookupControl.DataField := Value;
  end;
end;

function TTntCustomDBLookupListBoxLX.GetDataSource: TDataSource;
begin
  result := FLookupControl.DataSource;
end;

procedure TTntCustomDBLookupListBoxLX.SetDataSource(const Value: TDataSource);
begin
  FLookupControl.DataSource := Value;
end;

function TTntCustomDBLookupListBoxLX.GetKeyField: AnsiString;
begin
  result := FLookupControl.KeyField;
end;

procedure TTntCustomDBLookupListBoxLX.SetKeyField(const Value: AnsiString);
begin
  FLookupControl.KeyField := Value;
end;

function TTntCustomDBLookupListBoxLX.GetListField: AnsiString;
begin
  result := FLookupControl.ListField;
end;

procedure TTntCustomDBLookupListBoxLX.SetListField(const Value: AnsiString);
begin
  FLookupControl.ListField := Value;
end;

function TTntCustomDBLookupListBoxLX.GetListFieldIndex: Integer;
begin
  result := FLookupControl.ListFieldIndex;
end;

procedure TTntCustomDBLookupListBoxLX.SetListFieldIndex(const Value: Integer);
begin
  FLookupControl.ListFieldIndex := Value;
end;

function TTntCustomDBLookupListBoxLX.GetListSource: TDataSource;
begin
  result := FLookupControl.ListSource;
end;

procedure TTntCustomDBLookupListBoxLX.SetListSource(const Value: TDataSource);
begin
  FLookupControl.ListSource := Value;
end;

function TTntCustomDBLookupListBoxLX.GetNullValueKey: TShortCut;
begin
  result := FLookupControl.NullValueKey;
end;

procedure TTntCustomDBLookupListBoxLX.SetNullValueKey(const Value: TShortCut);
begin
  FLookupControl.NullValueKey := Value;
end;

function TTntCustomDBLookupListBoxLX.GetReadOnly: Boolean;
begin
  result := FLookupControl.ReadOnly;
end;

procedure TTntCustomDBLookupListBoxLX.SetReadOnly(const Value: Boolean);
begin
  FLookupControl.ReadOnly := Value;
end;

function TTntCustomDBLookupListBoxLX.GetRowCount: Integer;
begin
  result := ClientHeight div ItemHeight;
end;

procedure TTntCustomDBLookupListBoxLX.SetRowCount(const Value: Integer);
begin
  ClientHeight := ItemHeight * Value;
end;

procedure TTntCustomDBLookupListBoxLX.CNKeyDown(var Message: TWMKeyDown);
begin
  Message.Msg := WM_KeyDown;
  FLookupControl.Dispatch(Message);
  Message.Msg := CN_KeyDown;
  if Message.CharCode <> 0 then
    inherited;
end;

//--

procedure TTntCustomDBLookupListBoxLX.UpdateTabWidths;
begin
  Invalidate;
end;

procedure TTntCustomDBLookupListBoxLX.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not DrawLookupItem(Canvas, FLookupControl, OnDrawItem, Index, Rect, State) then
    inherited;
end;

procedure TTntCustomDBLookupListBoxLX.DoSelectItem(const Item: WideString; Alignment: TAlignment);
begin
  ItemIndex := Items.IndexOf(Item);
  if FLookupControl.ListFields.Count > 0 then
    TabWidth := ClientWidth * 3 div FLookupControl.ListFields.Count div 4;
end;

function TTntCustomDBLookupListBoxLX.GetSelectedItem: WideString;
begin
  if ItemIndex = -1 then
    result := ''
  else
    result := Items[ItemIndex];
end;

procedure TTntCustomDBLookupListBoxLX.Click;
begin
  inherited;
  FLookupControl.SelectItem(GetSelectedItem, ItemIndex);
end;

{ _TTntInternalCustomDBLookupComboBoxLX }

procedure _TTntInternalCustomDBLookupComboBoxLX.DoDropDown;
begin
  inherited DropDown;
end;

procedure _TTntInternalCustomDBLookupComboBoxLX.DropDown;
begin
  DoDropDown;
end;

{ TTntCustomDBLookupComboBoxLX }

constructor TTntCustomDBLookupComboBoxLX.Create(AOwner: TComponent);
begin
  FDeferChangesTilCloseup := False;
  inherited;
  Style := csDropDown;
  BorderWidth := 0;
  ItemHeight := ComboLx_GetItemHtForDefaultFont;
  FLookupControl := TTntDBLookupControlLX.Create(Self);
  FLastSelectedItemIndex := -1;
  FLookupControl.OnSelectItem := DoSelectItem;
  FLookupControl.OnUpdateTabWidths := UpdateTabWidths;
  FLookupControl.FListItems := Items;
end;

procedure TTntCustomDBLookupComboBoxLX.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ComboLX_AfterInherited_CreateParams(Self, Params);
end;

function TTntCustomDBLookupComboBoxLX.GetItemHt: Integer;
begin
  Result := ComboLx_GetItemHt(Self, inherited GetItemHt);
end;

destructor TTntCustomDBLookupComboBoxLX.Destroy;
begin
  FLookupControl.FListItems := nil;
  FLookupControl.OnSelectItem := nil;
  inherited;
end;

procedure TTntCustomDBLookupComboBoxLX.CNMeasureItem(var Message: TWMMeasureItem);
begin
  inherited;
  ComboLX_AfterInherited_CNMeasureItem(Self, Message);
end;

function TTntCustomDBLookupComboBoxLX.GetField: TField;
begin
  result := FLookupControl.Field;
end;

function TTntCustomDBLookupComboBoxLX.GetKeyValue: Variant;
begin
  result := FLookupControl.KeyValue;
end;

procedure TTntCustomDBLookupComboBoxLX.SetKeyValue(const Value: Variant);
begin
  FLookupControl.KeyValue := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetDataField: AnsiString;
begin
  result := FLookupControl.DataField;
end;

procedure TTntCustomDBLookupComboBoxLX.SetDataField(const Value: AnsiString);
begin
  if Value <> FLookupControl.DataField then begin
    FLookupControl.DataField := ''; {required to properly refresh lookup fields}
    FLookupControl.DataField := Value;
  end;
end;

function TTntCustomDBLookupComboBoxLX.GetDataSource: TDataSource;
begin
  result := FLookupControl.DataSource;
end;

procedure TTntCustomDBLookupComboBoxLX.SetDataSource(const Value: TDataSource);
begin
  FLookupControl.DataSource := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetKeyField: AnsiString;
begin
  result := FLookupControl.KeyField;
end;

procedure TTntCustomDBLookupComboBoxLX.SetKeyField(const Value: AnsiString);
begin
  FLookupControl.KeyField := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetListField: AnsiString;
begin
  result := FLookupControl.ListField;
end;

procedure TTntCustomDBLookupComboBoxLX.SetListField(const Value: AnsiString);
begin
  FLookupControl.ListField := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetListFieldIndex: Integer;
begin
  result := FLookupControl.ListFieldIndex;
end;

procedure TTntCustomDBLookupComboBoxLX.SetListFieldIndex(const Value: Integer);
begin
  FLookupControl.ListFieldIndex := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetListSource: TDataSource;
begin
  result := FLookupControl.ListSource;
end;

procedure TTntCustomDBLookupComboBoxLX.SetListSource(const Value: TDataSource);
begin
  FLookupControl.ListSource := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetNullValueKey: TShortCut;
begin
  result := FLookupControl.NullValueKey;
end;

procedure TTntCustomDBLookupComboBoxLX.SetNullValueKey(const Value: TShortCut);
begin
  FLookupControl.NullValueKey := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetReadOnly: Boolean;
begin
  result := FLookupControl.ReadOnly;
end;

procedure TTntCustomDBLookupComboBoxLX.SetReadOnly(const Value: Boolean);
begin
  FLookupControl.ReadOnly := Value;
end;

function TTntCustomDBLookupComboBoxLX.GetDropDownRows: Integer;
begin
  result := DropDownCount;
end;

procedure TTntCustomDBLookupComboBoxLX.SetDropDownRows(const Value: Integer);
begin
  DropDownCount := Value;
end;

procedure TTntCustomDBLookupComboBoxLX.CNKeyDown(var Message: TWMKeyDown);
begin
  if (FLookupControl.ListActive) then begin
    Message.Msg := WM_KeyDown;
    FLookupControl.Dispatch(Message);
    Message.Msg := CN_KeyDown;
    if (Message.CharCode = VK_ESCAPE) and FLookupControl.ListActive and ResetContents(False) then
      Message.CharCode := 0;
  end;
  if Message.CharCode <> 0 then
    inherited;
end;

//--

procedure TTntCustomDBLookupComboBoxLX.DoDropDown;
begin
  inherited;
  UpdateDroppedWidth(Self, FLookupControl.FTabWidths);
end;

procedure TTntCustomDBLookupComboBoxLX.UpdateTabWidths(Sender: TObject);
begin
  Invalidate;
end;

procedure TTntCustomDBLookupComboBoxLX.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if not DrawLookupItem(Canvas, FLookupControl, OnDrawItem, Index, Rect, State) then
    inherited;
end;

procedure TTntCustomDBLookupComboBoxLX.DropDown;
begin
  DroppedDown := True;
end;

function TTntCustomDBLookupComboBoxLX.GetListVisible: Boolean;
begin
  result := DroppedDown;
end;

procedure TTntCustomDBLookupComboBoxLX.DoSelectItem(const Item: WideString; Alignment: TAlignment);
begin
  if (ItemIndex = -1) or (not WideSameText(Items[ItemIndex], Item)) then
    ItemIndex := Items.IndexOf(Item);
  if ItemIndex = -1 then
    Text := Item;
  if ItemIndex <> -1 then
    FLastSelectedItemIndex := ItemIndex;
end;

function TTntCustomDBLookupComboBoxLX.ResetContents(AllowNull: Boolean): Boolean;

  function FindNearestItemIndex: Integer;
  var
    i: integer;
  begin
    result := -1;
    if (Text <> '') then begin
      for i := 0 to Items.Count - 1 do begin
        if Pos(Text, Items[i]) = 1 then begin
          result := i;
          break;
        end;
      end;
    end;
    if (result = -1) then
      result := FLastSelectedItemIndex;
  end;

begin
  result := False;
  if ((not AllowNull) or (Text <> ''))
  and (Items.IndexOf(Text) = -1) then begin
    ItemIndex := FindNearestItemIndex;
    if ItemIndex <> -1 then begin
      FLookupControl.SelectItem(Text, ItemIndex);
      result := True;
    end;
  end;
end;

procedure TTntCustomDBLookupComboBoxLX.DoExit;
begin
  if FLookupControl.ListActive then
    ResetContents(True);
  FLastSelectedItemIndex := ItemIndex;
  inherited;
end;

procedure TTntCustomDBLookupComboBoxLX.Click;
begin
  inherited;
  if FLookupControl.ListActive then begin
    if DroppedDown and FDeferChangesTilCloseup then
      FProcessChangeOnCloseUp := True
    else
      HandleChangedItem;
  end;
end;

procedure TTntCustomDBLookupComboBoxLX.Change;
begin
  inherited;
  if FLookupControl.ListActive then begin
    if DroppedDown and FDeferChangesTilCloseup then
      FProcessChangeOnCloseUp := True
    else
      HandleChangedItem;
  end;
end;

procedure TTntCustomDBLookupComboBoxLX.CloseUp;
begin
  inherited;
  if FProcessChangeOnCloseUp then begin
    HandleChangedItem;
    FProcessChangeOnCloseUp := False;
  end;
end;

procedure TTntCustomDBLookupComboBoxLX.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if  (not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]))
  and (not (csLoading in ComponentState)) then
    ItemHeight := ComboLx_GetItemHtForFont(Self);
end;

procedure TTntCustomDBLookupComboBoxLX.HandleChangedItem;
var
  SaveSelStart, SaveSelLength: Integer;
  SaveText: WideString;
begin
  SaveSelStart := SelStart;
  SaveSelLength := SelLength;
  SaveText := Text;
  FLookupControl.SelectItem(Text, ItemIndex);
  Text := SaveText;
  if Items.IndexOf(Text) <> -1 then
    FLastSelectedItemIndex := Items.IndexOf(Text);
  SelStart := SaveSelStart;
  SelLength := SaveSelLength;
end;

end.
