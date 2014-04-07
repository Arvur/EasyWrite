
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXDataSet;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, DB, TntLXClasses, TntDB;

type
  TBookmarkInfo = record
    Data: Pointer;
    Flag: TBookmarkFlag;
  end;

const
  MAX_FIELD_COUNT = (High(Word) - SizeOf(TBookMarkInfo)) div SizeOf(Variant); // GetRecordSize can't be larger than a Word

type
  TRecBuff = record
    Bookmark: TBookmarkInfo;
    Values: array[0..MAX_FIELD_COUNT] of Variant; // max field count is 4095, max recordsize of 65528 bytes
  end;
  PRecBuff = ^TRecBuff;

  TTntDataSet = class(TDataSet)
  private
    FBookmarkList: TTntList;
    FRecordPos: Integer;
    FSort: AnsiString;
    FSortOptions: TLocateOptions;
    procedure SnapRecordPosition(VerifyActual: Boolean);
    function GetValuesForRecord(Bookmark: TBookmark; GetFields: TList): Variant;
    procedure UpdateBookmarkList;

    function FilteredIsPossible: Boolean;
    function RecordPassesFilter: Boolean;
    procedure SetSort(const Value: AnsiString);
    function QuickFind(Value: Variant; var Index: Integer; Options: TLocateOptions; Duplicates: TDuplicates): Boolean;
    function GetValuesForBuffer(Buffer: PAnsiChar; GetFields: TList): Variant;
    procedure SetSortOptions(const Value: TLocateOptions);
  protected
    { new required overrides }
    procedure InternalUpdateBookmarkList(BookmarkList: TTntList; Bookmark: TBookmark); virtual; abstract;
    function GetFieldValue(Bookmark: TBookmark; FieldIndex: Integer): Variant; virtual; abstract;
    procedure SetFieldValue(Bookmark: TBookmark; FieldIndex: Integer; const Value: Variant); virtual; abstract;
    procedure FreeRecord(Bookmark: TBookmark); virtual; abstract;
    function CreateRecord: TBookmark; virtual; abstract;
    { new optional overrides }
    function LocateRecord(const KeyFields: AnsiString; const KeyValues: Variant;
      Options: TLocateOptions): Integer; virtual;
    procedure ApplyFilterToBookmarkList(BookmarkList: TTntList); virtual;
    procedure ApplySortToBookmarkList(BookmarkList: TTntList); virtual;
    procedure BindQuickFields; virtual;
  protected
    // Record buffer methods
    function AllocRecordBuffer: PAnsiChar; override;
    procedure FreeRecordBuffer(var Buffer: PAnsiChar); override;
    procedure InternalInitRecord(Buffer: PAnsiChar); override;
    function GetRecord(Buffer: PAnsiChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;

    // Field access methods
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;

    // Bookmark methods
    procedure GetBookmarkData(Buffer: PAnsiChar; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: PAnsiChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PAnsiChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PAnsiChar; Value: TBookmarkFlag); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PAnsiChar); override;

    // Navigational methods
    procedure InternalFirst; override;
    procedure InternalLast; override;

    // Editing methods
    procedure InternalCancel; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;

    // Misc methods
    function IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;

    { Optional overrides }
    procedure InternalRefresh; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    // Filter overrides
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: AnsiString); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    function Locate(const KeyFields: AnsiString; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: AnsiString; const KeyValues: Variant;
      const ResultFields: AnsiString): Variant; override;

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
  published
    property Active;
    property BufferCount;
    property Filtered;
    property Sort: AnsiString read FSort write SetSort;
    property SortOptions: TLocateOptions read FSortOptions write SetSortOptions default [loCaseInsensitive];
    // events
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
  end;

{$IFDEF COMPILER_10_UP}
  TTntLXWideStringField = class(TTntWideStringField)
  protected
    function GetDataSize: Integer; override;
    procedure SetAsWideString(const Value: WideString); override;
    procedure SetVarValue(const Value: Variant); override;
    function GetAsWideString: WideString; override;
    function GetAsVariant: Variant; override;
  end;

  TTntLXWideMemoStringField = class(TTntLXWideStringField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure RegisterTntFields;
{$ENDIF}

implementation

uses
  SysUtils, Variants, Forms, SyncObjs,
  {$IFDEF COMPILER_10_UP} WideStrUtils, {$ENDIF}
  TntLXUtils, TntSysUtils;

{ TTntDataSet }

constructor TTntDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FBookmarkList := TTntList.Create;
  FSortOptions := [loCaseInsensitive];
end;

destructor TTntDataSet.Destroy;
begin
  inherited;
  FreeAndNil(FBookmarkList);
end;

function TTntDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := GetTntFieldClass(inherited GetFieldClass(FieldType));
  {$IFDEF COMPILER_10_UP}
  if Result = TTntWideStringField then
    Result := TTntLXWideStringField;
  {$ENDIF}
end;

function TTntDataSet.AllocRecordBuffer: PAnsiChar;
begin
  Result := AllocMem(GetRecordSize);
  Initialize(PRecBuff(Result).Values[0], FieldCount);
end;

procedure TTntDataSet.FreeRecordBuffer(var Buffer: PAnsiChar);
begin
  Finalize(PRecBuff(Buffer).Values[0], FieldCount);
  FreeMem(Buffer);
end;

procedure TTntDataSet.InternalInitRecord(Buffer: PAnsiChar);
var
  i: integer;
begin
  if Buffer <> nil then begin
    FillChar(PRecBuff(Buffer).Bookmark, SizeOf(TBookmarkInfo), 0);
    for i := 0 to FieldCount - 1 do
      PRecBuff(Buffer).Values[i] := Null;
  end;
end;

function TTntDataSet.GetRecord(Buffer: PAnsiChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Rec: PRecBuff;
  i: integer;
begin
  if RecordCount = 0 then begin
    FRecordPos := -1;
    Result := grEOF;
  end else begin
    Result := grOk;
    case GetMode of
      gmPrior:
        if FRecordPos > 0 then
          Dec(FRecordPos)
        else begin
          FRecordPos := -1;
          Result := grBOF;
        end;
      gmCurrent:
        if (FRecordPos < 0) or (FRecordPos > (RecordCount - 1)) then
          Result := grError;
      gmNext:
        if FRecordPos < RecordCount - 1 then begin
          Inc(FRecordPos)
        end else begin
          FRecordPos := RecordCount;
          Result := grEOF;
        end;
    end;
  end;
  if Result = grOk then begin
    // copy over values
    Rec := PRecBuff(Buffer);
    Rec.Bookmark.Data := FBookmarkList[FRecordPos];
    Rec.Bookmark.Flag := bfCurrent;
    for i := 0 to FieldCount - 1 do
      Rec.Values[i] := GetFieldValue(FBookmarkList[FRecordPos], i);
  end else begin
    // clear buffer, handle errors
    InternalInitRecord(Buffer);
    if (Result = grError) and DoCheck then
      DatabaseError('Internal Error: TTntDataSet.InternalGetRecord has no current record.');
  end;
end;

function TTntDataSet.GetRecordSize: Word;
begin
  Result := SizeOf(TBookmarkInfo) + (FieldCount * SizeOf(Variant));
end;

procedure VarToBuffer(Field: TField; Data: Variant; Buffer: Pointer);
begin
  case Field.DataType of
    ftGuid, ftFixedChar, ftString:
      AnsiString(Buffer^) := Data;
    ftWideString:
      {$IFDEF COMPILER_10_UP}
      if Field is TTntLXWideStringField then
        OleVariant(Buffer^) := Data
      else
        WStrCopy(Buffer, PWideChar(WideString(Data)));
      {$ELSE}
      WideString(Buffer^) := Data;
      {$ENDIF}
    ftSmallint:
      SmallInt(Buffer^) := Data;
    ftWord:
      Word(Buffer^) := Data;
    ftAutoInc, ftInteger:
      Integer(Buffer^) := Data;
    ftFloat, ftCurrency:
      Double(Buffer^) := Data;
    ftBCD:
      Currency(Buffer^) := Data;
    ftBoolean:
      WordBool(Buffer^) := Data;
    ftDate, ftTime, ftDateTime:
      TDateTime(Buffer^) := Data;
    ftInterface:
      IUnknown(Buffer^) := Data;
    ftIDispatch:
      IDispatch(Buffer^) := Data;
    ftLargeInt:
      LargeInt(Buffer^) := Round(Double(Data));
  else
    DatabaseError('Internal Error: VarToBuffer() encountered unexpected field type.');
  end;
end;

function TTntDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
var
  Value: Variant;
begin
  ForceAssigned(ActiveBuffer, 'ActiveBuffer');
  Value := PRecBuff(ActiveBuffer).Values[Field.Index];
  Result := (not VarIsNull(Value)) and (not VarIsEmpty(Value));
  if Result and (Buffer <> nil) then
    VarToBuffer(Field, Value, Buffer);
end;

function TTntDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TTntDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(Field, Buffer, True);
end;

function BufferToVar(Field: TField; Buffer: Pointer): Variant;
begin
  case Field.DataType of
    ftGuid, ftFixedChar, ftString:
      Result := AnsiString(Buffer^);
    ftWideString:
      {$IFDEF COMPILER_10_UP}
      if Field is TTntLXWideStringField then
        Result := OleVariant(Buffer^)
      else
        Result := WideString(PWideChar(Buffer));
      {$ELSE}
      Result := WideString(Buffer^);
      {$ENDIF}
    ftSmallint:
      Result := SmallInt(Buffer^);
    ftWord:
      Result := Word(Buffer^);
    ftAutoInc, ftInteger:
      Result := Integer(Buffer^);
    ftFloat, ftCurrency:
      Result := Double(Buffer^);
    ftBCD:
      Result := Currency(Buffer^);
    ftBoolean:
      Result := WordBool(Buffer^);
    ftDate, ftTime, ftDateTime:
      Result := TDateTime(Buffer^);
    ftInterface:
      Result := IUnknown(Buffer^);
    ftIDispatch:
      Result := IDispatch(Buffer^);
    ftLargeInt:
      Result := Double(Buffer^);
  else
    DatabaseError('Internal Error: BufferToVar() encountered unexpected field type.');
  end;
end;

procedure TTntDataSet.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
begin
  ForceAssigned(ActiveBuffer, 'ActiveBuffer');
  if Buffer = nil then
    PRecBuff(ActiveBuffer).Values[Field.Index] := Null
  else
    PRecBuff(ActiveBuffer).Values[Field.Index] := BufferToVar(Field, Buffer);
  DataEvent(deFieldChange, Longint(Field));
end;

procedure TTntDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
  SetFieldData(Field, Buffer, True);
end;

procedure TTntDataSet.GetBookmarkData(Buffer: PAnsiChar; Data: Pointer);
begin
  PPointer(Data)^ := PRecBuff(Buffer).Bookmark.Data;
end;

procedure TTntDataSet.SetBookmarkData(Buffer: PAnsiChar; Data: Pointer);
begin
  PRecBuff(Buffer).Bookmark.Data := PPointer(Data)^;
end;

function TTntDataSet.GetBookmarkFlag(Buffer: PAnsiChar): TBookmarkFlag;
begin
  Result := PRecBuff(Buffer).Bookmark.Flag;
end;

procedure TTntDataSet.SetBookmarkFlag(Buffer: PAnsiChar; Value: TBookmarkFlag);
begin
  PRecBuff(Buffer).Bookmark.Flag := Value;
end;

function TTntDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if Bookmark = nil then
    Result := False
  else
    Result := FBookmarkList.IndexOf(PPointer(Bookmark)^) <> -1;
end;

procedure TTntDataSet.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: Integer;
begin
  Index := FBookmarkList.IndexOf(PPointer(Bookmark)^);
  if Index <> -1 then
    FRecordPos := Index
  else
    DatabaseError('Bookmark not found.');
end;

procedure TTntDataSet.InternalSetToRecord(Buffer: PAnsiChar);
begin
  InternalGotoBookmark(@PRecBuff(Buffer).Bookmark.Data);
end;

procedure TTntDataSet.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TTntDataSet.InternalLast;
begin
  FRecordPos := RecordCount;
end;

procedure TTntDataSet.SnapRecordPosition(VerifyActual: Boolean);
begin
  { Last, not EOF}
  if FRecordPos > (RecordCount - 1) then
    FRecordPos := RecordCount - 1;
  {First, not BOF}
  if FRecordPos < 0 then
    FRecordPos := 0;
  {Verify Actual}
  if VerifyActual and (RecordCount = 0) then
    DatabaseError('Internal Error: SnapRecordPosition() required a record count.');
end;

procedure TTntDataSet.InternalCancel;
begin
  if RecordCount = 0 then begin
    CursorPosChanged;
    Resync([]);
  end;
end;

procedure TTntDataSet.InternalDelete;
begin
  // remove row
  SnapRecordPosition(True);
  FreeRecord(FBookmarkList[FRecordPos]);
  FBookmarkList.Delete(FRecordPos);

  // update position
  if RecordCount = 0 then begin
    FRecordPos := -1;
    CursorPosChanged;
    Resync([]);
  end
  else
    SnapRecordPosition(False);
end;

procedure TTntDataSet.InternalPost;
var
  i: integer;
  Bookmark: TBookmark;
  NewValues: Variant;
  NewIndex: Integer;
  GetFields: TList;
begin
  inherited;
  if (State <> dsInsert) then begin
    // get Bookmark
    SnapRecordPosition(True);
    Bookmark := FBookmarkList[FRecordPos];
  end else begin
    // deal with adding a record
    SnapRecordPosition(False);
    Bookmark := CreateRecord;
  end;

  // commit values
  for i := 0 to FieldCount - 1 do begin
    if Fields[i].FieldKind <> fkCalculated then
      SetFieldValue(Bookmark, i, PRecBuff(ActiveBuffer).Values[i]);
  end;

  // determine new index of bookmark
  if (State <> dsInsert) then
    FBookmarkList.Delete(FRecordPos); { add it back in proper location }
  try
    if Sort = '' then begin
      // no sort order
      if (State = dsInsert) and (PRecBuff(ActiveBuffer).BookMark.Flag = bfEOF) then
        NewIndex := RecordCount
      else
        NewIndex := FRecordPos
    end else begin
      // respect sort order
      GetFields := TList.Create;
      try
        GetFieldList(GetFields, Sort);
        NewValues := GetValuesForBuffer(ActiveBuffer, GetFields);
      finally
        FreeAndNil(GetFields);
      end;
      QuickFind(NewValues, NewIndex, SortOptions, dupAccept);
    end;
  finally
    // insert bookmark into list
    FBookmarkList.Insert(NewIndex, Bookmark);
  end;
  FRecordPos := NewIndex;
end;

procedure TTntDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then SetBookmarkFlag(Buffer, bfEOF);
  InternalPost;
end;

procedure TTntDataSet.UpdateBookmarkList;
var
  Bookmark: TBookmark;
  NewRecordPos: Integer;
  SaveRecordPos: Integer;
begin
  SaveRecordPos := FRecordPos;
  if (FRecordPos > -1) and (FRecordPos < RecordCount) then
    Bookmark := FBookmarkList[FRecordPos]
  else
    Bookmark := nil;
  InternalUpdateBookmarkList(FBookmarkList, Bookmark);
  ApplyFilterToBookmarkList(FBookmarkList);
  ApplySortToBookmarkList(FBookmarkList);
  if Bookmark <> nil then begin
    NewRecordPos := FBookmarkList.IndexOf(Bookmark);
    if NewRecordPos <> -1 then
      FRecordPos := NewRecordPos
    else if SaveRecordPos < FBookmarkList.Count then
      FRecordPos := SaveRecordPos
    else begin
      FRecordPos := -1;
      Resync([]);
    end;
  end;
end;

procedure TTntDataSet.ApplyFilterToBookmarkList(BookmarkList: TTntList);
var
  SaveState: TDataSetState;
  FilteredBookmarkList: TTntList;
begin
  if FilteredIsPossible then begin
    SaveState := SetTempState(dsFilter);
    try
      FilteredBookmarkList := TTntList.Create;
      try
        First;
        While not EOF do begin
          if RecordPassesFilter then begin
            FilteredBookmarkList.Add(PRecBuff(ActiveBuffer).Bookmark.Data);
          end;
          Next;
        end;
        BookmarkList.Clear;
        BookmarkList.Assign(FilteredBookmarkList);
      finally
        FreeAndNil(FilteredBookmarkList);
      end;
    finally
      RestoreState(SaveState);
    end;
  end;
end;

var
  SortCriticalSection: TCriticalSection;
  SortingDataSet: TTntDataSet;
  SortingFields: TList;

function GlobalCompareBookmarks(Item1, Item2: Pointer): Integer;
var
  Value1, Value2: Variant;
begin
  Value1 := SortingDataSet.GetValuesForRecord(Item1, SortingFields);
  Value2 := SortingDataSet.GetValuesForRecord(Item2, SortingFields);
  Result := CompareVariants(Value1, Value2, SortingDataSet.SortOptions);
end;

procedure TTntDataSet.ApplySortToBookmarkList(BookmarkList: TTntList);
begin
  if Sort <> '' then begin
    SortCriticalSection.Enter;
    try
      SortingDataSet := Self;
      try
        SortingFields := TList.Create;
        try
          try
            GetFieldList(SortingFields, Sort);
          except
            FSort := '';
            raise;
          end;
          BookmarkList.Sort(GlobalCompareBookmarks);
        finally
          FreeAndNil(SortingFields);
        end;
      finally
        SortingDataSet := nil;
      end;
    finally
      SortCriticalSection.Leave;
    end;
  end;
end;

function TTntDataSet.IsCursorOpen: Boolean;
begin
  Result := FieldCount > 0;
end;

procedure TTntDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(Pointer);
  FRecordPos := -1;
  InternalInitFieldDefs;
  if DefaultFields then begin
    CreateFields;
  end;
  BindQuickFields;
  BindFields(True);
  Assert(FieldCount <= MAX_FIELD_COUNT, 'TTntDataSet.InternalOpen: FieldCount exceeds limits.');
  Assert(FieldCount > 0, 'TTntDataSet.InternalOpen: FieldCount must be greater than zero.');
  UpdateBookmarkList;
end;

procedure TTntDataSet.BindQuickFields;
begin
  //
end;

procedure TTntDataSet.InternalClose;
begin
  if DefaultFields then
    DestroyFields;
  FRecordPos := -1;
  FBookmarkList.Clear;
end;

procedure TTntDataSet.InternalHandleException;
begin
  SafeShowException(nil);
end;

function TTntDataSet.GetRecordCount: Integer;
begin
  Result := FBookmarkList.Count;
end;

procedure TTntDataSet.InternalRefresh;
begin
  UpdateCursorPos;
  UpdateBookmarkList;
end;

function TTntDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  Result := FRecordPos + 1;
end;

procedure TTntDataSet.SetRecNo(Value: Integer);
begin
  FRecordPos := Value - 1;
  SnapRecordPosition(False);
  Resync([]);
end;

function TTntDataSet.GetValuesForBuffer(Buffer: PAnsiChar; GetFields: TList): Variant;
var
  I: Integer;
begin
  if GetFields.Count = 1 then
    Result := PRecBuff(ActiveBuffer).Values[TField(GetFields[0]).Index]
  else begin
    Result := VarArrayCreate([0, GetFields.Count - 1], varVariant);
    for I := 0 to GetFields.Count - 1 do
      Result[I] := PRecBuff(ActiveBuffer).Values[TField(GetFields[I]).Index]
  end;
end;

function TTntDataSet.GetValuesForRecord(Bookmark: TBookmark; GetFields: TList): Variant;
var
  I: Integer;
begin
  if GetFields.Count = 1 then
    Result := GetFieldValue(Bookmark, TField(GetFields[0]).Index)
  else begin
    Result := VarArrayCreate([0, GetFields.Count - 1], varVariant);
    for I := 0 to GetFields.Count - 1 do
      Result[I] := GetFieldValue(Bookmark, TField(GetFields[I]).Index)
  end;
end;

function TTntDataSet.QuickFind(Value: Variant; var Index: Integer; Options: TLocateOptions; Duplicates: TDuplicates): Boolean;
var
  L, H, I, C: Integer;
  ListValue: Variant;
  GetFields: TList;
begin
  Assert(Sort <> '', 'QuickFind requires Sort fields.');
  GetFields := TList.Create;
  try
    GetFieldList(GetFields, Sort);
    Result := False;
    L := 0;
    H := FBookmarkList.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      ListValue := GetValuesForRecord(FBookmarkList[i], GetFields);
      C := CompareVariants(ListValue, Value, Options);
      if C < 0 then
        L := I + 1
      else begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          if Duplicates <> dupAccept then L := I;
        end;
      end;
    end;
    Index := L;
  finally
    GetFields.Free;
  end;
end;

function ListEquals(List1, List2: TList): Boolean;
var
  i: integer;
begin
  if List1.Count <> List2.Count then
    Result := False
  else begin
    Result := True;
    for i := 0 to List1.Count - 1 do begin
      if List1[i] <> List2[i] then begin
        Result := False;
        break;
      end;
    end;
  end;
end;

function TTntDataSet.LocateRecord(const KeyFields: AnsiString;
  const KeyValues: Variant; Options: TLocateOptions): Integer;
var
  KeyFieldList: TList;
  i: Integer;
  SortFieldList: TList;
begin
  CheckBrowseMode;
  try
    KeyFieldList := TList.Create;
    try
      GetFieldList(KeyFieldList, KeyFields);
      SortFieldList := nil;
      try
        if (Sort <> '') then begin
          SortFieldList := TList.Create;
          GetFieldList(SortFieldList, Sort);
        end;
        if (SortFieldList <> nil) and ListEquals(SortFieldList, KeyFieldList) then begin
          if not QuickFind(KeyValues, Result, Options, dupAccept) then
            Result := -1;
        end else begin
          Result := -1;
          for i := 0 to RecordCount - 1 do begin
            if VariantsEqual(GetValuesForRecord(FBookmarkList[i], KeyFieldList), KeyValues, Options) then
            begin
              Result := i;
              exit; { found it }
            end;
          end;
        end;
      finally
        SortFieldList.Free;
      end;
    finally
      KeyFieldList.Free;
    end;
  except
    Result := -1;
  end;
end;

function TTntDataSet.Locate(const KeyFields: AnsiString;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  LocateIndex: Integer;
begin
  LocateIndex := LocateRecord(KeyFields, KeyValues, Options);
  Result := (LocateIndex <> -1);
  if Result then begin
    DoBeforeScroll;
    FRecordPos := LocateIndex;
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TTntDataSet.Lookup(const KeyFields: AnsiString;
  const KeyValues: Variant; const ResultFields: AnsiString): Variant;
var
  LookupIndex: Integer;
  ResultFieldList: TList;
begin
  Result := Null;
  LookupIndex := LocateRecord(KeyFields, KeyValues, []);
  if (LookupIndex <> -1) then begin
    ResultFieldList := TList.Create;
    try
      GetFieldList(ResultFieldList, ResultFields);
      Result := GetValuesForRecord(FBookmarkList[LookupIndex], ResultFieldList);
    finally
      ResultFieldList.Free;
    end;
  end;
end;

function TTntDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  A, B: Integer;
begin
  A := FBookmarkList.IndexOf(Bookmark1);
  B := FBookmarkList.IndexOf(Bookmark2);
  if A > B then
    Result := 1
  else if A < B then
    Result := -1
  else
    Result := 0;
end;

//-- SORT/FILTER PROPERTIES

procedure TTntDataSet.SetSort(const Value: AnsiString);
begin
  if (FSort <> Value) then begin
    FSort := Value;
    if Active then
      Refresh;
  end;
end;

procedure TTntDataSet.SetSortOptions(const Value: TLocateOptions);
begin
  if (FSortOptions <> Value) then begin
    FSortOptions := Value;
    if Active then
      Refresh;
  end;
end;

function TTntDataSet.RecordPassesFilter: Boolean;
begin
  Result := False;
  if Assigned(OnFilterRecord) then
    OnFilterRecord(Self, Result)
  else if Filter <> '' then
    raise ETntGeneralError.Create('TTntDataSet.Filter property is not supported yet.');
end;

function TTntDataSet.FilteredIsPossible: Boolean;
begin
  Result :=  (Filtered)
         and (Assigned(OnFilterRecord) or (Filter <> ''));
end;

procedure TTntDataSet.SetFiltered(Value: Boolean);
var
  OldFilteredPossible: Boolean;
begin
  OldFilteredPossible := FilteredIsPossible;
  inherited;
  if OldFilteredPossible <> FilteredIsPossible then
    Refresh;
end;

procedure TTntDataSet.SetFilterOptions(Value: TFilterOptions);
var
  OldFilterOptions: TFilterOptions;
begin
  OldFilterOptions := FilterOptions;
  inherited;
  if  (FilteredIsPossible)
  and (OldFilterOptions <> FilterOptions) then
    Refresh;
end;

procedure TTntDataSet.SetFilterText(const Value: AnsiString);
var
  OldFilter: AnsiString;
  OldFilteredPossible: Boolean;
begin
  OldFilter := Filter;
  OldFilteredPossible := FilteredIsPossible;
  inherited;
  if (OldFilteredPossible <> FilteredIsPossible)
  or (FilteredIsPossible and (OldFilter <> Filter)) then
    Refresh;
end;

procedure TTntDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
var
  OldFilterEvent: TFilterRecordEvent;
  OldFilteredPossible: Boolean;
begin
  OldFilterEvent := OnFilterRecord;
  OldFilteredPossible := FilteredIsPossible;
  inherited;
  if (OldFilteredPossible <> FilteredIsPossible)
  or (FilteredIsPossible and (@OldFilterEvent <> @OnFilterRecord)) then
    Refresh;
end;

{$IFDEF COMPILER_10_UP}

{ TTntLXWideStringField }

function TTntLXWideStringField.GetAsVariant: Variant;
var
  VarResult: OleVariant;
begin
  if not GetData(@VarResult, False) then
    Result := Null
  else
    Result := VarResult;
end;

function TTntLXWideStringField.GetAsWideString: WideString;
var
  VarResult: OleVariant;
begin
  if not GetData(@VarResult, False) then
    Result := ''
  else
    Result := VarToWideStr(VarResult);
end;

function TTntLXWideStringField.GetDataSize: Integer;
begin
  Result := SizeOf(OleVariant);
end;

procedure TTntLXWideStringField.SetAsWideString(const Value: WideString);
var
  VarValue: OleVariant;
begin
  if (Size > 0) and (Length(Value) > Size) then
    VarValue := Copy(Value, 1, Size)
  else
    VarValue := Value;
  SetData(@VarValue, False);
end;

procedure TTntLXWideStringField.SetVarValue(const Value: Variant);
begin
  SetAsWideString(Value);
end;

{ TTntLXWideMemoStringField }

constructor TTntLXWideMemoStringField.Create(AOwner: TComponent);
begin
  inherited;
  SetDataType(ftWideMemo);
end;

procedure RegisterTntFields;
begin
  RegisterFields([TTntLXWideStringField]);
  RegisterFields([TTntLXWideMemoStringField]);
end;

{$ENDIF}

initialization
  SortCriticalSection := TCriticalSection.Create;

finalization
  SortCriticalSection.Free;


end.
