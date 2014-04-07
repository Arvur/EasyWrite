
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXClasses;

{$INCLUDE TntCompilers.inc}

interface

uses
  Types, Classes, TntClasses, Contnrs, SysUtils;

type
  IExtraErrorInfo = interface(IInterface)
    ['{191D1196-0A9C-4DBC-8CB3-420BAF473BAA}']
    function GetExtraErrorInfo: WideString;
  end;

  EExtraInfoException = class(Exception, IExtraErrorInfo)
  private
    FExtraInfo: WideString;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const Msg, ExtraInfo: WideString);
    function GetExtraErrorInfo: WideString;
  end;

  // TTntList is patterned after TList.  What it adds:
  //    * It is optimized to better handle insertions to beginning of list.
  //    * It is optimized to better handle deletions to beginning of list.
  //    * It is adds the ability to delete more than one item at a time.
  //    * It limits growth to 16K items at a time.
  //    * It provides an OnNotify event

  TListNotifyEvent = procedure(Ptr: Pointer; Action: TListNotification) of object;

  TTntList = class(TObject)
  private
    FList: PPointerList;
    PPreList: PPointerList;
    PEndBuff: PPointerList;
    FCount: Integer;
    FOnNotify: TListNotifyEvent;
    procedure CheckPreBuffSize;
    function PreBuffSize: Integer;
    function GetCapacity: Integer;
    property PreList: PPointerList read PPreList;
    function GetGrowthDelta: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer; DelCount: Integer = 1);
    class procedure Error(const Msg: WideString; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TTntList;
    function Extract(Item: Pointer): Pointer;
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TTntList; AOperator: TListAssignOp = laCopy; ListB: TTntList = nil);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PPointerList read FList;
    property OnNotify: TListNotifyEvent read FOnNotify write FOnNotify;
  end;

  TTntIntegerList = class(TObject)
  private
    FSorted: Boolean;
    FOnNotify: TListNotifyEvent;
    procedure SetSorted(const Value: boolean);
    function GetCommaText: WideString;
    procedure SetCommaText(const Value: WideString);
  protected
    FList: TTntList;
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
    function GetCount: Integer;
    procedure Sort;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Item: Integer): Integer;
    procedure AddList(List: TTntIntegerList);
    procedure Insert(Index: Integer; const Item: Integer);
    procedure Delete(Index: Integer);
    procedure ChangeValue(Value, NewValue: Integer);
    procedure DeleteValue(Value: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(Index1, Index2: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: Integer read Get write Put; default;

    function Equals(AList: TTntIntegerList): Boolean;
    function Find(const Item: Integer; var Index: Integer): Boolean;
    function IndexOf(Item: Integer): Integer;
    property Sorted: boolean read FSorted write SetSorted;

    procedure AddIntArray(const Ints: array of Integer; AllowDuplicates: Boolean = True);
    procedure LoadFromIntArray(const Ints: array of Integer);
    procedure SaveToIntArray(var Ints: TIntegerDynArray);
    function GetIntArray: TIntegerDynArray;

    procedure SaveToIntList(IntList: TTntIntegerList);
    procedure LoadFromIntList(IntList: TTntIntegerList);

    property CommaText: WideString read GetCommaText write SetCommaText;
    property OnNotify: TListNotifyEvent read FOnNotify write FOnNotify;
  end;

function SameIntArrays(const IntArray1, IntArray2: array of Integer): Boolean;

type
  TTntComponentList = class(TComponentList)
  private
    FOnNotify: TListNotifyEvent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property OnNotify: TListNotifyEvent read FOnNotify write FOnNotify;
  end;

  TTntComponentRef = class(TComponent)
  private
    FValue: TComponent;
    procedure SetValue(const Value: TComponent);
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Value: TComponent read FValue write SetValue;
  end;

implementation

uses
  RTLConsts, Math, Windows, TntSystem, TntSysUtils, TntLXUtils;

{ EExtraInfoException }

constructor EExtraInfoException.Create(const Msg, ExtraInfo: WideString);
begin
  inherited Create(Msg);
  FExtraInfo := ExtraInfo;
end;

function EExtraInfoException.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function EExtraInfoException._AddRef: Integer;
begin
  Result := E_NOTIMPL;
end;

function EExtraInfoException._Release: Integer;
begin
  Result := E_NOTIMPL;
end;

function EExtraInfoException.GetExtraErrorInfo: WideString;
begin
  Result := FExtraInfo;
end;

{ TTntList }

destructor TTntList.Destroy;
begin
  Clear;
  ReallocMem(PPreList, 0);
end;

function TTntList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = Capacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TTntList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TTntList.Delete(Index: Integer; DelCount: Integer = 1);
var
  Temps: array of Pointer;
  Temp: Pointer;
  i: integer;
begin
  if DelCount < 1 then
    raise ETntInternalError.CreateFmt('Internal Error: DelCount must be greater than 0. (%d)', [DelCount]);
  if (Index < 0) or ((Index + DelCount - 1) >= FCount) then
    Error(PResStringRec(@SListIndexError), Index);

  SetLength(Temps, DelCount);
  for i := Index to Index + DelCount - 1 do begin
    Temp := Items[Index];
    Temps[i - Index] := Temp;
  end;

  Dec(FCount, DelCount);

  if Index < (FCount - Index) then begin
    if Index > 0 then
      System.Move(FList^[0], FList^[DelCount], Index * SizeOf(Pointer));
    FList := Pointer(Integer(List) + (DelCount * SizeOf(Pointer)));
  end else if Index < FCount then begin
    System.Move(FList^[Index + DelCount], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  end;

  for i := 0 to High(Temps) do begin
    Temp := Temps[i];
    if Temp <> nil then
      Notify(Temp, lnDeleted);
  end;
end;

class procedure TTntList.Error(const Msg: WideString; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

class procedure TTntList.Error(Msg: PResStringRec; Data: Integer);
begin
  TList.Error(WideLoadResString(Msg), Data);
end;

procedure TTntList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(PResStringRec(@SListIndexError), Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(PResStringRec(@SListIndexError), Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TTntList.Expand: TTntList;
begin
  if FCount = Capacity then
    Grow;
  Result := Self;
end;

function TTntList.First: Pointer;
begin
  Result := Get(0);
end;

function TTntList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(PResStringRec(@SListIndexError), Index);
  Result := FList^[Index];
end;

function TTntList.GetGrowthDelta: Integer;
const
  GROWTH_DELTA_LIMIT = 16384;
begin
  if Capacity > 64 then
    Result := Min(Capacity div 4, GROWTH_DELTA_LIMIT)
  else
    if Capacity > 8 then
      Result := 16
    else
      Result := 4;
end;

procedure TTntList.Grow;
var
  Delta: Integer;
begin
  Delta := GetGrowthDelta;
  SetCapacity(Capacity + Delta);
end;

function TTntList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TTntList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(PResStringRec(@SListIndexError), Index);
  if Count = Capacity then
    Grow;

  if Index < (Count - Index) then begin
    // move into pre buff
    CheckPreBuffSize;
    FList := Pointer(Integer(List) - SizeOf(Pointer));
    if Index > 0 then
      System.Move(FList^[1], FList^[0], Index * SizeOf(Pointer));
  end else if Index < FCount then begin
    // move into post buff
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  end;

  FList^[Index] := Item;
  Inc(FCount);

  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TTntList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TTntList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(PResStringRec(@SListIndexError), NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TTntList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(PResStringRec(@SListIndexError), Index);
  if Item <> FList^[Index] then
  begin
    Temp := FList^[Index];
    FList^[Index] := Item;
    if Temp <> nil then
      Notify(Temp, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

function TTntList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TTntList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

function TTntList.PreBuffSize: Integer;
begin
  result := (Integer(List) - Integer(PreList)) div SizeOf(Pointer);
end;

procedure TTntList.CheckPreBuffSize;
var
  Delta: Integer;
  NewSize: Integer;
begin
  if PreList = List then begin
    Delta := GetGrowthDelta;
    NewSize := (Delta + Capacity);
    // build up a pre-buff, preserve existing capacity
    ReallocMem(PPreList,                   (NewSize * SizeOf(Pointer)));
    PEndBuff := Pointer(Integer(PreList) + (NewSize * SizeOf(Pointer)));
    FList    := Pointer(Integer(PreList) + (Delta * SizeOf(Pointer)));
    System.Move(PreList[0], List[0], FCount * SizeOf(Pointer));
  end;
end;

function TTntList.GetCapacity: Integer;
begin
  result := (Integer(PEndBuff) - Integer(FList)) div SizeOf(Pointer);
end;

procedure TTntList.SetCapacity(NewCapacity: Integer);
var
  OldPreBuff: Integer;
  NewSize: Integer;
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    raise ETntGeneralError.CreateFmt(SListCapacityError, [NewCapacity]);
  if NewCapacity <> Capacity then
  begin
    CheckPreBuffSize;
    OldPreBuff := PreBuffSize;
    NewSize := NewCapacity + OldPreBuff;
    ReallocMem(PPreList,                   (NewSize * SizeOf(Pointer)));
    PEndBuff := Pointer(Integer(PreList) + (NewSize * SizeOf(Pointer)));
    FList := Pointer(Integer(PreList)    + (OldPreBuff * SizeOf(Pointer)));
  end;
end;

procedure TTntList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(PResStringRec(@SListCountError), NewCount);
  if NewCount > Capacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else if NewCount < FCount then
    Delete(NewCount, FCount - NewCount);
  FCount := NewCount;
end;

procedure QuickSort(SortList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TTntList.Sort(Compare: TListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TTntList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TTntList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Assigned(OnNotify) then
    OnNotify(Ptr, Action);
end;

procedure TTntList.Assign(ListA: TTntList; AOperator: TListAssignOp; ListB: TTntList);
var
  I: Integer;
  LTemp, LSource: TTntList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TTntList.Create; // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TTntList.Create;
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

{ TTntIntegerList }

constructor TTntIntegerList.Create;
begin
  inherited;
  FList := TTntList.Create;
  FList.OnNotify := Notify;
  FSorted := False;
end;

destructor TTntIntegerList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TTntIntegerList.AddIntArray(const Ints: array of Integer; AllowDuplicates: Boolean = True);
var
  TempList: TTntIntegerList;
  {$IFNDEF COMPILER_9_UP}
  i: integer;
  {$ENDIF}
  Value: Integer;
begin
  if AllowDuplicates then begin
    TempList := TTntIntegerList.Create;
    try
      TempList.LoadFromIntArray(Ints);
      Self.AddList(TempList);
    finally
      TempList.Free;
    end;
  end else begin
    {$IFDEF COMPILER_9_UP}
    for Value in Ints do begin
    {$ELSE}
    for i := Low(Ints) to High(Ints) do begin
      Value := Ints[i];
    {$ENDIF}
      if IndexOf(Value) = -1 then
        Add(Value);
    end;
  end;
end;

procedure TTntIntegerList.LoadFromIntArray(const Ints: array of Integer);
begin
  FList.Count := Length(Ints);
  if FList.Count > 0 then
    CopyMemory(@FList.List[0], @Ints[0], Length(Ints) * SizeOf(Integer));
  if Sorted then
    Sort;
end;

procedure TTntIntegerList.SaveToIntArray(var Ints: TIntegerDynArray);
begin
  SetLength(Ints, Count);
  if Count > 0 then
    CopyMemory(@Ints[0], @FList.List[0], Count * SizeOf(Integer));
end;

function TTntIntegerList.GetIntArray: TIntegerDynArray;
begin
  SaveToIntArray(result);
end;

function TTntIntegerList.Add(Item: Integer): Integer;
begin
  if not Sorted then
    Result := FList.Count
  else
    Find(Item, Result);

  FList.Insert(Result, Pointer(Item))
end;

procedure TTntIntegerList.AddList(List: TTntIntegerList);
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
    Add(List[i]);
end;

procedure TTntIntegerList.Insert(Index: Integer; const Item: Integer);
begin
  if Sorted then raise ETntGeneralError.Create('Can not insert into sorted list.');
  FList.Insert(Index, Pointer(Item));
end;

procedure TTntIntegerList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TTntIntegerList.Clear;
begin
  FList.Clear;
end;

procedure TTntIntegerList.Exchange(Index1, Index2: Integer);
begin
  if Sorted then
    raise ETntInternalError.Create('Internal Error: Can not exhange items in a sorted list.');
  FList.Exchange(Index1, Index2);
end;

procedure TTntIntegerList.Move(Index1, Index2: Integer);
begin
  if Sorted then
    raise ETntInternalError.Create('Internal Error: Can not move items in a sorted list.');
  FList.Move(Index1, Index2);
end;

function TTntIntegerList.Get(Index: Integer): Integer;
begin
  result := Integer(FList[Index]);
end;

procedure TTntIntegerList.Put(Index: Integer; const Value: Integer);
begin
  FList[Index] := Pointer(Value);
end;

function TTntIntegerList.GetCount: Integer;
begin
  result := FList.Count;
end;

function TTntIntegerList.Find(const Item: Integer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareInts(Integer(FList[I]), Item);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

function TTntIntegerList.IndexOf(Item: Integer): Integer;
begin
  if not Sorted then
    result := FList.IndexOf(Pointer(Item))
  else if not Find(Item, Result) then
    Result := -1;
end;

procedure TTntIntegerList.Sort;
begin
  FList.Sort(TListSortCompare(@CompareInts));
end;

procedure TTntIntegerList.SetSorted(const Value: boolean);
begin
  if Sorted <> Value then begin
    FSorted := Value;
    if Sorted then Sort;
  end;
end;

procedure TTntIntegerList.DeleteValue(Value: Integer);
var
  idx: integer;
begin
  idx := IndexOf(Value);
  if idx <> -1 then
    Delete(idx);
end;

procedure TTntIntegerList.ChangeValue(Value, NewValue: Integer);
var
  idx: integer;
begin
  idx := IndexOf(Value);
  if idx <> -1 then
    put(idx, NewValue);
end;

procedure TTntIntegerList.SaveToIntList(IntList: TTntIntegerList);
begin
  IntList.LoadFromIntArray(GetIntArray);
end;

procedure TTntIntegerList.LoadFromIntList(IntList: TTntIntegerList);
begin
  IntList.SaveToIntList(Self);
end;

function TTntIntegerList.GetCommaText: WideString;
begin
  Result := IntArrayAsCommaText(GetIntArray);
end;

procedure TTntIntegerList.SetCommaText(const Value: WideString);
begin
  LoadFromIntArray(CommaTextAsIntArray(Value));
end;

procedure TTntIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Assigned(OnNotify) then
    OnNotify(Ptr, Action);
end;

function TTntIntegerList.Equals(AList: TTntIntegerList): Boolean;
begin
  if Count <> AList.Count then
    Result := False
  else
    Result := CompareMem(@FList.List[0], @AList.FList.List[0], Count * SizeOf(Integer));
end;

function SameIntArrays(const IntArray1, IntArray2: array of Integer): Boolean;
var
  IntListA: TTntIntegerList;
  IntListB: TTntIntegerList;
begin
  IntListA := TTntIntegerList.Create;
  IntListB := TTntIntegerList.Create;
  try
    IntListA.Sorted := True;
    IntListA.AddIntArray(IntArray1, False);

    IntListB.Sorted := True;
    IntListB.AddIntArray(IntArray2, False);

    Result := IntListA.Equals(IntListB);
  finally
    IntListA.Free;
    IntListB.Free;
  end;
end;

{ TTntComponentList }

procedure TTntComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(OnNotify) then
    OnNotify(Ptr, Action);
end;

{ TTntComponentRef }

procedure TTntComponentRef.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FValue) then
    FValue := nil;
end;

procedure TTntComponentRef.SetValue(const Value: TComponent);
begin
  if (FValue <> nil) then begin
    FValue.RemoveFreeNotification(Self);
  end;
  FValue := Value;
  if FValue <> nil then begin
    FValue.FreeNotification(Self);
  end;
end;

end.
