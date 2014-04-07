
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXVarArrayDataSet;

{$INCLUDE TntCompilers.inc}

interface

uses Classes, Db, TntLXDataSet, TntLXClasses;

type
  PVariantArray = ^TVariantArray;
  TVariantArray = array of Variant;
  TVarArrayList = class(TTntList)
  private
    function Get(Index: Integer): PVariantArray;
    procedure Put(Index: Integer; const Value: PVariantArray);
  public
    property Items[Index: Integer]: PVariantArray read Get write Put; default;
  end;

  TTntVarArrayDataSet = class(TTntDataSet)
  private
    procedure DumpAllRecords;
  protected
    FRecords: TVarArrayList;
    procedure InternalUpdateBookmarkList(BookmarkList: TTntList; Bookmark: TBookmark); override;
    function GetFieldValue(Bookmark: TBookmark; FieldIndex: Integer): Variant; override;
    procedure SetFieldValue(Bookmark: TBookmark; FieldIndex: Integer; const Value: Variant); override;
    procedure FreeRecord(Bookmark: TBookmark); override;
    function CreateRecord: TBookmark; override;
    procedure InternalClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Variants;

{ TVarArrayList }

function TVarArrayList.Get(Index: Integer): PVariantArray;
begin
  Result := PVariantArray(inherited Get(Index));
end;

procedure TVarArrayList.Put(Index: Integer; const Value: PVariantArray);
begin
  inherited Put(Index, Value);
end;

{ TTntVarArrayDataSet }

constructor TTntVarArrayDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FRecords := TVarArrayList.Create;
end;

destructor TTntVarArrayDataSet.Destroy;
begin
  inherited;
  DumpAllRecords;
  FreeAndNil(FRecords);
end;

procedure TTntVarArrayDataSet.DumpAllRecords;
var
  i: integer;
begin
  for i := 0 to FRecords.Count - 1 do
    Dispose(FRecords[i]);
  FRecords.Clear;
end;

function TTntVarArrayDataSet.CreateRecord: TBookmark;
begin
  New(PVariantArray(Result));
  SetLength(PVariantArray(Result)^, FieldCount);
  FRecords.Add(Result);
end;

procedure TTntVarArrayDataSet.FreeRecord(Bookmark: TBookmark);
begin
  Dispose(PVariantArray(Bookmark));
  FRecords.Delete(FRecords.IndexOf(Bookmark));
end;

function TTntVarArrayDataSet.GetFieldValue(Bookmark: TBookmark; FieldIndex: Integer): Variant;
begin
  if Fields[FieldIndex].FieldKind <> fkCalculated then
    Result := PVariantArray(Bookmark)^[FieldIndex]
  else
    Result := Unassigned;
end;

procedure TTntVarArrayDataSet.SetFieldValue(Bookmark: TBookmark; FieldIndex: Integer; const Value: Variant);
begin
  PVariantArray(Bookmark)^[FieldIndex] := Value;
end;

procedure TTntVarArrayDataSet.InternalUpdateBookmarkList(BookmarkList: TTntList; Bookmark: TBookmark);
begin
  BookmarkList.Assign(FRecords);
end;

procedure TTntVarArrayDataSet.InternalClose;
begin
  inherited;
  DumpAllRecords;
end;

end.
