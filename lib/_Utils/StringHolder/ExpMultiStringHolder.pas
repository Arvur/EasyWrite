{*******************************************************************************

         Explido Multiple String Holder Component
         Version 1.0

         Copyright (C) 2004 Explido Software GmbH & Co. KG
         All Rights Reserved.

 The contents of this file are subject to the Mozilla Public License
 Version 1.1 (the "License"); you may not use this file except in compliance
 with the License. You may obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1.1.html

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Original Code is: ExpMultiStringHolder.PAS, released on 2004-05-12.

 The Initial Developer of the Original Code is Marc Geldon [marcgeldon@web.de]
 Portions created by Marc Geldon are Copyright (C) 2004 Marc Geldon.
 All Rights Reserved.

 Contributor(s):
 -

 Change(s):
 -

 Last Modified: 2004-05-12
*******************************************************************************}

unit ExpMultiStringHolder;

interface

uses
  SysUtils, Classes, Dialogs;

type
  EExpMSH_Exception = class(Exception);

  {TExpMSH_CollectionItem}
  TExpMSH_CollectionItem = class(TCollectionItem)
  private
     FName: ShortString;
     FStrings: TStrings;
     procedure SetName(Value: ShortString);
     procedure SetStrings(const Value: TStrings);
  protected
     function GetDisplayName: String; override;
  public
     constructor Create(Collection: TCollection); override;
     destructor Destroy; override;
  published
     property Name: ShortString read FName write SetName;
     property Strings: TStrings read FStrings write SetStrings;
  end;

  {TExpMSH_Collection}
  TExpMSH_Collection = class(TCollection)
  private
  protected
     function GetItem(Index: Integer): TExpMSH_CollectionItem;
     procedure SetItem(Index: Integer; Value: TExpMSH_CollectionItem);
  public
     function DoesNameExist(Name: String): Boolean;
     property Items[Index: Integer]: TExpMSH_CollectionItem read GetItem write SetItem;
     function Add: TExpMSH_CollectionItem;
     function Insert(Index: Integer): TExpMSH_CollectionItem;
  end;

  {TExpMultiStringHolder}
  TExpMultiStringHolder = class(TComponent)
  private
    { Private-Deklarationen }
    FMultipleStrings: TExpMSH_Collection;
    procedure SetMultipleStrings(Value: TExpMSH_Collection);
    function GetItemByName(Name: ShortString): TExpMSH_CollectionItem;
    function GetItemStringsByName(Name: ShortString): TStrings;
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemByName[Name: ShortString]: TExpMSH_CollectionItem read GetItemByName;
    property ItemStringsByName[Name: ShortString]: TStrings read GetItemStringsByName;
  published
    { Published-Deklarationen }
    property MultipleStrings: TExpMSH_Collection read FMultipleStrings write SetMultipleStrings;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Explido', [TExpMultiStringHolder]);
end;

// -----------------------------------------------------------------------------
// TExpMSH_CollectionItem
// -----------------------------------------------------------------------------
procedure TExpMSH_CollectionItem.SetName(Value: ShortString);
begin
   // Leerzeichen entfernen
   Value := UpperCase(Trim(Value));

   // soll der Name gelöscht werden bzw. leerer Name, dann
   // Namen löschen und Prozedur beenden
   If (Value = '') then
   begin
      FName := '';
      Exit;
   end;

   // hat ein anderes CollectionItem auf der Collection bereits den Namen?
   // wenn nicht -> Name setzen
   // wenn ja -> Fehler
   If (TExpMSH_Collection(Collection).DoesNameExist(Value) = False) then
      FName := Value
   else
      raise EExpMSH_Exception.Create('Name existiert bereits.');
end;

procedure TExpMSH_CollectionItem.SetStrings(const Value: TStrings);
begin
   FStrings.Assign(Value);
end;

function TExpMSH_CollectionItem.GetDisplayName: String;
begin
  // diese Funktion handelt den angezeigten Namen im CollectionEditor von Delphi
  If (FName <> '') then
     Result := FName
  else
     Result := '(kein Name)';
end;

constructor TExpMSH_CollectionItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
   FStrings := TStringList.Create;
end;

destructor TExpMSH_CollectionItem.Destroy;
begin
   FreeAndNil(FStrings);
   inherited Destroy;
end;

// -----------------------------------------------------------------------------
// TExpMSH_Collection
// -----------------------------------------------------------------------------
function TExpMSH_Collection.GetItem(Index: Integer): TExpMSH_CollectionItem;
begin
   Result := TExpMSH_CollectionItem(inherited GetItem(Index));
end;

procedure TExpMSH_Collection.SetItem(Index: Integer; Value: TExpMSH_CollectionItem);
begin
   inherited SetItem(Index, Value);
end;

function TExpMSH_Collection.DoesNameExist(Name: String): Boolean;
var
   i: Integer;
begin
   // hat ein CollectionItem auf der Collection schon denselben Namen?
   Result := False;

   Name := UpperCase(Name);

   for i := 0 to Count-1 do
   begin
      If (Items[i].Name = Name) then
         Result := True;
   end;
end;

function TExpMSH_Collection.Add: TExpMSH_CollectionItem;
begin
   Result := TExpMSH_CollectionItem.Create(Self);
   inherited Added(TCollectionItem(Result));
end;

function TExpMSH_Collection.Insert(Index: Integer): TExpMSH_CollectionItem;
begin
   Result := Add;
   Result.Index := Index;
end;

// -----------------------------------------------------------------------------
// TExpMultiStringHolder
// -----------------------------------------------------------------------------

constructor TExpMultiStringHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMultipleStrings := TExpMSH_Collection.Create(TExpMSH_CollectionItem);
end;

destructor TExpMultiStringHolder.Destroy;
begin
  FreeAndNil(FMultipleStrings);
  inherited Destroy;
end;

procedure TExpMultiStringHolder.SetMultipleStrings(Value: TExpMSH_Collection);
begin
  FMultipleStrings.Assign(Value);
end;

function TExpMultiStringHolder.GetItemByName(Name: ShortString): TExpMSH_CollectionItem;
var
   i: Integer;
begin
   Result := nil;

   for i := 0 to MultipleStrings.Count-1 do
   begin
      If (MultipleStrings.Items[i].Name = Name) then
      begin
         Result := MultipleStrings.Items[i];
         break;
      end;
   end;

   if (Result = nil) then
     raise EExpMSH_Exception.Create('Name wurde nicht gefunden.');
end;

function TExpMultiStringHolder.GetItemStringsByName(Name: ShortString): TStrings;
begin
   Result := GetItemByName(Name).Strings;
end;



end.
