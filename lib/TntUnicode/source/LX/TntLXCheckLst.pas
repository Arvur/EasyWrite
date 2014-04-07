
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXCheckLst;

{$INCLUDE TntCompilers.inc}

interface

uses
  TntCheckLst, TntClasses;

type
  TTntCheckListBoxLX = class(TTntCheckListBox)
  public
    function AnyChecks: Boolean;
    function GetCheckedCount: Integer;
    procedure GetCheckedStrings(sList: TTntStrings);
    procedure SetCheckedStrings(sList: TTntStrings; ByObject: Boolean = False);
  end;

implementation

function TTntCheckListBoxLX.GetCheckedCount: Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Items.count - 1 do
    if Checked[i] then
      Inc(Result);
end;

function TTntCheckListBoxLX.AnyChecks: Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Items.Count - 1 do begin
    if Checked[i] then begin
      Result := True;
      break;
    end;
  end;
end;

procedure TTntCheckListBoxLX.GetCheckedStrings(sList: TTntStrings);
var
  i: integer;
begin
  sList.BeginUpdate;
  try
    sList.Clear;
    for i := 0 to Items.count - 1 do
      if Checked[i] then
        sList.AddObject(Items[i], Items.Objects[i]);
  finally
    sList.EndUpdate;
  end;
end;

procedure TTntCheckListBoxLX.SetCheckedStrings(sList: TTntStrings; ByObject: Boolean = False);
var
  i: integer;
begin
  Items.BeginUpdate;
  try
    for i := 0 to Items.count - 1 do
      if ByObject then
        Checked[i] := sList.IndexOfObject(Items.Objects[i]) <> -1
      else
        Checked[i] := sList.IndexOf(Items[i]) <> -1;
  finally
    Items.EndUpdate;
  end;
end;

end.
