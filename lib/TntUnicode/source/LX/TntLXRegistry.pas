
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXRegistry;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, TntRegistry, TntClasses, TntLXUtils;

type
  TTntRegistryLX = class(TTntRegistry)
  private
    function NormalizedPath: WideString;
    function MakeDescendPath(Key: WideString): WideString;
  public
    procedure GotoKey(Key: WideString; CanCreate: Boolean);
    function CanDescend(Key: WideString): boolean;
    procedure Descend(Key: WideString; CanCreate: Boolean);
    procedure Ascend;

    function ReadIntegerDef(Name: WideString; Def: Integer): integer;
    function ReadFloatDef(Name: WideString; Def: Real): Real;
    function ReadStringDef(Name, Def: WideString): WideString;
    function ReadBoolDef(Name: WideString; Def: Boolean): Boolean;

    procedure LoadForm(Form: TCustomForm);
    procedure SaveForm(Form: TCustomForm);
  end;

function RegReadBoolean(RootKey: HKEY; const RegKey, RegValue: WideString): Boolean;
function RegReadInteger(RootKey: HKEY; const RegKey, RegValue: WideString): Integer;
function RegReadWideString(RootKey: HKEY; const RegKey, RegValue: WideString): WideString;

function RegReadBooleanDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: Boolean): Boolean;
function RegReadIntegerDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: Integer): Integer;
function RegReadWideStringDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: WideString): WideString;

procedure RegWriteBoolean(RootKey: HKEY; const RegKey, RegValue: WideString; Value: Boolean);
procedure RegWriteInteger(RootKey: HKEY; const RegKey, RegValue: WideString; Value: Integer);
procedure RegWriteWideString(RootKey: HKEY; const RegKey, RegValue: WideString; Value: WideString);

function RegKeyExists(RootKey: HKEY; const RegKey: WideString): Boolean;
function RegValueExists(RootKey: HKEY; const RegKey, RegValue: WideString): Boolean;

procedure RegDeleteValueLx(RootKey: HKEY; const RegKey, RegValue: WideString);

procedure RegReplaceEx2(Reg: TTntRegistryLX; const Source, Dest: TTntStrings; OnProgress: TIntegerEvent = nil);
procedure RegReplaceEx(const Source, Dest: TTntStrings; RootKey: HKEY; TopLevelKey: WideString = ''; OnProgress: TIntegerEvent = nil);
function GetExpandedPathFromReg(const RawPath: WideString): WideString;

implementation

uses
  Consts, RTLConsts, TntSysUtils;

function RegReadBoolean(RootKey: HKEY; const RegKey, RegValue: WideString): Boolean;
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, False);
    Result := Reg.ReadBool(RegValue);
  finally
    Reg.Free;
  end;
end;

function RegReadInteger(RootKey: HKEY; const RegKey, RegValue: WideString): Integer;
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, False);
    Result := Reg.ReadInteger(RegValue);
  finally
    Reg.Free;
  end;
end;

function RegReadWideString(RootKey: HKEY; const RegKey, RegValue: WideString): WideString;
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, False);
    Result := Reg.ReadString(RegValue);
  finally
    Reg.Free;
  end;
end;

function RegReadBooleanDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: Boolean): Boolean;
begin
  if RegValueExists(RootKey, RegKey, RegValue) then
    Result := RegReadBoolean(RootKey, RegKey, RegValue)
  else
    Result := Default;
end;

function RegReadIntegerDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: Integer): Integer;
begin
  if RegValueExists(RootKey, RegKey, RegValue) then
    Result := RegReadInteger(RootKey, RegKey, RegValue)
  else
    Result := Default;
end;

function RegReadWideStringDef(RootKey: HKEY; const RegKey, RegValue: WideString; Default: WideString): WideString;
begin
  if RegValueExists(RootKey, RegKey, RegValue) then
    Result := RegReadWideString(RootKey, RegKey, RegValue)
  else
    Result := Default;
end;

procedure RegWriteBoolean(RootKey: HKEY; const RegKey, RegValue: WideString; Value: Boolean);
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, True);
    Reg.WriteBool(RegValue, Value);
  finally
    Reg.Free;
  end;
end;

procedure RegWriteInteger(RootKey: HKEY; const RegKey, RegValue: WideString; Value: Integer);
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, True);
    Reg.WriteInteger(RegValue, Value);
  finally
    Reg.Free;
  end;
end;

procedure RegWriteWideString(RootKey: HKEY; const RegKey, RegValue: WideString; Value: WideString);
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := RootKey;
    Reg.GotoKey(RegKey, True);
    Reg.WriteString(RegValue, Value);
  finally
    Reg.Free;
  end;
end;

function RegKeyExists(RootKey: HKEY; const RegKey: WideString): Boolean;
var
  Reg: TTntRegistryLX;
begin
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := RootKey;
    Result := Reg.OpenKey(RegKey, False);
  finally
    Reg.Free;
  end;
end;

function RegValueExists(RootKey: HKEY; const RegKey, RegValue: WideString): Boolean;
var
  Reg: TTntRegistryLX;
begin
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := RootKey;
    Result := Reg.OpenKey(RegKey, False) and Reg.ValueExists(RegValue);
  finally
    Reg.Free;
  end;
end;

procedure RegDeleteValueLx(RootKey: HKEY; const RegKey, RegValue: WideString);
var
  Reg: TTntRegistryLX;
begin
  Assert(RegKey <> '');
  if RegValueExists(RootKey, RegKey, RegValue) then begin
    Reg := TTntRegistryLX.Create;
    try
      Reg.RootKey := RootKey;
      Reg.GotoKey(RegKey, True);
      Reg.DeleteValue(RegValue);
    finally
      Reg.Free;
    end;
  end;
end;

//----------------------------------------------------------------

function RegAbsolutePath(RegPath: WideString): WideString;
begin
  result := RegPath;
  // ensure leading RegPathDelim
  if  (Length(result) = 0)
  or (result[1] <> PathDelim) then
    result := PathDelim + result;
  // if not root, remove trailing backslash
  if (result <> PathDelim) then
    result := WideExcludeTrailingBackslash(result);
end;

procedure RegReplaceEx2(Reg: TTntRegistryLX; const Source, Dest: TTntStrings; OnProgress: TIntegerEvent);
var
  i: integer;
  Keys: TTntStringList;
  Values: TTntStringList;
  TempValue: WideString;
  s: integer;
begin
  Values := TTntStringList.Create;
  Reg.GetValueNames(Values);
  for i := 0 to Values.Count - 1 do begin
    if Reg.GetDataType(Values[i]) in [rdString, rdExpandString] then begin
      TempValue := Reg.ReadString(Values[i]);
      for s := 0 to Source.Count - 1 do
        if WideTextPos(Source[s], TempValue) <> 0 then begin
          Reg.WriteString(Values[i], Tnt_WideStringReplace(TempValue, Source[s], Dest[s], [rfReplaceAll, rfIgnoreCase]));
      end;
    end;
  end;
  Values.Free;

  Keys := TTntStringList.Create;
  Reg.GetKeyNames(Keys);
  for i := 0 to Keys.count - 1 do begin
    if Assigned(OnProgress) then
      OnProgress(i);
    if Reg.CanDescend(Keys[i]) then begin
      Reg.Descend(Keys[i], False);
      RegReplaceEx2(Reg, Source, Dest, OnProgress);
      Reg.Ascend;
    end;
  end;

  Keys.Free;
end;

procedure RegReplaceEx(const Source, Dest: TTntStrings; RootKey: HKEY; TopLevelKey: WideString = ''; OnProgress: TIntegerEvent = nil);
var
  Reg: TTntRegistryLX;
begin
  Assert(Source.Count = Dest.Count);
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKey(TopLevelKey, False) then
      RegReplaceEx2(Reg, Source, Dest, OnProgress);
  finally
    Reg.Free;
  end;
end;

function GetExpandedPathFromReg(const RawPath: WideString): WideString;
var
  AnsiExePath: AnsiString;
begin
  if Win32PlatformIsUnicode then begin
    SetLength(Result, MAX_PATH + 1);
    ExpandEnvironmentStringsW(PWideChar(RawPath), PWideChar(Result), MAX_PATH + 1);
    Result := PWideChar(Result);
  end else begin
    SetLength(AnsiExePath, MAX_PATH + 1);
    ExpandEnvironmentStringsA(PAnsiChar(AnsiString(RawPath)), PAnsiChar(AnsiExePath), MAX_PATH + 1);
    Result := PAnsiChar(AnsiExePath);
  end;
end;

{ TTntRegistryLX }

function TTntRegistryLX.NormalizedPath: WideString;
begin
  result := WideExcludeTrailingBackslash(RegAbsolutePath(CurrentPath));
end;

procedure TTntRegistryLX.GotoKey(Key: WideString; CanCreate: Boolean);
var
  AbsPath: WideString;
begin
  AbsPath := RegAbsolutePath(Key);
  if not OpenKey(AbsPath, CanCreate) then
    raise ETntInternalError.Create('Internal Error: Unable to open key: ' + AbsPath);
end;

function TTntRegistryLX.MakeDescendPath(Key: WideString): WideString;
begin
  if Pos(PathDelim, Key) > 0 then
    raise ETntInternalError.Create('Internal Error: Key contains a backslash: ' + Key
      + CRLF + '  Path = ' + Self.NormalizedPath);
  result := NormalizedPath + PathDelim + Key;
end;

function TTntRegistryLX.CanDescend(Key: WideString): boolean;
var
  TestReg: TTntRegistry;
begin
  result := KeyExists(MakeDescendPath(Key));
  if Result then begin
    TestReg := TTntRegistry.Create;
    try
      TestReg.RootKey := Self.RootKey;
      if not TestReg.OpenKey(RegAbsolutePath(MakeDescendPath(Key)), False) then
        Result := False;
    finally
      TestReg.Free;
    end;
  end;
end;

procedure TTntRegistryLX.Descend(Key: WideString; CanCreate: Boolean);
begin
  GotoKey(MakeDescendPath(Key), CanCreate);
end;

procedure TTntRegistryLX.Ascend;
var
  PosSlash: Integer;
  path: WideString;
begin
  path := NormalizedPath;
  PosSlash := WideLastDelimiter(PathDelim, path);
  if PosSlash = 0 then
    raise ETntInternalError.Create('Internal Error: Can not ascend any more.');
  SetLength(path, PosSlash - 1);
  GotoKey(path, False);
end;

function TTntRegistryLX.ReadIntegerDef(Name: WideString; Def: Integer): integer;
begin
  if ValueExists(Name) then
    result := ReadInteger(Name)
  else
    result := Def;
end;

function TTntRegistryLX.ReadFloatDef(Name: WideString; Def: Real): Real;
begin
  if ValueExists(Name) then
    result := ReadFloat(Name)
  else
    result := Def;
end;

function TTntRegistryLX.ReadStringDef(Name: WideString; Def: WideString): WideString;
begin
  if ValueExists(Name) then
    result := ReadString(Name)
  else
    result := Def;
end;

function TTntRegistryLX.ReadBoolDef(Name: WideString; Def: Boolean): Boolean;
begin
  if ValueExists(Name) then
    result := ReadBool(Name)
  else
    result := Def;
end;

//===========================================
function FormSizeFixed(Form: TCustomForm): Boolean;
begin
  result := Form.BorderStyle in [bsDialog, bsSingle, bsNone, bsToolWindow];
end;

const
  TOP_VAL = 'Top';
  LEFT_VAL = 'Left';
  WINDOW_PLACE_VAL = 'WindowPlacement';

procedure TTntRegistryLX.SaveForm(Form: TCustomForm);
var
  Placement: TWindowPlacement;
begin
  Assert(Form <> nil, 'Form not assigned.');
  if FormSizeFixed(Form) then begin
    // fixed width and height
    WriteInteger(TOP_VAL, Form.Top);
    WriteInteger(LEFT_VAL, Form.Left);
  end else begin
    // resizable
    Placement.length := SizeOf(Placement);
    GetWindowPlacement(Form.Handle, @Placement);
    WriteBinaryData(WINDOW_PLACE_VAL, Placement, SizeOf(Placement));
  end;
end;

type TAccessCustomForm = class(TCustomForm);

procedure TTntRegistryLX.LoadForm(Form: TCustomForm);
var
  Placement: TWindowPlacement;
begin
  Assert(Form <> nil, 'Form not assigned.');
  if FormSizeFixed(Form) then begin
    // fixed width and height
    if ValueExists(TOP_VAL) or ValueExists(LEFT_VAL) then
      TAccessCustomForm(Form).Position := poDesigned;
    Form.Top := ReadIntegerDef(TOP_VAL, Form.Top);
    Form.Left := ReadIntegerDef(LEFT_VAL, Form.Left);
  end else if ValueExists(WINDOW_PLACE_VAL) then begin
    // resizable
    TAccessCustomForm(Form).Position := poDesigned;
    ReadBinaryData(WINDOW_PLACE_VAL, Placement, SizeOf(Placement));
    Placement.length := SizeOf(Placement);
    SetWindowPlacement(Form.Handle, @Placement);
  end;
end;

end.
