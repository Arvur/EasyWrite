{*******************************************************}
{                                                       }
{                       EhLib v4.1                      }
{                     (Build 4.1.01)                    }
{                    Registration unit                  }
{                                                       }
{   Copyright (c) 1998-2004 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

{$I EhLib.Inc}

unit PropStorageReg;

interface

procedure Register;

implementation

uses Classes, TypInfo, Windows, Controls, SysUtils,
 {$IFDEF EH_LIB_6}DesignIntf, DesignEditors, VCLEditors, Variants, {$ELSE}DsgnIntf, {$ENDIF}
  EhLibVCL,
  // ComCtrls, DB,
  PropStorageEh, PropStorageEditEh;

// Property storing

{ TPropertyNamesEhProperty }

type
  TPropertyNamesEhProperty = class(TPropertyEditor {TClassProperty})
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TPropStorageEhEditor }

  TPropStorageEhEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TPropertyNamesEhProperty }

procedure TPropertyNamesEhProperty.Edit;
var
  Obj: TPersistent;
begin
  Obj := GetComponent(0);
  while (Obj <> nil) and not (Obj is TComponent) do
    Obj := GetUltimateOwner(Obj);
  if EditPropStorage(TPropStorageEh(Obj)) then
    Designer.Modified;
end;

function TPropertyNamesEhProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly {, paSubProperties}];
end;

function TPropertyNamesEhProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

{ TPropStorageEhEditor }

procedure TPropStorageEhEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditPropStorage(TPropStorageEh(Component))  then
         Designer.Modified;
  end;
end;

function TPropStorageEhEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Stored properties ...';
  end;
end;

function TPropStorageEhEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFDEF EH_LIB_6}

type
 TPropStorageEhSelectionEditor = class(TSelectionEditor)
 public
   procedure RequiresUnits(Proc: TGetStrProc); override;
 end;

{ TPropStorageEhSelectionEditor }

procedure TPropStorageEhSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
   inherited RequiresUnits(Proc);
   Proc('PropFilerEh');
end;

{$ENDIF}

{ TRegistryKeyProperty }
type

  TRegistryKeyProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TRegistryKeyProperty }

function TRegistryKeyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList];
end;

function TRegistryKeyProperty.GetValue: string;
begin
  if not RegistryKeyToIdent(HKEY(GetOrdValue), Result) then
    FmtStr(Result, '%d', [GetOrdValue]);
end;

procedure TRegistryKeyProperty.GetValues(Proc: TGetStrProc);
begin
  GetRegistryKeyValues(Proc);
end;

procedure TRegistryKeyProperty.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToRegistryKey(Value, NewValue)
    then SetOrdValue(NewValue)
    else inherited SetValue(Value);
end;

procedure Register;
begin
{$IFDEF EH_LIB_6}
  GroupDescendentsWith(TPropStorageEh, Controls.TControl);
  GroupDescendentsWith(TPropStorageManagerEh, Controls.TControl);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TStrings), TPropStorageEh, 'StoredProps', TPropertyNamesEhProperty);
  RegisterComponentEditor(TPropStorageEh, TPropStorageEhEditor);
{$IFDEF EH_LIB_6}
  RegisterSelectionEditor(TPropStorageEh, TPropStorageEhSelectionEditor);
{$ENDIF}

  RegisterComponents('EhLib', [TPropStorageEh, TIniPropStorageManEh, TRegPropStorageManEh]);
  RegisterPropertyEditor(TypeInfo(HKEY), TRegPropStorageManEh, 'Key', TRegistryKeyProperty);
end;

end.
