
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXAdoDataSet;

{$INCLUDE TntCompilers.inc}

{------------------
   Main Overrides:
 
   --BUG FIXES--
   1. GetFieldClass:         Provide date/time field classes to correct international issues.
   2. GetFieldClass:         Use TTntWideStringField: corrects TWideStringField.GetAsWideString.
   3. GetFieldClass:         Convert wide memo fields to WideString fields.
   3. InternalInitFieldDefs: Convert memo fields to WideString fields.
   4. UpdateIndexDefs:       Bug fix for AdoExpress (ADO 2.7 and multi-column indexes)
   5. GetFieldData:          Corrects issue in TWideStringField.GetAsWideString for NULL values
   6. SetFieldData:          Ignores attempts to set field data when no changes are made.
   7. Locate:                Fully supports Unicode KeyValues.
   8. Lookup:                Fully supports Unicode KeyValues.

   --FEATURES--
   9. DoAfterOpen:           Updates .Required, .DefaultExpression, and .DisplayLabel for fields
   10. DoAfterInsert:         Populates new record with default values.
   11. GetStateFieldValue:    Uses default value for dsOldValue and when (State = dsInsert)
   12. Locate:                Employs Seek() (which is much faster) when possible.

   --ADDITONS--
   13. GetFieldBaseInfo:      Provides a way to determine a fields' underlying table and column name.
   -------------------}

interface

uses
  SysUtils, Classes, Db, ADODB, ADOInt, Variants,
  {$IFDEF COMPILER_9_UP} WideStrUtils, {$ELSE} TntWideStrUtils, {$ENDIF}
  TntClasses;

{TNT-WARN TADODataSet}
type
  TTntADODataSetLX = class(TADODataSet{TNT-ALLOW TADODataSet})
  private
    StoredDefaults: TTntStringList;
    FIsMSSqlServer: Boolean;
    FIsMSJet: Boolean;
    FIsOracle: Boolean;
    procedure UpdateIndexDefs_FixedForAdo27;
    procedure RetrieveDefaultExpressions;
    function GetFieldDefaultValue(Field: TField): Variant;
    procedure ApplyDefaultExpressions;
    function SetFieldData_IsNeeded(Field: TField; Buffer: Pointer; NativeFormat: Boolean): boolean;
    function GetParamCheck: Boolean;
    procedure SetParamCheck(const Value: Boolean);
  private
    FLookupCursor: _Recordset;
    function LocateRecord(const KeyFields: AnsiString;
      const KeyValues: OleVariant; Options: TLocateOptions;
      SyncCursor: Boolean): Boolean;
  protected
    FCaptureFieldDescriptions: Boolean;
    FAssumeProviderCanOpenSchema: Boolean;
    FSupressOpenSchemaErrors: Boolean;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    {$IFNDEF COMPILER_10_UP}
    procedure InternalInitFieldDefs; override;
    {$ENDIF}
    procedure UpdateIndexDefs; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoAfterClose; override;
    procedure DoAfterInsert; override;
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
    procedure DestroyLookupCursor; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure InternalLast; override;
  protected
    function GetIsJetBooleanFieldRequired(Field: TField): Boolean; virtual;
    procedure GetFieldBaseInfo(Field: TField;
      out BaseTableName, BaseColumnName: WideString);
    procedure GetFieldBaseInfoEx(Field: TField; out BaseCatalogName, BaseSchemaName: Variant;
      out BaseTableName, BaseColumnName: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFNDEF COMPILER_10_UP}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
    {$ENDIF}
    function Locate(const KeyFields: AnsiString; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: AnsiString; const KeyValues: Variant;
      const ResultFields: AnsiString): Variant; override;
    procedure Requery(Options: TExecuteOptions = []); virtual;
    property IsMSSqlServer: Boolean read FIsMSSqlServer;
    property IsMSJet: Boolean read FIsMSJet;
    property IsOracle: Boolean read FIsOracle;
    property SupressOpenSchemaErrors: Boolean read FSupressOpenSchemaErrors write FSupressOpenSchemaErrors default False;
  published
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck default False;
  end;
  TTntADODataSetLXClass = class of TTntADODataSetLX;

{TNT-WARN TADOTable}
  TTntADOTableLX = class(TTntADODataSetLX)
  private
    function GetTableDirect: Boolean;
    procedure SetTableDirect(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetIndexNames(List: TTntStrings);
    property IndexDefs;
  published
    property CommandTimeout;
    property EnableBCD;
    property IndexFieldNames;
    property IndexName;
    property MasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored False;
    property TableDirect: Boolean read GetTableDirect write SetTableDirect default False;
    property TableName: WideString read GetCommandText write SetCommandText;
  end;

{TNT-WARN TADOQuery}
  TTntADOQueryLX = class(TTntADODataSetLX)
  private
    FSQL: TTntStrings;
    FRowsAffected: Integer;
    function GetSQL: TTntStrings;
    procedure SetSQL(const Value: TTntStrings);
  protected
    procedure QueryChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecSQL: Integer; {for TQuery compatibility}
    property RowsAffected: Integer read FRowsAffected;
  published
    property CommandTimeout;
    property DataSource;
    property EnableBCD;
    property ParamCheck;
    property Parameters;
    property Prepared;
    property SQL: TTntStrings read GetSQL write SetSQL;
  end;

implementation
 
uses
  TntLXUtils, TntLXDBUtils, ActiveX, TntDB, TntSysUtils, TntLXDataSet, ComObj;

resourcestring
  SJetDatabaseNeedsCompacting = 'This database should be compacted and repaired.';
 
type
  TAccessADOCommand = class(TADOCommand);
  TAccessADOConnection = class(TADOConnection);
 
{ TTntADOSchemaCache }
 
type
  TTntADOSchemaCache = class(TComponent)
  private
    SchemaList: TInterfaceList;
    SchemaTbls: TTntStringList;
    procedure ConnectionChange(Sender: TObject; Connecting: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
 
constructor TTntADOSchemaCache.Create(AOwner: TComponent);
begin
  inherited;
  Include(FComponentStyle, csTransient);
  SchemaList := TInterfaceList.Create;
  SchemaTbls := TTntStringList.Create;
  if AOwner <> nil then
    TAccessADOConnection(AOwner as TADOConnection).RegisterClient(Self, ConnectionChange);
end;
 
destructor TTntADOSchemaCache.Destroy;
begin
  FreeAndNil(SchemaTbls);
  FreeAndNil(SchemaList);
  inherited;
end;
 
procedure TTntADOSchemaCache.ConnectionChange(Sender: TObject; Connecting: Boolean);
begin
  if (not Connecting) then begin
    SchemaTbls.Clear;
    SchemaList.Clear;
  end;
end;
 
function GetSchemaCache(Connection: TADOConnection): TTntADOSchemaCache;
var
  i: integer;
begin
  Assert(Connection <> nil);
  Result := nil;
  for i := 0 to Connection.ComponentCount - 1 do begin
    if Connection.Components[i] is TTntADOSchemaCache then begin
      Result := TTntADOSchemaCache(Connection.Components[i]);
      break { found it! }
    end;
  end;
  if (Result = nil) then
    Result := TTntADOSchemaCache.Create(Connection);
end;

{ TTntADODataSetLX }
 
constructor TTntADODataSetLX.Create(AOwner: TComponent);
begin
  inherited;
  inherited ParamCheck := False;
  FCaptureFieldDescriptions := False;
  StoredDefaults := TTntStringList.Create;
end;
 
destructor TTntADODataSetLX.Destroy;
begin
  inherited;
  FreeAndNil(StoredDefaults)
end;

function TTntADODataSetLX.GetParamCheck: Boolean;
begin
  Result := inherited ParamCheck;
end;

procedure TTntADODataSetLX.SetParamCheck(const Value: Boolean);
begin
  if inherited ParamCheck <> Value then begin
    inherited ParamCheck := Value;
    if Value then
    begin
      CheckInactive;
      Command.CommandText := CommandText;
    end;
  end;
end;

function TTntADODataSetLX.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  {$IFDEF COMPILER_10_UP}
  if FieldType = ftWideMemo then
    Result := TTntLXWideMemoStringField
  else
  {$ENDIF}
  begin
    Result := GetTntFieldClass(inherited GetFieldClass(FieldType));
    // ADO has TWideStringField, use that if you want Unicode
    if Result = TTntStringField then
      Result := TStringField{TNT-ALLOW TStringField};
  end;
end;

{$IFNDEF COMPILER_10_UP}
procedure TTntADODataSetLX.InternalInitFieldDefs;
var
  f: integer;
  FieldDef: TFieldDef;
begin
  inherited;
  for f := 0 to FieldDefs.Count - 1 do begin
    FieldDef := FieldDefs[f];
    if FieldDef.DataType = ftMemo then begin
      FieldDef.DataType := ftWideString;
      FieldDef.Size := MaxInt;
    end;
  end;
end;
{$ENDIF}

procedure TTntADODataSetLX.InternalLast;
const
  ERROR_CURRENT_RECORD_DELETED = HResult($800A0BCD);  //  Either BOF or EOF is True, or the current record has been deleted. Requested operation requires a current record
begin
  try
    inherited;
  except
    on E: Exception do begin
      if (E is EOleException) and (EOleException(E).ErrorCode = ERROR_CURRENT_RECORD_DELETED) then
      begin
        InternalRequery;
        inherited;
      end;
    end;
  end;
end;

procedure TTntADODataSetLX.UpdateIndexDefs;
begin
  UpdateIndexDefs_FixedForAdo27;
end;

procedure TTntADODataSetLX.DoBeforeOpen;
begin
  inherited;
  // figure db type
  if (Recordset <> nil) then begin
    if IDispatch(Recordset.Get_ActiveConnection) <> nil then begin
      FIsMSSqlServer := TntLXDBUtils.IsMSSqlServer(Recordset.Get_ActiveConnection.ConnectionString);
      FIsMSJet       := TntLXDBUtils.IsMSJet(Recordset.Get_ActiveConnection.ConnectionString);
      FIsOracle      := TntLXDBUtils.IsOracle(Recordset.Get_ActiveConnection.ConnectionString);
    end else begin
      FIsMSSqlServer := False;
      FIsMSJet       := False;
      FIsOracle      := False;
    end
  end else if (Connection <> nil) then begin
    FIsMSSqlServer := TntLXDBUtils.IsMSSqlServer(Connection.ConnectionString);
    FIsMSJet       := TntLXDBUtils.IsMSJet(Connection.ConnectionString);
    FIsOracle       := TntLXDBUtils.IsOracle(Connection.ConnectionString);
  end else begin
    FIsMSSqlServer := TntLXDBUtils.IsMSSqlServer(ConnectionString);
    FIsMSJet       := TntLXDBUtils.IsMSJet(ConnectionString);
    FIsOracle       := TntLXDBUtils.IsOracle(ConnectionString);
  end;
  FAssumeProviderCanOpenSchema := True;
end;
 
procedure TTntADODataSetLX.DoAfterOpen;
begin
  // get shema info
  if FCaptureFieldDescriptions or (LockType <> ltReadOnly) then
    RetrieveDefaultExpressions;
  inherited;
end;
 
procedure TTntADODataSetLX.OpenCursor(InfoQuery: Boolean);
begin
  try
    inherited;
  except
    InternalClose;
    // try to recover
    case GetTntConnectionStatus(Connection) of
      csGood, csBroken:
        raise;
      csTryRestoreInProgress:
        Abort;
      csRestoredToGood:
        if not Active then
          inherited;
      else
        raise ETntInternalError.Create('Unexpected connection status.');
    end;
  end;
end;

procedure TTntADODataSetLX.DoAfterClose;
begin
  inherited;
  StoredDefaults.Clear;
end;
 
procedure TTntADODataSetLX.DoAfterInsert;
begin
  ApplyDefaultExpressions;
  SetModified(False);
  inherited;
end;
 
function TTntADODataSetLX.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  if (Self.State <> dsInsert) then
    Result := inherited GetStateFieldValue(State, Field)
  else if (State = dsOldValue) then
    Result := GetFieldDefaultValue(Field)
  else
    Result := Field.Value
end;
 
{$IFNDEF COMPILER_10_UP}
function TTntADODataSetLX.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(Field, Buffer, True); { possibly the same as inherited, but we must make sure }
end;
 
function TTntADODataSetLX.GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean;
begin
  Result := inherited GetFieldData(Field, Buffer, NativeFormat);
  if (not Result) and (Buffer <> nil) and (Field.DataType = ftWideString) then
    WideString(Buffer^) := ''; { TWideStringField.GetAsWideString should do this for NULL values }
end;
 
function TTntADODataSetLX.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer); { possibly the same as inherited, but we must make sure }
end;
{$ENDIF}

procedure TTntADODataSetLX.SetFieldData(Field: TField; Buffer: Pointer);
begin
  SetFieldData(Field, Buffer, True) { possibly the same as inherited, but we must make sure }
end;

procedure TTntADODataSetLX.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
begin
  if SetFieldData_IsNeeded(Field, Buffer, NativeFormat) then
    inherited; { only call inherited if a change is made }
end;
 
//=============================================================================================
//============                       UTIL PROCS                            ====================
//=============================================================================================
procedure TTntADODataSetLX.UpdateIndexDefs_FixedForAdo27;
//  As of ADO 2.7, OpenSchema(adSchemaIndexes,...) no longer supports the
//    ORDINAL_POSITION column, but this can be determined by the sequence of
//      rows returned where the INDEX_NAME column is the same.
//        This new approach is backward compatible.
const
  cfIndex      = 3;
const
  SUnique     = 'UNIQUE';      { Do not localize + 5 }
  SIndexName  = 'INDEX_NAME';
  SColumnName = 'COLUMN_NAME';
  SPrimaryKey = 'PRIMARY_KEY';
  SAutoUpdate = 'AUTO_UPDATE';
var
  IndexInfo: _Recordset;
begin
  try
    FieldDefs.Update;
    IndexDefs.Clear;
    if (CommandType in [cmdTable, cmdTableDirect]) and (CommandText <> '') then
    begin
      SetConnectionFlag(cfIndex, True);
      try
        IndexInfo := TAccessADOCommand(Command).ActiveConnection.OpenSchema(adSchemaIndexes,
          VarArrayOf([Unassigned, Unassigned, Unassigned, Unassigned, CommandText]),
          EmptyParam);
        while not IndexInfo.EOF do
        begin
          if IndexDefs.IndexOf(VarToWideStr(IndexInfo.Fields[SIndexName].Value)) <> -1 then
            with IndexDefs.Find(VarToWideStr(IndexInfo.Fields[SIndexName].Value)) do
              Fields := WideFormat('%s;%s', [Fields, IndexInfo.Fields[SColumnName].Value])
          else
            with IndexDefs.AddIndexDef do
            begin
              Name := VarToWideStr(IndexInfo.Fields[SIndexName].Value);
              Fields := VarToWideStr(IndexInfo.Fields[SColumnName].Value);
              if IndexInfo.Fields[SPrimaryKey].Value = True then
                Options := Options + [ixPrimary];
              if IndexInfo.Fields[SUnique].Value = True then
                Options := Options + [ixUnique];
              if IndexInfo.Fields[SAutoUpdate].Value = False then
                Options := Options + [ixNonMaintained];
            end;
          IndexInfo.MoveNext;
        end;
      finally
        SetConnectionFlag(cfIndex, False);
      end;
    end;
  except
    { do nothing }
  end;
end;

procedure TTntADODataSetLX.GetFieldBaseInfo(Field: TField;
  out BaseTableName, BaseColumnName: WideString);
var
  BaseCatalogName: Variant;
  BaseSchemaName: Variant;
begin
  GetFieldBaseInfoEx(Field, BaseCatalogName, BaseSchemaName, BaseTableName, BaseColumnName);
  if BaseTableName = '' then begin
    BaseTableName := GetUniqueTableName(CommandText);
    Assert(BaseTableName <> '');
  end;
  if BaseColumnName = '' then
    BaseColumnName := Field.FieldName;
end;
 
procedure TTntADODataSetLX.GetFieldBaseInfoEx(Field: TField; out BaseCatalogName, BaseSchemaName: Variant;
  out BaseTableName, BaseColumnName: WideString);
var
  FieldProps: ADOInt.Properties;
begin
  Assert(Field.DataSet = Self, 'Internal Error: Field must be a member of this dataset.');
  try
    Assert(Field.FieldKind = fkData);
    FieldProps := Recordset.Fields.Get_Item(Field.FieldNo - 1).Properties;
    BaseTableName := VarToWideStr(FieldProps.Item['BaseTableName'].Value);
    BaseColumnName := VarToWideStr(FieldProps.Item['BaseColumnName'].Value);
  except
    FieldProps := nil;
    BaseTableName := '';
    BaseColumnName := '';
  end;
  if (IsMSJet)
  or (FieldProps = nil)
  or (BaseTableName = '')
  or (BaseColumnName = '') then begin
    BaseCatalogName := Null;
    BaseSchemaName := Null;
  end else begin
    BaseCatalogName := FieldProps.Item['BaseCatalogName'].Value;
    BaseSchemaName := FieldProps.Item['BaseSchemaName'].Value;
    if IsMSSqlServer and VarIsNull(BaseSchemaName) then
      BaseSchemaName := 'dbo';
    if IsOracle then
    begin
      if VarIsNull(BaseCatalogName) then
        BaseCatalogName := Unassigned;
      if VarIsNull(BaseSchemaName) then
        BaseSchemaName := Unassigned;
    end;
  end;
end;

function TTntADODataSetLX.GetIsJetBooleanFieldRequired(Field: TField): Boolean;
begin
  Result := False; // descandants can use DAO to check the field's Required property.
end;

procedure TTntADODataSetLX.RetrieveDefaultExpressions;

    function GetSchemaColumnsFor(SchemaCache: TTntADOSchemaCache; Field: TField): ADOInt.Recordset;
    const
      E_NOTIMPL = HRESULT($800A0CB3);
    var
      SchemaTbls_Index: Integer;
      BaseCatalogName: Variant;
      BaseSchemaName: Variant;
      BaseTableName: WideString;
      BaseColumnName: WideString;
    begin
      GetFieldBaseInfoEx(Field, BaseCatalogName, BaseSchemaName, BaseTableName, BaseColumnName);
      SchemaTbls_Index := SchemaCache.SchemaTbls.IndexOf(WideFormat('%s.%s.%s',
        [VarToWideStr(BaseCatalogName), VarToWideStr(BaseSchemaName), BaseTableName]));
      if SchemaTbls_Index <> -1 then
        Result := SchemaCache.SchemaList[SchemaTbls_Index] as ADOInt.Recordset
      else if BaseTableName = '' then
        Result := nil
      else begin
        if (FAssumeProviderCanOpenSchema = False) then
          Result := nil
        else begin
          TAccessADOCommand(Command).OpenConnection;
          try
            Result := TAccessADOCommand(Command).ActiveConnection.OpenSchema(adSchemaColumns,
              VarArrayOf([BaseCatalogName, BaseSchemaName, BaseTableName]), EmptyParam);
            SchemaCache.SchemaList.Add(Result);
            SchemaCache.SchemaTbls.Add(WideFormat('%s.%s.%s',
              [VarToWideStr(BaseCatalogName), VarToWideStr(BaseSchemaName), BaseTableName]));
          except
            on E: EOleException do
              if E.ErrorCode = E_NOTIMPL then begin
                FAssumeProviderCanOpenSchema := False;
                E.Message := 'Internal Error: ' + E.Message + CRLF + CRLF + 'Consider setting SupressOpenSchemaErrors := True';
                if (not SupressOpenSchemaErrors) then
                  raise;
              end else
                raise
            else
              raise;
          end;
        end;
      end;
      if (Result <> nil) then begin
        if (Result.RecordCount = 0) then
          Result := nil
        else begin
          Result.MoveFirst;
          Result.Find(WideFormat('COLUMN_NAME = ''%s''', [BaseColumnName]), 0,
            adSearchForward, adBookmarkFirst);
          if Result.BOF or Result.EOF then
            Result := nil;
        end;
      end;
    end;

    procedure UpdateFieldWithSchemaDefault(Field: TField; const Schema: ADOInt.Recordset);
    begin
      if (Field <> nil) then begin
        // Required
        if (Field.DataType = ftBoolean) and (IsMSJet) then begin
          // Jet/Boolean fields don't report Is_nullable correctly
          Field.Required := GetIsJetBooleanFieldRequired(Field);
        end else
          Field.Required := not Schema.Fields['IS_NULLABLE'].Value;
        // DefaultExpression
        if Schema.Fields['COLUMN_HASDEFAULT'].Value then begin
          Field.DefaultExpression := Trim(VarToWideStr(Schema.Fields['COLUMN_DEFAULT'].Value));
        end;
        // DisplayLabel
        if FCaptureFieldDescriptions then
          if Field.DisplayLabel = Field.FieldName then
            Field.DisplayLabel := VarToWideStr(Schema.Fields['DESCRIPTION'].Value);
      end;
    end;
 
    function StringWrappedBy(const s: WideString; Delim: WideChar): Boolean;
    begin
      Result := (Pos(Delim, s) = 1)
            and (TntWideLastChar(s) = Delim);
    end;
 
    procedure AddStoredDefaultInfo(Field: TField);
    var
      CanBeStored: Boolean;
      StoredString: WideString;
      DefExpression: WideString;
      P: PWideChar;
    begin
      DefExpression := Field.DefaultExpression;
      StoredString := '';
      CanBeStored := False;
      if (Length(DefExpression) >= 2)
      and (DefExpression[1] = '(')
      and (DefExpression[Length(DefExpression)] = ')') then
        DefExpression := Copy(DefExpression, 2, Length(DefExpression) - 2);
      if IsMSSqlServer
      and (WideSameText(DefExpression, 'NEWSEQUENTIALID()') or WideSameText(DefExpression, 'NEWSEQUENTIALID')) then
      begin
        // can't request this pro-actively
        DefExpression := '';
        Field.DefaultExpression := '';
        Field.Required := False;
      end;
      if StringWrappedBy(DefExpression, '"') then begin
        P := PWideChar(DefExpression);
        StoredString := WideExtractQuotedStr(P, '"');
        CanBeStored := True;
      end else if StringWrappedBy(DefExpression, '''') then begin
        P := PWideChar(DefExpression);
        StoredString := WideExtractQuotedStr(P, '"');
        CanBeStored := True;
      end else if (DefExpression = ExtractNumbers(DefExpression)) then begin
        StoredString := DefExpression;
        CanBeStored := True;
      end else if IsMSSqlServer then begin
        // MS SqlServer
        if WideSameText(DefExpression, 'suser_sname()')
        and (IDispatch(Recordset.Get_ActiveConnection) <> nil) then begin
          StoredString :=
            GetSqlResult(Recordset.Get_ActiveConnection, 'SELECT ' + DefExpression);
          CanBeStored := True;
        end
      end else if IsMSJet then begin
        // MS JET
        if WideSameText('Yes', DefExpression) then
          DefExpression := 'True'
        else if WideSameText('No', DefExpression) then
          DefExpression := 'False';

        if WideSameText('True', DefExpression)
        or WideSameText('False', DefExpression) then begin
          StoredString := DefExpression;
          CanBeStored := True;
        end;
      end;
      StoredDefaults.AddObject(StoredString, TObject(CanBeStored));
    end;

    function FirstFieldBaseTableName: WideString;
    begin
      if (FieldCount = 0)
      or (Fields[0].FieldKind <> fkData) then
        Result := ''
      else begin
        Result := VarToWideStr(
          Recordset.Fields.Get_Item(Fields[0].FieldNo - 1).Properties.Item['BaseTableName'].Value);
      end;
    end;

var
  f: integer;
  Schema: ADOInt.Recordset;
  SchemaColumnsCache: TTntADOSchemaCache;
  IntSchemaColumnsCache: TTntADOSchemaCache;
  UniqueTableName: WideString;
begin
  StoredDefaults.Clear;

  IntSchemaColumnsCache := nil;
  try
    // get schema cache
    if Connection <> nil then
      SchemaColumnsCache := GetSchemaCache(Connection)
    else begin
      IntSchemaColumnsCache := TTntADOSchemaCache.Create(nil);
      SchemaColumnsCache := IntSchemaColumnsCache;
    end;
    // see if all fields come from same table
    if CommandType in [cmdTable, cmdTableDirect] then
      UniqueTableName := CommandText
    else
      UniqueTableName := GetUniqueTableName(CommandText);
    // update fields based on schema info
    if (UniqueTableName <> '')
    and (WideSameText(FirstFieldBaseTableName, UniqueTableName))
    and (GetSchemaColumnsFor(SchemaColumnsCache, Fields[0]) <> nil) then
    begin
      // known table name, for each record in schema, update field
      Schema := GetSchemaColumnsFor(SchemaColumnsCache, Fields[0]);
      Assert(Schema <> nil, 'Internal Error: No schema found.');
      Schema.MoveFirst;
      While not Schema.EOF do begin
        UpdateFieldWithSchemaDefault(FindField(VarToWideStr(Schema.Fields['COLUMN_NAME'].Value)),
          Schema);
        Schema.MoveNext;
      end;
    end else begin
      // handle each field independently, for each field, find schema info
      for f := 0 to FieldCount - 1 do begin
        if Fields[f].FieldKind = fkData then begin
          // data field - only set defaults for data fields
          Schema := GetSchemaColumnsFor(SchemaColumnsCache, Fields[f]);
          if Schema <> nil then
            UpdateFieldWithSchemaDefault(Fields[f], Schema);
        end;
      end;
    end;
    // try to pre-calculate defaults is possible
    for f := 0 to FieldCount - 1 do begin
      AddStoredDefaultInfo(Fields[f]);
    end;
  finally
    IntSchemaColumnsCache.Free;
  end;
end;
 
function TTntADODataSetLX.GetFieldDefaultValue(Field: TField): Variant;
var
  StoredDefault: WideString;
  CanBeStored: Boolean;
begin
  if Field.DefaultExpression = '' then
    Result := Null
  else begin
    StoredDefault := StoredDefaults[Field.Index];
    CanBeStored   := Boolean(StoredDefaults.Objects[Field.Index]);
    if CanBeStored then
      Result := StoredDefault
    else begin
      if IsMSSqlServer
      and WideSameText(Field.DefaultExpression, 'getdate()') then
        // MS SQLSERVER - getdate()
        Result := Now
 
      else if IsMSJet
      and WideSameText(Field.DefaultExpression, 'now()') then
        // MS JET - now()
        Result := Now
 
      else if IsMSJet
      and WideSameText(Field.DefaultExpression, 'date()') then
        // MS JET - date()
        Result := Date
 
      else if (IDispatch(Recordset.Get_ActiveConnection) <> nil) then
        // request value from server
        Result :=
          GetSqlResult(Recordset.Get_ActiveConnection, 'SELECT ' + Field.DefaultExpression)
 
      else
        Result := Null;
    end;
  end;
end;
 
procedure TTntADODataSetLX.ApplyDefaultExpressions;
var
  f: integer;
  DefaultValue: Variant;
begin
  // set defaults
  for f := 0 to FieldCount - 1 do begin
    if Fields[f].DefaultExpression <> '' then begin
      try
        DefaultValue := GetFieldDefaultValue(Fields[f]);
        if not VarIsNull(DefaultValue) then
          Fields[f].Value := DefaultValue;
      except
        Fields[f].DefaultExpression := '';
      end;
    end;
  end;
end;
 
function TTntADODataSetLX.SetFieldData_IsNeeded(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): boolean;
 
  function CanConvertBufferToVar: Boolean;
  begin
    Result := Field.DataType in
      [ftDate, ftTime, ftDateTime, ftBCD, ftLargeInt];
  end;
 
  function BufferToVar: Variant;
  begin
    Assert(Buffer <> nil, 'Buffer not assigned.');
    case Field.DataType of
      ftDate, ftTime, ftDateTime:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(Result).VDate, False)
        else
          Result := TDateTime(Buffer^);
      ftBCD:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(Result).VCurrency, False)
        else
          Result := Currency(Buffer^);
      ftLargeInt:
        begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).Lo64 := Int64(Buffer^);
        end;
      else
        raise ETntInternalError.Create('Internal Error: Unexpected data type.');
    end;
  end;
 
begin
  if Buffer = nil then
    Result := not Field.IsNull { must set it to Null }
  else if Field.IsNull then
    Result := True             { must set to Non-Null }
  else begin
    // determine if new value is the same as the old one
    case Field.DataType of
      ftString, ftFixedChar, ftGuid:
        Result := (PAnsiChar(Buffer)   <> Field.AsString{TNT-ALLOW AsString});
      ftWideString:
        {$IFDEF COMPILER_10_UP}
        if Field is TTntLXWideStringField then
          Result := (OleVariant(Buffer^) <> GetAsWideString(Field))
        else
          Result := (WideString(PWideChar(Buffer)) <> GetAsWideString(Field));
        {$ELSE}
        Result := (WideString(Buffer^) <> GetAsWideString(Field));
        {$ENDIF}
      ftAutoInc, ftInteger:
        Result := (LongInt(Buffer^)    <> Field.AsInteger);
      ftSmallInt:
        Result := (SmallInt(Buffer^)   <> Field.AsInteger);
      ftWord:
        Result := (Word(Buffer^)       <> Field.AsInteger);
      ftBoolean:
        Result := (WordBool(Buffer^)   <> Field.AsBoolean);
      ftFloat, ftCurrency:
        Result := (Double(Buffer^)     <> Field.AsFloat);
      else
        Result := (not CanConvertBufferToVar) or (not VariantsEqual(Field.Value, BufferToVar))
    end;
  end;
end;
 
//-----------------------------------------------
 
function GetFilterStr(Field: TField; Value: Variant; Partial: Boolean = False): WideString;
var
  Operator,
  FieldName,
  QuoteCh: WideString;
begin
  QuoteCh := '';
  Operator := '=';
  FieldName := Field.FieldName;
  if Pos(' ', FieldName) > 0 then
    FieldName := WideFormat('[%s]', [FieldName]);
  if VarIsNull(Value) or VarIsClear(Value) then
    Value := 'Null'
  else
    case Field.DataType of
      ftDate, ftTime, ftDateTime:
        QuoteCh := '#';
      ftString, ftFixedChar, ftWideString
      {$IFDEF COMPILER_10_UP}
      , ftFixedWideChar, ftWideMemo
      {$ENDIF}
      :
        begin
          if Partial and (Value <> '') then
          begin
            Value := Value + '*';
            Operator := ' like ';     { Do not localize }
          end;
          if Pos('''', Value) > 0 then
            QuoteCh := '#' else
            QuoteCh := '''';
        end;
    end;
  Result := WideFormat('(%s%s%s%s%2:s)', [FieldName, Operator, QuoteCh, VarToWideStr(Value)]);
end;

procedure TTntADODataSetLX.DestroyLookupCursor;
begin
  FLookupCursor := nil;
  inherited;
end;
 
function TTntADODataSetLX.LocateRecord(const KeyFields: AnsiString;
  const KeyValues: OleVariant; Options: TLocateOptions;
  SyncCursor: Boolean): Boolean;
var
  Fields: TList;
  Buffer: PAnsiChar;
  I, FieldCount: Integer;
  Partial: Boolean;
  SortList, FieldExpr, LocateFilter: WideString;
  SizeOfTRecInfo: Integer;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  CursorPosChanged;
  Buffer := TempBuffer;
  Partial := loPartialKey in Options;
  Fields := TList.Create;
  DoBeforeScroll;
  try
    try
      GetFieldList(Fields, KeyFields);
      if not Assigned(FLookupCursor) then
        FLookupCursor := Recordset.Clone(adLockReadOnly);
      if CursorLocation = clUseClient then
      begin
        for I := 0 to Fields.Count - 1 do
          with TField(Fields[I]) do
            if Pos(' ', FieldName) > 0 then
            SortList := WideFormat('%s[%s],', [SortList, FieldName]) else
            SortList := WideFormat('%s%s,', [SortList, FieldName]);
        SetLength(SortList, Length(SortList)-1);
        if FLookupCursor.Sort <> SortList then
          FLookupCursor.Sort := SortList;
      end;
      FLookupCursor.Filter := '';
      InitRecord(Buffer);
      FieldCount := Fields.Count;
      if FieldCount = 1 then begin
        if (VarIsNull(KeyValues))
        and (TField(Fields[0]).Required)
        and (CursorLocation = clUseClient) then begin
          Result := False;
          exit;
        end else
          FLookupCursor.Find(GetFilterStr(FieldByName(KeyFields), KeyValues, Partial), 0,
            adSearchForward, EmptyParam)
      end else
      begin
        for I := 0 to FieldCount - 1 do
        begin
          FieldExpr := GetFilterStr(Fields[I], KeyValues[I], (Partial and (I = FieldCount-1)));
          if LocateFilter <> '' then
             LocateFilter := LocateFilter + ' AND ' + FieldExpr else    { Do not localize }
             LocateFilter := FieldExpr;
        end;
        FLookupCursor.Filter := LocateFilter;
      end;
    finally
      Fields.Free;
    end;
    Result := not FLookupCursor.EOF;
    if Result then
      if SyncCursor then
      begin
        Recordset.Bookmark := FLookupCursor.Bookmark;
        if Recordset.EOF or Recordset.BOF then
        begin
          Result := False;
          CursorPosChanged;
        end
      end
      else
        { For lookups, read all field values into the temp buffer }
        for I := 0 to Self.Fields.Count - 1 do
         with Self.Fields[I] do
          if FieldKind = fkData then begin
            SizeOfTRecInfo := GetRecordSize - (Self.Fields.Count * SizeOf(OleVariant));
            {$IFOPT R+}
              {$RANGECHECKS OFF}
              PVariantList(Buffer+SizeOfTRecInfo)[Index] := FLookupCursor.Fields[FieldNo-1].Value;
              {$RANGECHECKS ON}
            {$ELSE}
              PVariantList(Buffer+SizeOfTRecInfo)[Index] := FLookupCursor.Fields[FieldNo-1].Value;
            {$ENDIF}
          end;
  except
    Result := False;
  end;
end;
 
function TTntADODataSetLX.Lookup(const KeyFields: AnsiString; const KeyValues: Variant;
  const ResultFields: AnsiString): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;
 
function TTntADODataSetLX.Locate(const KeyFields: AnsiString; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
 
  function IndexFieldsMatch(const KeyFields: AnsiString): Boolean;
  var
    i: integer;
    FieldList: TList;
  begin
    Result := False;
    // seek might be possible
    if WideSameText(IndexName, KeyFields) then
      // pretty good guess!
      Result := True
    else if IndexFieldCount = 1 then
      Result := WideSameText(IndexFields[0].FieldName, KeyFields)
    else if IndexFieldCount > 1 then begin
      // compare field list
      FieldList := TList.create;
      try
        GetFieldList(FieldList, KeyFields);
        if FieldList.Count = IndexFieldCount then begin
          // same number of fields
          Result := True; {prove it wrong}
          for i := 0 to FieldList.Count - 1 do begin
            if FieldList[i] <> IndexFields[i] then begin
              Result := False;
              break; { found one that didn't match }
            end;
          end;
        end;
      finally
        FieldList.Free;
      end;
    end;
  end;
 
begin
  if (not (loPartialKey in Options))
  and (not Filtered)
  and Supports([coSeek])
  and IndexFieldsMatch(KeyFields) then begin
    { seek is much faster }
    Result := Seek(KeyValues, soFirstEQ)
  end else begin
    { normal locate }
    DoBeforeScroll;
    Result := LocateRecord(KeyFields, KeyValues, Options, True);
    if Result then
    begin
      Resync([rmExact, rmCenter]);
      DoAfterScroll;
    end;
  end;
end;
 
procedure TTntADODataSetLX.Requery(Options: TExecuteOptions);
begin
  try
    inherited;
  except
    // try to recover
    case GetTntConnectionStatus(Connection) of
      csGood, csBroken:
        raise;
      csTryRestoreInProgress:
        Abort;
      csRestoredToGood:
        if Active then
          inherited
        else
          Open;
      else
        raise ETntInternalError.Create('Unexpected connection status.');
    end;
  end;
end;

{ TODO: Test if Delphi 10 still has this bug in TDataSet.DataEvent. }

procedure TTntADODataSetLX.DataEvent(Event: TDataEvent; Info: Integer);
begin
  {$IFDEF COMPILER_7_UP} // Bug fix for Delphi 7 TDataSet.DataEvent with DisableControls
  if (Event = deUpdateState) and ControlsDisabled then
    inherited DataEvent(deDisabledStateChange, Info)
  else
  {$ENDIF}
    inherited;
  if (Event = deFieldListChange) and Active and (StoredDefaults.Count > 0) then
    RetrieveDefaultExpressions;
end;

{ TTntADOTableLX }

constructor TTntADOTableLX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CommandType := cmdTable;
  TAccessADOCommand(Command).CommandTextAlias := 'TableName'; { Do not localize }
end;

function TTntADOTableLX.GetReadOnly: Boolean;
begin
  Result := LockType = ltReadOnly;
end;

procedure TTntADOTableLX.SetReadOnly(const Value: Boolean);
begin
  if Value then
    LockType := ltReadOnly
  else
    LockType := ltOptimistic;
end;

function TTntADOTableLX.GetTableDirect: Boolean;
begin
  Result := CommandType = cmdTableDirect;
end;
 
procedure TTntADOTableLX.SetTableDirect(const Value: Boolean);
begin
  if Value then
    CommandType := cmdTableDirect
  else
    CommandType := cmdTable;
end;

procedure TTntADOTableLX.GetIndexNames(List: TTntStrings);
begin
  IndexDefs.Update;
  IndexDefs.GetItemNames(List.AnsiStrings);
end;
 
{ TTntADOQueryLX }
 
constructor TTntADOQueryLX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TTntStringList.Create;
  TTntStringList(FSQL).OnChange := QueryChanged;
  TAccessADOCommand(Command).CommandTextAlias := 'SQL'; { Do not localize }
end;
 
destructor TTntADOQueryLX.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSQL);
end;
 
procedure InitializeMasterFields(Dataset: TTntADODataSetLX);
var
  I: Integer;
  MasterFieldList: WideString;
begin
  with DataSet do
    { Assign MasterFields from parameters as needed by the MasterDataLink }
    if (Parameters.Count > 0) and Assigned(MasterDataLink.DataSource) and
      Assigned(MasterDataLink.DataSource.DataSet) then
    begin
      for I := 0 to Parameters.Count - 1 do
        if (Parameters[I].Direction in [pdInput, pdInputOutput]) and
          (MasterDataLink.DataSource.DataSet.FindField(Parameters[I].Name) <> nil) then
          MasterFieldList := MasterFieldList + Parameters[I].Name + ';';
      MasterFields := Copy(MasterFieldList, 1, Length(MasterFieldList)-1);
      SetParamsFromCursor;
    end;
end;

function TTntADOQueryLX.ExecSQL: Integer;
begin
  InitializeMasterFields(Self);
  Command.Execute(FRowsAffected, EmptyParam);
  Result := FRowsAffected;
end;

procedure TTntADOQueryLX.QueryChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Close;
  CommandText := FSQL.Text;
end;

function TTntADOQueryLX.GetSQL: TTntStrings;
begin
  Result := FSQL;
end;

procedure TTntADOQueryLX.SetSQL(const Value: TTntStrings);
begin
  FSQL.Assign(Value);
end;

end.
