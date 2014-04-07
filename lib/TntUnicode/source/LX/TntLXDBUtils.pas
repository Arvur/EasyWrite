
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXDBUtils;

{$INCLUDE TntCompilers.inc}

interface

uses
  Types, SysUtils, Classes, DB, ADODB, ADOInt, TntLXUtils, TntClasses, TntLXClasses, TntDB;

const
  SQL_FALSE = '0';
  SQL_TRUE = '-1';

type
  TFieldMapping = record
    FromField: TField;
    ToField: TField;
  end;
  TFieldMappingArray = array of TFieldMapping;

  ITntDatasetMetaDataProvider = interface
    ['{9308514C-1544-42A3-AFCB-F314C60D74FF}']
    function GetFieldDescription(const FieldName: WideString): WideString;
    function HelpNotes: WideString;
  end;

  ITntDatasetStatusProvider = interface
    ['{B5B40B46-9CAF-464A-A057-396685789FBE}']
    function IsValid(const TargetField: TField; SourceText: WideString): Boolean;
    function SkipRecordField: TField;
    function IsRecordSkippedBasedOn(SourceText: WideString): Boolean;
  end;

  ITntImportDataSet = interface
    ['{DD58749A-45A3-49B1-B8B1-9E7463B474A4}']
    procedure BeforeImport;
    procedure AfterImport;
    procedure PreviewImportedRecord(SourceDataSet: TDataSet; FieldMapping: TFieldMappingArray);
  end;

  TTntADOConnectionStatus = (csGood, csBroken, csRestoredToGood, csTryRestoreInProgress);
  ITntReconnectManager = interface
    ['{963CACAD-0310-432C-AAF4-ADC986C00F83}']
    procedure TryRestoringConnection;
  end;

  ESkipRecord = class(Exception);

procedure PrepareFieldMapping(SourceDataSet, TargetDataSet: TDataSet; const FieldMappingText: WideString;
  out FieldMapping: TFieldMappingArray);
procedure SetAllWideStringFields_OnGetText(DataSet: TDataSet; GetText: TFieldGetWideTextEvent);
procedure PrepareDataSetWideStringFields(DataSet: TDataSet; NumRecordsToCheck: Integer = 25;
  GetText: TFieldGetWideTextEvent = nil; IncludeFieldDisplayNames: Boolean = True; MinLength: Integer = 5);
procedure PrepDataSetNumericFields(DataSet: TDataSet);
function FieldByDisplayName(DataSet: TDataSet; Name: WideString; VisibleOnly: Boolean = True): TField;
procedure SafePost(DataSet: TDataSet);

procedure VerifyMatchingFieldNames(Table1, Table2: TDataSet);
procedure VerifyMatchingFieldTypes(Table1, Table2: TDataSet);
procedure VerifyMatchingFields(Table1, Table2: TDataSet);
procedure CopyField(FromField, ToField: TField; ErrorOnTruncate: Boolean = False);
procedure CopyFields(FromTable, ToTable: TDataSet); 

procedure ForceLocate(DataSet: TDataSet; const KeyFields: AnsiString; const KeyValues: Variant;
  Options: TLocateOptions);
procedure ForceNoLocate(DataSet: TDataSet; const KeyFields: AnsiString; const KeyValues: Variant;
  Options: TLocateOptions);
function SQLServerNTLogin: WideString;
function SqlServerConnectString(const ServerName, DatabaseName, Login, Password: WideString; ConnectionPooling: Boolean = True): WideString;
function MakeMDBConnectString(MDBFileName, Password: WideString; Exclusive: Boolean = False): WideString;
function MakeUDLConnectString(UdlFileName: WideString): WideString;
function IsMsJet(Connection: _Connection): Boolean; overload;
function IsMsJet(const ConnectionString: WideString): Boolean; overload;
function IsMSSqlServer(Connection: _Connection): Boolean; overload;
function IsMSSqlServer(const ConnectionString: WideString): Boolean; overload;
function IsOracle(Connection: _Connection): Boolean; overload;
function IsOracle(const ConnectionString: WideString): Boolean; overload;

//  US Date Format: 2003-06-11 11:54:43.800

function DateToUSDateStr(TheDate: TTntDate): WideString;
function TimeToUSTimeStr(TheTime: TDateTime): WideString;
function DateTimeToUSDateTimeStr(DateTime: TDateTime): WideString;
function USDateStrToDate(const DateStr: WideString): TTntDate;
function USTimeStrToTime(const TimeStr: WideString): TDateTime;
function USDateTimeStrToDateTime(DateTimeStr: WideString): TDateTime;

//  SQL Date Format: 2003-06-11 11:54:43.800

function DateToSQLDateStr(TheDate: TTntDate): WideString;
function TimeToSQLTimeStr(TheTime: TDateTime; IncludeMsec: Boolean): WideString;
function DateTimeToSQLDateTimeStr(DateTime: TDateTime; IncludeMsec: Boolean): WideString;

function DateToSQL(Connection: _Connection; Date: TTntDate): WideString;
function TimeToSQL(Connection: _Connection; Time: TDateTime): WideString;
function DateTimeToSQL(Connection: _Connection; DateTime: TDateTime): WideString;
function BoolToSQL(const Value: Boolean): WideString;
function TreatFieldAsText(field: TField): Boolean;
function GetSQLName(Name: WideString): WideString;
function FixSQLText(const Value: WideString): WideString;
function FixSQLLikeText(const Text: WideString): WideString;
function QuotedSQLText(const Value: WideString): WideString;
function QuotedSQLLikeText(const Value: WideString): WideString;

function GetTntConnectionStatus(Connection: TADOConnection): TTntADOConnectionStatus;
function GetSqlResult(Connection: OleVariant;        const SQL: WideString; var RecCount: Integer): Variant; overload;
function GetSqlResult(Connection: TADOConnection;    const SQL: WideString; var RecCount: Integer): Variant; overload;
function GetSqlResult(Connection: OleVariant;        const SQL: WideString): Variant; overload;
function GetSqlResult(Connection: TADOConnection;    const SQL: WideString): Variant; overload;
function GetSqlResultDef(Connection: OleVariant;     const SQL: WideString; Default: Variant): Variant; overload;
function GetSqlResultDef(Connection: TADOConnection; const SQL: WideString; Default: Variant): Variant; overload;

procedure GetSqlIntegerArrayBasedOnFirstField(Connection: OleVariant; const SQL: WideString; out IntArray: TIntegerDynArray; CommandTimeout: Integer = -1);
procedure AddSqlIntegersBasedOnFirstField(Connection: OleVariant; const SQL: WideString; IntList: TTntIntegerList; AllowDuplicates: Boolean = True);
function GetVariantArrayFromSQL(Connection: OleVariant; const SQL: WideString;
  MinFieldCount: Integer = 1): OleVariant;

procedure AddSqlStringListBasedOnFirstField(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);
procedure GetSqlStringListBasedOnFirstField(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);
procedure GetSqlStringListBasedOnFirstField_VariantMethod(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);

procedure InitValueList(const ConnectionObject: Connection;
  const TableName, FieldName: WideString; sList: TTntStrings; TopUsedCount: Cardinal = Cardinal(MaxInt));

function StripBrackets(const Name: WideString): WideString;
procedure ParseOrderBySQL(const SQL: WideString; Items: TTntStrings); { object = 1 for DESC }
function CanonizeOrderBySQLFromItems(const Items: TTntStrings): WideString;
function CanonizeOrderBySQL(const SQL: WideString): WideString;
function GetUniqueTableName(SQL: AnsiString): WideString;

implementation

uses
  Math, Variants,
  {$IFDEF COMPILER_9_UP} WideStrUtils {$ELSE} TntWideStrUtils {$ENDIF},
  TntSysUtils, Contnrs, DBCommon;

procedure PrepareFieldMapping(SourceDataSet, TargetDataSet: TDataSet; const FieldMappingText: WideString;
  out FieldMapping: TFieldMappingArray);
var
  sList: TTntStringList;
  i: integer;
begin
  sList := TTntStringList.Create;
  try
    sList.Text := Trim(FieldMappingText);
    // clean invalid lines
    for i := sList.Count - 1 downto 0 do
    begin
      if (sList.Names[i] = '')
      or (sList.ValueFromIndex[i] = '.')
      or (sList.ValueFromIndex[i] = '') then
        sList.Delete(i);
    end;
    // build FieldMapping
    SetLength(FieldMapping, sList.Count);
    for i := 0 to sList.Count - 1 do begin
      FieldMapping[i].FromField := SourceDataSet.FieldByName(sList.Names[i]);
      FieldMapping[i].ToField := TargetDataSet.FieldByName(sList.ValueFromIndex[i]);
    end;
  finally
    FreeAndNil(sList);
  end;
end;

procedure SetAllWideStringFields_OnGetText(DataSet: TDataSet; GetText: TFieldGetWideTextEvent);
var
  i: integer;
begin
  for i := 0 to DataSet.FieldCount - 1 do
    if (DataSet.Fields[i] is TTntWideStringField) then
      TTntWideStringField(DataSet.Fields[i]).OnGetText := GetText;
end;

procedure PrepareDataSetWideStringFields(DataSet: TDataSet; NumRecordsToCheck: Integer = 25;
  GetText: TFieldGetWideTextEvent = nil; IncludeFieldDisplayNames: Boolean = True; MinLength: Integer = 5);
var
  MaxLen: array of integer;
  i: integer;
  rec: integer;
begin
  DataSet.DisableControls;
  try
    // prepare max len array
    SetLength(MaxLen, DataSet.FieldCount);
    for i := 0 to High(MaxLen) do
      MaxLen[i] := MinLength; {a really small value}

    if IncludeFieldDisplayNames then begin
      for i := 0 to DataSet.FieldCount - 1 do begin
        // setup WideString gettext event handlers
        if Assigned(GetText)
        and (DataSet.Fields[i] is TTntWideStringField) then
          TTntWideStringField(DataSet.Fields[i]).OnGetText := GetText;
        // init max length to field name
        MaxLen[i] := max(MaxLen[i], Length(DataSet.Fields[i].DisplayName));
      end;
    end;

    DataSet.First;
    for rec := 1 to NumRecordsToCheck do begin
      for i := 0 to DataSet.FieldCount - 1 do begin
        MaxLen[i] := max(MaxLen[i], Length(Trim(GetWideDisplayText(DataSet.Fields[i]))));
      end;
      DataSet.Next;
    end;

    // for each field...
    for i:= 0 to DataSet.FieldCount - 1 do begin
      // set best display width
      DataSet.fields[i].DisplayWidth := MaxLen[i];
    end;

    // move back to first
    DataSet.first;
  finally
    DataSet.EnableControls;
  end;
end;

procedure PrepDataSetNumericFields(DataSet: TDataSet);
var
  i: integer;
  d: integer;
begin
  // for each field...
  for i:= 0 to DataSet.fieldcount - 1 do begin
    // setup floating point fields to not display too many digits after decimal
    if (DataSet.Fields[i] is TBcdField)
    or (DataSet.Fields[i] is TFloatField) then
    with DataSet.Fields[i] as TNumericField do begin
      DisplayFormat := '#.'; { do not localize }
      for d := 1 to CurrencyDecimals do
        DisplayFormat := DisplayFormat + '0';
    end;
  end;
end;

function FieldByDisplayName(DataSet: TDataSet; Name: WideString; VisibleOnly: Boolean = True): TField;
var
  i: Integer;
begin
  // FieldName gets priority over DisplayName
  Result := DataSet.FindField(Name);
  if (Result <> nil)
  and (VisibleOnly and (not Result.Visible)) then begin
    Result := nil;
  end;
  if Result = nil then
  begin
    for i := 0 to DataSet.FieldCount - 1 do
    begin
      if (DataSet.Fields[i].Visible or (not VisibleOnly))
      and WideSameText(Name, DataSet.Fields[i].DisplayName) then
      begin
        Result := DataSet.Fields[i];
        break;
      end;
    end;
  end;
end;

procedure SafePost(DataSet: TDataSet);
begin
  try
    DataSet.Post;
  except
    DataSet.Cancel;
    raise;
  end;
end;

function DataFieldCount(Table: TDataSet): Integer;
var
  Field: TField;
  {$IFNDEF COMPILER_9_UP}
  i: integer;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF COMPILER_9_UP}
  for Field in Table.Fields do begin
  {$ELSE}
  for i := 0 to Table.Fields.Count - 1 do begin
    Field := Table.Fields[i];
  {$ENDIF}
    if Field.FieldKind = fkData then
      Inc(Result);
  end;
end;
 
procedure VerifyMatchingFieldNames(Table1, Table2: TDataSet);
var
  f: integer;
begin
  if DataFieldCount(Table1) <> DataFieldCount(TAble2) then
    raise ETntInternalError.Create('Data field count for ' + Table1.Name + ' differs.');
  for f := 0 to Table1.FieldCount - 1 do begin
    if Table1.Fields[f].FieldKind = fkData then begin
      // try to re-order table2 fields to match
      if (not WideSameText(Table1.Fields[f].FieldName, Table2.Fields[f].FieldName)) then begin
        if Table2.FindField(Table1.Fields[f].FieldName) = nil then
          raise ETntInternalError.CreateFmt('Couldn''t find field (%s.%s) in remote table.',
            [Table1.Name, Table1.Fields[f].FieldName]);
        Table2.FieldByName(Table1.Fields[f].FieldName).Index := f;
      end;
    end;
  end;
  for f := 0 to Table1.FieldCount - 1 do begin
    // verify that field names match
    if Table1.Fields[f].FieldKind = fkData then begin
      if not WideSameText(Table1.Fields[f].FieldName, Table2.Fields[f].FieldName) then
        raise ETntInternalError.CreateFmt('Field name differs.' + CRLF + '[%s.%s] <> [%s.%s]',
          [Table1.Name, Table1.Fields[f].FieldName, Table2.Name, Table2.Fields[f].FieldName]);
    end;
  end;
end;
 
procedure VerifyMatchingFieldTypes(Table1, Table2: TDataSet);
var
  f: integer;
begin
  if Table1.FieldCount <> Table2.FieldCount then
    raise ETntInternalError.Create('Field count for ' + Table1.Name + ' differs.');
  for f := 0 to Table1.FieldCount - 1 do begin
    // verify that field types match
    if Table1.Fields[f].FieldKind = fkData then begin
      if (Table1.Fields[f].DataType <> Table2.Fields[f].DataType) then
        raise ETntInternalError.CreateFmt('Field type differs for %s.%s.' + CRLF + '%s <> %s',
          [Table1.Name, Table1.Fields[f].FieldName, Table1.Fields[f].ClassName, Table2.Fields[f].ClassName]);
    end;
  end;
end;
 
procedure VerifyMatchingFields(Table1, Table2: TDataSet);
begin
  VerifyMatchingFieldNames(Table1, Table2);
  VerifyMatchingFieldTypes(Table1, Table2);
end;
 
procedure CopyField(FromField, ToField: TField; ErrorOnTruncate: Boolean = False);
begin
  if ToField.Required and FromField.IsNull and (FromField is TStringField{TNT-ALLOW TStringField}) then
    ToField.AsString{TNT-ALLOW AsString} := '' // try to insert a blank string instead of generating error!
  else if (not VariantsEqual(ToField.Value, FromField.Value)) then begin
    ToField.Assign(FromField);
    if ErrorOnTruncate and (ToField is TStringField{TNT-ALLOW TStringField}) then begin
      if Length(GetAsWideString(FromField)) > ToField.Size then
        raise Exception.CreateFmt('Truncation error.  The value "%s" was longer than %d characters.',
          [GetAsWideString(FromField), ToField.Size]);
    end;
  end;
end;

procedure CopyFields(FromTable, ToTable: TDataSet);
var
  f: integer;
  FromField, ToField: TField;
begin
  for f := 0 to FromTable.FieldCount - 1 do begin
    FromField := FromTable.Fields[f];
    if (FromField.FieldKind = fkData) then begin
      ToField := ToTable.Fields[f];
      CopyField(FromField, ToField);
    end;
  end;
end;

procedure ForceLocate(DataSet: TDataSet; const KeyFields: AnsiString; const KeyValues: Variant;
  Options: TLocateOptions);
begin
  if not DataSet.Locate(KeyFields, KeyValues, Options) then
    raise ETntInternalError.CreateFmt('Internal Error: %s (%s) does not exist.', [KeyFields, VarArrayToStr(KeyValues)]);
end;

procedure ForceNoLocate(DataSet: TDataSet; const KeyFields: AnsiString; const KeyValues: Variant;
  Options: TLocateOptions);
begin
  if DataSet.Locate(KeyFields, KeyValues, Options) then
    raise ETntInternalError.CreateFmt('Internal Error: %s (%s) already exists.', [KeyFields, VarArrayToStr(KeyValues)]);
end;

function SQLServerNTLogin: WideString;
begin
  Result := WideGetComputerName + '\' + WideGetUserName;
end;

function SqlServerConnectString(const ServerName, DatabaseName, Login, Password: WideString; ConnectionPooling: Boolean = True): WideString;
const
  SQL_TARGET_DB =
    'Provider=SQLOLEDB;Data Source=%s;Initial Catalog=%s;';
  SQL_TARGET_NODB =
    'Provider=SQLOLEDB;Data Source=%s;';
  SQL_LOGIN =
    'User ID=%s;Password=%s;';
  WINDOWS_LOGIN =
    'Integrated Security=SSPI;';
  NO_POOLING_EXTENSION =
    'OLE DB Services=-2;';
begin
  // basic
  if (ServerName <> '') and (DatabaseName <> '') then
    Result := WideFormat(SQL_TARGET_DB, [ServerName, DatabaseName])
  else if (ServerName <> '') then
    Result := WideFormat(SQL_TARGET_NODB, [ServerName])
  else
    Result := '';
  if (Result <> '') then begin
    // add credentials
    if (not WideSameText(SQLServerNTLogin, Login)) then
      Result := Result + WideFormat(SQL_LOGIN, [Login, Password])
    else begin
      Result := Result + WINDOWS_LOGIN;
      Assert(Password = '');
    end;
    // add pooling
    if (not ConnectionPooling) then
      Result := Result + NO_POOLING_EXTENSION;
  end;
end;

function MakeMDBConnectString(MDBFileName, Password: WideString; Exclusive: Boolean = False): WideString;
const
  JET_4_PROVIDER = 'Microsoft.Jet.OLEDB.4.0';
  MDB_CONNECT_STR = 'Provider=%s;Data Source=%s;Jet OLEDB:Database Password=%s';
  MDB_EXCLUSIVE_CLAUSE = ';Mode=Share Exclusive';
begin
  if MDBFileName = '' then
    Result := ''
  else begin
    Result := WideFormat(MDB_CONNECT_STR, [JET_4_PROVIDER, MDBFileName, Password]);
    if Exclusive then
      Result := Result + MDB_EXCLUSIVE_CLAUSE;
  end
end;

function MakeUDLConnectString(UdlFileName: WideString): WideString;
begin
  if UdlFileName <> '' then
    Result := 'FILE NAME=' + UdlFileName
  else
    Result := '';
end;

function IsMsJet(const ConnectionString: WideString): Boolean;
begin
  Result := WideTextPos('PROVIDER=MICROSOFT.JET.OLEDB', ConnectionString) > 0;
end;

function IsMsJet(Connection: _Connection): Boolean;
begin
  Result := IsMsJet(Connection.ConnectionString);
end;

function IsMSSqlServer(const ConnectionString: WideString): Boolean;
begin
  Result := WideTextPos('PROVIDER=SQLOLEDB', ConnectionString) > 0;
end;

function IsMSSqlServer(Connection: _Connection): Boolean;
begin
  Result := IsMSSqlServer(Connection.ConnectionString);
end;

function IsOracle(const ConnectionString: WideString): Boolean;
begin
  Result := WideTextPos('PROVIDER=MSDAORA', ConnectionString) > 0;
end;

function IsOracle(Connection: _Connection): Boolean;
begin
  Result := IsOracle(Connection.ConnectionString);
end;

function DateToUSDateStr(TheDate: TTntDate): WideString;
var
  M, D, Y: Word;
begin
  DecodeDate(TheDate, Y, M, D);
  Result := WideFormat('%d/%d/%d', [M, D, Y]); { do not localize }
end;

function TimeToUSTimeStr(TheTime: TDateTime): WideString;
var
  H, M, S, MS: Word;
  AmPmStr: WideString;
begin
  AmPmStr := 'AM'; { do not localize }
  DecodeTime(TheTime, H, M, S, MS);
  if H > 12 then begin
    Dec(H, 12);
    AmPmStr := 'PM'; { do not localize }
  end;
  Result := WideFormat('%d:%.2d:%.2d %s', [H, M, S, AmPmStr]); { do not localize }
end;

function DateTimeToUSDateTimeStr(DateTime: TDateTime): WideString;
begin
  Result := DateToUSDateStr(GetTntDate(DateTime)) + ' '
          + TimeToUSTimeStr(DateTime);
end;

function EnsureProperYear(Y: Integer): Integer;
var
  Century: Integer;
begin
  if Y > 100 then
    Result := Y
  else begin
    Century := (CurrentYear div 100) * 100;
    Result := Century + Y;
    if Result >= CurrentYear + 25 then
      Result := Result - 100
    else if Result < CurrentYear - 75 then
      Result := Result + 100;
  end;
end;

function USDateStrToDate(const DateStr: WideString): TTntDate;
var
  M, D, Y: Integer;
  WorkStr: WideString;
begin
  try
    WorkStr := Trim(DateStr);
    M := StrToInt(GrabTil(WorkStr, '/', 'month'));
    D := StrToInt(GrabTil(WorkStr, '/', 'day'));
    Y := StrToInt(WorkStr);
    Y := EnsureProperYear(Y);
    Result := SafeEncodeDate(Y, M, D);
  except
    raise ETntInternalError.CreateFmt('Internal Error: Unexpected date: %s' + CRLF + 'Expected format mm/dd/yyyy', [DateStr]);
  end;
end;

function USTimeStrToTime(const TimeStr: WideString): TDateTime;
var
  H, M, S: Word;
  AmPmStr: WideString;
  WorkStr: WideString;
begin
  WorkStr := Trim(TimeStr);
  H := StrToInt(GrabTil(WorkStr, ':', 'hour'));
  M := StrToInt(GrabTil(WorkStr, ':', 'minute'));
  if Pos(' ', WorkStr) = 0 then begin
    S := StrToInt(Trim(WorkStr));
    WorkStr := ''
  end else
    S := StrToInt(GrabTil(WorkStr, ' ', 'second'));
  AmPmStr := Trim(WorkStr);
  if WideSameText(AmPmStr, 'PM') then
    Inc(H, 12);
  Result := EncodeTime(H, M, S, 0);
end;

function USDateTimeStrToDateTime(DateTimeStr: WideString): TDateTime;
var
  DateStr, TimeStr: WideString;
  Pos_Space: Integer;
begin
  Assert(DateTimeStr <> '');
  DateTimeStr := Trim(DateTimeStr);
  Pos_Space := Pos(' ', DateTimeStr);
  DateStr := Copy(DateTimeStr, 1, Pos_Space - 1);
  if Pos('/', DateStr) = 0 then begin
    // no date
    TimeStr := DateTimeStr;
    Result := USTimeStrToTime(TimeStr);
  end else begin
    TimeStr := Trim(Copy(DatetimeStr, Pos_Space + 1, Length(DateTimeStr)));
    if TimeStr = '' then begin
      // no time
      Result := USDateStrToDate(DateStr)
    end else
      Result := USDateStrToDate(DateStr) + USTimeStrToTime(TimeStr);
  end;
end;

function DateToSQLDateStr(TheDate: TTntDate): WideString;
var
  M, D, Y: Word;
begin
  DecodeDate(TheDate, Y, M, D);
  Result := WideFormat('%.4d-%.2d-%.2d', [Y, M, D]); { do not localize }
end;

function TimeToSQLTimeStr(TheTime: TDateTime; IncludeMsec: Boolean): WideString;
var
  H, M, S, MS: Word;
begin
  DecodeTime(TheTime, H, M, S, MS);
  Result := WideFormat('%.2d:%.2d:%.2d', [H, M, S]); { do not localize }
  if IncludeMsec then
    Result := WideFormat('%s.%.3d', [Result, MS]);
end;

function DateTimeToSQLDateTimeStr(DateTime: TDateTime; IncludeMsec: Boolean): WideString;
begin
  Result := DateToSQLDateStr(GetTntDate(DateTime)) + ' '
          + TimeToSQLTimeStr(DateTime, IncludeMsec);
end;

function SQLDateSep(Connection: _Connection): WideString;
const
  JET_DateSep = '#';
  SQL_DateSep = '''';
begin
  if (Connection <> nil) and IsMSJet(Connection) then
    Result := JET_DateSep
  else
    Result := SQL_DateSep;
end;

function DateToSQL(Connection: _Connection; Date: TTntDate): WideString;
begin
  Result := SQLDateSep(Connection) + DateToUSDateStr(Date) + SQLDateSep(Connection);
end;

function TimeToSQL(Connection: _Connection; Time: TDateTime): WideString;
begin
  Result := SQLDateSep(Connection) + TimeToUSTimeStr(Time) + SQLDateSep(Connection);
end;

function DateTimeToSQL(Connection: _Connection; DateTime: TDateTime): WideString;
begin
  Result := SQLDateSep(Connection) + DateTimeToUSDateTimeStr(DateTime) + SQLDateSep(Connection);
end;

function BoolToSQL(const Value: Boolean): WideString;
begin
  if Value then
    Result := SQL_TRUE
  else
    Result := SQL_FALSE;
end;

function GetSQLName(Name: WideString): WideString;
var
  i: integer;
  BracketsNeeded: Boolean;
begin
  Name := Trim(Name);
  BracketsNeeded := False;
  for i := 1 to Length(Name) do begin
    if (not IsWideCharAlphaNumeric(Name[i])) and (Name[i] <> '_') then begin
      BracketsNeeded := True;
      break; { we found a bracket condition }
    end;
  end;
  if BracketsNeeded then
    Result := WideFormat('[%s]', [Name])
  else
    Result := Name;
end;

function TreatFieldAsText(field: TField): Boolean;
begin
  {$IFDEF COMPILER_10_UP}
  Result := Field.DataType in [ftString, ftWideString, ftMemo, ftWideMemo];
  {$ELSE}
  Result := Field.DataType in [ftString, ftWideString, ftMemo];
  {$ENDIF}
end;

function FixSQLText(const Value: WideString): WideString;
begin
  // replace all single quotation marks with two single quotation marks
  Result := Tnt_WideStringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

function FixSQLLikeText(const Text: WideString): WideString;
begin
  Result := FixSQLText(Text);
  Result := Tnt_WideStringReplace(Result, '[', '[[]', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '-', '[-]', [rfReplaceAll]);
end;

function QuotedSQLText(const Value: WideString): WideString;
begin
  Result := WideQuotedStr(Value, '''');
end;

function QuotedSQLLikeText(const Value: WideString): WideString;
begin
  Result := Value;
  Result := Tnt_WideStringReplace(Result, '[', '[[]', [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '-', '[-]', [rfReplaceAll]);
  Result := QuotedSQLText(Result);
end;

var
  GReconnectInProgress: Boolean;

function GetTntConnectionStatus(Connection: TADOConnection): TTntADOConnectionStatus;

  function BrokenConnection: Boolean;
  begin
    if (Connection = nil)
    or (Connection.ConnectionObject = nil)
    or (not Connection.Connected) then
      Result := True
    else begin
      try
        Result := False;
        Connection.Execute('SELECT 0');
      except
        Result := True;
      end;
    end;
  end;

begin
  if not BrokenConnection then
    Result := csGood
  else begin
    Result := csBroken;
    if (Supports(Connection, ITntReconnectManager)) then begin
      if GReconnectInProgress then
        Result := csTryRestoreInProgress
      else begin
        GReconnectInProgress := True;
        try
          (Connection as ITntReconnectManager).TryRestoringConnection;
          if (not BrokenConnection) then
            Result := csRestoredToGood;
        finally
          GReconnectInProgress := False;
        end;
      end;
    end;
  end;
end;

function GetSqlResult(Connection: OleVariant; const SQL: WideString; var RecCount: Integer): Variant; overload;
var
  AdoRS: _Recordset;
begin
  Result := Null;
  AdoRS := CoRecordset.Create;
  AdoRS.CursorLocation := adUseClient;
  AdoRS.Open(SQL, Connection, adOpenStatic, adLockReadOnly, adCmdText);
  RecCount := AdoRs.RecordCount;
  if RecCount = 1 then
    Result := AdoRS.Fields[0].Value;
  AdoRS.Close;
end;

function GetSqlResult(Connection: TADOConnection; const SQL: WideString; var RecCount: Integer): Variant; overload;
begin
  ForceAssigned(Connection, 'Connection');
  try
    Result := GetSqlResult(Connection.ConnectionObject, SQL, RecCount);
  except
    case GetTntConnectionStatus(Connection) of
      csGood, csBroken: raise;
      csTryRestoreInProgress: Abort;
      csRestoredToGood:  Result := GetSqlResult(Connection, SQL, RecCount);
      else
        raise ETntInternalError.Create('Unexpected connection status.');
    end;
  end;
end;

function GetSqlResult(Connection: OleVariant; const SQL: WideString): Variant; overload;
var
  RecCount: Integer;
begin
  Result := GetSqlResult(Connection, SQL, RecCount);
end;

function GetSqlResult(Connection: TADOConnection; const SQL: WideString): Variant; overload;
begin
  ForceAssigned(Connection, 'Connection');
  try
    Result := GetSqlResult(Connection.ConnectionObject, SQL);
  except
    case GetTntConnectionStatus(Connection) of
      csGood, csBroken: raise;
      csTryRestoreInProgress: Abort;
      csRestoredToGood:  Result := GetSqlResult(Connection, SQL);
      else
        raise ETntInternalError.Create('Unexpected connection status.');
    end;
  end;
end;

function GetSqlResultDef(Connection: OleVariant; const SQL: WideString; Default: Variant): Variant;
var
  RecCount: Integer;
begin
  Result := GetSqlResult(Connection, Sql, RecCount);
  if Result = Null then
    Result := Default;
end;

function GetSqlResultDef(Connection: TADOConnection; const SQL: WideString; Default: Variant): Variant; overload;
begin
  ForceAssigned(Connection, 'Connection');
  try
    Result := GetSqlResultDef(Connection.ConnectionObject, SQL, Default);
  except
    case GetTntConnectionStatus(Connection) of
      csGood, csBroken: raise;
      csTryRestoreInProgress: Abort;
      csRestoredToGood:  Result := GetSqlResultDef(Connection, SQL, Default);
      else
        raise ETntInternalError.Create('Unexpected connection status.');
    end;
  end;
end;

procedure GetSqlIntegerArrayBasedOnFirstField(Connection: OleVariant; const SQL: WideString; out IntArray: TIntegerDynArray; CommandTimeout: Integer = -1);
var
  AdoRS: _Recordset;
  i: Integer;
  SaveCommandTimeOut: Integer;
begin
  SaveCommandTimeOut := Connection.CommandTimeOut;
  try
    if CommandTimeout <> -1 then
      Connection.CommandTimeOut := CommandTimeout;
    AdoRS := CoRecordset.Create;
    AdoRS.CursorLocation := adUseClient;
    AdoRS.Open(SQL, Connection, adOpenStatic, adLockReadOnly, adCmdText);
    SetLength(IntArray, AdoRS.RecordCount);
    i := 0;
    while not AdoRS.EOF do begin
      IntArray[i] := VarToInt(AdoRS.Fields[0].Value);
      inc(i);
      AdoRS.MoveNext;
    end;
  finally
    Connection.CommandTimeOut := SaveCommandTimeOut;
  end;
end;

procedure AddSqlIntegersBasedOnFirstField(Connection: OleVariant; const SQL: WideString; IntList: TTntIntegerList; AllowDuplicates: Boolean = True);
var
  IntArray: TIntegerDynArray;
begin
  GetSqlIntegerArrayBasedOnFirstField(Connection, SQL, IntArray);
  IntList.AddIntArray(IntArray, AllowDuplicates);
end;

procedure AddSqlStringListBasedOnFirstField(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);
var
  AdoRS: _Recordset;
begin
  Assert(sList <> nil);
  AdoRS := CoRecordset.Create;
  AdoRS.CursorLocation := adUseServer;
  AdoRS.Open(SQL, Connection, adOpenForwardOnly, adLockReadOnly, adCmdText);
  while not AdoRS.EOF do begin
    sList.Add(VarToWideStr(AdoRS.Fields[0].Value));
    AdoRS.MoveNext;
  end;
end;

procedure GetSqlStringListBasedOnFirstField(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);
begin
  Assert(sList <> nil);
  sList.Clear;
  AddSqlStringListBasedOnFirstField(Connection, SQL, sList);
end;

function GetVariantArrayFromSQL(Connection: OleVariant; const SQL: WideString; MinFieldCount: Integer = 1): OleVariant;
var
  AdoRS: _Recordset;
begin
  Result := Unassigned;
  AdoRS := CoRecordset.Create;
  AdoRS.CursorLocation := adUseServer;
  AdoRS.Open(SQL, Connection, adOpenForwardOnly, adLockReadOnly, adCmdText);
  if not AdoRS.EOF then begin
    Result := AdoRS.GetRows(Integer(adGetRowsRest), EmptyParam, EmptyParam);
    Assert(VarArrayDimCount(Result) = 2); { two dimensional }
    Assert(VarArrayLowBound(Result, 1) = 0);
    Assert(VarArrayHighBound(Result, 1) >= MinFieldCount - 1); { number of fields > MinFieldCount }
  end;
end;

procedure GetSqlStringListBasedOnFirstField_VariantMethod(Connection: OleVariant; const SQL: WideString;
  sList: TTntStrings);
var
  V: Variant;
  i: integer;
begin
  Assert(sList <> nil);
  V := GetVariantArrayFromSQL(Connection, SQL);
  sList.Clear;
  if not VarIsEmpty(V) then begin
    for i := VarArrayLowBound(V, 2) to VarArrayHighBound(V, 2) do begin
      // for each record
      sList.Add(VarToWideStr(v[0, i]));
    end;
  end;
end;

procedure InitValueList(const ConnectionObject: Connection;
  const TableName, FieldName: WideString; sList: TTntStrings; TopUsedCount: Cardinal = Cardinal(MaxInt));
var
  Query: WideString;
  ListQuery: Recordset;
  ResultList: TTntStringList;
begin
  ForceAssigned(sList, 'sList');
  ListQuery := CoRecordset.Create;
  ListQuery.CursorLocation := adUseServer;
  // open query
  try
    try
      if (TopUsedCount = 0) or (TopUsedCount = Cardinal(MaxInt)) then begin
        Query := WideFormat('SELECT DISTINCT [%s] FROM [%s]', [FieldName, TableName])
      end else begin
        Query := WideFormat('SELECT TOP %0:d [%1:s], COUNT(*) FROM [%2:s] GROUP BY [%1:s] '
                 + ' ORDER BY COUNT(*) DESC', [TopUsedCount, FieldName, TableName])
      end;
      ListQuery.Open(Query, ConnectionObject, adOpenForwardOnly, adLockReadOnly, adCmdText);
    except
      if ListQuery.State <> adStateClosed then
        ListQuery.Close;
      if (TopUsedCount = 0) or (TopUsedCount = Cardinal(MaxInt)) then
        Query := WideFormat('SELECT [%s] FROM [%s]', [FieldName, TableName])
      else begin
        Query := WideFormat('SELECT TOP %d [%s] FROM [%s]', [TopUsedCount, FieldName, TableName])
      end;
      ForceFmt(ListQuery.State = adStateClosed, 'Internal Error: Query not closed.  State = %d.', [ListQuery.State]);
      ListQuery.Open(Query, ConnectionObject, adOpenForwardOnly, adLockReadOnly, adCmdText);
    end;
  except
    on E: Exception do begin
      E.Message := Query + CRLF + CRLF + E.Message;
      raise;
    end;
  end;

  // collect results
  ResultList := TTntStringList.Create;
  try
    ResultList.Sorted := True;
    ResultList.Duplicates := dupIgnore;
    while not ListQuery.Eof do begin
      ResultList.Add(VarToWideStr(ListQuery.Fields[0].Value));
      ListQuery.MoveNext;
    end;
    ListQuery.Close;
    sList.Assign(ResultList);
  finally
    ResultList.Free;
  end;
end;

function StripBrackets(const Name: WideString): WideString;
begin
  Result := Name;
  if (Length(Result) > 2)
  and (Result[1] = '[')
  and (Result[Length(Result)] = ']') then begin
    Delete(Result, Length(Result), 1);
    Delete(Result, 1, 1);
  end;
end;

procedure ParseOrderBySQL(const SQL: WideString; Items: TTntStrings); { object = 1 for DESC }

  function StringEndsWith(const S, EndsWith: WideString): Boolean;
  var
    EndOfString: WideString;
  begin
    EndOfString := Copy(S, Length(S) - Length(EndsWith) + 1, Length(EndsWith));
    Result := WideSameText(EndOfString, EndsWith);
  end;

  function DeleteEndOfString(const S: WideString; Length: Integer): WideString;
  begin
    Result := S;
    Delete(Result, System.Length(S) - Length + 1, Length);
  end;

type
  TSQLDelimRec = record
    BeginChar: WideChar;
    EndChar: WideChar;
    AllowNesting: Boolean;
    Literals: array [0..2] of WideString;
  end;
  PSQLDelimRec = ^TSQLDelimRec;

const
  DelimRecs: array[0..2] of TSQLDelimRec =
    (
       (BeginChar: '['; EndChar: ']';   AllowNesting: True;  Literals: ('', '', '')),
       (BeginChar: '('; EndChar: ')';   AllowNesting: True;  Literals: ('', '', '')),
       (BeginChar: ''''; EndChar: ''''; AllowNesting: False; Literals: ('''''', '', ''))
    );

  function FindDelimBegin(BeginChar: WideChar): PSQLDelimRec;
  var
    i: integer;
  begin
    Result := nil;
    for i := Low(DelimRecs) to High(DelimRecs) do begin
      if DelimRecs[i].BeginChar = BeginChar then
        Result := @DelimRecs[i];
    end;
  end;

var
  Line: WideString;

  procedure FlushLine;
  begin
    Line := Trim(Line);
    if Line <> '' then begin
      Items.Add(Line);
      Line := '';
    end;
  end;

var
  DelimStack: TStack;
  i: integer;
  Ch: WideChar;
  TopDelim: PSQLDelimRec;
  x: integer;
  Literal: WideString;
  FoundLiteral: Boolean;
  PDelimBegin: PSQLDelimRec;
begin
  DelimStack := TStack.Create;
  try
    // parse SQL
    Items.Clear;
    Line := '';
    i := 1;
    while i <= Length(SQL) do begin
      Ch := SQL[i];
      // peek stack
      if DelimStack.Count > 0 then
        TopDelim := DelimStack.Peek
      else
        TopDelim := nil;
      PDelimBegin := FindDelimBegin(Ch);

      // check literals
      if TopDelim <> nil then begin
        FoundLiteral := False;
        for x := Low(TopDelim.Literals) to High(TopDelim.Literals) do begin
          Literal := TopDelim.Literals[x];
          if Literal <> '' then begin
            if Literal = Copy(SQL, i, Length(Literal)) then
            begin
              Line := Line + Literal;
              inc(i, Length(Literal));
              FoundLiteral := True;
              break;
            end;
          end;
        end;
        if FoundLiteral then
          continue;
      end;

      if (TopDelim <> nil) and (Ch = TopDelim.EndChar) then
        DelimStack.Pop
      else if ((TopDelim = nil) or (TopDelim.AllowNesting)) and (PDelimBegin <> nil) then
        DelimStack.Push(PDelimBegin);

      if (TopDelim = nil) and (Ch = ',') then begin
        FlushLine;
        inc(i);
        continue;
      end;

      Line := Line + Ch;
      inc(i);
    end;
    FlushLine;

    // handle ASC/DESC
    for i := 0 to Items.Count - 1 do begin
      if StringEndsWith(Items[i], ' DESC') then begin
        Items.Objects[i] := TObject(1);
        Items[i] := Trim(DeleteEndOfString(Items[i], Length(' DESC')));
      end else if StringEndsWith(Items[i], ' ASC') then begin
        Items.Objects[i] := nil;
        Items[i] := Trim(DeleteEndOfString(Items[i], Length(' ASC')));
      end else begin
        Items.Objects[i] := nil;
      end;
    end;
  finally
    DelimStack.Free;
  end;
end;

function CanonizeOrderBySQLFromItems(const Items: TTntStrings): WideString;
var
  i: integer;
  Item: WideString;
begin
  Result := '';
  for i := 0 to Items.Count - 1 do begin
    Item := Items[i];
    if Items.Objects[i] = nil then
      Item := Item + ' ASC'
    else
      Item := Item + ' DESC';
    ListAddItem(Result, Item, ', ');
  end;
end;

function CanonizeOrderBySQL(const SQL: WideString): WideString;
var
  Items: TTntStringList;
begin
  Items := TTntStringList.Create;
  try
    ParseOrderBySQL(SQL, Items);
    Result := CanonizeOrderBySQLFromItems(Items);
  finally
    Items.Free;
  end;
end;

function GetUniqueTableName(SQL: AnsiString): WideString;
var
  P: PAnsiChar;
  Token: AnsiString;
  CurSection: TSQLToken;
 
  function NextToken: TSQLToken;
  begin
    result := NextSQLToken(P, Token, CurSection);
    CurSection := result;
  end;
 
begin
  if Pos(' ', SQL) = 0 then
    Result := SQL // just a table name ?
  else begin
    P := PAnsiChar(SQL);
    CurSection := stUnknown;
    result := '';
    // SELECT
    if (NextToken = stSelect) then begin
      while NextToken <> stEnd do begin
        if CurSection = stFrom then
          break;
      end;
      if (CurSection = stFrom)
      and (NextToken = stTableName) then begin
        result := Token;
        if not (NextToken in [stEnd, stWhere, stOrderBy]) then
          result := '';
      end;
    end;
  end;
end;

end.
