
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXUtils;

{$INCLUDE TntCompilers.inc}

interface

{$IFNDEF COMPILER_6_UP}
  Tnt LX Controls need Delphi 6 or newer.
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

uses
  Types, Classes, Windows, DB, SysUtils, TntClasses, TypInfo, ShellApi;

resourcestring
  SNoneLookup = '<none>';

type
  TCurrencyDynArray = array of Currency;
  TStringEvent = procedure(const Value: WideString) of object;
  TIntegerEvent = procedure(const Value: integer) of object;
  TBooleanEvent = procedure(const Value: Boolean) of object;

// validation
procedure ReportUnexpectedEnum(TypeInfo_Enum: PTypeInfo; Enum: Integer);
procedure Force(MustBeTrue: Boolean; const ErrorMsg: WideString);
procedure ForceFmt(MustBeTrue: Boolean; const ErrorMsg: WideString; const Args: array of const);
procedure ForceAssigned(x: Pointer; const Name: WideString);
procedure ForceFileExists(const FileName: WideString);
procedure ForceInterface(AObject: TObject; const ObjectName: WideString;
  const IID: TGUID; const InterfaceName: WideString);
procedure ForceGetInterface(AObject: TObject; const ObjectName: WideString;
  const IID: TGUID; const InterfaceName: WideString; out Obj);

// exception handling
{TNT-WARN ApplicationHandleException}
{TNT-WARN ApplicationShowException}
procedure SafeShowException(E: Exception);

// general
function CompareInts(const A, B: Integer): Integer;
function CompareDoubles(const A, B: Double): Integer;
function GetBit(const Val: Integer; Bit: Integer): Boolean;
function SetBit(const Val: Integer; Bit: Integer; Value: Boolean): Integer;
function ByteToHex(b: byte): WideString;
function HexToByte(hex: WideString): Byte;
function SimpleXorEncrypt(const S: WideString; Key: Word = 9999): WideString;
function SimpleXorDecrypt(const S: WideString; Key: Word = 9999): WideString;

// random
function RandomInteger: Integer;

// delimited strings
procedure ListAddItem(var List: WideString; const Item, ItemSep: WideString);
procedure ListAddNonBlankItem(var List: WideString; const Item, ItemSep: WideString);
function IntArrayAsCommaText(IntArray: array of Integer): WideString;
function CommaTextAsIntArray(const CommaText: WideString): TIntegerDynArray;
function IntegerInArray(Value: Integer; IntArray: array of Integer): Boolean;
function CommaTextAsText(const CommaText: WideString): WideString;
function TextAsCommaText(const Text: WideString): WideString;
function CommaTextAsStringArray(const ACommaText: WideString): TWideStringDynArray;
function IndexOfStringInArray(const Arr: array of WideString; const Value: WideString): Integer;
procedure AddStringToArrayIfNew(var Arr: TWideStringDynArray; const Value: WideString);

// dates
type
  TTntDate = type Integer;

const
  NO_DATE = 0;
  NO_DATE_STR = '0';
  NO_TIME = -1;
  SYS_DAY = 1;
  SYS_HOUR = SYS_DAY / 24;
  SYS_MINUTE = SYS_HOUR / 60;
  SYS_SECOND = SYS_MINUTE / 60;
  SYS_MILLISECOND = SYS_SECOND / 1000;
  SYS_MICROSECOND = SYS_SECOND / 1000000;

function TntDateToStr(ADate: TTntDate): WideString;
function GetTntDate(DateTime: TDateTime): TTntDate;
function MakeMinutes(Minutes: Integer): TDateTime;
function MakeHours(Hours: Integer): TDateTime;
function SafeEncodeDate(Year, Month, Day: Word): TTntDate;
function SafeSetDay(const Date: TTntDate; DayOfMonth: Integer): TTntDate;
function GetDay(Date: TDateTime): Integer;
function GetMonth(Date: TDateTime): Integer;
function GetYear(Date: TDateTime): Integer;
function IncYear(const Date: TDateTime; NumberOfYears: Integer): TDateTime;
function DIncYear(const Date: TTntDate; NumberOfYears: Integer): TTntDate;
function DIncMonth(const Date: TTntDate; NumberOfMonths: Integer): TTntDate;
function IncWeek(const Date: TDateTime; NumberOfWeeks: Integer): TDateTime;
function DIncWeek(const Date: TTntDate; NumberOfWeeks: Integer): TTntDate;
function IncDay(const Date: TDateTime; NumberOfDays: Integer): TDateTime;
function DIncDay(const Date: TTntDate; NumberOfDays: Integer): TTntDate;
function FirstOfMonth(const Date: TDateTime): TTntDate;
function LastOfMonth(const Date: TDateTime): TTntDate;
function FirstOfYear(const Date: TDateTime): TTntDate;
function LastOfYear(const Date: TDateTime): TTntDate;
function RealMonthDiff(Date2, Date1: TDateTime): Integer;

// time
function GetHour(DateTime: TDateTime): Word;
function GetMinute(DateTime: TDateTime): Word;

// variants
function VarToInt(const V: Variant): Integer;
function VarToBool(const V: Variant): Boolean;
function VarArrayToStr(const V: Variant): WideString;
function VariantsEqual(DbValues, KeyValues: Variant; Options: TLocateOptions = []): Boolean;
function CompareVariants(Value1, Value2: Variant; Options: TLocateOptions): Integer;

// windows
function KeyIsDown(vKey: Integer): Boolean;
function MouseIsDown: Boolean;
function IsAdmin: Boolean;
procedure GiveApplicationShutdownPrivilege;
function GetLibraryFullName(LibFileName: WideString): WideString;
function KBSPace(Clusters,  SectorsPerCluster, BytesPerSector: Int64): Int64;

// shell file ops
const
  FOF_NOCOPYSECURITYATTRIBS = $800;
  FOF_NORECURSION           = $1000;
  FOF_NO_CONNECTED_ELEMENTS = $2000;
  FOF_WANTNUKEWARNING       = $4000;

procedure TntShFileOp(Wnd: HWND; wFunc: UINT; pFrom, pTo: WideString;
  fFlags: FILEOP_FLAGS; lpszSimpleProgressTitle: WideString);
procedure ShDeleteItemEx(Wnd: HWND; Item: WideString; Flags: FILEOP_FLAGS;
  SimpleProgressTitle: WideString = '');
procedure ShCopyItemEx(Wnd: HWND; FromItem, ToItem: WideString; Flags: FILEOP_FLAGS;
  SimpleProgressTitle: WideString = '');
procedure ShDeleteItemSilently(Item: WideString);

// file
resourcestring
  SFileNotExists = 'File "%s" doesn''t exist.';
  SPathNotExists = 'Directory "%s" doesn''t exist.';
  SFileCouldntCreate = 'Could not create %s';
  SRenameFileError = 'Could not rename "%s" to "%s".';

const
  DEFAULT_TEMP_PATH = WideString('**WideGetTempPath**');

function SizeOfFile(FileName: WideString): Int64;
procedure UndoReadOnlyIfExists(FileOrDirName: WideString);
procedure ForceCopyFile(const FromFile, ToFile: WideString);
procedure ForceRenameFile(const OldName, NewName: WideString);
procedure TntForceDirectories(const Dir: WideString);
procedure ForceDeleteFile(const FileName: WideString);
procedure ForceDeleteFileIfExists(const FileName: WideString);
function MakePath(const Path: WideString): WideString;
function MakeDir(const Path: WideString): WideString;
function ExtractFileRoot(FileName: WideString): WideString;
procedure AddFilesInPath(Path: WideString; FileList: TTntStrings; Mask: WideString = '*.*'; Recursive: Boolean = False);
procedure GetFilesInPath(Path: WideString; FileList: TTntStrings; Mask: WideString = '*.*'; Recursive: Boolean = False);
procedure AddDirsInPath(Path: WideString; FileList: TTntStrings);
procedure GetDirsInPath(Path: WideString; FileList: TTntStrings);
function GetExpandedFileName(FileName: WideString): WideString;
function AttemptNetworkConnect(const NetPath: Widestring): Boolean; overload;
function AttemptNetworkConnect(const NetPath, UserName, Password: WideString): Boolean; overload;
function WideGetTempPath: WideString;
function CreateGarbageCollectedTempPath: WideString;
procedure DeleteGarbageCollectedFolder(const FolderName: WideString);
function WideGetComputerName: WideString;
function WideGetUserName: WideString;
function GetWindowsDir: WideString;
function GetSystemDir: WideString;
function GetTempFileWithExt(Extension: WideString = '.tmp';
  InitialPath: WideString = DEFAULT_TEMP_PATH; Prefix: WideString = 'tmp'): WideString;
function GetGarbageCollectedTempFile(Extension: WideString = '.tmp'): WideString;
function GetExePath: WideString;
function GetExeFileName: WideString;
function ExtractExactFileName(const FileName: WideString): WideString;
procedure CopyAllAttributes(Src, Dest: WideString);
function GetFileTimeStamp(const FileName: WideString): TDateTime;
function SetCompressedAttribute(FileName: PWideChar; forceCompress: Boolean): Boolean;

// special dirs
function GetSpecialDir(Kind: Integer): WideString;
function GetMyDocumentsDir: WideString;
function GetProgramFilesDir: WideString;
function GetProgramFilesCommonFilesDir: WideString;
function GetDesktopDir: WideString;
function GetCommonDesktopDir: WideString;
function GetStartMenuDir: WideString;
function GetCommonStartMenuDir: WideString;
function GetStartMenuProgramsDir: WideString;
function GetCommonStartMenuProgramsDir: WideString;
function GetLocalAppDataDir: WideString;
function GetAppDataDir: WideString;
function GetCommonAppDataDir: WideString;
function GetFontsDir: WideString;

// shell
type
  EShellOpenError = class(Exception)
  public
    ErrorCode: Integer;
  end;

procedure WideShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: WideString; ShowCmd: Integer);
procedure ShellOpenFileEx(Wnd: HWND; const FileName: WideString; const Parameters: WideString = '';
  const Directory: WideString = ''; ShowCmd: Integer = SW_SHOW);
procedure ShellPrintFileEx(Wnd: HWND; const FileName: WideString; const Directory: WideString = '');
function ExtractSmallIcon(const FileName: WideString): HICON;

// process
resourcestring
  SProcessFailWithCode = 'Process "%s" failed with exit code (%d)';

type
  TWaitMethod = procedure of object;

function StartProcess(Target: WideString;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL;
  _dwCreationFlags: Cardinal = NORMAL_PRIORITY_CLASS): TProcessInformation;
procedure RunProcessAndWait(Target: WideString; WaitProc: TWaitMethod = nil;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL);
function RunProcessAndWaitEx(Target: WideString; WaitProc: TWaitMethod = nil;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL): Cardinal;
procedure RunProcessWithConsoleOutput(CommandLine, CurrentDirectory: WideString; OnOutputLine: TStringEvent);
function RunProcessWithConsoleOutputEx(CommandLine, CurrentDirectory: WideString; OnOutputLine: TStringEvent): Cardinal;

// component streaming
function ComponentToText(Component: TComponent): AnsiString;
function TextToComponent(const Text: AnsiString): TComponent;

// parsing
function MatchesPattern(const OriginalPattern, Text: WideString): Boolean;
function StringMatchesPatternInList(PatternList: TTntStrings; const S: WideString): Boolean;
function GrabTil(var r: WideString; symbol: WideString; part: WideString): WideString;
function ExtractNumbers(const s: WideString): WideString;
function StripSpaces(const W: WideString): WideString;
function CountWord(Word, Text: WideString): Integer;
function UpdateUniqueNameWord(var Val: WideString; const OldWord, NewWord: WideString): Boolean;
function Tabs(Num: Integer): WideString;
procedure AddSearchWords(SearchWords: TTntStrings; const Words: WideString; AddCompositeWords: Boolean; MaxLen: Integer; _Object: TObject = nil);

// Locale Info
const
  UNKNOWN_LOCALE_STR = WideString('???');

function GetLocaleString(LocaleID: LCID; LocaleType: Integer): WideString;
function GetAbbrLangName(LocaleID: LCID): WideString;
function GetNativeLangName(LocaleID: LCID): WideString;
function GetEngLocaleName(lcd: LCID): WideString;
function IsoCodePage(ACP: Cardinal): WideString;

// COM Server Info
function DllPathFromClsID(clsid: TGUID): WideString;
function DllPathFromProgID(const ProgID: WideString): WideString;
function ProgIDVersion(ProgID: WideString): Int64;

// Office 2000 (and newer) info
type
  TOfficeApps = (oaWord, oaExcel, oaOutlook, oaAccess);

var
  Office2000AppExists: array[TOfficeApps] of Boolean = (False, False, False, False);
  OfficeAppVersion: array[TOfficeApps] of WideString = ('', '', '', '');

procedure EnhanceOfficeAutomationError(E: Exception);

// System Info
resourcestring
  SProgramRequires = 'This program requires %s.';

function Win32PlatformStr(X: Integer): WideString;
function GetSysPlatformStr: WideString;
function IsReallyWindowsXP: Boolean;
function Is_WINE: Boolean;
function GetKernelVersion(out ProductName: WideString): Int64;

function WinInetVersion: Int64;
function MapiVendor: WideString;
function MapiVersion: Int64;
function CapiComVersion: Int64;
function MdacVersion: Int64;
function JetOleDBVersion: Int64;
function DaoVersion: Int64;
function SqlDmoVersion: Int64;
function DoesSQL7EntMgrExist: Boolean;
function SqlMerge9AxVersion: Int64;
function Crystal11Version: Int64;
function RpaWinetVersion(out Vendor: WideString): Int64;

// email handling
function Get_mailto_AppName: WideString;
procedure Get_mailto_IconHandles(var Large, Small: HICON);
function Get_mailto_ShellOpenCommand: WideString;
function Is_mailto_OutlookExpress: Boolean;
function Get_mailto_ListSeparator: WideString;
function Make_mailto_Command(const EmailList: WideString; const DefaultDisplayName: WideString): WideString;
procedure ParseEmailAddress(Value: WideString; const DefaultDisplayName: WideString;
  out EmailAddress, TheDisplayName: WideString);
function NormalizeEmailAddress(Value: WideString; const DefaultDisplayName: WideString; IncludeDisplayName: Boolean; AllowDisplayNameOnly: Boolean): WideString;
procedure BuildNormalizedEmailList(sList: TTntStrings; CommaText: WideString;
  const DefaultDisplayName: WideString; IncludeDisplayNames, AllowDisplayNamesOnly: Boolean);
function NormalizeEmailAddressList(const Value: WideString;
  const DefaultDisplayName: WideString; IncludeDisplayNames, AllowDisplayNamesOnly: Boolean;
    EmailSeparator: WideString = ', '): WideString;

implementation

uses
  Variants, ActiveX, Contnrs, SysConst, ShlObj, SHFolder,
  {$IFDEF COMPILER_9_UP} WideStrUtils, WideStrings, {$ENDIF}
  TntWideStrUtils, TntSysUtils, TntWindows, TntLXVerUtils, TntRegistry,
  TntLXRegistry;

//======================================== VALIDATION

procedure ReportUnexpectedEnum(TypeInfo_Enum: PTypeInfo; Enum: Integer);
var
  EnumName: WideString;
  TypeName: WideString;
  TypeData: PTypeData;
begin
  if TypeInfo_Enum = nil then
    raise ETntInternalError.CreateFmt('Internal Error: ReportUnexpectedEnum, (TypeInfo_Enum = nil).  (Value = %d)', [Enum]);
  TypeName := TypeInfo_Enum.Name;
  // check type
  if (TypeInfo_Enum.Kind <> tkEnumeration) then
    raise ETntInternalError.CreateFmt('Internal Error: ReportUnexpectedEnum expected tkEnumeration, but received %s (%s).  (Value = %d)',
      [TypeName, GetEnumName(TypeInfo(TTypeKind), Integer(TypeInfo_Enum.Kind)), Enum]);
  // check value
  TypeData := GetTypeData(TypeInfo_Enum);
  if (Enum < TypeData.MinValue) then
    raise ETntInternalError.CreateFmt('Internal Error: Unexpected %s: %d (below minimum of %d)', [TypeName, Enum, TypeData.MinValue])
  else if (Enum > TypeData.MaxValue) then
    raise ETntInternalError.CreateFmt('Internal Error: Unexpected %s: %d (above maximum of %d)', [TypeName, Enum, TypeData.MaxValue])
  else begin
    // report unexpected value
    EnumName := GetEnumName(TypeInfo_Enum, Enum);
    raise ETntInternalError.CreateFmt('Internal Error: Unexpected %s: %s', [TypeName, EnumName]);
  end;
end;

procedure Force(MustBeTrue: Boolean; const ErrorMsg: WideString);
begin
  If MustBeTrue = False then
    raise Exception.Create(ErrorMsg);
end;

procedure ForceFmt(MustBeTrue: Boolean; const ErrorMsg: WideString; const Args: array of const);
begin
  If MustBeTrue = False then
    raise Exception.CreateFmt(ErrorMsg, Args);
end;

procedure ForceAssigned(x: Pointer; const Name: WideString);
begin
  if x = nil then
    raise ETntInternalError.CreateFmt('Internal Error: %s not assigned.', [Name]);
end;

procedure ForceFileExists(const FileName: WideString);
begin
  Force(WideFileExists(FileName), 'Internal Error: File does not exist: ' + FileName);
end;

procedure ForceInterface(AObject: TObject; const ObjectName: WideString;
  const IID: TGUID; const InterfaceName: WideString);
var
  Obj: IUnknown;
begin
  ForceAssigned(AObject, ObjectName);
  if not AObject.GetInterface(IID, Obj) then
    raise ETntInternalError.CreateFmt('Internal Error: %s must support %s.', [ObjectName, InterfaceName]);
end;

procedure ForceGetInterface(AObject: TObject; const ObjectName: WideString;
  const IID: TGUID; const InterfaceName: WideString; out Obj);
begin
  ForceAssigned(AObject, ObjectName);
  if not AObject.GetInterface(IID, Obj) then
    ForceInterface(AObject, ObjectName, IID, InterfaceName);
end;

//-------------------------------------- Exception Handling
procedure SafeShowException(E: Exception);
begin
  if E = nil then
    E := ExceptObject as Exception;
  if E is EAbort then
    exit;
  if Assigned(Classes.ApplicationShowException{TNT-ALLOW ApplicationShowException}) then
    Classes.ApplicationShowException{TNT-ALLOW ApplicationShowException}(E)
  else if (ExceptObject = E) then
    SysUtils.ShowException(E, ExceptAddr)
  else
    SysUtils.ShowException(E, nil);
end;

//======================================== GENERAL

function CompareInts(const A, B: Integer): Integer;
begin
  if A > B then
    Result := 1
  else if A < B then
    Result := -1
  else
    Result := 0
end;

function CompareDoubles(const A, B: Double): Integer;
begin
  if A > B then
    Result := 1
  else if A < B then
    Result := -1
  else
    Result := 0
end;

function GetBit(const Val: Integer; Bit: Integer): Boolean;
begin
  Result := (Val and (1 shl Bit)) <> 0;
end;

function SetBit(const Val: Integer; Bit: Integer; Value: Boolean): Integer;
begin
  if Value then
    Result := Val or (1 shl Bit)
  else
    Result := Val and (not (1 shl Bit));
end;

const
  HexDigits = WideString('0123456789ABCDEF'); {0 - 15}

function ByteToHex(b: byte): WideString;
begin
  SetLength(Result, 2);
  Result[1] := HexDigits[(b shr 4)   + 1];
  Result[2] := HexDigits[(b and $0F) + 1];
end;

function HexToByte(hex: WideString): Byte;
var
  pHi: Integer;
  pLo: Integer;
begin
  if Length(hex) <> 2 then
    raise ETntInternalError.Create('Internal Error: Hex length <> 2. (HexToByte)');

  pHi := Pos(hex[1], HexDigits) - 1;
  pLo := Pos(hex[2], HexDigits) - 1;

  if (pHi = -1) or (pLo = -1) then
    // invalid character
    raise ETntInternalError.Create('Internal Error: Illegal character. (HexToByte)');

  // compose this byte from two hex digits (4-bits per digit)
  Result := (pHi shl 4) + pLo;
end;

const
  C1 = 52845;
  C2 = 22719;

function SimpleXorEncrypt(const S: WideString; Key: Word = 9999): WideString;

  procedure EncryptByte(b: Byte);
  var
    c: byte;
  begin
    c := b xor (Key shr 8);
    Result := Result + ByteToHex(c);
    Key := Word(Int64(c + Key) * C1 + C2);
  end;

var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(S) do begin
    EncryptByte(HiByte(Word(s[i])));
    EncryptByte(LoByte(Word(s[i])));
  end;
end;

function SimpleXorDecrypt(const S: WideString; Key: Word = 9999): WideString;

    function DecryptByte(b: byte): byte;
    begin
      Result := b xor (Key shr 8);
      Key := Word(Int64(b + Key) * C1 + C2);
    end;

var
  i: Integer;
  r1: WideString;
  r2: WideString;
  W: Word;
  A, B: Byte;
begin
  if (Length(s) mod 4) <> 0 then
    raise ETntInternalError.Create('Internal Error: Length must be a multiple of four. (Decrypt)');
  Result := '';
  for i := 0 to (Length(s) div 4) - 1 do begin
    r1 := Copy(s, (i * 4) + 1, 2);
    r2 := Copy(s, (i * 4) + 3, 2);
    B := DecryptByte(HexToByte(r1));
    A := DecryptByte(HexToByte(r2));
    W := MakeWord(A, B);
    Result := Result + WideChar(W);
  end;
end;

//======================================== RANDOM

var
  RandomizedCalled: Boolean;

function RandomInteger: Integer;
begin
  if not RandomizedCalled then begin
    Randomize;
    RandomizedCalled := True;
  end;
  {$IFDEF COMPILER_9_UP}
  Result := Integer(Random(High(LongInt))); { LongInt is signed 32-bit }
  {$ELSE}
  Result := Integer(Random(High(LongWord))); { LongWord is unsigned 32-bit }
  {$ENDIF}
end;

//======================================== DELIMITED STRINGS

procedure ListAddItem(var List: WideString; const Item, ItemSep: WideString);
begin
  if List <> '' then
    List := List + ItemSep;
  List := List + Item;
end;

procedure ListAddNonBlankItem(var List: WideString; const Item, ItemSep: WideString);
begin
  if Item <> '' then
    ListAddItem(List, Item, ItemSep);
end;

function IntArrayAsCommaText(IntArray: array of Integer): WideString;
var
  i: integer;
begin
  Result := '';
  for i := Low(IntArray) to High(IntArray) do
    ListAddItem(Result, IntToStr(IntArray[i]), ', ');
end;

function CommaTextAsIntArray(const CommaText: WideString): TIntegerDynArray;
var
  sList: TTntStringList;
  i: integer;
  val: Integer;
  idx: Integer;
begin
  sList := TTntStringList.Create;
  try
    sList.CommaText := CommaText;
    SetLength(Result, sList.Count);
    Idx := 0;
    for i := 0 to sList.Count - 1 do begin
      try
        Val := StrToInt(sList[i]);
        Result[Idx] := Val;
        Inc(Idx);
      except
        on EConvertError do
          {do nothing};
        else
          raise;
      end;
    end;
    SetLength(Result, Idx);
  finally
    sList.Free;
  end;
end;

function IntegerInArray(Value: Integer; IntArray: array of Integer): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(IntArray) to High(IntArray) do begin
    if Value = IntArray[i] then begin
      Result := True;
      break;
    end;
  end;
end;

function CommaTextAsText(const CommaText: WideString): WideString;
var
  sList: TTntStringList;
begin
  sList := TTntStringList.Create;
  try
    sList.CommaText := CommaText;
    Result := sList.Text;
  finally
    sList.Free;
  end;
end;

function TextAsCommaText(const Text: WideString): WideString;
var
  sList: TTntStringList;
begin
  sList := TTntStringList.Create;
  try
    sList.Text := Text;
    Result := sList.CommaText;
  finally
    sList.Free;
  end;
end;

function CommaTextAsStringArray(const ACommaText: WideString): TWideStringDynArray;
var
  i: integer;
begin
  with TTntStringList.Create do
  try
    CommaText := ACommaText;
    SetLength(Result, Count);
    for i := 0 to Count - 1 do
      Result[i] := Strings[i];
  finally
    Free;
  end;
end;

function IndexOfStringInArray(const Arr: array of WideString; const Value: WideString): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := Low(Arr) to High(Arr) do begin
    if WideSameText(Arr[i], Value) then begin
      Result := i;
      break;
    end;
  end;
end;

procedure AddStringToArrayIfNew(var Arr: TWideStringDynArray; const Value: WideString);
begin
  if IndexOfStringInArray(Arr, Value) = -1 then begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := Value;
  end;
end;

//======================================== DATE

function TntDateToStr(ADate: TTntDate): WideString;
begin
  if ADate = NO_DATE then
    Result := ''
  else
    Result := DateToStr(ADate);
end;

function GetTntDate(DateTime: TDateTime): TTntDate;
begin
  Result := trunc(DateTime);
end;

function MakeMinutes(Minutes: Integer): TDateTime;
begin
  Result := Minutes * SYS_MINUTE;
end;

function MakeHours(Hours: Integer): TDateTime;
begin
  Result := Hours * SYS_HOUR;
end;

function SafeEncodeDate(Year, Month, Day: Word): TTntDate;
var
  DayTable: PDayTable;
begin
  if (Month < 1) or (Month > 12) then
    raise Exception.CreateFmt('Month out of range: %d', [Month]);
  if (Day < 1) then
    raise Exception.CreateFmt('Day out of range: %d', [Day]);
  // check day
  DayTable := @MonthDays[IsLeapYear(Year)];
  if Day > DayTable^[Month] then Day := DayTable^[Month];
  // encode Result
  Result := GetTntDate(EncodeDate(Year, Month, Day));
end;

function SafeSetDay(const Date: TTntDate; DayOfMonth: Integer): TTntDate;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := SafeEncodeDate(Year, Month, DayOfMonth);
end;

function GetDay(Date: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  Result := D;
end;

function GetMonth(Date: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  Result := M;
end;

function GetYear(Date: TDateTime): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(Date, Y, M, D);
  Result := Y;
end;

function IncYear(const Date: TDateTime; NumberOfYears: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  // extract and increment
  DecodeDate(Date, Year, Month, Day);
  Year := Year + NumberOfYears;
  // encode Result
  Result := SafeEncodeDate(Year, Month, Day);
  ReplaceTime(Result, Date);
end;

function DIncYear(const Date: TTntDate; NumberOfYears: Integer): TTntDate;
begin
  Result := GetTntDate(IncYear(Date, NumberOfYears));
end;

function DIncMonth(const Date: TTntDate; NumberOfMonths: Integer): TTntDate;
begin
  Result := GetTntDate(IncMonth(Date, NumberOfMonths));
end;

function IncWeek(const Date: TDateTime; NumberOfWeeks: Integer): TDateTime;
begin
  Result := Date + (NumberOfWeeks * 7);
end;

function DIncWeek(const Date: TTntDate; NumberOfWeeks: Integer): TTntDate;
begin
  Result := Date + (NumberOfWeeks * 7);
end;

function IncDay(const Date: TDateTime; NumberOfDays: Integer): TDateTime;
begin
  Result := Date + NumberOfDays;
end;

function DIncDay(const Date: TTntDate; NumberOfDays: Integer): TTntDate;
begin
  Result := Date + NumberOfDays;
end;

function FirstOfMonth(const Date: TDateTime): TTntDate;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := SafeEncodeDate(Year, Month, 1);
end;

function LastOfMonth(const Date: TDateTime): TTntDate;
begin
  Result := FirstOfMonth(IncMonth(Date, 1)) - 1; {(First of Next Month) - 1}
end;

function FirstOfYear(const Date: TDateTime): TTntDate;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := SafeEncodeDate(Year, 1, 1);
end;

function LastOfYear(const Date: TDateTime): TTntDate;
begin
  Result := FirstOfYear(IncYear(Date, 1)) - 1; {(First of Next Year) - 1}
end;

function RealMonthDiff(Date2, Date1: TDateTime): Integer;
var
  Y2, Y1, M2, M1, D2, D1: Word;
begin
  DecodeDate(Date1, Y1, M1, D1);
  DecodeDate(Date2, Y2, M2, D2);
  Result := ((Y2*12)+M2) - ((Y1*12)+M1);
end;

// time
function GetHour(DateTime: TDateTime): Word;
var
  Min, Sec, MSec: Word;
begin
  DecodeTime(DateTime, Result, Min, Sec, MSec);
end;

function GetMinute(DateTime: TDateTime): Word;
var
  Hour, Sec, MSec: Word;
begin
  DecodeTime(DateTime, Hour, Result, Sec, MSec);
end;

//======================================== VARIANTS

function VarToInt(const V: Variant): Integer;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := 0;
end;

function VarToBool(const V: Variant): Boolean;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := False;
end;

function VarArrayToStr(const V: Variant): WideString;
var
  i: integer;
begin
  case VarArrayDimCount(V) of
    0:
      Result := VarToWideStr(V);
    1:
      begin
        Result := '';
        for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
          ListAddItem(Result, VarToWideStr(V[i]), ', ')
      end
    else
      Result := '<internal error: multi-dimensional variant array>'; { do not localize }
  end;
end;

function DbCompareString(DbString: WideString; const KeyString: WideString; Options: TLocateOptions): Integer;
begin
  // handle loPartialKey, by shortening KeyString to match DbString
  if (loPartialKey in Options) then begin
    if Length(DbString) > Length(KeyString) then
      SetLength(DBString, Length(KeyString))
  end;

  // handle loCaseInsensitive, by setting CompareFlags
  if (loCaseInsensitive in Options) then
    Result := WideCompareText(DbString, KeyString)
  else
    Result := WideCompareStr(DbString, KeyString);
end;

function NonArrayVariantsEqual(DbValue, KeyValue: Variant; Options: TLocateOptions = []): Boolean;
begin
  Assert((not VarIsArray(DbValue)) and (not VarIsArray(KeyValue)),
    'InternalError: Array type variants found.');
  try
    if Options = [] then begin
      // no options, compare variants directly
      if VarIsEmpty(DbValue) then
        Result := VarIsEmpty(KeyValue)
      else if VarIsNull(DbValue) then
        Result := VarIsNull(KeyValue)
      else if VarIsEmpty(KeyValue) or VarIsNull(KeyValue) then
        Result := False
      else begin
        //VT_DECIMAL is MS's Decimal variant type which is unsupported by Delphi 5/6/7
        //and we therefore have to convert it to varDouble for the comparison.
        //varCurrency could also be used but it has DbValue very limited precision.
        if TVarData(DbValue).VType = VT_DECIMAL then
          DbValue := VarAsType(DbValue, varDouble);
        if TVarData(KeyValue).VType = VT_DECIMAL then
          KeyValue := VarAsType(KeyValue, varDouble);
        // compare directly
        Result := (DbValue = KeyValue)
      end;
    end else begin
      // force WideString comparison with options
      Result := DbCompareString(DbValue, KeyValue, Options) = 0;
    end;
  except
    Result := False;
  end;
end;

function VariantsEqual(DbValues, KeyValues: Variant; Options: TLocateOptions = []): Boolean;
var
  i: integer;
begin
  if (VarArrayDimCount(DbValues) <> VarArrayDimCount(KeyValues)) then
    // different array dim count
    Result := False
  else if not VarIsArray(DbValues) then
    // both are not array
    Result := NonArrayVariantsEqual(DbValues, KeyValues, Options)
  else if (VarArrayLowBound( DbValues, 1) <> VarArrayLowBound( KeyValues, 1))
  or      (VarArrayHighBound(DbValues, 1) <> VarArrayHighBound(KeyValues, 1)) then
    // array, different bounds
    Result := False
  else begin
    // array, same dim count, same bounds
    Result := True; { assume equal }
    for i := VarArrayLowBound(DbValues, 1) to VarArrayHighBound(DbValues, 1) do begin
      if not VariantsEqual(DbValues[i], KeyValues[i]) then begin
        Result := False;
        break; { found one that is NOT equal }
      end;
    end;
  end;
end;

//--------------------------

function CompareNonArrayVariants(Value1, Value2: Variant; Options: TLocateOptions): Integer;

  function VarIsEmptyOrNull(V: Variant): Boolean;
  begin
    Result := VarIsNull(V) or VarIsEmpty(V);
  end;

var
  VarRelationship: TVariantRelationship;
begin
  Assert((not VarIsArray(Value1)) and (not VarIsArray(Value2)), 'InternalError: Array type variants found.');
  if (Options <> []) and VarIsStr(Value1) and VarIsStr(Value2) then
    Result := DbCompareString(Value1, Value2, Options)
  else begin
    VarRelationship := VarCompareValue(Value1, Value2);
    if VarRelationship = vrNotEqual then begin
      // one of them is null/empty
      if VarIsEmptyOrNull(Value1) and (not VarIsEmptyOrNull(Value2)) then
        VarRelationship := vrLessThan
      else if (not VarIsEmptyOrNull(Value1)) and VarIsEmptyOrNull(Value2) then
        VarRelationship := vrGreaterThan
      else
        VarRelationship := vrEqual;
    end;
    if VarRelationship = vrLessThan then
      Result := -1
    else if VarRelationship = vrGreaterThan then
      Result := 1
    else
      Result := 0;
  end;
end;

function CompareVariants(Value1, Value2: Variant; Options: TLocateOptions): Integer;
var
  i: integer;
begin
  Assert(VarArrayDimCount(Value1) = VarArrayDimCount(Value2), 'CompareVariants: Must have same dim count.');
  if (not VarIsArray(Value1)) then
    // both are not array
    Result := CompareNonArrayVariants(Value1, Value2, Options)
  else begin
    Assert(VarArrayLowBound( Value1, 1) = VarArrayLowBound( Value2, 1), 'CompareVariants: Must have same lower bound.');
    Assert(VarArrayHighBound(Value1, 1) = VarArrayHighBound(Value2, 1), 'CompareVariants: Must have same upper bound.');
    // array, same dim count, same bounds
    Result := 0; { assume equal (zero dim count?!) }
    for i := VarArrayLowBound(Value1, 1) to VarArrayHighBound(Value1, 1) do begin
      Result := CompareVariants(Value1[i], Value2[i], Options);
      if Result <> 0 then
        break; { found most significant value that is NOT equal }
    end;
  end;
end;

//======================================== WINDOWS

function KeyIsDown(vKey: Integer): Boolean;
begin
  Result := not (GetAsyncKeyState(vKey) in [0, 1]);
end;

function MouseIsDown: Boolean;
begin
  Result := KeyIsDown(VK_LBUTTON);
end;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;

function IsAdmin: Boolean;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Result := True
  else begin
    Result := False;
    bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
    if not bSuccess then
    begin
      if GetLastError = ERROR_NO_TOKEN then
        bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
    end;
    if bSuccess then
    begin
      GetMem(ptgGroups, 1024);
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
      CloseHandle(hAccessToken);
      if bSuccess then
      begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
          SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
            0, 0, 0, 0, 0, 0, psidAdministrators);
        {$R-}
        for x := 0 to ptgGroups.GroupCount - 1 do begin
          if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
          begin
            Result := True;
            Break;
          end;
        end;
        {$R+}
        FreeSid(psidAdministrators);
      end;
      FreeMem(ptgGroups);
    end;
  end;
end;

procedure GiveApplicationShutdownPrivilege;
var
  hToken: THANDLE;
  tkp, tkDumb: TTokenPrivileges;
  DumbInt: Cardinal;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    // Get a token for this process
    ZeroMemory(@tkp, SizeOf(tkp));
    Win32Check(OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken));

    // Get the LUID for the Shutdown privilege
    LookupPrivilegeValue(nil, 'SeShutdownPrivilege', tkp.Privileges[0].Luid);
    tkp.PrivilegeCount := 1; // one privilege to set
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

    // Add the shutdown privilege to this process
    Win32Check(AdjustTokenPrivileges(hToken, false, tkp, sizeof(tkDumb), tkDumb, DumbInt));
  end;
end;

function GetLibraryFullName(LibFileName: WideString): WideString;
var
  HMod: HMODULE;
begin
  Result := '';
  HMod := Tnt_LoadLibraryExW(PWideChar(LibFileName), 0, DONT_RESOLVE_DLL_REFERENCES);
  if HMod <> 0 then
  try
    Result := WideGetModuleFileName(HMod);
  finally
    FreeLibrary(HMod);
  end;
  if not WideFileExists(Result) then
    Result := '';
end;

function KBSPace(Clusters,  SectorsPerCluster, BytesPerSector: Int64): Int64;
begin
  Result := Clusters * SectorsPerCluster * BytesPerSector div (1024 * 1024);
end;

//======================================== SHELL FILE OPS

resourcestring
  SFileOpAborted = 'The file operation was aborted.';

procedure TntShFileOp(Wnd: HWND; wFunc: UINT; pFrom, pTo: WideString;
  fFlags: FILEOP_FLAGS; lpszSimpleProgressTitle: WideString);
var
  FileOp: TSHFileOpStructW;
  FileOpResult: Cardinal;
begin
  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.Wnd                   := Wnd;
  FileOp.wFunc                 := WFunc;
  FileOp.fFlags                := fFlags;
  FileOp.fAnyOperationsAborted := FALSE;
  FileOp.hNameMappings         := nil;
  FileOp.pFrom                 := PWideChar(pFrom + WideString(#0#0));
  FileOp.pTo                   := PWideChar(pTo + WideString(#0#0));
  FileOp.lpszProgressTitle     := PWideChar(lpszSimpleProgressTitle);
  FileOpResult := Tnt_SHFileOperationW(FileOp);
  if (FileOpResult <> 0)
  or FileOp.fAnyOperationsAborted then begin
    if (FOF_NOCONFIRMATION and fFlags) = 0 then
      Abort
    else
      raise ETntGeneralError.Create(SFileOpAborted);
  end;
end;

procedure ShDeleteItemEx(Wnd: HWND; Item: WideString; Flags: FILEOP_FLAGS;
  SimpleProgressTitle: WideString = '');
begin
  if WideDirectoryExists(Item) then
    Item := MakeDir(Item);
  TntShFileOp(Wnd, FO_DELETE, Item, '', Flags, SimpleProgressTitle);
end;

procedure ShCopyItemEx(Wnd: HWND; FromItem, ToItem: WideString; Flags: FILEOP_FLAGS;
  SimpleProgressTitle: WideString = '');
begin
  TntShFileOp(Wnd, FO_COPY, FromItem, ToItem, Flags, SimpleProgressTitle);
end;

procedure ShDeleteItemSilently(Item: WideString);
begin
  // silently delete item using shell
  try
    ShDeleteItemEx(0, Item, FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_SILENT);
  except
  end;
end;

//======================================== FILE

function SizeOfFile(FileName: WideString): Int64;
var
  SearchRec: TSearchRecW;
begin
  if WideFindFirst(FileName, faAnyFile, SearchRec) <> 0 then
    raise ETntGeneralError.CreateFmt(SFileNotExists, [FileName]);
  Result := SearchRec.Size;
  WideFindClose(SearchRec);
end;

procedure UndoReadOnlyIfExists(FileOrDirName: WideString);
var
  OldAttributes: Cardinal;
begin
  if (WideFileExists(FileOrDirName) or WideDirectoryExists(FileOrDirName)) then begin
    OldAttributes := WideFileGetAttr(FileOrDirName);
    if OldAttributes = INVALID_FILE_ATTRIBUTES then
      RaiseLastOSError;
    if ((OldAttributes and faReadOnly) <> 0) then begin
      Win32Check(WideFileSetAttr(FileOrDirName, OldAttributes and not faReadOnly));
    end;
  end;
end;

procedure ForceCopyFile(const FromFile, ToFile: WideString);
begin
  ForceFileExists(FromFile);
  ForceDeleteFileIfExists(ToFile);
  Win32Check(WideCopyFile(FromFile, ToFile, False));
end;

procedure ForceRenameFile(const OldName, NewName: WideString);

  procedure ForceRenameFile_Internal(const OldName, NewName: WideString);
  begin
    if not WideRenameFile(OldName, NewName) then
    try
      RaiseLastOSError;
    except
      on E: Exception do begin
        E.Message := E.Message + CRLF + CRLF + WideFormat(SRenameFileError, [OldName, NewName]);
        raise;
      end;
    end;
  end;

var
  IntermediateFile: WideString;
  i: integer;
begin
  if not WideSameText(OldName, NewName) then
    ForceRenameFile_Internal(OldName, NewName)
  else begin
    // name change is in case only
    i := 0;
    repeat
      inc(i);
      IntermediateFile := WideExtractFilePath(OldName) + ExtractFileRoot(OldName)
                        + '_' + IntToStr(i) + WideExtractFileExt(OldName);
    until not WideFileExists(IntermediateFile);
    // don't rename directly, but rather use intermediate
    ForceRenameFile_Internal(OldName, IntermediateFile);
    ForceRenameFile_Internal(IntermediateFile, NewName);
  end;
end;

procedure TntForceDirectories(const Dir: WideString);
begin
  if not WideForceDirectories(Dir) then
    raise ETntGeneralError.CreateFmt(SFileCouldntCreate, [Dir]);
end;

procedure ForceDeleteFile(const FileName: WideString);
begin
  if not WideDeleteFile(FileName) then begin
    UndoReadOnlyIfExists(FileName);
    Win32Check(WideDeleteFile(FileName));
  end;
end;

procedure ForceDeleteFileIfExists(const FileName: WideString);
begin
  if WideFileExists(FileName) then
    ForceDeleteFile(FileName);
end;

function MakePath(const Path: WideString): WideString;
begin
  if TntWideLastChar(Path) = PathDelim then
    Result := Path
  else
    Result := Path + PathDelim;
end;

function MakeDir(const Path: WideString): WideString;
begin
  Result := Path;
  if (TntWideLastChar(Result) = PathDelim) then
    Delete(Result, Length(Result), 1);
end;

function ExtractFileRoot(FileName: WideString): WideString;
var
  FilePart: WideString;
  FileExt: WideString;
begin
  // This takes a file name like "c:\dir\filename.ext" and returns "filename"
  FilePart := WideExtractFileName(FileName);
  FileExt := WideExtractFileExt(FilePart);
  Result := Copy(FilePart, 1, Length(FilePart) - Length(FileExt));
end;

procedure AddFilesInPath(Path: WideString; FileList: TTntStrings; Mask: WideString = '*.*'; Recursive: Boolean = False);
var
  DosError: Integer;
  SearchRec: TSearchRecW;
  Dirs: TTntStringList;
  i: integer;
begin
  Path := MakePath(Path);
  if not WideDirectoryExists(Path) then
    raise ETntGeneralError.CreateFmt(SPathNotExists, [Path]);

  DosError := WideFindFirst(Path +  Mask, faAnyFile, SearchRec);
  While (DosError = 0) do
  begin
    if not (SearchRec.Attr and faDirectory = faDirectory) then
      FileList.Add(Path + SearchRec.Name);
    DosError := WideFindNext(SearchRec);
  end;
  WideFindClose(SearchRec);

  if Recursive then begin
    Dirs := TTntStringList.Create;
    try
      GetDirsInPath(Path, Dirs);
      for i := 0 to Dirs.Count - 1 do
        AddFilesInPath(Dirs[i], FileList, Mask, Recursive);
    finally
      Dirs.Free;
    end;
  end;
end;

procedure GetFilesInPath(Path: WideString; FileList: TTntStrings; Mask: WideString = '*.*'; Recursive: Boolean = False);
begin
  FileList.BeginUpdate;
  try
    FileList.Clear;
    AddFilesInPath(Path, FileList, Mask, Recursive);
  finally
    FileList.EndUpdate;
  end;
end;

procedure AddDirsInPath(Path: WideString; FileList: TTntStrings);
var
   DosError: Integer;
   SearchRec: TSearchRecW;
begin
  Path := MakePath(Path);
  if not WideDirectoryExists(Path) then
    raise ETntGeneralError.CreateFmt(SPathNotExists, [Path]);
  DosError := WideFindFirst(Path +  '*.*', faDirectory, SearchRec);
  While (DosError = 0) do
  begin
    if (SearchRec.Attr and faDirectory = faDirectory)
    and (SearchRec.Name[1] <> '.') then
      FileList.Add(Path + SearchRec.Name);
    DosError := WideFindNext(SearchRec);
  end;
  WideFindClose(SearchRec);
end;

procedure GetDirsInPath(Path: WideString; FileList: TTntStrings);
begin
  FileList.BeginUpdate;
  try
    FileList.Clear;
    AddDirsInPath(Path, FileList);
  finally
    FileList.EndUpdate;
  end;
end;

function GetExpandedFileName(FileName: WideString): WideString;

    function FindFileName(FileName: WideString): WideString;
    var
      sr: TSearchRecW;
    begin
      if WideFindFirst(FileName, faAnyFile, sr) <> 0 then
        Result := FileName
      else begin
        try
          Result := sr.FindData.cFileName;
        finally
          WideFindClose(sr);
        end;
      end;
    end;

var
  Dir: WideString;
begin
  if FileName = '' then
    Result := ''
  else begin
    FileName := WideExpandFileName(FileName);
    Dir := WideExtractFileDir(FileName);
    if Dir = '' then
      Result := FindFileName(FileName)
    else if WideSameText(Dir, FileName) then
      Result := FileName
    else
      Result := MakePath(GetExpandedFileName(Dir)) + FindFileName(FileName)
  end;
end;

function Internal_AttemptNetworkConnect(const NetPath: WideString; UserName, Password: PWideChar): Boolean; overload;
var
  NetRes : TNetResourceW;
  Ansi_UserName, Ansi_Password: PAnsiChar;
begin
  ZeroMemory(@NetRes, SizeOf(NetRes));
  with NetRes do
  begin
    dwScope := RESOURCE_GLOBALNET;
    dwType := RESOURCETYPE_DISK;
    dwDisplayType := RESOURCEDISPLAYTYPE_GENERIC;
    dwUsage := RESOURCEUSAGE_CONNECTABLE;
  end;
  if Win32PlatformIsUnicode then begin
    NetRes.lpRemoteName := PWideChar(NetPath);
    Result := (WNetAddConnection2W(NetRes, Password, UserName, 0) = NO_ERROR)
  end else begin
    // remotename
    TNetResourceA(NetRes).lpRemoteName := PAnsiChar(AnsiString(NetPath));
    // username
    if UserName = nil then
      Ansi_UserName := nil
    else
      Ansi_UserName := PAnsiChar(AnsiString(UserName));
    // password
    if Password = nil then
      Ansi_Password := nil
    else
      Ansi_Password := PAnsiChar(AnsiString(Password));
    // connect
    Result := (WNetAddConnection2A(TNetResourceA(NetRes), Ansi_Password, Ansi_UserName, 0) = NO_ERROR)
  end;
end;

function AttemptNetworkConnect(const NetPath: Widestring): Boolean; overload;
begin
  Result := Internal_AttemptNetworkConnect(NetPath, nil, nil);
end;

function AttemptNetworkConnect(const NetPath, UserName, Password: WideString): Boolean; overload;
begin
  Result := Internal_AttemptNetworkConnect(NetPath, PWideChar(UserName), PWideChar(Password));
end;

function WideGetTempPath: WideString;
begin
  SetLength(Result, MAX_PATH);
  Tnt_GetTempPathW(MAX_PATH, PWideChar(Result));
  Result := PWideChar(Result);
end;

var
  GarbageCollectedFolderList: TTntStrings = nil;

function CreateGarbageCollectedTempPath: WideString;
begin
  repeat
    Result := MakePath(WideGetTempPath + 'Temp' + IntToStr(RandomInteger));
  until (not WideFileExists(Result)) and (not WideDirectoryExists(Result));
  TntForceDirectories(Result);
  ForceAssigned(GarbageCollectedFolderList, 'GarbageCollectedFolderList');
  GarbageCollectedFolderList.Add(Result);
end;

procedure DeleteGarbageCollectedFolder(const FolderName: WideString);
begin
  try
    if WideDirectoryExists(FolderName) then begin
      ShDeleteItemSilently(FolderName);
    end;
    ForceAssigned(GarbageCollectedFolderList, 'GarbageCollectedFolderList');
    GarbageCollectedFolderList.Delete(GarbageCollectedFolderList.IndexOf(FolderName));
  except
  end;
end;

procedure DeleteGarbageCollectedFolders;
var
  i: integer;
begin
  ForceAssigned(GarbageCollectedFolderList, 'GarbageCollectedFolderList');
  for i := GarbageCollectedFolderList.Count - 1 downto 0 do begin
    DeleteGarbageCollectedFolder(GarbageCollectedFolderList[i]);
  end;
  GarbageCollectedFolderList.Clear;
end;

function WideGetComputerName: WideString;
var
  nSize: Cardinal;
begin
  SetLength(Result, MAX_COMPUTERNAME_LENGTH + 1);
  nSize := Length(Result);
  Tnt_GetComputerNameW(PWideChar(Result), nSize);
  Result := PWideChar(Result);
end;

function WideGetUserName: WideString;
var
  nSize: Cardinal;
begin
  SetLength(Result, 255);
  nSize := Length(Result);
  Tnt_GetUserNameW(PWideChar(Result), nSize);
  Result := PWideChar(Result);
end;

function GetWindowsDir: WideString;
begin
  SetLength(Result, MAX_PATH);
  Tnt_GetWindowsDirectoryW(PWideChar(Result), MAX_PATH);
  Result := PWideChar(Result);
end;

function GetSystemDir: WideString;
begin
  SetLength(Result, MAX_PATH);
  Tnt_GetSystemDirectoryW(PWideChar(Result), MAX_PATH);
  Result := PWideChar(Result);
end;

function GetTempFileWithExt(Extension: WideString = '.tmp'; InitialPath: WideString = DEFAULT_TEMP_PATH; Prefix: WideString = 'tmp'): WideString;
begin
  if InitialPath = DEFAULT_TEMP_PATH then
    InitialPath := WideGetTempPath;
  InitialPath := MakePath(InitialPath);
  TntForceDirectories(InitialPath);
  repeat
    Result := InitialPath + Prefix + '-' + IntToHex(Word(RandomInteger), 0) + Extension;
  until (not WideFileExists(Result));
end;

var
  GarbageCollectedFileList: TTntStrings = nil;

function GetGarbageCollectedTempFile(Extension: WideString = '.tmp'): WideString;
begin
  Result := GetTempFileWithExt(Extension);
  ForceAssigned(GarbageCollectedFileList, 'GarbageCollectedFileList');
  GarbageCollectedFileList.Add(Result);
end;

procedure DeleteGarbageCollectedFiles;
var
  i: integer;
begin
  ForceAssigned(GarbageCollectedFileList, 'GarbageCollectedFileList');
  for i := 0 to GarbageCollectedFileList.Count - 1 do
    WideDeleteFile(GarbageCollectedFileList[i]);
  GarbageCollectedFileList.Clear;
end;

function GetExePath: WideString;
begin
  Result := WideExtractFilePath(GetExeFileName);
end;

function GetExeFileName: WideString;
begin
  Result := WideGetModuleFileName(0);
end;

function ExtractExactFileName(const FileName: WideString): WideString;
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
begin
  FindHandle := WinCheckFileH(Tnt_FindFirstFileW(PWideChar(FileName), FindData));
  try
    Result := FindData.cFileName;
  finally
    Win32Check(Windows.FindClose(FindHandle));
  end;
end;

procedure CopyAllAttributes(Src, Dest: WideString);
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
  hDest: THandle;
begin
  FindHandle := WinCheckFileH(Tnt_FindFirstFileW(PWideChar(Src), FindData));
  try
    // attributes
    Win32Check(WideFileSetAttr(Dest, FindData.dwFileAttributes));

    // time stamps
    hDest := WinCheckFileH(WideFileOpen(Dest, fmOpenWrite or fmShareDenyWrite));
    try
      Win32Check(SetFileTime(hDest, @FindData.ftCreationTime, @FindData.ftLastAccessTime,
        @FindData.ftLastWriteTime))
    finally
      FileClose(hDest);
    end;
  finally
    Win32Check(Windows.FindClose(FindHandle));
  end;
end;

function GetFileTimeStamp(const FileName: WideString): TDateTime;
var
  DosError: Integer;
  SearchRec: TSearchRecW;
begin
  Result := 0;
  DosError := WideFindFirst(FileName, faAnyFile, SearchRec);
  if DosError = 0 then begin
    Result := FileDateToDateTime(SearchRec.Time);
    WideFindClose(SearchRec);
  end;
end;

{
  To set a file's compression state, use the DeviceIoControl function with the
  FSCTL_SET_COMPRESSION operation.

  Call the following function with the name of the file to compress and
  boolean parameter 'forceCompress'. If that one is true, file will be compressed.
  If it is false, the file will be compressed only if its parent folder is
  compressed (reason for that parameter: if you MoveFile uncompressed file from
  uncompressed folder to compressed folder, file will not become automatically
  compressed - at least under some NT 4 service packs).
}


const
//  COMPRESSION_FORMAT_NONE = 0;
//  COMPRESSION_FORMAT_LZNT1 = 2;
  COMPRESSION_FORMAT_DEFAULT = 1;
  FILE_DEVICE_FILE_SYSTEM = 9;
  METHOD_BUFFERED = 0;
  FILE_READ_DATA = 1;
  FILE_WRITE_DATA = 2;
  FSCTL_SET_COMPRESSION = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or (16 shl 2) or METHOD_BUFFERED;

{$R-}

function SetCompressedAttribute(FileName: PWideChar; forceCompress: Boolean): Boolean;
var
  hnd: Integer;
  Comp: SHORT;
  res: DWORD;
begin
  Result := False;
  if Win32PlatformIsUnicode then begin
    if ((Tnt_GetFileAttributesW(PWideChar(WideExtractFilePath(FileName)))
        and FILE_ATTRIBUTE_COMPRESSED) <> 0)
    then
      Result := True // already compressed
    else if (not forceCompress) then
      Result := False // don't force
    else begin
      hnd := Tnt_CreateFileW(FileName, GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS, 0);
      try
        Comp := COMPRESSION_FORMAT_DEFAULT;
        Result := DeviceIoControl(hnd, FSCTL_SET_COMPRESSION, @Comp, SizeOf(SHORT), nil, 0, res, nil);
      finally
        CloseHandle(hnd);
      end;
    end;
  end;
end;

{$R+}

//======================================== SPECIAL DIRS
{
//    this code requires shell32.dll 4.71 which comes
//    with IE 4.0 w/ shell integration (desktop update)
//    thus it will not work on Windows 95 with just IE 4
function GetSpecialDir(Kind: Integer): WideString;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPathW(0, PWideChar(Result), Kind, FALSE);
  Result := PWideChar(Result);
end;}

function GetSpecialDir(Kind: Integer): WideString;
var
  PIDL: PItemIDList;
  Malloc: IMalloc;
begin
  SHGetSpecialFolderLocation(0, Kind, PIDL);
  SetLength(Result, MAX_PATH);
  if Tnt_SHGetPathFromIDListW(PIDL, PWideChar(Result)) then
    Result := PWideChar(Result)
  else
    Result := ''; // folder isn't part of file system
  // free pidl with malloc
  SHGetMalloc(Malloc);
  Malloc.Free(PIDL);
end;

function GetMyDocumentsDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_PERSONAL);
  if Result = '' then begin
    Result := 'C:\My Documents';
    TntForceDirectories(Result);
  end;
end;

function GetProgramFilesDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_PROGRAM_FILES);
  if Result = '' then begin
    Result := 'C:\Program Files';
    TntForceDirectories(Result);
  end;
end;

function GetProgramFilesCommonFilesDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_PROGRAM_FILES_COMMON);
  if Result = '' then begin
    Result := GetProgramFilesDir + '\Common Files';
    TntForceDirectories(Result);
  end;
end;

function GetDesktopDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_DESKTOPDIRECTORY);
end;

function GetCommonDesktopDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_COMMON_DESKTOPDIRECTORY);
  if Result = '' then
    Result := GetDesktopDir;
end;

function GetStartMenuDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_STARTMENU);
end;

function GetCommonStartMenuDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_COMMON_STARTMENU);
  if Result = '' then
    Result := GetStartMenuDir;
end;

function GetStartMenuProgramsDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_PROGRAMS);
end;

function GetCommonStartMenuProgramsDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_COMMON_PROGRAMS);
  if Result = '' then
    Result := GetStartMenuProgramsDir;
end;

function GetLocalAppDataDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_LOCAL_APPDATA);
  if Result = '' then begin
    Result := GetWindowsDir + '\Local Application Data';
    TntForceDirectories(Result);
  end;
end;

function GetAppDataDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_APPDATA);
  if Result = '' then begin
    Result := GetWindowsDir + '\Application Data';
    TntForceDirectories(Result);
  end;
end;

function GetCommonAppDataDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_COMMON_APPDATA);
  if Result = '' then begin
    Result := GetWindowsDir + '\Common Application Data';
    TntForceDirectories(Result);
  end;
end;

function GetFontsDir: WideString;
begin
  Result := GetSpecialDir(CSIDL_FONTS);
end;

//--------------------------- shell
resourcestring
  SShellError_LOW_RESOURCES = 'The operating system is out of memory or resources.';
  SShellError_BAD_FORMAT = 'The .exe file is invalid (non-Microsoft Win32® .exe or error in .exe image).';
  SShellError_ACCESSDENIED = 'The operating system denied access to the specified file.';
  SShellError_ASSOCINCOMPLETE = 'The file name association is incomplete or invalid.';
  SShellError_DDEBUSY = 'The Dynamic Data Exchange (DDE) transaction could not be completed because other DDE transactions were being processed.';
  SShellError_DDEFAIL = 'The DDE transaction failed.';
  SShellError_DDETIMEOUT = 'The DDE transaction could not be completed because the request timed out.';
  SShellError_DLLNOTFOUND = 'The specified dynamic-link library (DLL) was not found.';
  SShellError_FNF = 'The specified file was not found.';
  SShellError_NOASSOC = 'There is no application associated with the given file name extension.';
  SShellError_NOASSOC_Print = 'The specified file is not printable.';
  SShellError_OOM = 'There was not enough memory to complete the operation.';
  SShellError_PNF = 'The specified path was not found.';
  SShellError_SHARE = 'A sharing violation occurred.';

procedure CheckShellExecute(ErrorCode: Integer; const Operation, FileName: WideString);
var
  ErrorMsg: WideString;
  ShellException: EShellOpenError;
begin
  ErrorMsg := '';
  case ErrorCode of
    0:                      ErrorMsg := SShellError_LOW_RESOURCES;
    ERROR_BAD_FORMAT:       ErrorMsg := SShellError_BAD_FORMAT;
    SE_ERR_ACCESSDENIED:    ErrorMsg := SShellError_ACCESSDENIED;
    SE_ERR_ASSOCINCOMPLETE: ErrorMsg := SShellError_ASSOCINCOMPLETE;
    SE_ERR_DDEBUSY:         ErrorMsg := SShellError_DDEBUSY;
    SE_ERR_DDEFAIL:         ErrorMsg := SShellError_DDEFAIL;
    SE_ERR_DDETIMEOUT:      ErrorMsg := SShellError_DDETIMEOUT;
    SE_ERR_DLLNOTFOUND:     ErrorMsg := SShellError_DLLNOTFOUND;
    SE_ERR_FNF:             ErrorMsg := SShellError_FNF;
    SE_ERR_NOASSOC:     begin
                          if WideSameText(Operation, 'print') then
                            ErrorMsg := SShellError_NOASSOC_Print
                          else
                            ErrorMsg := SShellError_NOASSOC;
                        end;
    SE_ERR_OOM:             ErrorMsg := SShellError_OOM;
    SE_ERR_PNF:             ErrorMsg := SShellError_PNF;
    SE_ERR_SHARE:           ErrorMsg := SShellError_SHARE;
  end;
  if ErrorMsg <> '' then begin
    ShellException := EShellOpenError.Create(ErrorMsg);
    ShellException.ErrorCode := ErrorCode;
    raise ShellException;
  end else if ErrorCode <= 32 then begin
    ShellException := EShellOpenError.CreateFmt('Internal Error: ShellExecute() returned code %d for "%s".',
      [ErrorCode, FileName]);
    ShellException.ErrorCode := ErrorCode;
    raise ShellException;
  end;
end;

procedure WideShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: WideString; ShowCmd: Integer);
var
  ErrCode: Cardinal;
begin
  ErrCode := Tnt_ShellExecuteW(hWnd, PWideChar(Operation), PWideChar(FileName),
    PWideChar(Parameters), PWideChar(Directory), ShowCmd);
  CheckShellExecute(ErrCode, Operation, FileName);
end;

procedure ShellOpenFileEx(Wnd: HWND; const FileName: WideString; const Parameters: WideString = '';
  const Directory: WideString = ''; ShowCmd: Integer = SW_SHOW);
begin
  WideShellExecute(Wnd, 'open', FileName, Parameters, Directory, ShowCmd);
end;

procedure ShellPrintFileEx(Wnd: HWND; const FileName: WideString; const Directory: WideString = '');
begin
  WideShellExecute(Wnd, 'print', FileName, '', Directory, SW_SHOW);
end;

function ExtractSmallIcon(const FileName: WideString): HICON;
var
  FileInfoW: TSHFileInfoW;
begin
  Tnt_SHGetFileInfoW(Pointer(PWideChar(FileName)), 0, FileInfoW, SizeOf(TSHFileInfoW), SHGFI_ICON or SHGFI_SMALLICON);
  Result := FileInfoW.hIcon;
end;

//===============================================

function StartProcess(Target: WideString;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL;
  _dwCreationFlags: Cardinal = NORMAL_PRIORITY_CLASS): TProcessInformation;
var
  StartupInfo: TStartupInfoW;
begin
  // init StatupInfo
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);
    dwFlags := _dwFlags;
    wShowWindow := _wShowWindow;
  end;

  // create process
  Win32Check(Tnt_CreateProcessW(nil, PWideChar(Target), nil, nil, False,
    _dwCreationFlags, nil, nil, StartupInfo, Result));
end;

procedure RunProcessAndWait(Target: WideString; WaitProc: TWaitMethod = nil;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL);
var
  ExitCode: Cardinal;
begin
  ExitCode := RunProcessAndWaitEx(Target, WaitProc, _dwFlags, _wShowWindow);
  if ExitCode <> 0 then
    raise ETntGeneralError.CreateFmt(SProcessFailWithCode, [Target, ExitCode]);
end;

function RunProcessAndWaitEx(Target: WideString; WaitProc: TWaitMethod = nil;
  _dwFlags: DWORD = STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
  _wShowWindow: Word = SW_SHOWNORMAL): Cardinal;
var
  ProcessInfo: TProcessInformation;
begin
  ProcessInfo := StartProcess(Target, _dwFlags, _wShowWindow);

  // wait for process to finish
  if not Assigned(WaitProc) then
    WinCheckFileH(WaitForSingleObject(ProcessInfo.hProcess, INFINITE))
  else begin
    while WinCheckFileH(WaitForSingleObject(ProcessInfo.hProcess, 0)) = WAIT_TIMEOUT do
      WaitProc;
  end;

  // get exit code
  Win32Check(GetExitCodeProcess(ProcessInfo.hProcess, Result));

  // close handles
  CloseHandle(ProcessInfo.hThread);
  CloseHandle(ProcessInfo.hProcess);
end;

procedure RunProcessWithConsoleOutput(CommandLine, CurrentDirectory: WideString; OnOutputLine: TStringEvent);
var
  ExitCode: Cardinal;
begin
  ExitCode := RunProcessWithConsoleOutputEx(CommandLine, CurrentDirectory, OnOutputLine);
  if ExitCode <> 0 then
    raise ETntGeneralError.CreateFmt(SProcessFailWithCode, [CommandLine, ExitCode]);
end;

function RunProcessWithConsoleOutputEx(CommandLine, CurrentDirectory: WideString; OnOutputLine: TStringEvent): Cardinal;

  function _PWideCharWithNil(const S: WideString): PWideChar;
  begin
    if S = '' then
      Result := nil {Win9x needs nil for some parameters instead of empty strings}
    else
      Result := PWideChar(S);
  end;

const
  BUF_LEN = 512;
var
  SA: TSecurityAttributes;
  SI: TStartupInfoW;
  PI: TProcessInformation;
  StdOutPipeRead,
  StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..BUF_LEN] of AnsiChar;
  I,
  BytesRead: Cardinal;
  LastError: Integer;
  Item: WideString;
  Line: WideString;
begin
  ZeroMemory(@SA, SizeOf(SA));
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;

  // create pipe for standard output redirection
  Win32Check(CreatePipe(StdOutPipeRead,  // read handle
             StdOutPipeWrite, // write handle
             @SA,             // security attributes
             BUF_LEN          // number of bytes reserved for pipe - 0 default
             ));
  try
    // Make child process use StdOutPipeWrite as standard out,
    // and make sure it does not show on screen.
    with SI do
    begin
      ZeroMemory(@SI, SizeOf(SI));
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect std input
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;

    // launch the command line compiler
    // Note: The project config file (ProjectName.cfg) must already been written
    //       and must contain search pathes to all files needed. The default Lib path of Delphi
    //       is used, though.
    Win32Check(Tnt_CreateProcessW(nil, PWideChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS,
      nil, _PWideCharWithNil(CurrentDirectory), SI, PI));
    try
      try
        // Now that the handle has been inherited, close it to be safe.
        // We don't want to read or write to it accidentally.
        CloseHandle(StdOutPipeWrite);
        StdOutPipeWrite := 0;

        // get all output until dcc finishes
        Line := '';
        repeat
          // read block of characters (might contain carriage returns and line feeds)
          WasOK := ReadFile(StdOutPipeRead, Buffer, BUF_LEN, BytesRead, nil);
          if (not WasOK) then begin
            LastError := GetLastError;
            if (LastError <> ERROR_BROKEN_PIPE)
            and (LastError <> ERROR_SUCCESS) then
              raise EOSError.CreateFmt(SOSError, [LastError, WideSysErrorMessage(LastError)]);
          end;
          // has anything been read?
          if BytesRead > 0 then
          begin
            // zero terminate buffer
            Buffer[BytesRead] := #0;
            // combine the buffer with the rest of the last run
            Line := Line + Buffer;
            Item := '';
            // parse entire line
            for I := 1 to Length(Line) do
            begin
              // a line feed doesn't mean anything to us
              if Line[I] = LF then
                Continue
              else if Line[I] <> CR then
                Item := Item + Line[I]
              else begin
                OnOutputLine(Item);
                Item := '';
              end;
            end;
            // keep not processed characters until next run
            Line := Item;
          end;
        until (not WasOK) or (BytesRead = 0);

        // take care of any pending output before a CR
        if Item <> '' then
          OnOutputLine(Item);

      except
        Win32Check(TerminateProcess(PI.hProcess, 0));
        raise;
      end;
      // wait for dcc32 to finish (should be already at this point)
      WaitForSingleObject(PI.hProcess, INFINITE);
      Win32Check(GetExitCodeProcess(PI.hProcess, Result));
    finally
      // Close all remaining handles
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    CloseHandle(StdOutPipeRead);
    if StdOutPipeWrite <> 0 then
      CloseHandle(StdOutPipeWrite);
  end;
end;

//======================================== COMPONENT STREAMING

function ComponentToText(Component: TComponent): AnsiString;
var
  BinStream: TTntMemoryStream;
  StrStream: TStringStream{TNT-ALLOW TStringStream};
begin
  BinStream := TTntMemoryStream.Create;
  try
    BinStream.WriteComponent(Component);
    BinStream.Position := 0;
    StrStream := TStringStream{TNT-ALLOW TStringStream}.Create('');
    try
      ObjectBinaryToText(BinStream, StrStream);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

function TextToComponent(const Text: AnsiString): TComponent;
var
  StrStream: TStringStream{TNT-ALLOW TStringStream};
  BinStream: TTntMemoryStream;
begin
  StrStream := TStringStream{TNT-ALLOW TStringStream}.Create(Text);
  try
    BinStream := TTntMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Position := 0;
      Result := BinStream.ReadComponent(nil);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

//======================================== PARSING

function MatchesPattern(const OriginalPattern, Text: WideString): Boolean;
var
  Pattern: WideString;
  LeadingWildCard: Boolean;
  TrailingWildCard: Boolean;
  Stub: WideString;
begin
  Result := False;
  Pattern := OriginalPattern;
  if Pos('*', Pattern) = 0 then begin
    // no wild cards
    if WideSameText(Pattern, Text) then
      Result := True;
  end else begin
    // check pattern: leading wildcard
    LeadingWildCard := False;
    if Pattern[1] = '*' then begin
      LeadingWildCard := True;
      Delete(Pattern, 1, 1);
    end;
    // check pattern: trailing wildcard
    TrailingWildCard := False;
    if Pos('*', Pattern) = Length(Pattern) then begin
      TrailingWildCard := True;
      Delete(Pattern, Length(Pattern), 1);
    end;
    // verify no other wild cards
    if Pos('*', Pattern) > 0 then
      raise ETntGeneralError.CreateFmt('This pattern is not supported: "%s"', [OriginalPattern]);
    // compare pattern
    if LeadingWildCard and TrailingWildCard then begin
      if WideTextPos(Pattern, Text) > 0 then
        Result := True; { pattern is found within name }
    end else if LeadingWildCard then begin
      Stub := Copy(Text, Length(Text) - Length(Pattern) + 1, Length(Pattern));
      if WideSameText(Pattern, Stub) then begin
        Result := True; { pattern is found at end of name }
      end;
    end else if TrailingWildCard then begin
      if WideTextPos(Pattern, Text) = 1 then
        Result := True; {pattern is found at beginning of name }
    end;
  end;
end;

function StringMatchesPatternInList(PatternList: TTntStrings; const S: WideString): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to PatternList.Count - 1 do begin
    if MatchesPattern(PatternList[i], S) then begin
      Result := True;
      break;
    end;
  end;
end;

function GrabTil(var r: WideString; symbol: WideString; part: WideString): WideString;
var
  idx: integer;
begin
  // find symbol, make sure it exists
  idx := Pos(symbol, r);
  if idx = 0 then
    raise ETntInternalError.CreateFmt('Internal Error: Unexpected format, couldn''t find "%s" in '
                       + CRLF + '"%s".'
                       + CRLF + 'This corresponded to part: %s.', [symbol, r, part]);
  // return portion of WideString before symbol
  Result := Copy(r, 1, idx - 1);
  // delete symbol
  Delete(r, 1, idx + Length(symbol) - 1);
end;

function ExtractNumbers(const s: WideString): WideString;
var
  i: integer;
begin
  Result := s;
  for i := Length(Result) downto 1 do
    if not IsWideCharDigit(s[i]) then
      Delete(Result, i, 1);
end;

function StripSpaces(const W: WideString): WideString;
var
  i: integer;
begin
  Result := W;
  for i := Length(W) downto 1 do
    if IsWideCharSpace(W[i]) then
      Delete(Result, i, 1);
end;

function CountWord(Word, Text: WideString): Integer;
var
  FindStr: PWideChar;
  x: Integer;
begin
  Result := 0;
  if (Word <> '') and (Text <> '') then begin
    x := 1;
    while x <= Length(Text) - Length(Word) + 1 do begin
      FindStr := WStrPos(PWideChar(Text) + x - 1, PWideChar(Word));
      if FindStr = nil then
        x := Length(Text) + 1
      else begin
        inc(Result);
        x := PWideChar(FindStr) - PWideChar(Text) + Length(Word) + 1;
      end;
    end;
  end;
end;

function UpdateUniqueNameWord(var Val: WideString; const OldWord, NewWord: WideString): Boolean;
begin
  Result := True;
  if (OldWord <> '')
  and (NewWord <> '')
  and (NewWord <> OldWord) then begin
    if CountWord(OldWord, Val) <> 1 then
      Result := False { old word didn't exist uniquely }
    else
      Val := Tnt_WideStringReplace(Val, OldWord, NewWord, [], True); { there should only be one occurrence. }
  end;
end;

function Tabs(Num: Integer): WideString;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Num do begin
    Result := Result + WideChar(VK_TAB);
  end;
end;

procedure AddSearchWords(SearchWords: TTntStrings; const Words: WideString; AddCompositeWords: Boolean; MaxLen: Integer; _Object: TObject = nil);
var
  i: integer;
  pwc: PWideChar;
  s: WideString;
  all: WideString;
  b: integer;

  procedure DropLeadingZerosIfNumber(var s: WideString);
  begin
    if StrToIntDef(s, 0) <> 0 then
      s := IntToStr(StrToInt(s));
  end;

  procedure Flush;
  begin
    if s <> '' then begin
      s := Tnt_WideUpperCase(s);
      DropLeadingZerosIfNumber(s);
      SearchWords.AddObject(Copy(s, 1, MaxLen), _Object);
      if AddCompositeWords and (Length(all) < MaxLen) then
        all := all + s;
      s := '';
    end;
  end;

begin
  Assert(MaxLen >= 0);
  if MaxLen = 0 then
    exit;

  // pull out all of the search words from Words and put them in SearchWords
  all := '';
  s := '';
  b := 1;
  pwc := nil;
  if Length(Words) > 0 then
    pwc := @Words[1];
  for i := 1 to Length(Words) do begin
    if not IsWideCharAlphaNumeric(pwc^) then begin
      if b <> i then begin
        s := Copy(Words, b, i - b);
        Flush;
      end;
      b := i + 1;
    end;
    inc(pwc);
  end;
  if b <> Length(Words) + 1 then begin
    s := Copy(Words, b, Length(Words) - b + 1);
    Flush;
  end;
  if AddCompositeWords and (all <> '') then begin
    s := all;
    DropLeadingZerosIfNumber(s);
    SearchWords.AddObject(Copy(s, 1, MaxLen), _Object);
  end;
end;

//================================ LOCALE INFO

function GetLocaleString(LocaleID: LCID; LocaleType: Integer): WideString;
begin
  Result := WideGetLocaleStr(LocaleID, LocaleType, UNKNOWN_LOCALE_STR);
end;

function GetAbbrLangName(LocaleID: LCID): WideString;
begin
  Result := GetLocaleString(LocaleID, LOCALE_SABBREVLANGNAME);
end;

function GetNativeLangName(LocaleID: LCID): WideString;
begin
  Result := GetLocaleString(LocaleID, LOCALE_SNATIVELANGNAME);
end;

function GetEngLocaleName(lcd: LCID): WideString;
begin
  Result := GetLocaleString(lcd, LOCALE_SENGLANGUAGE)
   + ' (' + GetLocaleString(lcd, LOCALE_SENGCOUNTRY) + ')';
end;

function IsoCodePage(ACP: Cardinal): WideString;
begin
  case ACP of
    1250: Result := 'ISO 8859-2,  Latin 2 (Central Europe)';
    1251: Result := 'ISO 8859-5,  Cyrillic (Slavic)';
    1252: Result := 'ISO 8859-1,  Latin 1 (ANSI)';
    1253: Result := 'ISO 8859-7,  Greek';
    1254: Result := 'ISO 8859-9,  Latin 5 (Turkish)';
    1255: Result := 'ISO 8859-8,  Hebrew';
    1256: Result := 'ISO 8859-6,  Arabic';
    1257: Result := 'ISO 8859-4,  Scandinavia and Baltic Rim'
    else
      Result := '(Unknown ISO equivalent)';
  end;
end;

//=========================== COM Server Info

function DllPathFromClsID(clsid: TGUID): WideString;
var
  Reg: TTntRegistryLX;
begin
  Result := '';
  Reg := TTntRegistryLX.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKey('CLSID\' + GuidToString(clsid) + '\InprocServer32', False) then begin
      Result := GetExpandedPathFromReg(Reg.ReadString(''));
    end;
  finally
    Reg.Free;
  end;
end;

function DllPathFromProgID(const ProgID: WideString): WideString;
var
  ClassID: TCLSID;
begin
  Result := '';
  if Succeeded(CLSIDFromProgID(PWideChar(ProgID), ClassID)) then
    Result := DllPathFromClsID(ClassID);
end;

function ProgIDVersion(ProgID: WideString): Int64;
var
  DllFileName: WideString;
begin
  Result := 0;
  DllFileName := DllPathFromProgID(ProgID);
  if (DllFileName <> '')
  and WideFileExists(DllFileName) then begin
    Result := GetFullVer(DllFileName);
  end;
end;

//=====================================================
//   Office App Existence

function OfficeLocation(Version, Prog: WideString): WideString;
begin
  Result := '';
  with TTntRegistry.Create do
  try
    Access := KEY_READ;
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(WideFormat('Software\Microsoft\Office\%s\%s\InstallRoot', [Version, Prog]), False)
    and ValueExists('Path') then
      Result := ReadString('Path');
  finally
    Free;
  end;
end;

function GetOfficeAppVersion(Prog, ExeName: WideString): WideString;
var
  Version: Integer;
  VerStr: WideString;
begin
  {9 = Office 2000}
  {10 = Office XP/2002}
  {11 = Office 2003}
  {12 = Office 2007}
  Result := '';
  for Version := 9 {Office 2000} to 20 do begin
    VerStr := IntToStr(Version) + '.0';
    if WideFileExists(OfficeLocation(VerStr, Prog) + ExeName) then
      Result := VerStr;
  end;
end;

resourcestring
  SOfficeInstallIssue = 'There is some problem with the way Office is installed on your machine.';
  SOfficeInstalIssue_AntiVirus = 'The problem may have to do with anti-virus software ("script-blocking" features).';
  SOfficeInstallIssue_Reinstall = 'The problem may be resolved by uninstalling and reinstalling Microsoft Office.';

procedure EnhanceOfficeAutomationError(E: Exception);
begin
  E.Message := E.Message + CRLF + CRLF
    + SOfficeInstallIssue + '  '
    + SOfficeInstalIssue_AntiVirus + '  '
    + SOfficeInstallIssue_Reinstall;
end;

//======================================== SYSTEM INFO

function Win32PlatformStr(X: Integer): WideString;
begin
  case X of
    VER_PLATFORM_WIN32s:
      Result := 'Win32s';
    VER_PLATFORM_WIN32_WINDOWS:
      Result := 'Win9x';
    VER_PLATFORM_WIN32_NT:
      Result := 'NT'
    else
      Result := IntToStr(X);
  end;
end;

function GetSysPlatformStr: WideString;
begin
  Result := WideFormat('%s %d.%d (Build %d) %s',
    [Win32PlatformStr(Win32Platform), Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]);
end;

function IsReallyWindowsXP: Boolean;
var
  Ver: TVSFixedFileInfo;
begin
  Result := Win32PlatformIsXP;
  if not Result then
  try
    Ver := GetVersionFixedFileInfo(GetSystemDir + '\kernel32.dll');
    if Ver.dwFileVersionMS >= Cardinal(MakeLong(1, 5)) then
      Result := True;
  except
  end;
end;

function GetKernelVersion(out ProductName: WideString): Int64;
var
  DllName: WideString;
begin
  Result := 0;
  ProductName := '';
  DllName := GetLibraryFullName('kernel32');
  If DllName <> '' then begin
    Result := GetFullVer(DllName);
    ProductName := GetVersionStringInfo(VER_PRODUCTNAME, DllName);
  end;
end;

function Is_WINE: Boolean;
var
  ProductName: WideString;
  KernelVersion: Int64;
  WINESERVER: WideString;
begin
  KernelVersion := GetKernelVersion(ProductName);
  if KernelVersion <> 0 then
    Result := WideSameText('Wine', ProductName)
  else begin
    WINESERVER := GetEnvironmentVariable('WINESERVER');
    Result := (WINESERVER <> '') and WideFileExists(WINESERVER);
  end;
end;

function WinInetVersion: Int64;
var
  WinInetFileName: WideString;
begin
  Result := 0;
  WinInetFileName := GetLibraryFullName('wininet');
  If WinInetFileName <> '' then begin
    Result := GetFullVer(WinInetFileName);
  end;
end;

function MapiVendor: WideString;
var
  MapiDll: WideString;
begin
  Result := '';
  MapiDll := GetLibraryFullName('mapi32');
  If (MapiDll <> '') then
    Result := GetVersionStringInfo(VER_COMPANYNAME, MapiDll);
end;

function MapiVersion: Int64;
var
  MapiDll: WideString;
begin
  Result := 0;
  MapiDll := GetLibraryFullName('mapi32');
  If MapiDll <> '' then
    Result := GetFullVer(MapiDll);
end;

function CapiComVersion: Int64;
begin
  Result := ProgIDVersion('CAPICOM.EncryptedData');
end;

function MdacVersion: Int64;
begin
  Result := ProgIDVersion('RowPosition.RowPosition');
  if Result = 0 then
    Result := ProgIDVersion('RowPosition.RowPosition.1');
end;

function JetOleDBVersion: Int64;
begin
  Result := ProgIDVersion('Microsoft.Jet.OLEDB.4.0');
end;

function DaoVersion: Int64;
begin
  Result := ProgIDVersion('DAO.TableDef.36');
  if Result = 0 then
    Result := ProgIDVersion('DAO.TableDef');
end;

function SqlDmoVersion: Int64;
begin
  Result := ProgIDVersion('SQLDMO.Application.8.0');
  if Result = 0 then
    Result := ProgIDVersion('SQLDMO.Application');
end;

function SqlMerge9AxVersion: Int64;
begin
  Result := ProgIDVersion('SQLMerge.SQLMerge.3');
  if Result = 0 then
    Result := ProgIDVersion('SQLMerge.SQLMerge');
end;

function DoesSQL7EntMgrExist: Boolean;
var
  Reg: TTntRegistryLX;
  Binn: WideString;
  SemDll: WideString;
begin
  Result := False;
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\MSSQLServer\Setup') then begin
      if Reg.ValueExists('SQLPath') then begin
        Binn := MakePath(Reg.ReadString('SQLPath')) + 'Binn\';
        if WideDirectoryExists(Binn) then begin
          SemDll := Binn + 'semdll.dll';
          if WideFileExists(SemDll)
          and (GetMajorVersion(GetFullVer(SemDll)) = 1999) then
            Result := True;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function Crystal11Version: Int64;
begin
  Result := ProgIDVersion('CrystalRuntime.Report.11');
  if Result = 0 then
    Result := ProgIDVersion('CrystalRuntime.Report');
end;

function RpaWinetVersion(out Vendor: WideString): Int64;
var
  DllName: WideString;
begin
  Result := 0;
  Vendor := '';
  DllName := GetLibraryFullName('Rpawinet');
  If DllName <> '' then begin
    Result := GetFullVer(DllName);
    Vendor := GetVersionStringInfo(VER_COMPANYNAME, DllName);
  end;
end;

//======================================== Email handling
resourcestring
  SDefaultEmailProgram = 'your email program';

function Get_mailto_AppName: WideString;
var
  Reg: TTntRegistryLX;
begin
  // look to "HKEY_LOCAL_MACHINE\"
  Result := '';
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Clients\Mail') then
      Result := Trim(Reg.ReadStringDef('', ''));
  finally
    FreeAndNil(Reg);
  end;
  if Result = '' then
    Result := SDefaultEmailProgram;
end;

procedure Get_mailto_IconHandles(var Large, Small: HICON);
var
  Reg: TTntRegistryLX;
  MailToIconStr: WideString;
  CommaDelimPos: Integer;
  RawPath: WideString;
  ExePath: WideString;
  IconIndexStr: WideString;
  IconIndex: Integer;
begin
  // look to "HKEY_CLASSES_ROOT\mailto\DefaultIcon"
  MailToIconStr := '';
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKeyReadOnly('mailto\DefaultIcon') then
      MailToIconStr := Trim(Reg.ReadStringDef('', ''));
  finally
    FreeAndNil(Reg);
  end;
  if MailToIconStr = '' then begin
    Large := 0;
    Small := 0;
  end else begin
    // get exe path
    CommaDelimPos := WideLastDelimiter(',', MailToIconStr);
    RawPath := WideDequotedStr(Copy(MailtoIconStr, 1, CommaDelimPos - 1), '"');
    ExePath := GetExpandedPathFromReg(RawPath);

    // get IconIndex
    IconIndexStr := Copy(MailtoIconStr, CommaDelimPos + 1, MaxInt);
    IconIndex := StrToIntDef(IconIndexStr, 0);
    // extract icon
    Large := 0;
    Small := 0;
    Tnt_ExtractIconExW(PWideChar(ExePath), IconIndex, Large, Small, 1);
    // if necessary, try default icon
    if (Large = 0) and (Small = 0) and (IconIndex <> 0) then begin
      Tnt_ExtractIconExW(PWideChar(ExePath), 0, Large, Small, 1);
    end;
  end;
end;

function Get_mailto_ShellOpenCommand: WideString;
var
  Reg: TTntRegistryLX;
begin
  // look to "HKEY_CLASSES_ROOT\mailto\shell\open\command"
  Result := '';
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    if Reg.OpenKeyReadOnly('mailto\shell\open\command') then
      Result := Reg.ReadStringDef('', '');
  finally
    FreeAndNil(Reg);
  end;
end;

function Is_mailto_OutlookExpress: Boolean;
begin
  Result := (WideTextPos('\msimn.exe"', Get_mailto_ShellOpenCommand) <> 0);
end;

function Get_mailto_ListSeparator: WideString;
var
  MailToCommand: WideString;
begin
  MailToCommand := Get_mailto_ShellOpenCommand;
  if (WideTextPos('\hmmapi.dll"', MailToCommand) <> 0) { HotMail }
  or (WideTextPos('\msimn.exe"', MailToCommand) <> 0)  { Outlook Express }
  or (WideTextPos('\OUTLOOK.EXE"', MailToCommand) <> 0){ Outlook }
  then
    // see http://support.microsoft.com/kb/q188019/
    Result := ';' // Microsoft seems to like semicolon
  else
    Result := ','; // everybody else seems to like comma
end;

function Make_mailto_Command(const EmailList: WideString; const DefaultDisplayName: WideString): WideString;
begin
  Result := 'mailto:' + NormalizeEmailAddressList(EmailList, DefaultDisplayName,
    False{ No display names for now, too many issues.  Need more documentation on how this can be done for each email client. },
      False, Get_mailto_ListSeparator);
end;

procedure ParseEmailAddress(Value: WideString; const DefaultDisplayName: WideString;
  out EmailAddress, TheDisplayName: WideString);
var
  LeftBracketPos: Integer;
begin
  EmailAddress := '';
  TheDisplayName := '';
  // parse input
  Value := Trim(Value);
  if TntWideLastChar(Value) = '>' then begin
    LeftBracketPos := WideLastDelimiter('<', Value);
    TheDisplayName := Trim(Copy(Value, 1, LeftBracketPos - 1));
    EmailAddress := Trim(Copy(Value, LeftBracketPos + 1, MaxInt));
    if TntWideLastChar(EmailAddress) = '>' then
      Delete(EmailAddress, Length(EmailAddress), 1);
  end else begin
    EmailAddress := Value;
  end;
  EmailAddress := Trim(WideDequotedStr(EmailAddress, '"'));
  TheDisplayName := Trim(WideDequotedStr(TheDisplayName, '"'));
  if (TheDisplayName = '') and (Pos('@', EmailAddress) = 0) then begin
    // it's just a display name
    TheDisplayName := EmailAddress;
    EmailAddress := '';
  end else if (EmailAddress = '') and (Pos('@', TheDisplayName) > 1) and (Pos(' ', TheDisplayName) = 0) then begin
    // it's just an email address
    EmailAddress := TheDisplayName;
    TheDisplayName := '';
  end;
  // check values
  EmailAddress := StripSpaces(EmailAddress);
  EmailAddress := Tnt_WideStringReplace(EmailAddress, '<', '', [rfReplaceAll]);
  EmailAddress := Tnt_WideStringReplace(EmailAddress, '>', '', [rfReplaceAll]);
  if (TheDisplayName = '') and (EmailAddress <> '') then
    TheDisplayName := DefaultDisplayName;
  TheDisplayName := Tnt_WideStringReplace(TheDisplayName, '"', '''', [rfReplaceAll]);
  TheDisplayName := Tnt_WideStringReplace(TheDisplayName, '<', '{', [rfReplaceAll]);
  TheDisplayName := Tnt_WideStringReplace(TheDisplayName, '>', '}', [rfReplaceAll]);
end;

function NormalizeEmailAddress(Value: WideString; const DefaultDisplayName: WideString; IncludeDisplayName: Boolean; AllowDisplayNameOnly: Boolean): WideString;

  function FixDisplayName(const ADisplayName: WideString): WideString;
  begin
    Result := ADisplayName;
    Result := Tnt_WideStringReplace(Result, CRLF, ', ', [rfReplaceAll]);
    Result := Tnt_WideStringReplace(Result, CR, ', ', [rfReplaceAll]);
    Result := Tnt_WideStringReplace(Result, LF, ', ', [rfReplaceAll]);
  end;

var
  EmailAddress: WideString;
  TheDisplayName: WideString;
begin
  ParseEmailAddress(Value, DefaultDisplayName, EmailAddress, TheDisplayName);
  // compile result
  if (not IncludeDisplayName) or (TheDisplayName = '') then
    Result := EmailAddress
  else if (EmailAddress <> '') or (AllowDisplayNameOnly) then
    Result := '"' + FixDisplayName(TheDisplayName) + '" <' + EmailAddress + '>'
  else
    Result := '';
end;

procedure BuildNormalizedEmailList(sList: TTntStrings; CommaText: WideString;
  const DefaultDisplayName: WideString; IncludeDisplayNames, AllowDisplayNamesOnly: Boolean);

    function FindEmailDelimiterPos(CommaText: WideString): Integer;
    var
      i: integer;
      InQuote: Boolean;
      PreCommaStr: WideString;
    begin
      Result := 0;
      InQuote := False;
      for i := 1 to Length(CommaText) do begin
        if CommaText[i] = '"' then begin
          // quote char
          InQuote := not InQuote;
        end else if (not InQuote) then begin
          // not in a quoted str
          if (CommaText[i] in [WideChar(','), WideChar(';')]) then begin
            // found a delimiter
            PreCommaStr := Trim(Copy(CommaText, 1, i - 1));
            if (PreCommaStr = '')
            or ((TntWideLastChar(PreCommaStr) = '>') and (WideLastDelimiter('<', PreCommaStr) > WideLastDelimiter('"', PreCommaStr)))
            or (WideLastDelimiter('@', PreCommaStr) > WideLastDelimiter('"', PreCommaStr)) then
            begin
              // delimiter looks valid
              Result := i;
              break; { found it! }
            end;
          end;
        end;
      end;
    end;

var
  CommaPos: Integer;
  EmailAddress: WideString;
begin
  CommaText := Trim(CommaText);
  // prepare delimiters
  if (Pos('<', CommaText) = 0) or (CountWord('<', CommaText) <> CountWord('>', CommaText)) then begin
    // no display names, so we can deal with spaces more aggressively
    if (CountWord('@', CommaText) > 1) then
      // multiple addresses ie. "abc@test.com test@test.com"
      CommaText := Tnt_WideStringReplace(CommaText, ' ', ',', [rfReplaceAll]);
  end else begin
    // display names, so delimit after each email address
    CommaText := Tnt_WideStringReplace(CommaText, '>', '>,', [rfReplaceAll]);
    if CommaText[Length(CommaText)] = ',' then
      Delete(CommaText, Length(CommaText), 1);
  end;
  // populate list
  sList.Clear;
  repeat
    CommaPos := FindEmailDelimiterPos(CommaText);
    if CommaPos > 0 then
      EmailAddress := NormalizeEmailAddress(Copy(CommaText, 1, CommaPos - 1),
        DefaultDisplayName, IncludeDisplayNames, AllowDisplayNamesOnly)
    else
      EmailAddress := NormalizeEmailAddress(CommaText,
        DefaultDisplayName, IncludeDisplayNames, AllowDisplayNamesOnly);
    if (EmailAddress <> '')
    and (sList.IndexOf(EmailAddress) = -1) then
      sList.Add(EmailAddress);
    Delete(CommaText, 1, CommaPos);
  until (CommaPos = 0);
end;

function GetEmailCommaText(sList: TTntStrings; const Delimiter: WideString): WideString;
var
  i: integer;
begin
  Result := '';
  for i := 0 to sList.Count - 1 do begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + sList[i];
  end;
end;

function NormalizeEmailAddressList(const Value: WideString;
  const DefaultDisplayName: WideString; IncludeDisplayNames, AllowDisplayNamesOnly: Boolean;
    EmailSeparator: WideString = ', '): WideString;
var
  sList: TTntStringList;
begin
  Result := Trim(Value);
  if Result <> '' then begin
    // parse
    sList := TTntStringList.Create;
    try
      BuildNormalizedEmailList(sList, Result, DefaultDisplayName, IncludeDisplayNames, AllowDisplayNamesOnly);
      Result := GetEmailCommaText(sList, EmailSeparator);
    finally
      FreeAndNil(sList);
    end;
  end;
end;

initialization
  GarbageCollectedFolderList := TTntStringList.Create;
  GarbageCollectedFileList := TTntStringList.Create;

  OfficeAppVersion[oaWord]    := GetOfficeAppVersion('Word', 'WINWORD.EXE');
  OfficeAppVersion[oaExcel]   := GetOfficeAppVersion('Excel', 'EXCEL.EXE');
  OfficeAppVersion[oaOutlook] := GetOfficeAppVersion('Outlook', 'OUTLOOK.EXE');
  OfficeAppVersion[oaAccess]  := GetOfficeAppVersion('Access', 'MSACCESS.EXE');

  Office2000AppExists[oaWord]    := OfficeAppVersion[oaWord] <> '';
  Office2000AppExists[oaExcel]   := OfficeAppVersion[oaExcel] <> '';
  Office2000AppExists[oaOutlook] := OfficeAppVersion[oaOutlook] <> '';
  Office2000AppExists[oaAccess]  := OfficeAppVersion[oaAccess] <> '';

finalization
  DeleteGarbageCollectedFolders;
  DeleteGarbageCollectedFiles;

  FreeAndNil(GarbageCollectedFolderList);
  FreeAndNil(GarbageCollectedFileList);

end.
