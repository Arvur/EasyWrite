
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXVerUtils;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows;

// string info
function GetVersionLanguageID(const FileName: WideString): LANGID;
function GetVersionStringInfo(const StringName, FileName: WideString; Language: Word = 0; CodePage: Word = 0): WideString;

// fixed file info
function GetVersionFixedFileInfo(const FileName: WideString): TVSFixedFileInfo;

// full ver  ($0001 0002 0003 0004)
type
  TVerStrStyle = (vsFull {'1.2.3.4'}, vsRelease{'1.2.3'}, vsReleaseUser{'1.2 r3'}, vsApp{'1.2'});

function GetFullVer(const FileName: WideString): Int64;
function FullVerToStr(const FullVer: Int64; Style: TVerStrStyle = vsFull): WideString;
function StrToFullVer(S: WideString): Int64;
function EncodeFullVer(Major, Minor, Release, Build: Word): Int64;
procedure DecodeFullVer(const FullVer: Int64; var Major, Minor, Release, Build: Word);

// release ver ($0001 0002 0003 [0000]}
function GetReleaseVer(const FullVer: Int64): Int64; overload;
function GetReleaseVer(const FileName: WideString): Int64; overload;

// app ver ($0001 0002)
function GetAppVer(const FileName: WideString): Cardinal; overload;
function GetAppVer(const FullVer: Int64): Cardinal; overload;
function GetAppVer(const Major, Minor: Word): Cardinal; overload;
function AppVerToStr(const AppVer: Cardinal): WideString; {'1.2'}
function GetMajorVersion(const FullVer: Int64): Word;

function MakeDbVer(Hi, Lo: Word): Cardinal;

implementation

uses
  SysUtils, TntSysUtils, TntWindows, TntLXUtils;

function GetVerBlock(const FileName: WideString): Pointer;
var
  Dummy: DWord;
  SizePBLock: DWord;
  pVerBlock: Pointer;
begin
  ForceFileExists(FileName);
  Result := nil;
  SizePBlock := Tnt_GetFileVersionInfoSizeW(PWideChar(FileName), Dummy);
  if SizePBlock > 0 then begin
    // file has version info
    pVerBlock := AllocMem(SizePBlock);
    if (pVerBlock <> nil) then begin
      // allocated pVerBlock
      if Tnt_GetFileVersionInfoW(PWideChar(FileName), 0, SizePBlock, pVerBlock) then begin
        // success
        Result := pVerBlock
      end else begin
        // failure
        FreeMem(pVerBlock);
      end;
    end;
  end;
end;

//======================== STRING INFO

type
  TLangCodePage = packed record
    wLanguage: Word;
    wCodePage: Word;
  end;
  PLangCodePage = ^TLangCodePage;

function GetFirstVersionLangCodePage(const FileName: WideString): TLangCodePage;
var
  pVerBlock: Pointer;
  pLangCP: PLangCodePage;
  cbLangCP: Cardinal;
begin
  ZeroMemory(@Result, SizeOf(Result));
  pVerBlock := GetVerBlock(FileName);
  if pVerBlock <> nil then
  try
    if Tnt_VerQueryValueW(pVerBlock, VQV_VARFILEINFO_TRANSLATION, Pointer(pLangCP), cbLangCP)
    and (cbLangCP >= SizeOf(TLangCodePage)) then
      Result := pLangCP^; { grab first one (Delphi only adds one, but in theory there could be more) }
  finally
    FreeMem(pVerBlock);
  end;
end;

function GetVersionLanguageID(const FileName: WideString): LANGID;
begin
  Result := GetFirstVersionLangCodePage(FileName).wLanguage;
end;

function GetVersionStringInfo(const StringName, FileName: WideString;
  Language: Word = 0; CodePage: Word = 0): WideString;
var
  pVerBlock: Pointer;
  cbLangCP: Cardinal;
  SubBlock: WideString;
  PointerResult: Pointer;
begin
  Result := '';
  pVerBlock := GetVerBlock(FileName);
  if pVerBlock <> nil then {pVerBlock is filled}
  try
    // ensure Language / CodePage
    if (Language = 0) or (CodePage = 0) then begin
      with GetFirstVersionLangCodePage(FileName) do begin
        Language := wLanguage;
        CodePage := wCodePage;
      end;
    end;
    // figure SubBlock
    SubBlock := WideFormat('%s\%.4x%.4x\%s', [VQV_STRINGFILEINFO, Language, CodePage, StringName]);
    // get Value
    if Tnt_VerQueryValueW(pVerBlock, PWideChar(SubBlock), PointerResult, cbLangCP) then begin
      SetString(Result, PWideChar(PointerResult), cbLangCP);
      Result := PWideChar(Result);
    end
  finally
    FreeMem(pVerBlock);
  end;
end;

//======================== FILE VERSION INFO
function GetVersionFixedFileInfo(const FileName: WideString): TVSFixedFileInfo;
var
  SizeStruct: DWord;
  pVerBlock: Pointer;
  PFFInfo: PVSFIxedFileInfo;
begin
  ZeroMemory(@Result, SizeOf(Result));
  pVerBlock := GetVerBlock(FileName);
  if pVerBlock <> nil then
  try
     if Tnt_VerQueryValueW(pVerBlock, VQV_FIXEDFILEINFO, Pointer(PFFInfo), SizeStruct) then
       Result := PFFInfo^;
  finally
    FreeMem(pVerBlock);
  end;
end;

function GetFullVer(const FileName: WideString): Int64;    // $0001000200030004
begin
  with GetVersionFixedFileInfo(FileName) do begin
    Int64Rec(Result).Hi := dwFileVersionMS;
    Int64Rec(Result).Lo := dwFileVersionLS;
  end;
end;

function FullVerToStr(const FullVer: Int64; Style: TVerStrStyle = vsFull): WideString;
begin
  case Style of
    vsFull:
      Result := WideFormat('%d.%d.%d.%d', [HiWord(Int64Rec(FullVer).Hi),
                                           LoWord(Int64Rec(FullVer).Hi),
                                           HiWord(Int64Rec(FullVer).Lo),
                                           LoWord(Int64Rec(FullVer).Lo)]);
    vsRelease:
      Result := WideFormat('%d.%d.%d', [HiWord(Int64Rec(FullVer).Hi),
                                        LoWord(Int64Rec(FullVer).Hi),
                                        HiWord(Int64Rec(FullVer).Lo)]);

    vsReleaseUser:
      Result := WideFormat('%d.%d r%d', [HiWord(Int64Rec(FullVer).Hi),
                                         LoWord(Int64Rec(FullVer).Hi),
                                         HiWord(Int64Rec(FullVer).Lo)]);
    vsApp:
      Result := WideFormat('%d.%d', [HiWord(Int64Rec(FullVer).Hi),
                                     LoWord(Int64Rec(FullVer).Hi)]);
  end;
end;

function StrToFullVer(S: WideString): Int64;

  function PullNextInt(PeriodPos: Integer): Word;
  begin
    Result := StrToIntDef(TrimRight(Copy(S, 1, PeriodPos - 1)), 0);
    Delete(S, 1, PeriodPos);
    S := TrimLeft(S);
  end;

var
  PeriodPos: Integer;
  A, B, C, D: Word;
begin
  B := 0;
  C := 0;
  D := 0;
  S := Trim(S);
  PeriodPos := Pos('.', S);
  if PeriodPos = 0 then
    A := StrToIntDef(S, 0)
  else begin
    A := PullNextInt(PeriodPos);
    PeriodPos := Pos('r', S);  {vsReleaseUser}
    if PeriodPos = 0 then
      PeriodPos := Pos('.', S);
    if PeriodPos = 0 then
      B := StrToIntDef(S, 0)
    else begin
      B := PullNextInt(PeriodPos);
      PeriodPos := Pos('.', S);
      if PeriodPos = 0 then
        C := StrToIntDef(S, 0)
      else begin
        C := PullNextInt(PeriodPos);
        if S <> '' then
          D := StrToIntDef(S, 0);
      end;
    end;
  end;
  Result := EncodeFullVer(A, B, C, D);
end;

function EncodeFullVer(Major, Minor, Release, Build: Word): Int64;
begin
  Result := (Int64(Major) shl 48)
          + (Int64(Minor) shl 32)
               + (Release shl 16)
                  + Build;
end;

procedure DecodeFullVer(const FullVer: Int64; var Major, Minor, Release, Build: Word);
var
  Temp: Int64;
begin
  Temp := (FullVer and $000000000000FFFF);
  Build := Temp;
  Temp := (FullVer and $00000000FFFF0000) shr 16;
  Release := Temp;
  Temp := (FullVer and $0000FFFF00000000) shr 32;
  Minor := Temp;
  Temp := (FullVer and $FFFF000000000000) shr 48;
  Major := Temp;
end;

function GetReleaseVer(const FullVer: Int64): Int64;
begin
  Result := FullVer and $FFFFFFFFFFFF0000;
end;

function GetReleaseVer(const FileName: WideString): Int64;
begin
  Result := GetReleaseVer(GetFullVer(FileName));
end;

function GetAppVer(const FileName: WideString): Cardinal;
begin
  Result := GetVersionFixedFileInfo(FileName).dwFileVersionMS;
end;

function GetAppVer(const FullVer: Int64): Cardinal;
begin
  Result := Int64Rec(FullVer).Hi;
end;

function GetAppVer(const Major, Minor: Word): Cardinal;
begin
  Result := (Major shl 16) or Minor {Major.Minor}
end;

function AppVerToStr(const AppVer: Cardinal): WideString;
begin
  Result := WideFormat('%d.%d', [HiWord(AppVer),
                                 LoWord(AppVer)]);
end;

function GetMajorVersion(const FullVer: Int64): Word;
var
  Release, Minor, Build: Word;
begin
  DecodeFullVer(FullVer, Result, Minor, Release, Build);
end;

function MakeDbVer(Hi, Lo: Word): Cardinal;
begin
  result := MakeLong(Lo, Hi);
end;

end.


