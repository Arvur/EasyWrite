
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXSafeWinInet;

{$INCLUDE TntCompilers.inc}

interface

uses
  Controls, Windows, WinInet;

var
  SInternetConnectionDisabled: WideString;

// Internet "Is Allowed" functions
function InternetIsAllowed: Boolean;
procedure BeginAllowingInternet_BasedOnReg(const RegKey, RegValue: WideString);

// safe shell "open" integration
procedure SafeOpenURL(Url: WideString);
procedure SemiSafeOpenURL(Url: WideString);

// work offline procs
function IsGlobalWorkOffline: Boolean;
procedure SetGlobalWorkOffline(fGoOffline: BOOL);
function PromptToGoOnline(const URL: WideString): Boolean;
function IsURLAvailableOffline(const Url: WideString): BOOL;
function GetOfflineHandCursor: TCursor;

// connectivity
function InternetIsConnected(Quiet: Boolean): Boolean;
function ConfigureAdvancedConnectionSettings: Boolean;

implementation

uses
  Dialogs, SysUtils, TntSysUtils, TntDialogs, TntLXRegistry, TntLXVclUtils,
  UrlMon, ShlObj, Forms, TntWindows, TntLXFetchUrl, TntLXSafeWinInet_AdvancedDlg,
  TntLXSafeWinInet_ConnectionDlg;

//==================== "SAFE": application can globally disable internet connection ===========
resourcestring
  SDefaultInternetConnectionDisabled = 'Connection to Internet is currently not allowed.';
  SThisActionWillCauseWebBrowserConnection = 'This action will cause your Web browser to connect to the Internet.';
  SProceedConfirmation = 'Do you want to proceed?';

var
  GSaved_RegKey: WideString = '';
  GInternetIsAllowed: Boolean = False;
  // troubleshooting settings
  GAutoPrime_Enable: Boolean = False;
  GAutoPrime_MaxTrialCount: Integer = 0;
  GAutoPrime_URL: WideString = '';
  GAutoPrime_PostDelay: Integer = 0;
  GAlwaysReport_InternetAsConnected: Boolean = False;


function InternetIsAllowed: Boolean;
begin
  Result := GInternetIsAllowed;
end;

procedure BeginAllowingInternet_BasedOnReg(const RegKey, RegValue: WideString);
begin
  GSaved_RegKey := RegKey;
  if (RegKey = '') then
    GInternetIsAllowed := True
  else if RegValueExists(HKEY_CURRENT_USER, RegKey, RegValue) then
    GInternetIsAllowed := RegReadBoolean(HKEY_CURRENT_USER, RegKey, RegValue)
  else if RegValueExists(HKEY_LOCAL_MACHINE, RegKey, RegValue) then
    GInternetIsAllowed := RegReadBoolean(HKEY_LOCAL_MACHINE, RegKey, RegValue)
  else
    GInternetIsAllowed := True;
  //--
  GAutoPrime_Enable := RegReadBooleanDef(HKEY_CURRENT_USER, RegKey, 'AutoPrime_Enable', False);
  GAutoPrime_MaxTrialCount := RegReadIntegerDef(HKEY_CURRENT_USER, RegKey, 'AutoPrime_MaxTrialCount', 0);
  GAutoPrime_URL := RegReadWideStringDef(HKEY_CURRENT_USER, RegKey, 'AutoPrime_URL', '');
  GAutoPrime_PostDelay := RegReadIntegerDef(HKEY_CURRENT_USER, RegKey, 'AutoPrime_PostDelay', 0);
  GAlwaysReport_InternetAsConnected := RegReadBooleanDef(HKEY_CURRENT_USER, RegKey, 'AlwaysReport_InternetAsConnected', False);
end;

procedure SaveAdvancedOptions;
begin
  if (GSaved_RegKey <> '') then
  begin
    RegWriteBoolean(HKEY_CURRENT_USER, GSaved_RegKey, 'AutoPrime_Enable', GAutoPrime_Enable);
    RegWriteInteger(HKEY_CURRENT_USER, GSaved_RegKey, 'AutoPrime_MaxTrialCount', GAutoPrime_MaxTrialCount);
    RegWriteWideString(HKEY_CURRENT_USER, GSaved_RegKey, 'AutoPrime_URL', GAutoPrime_URL);
    RegWriteInteger(HKEY_CURRENT_USER, GSaved_RegKey, 'AutoPrime_PostDelay', GAutoPrime_PostDelay);
    RegWriteBoolean(HKEY_CURRENT_USER, GSaved_RegKey, 'AlwaysReport_InternetAsConnected', GAlwaysReport_InternetAsConnected);
  end;
end;

procedure SafeOpenURL(Url: WideString);
begin
//  if InternetIsAllowed then
    ShellOpenFile(Url)
//  else
//    raise ETntGeneralError.Create(SInternetConnectionDisabled);
end;

procedure SemiSafeOpenURL(Url: WideString);
begin
  if (not InternetIsAllowed)
  and (WideTextPos('http', Url) = 1)
  and (WideMessageDlg(SThisActionWillCauseWebBrowserConnection
    + CRLF + SProceedConfirmation, mtConfirmation, mbYesNoCancel, -1) <> mrYes)
  then
    Abort;
  ShellOpenFile(Url)
end;

//--------------------------- GLOBAL WORK OFFLINE procs ---------------------------------------

// Returns true if the global state is offline. Otherwise, false.
function IsGlobalWorkOffline: Boolean;
var
  dwState: DWORD;
  dwSize: DWORD;
begin
  dwState := 0;
  dwSize := sizeof(DWORD);
  Result := False;
  if InternetQueryOption(nil, INTERNET_OPTION_CONNECTED_STATE, @dwState, dwSize) then
    if (dwState and INTERNET_STATE_DISCONNECTED_BY_USER) <> 0 then
      Result := True;
end;

procedure SetGlobalWorkOffline(fGoOffline: BOOL);
var
  ci: INTERNET_CONNECTED_INFO;
begin
  ZeroMemory(@ci, SizeOf(ci));
  if fGoOffline then begin
    ci.dwConnectedState := INTERNET_STATE_DISCONNECTED_BY_USER;
    ci.dwFlags := ISO_FORCE_DISCONNECTED;
  end else begin
    ci.dwConnectedState := INTERNET_STATE_CONNECTED;
  end;
  InternetSetOption(nil, INTERNET_OPTION_CONNECTED_STATE, @ci, sizeof(ci));
end;

function PromptToGoOnline(const URL: WideString): Boolean;
begin
  {$IFDEF COMPILER_9_UP}
  InternetGoOnline(PAnsiChar(AnsiString(URL)), Application.ActiveFormHandle, 0);
  {$ELSE}
  InternetGoOnline(PAnsiChar(AnsiString(URL)), Application.Handle, 0);
  {$ENDIF}
  Result := not IsGlobalWorkOffline;
end;

function IsURLAvailableOffline(const Url: WideString): BOOL;
var
  hr: HRESULT;
  dwUsesNet, dwCached: DWORD;
  dwSize: DWORD;
begin
  if (Url = '') or (not IsGlobalWorkOffline) then
    Result := True
  else begin
    // First, let URL monikers check the protocol scheme.
    hr := CoInternetQueryInfo(PWideChar(Url), QUERY_USES_NETWORK, 0,
      @dwUsesNet, sizeof(dwUsesNet), dwSize, 0);
    if FAILED(hr) or (dwUsesNet = 0) then
      Result := True { doesn't even use network }
    else begin
      // Then let URL monikers peek in the cache.
      hr := CoInternetQueryInfo(PWideChar(Url), QUERY_IS_CACHED_OR_MAPPED, 0,
        @dwCached, sizeof(dwCached), dwSize, 0);
      if FAILED(hr) then
        Result := False
      else
        Result := (dwCached <> 0);
    end;
  end;
end;

var
  crOfflineHandCursor: Integer = 0;
  hShDocVwDll: HMODULE = 0;

function GetOfflineHandCursor: TCursor;
var
  hCurs: HCURSOR;
begin
  if (crOfflineHandCursor = 0) and (hShDocVwDll = 0) then
  begin
    hShDocVwDll := Tnt_LoadLibraryW('shdocvw.dll');
    if (hShDocVwDll <> 0) then begin
      hCurs := LoadCursor(hShDocVwDll, MAKEINTRESOURCE(IDC_OFFLINE_HAND));
      crOfflineHandCursor := -4000; { somewhat arbitrary starting point }
      while Screen.Cursors[crOfflineHandCursor] <> Screen.Cursors[crDefault] do
        Dec(crOfflineHandCursor);
      Screen.Cursors[crOfflineHandCursor] := hCurs;
    end;
  end;
  if crOfflineHandCursor = 0 then
    crOfflineHandCursor := crNo; // closest thing
  Result := crOfflineHandCursor;
end;

//----------------------- Internet Connectivity procs -----------------------------------------

var
  _ReportInternetAsConnected: Boolean = False;

function Original_InternetIsConnected(Quiet: Boolean): Boolean;

  procedure AutoPrimeWinHttp(const URL: WideString);
  var
    FetchUrl: TTntFetchUrl;
  begin
    _ReportInternetAsConnected := True;
    try
      FetchUrl := TTntFetchUrl.Create(nil);
      try
        FetchUrl.Quiet := True;
        FetchUrl.FetchUrl(URL, '', '', '');
      finally
        FetchUrl.Free;
      end;
    finally
      _ReportInternetAsConnected := False;
    end;
  end;

var
  Flags: DWord;
  i: integer;
begin
  if (not InternetIsAllowed) then
    // not allowed
    Result := False
  else if IsGlobalWorkOffline then
    // working offline
    Result := False
  else if _ReportInternetAsConnected then
    // diagnostic mode
    Result := True
  else begin
    // detect connection
    Flags := 0;
    Result := InternetGetConnectedState(@Flags, 0);
    if (not Result) and (not Quiet) then begin
      // auto-dial
      InternetAttemptConnect(0);
      // auto-prime wininet (optional)
      if GAutoPrime_Enable and (GAutoPrime_URL <> '') then begin
        for i := 1 to GAutoPrime_MaxTrialCount do begin
          AutoPrimeWinHttp(GAutoPrime_URL);
          if GAutoPrime_PostDelay > 0 then
            Sleep(GAutoPrime_PostDelay);
          // re-detect connection
          Flags := 0;
          Result := InternetGetConnectedState(@Flags, 0);
          if Result then
            break; { connection established! }
        end;
      end;
      // re-detect connection
      Flags := 0;
      Result := InternetGetConnectedState(@Flags, 0) or GAlwaysReport_InternetAsConnected;
    end;
  end;
end;

function InternetIsConnected(Quiet: Boolean): Boolean;
var
  Retry: Boolean;
begin
  Result := False;
  while True do begin
    Result := Original_InternetIsConnected(Quiet);
    if Result or Quiet then
      break
    else begin
      ReportInternetConnectionIssue(Retry);
      if (not Retry) then
        break;
    end;
  end;
end;

function ConfigureAdvancedConnectionSettings: Boolean;
begin
  Result := _ConfigureAdvancedConnectionSettings(GAutoPrime_Enable, GAutoPrime_MaxTrialCount,
    GAutoPrime_URL, GAutoPrime_PostDelay, GAlwaysReport_InternetAsConnected);
  if Result then
    SaveAdvancedOptions;
end;

initialization
  SInternetConnectionDisabled := SDefaultInternetConnectionDisabled;

finalization
  if (hShDocVwDll <> 0) then
    FreeLibrary(hShDocVwDll);

end.






