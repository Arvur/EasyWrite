
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXSetupUtils;

{$INCLUDE TntCompilers.inc}

interface

uses
  SysUtils, TntClasses, Windows;

{$WARN SYMBOL_PLATFORM OFF}

procedure RegisterFileAssociationEx(const FileExt, DestExe, Description, Icon, OpenCommandStr: WideString);
procedure RegisterFileAssociation(const FileExt: WideString; const DestExe: WideString);
procedure UnregisterFileAssociation(const FileExt: WideString; const DestExe: WideString);
procedure MakeShortcut(const DestExe: WideString; const LinkFileName: WideString);

procedure CreateProgramGroupShortcut(Common: Boolean; const Group: WideString; const Exe: WideString);
procedure DeleteProgramGroupShortcut(Common: Boolean; const Group: WideString; const Exe: WideString);

procedure CreateDesktopShortcut(Common: Boolean; const Exe: WideString);
procedure DeleteDesktopShortcut(Common: Boolean; const Exe: WideString);

procedure InstallScreenSaver(const TempScr: WideString);
procedure DisplayScreenSaverPreview;
procedure TntExitWindows(Flags : Word = EWX_REBOOT);
procedure DeleteApplicationWhenDone;

procedure RegisterDll(const DllName: WideString);
procedure UnregisterDll(const DllName: WideString);

procedure RegisterExe(const ExeName: WideString);
procedure UnregisterExe(const ExeName: WideString);

const
  BorlandLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize

implementation

uses
  Forms, CommCtrl, ShlObj, ActiveX, ComObj, Dialogs,
  TntSysUtils, TntWindows, TntLXUtils, TntLXVerUtils, TntLXRegistry, TntLXVclUtils;

resourcestring
  SDocument = 'Document';

procedure RegisterFileAssociationEx(const FileExt, DestExe, Description, Icon, OpenCommandStr: WideString);
var
  Reg: TTntRegistryLX;
  FType: WideString;
begin
  Force(Pos('.', FileExt) = 0, 'FileExt can not contain a period.');
  Force(Pos('/', FileExt) = 0, 'FileExt can not contain a slash.');
  Force(Pos(' ', FileExt) = 0, 'FileExt can not contain a space.');
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;

    // register file type
    Reg.GotoKey('.' + FileExt, True);
    FType := FileExt + 'file'; { do not localize }
    Reg.WriteString('', FType);

    // setup file type
    Reg.GotoKey(FType, True);
    Reg.WriteString('', Description);

    // setup icon
    Reg.Descend('DefaultIcon', True);
    try
      Reg.WriteString('', DestExe + ',' + Icon);
    finally
      Reg.Ascend;
    end;

    // setup shell open command
    Reg.Descend('shell', True);
    Reg.Descend('open', True);
    Reg.Descend('command', True);
    Reg.WriteString('', WideFormat(OpenCommandStr, [DestExe, '%1']));
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure RegisterFileAssociation(const FileExt: WideString; const DestExe: WideString);
begin
  RegisterFileAssociationEx(FileExt, DestExe,
    ExtractFileRoot(DestExe) + ' ' + SDocument, '1', '"%0:s" "%1:s"');
end;

procedure UnregisterFileAssociation(const FileExt: WideString; const DestExe: WideString);
var
  Reg: TTntRegistryLX;
  FType: WideString;
begin
  Force(Pos('.', FileExt) = 0, 'FileExt can not contain a period.');
  Force(Pos('/', FileExt) = 0, 'FileExt can not contain a slash.');
  Force(Pos(' ', FileExt) = 0, 'FileExt can not contain a space.');
  Reg := TTntRegistryLX.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    // unregister file type
    Reg.DeleteKey('.' + FileExt);
    FType := FileExt + 'file'; { do not localize }
    Reg.DeleteKey(FType);
  finally
    Reg.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure MakeShortcut(const DestExe: WideString; const LinkFileName: WideString);
var
  Link: IShellLinkW;
  Link_Ansi: IShellLinkA;
  F: IPersistFile;
begin
  if Win32PlatformIsUnicode then begin
    Link := CreateComObject(CLSID_ShellLink) as IShellLinkW;
    Link.SetPath(PWideChar(DestExe));
    Link.SetWorkingDirectory(PWideChar(WideExtractFilePath(DestExe)));
    F := Link as IPersistFile;
  end else begin
    Link_Ansi := CreateComObject(CLSID_ShellLink) as IShellLinkA;
    Link_Ansi.SetPath(PAnsiChar(AnsiString(DestExe)));
    Link_Ansi.SetWorkingDirectory(PAnsiChar(AnsiString(WideExtractFilePath(DestExe))));
    F := Link_Ansi as IPersistFile;
  end;
  F.Save(PWideChar(LinkFileName), False);
end;

procedure CreateProgramGroupShortcut(Common: Boolean; const Group: WideString; const Exe: WideString);
var
  GroupDir: WideString;
  NewGroupDir: Boolean;
  Win95Shell: Boolean;
begin
  if Common then
    GroupDir := GetCommonStartMenuProgramsDir + '\' + Group
  else
    GroupDir := GetStartMenuProgramsDir + '\' + Group;
  NewGroupDir := not WideDirectoryExists(GroupDir);
  TntForceDirectories(GroupDir);
  MakeShortcut(Exe, GroupDir + '\' + ExtractFileRoot(Exe) + '.lnk');
  Win95Shell := not CheckWin32Version(4, 1); // I suppose I really should check for Shell32.dll or something like that to recognize IE4 w/ desktop/shell update
  if NewGroupDir and Win95Shell then
   	ShellOpenFile(GroupDir); // give the user a chance to copy/paste icons to desktop, etc.
end;

procedure DeleteProgramGroupShortcut(Common: Boolean; const Group: WideString; const Exe: WideString);
var
  GroupDir: WideString;
begin
  if Common then
    GroupDir := GetCommonStartMenuProgramsDir + '\' + Group
  else
    GroupDir := GetStartMenuProgramsDir + '\' + Group;
  ForceDeleteFileIfExists(GroupDir + '\' + ExtractFileRoot(Exe) + '.lnk');
  // try to delete folder if possible
  WideRemoveDir(GroupDir);
end;

procedure CreateDesktopShortcut(Common: Boolean; const Exe: WideString);
var
  DesktopDir: WideString;
begin
  if Common then
    DesktopDir := GetCommonDesktopDir
  else
    DesktopDir := GetDesktopDir;
  MakeShortcut(Exe, DesktopDir + '\' + ExtractFileRoot(Exe) + '.lnk');
end;

procedure DeleteDesktopShortcut(Common: Boolean; const Exe: WideString);
var
  DesktopDir: WideString;
begin
  if Common then
    DesktopDir := GetCommonDesktopDir
  else
    DesktopDir := GetDesktopDir;
  ForceDeleteFileIfExists(DesktopDir + '\' + ExtractFileRoot(Exe) + '.lnk');
end;

// =================================================================================
//  Screen Saver Install Utils

function DeskCpl_Is_Bad: Boolean;
var
  DeskCplFileName: WideString;
  IsWin95: Boolean;
  DeskCplVersion: Int64;
begin
  Result := True;
  IsWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS)
         and (Win32MajorVersion = 4)
         and (Win32MinorVersion = 0);  { Windows 95 }

  // Check Desk.Cpl, Windows 95/IE4 combo is bad
  DeskCplFileName := GetLibraryFullName('desk.cpl');
  if DeskCplFileName <> '' then begin
    DeskCplVersion := GetFullVer(DeskCplFileName);
    Result := IsWin95
         and (    (DeskCplVersion = EncodeFullVer(4, 72, 3110, 0))
               or (DeskCplVersion = EncodeFullVer(4, 71, 1712, 0))   );
                  { 4.71.1712.0 or 4.72.3110.0 comes with IE4 }
  end;

  if not Result { not bad } then begin
    // NT 4.0 w/ less than IE 4.0 is bad
    Result :=  (Win32Platform = VER_PLATFORM_WIN32_NT)
           and (Win32MajorVersion = 4)
           and (Win32MinorVersion = 0)  { Windows NT 4.0 }
           and (GetAppVer(WinInetVersion) < GetAppVer(4, 72)) { less than IE 4.01 (4.72) }
  end;
end;

procedure Show_ScreenSaver_Tab;
var
  DefaultPath: WideString;
  DPWnd: HWND;
  TabCtrlWnd: HWND;
  start: TDateTime;
begin
  try
    DefaultPath := MakePath(GetWindowsDir);
    if not WideFileExists(DefaultPath + 'Control.exe') then
      DefaultPath := MakePath(GetSystemDir);
    ShellOpenFile('Control.exe', 'desk.cpl', DefaultPath, SW_SHOW);
    DPWnd := 0;
    start := Now;
    While (DPWnd = 0) do begin
      if (Now - start) > (2 * SYS_SECOND) then
        exit;
      DPWnd := FindWindow('#32770', 'Display Properties');
      if DPWnd = 0 then
        Sleep(250);
    end;
    if DPWnd <> 0 then begin
      TabCtrlWnd := FindWindowEx(DPWnd, 0, 'SysTabControl32', '');
      if TabCtrlWnd <> 0 then begin
        PostMessage(TabCtrlWnd, TCM_SETCURFOCUS, 1, 0);
      end;
    end;
  except
    // oh well!
  end;
end;

procedure InstallScreenSaver(const TempScr: WideString);
var
  Scr: WideString;
  ShortName: AnsiString;
begin
  // copy into windows directory
  Scr := MakePath(GetWindowsDir) + WideExtractFileName(TempScr);
  ForceCopyFile(TempScr, Scr);

  // select as default
  ShortName := WideExtractShortPathName(Scr);
  WritePrivateProfileString('boot', 'SCRNSAVE.EXE', PAnsiChar(ShortName), 'system.ini');

  // let system know we've changed (via WM_WININICHANGE)
  SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, Cardinal(TRUE), nil, Cardinal(TRUE));
end;

procedure DisplayScreenSaverPreview;
var
  ShortName: AnsiString;
  Scr: WideString;
begin
  // Get ShortName (scr)
  SetLength(ShortName, MAX_PATH);
  GetPrivateProfileString('boot', 'SCRNSAVE.EXE', '', PAnsiChar(ShortName), MAX_PATH, 'system.ini');
  ShortName := PAnsiChar(ShortName);
  // Get Scr name
  Scr := WideExpandFileName(ShortName);

  if (Scr = '')
  or (DeskCpl_Is_Bad) then
    Show_ScreenSaver_Tab
  else begin
    try
      ShellOpenFile('rundll32.exe', 'desk.cpl,InstallScreenSaver ' + Scr, GetWindowsDir, SW_SHOWNORMAL);
    except
      Show_ScreenSaver_Tab;
    end;
  end;
end;

procedure TntExitWindows(Flags : Word = EWX_REBOOT);
var
  iToken: THandle;
  iPriveleg: TTokenPrivileges;
  dum1: TTokenPrivileges;
  dum2: Cardinal;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    // prepare a TTokenPrivileges record
    ZeroMemory(@iPriveleg, SizeOf(iPriveleg));
    { Get the LUID for shutdown privilege. }
    Win32Check(LookupPrivilegeValue(nil, 'SeShutdownPrivilege', iPriveleg.Privileges[0].Luid));
    iPriveleg.PrivilegeCount := 1; { One privilege to set }
    iPriveleg.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

    // Get a token for this process, adjust privileges
    Win32Check(OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, iToken));
    ZeroMemory(@dum1, SizeOf(dum1));
    dum2 := 0;
    Win32Check(AdjustTokenPrivileges(iToken, False, iPriveleg, SizeOf(dum1), dum1, dum2));
  end;
  Win32Check(ExitWindowsEx(Flags, 0));
end;

procedure DeleteApplicationWhenDone;
var
  pi: PROCESS_INFORMATION;
  BatchFileName: WideString;
  sList: TTntStringList;
begin
  { The code was originally based on Jeffrey Richter's Jan 1996 MSJ article.
    Create a batch file that continuously attempts to delete our executable
    file.  When the executable no longer exists, remove its containing
    subdirectory, and then delete the batch file too. }

  // create the batch file
  BatchFileName := GetTempFileWithExt('.bat');
  sList := TTntStringList.Create;
  sList.Add(':Repeat');
  sList.Add(WideFormat('del "%s"', [WideExtractShortPathName(GetExeFileName)]));
  sList.Add(WideFormat('if exist "%s" goto Repeat', [WideExtractShortPathName(GetExeFileName)]));
  sList.Add(WideFormat('rmdir "%s"', [WideExtractShortPathName(WideExtractFilePath(GetExeFileName))]));
  sList.Add(WideFormat('del "%s"', [BatchFileName]));
  sList.AnsiStrings.SaveToFile(BatchFileName);

  // Get ready to spawn the batch file we just created.
  // We want its console window to be invisible to the user.
  // Spawn the batch file with low-priority and suspended.
  pi := StartProcess(BatchFileName, STARTF_USESHOWWINDOW, SW_HIDE,
    CREATE_SUSPENDED or IDLE_PRIORITY_CLASS);

  // Lower the batch file's priority even more.
  SetThreadPriority(pi.hThread, THREAD_PRIORITY_IDLE);

  // Raise our priority so that we terminate as quickly as possible.
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

  // Allow the batch file to run and clean-up our handles.
  CloseHandle(pi.hProcess);
  ResumeThread(pi.hThread);
  CloseHandle(pi.hThread);

  { We want to terminate right away now so that we can be deleted. }
end;

procedure RegisterDll(const DllName: WideString);
var
  DllHandle: THandle;
  RegProc: TDLLRegisterServer;
begin
  DllHandle := WinCheckH(Tnt_LoadLibraryW(PWideChar(DllName)));
  try
    RegProc := WinCheckP(GetProcAddress(DllHandle, 'DllRegisterServer'));
    try
      OleCheck(RegProc);
    except
      on E: Exception do begin
        E.Message := E.Message + CRLF
                   + CRLF + WideFormat('Error occurred while registering %s.', [DllName]);
        raise;
      end;
    end;
  finally
    FreeLibrary(DllHandle);
  end;
end;

procedure UnregisterDll(const DllName: WideString);
var
  DllHandle: THandle;
  RegProc: TDLLUnregisterServer;
begin
  DllHandle := WinCheckH(Tnt_LoadLibraryW(PWideChar(DllName)));
  try
    RegProc := WinCheckP(GetProcAddress(DllHandle, 'DllUnregisterServer'));
    try
      OleCheck(RegProc);
    except
      on E: Exception do begin
        E.Message := E.Message + CRLF
                   + CRLF + WideFormat('Error occurred while unregistering %s.', [DllName]);
        raise;
      end;
    end;
  finally
    FreeLibrary(DllHandle);
  end;
end;

procedure RegisterExe(const ExeName: WideString);
begin
  RunProcessAndWait(ExeName + ' /RegServer');
end;

procedure UnregisterExe(const ExeName: WideString);
begin
  RunProcessAndWait(ExeName + ' /UnregServer');
end;

end.
