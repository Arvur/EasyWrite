{ ****************************************************************** }
{                                                                    }
{   VCL component TTellTail : Delphi 5                               }
{                                                                    }
{   One Application Instance Component with Command Tail Transfer    }
{                                                                    }
{   Code generated by Component Create for Delphi                    }
{                                                                    }
{   Copyright � 2001 Small Software Systems LLC                      }
{                                                                    }
{   TTellTail Version 1.010 February 1, 2001                         }
{                                                                    }
{ ****************************************************************** }
{ Note on Comments:  The blocks of '//' comments are the most        }
{ informative on the component implementation and usage.  Also see   }
{ the included ReadMe.txt file.                                      }
{                                                                    }
{ The blocks of '//' comments tell you how the component actually    }
{ works and/or provide tips for using same.  See the comments on the }
{ Execute method for a description of the basic scheme.              }
{                                                                    }
{ Note on Code:  The method bodies declared Public are listed first  }
{ in the source code so private or protected helper functions are    }
{ implemented further down.                                          }
{                                                                    }
{ Update: Dec. 1, 1999:                                              }
{                                                                    }
{ Added code to automatically create GUID-based names for the Kernel }
{ Objects when dropped on a form(thanks Jeff Overcash.)              }
{                                                                    }
{ Note that if you create the component dynamically then you must    }
{ assign names to the Kernel Objects(the Mutex and Memory Mapped     }
{ File.)  In Delphi IDE Cntrl-Shift-g creates a GUID.                }
{                                                                    }
{ Update: Dec. 3, 1999:                                              }
{                                                                    }
{ Added Event type and code to copy application-defined shared data  }
{ in addition to the command tail and working directory.  See the    }
{ comments for TSharedDataEvent for details.                         }
{                                                                    }
{ Update: Feb. 3, 2001:  version 1.010                               }
{                                                                    }
{ Changed Execute to overloaded Boolean Function.  May be called     }
{ with or without Kernel Object Names.  Also cleaned up some code    }
{ in the function itself so it no longer sets Enabled to False.      }
{ Cleaned up some logical errors of program flow when using events.  }
{ ****************************************************************** }

Unit TellTail;


Interface

Uses Windows, Messages, SysUtils, Classes, Controls, Forms;

Const
  MaxTail = 511;
  WaitTimedOut : DWORD = $20000001; // GetLastError user errors set bit 29
  WaitAbandoned : DWORD = $20000002;
  InvalidMutexHandle : DWORD = $20000003;
  InvalidKernelObjName : DWORD = $20000004;
  RunWhenNotEnabled : DWORD = $20000005;

Type

  // TSharedDataEvent
  //
  // This Event facilitates application-defined data sharing
  // between the main and secondary instances of the application
  // in addition to the command tail and working directory automatically
  // shared by the TellTail component.  Assign event handlers to the
  // OnWriteSharedData and OnReadSharedData events to transfer additional data.
  //
  // The OnWriteSharedData handler must assign the address of the data to
  // the DataPtr param and assign the requested number of bytes to copy
  // to the DataSize param.  The value param MaxDataSize will have the
  // maximum number of bytes you can transfer.  This is calculated by
  // subtracting the size of SharedMemRecord used to transfer the command tail
  // and working directory from the size of a Page of memory in the swapfile
  // on the host system.  I call GetSystemInfo to find out the page size before
  // I create the memory mapped file.  The minimum you can map in the swapfile
  // is one page of ram so if it turns out you are on an Alpha doing this
  // calculation will allow your program to use the additional 4K.
  //
  // The OnReadSharedData handler must assign the address of a buffer to
  // receive the data to DataPtr, the number of bytes to read to DataSize,
  // and likewise the MaxDataSize can be read as a guide to how much can
  // be read from the additional data portion of the memory mapped file.
  //
  // Needless to say, the number of bytes read or written will not be
  // greater than MaxDataSize.
  //
  // I'm using this Event scheme where I actually copy the additional
  // data inside the component in an attempt to relieve the programmer
  // using my component of the burden of avoiding access violations when
  // manipulating additional shared data.  Of course you have the source,
  // so you can adapt the basic scheme here to some specialized implementation
  // of your own.  The included demo will have a lame example of using these
  // events.  After all, I have to try it out anyway before posting the
  // component anywhere! :)
  //
  TSharedDataEvent = Procedure(Sender : TObject; Var DataPtr : Pointer;
    Var DataSize : Cardinal; MaxDataSize : Cardinal) Of Object;

  TTellTail = Class(TComponent)
  Private
      { Private fields of TTellTail }
        { Storage for property Enabled }
    FEnabled : Boolean;
        { Storage for property MapFileName }
    FMapFileName : String;
        { Storage for property RestoreMainInstance }
    FRestoreMainInstance : Boolean;
        { Storage for property MutexName }
    FMutexName : String;
        { Storage for property MutexTimeOut }
    FMutexTimeOut : LongInt;
        { Link for OnMapFileFailure Event }
    FMapFileFailure : TNotifyEvent;
        { Link for OnMutexFailure Event }
    FMutexFailure : TNotifyEvent;
        { Link for OnWriteSharedData Event }
    FWriteSharedData : TSharedDataEvent;
        { Link for OnReadSharedData Event }
    FReadSharedData : TSharedDataEvent;
      { Private methods of TTellTail }
        { Method to set variable and property values and create objects }
    Procedure AutoInitialize;
        { Method to free any objects created by AutoInitialize }
    Procedure AutoDestroy;

  Protected
      { Protected fields of TTellTail }
        { Command Tail from secondary instance }
    CommandTail : Array[0..MaxTail] Of Char;
        { Working Dir from secondary instance }
    WorkingDir : Array[0..MaxTail] Of Char;
        { True if currently the Main App Instance }
    MainInstance : Boolean;
        { Handle to Memory Mapped File }
    MapFileHandle : THandle;
        { Start of Memory Map Shared Data }
    MappedPagePtr : Pointer;
        { Handle to the Mutex }
    MutexHandle : THandle;
        { Error Code returned by GetLastError }
    ErrorCode : DWORD;
        { Size of a Page in the Swapfile }
    PageSize : DWORD;
        { System Info structure }
    SysInfo : SYSTEM_INFO;
        { Security Attributes for Mutex }
    SecAttr : SECURITY_ATTRIBUTES;

      { Protected methods of TTellTail }
      { function to create kernel objects }
    Function CreateKernelObjects : Boolean;
      { function to get the Mutex for map file access }
    Function GetMutex : Boolean;
      { procedure to surrender the Mutex }
    Procedure SurrenderMutex;
      { PrePCommandTail rebuilds command tail .. more comments on func body. }
    Function PrepCommandTail : String;
      { WriteAdditonalData invoked if OnWriteSharedData handler assigned }
    Procedure WriteAdditionalData(P : Pointer; Size : Cardinal);
      { ReadAdditonalData invoked if OnReadSharedData handler assigned }
    Procedure ReadAdditionalData(P : Pointer; Size : Cardinal);

  Public
      { Public fields and properties of TTellTail }

      { Public methods of TTellTail }
    Constructor Create(AOwner : TComponent); Override;
      { Allow Dynamic construction with Kernel Object Names }
    Constructor CreateWithNames(NameForMapFile, NameForMutex : String;
      AOwner : TComponent);
    Destructor Destroy; Override;
      { if returns False, check ErrorCode for cause }
    Function Execute : Boolean; Overload;
    Function Execute(NameForMapFile, NameForMutex : String) : Boolean; Overload;
      { returns ErrorCode value }
    Function GetErrorCode : DWORD;
      { True if we in the Main Instance }
    Function InMainInstance : Boolean;
      { True if shared data copied in Main Instance }
    Function CopyData : Boolean;
      { return Command Tail to caller }
    Function GetCommandTail : String;
      { return WorkingDir to caller - long filename format }
    Function GetWorkingDir : String;
      { calculate MaxDataSize for TSharedDataEvent handlers }
    Function GetMaxDataSize : Cardinal;
      { Reset ErrorCode to 0, MainInstance to False }
    Procedure Reset;
  Published
      { Published properties of TTellTail }
        { Indicates error if set to False }
    Property Enabled : Boolean Read FEnabled Write FEnabled Default True;
        { Memory Mapped File Name (use GUID) }
    Property MapFileName : String Read FMapFileName Write FMapFileName;
        { if True, send restore msg to main instance }
    Property RestoreMainInstance : Boolean
      Read FRestoreMainInstance Write FRestoreMainInstance
      Default True;
        { Name of Mutex Kernel Obj (use GUID) }
    Property MutexName : String Read FMutexName Write FMutexName;
        { Time to wait for Mutex to be signalled }
    Property MutexTimeOut : LongInt Read FMutexTimeOut Write FMutexTimeOut
      Default 250;
        { TTellTail Component Name }
    Property Name;
        { Event fired on Memory Mapped File Failure }
    Property OnMapFileFailure : TNotifyEvent Read FMapFileFailure Write FMapFileFailure;
        { Event fired on Mutex Failure }
    Property OnMutexFailure : TNotifyEvent Read FMutexFailure Write FMutexFailure;
        { Event fired to write additional shared data from secondary instance }
    Property OnWriteSharedData : TSharedDataEvent Read FWriteSharedData
      Write FWriteSharedData;
        { Event fired to read additional shared data in main instance }
    Property OnReadSharedData : TSharedDataEvent Read FReadSharedData
      Write FReadSharedData;
  End;

  TSharedMemRecord = Record
    MainInstanceHandle : THandle;
    HasData : LongBool;
    CommandTail : Array[0..MaxTail] Of Char;
    WorkingDir : Array[0..MaxTail] Of Char;
  End; // SharedMemRecord

  PSharedMemRecord = ^TSharedMemRecord;


Procedure Register;

Implementation

Uses ActiveX, ComObj;

Procedure Register;
Begin
     { Register TTellTail with Freeware as its
       default page on the Delphi component palette }
  RegisterComponents('Freeware', [TTellTail]);
End;

// ******* Methods of TTellTail declared as Public *******

Constructor TTellTail.Create(AOwner : TComponent);
Begin
     { Call the Create method of the parent class }
  Inherited Create(AOwner);

     { AutoInitialize sets the initial values of variables and      }
     { properties; also, it creates objects for properties of       }
     { standard Delphi object types (e.g., TFont, TTimer,           }
     { TPicture) and for any variables marked as objects.           }
     { AutoInitialize method is generated by Component Create.      }
  AutoInitialize;

     { Code to perform other tasks when the component is created }

End; // Create

// Allow Dynamic construction with Kernel Object Names
//

Constructor TTellTail.CreateWithNames(NameForMapFile, NameForMutex : String;
  AOwner : TComponent);
Begin
  Inherited Create(AOwner);
  FMapFileName := NameForMapFile;
  FMutexName := NameForMutex;
  AutoInitialize;
End;

Destructor TTellTail.Destroy;
Begin
     { AutoDestroy, which is generated by Component Create, frees any   }
     { objects created by AutoInitialize.                               }
  AutoDestroy;

     { Here, free any other dynamic objects that the component methods  }
     { created but have not yet freed.  Also perform any other clean-up }
     { operations needed before the component is destroyed.             }

  If MappedPagePtr <> Nil Then
    UnMapViewOfFile(MappedPagePtr);
  If MutexHandle <> 0 Then
    CloseHandle(MutexHandle);
  If MapFileHandle <> 0 Then
    CloseHandle(MapFileHandle);
     { Last, free the component by calling the Destroy method of the    }
     { parent class.                                                    }
  Inherited Destroy;
End; // Destroy

// Execute Method - a long comment to be sure but this serves as
// most of the documentation how to use TTellTail component.
//
// If Execute returns True, you are in the main instance.
// On success in 2nd app instance it should not return, but
// terminate the 2nd app after transfering the info to shared
// memory.
//
// Execute should be called on program startup.
// (A good place being inside the main form OnCreate routine.)
// It uses a Memory Mapped File page in the system swapfile
// to determine if we are in the Main Instance or secondary
// instance of the Application and acts accordingly, almost like
// using a fork() in unix.  If in the Main Instance, the page of
// the system swap file is mapped and the handle to the app
// copied there.  This handle is used by the secondary instance to
// restore the Main Instance window if it happens to be minimized.
// If in the secondary instance, it opens the shared page and copies
// the command tail(the command line of the secondary instance without
// ParamStr(0) since we already know the exe name) and startup or
// working directory of the secondary instance using a Record structure.
// Access to the shared page is protected by a Named Mutex.
//
// There is a LongBool in the record that is set to True when data is
// copied to the shared page in the Map File, and reset to False when
// the data is copied out in the Main Instance(using LongBool instead
// of a one byte Boolean should avoid alignment issues.)
//
// The Main Instance of your application should set up some means
// to periodically call the CopyData method of TellTail which returns
// True if data was retrieved.  This can be done with a TTimer or
// by assigning a handler to the Application.OnIdle event of the
// Main Instance.  The latter technique is used in the included demo.

Function TTellTail.Execute : Boolean;
Begin
  Result := Execute('', '');
End;

Function TTellTail.Execute(NameForMapFile, NameForMutex : String) : Boolean;
Var
  SharedRecPtr : PSharedMemRecord;
  MainHandle : THandle;
  tail  : PChar;
  P     : Pointer;
  S     : Cardinal;

Begin
  Result := False;
  Reset;
  If Not Enabled Then
  Begin
    ErrorCode := RunWhenNotEnabled;
    Exit;
  End;
  If NameForMapFile <> '' Then
    FMapFileName := NameForMapFile;
  If NameForMutex <> '' Then
    FMutexName := NameForMutex;
  tail := PChar(PrepCommandTail);
  If Not CreateKernelObjects Then
    Exit;
  SharedRecPtr := MappedPagePtr;
  If Not GetMutex Then
    Exit;
  If InMainInstance Then
  Begin
    ZeroMemory(MappedPagePtr, PageSize);
    SharedRecPtr^.MainInstanceHandle := Application.Handle;
    SurrenderMutex;
    Result := True;
  End // if MainInstance
  Else
  Begin
    With SharedRecPtr^ Do
    Begin
      If tail <> Nil Then
        StrLCopy(CommandTail, tail, MaxTail)
      Else
        CommandTail[0] := Char(0);
      StrPLCopy(WorkingDir, GetCurrentDir, MaxTail);
      HasData := True;
      If Assigned(FWriteSharedData) Then
      Begin
        OnWriteSharedData(Self, P, S, GetMaxDataSize);
        WriteAdditionalData(P, S);
      End; // if Assigned
      MainHandle := MainInstanceHandle;
    End; // with
    SurrenderMutex;
    If RestoreMainInstance And IsIconic(MainHandle) Then
    Begin
      SetForegroundWindow(MainHandle);
      If Not PostMessage(MainHandle, WM_SYSCOMMAND, SC_RESTORE, 0) Then
        PostMessage(MainHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
    End; // if RestoreMainInstance
    Application.Terminate;
  End; // else if not MainInstance pass data and quit
End; // Execute

// GetErrorCode Method : returns value of DWORD ErrorCode.
//
// The member variable ErrorCode can contain an error code
// returned by Win32Api GetLastError or a user-defined code.
// For this reason the const error codes I define set bit 29
// as stipulated in the Win32Api docs, and are all greater
// than $20000000( or 1 shl 29.)

Function TTellTail.GetErrorCode : DWORD;
Begin
  Result := ErrorCode;
End; // GetErrorCode

// CopyData Method : returns True if shared data retrieved.
// To be called in Main Instance of the application.
//

Function TTellTail.CopyData : Boolean;
Var
  RecPtr : PSharedMemRecord;
  P     : Pointer;
  S     : Cardinal;

Begin
  Result := False;
  If Not Enabled Then
  Begin
    ErrorCode := RunWhenNotEnabled;
    Exit;
  End;
  If Not InMainInstance Then
    exit;
  RecPtr := MappedPagePtr;
  If Not GetMutex Then
    exit;
  If Not RecPtr^.HasData Then
    SurrenderMutex
  Else
  Begin
    StrLCopy(CommandTail, RecPtr^.CommandTail, MaxTail);
    StrLCopy(WorkingDir, RecPtr^.WorkingDir, MaxTail);
    If Assigned(FReadSharedData) Then
    Begin
      OnReadSharedData(Self, P, S, GetMaxDataSize);
      ReadAdditionalData(P, S);
    End; // if Assigned
    RecPtr^.HasData := False;
    SurrenderMutex;
    If CommandTail = '' Then
      CommandTail := '(empty)';
    If WorkingDir = '' Then
      WorkingDir := '(unknown)';
    Result := True;
  End; // else
End; // CopyData

// InMainInstance Method : returns True if called from Main Instance
// of the application.
//

Function TTellTail.InMainInstance : Boolean;
Begin
  Result := MainInstance;
End; // InMainInstance

// GetCommandTail Method : returns command tail of secondary instance.
// To be called in Main Instance of the application after successful call
// of CopyData.
//

Function TTellTail.GetCommandTail : String;
Begin
  Result := String(CommandTail);
End; // GetCommandTail

// GetWorkingDir Method : returns working directory of secondary instance.
// To be called in Main Instance of the application after successful call
// of CopyData.
//

Function TTellTail.GetWorkingDir : String;
Begin
  Result := String(WorkingDir);
End; // GetWorkingDir

Function TTellTail.GetMaxDataSize : Cardinal;
Begin
  Result := Cardinal(PageSize) - Cardinal(sizeof(TSharedMemRecord));
End; // GetMaxDataSize

// Reset ErrorCode to 0, MainInstance to False
//

Procedure TTellTail.Reset;
Begin
  ErrorCode := 0;
  MainInstance := False;
End;

// ******* Methods of TTellTail declared as Protected *******

// CreateKernelObject Method : returns True if Map file and Mutex accessible.
// When called from Main Instance it actually creates the kernel objects,
// when called from the secondary instance it just attempts to open them.
//
// Uses OnMapFileFailure and OnMutexFailure event handlers if assigned, or
// returns False.
//

Function TTellTail.CreateKernelObjects : Boolean;
Begin
  Result := False;
  ErrorCode := 0;
  If Not Enabled Then
  Begin
    ErrorCode := RunWhenNotEnabled;
    exit;
  End;
  If MapFileName = '' Then
  Begin
    ErrorCode := InvalidKernelObjName;
    If Assigned(FMapFileFailure) Then
      OnMapFileFailure(Self);
    Exit;
  End; // if MapFileName
  If MutexName = '' Then
  Begin
    ErrorCode := InvalidKernelObjName;
    If Assigned(FMutexFailure) Then
      OnMutexFailure(Self);
    Exit;
  End; // if MutexName
  SetLastError(0);
  SecAttr.nLength := sizeof(SecAttr);
  SecAttr.lpSecurityDescriptor := Nil;
  SecAttr.bInheritHandle := BOOL(True);
  GetSystemInfo(SysInfo);
  PageSize := SysInfo.dwPageSize;
  MapFileHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, BOOL(False), PChar(MapFileName));
  If MapFileHandle <> 0 Then
  Begin
    MainInstance := False;
    MutexHandle := OpenMutex(MUTEX_ALL_ACCESS, BOOL(True), PChar(MutexName));
    If MutexHandle = 0 Then
    Begin
      ErrorCode := GetLastError;
      If Assigned(FMutexFailure) Then
        OnMutexFailure(Self);
      Exit;
    End;
  End
  Else
  Begin
    MapFileHandle := CreateFileMapping(THandle($FFFFFFFF), Nil, PAGE_READWRITE,
      0, PageSize, PChar(MapFileName));
    If MapFileHandle = 0 Then
    Begin
      ErrorCode := GetLastError;
      If Assigned(FMapFileFailure) Then
        OnMapFileFailure(Self);
      Exit;
    End;
    MainInstance := True;
    MutexHandle := CreateMutex(@SecAttr, BOOL(True), PChar(MutexName));
    If MutexHandle = 0 Then
    Begin
      ErrorCode := GetLastError;
      If Assigned(FMutexFailure) Then
        OnMutexFailure(Self);
      Exit;
    End;
  End; // else
  MappedPagePtr := MapViewOfFile(MapFileHandle, FILE_MAP_WRITE, 0, 0, PageSize);
  If MappedPagePtr = Nil Then
  Begin
    ErrorCode := GetLastError;
    SurrenderMutex;
    If Assigned(FMapFileFailure) Then
      OnMapFileFailure(Self);
    Exit;
  End; // If MappedPagePtr = Nil
  SurrenderMutex;
  Result := True;
End; // CreateKernelObjects

// GetMutex Method : returns True if successfully obtains the Mutex.
// On failure call GetErrorCode method for an idea of the cause.
//

Function TTellTail.GetMutex : Boolean;
Var
  WaitVal : DWORD;

Begin
  Result := False;
  SetLastError(0);
  WaitVal := WaitForSingleObject(MutexHandle, DWORD(MutexTimeOut));

  Case WaitVal Of
    WAIT_OBJECT_0 :
      Begin
        Result := True;
        ErrorCode := 0;
      End;
    WAIT_TIMEOUT : ErrorCode := WaitTimedOut; // result is False
    WAIT_ABANDONED :
      Begin
        Result := True;
        ErrorCode := WaitAbandoned;
      End;
    WAIT_FAILED : ErrorCode := GetLastError; // result is False
  End; // case
End; // GetMutex

// SurrenderMutex Method : Attempts to release the Mutex.
// Call GetErrorCode method to obtain result of the operation.
//

Procedure TTellTail.SurrenderMutex;
Begin
  ErrorCode := 0;
  SetLastError(0);
  If Not ReleaseMutex(MutexHandle) Then
    ErrorCode := GetLastError;
End; // SurrenderMutex

// PrePCommandTail Method : returns reformatted command tail string.
//
// (Helper function to prepare command tail for copying to shared memory.)
//
// Note:
// ParamStr(x) removes quotes : this function produces a command tail
// with quotes intact but without ParamStr(0).. the path to the second
// instance is in WorkingDir in long filename format.  I do this loop
// because the CmdLine variable may have the short filename format if
// the app was launched from a Dos box and that's a pain!
//

Function TTellTail.PrepCommandTail : String;
Var
  s     : String;
  x     : Integer;

Begin
  s := '';
  For x := 1 To ParamCount Do
    If Pos(' ', Paramstr(x)) <> 0 Then
      s := s + '"' + Paramstr(x) + '"' + ' '
    Else
      s := s + Paramstr(x) + ' ';
  x := Length(s);
  If (x > 0) And (s[x] = ' ') Then
    setLength(s, x - 1);
  Result := s;
End; // PrepCommandTail


Procedure TTellTail.WriteAdditionalData(P : Pointer; Size : Cardinal);
Var
  n     : Integer;
  Dest  : Pointer;

Begin
  n := Integer(MappedPagePtr);
  n := n + sizeof(TSharedMemRecord);
  Dest := Ptr(n);
  n := Integer(GetMaxDataSize);
  If Integer(Size) < n Then
    n := Integer(Size);
  CopyMemory(Dest, P, DWORD(n));
End; // WriteAdditionalData

Procedure TTellTail.ReadAdditionalData(P : Pointer; Size : Cardinal);
Var
  n     : Integer;
  Source : Pointer;

Begin
  n := Integer(MappedPagePtr);
  n := n + sizeof(TSharedMemRecord);
  Source := Ptr(n);
  n := Integer(GetMaxDataSize);
  If Integer(Size) < n Then
    n := Integer(Size);
  CopyMemory(P, Source, DWORD(n));
End; // ReadAdditionalData

// ******* Methods of TTellTail declared as Private *******

{ Method to set variable and property values and create objects }

Procedure TTellTail.AutoInitialize;
Var
  MyGuid : TGUID;

Begin
  If (csDesigning In ComponentState) Then
  Begin
    If FMapFileName = '' Then
      If CoCreateGuid(MyGuid) = S_OK Then
        FMapFileName := GuidToString(MyGuid);
    If FMutexName = '' Then
      If CoCreateGuid(MyGuid) = S_OK Then
        FMutexName := GuidToString(MyGuid);
  End; // if csDesigning
  CommandTail := '(empty)';
  WorkingDir := '(unknown)';
  MapFileHandle := INVALID_HANDLE_VALUE;
  MappedPagePtr := Nil;
  MutexHandle := INVALID_HANDLE_VALUE;
  PageSize := 4096;
  Enabled := True;
  RestoreMainInstance := True;
  MainInstance := False;
  MutexTimeOut := 250;
End; { of AutoInitialize }

{ Method to free any objects created by AutoInitialize }

Procedure TTellTail.AutoDestroy;
Begin
 { Nothing from AutoInitialize to destroy }
End; { of AutoDestroy }

End.