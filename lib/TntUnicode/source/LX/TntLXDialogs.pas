
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXDialogs;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Dialogs, TntDialogs;

type
  TTntOpenDialogLX = class(TTntOpenDialog)
  private
    FExplicit_ParentWnd: HWND;
  protected
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  public
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
  end;

  TTntSaveDialogLX = class(TTntSaveDialog)
  private
    FExplicit_ParentWnd: HWND;
  protected
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  public
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
  end;

function WidePromptForFileNameLX(var AFileName: WideString; const AFilter: WideString = '';
  const ADefaultExt: WideString = ''; const ATitle: WideString = '';
  const AInitialDir: WideString = ''; SaveDialog: Boolean = False): Boolean;

implementation

uses
  Consts, Controls, CommDlg, TntSysUtils, TntLXUtils, Forms;

function WidePromptForFileNameLX(var AFileName: WideString; const AFilter: WideString = '';
  const ADefaultExt: WideString = ''; const ATitle: WideString = '';
  const AInitialDir: WideString = ''; SaveDialog: Boolean = False): Boolean;
var
  Dialog: TTntOpenDialog;
begin
  if SaveDialog then
  begin
    Dialog := TTntSaveDialogLX.Create(nil);
    Dialog.Options := Dialog.Options + [ofOverwritePrompt];
  end
  else
    Dialog := TTntOpenDialogLX.Create(nil);
  with Dialog do
  try
    Title := ATitle;
    DefaultExt := ADefaultExt;
    if AFilter = '' then
      Filter := SDefaultFilter else
      Filter := AFilter;
    InitialDir := AInitialDir;
    FileName := AFileName;
    Result := Execute;
    if Result then
      AFileName := FileName;
  finally
    Free;
  end;
end;

function VistaHackDesired(Dialog: TOpenDialog{TNT-ALLOW TOpenDialog}): Boolean;
begin
  Result := Win32PlatformIsVista
            and (not (ofOldStyleDialog in Dialog.Options))
            and NewStyleControls
            // events - need hook
            and (not Assigned(Dialog.OnClose))
            and (not Assigned(Dialog.OnShow))
            and (not Assigned(Dialog.OnCanClose))
            and (not Assigned(Dialog.OnFolderChange))
            and (not Assigned(Dialog.OnSelectionChange))
            and (not Assigned(Dialog.OnTypeChange))
            and (not Assigned(Dialog.OnIncludeItem));
end;

{$IFDEF COMPILER_10_UP}
procedure OpenDialog_BeforeExecute(Dialog: TOpenDialog{TNT-ALLOW TOpenDialog}; var SaveAppModalPopupMode: TPopupMode);
begin
  SaveAppModalPopupMode := Application.ModalPopupMode;
  if VistaHackDesired(Dialog) then
    Application.ModalPopupMode := pmNone; // avoid redirector window
end;

procedure OpenDialog_AfterExecute(Dialog: TOpenDialog{TNT-ALLOW TOpenDialog}; SaveAppModalPopupMode: TPopupMode);
begin
  Application.ModalPopupMode := SaveAppModalPopupMode;
end;
{$ENDIF}

procedure OpenDialog_BeforeTaskModalDialog(Dialog: TOpenDialog{TNT-ALLOW TOpenDialog}; FExplicit_ParentWnd: HWND; var DialogData);
var
  OpenFileName: POpenFileName;
begin
  OpenFileName := @DialogData;
  if VistaHackDesired(Dialog) then
  begin
    {$IFDEF COMPILER_9}
    SendMessage(OpenFileName.hWndOwner, CM_RELEASE, 0, 0); // free redirector
    {$ENDIF}
    // so it centers correctly
    if FExplicit_ParentWnd <> 0 then
      OpenFileName.hWndOwner := FExplicit_ParentWnd
    else begin
      {$IFDEF COMPILER_9_UP}
      OpenFileName.hWndOwner := Application.ActiveFormHandle;
      {$ELSE}
      if Screen.ActiveForm <> nil then
        OpenFileName.hWndOwner := Screen.ActiveForm.Handle
      else
        OpenFileName.hWndOwner := Application.Handle;
      {$ENDIF}
    end;
    // removing hook is needed to display new vista-style dialog
    OpenFileName.Flags := OpenFileName.Flags and (not OFN_ENABLEHOOK);
    OpenFileName.lpfnHook := nil;
  end;
end;

{ TTntOpenDialogLX }

function TTntOpenDialogLX.Execute: Boolean;
{$IFDEF COMPILER_10_UP}
var
  SaveAppModalPopupMode: TPopupMode;
{$ENDIF}
begin
  {$IFDEF COMPILER_10_UP}
  OpenDialog_BeforeExecute(Self, SaveAppModalPopupMode);
  {$ENDIF}
  try
    Result := inherited Execute;
  finally
    {$IFDEF COMPILER_10_UP}
    OpenDialog_AfterExecute(Self, SaveAppModalPopupMode);
    {$ENDIF}
  end;
end;

{$IFDEF COMPILER_9_UP}
function TTntOpenDialogLX.Execute(ParentWnd: HWND): Boolean;
{$IFDEF COMPILER_10_UP}
var
  SaveAppModalPopupMode: TPopupMode;
{$ENDIF}
begin
  FExplicit_ParentWnd := ParentWnd;
  try
    {$IFDEF COMPILER_10_UP}
    OpenDialog_BeforeExecute(Self, SaveAppModalPopupMode);
    {$ENDIF}
    try
      Result := inherited Execute(ParentWnd);
    finally
      {$IFDEF COMPILER_10_UP}
      OpenDialog_AfterExecute(Self, SaveAppModalPopupMode);
      {$ENDIF}
    end;
  finally
    FExplicit_ParentWnd := 0;
  end;
end;
{$ENDIF}

function TTntOpenDialogLX.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
begin
  OpenDialog_BeforeTaskModalDialog(Self, FExplicit_ParentWnd, DialogData);
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

{ TTntSaveDialogLX }

function TTntSaveDialogLX.Execute: Boolean;
{$IFDEF COMPILER_10_UP}
var
  SaveAppModalPopupMode: TPopupMode;
{$ENDIF}
begin
  {$IFDEF COMPILER_10_UP}
  OpenDialog_BeforeExecute(Self, SaveAppModalPopupMode);
  {$ENDIF}
  try
    Result := inherited Execute;
  finally
    {$IFDEF COMPILER_10_UP}
    OpenDialog_AfterExecute(Self, SaveAppModalPopupMode);
    {$ENDIF}
  end;
end;

{$IFDEF COMPILER_9_UP}
function TTntSaveDialogLX.Execute(ParentWnd: HWND): Boolean;
{$IFDEF COMPILER_10_UP}
var
  SaveAppModalPopupMode: TPopupMode;
{$ENDIF}
begin
  FExplicit_ParentWnd := ParentWnd;
  try
    {$IFDEF COMPILER_10_UP}
    OpenDialog_BeforeExecute(Self, SaveAppModalPopupMode);
    {$ENDIF}
    try
      Result := inherited Execute(ParentWnd);
    finally
      {$IFDEF COMPILER_10_UP}
      OpenDialog_AfterExecute(Self, SaveAppModalPopupMode);
      {$ENDIF}
    end;
  finally
    FExplicit_ParentWnd := 0;
  end;
end;
{$ENDIF}

function TTntSaveDialogLX.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
begin
  OpenDialog_BeforeTaskModalDialog(Self, FExplicit_ParentWnd, DialogData);
  Result := inherited TaskModalDialog(DialogFunc, DialogData);
end;

end.
