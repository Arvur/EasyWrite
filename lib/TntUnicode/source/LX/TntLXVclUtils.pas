
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXVclUtils;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, ShellApi, Controls, Menus, Dialogs, TntMenus, TntStdCtrls, TntLXUtils;

// cursor status
procedure BeginCursor(Cursor: TCursor = crHourGlass);
procedure EndCursor;

// shell file ops
const
  DEFAULT_COPY_FLAGS = FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_NO_CONNECTED_ELEMENTS;

procedure ShDeleteItem(Item: WideString; Flags: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
  SimpleProgressTitle: WideString = '');
procedure ShCopyItem(FromItem, ToItem: WideString; Flags: FILEOP_FLAGS = DEFAULT_COPY_FLAGS;
  SimpleProgressTitle: WideString = '');

// shell
procedure ShellOpenFile(const FileName: WideString; const Parameters: WideString = '';
  const Directory: WideString = ''; ShowCmd: Integer = SW_SHOW);
procedure ShellPrintFile(const FileName: WideString; const Directory: WideString = '');

// controls
function IsSubControl(Control, SubControl: TWinControl): Boolean;
function ContainerIsFocused(Control: TWinControl): Boolean;

// vcl
function GetDefaultMenuItem(PopupMenu: TTntPopupMenu; UseFirstIfNoDefault: Boolean = True): TMenuItem{TNT-ALLOW TMenuItem};
procedure AutoFitLabelOverControl(ALabel: TTntLabel; Control: TControl; AllowControlStretch: Boolean = False);
procedure AutoSizeRadio(Radio: TTntRadioButton);
procedure AutoSizeCheckBox(CheckBox: TTntCheckBox);
procedure AutoPosition_Controls(Controls: array of TControl);
procedure CenterControls(Control, Anchor: TControl);
procedure PopupMenuForButton(PopupMenu: TTntPopupMenu; Button: TControl);

//-- Vista
function GetShieldIcon: HICON;
function Button_SetElevationRequiredState(hwnd: HWND; fRequired: BOOL): LRESULT;
function WideMessageDlg_VistaShield(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

implementation

uses
  Classes, Types, SysUtils, Contnrs, Forms, TntSysUtils, TntGraphics, TntDialogs, TntForms;

//=============================================== cursor status
var
  SavedCursors: TStack;

procedure BeginCursor(Cursor: TCursor = crHourGlass);
begin
  SavedCursors.Push(Pointer(Screen.Cursor));
  Screen.Cursor := Cursor;
end;

procedure EndCursor;
begin
  if SavedCursors.Count = 0 then
    Screen.Cursor := crDefault
  else
    Screen.Cursor := TCursor(SavedCursors.Pop);
end;

//=============================================== shell file ops
procedure ShDeleteItem(Item: WideString; Flags: FILEOP_FLAGS = FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
  SimpleProgressTitle: WideString = '');
begin
  {$IFDEF COMPILER_9_UP}
  ShDeleteItemEx(Application.ActiveFormHandle, Item, Flags, SimpleProgressTitle);
  {$ELSE}
  ShDeleteItemEx(Application.Handle, Item, Flags, SimpleProgressTitle);
  {$ENDIF}
end;

procedure ShCopyItem(FromItem, ToItem: WideString; Flags: FILEOP_FLAGS = DEFAULT_COPY_FLAGS;
  SimpleProgressTitle: WideString = '');
begin
  {$IFDEF COMPILER_9_UP}
  ShCopyItemEx(Application.ActiveFormHandle, FromItem, ToItem, Flags, SimpleProgressTitle);
  {$ELSE}
  ShCopyItemEx(Application.Handle, FromItem, ToItem, Flags, SimpleProgressTitle);
  {$ENDIF}
end;

//=============================================== shell
procedure ShellOpenFile(const FileName: WideString; const Parameters: WideString = '';
  const Directory: WideString = ''; ShowCmd: Integer = SW_SHOW);
begin
  {$IFDEF COMPILER_9_UP}
  ShellOpenFileEx(Application.ActiveFormHandle, FileName, Parameters, Directory, ShowCmd);
  {$ELSE}
  ShellOpenFileEx(Application.Handle, FileName, Parameters, Directory, ShowCmd);
  {$ENDIF}
end;

procedure ShellPrintFile(const FileName: WideString; const Directory: WideString = '');
begin
  {$IFDEF COMPILER_9_UP}
  ShellPrintFileEx(Application.ActiveFormHandle, FileName, Directory);
  {$ELSE}
  ShellPrintFileEx(Application.Handle, FileName, Directory);
  {$ENDIF}
end;

//=============================================== Controls

function IsSubControl(Control, SubControl: TWinControl): Boolean;
begin
  if SubControl = nil then
    Result := False
  else if Control = nil then
    Result := False
  else begin
    while (Control <> SubControl) and (SubControl.Parent <> nil) do
      SubControl := SubControl.Parent;
    Result := (Control = SubControl);
  end;
end;

function ContainerIsFocused(Control: TWinControl): Boolean;
begin
  If Control.Focused then
    Result := True
  else
    Result := IsSubControl(Control, GetParentForm(Control).ActiveControl);
end;

//======================================== vcl
function GetDefaultMenuItem(PopupMenu: TTntPopupMenu; UseFirstIfNoDefault: Boolean = True): TMenuItem{TNT-ALLOW TMenuItem};
var
  i: integer;
begin
  Result := nil;
  // refresh menu
  PopupMenu.Items.Click;
  // look for default item
  for i := 0 to PopupMenu.Items.Count - 1 do begin
    if PopupMenu.Items[i].Default then begin
      Result := PopupMenu.Items[i];
      break;
    end;
  end;
  if (Result = nil) and UseFirstIfNoDefault then begin
    // if no default found, return first item with an onclick handler
    for i := 0 to PopupMenu.Items.Count - 1 do begin
      if Assigned(PopupMenu.Items[i].OnClick) then begin
        Result := PopupMenu.Items[i];
        break;
      end;
    end;
  end;
end;

procedure AutoFitLabelOverControl(ALabel: TTntLabel; Control: TControl; AllowControlStretch: Boolean = False);
var
  Overlap: Integer;
begin
  Overlap := -(Control.Left - ALabel.Left - ALabel.Width) + 4;
  if (Overlap > 0) or AllowControlStretch then begin
    Control.Left := Control.Left + Overlap;
    Control.Width := Control.Width - Overlap;
  end;
end;

procedure AutoSizeRadio(Radio: TTntRadioButton);
const
  STD_GAP = 8;
begin
  Radio.Width := WideCanvasTextWidth(GetParentForm(Radio).Canvas, Radio.Caption) + GetSystemMetrics(SM_CXSMSIZE) - STD_GAP;
  if Is_WINE then
    Radio.Width := Radio.Width + STD_GAP;
end;

procedure AutoSizeCheckBox(CheckBox: TTntCheckBox);
//const
//  STD_GAP = 8;
begin
  CheckBox.Width := WideCanvasTextWidth(GetParentForm(CheckBox).Canvas, CheckBox.Caption) + GetSystemMetrics(SM_CXSMSIZE);// - STD_GAP;
//  if Is_WINE then
//    CheckBox.Width := CheckBox.Width + STD_GAP;
end;

procedure AutoPosition_Controls(Controls: array of TControl);
var
  i: integer;
begin
  for i := Low(Controls) to High(Controls) do begin
    if Controls[i] is TTntRadioButton then
      AutoSizeRadio(Controls[i] as TTntRadioButton);
    if Controls[i] is TTntCheckBox then
      AutoSizeCheckBox(Controls[i] as TTntCheckBox);
  end;
  for i := Low(Controls) + 1 to High(Controls) do begin
    Controls[i].Left := Controls[i - 1].Left + Controls[i - 1].Width + 4;
  end;
end;

procedure CenterControls(Control, Anchor: TControl);
begin
  Control.Left := Anchor.Left + ((Anchor.Width - Control.Width) div 2);
end;

procedure PopupMenuForButton(PopupMenu: TTntPopupMenu; Button: TControl);
var
  Pt: TPoint;
begin
  with Button do
    Pt := Point(Left, Top + Height);
  Pt := Button.Parent.ClientToScreen(Pt);
  PopupMenu.Popup(Pt.X, Pt.y);
end;

//-----------------------------------------------------------------
type
  SHSTOCKICONINFO = packed record
    cbSize: DWORD ;
    hIcon: HICON ;
    iSysImageIndex: Integer;
    iIcon: Integer;
    szPath: array[0..MAX_PATH-1] of WCHAR;
  end;
  SHSTOCKICONID = Word;

const
  SIID_SHIELD = 77;
  SHGSI_ICON = SHGFI_ICON;
  SHGSI_SMALLICON = SHGFI_SMALLICON;

var
  _Shell32Dll: THandle;
  _SHGetStockIconInfo: function (siid: SHSTOCKICONID; uFlags: UINT; var psii: SHSTOCKICONINFO): HRESULT stdcall;

function GetShieldIcon: HICON;
var
  sii: SHSTOCKICONINFO;
begin
  Result := 0;
  if Win32PlatformIsVista then begin
    if _Shell32DLL = 0 then
    begin
      _Shell32DLL := GetModuleHandle('shell32.dll');
      if _Shell32DLL <> 0 then
        @_SHGetStockIconInfo := GetProcAddress(_Shell32DLL, 'SHGetStockIconInfo');
    end;
    if Assigned(_SHGetStockIconInfo) then
    begin
      sii.cbSize := sizeof(sii);
      _SHGetStockIconInfo(SIID_SHIELD, SHGSI_ICON or SHGSI_SMALLICON, sii);
      Result := sii.hIcon;
    end;
  end;
end;

//-----------------------------------------------------------------

const BCM_FIRST =$1600;
const BCM_SETSHIELD = (BCM_FIRST + $000C);

function Button_SetElevationRequiredState(hwnd: HWND; fRequired: BOOL): LRESULT;
begin
  Result := 0;
  if Win32PlatformIsVista then
    Result := SendMessage(hwnd, BCM_SETSHIELD, 0, Integer(fRequired));
end;

//-----------------------------------------------------------------

{ TVistaShieldApplier }
type
  TVistaShieldApplier = class(TComponent)
  private
    procedure HandleOnShow(Sender: TObject);
  end;

procedure TVistaShieldApplier.HandleOnShow(Sender: TObject);
var
  i: integer;
  Dlg: TTntForm;
begin
  Dlg := (Sender as TTntForm);
  for i := 0 to Dlg.ControlCount - 1 do begin
    if (Dlg.Controls[i] is TTntButton)
    and (TTntButton(Dlg.Controls[i]).ModalResult in [mrYes, mrOk, mrRetry, mrAll, mrYesToAll]) then
      Button_SetElevationRequiredState(TTntButton(Dlg.Controls[i]).Handle, True);
  end;
end;

function WideMessageDlg_VistaShield(const Msg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
var
  Dlg: TTntForm;
begin
  Dlg := WideCreateMessageDialog(Msg, DlgType, Buttons);
  with Dlg do
    try
      HelpContext := HelpCtx;
      Position := poScreenCenter;
      OnShow := TVistaShieldApplier.Create(Dlg).HandleOnShow;
      Result := ShowModal;
    finally
      Free;
    end;
end;

//-----------------------------------------------------------------

initialization
  SavedCursors := TStack.Create;

finalization
  FreeAndNil(SavedCursors);

end.


