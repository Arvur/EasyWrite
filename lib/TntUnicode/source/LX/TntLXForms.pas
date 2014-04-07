
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXForms;

{$INCLUDE TntCompilers.inc}

interface

uses
  Forms, Classes, Controls, Windows, Messages, Menus, TntExtCtrls, ComCtrls,
  TntForms, TntComCtrls;

type
  PCustomForm = ^TCustomForm;

type
  TTntFormLX = class(TTntForm)
  private
    FParentForm: TCustomForm;
    WindowList: Pointer;
    SaveActiveWindow: HWND;
    procedure SetParentForm(const Value: TCustomForm);
    function GetFormStyle: TFormStyle;
    function IsForm: Boolean;
    procedure SetFormStyle(const Value: TFormStyle);
    procedure CheckNotMainOrMDI;
  private
    function GetMenu: TMainMenu{TNT-ALLOW TMainMenu};
    procedure SetMenu(Value: TMainMenu{TNT-ALLOW TMainMenu});
    procedure CMMenuChanged(var Message: TMessage); message CM_MENUCHANGED;
    procedure WMInitMenuPopup(var Msg: TWMInitMenuPopup); message WM_INITMENUPOPUP;
  private
    FMDIControlBar: TTntControlBar;
    FMDIToolBar: TTntToolBar;
    FMDIStatusBar: TTntStatusBar;
    procedure SetMDIControlBar(const Value: TTntControlBar);
    procedure SetMDIStatusBar(const Value: TTntStatusBar);
    procedure SetMDIToolBar(const Value: TTntToolBar);
    procedure UpdateControlBar;
    procedure UpdateStatusBar;
    procedure UpdateToolBar;
  private
    FChildStatusBar: TTntStatusBar;
    FPrevStatusBarWinProc: Pointer;
    FSubClassedStatusBar: TTntStatusBar;
    FStatusBarWin32Proc_ObjectInstance: Pointer;
    FSaveStatusBarAlign: TAlign;
    FSaveStatusBarSizeGrip: Boolean;
    FSaveStatusBarRect: TRect;
    procedure StatusBarWin32Proc(var Message: TMessage);
  private
    FUpdateCount: Integer;
    FMDIChild_Activated: Boolean;
    procedure WMMDIActivate(var Message: TWMMDIActivate); message WM_MDIACTIVATE;
    procedure RedockToMDIForm(Force: Boolean);
    procedure MDIChild_UpdateMdiDockState(Activating: Boolean);
  protected
    AutoNilRef: PCustomForm;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Resizing(State: TWindowState); override;
  protected
    FTaskBarButton: Boolean;
    procedure CheckForAppWindowHide;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetTaskBarButton(const Value: Boolean);
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure StartModal;
    procedure StopModal;
    property ParentForm: TCustomForm read FParentForm write SetParentForm default nil;
    procedure BeginUpdate;
    procedure EndUpdate;
    class procedure BeginClientUpdate;
    class procedure EndClientUpdate;
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
  published
    property FormStyle: TFormStyle read GetFormStyle write SetFormStyle stored IsForm default fsNormal;
    {$IFDEF COMPILER_9_UP}
    property PopupMode default pmAuto;
    {$ENDIF}
    property TaskBarButton: Boolean read FTaskBarButton write SetTaskBarButton default False;
    property Menu read GetMenu write SetMenu stored IsForm;
    property MDIControlBar: TTntControlBar read FMDIControlBar write SetMDIControlBar;
    property MDIToolBar: TTntToolBar read FMDIToolBar write SetMDIToolBar;
    property MDIStatusBar: TTntStatusBar read FMDIStatusBar write SetMDIStatusBar;
  end;

procedure CloseAllNonModalFormsExcept(ExceptForm: TTntFormLX; DoDestroy: Boolean = False);
procedure ConfirmCloseQueryOnAllNonModalForms;

implementation

uses
  SysUtils, RtlConsts, Consts, {$IFDEF COMPILER_7_UP} Themes, {$ENDIF} Graphics, CommCtrl,
  TntMenus, TntClasses, TntSysUtils, TntWindows, StdCtrls;

resourcestring
  SCannotShowDisabledWindowAsModal = 'Cannot make a disabled window modal';

var
  NonModalFormList: TList;
  CloseMainFormWhenDone: Boolean = False;

procedure CloseAllNonModalFormsExcept(ExceptForm: TTntFormLX; DoDestroy: Boolean = False);
var
  i: integer;
  Form: TTntFormLX;
begin
  for i := NonModalFormList.Count - 1 downto 0 do begin
    Form := TTntFormLX(NonModalFormList[i]);
    if Form <> ExceptForm then begin
      if DoDestroy then
        Form.Free
      else
        Form.Close;
    end;
  end;
end;

procedure ConfirmCloseQueryOnAllNonModalForms;
var
  i: integer;
  Form: TTntFormLX;
begin
  for i := NonModalFormList.Count - 1 downto 0 do begin
    Form := TTntFormLX(NonModalFormList[i]);
    if not Form.CloseQuery then
      Abort;
  end;
end;

{ TTntFormLX }

constructor TTntFormLX.Create(AOwner: TComponent);
begin
  // standard construction technique (look at TForm.Create)
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TTntFormLX) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        if not InitInheritedComponent(Self, TTntFormLX) then
          raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      finally
        Exclude(FFormState, fsCreating);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

constructor TTntFormLX.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  {$IFDEF COMPILER_9_UP}
  PopupMode := pmAuto;
  {$ENDIF}
end;

destructor TTntFormLX.Destroy;
begin
  if FormStyle = fsMDIChild then
    BeginClientUpdate; { keeps other mdi children from flickering }
  try
    Destroying;
    TaskBarButton := False;
    if AutoNilRef <> nil then
      AutoNilRef^ := nil;
    inherited;
    Classes.FreeObjectInstance(FStatusBarWin32Proc_ObjectInstance);
  finally
    if FormStyle = fsMDIChild then
      EndClientUpdate;
  end;
end;

procedure TTntFormLX.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (ParentForm <> nil) then begin
    Params.WndParent := ParentForm.Handle;
    // ws_child makes it position consistently, but causes keyboard focus to not be allowed when it has a border
    // ws_popup cancels out all bad effects of ws_child
    Params.Style := Params.Style or WS_CHILD or WS_POPUP;
  end;
  if TaskBarButton and (not (csDesigning in ComponentState)) then begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW and not WS_EX_TOOLWINDOW;
    Params.WndParent := Application.Handle; // override PopupMode=pmAuto
  end;
end;

procedure TTntFormLX.CreateHandle;
begin
  inherited;
  if (ParentForm <> nil) then
    Windows.SetParent(Handle, ParentForm.Handle); // windows.SetParent makes it contained
end;

procedure TTntFormLX.DoClose(var Action: TCloseAction);
begin
  if (FormStyle = fsMDIChild) then
    Action := caFree;
  inherited DoClose(Action);
  if (Application.MainForm = Self)
  and TaskBarButton
  and (not (csDesigning in ComponentState))
  and (Action <> caNone) then begin
    // check for deferred closing
    if (NonModalFormList.Count > 1) then begin
      // defer closing until all non-modal forms are closed
      Action := caNone;
      CloseMainFormWhenDone := True;
      Hide;
    end;
  end;
end;

procedure TTntFormLX.Loaded;
begin
  inherited;
  UpdateControlBar;
  UpdateToolBar;
  UpdateStatusBar;
  RedockToMDIForm(True);
end;

procedure TTntFormLX.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then begin
    if (AComponent = FParentForm) then begin
      FParentForm := nil;
      if not (csDestroying in ComponentState) then
        RecreateWnd;
    end;
    if (AComponent = FMDIControlBar) then begin
      FMDIControlBar := nil;
      UpdateControlBar;
    end;
    if (AComponent = FMDIToolBar) then begin
      FMDIToolBar := nil;
      UpdateToolBar;
    end;
    if (AComponent = FMDIStatusBar) then begin
      FMDIStatusBar := nil;
      UpdateStatusBar;
    end;
  end;
  inherited;
end;

procedure TTntFormLX.WndProc(var Message: TMessage);
begin
  {$IFDEF COMPILER_7_UP}
  if Message.Msg = WM_THEMECHANGED then
    UpdateControlBar;
  {$ENDIF}
  inherited;
end;

procedure TTntFormLX.Resizing(State: TWindowState);
begin
  inherited;
  if (Application.MainForm is TTntFormLX) then
    RedockToMDIForm(False);
end;

var
  AssignedExitProc: Boolean;

procedure TntLXFormExitProc;
begin
  if (Application.MainForm <> nil)
  and (Application.MainForm.FormStyle = fsMDIForm) then
    Application.MainForm.Hide; { This way we don't get the flicker of seeing each client windows close. }
end;

procedure TTntFormLX.CMVisibleChanged(var Message: TMessage);
begin
  // TCustomForm.CMShowingChanged is the first to call Application.UpdateVisible ...
  CheckForAppWindowHide;
  // avoid flicker at end of app
  if (Self = Application.MainForm) and Visible and (not AssignedExitProc) then begin
    AddExitProc(TntLXFormExitProc); // this should come after AddExitProc(DoneApplication) in TApplication.Run;
    AssignedExitProc := True;
  end;
  inherited;
end;

procedure TTntFormLX.WMSysCommand(var Message: TWMSysCommand);
begin
  with Message do begin
    if (CmdType and $FFF0 = SC_MINIMIZE) and (Application.MainForm = Self)
    and TaskBarButton and (not (csDesigning in ComponentState)) then
      DefaultHandler(Message) { bypass TApplication stuff for minimizing with hidden app window }
    else
      inherited;
  end;
end;

procedure TTntFormLX.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if (Message.Active <> WA_INACTIVE)
  and (not (csDesigning in ComponentState))
  and (not IsWindowEnabled(Handle)) then begin
    SetForegroundWindow(Application.Handle);
  end;
end;

function TTntFormLX.IsForm: Boolean;
begin
  Result := not IsControl;
end;

function TTntFormLX.IsShortCut(var Message: TWMKey): Boolean;

  function CutIfPossible: Boolean;
  begin
    Result := GetFocus <> 0;
    if Result then
      SendMessage(GetFocus, WM_CUT, 0, 0);
  end;

  function CopyIfPossible: Boolean;
  begin
    Result := GetFocus <> 0;
    if Result then
      SendMessage(GetFocus, WM_COPY, 0, 0);
  end;

  function PasteIfPossible: Boolean;
  begin
    Result := GetFocus <> 0;
    if Result then
      SendMessage(GetFocus, WM_PASTE, 0, 0);
  end;

  function UndoIfPossible: Boolean;
  begin
    if GetFocus <> 0 then
      Result := SendMessage(GetFocus, WM_UNDO, 0, 0) <> 0
    else
      Result := False;
  end;

  function SelectAllIfPossible: Boolean;
  begin
    Result := True;
    if Screen.ActiveControl is TCustomEdit{TNT-ALLOW TCustomEdit} then
      TCustomEdit{TNT-ALLOW TCustomEdit}(Screen.ActiveControl).SelectAll
    else if Screen.ActiveControl is TCustomComboBox{TNT-ALLOW TCustomComboBox} then
      TCustomComboBox{TNT-ALLOW TCustomComboBox}(Screen.ActiveControl).SelectAll
    else
      Result := False;
  end;

var
  ShiftState: TShiftState;
begin
  Result := inherited IsShortCut(Message);
  if (not Result) then begin
    ShiftState := KeyDataToShiftState(Message.KeyData);
    if ShiftState = [ssCtrl] then begin
      if Message.CharCode = Ord('X') then
        Result := CutIfPossible;
      if Message.CharCode = Ord('C') then
        Result := CopyIfPossible;
      if Message.CharCode = Ord('V') then
        Result := PasteIfPossible;
      if Message.CharCode = Ord('Z') then
        Result := UndoIfPossible;
      if Message.CharCode = Ord('A') then
        Result := SelectAllIfPossible;
    end;
  end;
end;

function TTntFormLX.GetFormStyle: TFormStyle;
begin
  Result := inherited FormStyle;
end;

procedure TTntFormLX.SetFormStyle(const Value: TFormStyle);
begin
  if Value <> FormStyle then begin
    if ((Value in [fsMDIForm, fsMDIChild]) or (Application.MainForm = Self))
    and (ParentForm <> nil)
    then
      raise ETntInternalError.Create('Internal Error: A parented form can not be a main or MDI form.');
    if (Value = fsMDIChild) and TaskBarButton then
      raise ETntInternalError.Create('Internal Error: An MDI child form can not have a taskbar button.');
    inherited FormStyle := Value;
    RedockToMDIForm(True);
  end;
end;

// --- StartModal / StopModal ---

procedure TTntFormLX.CheckNotMainOrMDI;
begin
  if ((Application.MainForm = Self) or (FormStyle in [fsMDIForm, fsMDIChild])) then
    raise ETntInternalError.Create('Internal Error: This operation is not valid for a main or MDI form.');
  if TaskBarButton then
    raise ETntInternalError.Create('Internal Error: This operation is not valid for a form with a TaskBar button.');
end;

procedure TTntFormLX.SetParentForm(const Value: TCustomForm);
begin
  if FParentForm <> Value then begin
    CheckNotMainOrMDI;
    FParentForm := Value;
    if FParentForm <> nil then
      FParentForm.FreeNotification(Self);
    RecreateWnd;
  end;
end;

procedure TTntFormLX.StartModal;
begin
  CheckNotMainOrMDI;
  if Visible then
    raise EInvalidOperation.Create(SCannotShowModal);
  if (not Enabled) then
    raise ETntGeneralError.Create(SCannotShowDisabledWindowAsModal);
  if (ParentForm <> nil) then
    raise ETntInternalError.Create('Internal Error: StartModal can not be used with a parented form.');
  SaveActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  FFormState := FFormState + [fsModal];
  Show;
end;

procedure TTntFormLX.StopModal;
begin
  CheckNotMainOrMDI;
  if GetActiveWindow <> Handle then SaveActiveWindow := 0;
  Hide;
  FFormState := FFormState - [fsModal];
  if Assigned(WindowList) then begin
    EnableTaskWindows(WindowList);
    WindowList := nil;
  end;
  if SaveActiveWindow <> 0 then begin
    SetActiveWindow(SaveActiveWindow);
    SaveActiveWindow := 0;
  end;
end;

// --- TaskBarButton ---

var
  _AppWindowStyleSet: Boolean = False;

procedure TTntFormLX.CheckForAppWindowHide;
begin
  if (not (csDesigning in ComponentState))
  and (not _AppWindowStyleSet) then
  begin
    // I just change the style, becuase I find that keeping the app window visible makes things work better
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
      GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW
        or WS_EX_TOOLWINDOW); // Thanks to Peter Below for this snipped of code!
    _AppWindowStyleSet := True;
  end;
end;

procedure TTntFormLX.SetTaskBarButton(const Value: Boolean);

  procedure AddForm(Form: TTntFormLX);
  begin
    NonModalFormList.Add(Form);
  end;

  procedure RemoveForm(Form: TTntFormLX);
  begin
    NonModalFormList.Remove(Form);
    if CloseMainFormWhenDone
    and (NonModalFormList.Count = 1) { the main form is all that's left }
    and (Application.MainForm = NonModalFormList[0]) then
      Application.MainForm.Close;
  end;

begin
  if FTaskBarButton <> Value then begin
    if FTaskBarButton then
      RemoveForm(Self);
    if (FormStyle = fsMDIChild)
    or (ParentForm <> nil)
    then
      raise ETntGeneralError.Create('A MDI child or a parented form can not have its own task bar button.');
    FTaskBarButton := Value;
    if FTaskBarButton then
      AddForm(Self);
    if  (not (csDestroying in ComponentState))
    and (not (csDesigning in ComponentState)) then begin
      RecreateWnd;
    end;
  end;
end;

// --- BeginUpdate / EndUpdate

procedure TTntFormLX.BeginUpdate;
begin
  if HandleAllocated and (FUpdateCount = 0) then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
  Inc(FUpdateCount);
end;

procedure TTntFormLX.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if HandleAllocated and (FUpdateCount = 0) then begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Handle, nil, 0,
      RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_NOINTERNALPAINT);
  end;
end;

function MainClientHandle: THandle;
begin
  if (Application.MainForm <> nil) then
    Result := Application.MainForm.ClientHandle
  else
    Result := 0;
end;

var
  ClientUpdateCount: Integer;
  ClientUpdateLocked: Boolean;

class procedure TTntFormLX.BeginClientUpdate;
begin
  if (MainClientHandle <> 0) and (ClientUpdateCount = 0) then begin
    ClientUpdateLocked := LockWindowUpdate(Application.MainForm.Handle);
    SendMessage(MainClientHandle, WM_SETREDRAW, 0, 0);
  end;
  Inc(ClientUpdateCount);
end;

class procedure TTntFormLX.EndClientUpdate;
begin
  if ClientUpdateCount > 0 then
    Dec(ClientUpdateCount);
  if (ClientUpdateCount = 0) then begin
    if (MainClientHandle <> 0) then begin
      SendMessage(MainClientHandle, WM_SETREDRAW, 1, 0);
      RedrawWindow(MainClientHandle, nil, 0,
        RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_NOINTERNALPAINT);
      Application.MainForm.Refresh;
    end;
    if ClientUpdateLocked then
      LockWindowUpdate(0);
  end;
end;

// --- No Owner Drawn Top Level Menu Items ---

procedure TTntFormLX.CMMenuChanged(var Message: TMessage);
begin
  NoOwnerDrawTopLevelItems(Menu);
  inherited;
end;

function TTntFormLX.GetMenu: TMainMenu{TNT-ALLOW TMainMenu};
begin
  Result := inherited Menu;
end;

procedure TTntFormLX.SetMenu(Value: TMainMenu{TNT-ALLOW TMainMenu});
{This is needed to catch initial assignment to Menu property when
Form is read from Stream. Without this, there is a slight
flicker the first time the mouse hovers over the menu}
begin
  NoOwnerDrawTopLevelItems(Value);
  inherited Menu := Value;
end;

procedure TTntFormLX.WMInitMenuPopup(var Msg: TWMInitMenuPopup);
{If the window menu is about to drop down, tell Windows to refresh it. This
adds any open MDI children to menu}
begin
  if (FormStyle = fsMDIForm) and Assigned(WindowMenu) and (WindowMenu.Handle = Msg.MenuPopup) then
    SendMessage(ClientHandle, WM_MDIREFRESHMENU, 0, 0);
  inherited;
end;

//---------------- MDI Control Bar, MDI Tool Bar, MDI Status Bar ------------------------

procedure TTntFormLX.UpdateControlBar;
begin
  {$IFDEF COMPILER_7_UP}
  if (MDIControlBar <> nil) and (not (csDesigning in ComponentState)) then begin
    if ThemeServices.ThemesEnabled then
      MDIControlBar.Color := clMenuBar
    else
      MDIControlBar.Color := clMenu;
  end;
  {$ENDIF}
end;

procedure TTntFormLX.UpdateToolBar;
begin
  //
end;

procedure TTntFormLX.UpdateStatusBar;
begin
  if FSubClassedStatusBar <> MDIStatusBar then begin
    // unsubclass old one
    if FSubClassedStatusBar <> nil then begin
      if FSubClassedStatusBar.HandleAllocated then
        SetWindowLong(FSubClassedStatusBar.Handle, GWL_WNDPROC, Integer(FPrevStatusBarWinProc));
      FPrevStatusBarWinProc := nil;
      FSubClassedStatusBar := nil;
    end;
    if MDIStatusBar <> nil then begin
      if FStatusBarWin32Proc_ObjectInstance = nil then
        FStatusBarWin32Proc_ObjectInstance := MakeObjectInstance(StatusBarWin32Proc);
      FPrevStatusBarWinProc := Pointer(SetWindowLong(MDIStatusBar.Handle, GWL_WNDPROC, Integer(FStatusBarWin32Proc_ObjectInstance)));
      FSubClassedStatusBar := MDIStatusBar;
    end;
  end;
end;

procedure TTntFormLX.RedockToMDIForm(Force: Boolean);
begin
  if  (not (csLoading in ComponentState))
  and (FormStyle = fsMDIChild)
  and (Application.MainForm <> nil)
  and (Application.MainForm.ActiveMDIChild = Self) then begin
    if Force then BeginClientUpdate;
    try
      if Force then begin
        MDIChild_UpdateMdiDockState(WindowState <> wsMaximized);
      end;
      MDIChild_UpdateMdiDockState(WindowState = wsMaximized);
    finally
      if Force then EndClientUpdate;
    end;
  end;
end;

procedure TTntFormLX.SetMDIControlBar(const Value: TTntControlBar);
begin
  if (FMDIControlBar <> Value) then begin
    FMDIControlBar := Value;
    if MDIControlBar <> nil then
      MDIControlBar.FreeNotification(Self);
    UpdateControlBar;
    RedockToMDIForm(True);
  end;
end;

procedure TTntFormLX.SetMDIToolBar(const Value: TTntToolBar);
begin
  if (FMDIToolBar <> Value) then begin
    FMDIToolBar := Value;
    if MDIToolBar <> nil then
      MDIToolBar.FreeNotification(Self);
    UpdateToolBar;
    RedockToMDIForm(True);
  end;
end;

procedure TTntFormLX.SetMDIStatusBar(const Value: TTntStatusBar);
begin
  if (FMDIStatusBar <> Value) then begin
    FMDIStatusBar := Value;
    if MDIStatusBar <> nil then
      MDIStatusBar.FreeNotification(Self);
    UpdateStatusBar;
    RedockToMDIForm(True);
  end;
end;

//---------- MDI Child / Update MDI Dock State (Control Bar, Tool Bar, Status Bar ) -----------

procedure TTntFormLX.WMMDIActivate(var Message: TWMMDIActivate);
var
  BecomingActive: Boolean;
  LosingActive: Boolean;
begin
  inherited;
  if (FormStyle = fsMDIChild) then
  begin
    BecomingActive := Message.ActiveWnd = Handle;
    LosingActive := Message.DeactiveWnd = Handle;
    if (BecomingActive) then begin
      UpdateWindowState;
      MDIChild_UpdateMdiDockState(WindowState = wsMaximized)
    end else if (LosingActive) then
      MDIChild_UpdateMdiDockState(False);
  end;
end;

function GetFirstPanelWidth(SB: TTntStatusBar): Integer;
begin
  if (SB.SimplePanel) or (SB.Panels.Count = 0) then
    Result := 100
  else
    Result := SB.Panels[0].Width;
end;

function GetTotalPanelWidth(SB: TTntStatusBar): Integer;
var
  i: integer;
begin
  if (SB.SimplePanel) or (SB.Panels.Count = 0) then
    Result := GetFirstPanelWidth(SB)
  else begin
    Result := 0;
    for i := 0 to SB.Panels.Count - 1 do
      Inc(Result, SB.Panels[i].Width);
  end;
end;

type
  TAccessCollection = class(TCollection);

procedure TTntFormLX.MDIChild_UpdateMdiDockState(Activating: Boolean);
var
  MainControlBar: TTntControlBar;
  MainToolBar: TTntToolBar;
  MainStatusBar: TTntStatusBar;
begin
  if csLoading in ComponentState then
    exit;

  if (FormStyle <> fsMDIChild) then
    raise ETntInternalError.Create('Internal Error: MDIChild_UpdateMdiDockState can only be called from MDI child forms.');

  if (Activating <> FMDIChild_Activated)
  and (Application.MainForm is TTntFormLX) then begin
    BeginClientUpdate;
    try
      with TTntFormLX(Application.MainForm) do begin
        MainControlBar := MDIControlBar;
        MainToolBar := MDIToolBar;
        MainStatusBar := MDIStatusBar;
      end;

      // dock tool bar
      if  (MDIToolBar    <> nil) and (MainToolBar    <> nil)
      and (MDIControlBar <> nil) and (MainControlBar <> nil) then begin
        MDIToolBar.Hide;
        if Activating then begin
          MDIToolBar.Parent := MainControlBar;
          MDIToolBar.Left := MainToolBar.Left + 1;
        end else begin
          MDIToolBar.Parent := MDIControlBar;
          MDIToolBar.Left := 0;
        end;
        MDIControlBar.Top := 0;
        MDIToolBar.Show;
      end;

      // dock status bar
      if (MDIStatusBar <> nil)
      and (MainStatusBar <> nil) then begin
        if Activating then begin
          with MDIStatusBar do begin
            FSaveStatusBarRect := Rect(Left, Top, Left + Width, Top + Height);
            FSaveStatusBarAlign := Align;
            FSaveStatusBarSizeGrip := SizeGrip;
            SizeGrip := False;
            Align := alNone;
            Top := 0;
            Height := MainStatusBar.Height;
            Left := GetFirstPanelWidth(MainStatusBar) + 2;
            Width := GetTotalPanelWidth(MDIStatusBar) - 2;
            Parent := MainStatusBar;
          end;
          TTntFormLX(Application.MainForm).FChildStatusBar := MDIStatusBar;
          TAccessCollection(MainStatusBar.Panels).Update(nil);
        end else begin
          { de-activating }
          with MDIStatusBar do begin
            Parent := Self;
            Align := FSaveStatusBarAlign;
            with FSaveStatusBarRect do
              SetBounds(Left, Top, Right - Left, Bottom - Top);
            SizeGrip := FSaveStatusBarSizeGrip;
          end;
          TTntFormLX(Application.MainForm).FChildStatusBar := nil;
          TAccessCollection(MainStatusBar.Panels).Update(nil);
        end;
      end;
      FMDIChild_Activated := Activating;
    finally
      EndClientUpdate;
    end;
  end;
end;

//----------------------------------------------

procedure TTntFormLX.StatusBarWin32Proc(var Message: TMessage);
var
  ClientBarWidth: Integer;
  FirstPanelWidth: Integer;
  P: PInteger;
  i: integer;
begin
  if FChildStatusBar <> nil then
    ClientBarWidth := GetTotalPanelWidth(FChildStatusBar)
  else begin
    ClientBarWidth := 0;
  end;
  if (FChildStatusBar <> nil) and (Message.Msg = SB_SETPARTS) then begin
    P := PInteger(Message.lParam);
    for i := 1 to Message.WParam do begin
      if P^ <> -1 then
        P^ := P^ + ClientBarWidth;
      Inc(P);
    end;
  end;
  with Message do begin
    Result := CallWindowProc(FPrevStatusBarWinProc, FSubClassedStatusBar.Handle, Msg, wParam, lParam);
  end;
  if (FChildStatusBar <> nil) and (Message.Msg = WM_PAINT) then begin
    FirstPanelWidth := GetFirstPanelWidth(MDIStatusBar);
    with MDIStatusBar.Canvas do begin
      Pen.Color := MDIStatusBar.Color;
      MoveTo(FirstPanelWidth + -1, 0);
      LineTo(FirstPanelWidth + -1, MDIStatusBar.ClientHeight);
      MoveTo(FirstPanelWidth + 0, 0);
      LineTo(FirstPanelWidth + 0, MDIStatusBar.ClientHeight);
      Pen.Color := FChildStatusBar.Color;
      MoveTo(FirstPanelWidth + 1, 0);
      LineTo(FirstPanelWidth + 1, MDIStatusBar.ClientHeight);
      MoveTo(FirstPanelWidth + ClientBarWidth, 0);
      LineTo(FirstPanelWidth + ClientBarWidth, MDIStatusBar.ClientHeight);
      Pen.Color := MDIStatusBar.Color;
      MoveTo(FirstPanelWidth + ClientBarWidth + 1, 0);
      LineTo(FirstPanelWidth + ClientBarWidth + 1, MDIStatusBar.ClientHeight);
      MoveTo(FirstPanelWidth + ClientBarWidth + 2, 0);
      LineTo(FirstPanelWidth + ClientBarWidth + 2, MDIStatusBar.ClientHeight - 1);
      Pen.Color := clWhite;
      MoveTo(FirstPanelWidth - 1, 2);
      LineTo(FirstPanelWidth - 1, MDIStatusBar.ClientHeight);
      Pen.Color := clDkGray;
      MoveTo(FirstPanelWidth + ClientBarWidth + 2, 2);
      LineTo(FirstPanelWidth + ClientBarWidth + 2, MDIStatusBar.ClientHeight - 1);
    end;
  end;
end;

//----------------------------------------------

initialization
  {$IFDEF COMPILER_10_UP}
  Application.ModalPopupMode := pmAuto;
  {$ENDIF}
  NonModalFormList := TList.Create;

finalization
  NonModalFormList.Free;

end.
