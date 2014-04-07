
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXPasswordEdit;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms, ExtCtrls, StdCtrls, ActnList, Graphics,
  Menus, TntStdCtrls, TntExtCtrls, TntMenus, TntActnList, TntLXForms;

type
  TTntPasswordEditCapsLockWarningFrm = class(TTntFormLX)
    CapsLockShape: TTntShape;
    CapsLockLbl: TTntLabel;
    CapsLockLogo: TTntImage;
    Label1: TTntLabel;
    Label2: TTntLabel;
    TntActionList1: TTntActionList;
    TntPopupMenu1: TTntPopupMenu;
    ShowPasswordAction: TTntAction;
    ShowPasswordAction1: TTntMenuItem;
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormClick(Sender: TObject);
    procedure ShowPasswordActionExecute(Sender: TObject);
    procedure ShowPasswordActionUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTntPasswordEdit = class(TTntEdit)
  private
    FLastClientOrigin: TPoint;
    FChangingCapsLockWarningFrm: Boolean;
    FCapsLockWarningFrm: TTntPasswordEditCapsLockWarningFrm;
    FAllowPasswordToBeRevealed: Boolean;
    procedure Delayed_HideCapsLockForm;
    procedure Delayed_ShowCapsLockForm;
    procedure RemoveAllInternalMsgs;
    procedure ProcessInternalMsg(Code: Integer);
    procedure Now_HideCapsLockForm;
    procedure Now_ShowCapsLockForm;
    function MsgCodeExists(Code: Integer): Boolean;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetPopupMenu: TPopupMenu{TNT-ALLOW TPopupMenu}; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowPasswordToBeRevealed: Boolean read FAllowPasswordToBeRevealed write FAllowPasswordToBeRevealed default True;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, TntSysUtils, TntLXUtils;

resourcestring
  CapsLockLblCaption = 'Caps Lock is On.';
  ShowPasswordActionCaption = 'Show Password';
  PasswordMsg1 = 'Having Caps Lock on may cause you to enter your password incorrectly.';
  PasswordMsg2 = 'You should press Caps Lock to turn it off before entering your password.';

function CapsLockIsOn: Boolean;
begin
  Result := ($00000001 and GetKeyState(VK_CAPITAL)) <> 0;
end;

var
  InternalMsg: Cardinal;

const
  SHOW_WARNING_MSG = 1;
  HIDE_WARNING_MSG = 2;

{ TTntPasswordEditCapsLockWarningFrm }

procedure TTntPasswordEditCapsLockWarningFrm.TntFormCreate(Sender: TObject);
begin
  CapsLockLbl.Caption := CapsLockLblCaption;
  ShowPasswordAction.Caption := ShowPasswordActionCaption;
  Label1.Caption := PasswordMsg1;
  Label2.Caption := PasswordMsg2;
end;

procedure TTntPasswordEditCapsLockWarningFrm.ShowPasswordActionUpdate(
  Sender: TObject);
begin
  ShowPasswordAction.Checked := (Owner as TTntPasswordEdit).PasswordChar = #0;
end;

procedure TTntPasswordEditCapsLockWarningFrm.ShowPasswordActionExecute(
  Sender: TObject);
begin
  (Owner as TTntPasswordEdit).Now_HideCapsLockForm;
  if (Owner as TTntPasswordEdit).PasswordChar = #0 then
    (Owner as TTntPasswordEdit).PasswordChar := #9679
  else
    (Owner as TTntPasswordEdit).PasswordChar := #0;
  if CapsLockIsOn then
    (Owner as TTntPasswordEdit).Now_ShowCapsLockForm;
end;

procedure TTntPasswordEditCapsLockWarningFrm.TntFormClick(Sender: TObject);
begin
  (Owner as TTntPasswordEdit).Now_HideCapsLockForm;
end;


{ TTntPasswordEdit }

constructor TTntPasswordEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAllowPasswordToBeRevealed := True;
end;

function TTntPasswordEdit.GetPopupMenu: TPopupMenu{TNT-ALLOW TPopupMenu};
begin
  Result := inherited GetPopupMenu;
  if (Result = nil)
  and AllowPasswordToBeRevealed
  and (not (csDesigning in ComponentState)) then
  begin
    if FCapsLockWarningFrm = nil then
      FCapsLockWarningFrm := TTntPasswordEditCapsLockWarningFrm.Create(Self);
    Result := FCapsLockWarningFrm.TntPopupMenu1;
  end;
end;

procedure TTntPasswordEdit.RemoveAllInternalMsgs;
var
  Msg: TMsg;
  MsgAvail: BOOL;
begin
  repeat
    MsgAvail := PeekMessage{TNT-ALLOW PeekMessage}(Msg, Handle, InternalMsg, InternalMsg, PM_REMOVE)
  until
    not MsgAvail;
end;

function TTntPasswordEdit.MsgCodeExists(Code: Integer): Boolean;
var
  Msg: TMsg;
begin
  Result := False;
  if PeekMessage{TNT-ALLOW PeekMessage}(Msg, Handle, InternalMsg, InternalMsg, PM_NOREMOVE) then
    Result := (Msg.wParam = Code);
end;

procedure TTntPasswordEdit.Delayed_ShowCapsLockForm;
begin
  if (not FChangingCapsLockWarningFrm)
  and (not MsgCodeExists(HIDE_WARNING_MSG)) then begin
    RemoveAllInternalMsgs;
    PostMessage(Handle, InternalMsg, SHOW_WARNING_MSG, 0);
  end;
end;

procedure TTntPasswordEdit.Delayed_HideCapsLockForm;
begin
  if (not FChangingCapsLockWarningFrm)
  and (not MsgCodeExists(SHOW_WARNING_MSG)) then begin
    RemoveAllInternalMsgs;
    PostMessage(Handle, InternalMsg, HIDE_WARNING_MSG, 0);
  end;
end;

procedure TTntPasswordEdit.Now_ShowCapsLockForm;
begin
  RemoveAllInternalMsgs;
  SendMessage(Handle, InternalMsg, SHOW_WARNING_MSG, 0);
end;

procedure TTntPasswordEdit.Now_HideCapsLockForm;
begin
  RemoveAllInternalMsgs;
  SendMessage(Handle, InternalMsg, HIDE_WARNING_MSG, 0);
end;

procedure TTntPasswordEdit.ProcessInternalMsg(Code: Integer);

    procedure ShowCapsLockForm;
    var
      Rect: TRect;
    begin
      if (PasswordChar = #0)
      or (IsReallyWindowsXP)
      then
        exit; // XP has its own mechanism

      if FCapsLockWarningFrm = nil then
        FCapsLockWarningFrm := TTntPasswordEditCapsLockWarningFrm.Create(Self);

      Rect := GetClientRect;
      Rect.TopLeft := ClientToScreen(Rect.TopLeft);
      Rect.BottomRight := ClientToScreen(Rect.BottomRight);

      FCapsLockWarningFrm.Top := Rect.Bottom + 5;
      FCapsLockWarningFrm.Left := Rect.Left;

      if not FCapsLockWarningFrm.Visible then
        FCapsLockWarningFrm.Show;
      if not Self.Focused then
        Self.SetFocus;
    end;

    procedure HideCapsLockForm;
    begin
      if FCapsLockWarningFrm <> nil then
        FCapsLockWarningFrm.Hide;
    end;

begin
  if not FChangingCapsLockWarningFrm then begin
    FChangingCapsLockWarningFrm := True;
    try
      RemoveAllInternalMsgs;
      if Code = SHOW_WARNING_MSG then
        ShowCapsLockForm
      else if Code = HIDE_WARNING_MSG then
        HideCapsLockForm
    finally
      FChangingCapsLockWarningFrm := False;
    end;
  end;
end;

procedure TTntPasswordEdit.WndProc(var Message: TMessage);
var
  NewClientOrigin: TPoint;
begin
  if (Message.Msg = InternalMsg) then begin
    ProcessInternalMsg(Message.WParam);
  end else begin
    // check for window move
    NewClientOrigin := GetClientOrigin;
    if not PointsEqual(FLastClientOrigin, NewClientOrigin) then begin
      FLastClientOrigin := NewClientOrigin;
      Now_HideCapsLockForm;
    end else if Message.Msg = WM_KILLFOCUS then begin
      Delayed_HideCapsLockForm;
    end else if (Message.Msg = WM_SETFOCUS)
    and (not FChangingCapsLockWarningFrm)
    and CapsLockIsOn then begin
      Delayed_ShowCapsLockForm;
    end;
    inherited;
  end;
end;

procedure TTntPasswordEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_CAPITAL) and CapsLockIsOn then
    Now_ShowCapsLockForm
  else
    Now_HideCapsLockForm;
  inherited;
end;

initialization
  InternalMsg := RegisterWindowMessage('TTntPasswordEdit.InternalMsg');

end.
