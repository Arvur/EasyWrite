
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXCancelableFrm;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Classes, Controls, StdCtrls, TntLXForms, TntStdCtrls, TntClasses, TntLXUtils;

type
  TTntCancelableForm = class(TTntFormLX)
    MsgLbl: TTntLabel;
    CancelBtn: TTntButton;
    procedure CancelBtnClick(Sender: TObject);
  private
    FMsgStack: TTntStringList;
    FPreviousGlobalFlashMsg: TStringEvent;
    FPreviousSuppressFlash: Boolean;
    FCancelPressed: Boolean;
    procedure CheckCancel;
    procedure SetMsgWithCheck(const Msg: WideString);
    procedure SetMsg(const Msg: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartMessage(const Msg: WideString);
    procedure StopMessage;
    property CancelPressed: Boolean read FCancelPressed;
  end;

procedure TCancelableThread_Create(ThreadSafeEvent: TNotifyEvent; Sender: TObject;
  const WaitMsg: WideString; FreeObjectOnTerminate: TObject);

procedure BeginCancelForm(const Msg: WideString);
procedure EndCancelForm;
procedure CheckCancelForm;
procedure UpdateCancelMsgWithCheck(const Msg: WideString);

implementation

{$R *.dfm}

uses
  SysUtils, SysConst, Forms, TntSysUtils, TntLXFlashMsg, Math, TntLXVclUtils;

var
  GSimpleCancelForm: TTntCancelableForm;
  SaveCursor: TCursor;

procedure BeginCancelForm(const Msg: WideString);
begin
  if GSimpleCancelForm = nil then begin
    SaveCursor := Screen.Cursor;
    GSimpleCancelForm := TTntCancelableForm.Create(Application);
    GSimpleCancelForm.StartModal;
    // These TntLXFlashMsg vars get reset when cancel form is destroyed...
    TntLXFlashMsg.SupressFlash := True;
    TntLXFlashMsg.FOnGlobalFlashMsg := GSimpleCancelForm.SetMsgWithCheck;
  end;
  GSimpleCancelForm.StartMessage(Msg);
end;

procedure EndCancelForm;
begin
  Assert(GSimpleCancelForm <> nil);
  GSimpleCancelForm.StopMessage;
  if (GSimpleCancelForm.FMsgStack.Count = 0) then begin
    GSimpleCancelForm.StopModal;
    FreeAndNil(GSimpleCancelForm);
    Screen.Cursor := crDefault;
    Screen.Cursor := SaveCursor;
  end;
end;

procedure CheckCancelForm;
begin
  Assert(GSimpleCancelForm <> nil);
  GSimpleCancelForm.CheckCancel;
end;

procedure UpdateCancelMsgWithCheck(const Msg: WideString);
begin
  Assert(GSimpleCancelForm <> nil);
  GSimpleCancelForm.SetMsgWithCheck(Msg);
end;

resourcestring
  SCancelBtn = 'Cancel';
  SPleaseWait = 'Please Wait';

{ TTntCancelableForm }

constructor TTntCancelableForm.Create(AOwner: TComponent);
begin
  inherited;
  FMsgStack := TTntStringList.Create;
  FPreviousGlobalFlashMsg := TntLXFlashMsg.FOnGlobalFlashMsg;
  FPreviousSuppressFlash := TntLXFlashMsg.SupressFlash;
  Caption := SPleaseWait;
  CancelBtn.Caption := SCancelBtn;
end;

destructor TTntCancelableForm.Destroy;
begin
  TntLXFlashMsg.FOnGlobalFlashMsg := FPreviousGlobalFlashMsg;
  TntLXFlashMsg.SupressFlash := FPreviousSuppressFlash;
  FreeAndNil(FMsgStack);
  inherited;
end;

procedure TTntCancelableForm.CheckCancel;
begin
  Application.ProcessMessages;
  if FCancelPressed then
    Abort;
end;

procedure TTntCancelableForm.SetMsgWithCheck(const Msg: WideString);
begin
  CheckCancel;
  SetMsg(Msg);
end;

procedure TTntCancelableForm.SetMsg(const Msg: WideString);
begin
  MsgLbl.Caption := Msg;
  ClientWidth := Max(ClientWidth, MsgLbl.Width + (MsgLbl.Left * 2));
  ClientHeight := Max(ClientHeight, MsgLbl.Height + MsgLbl.Top * 3 + CancelBtn.Height);
  Left := (Screen.Width - Width) div 2;
  CancelBtn.Left := (ClientWidth - CancelBtn.Width) div 2;
  CancelBtn.Top := ClientHeight - MsgLbl.Top - CancelBtn.Height;
  Refresh;
end;

procedure TTntCancelableForm.StartMessage(const Msg: WideString);
begin
  FMsgStack.Add(MsgLbl.Caption); // push
  SetMsg(Msg);
end;

procedure TTntCancelableForm.StopMessage;
begin
  Assert(FMsgStack.Count > 0);
  SetMsg(FMsgStack[FMsgStack.Count - 1]);
  FMsgStack.Delete(FMsgStack.Count - 1); // pop
end;

procedure TTntCancelableForm.CancelBtnClick(Sender: TObject);
begin
  FCancelPressed := True;
end;

{ TCancelableThread }
type
  TCancelableThread = class(TThread)
  private
    FThreadSafeEvent: TNotifyEvent;
    FSender: TObject;
    FFreeObjectOnTerminate: TObject;
    FException: Exception;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
  end;

destructor TCancelableThread.Destroy;
begin
  FException.Free;
  inherited;
end;

procedure TCancelableThread.Execute;
begin
  try
    try
      if Assigned(FThreadSafeEvent) then
        FThreadSafeEvent(FSender);
    finally
      try
        FFreeObjectOnTerminate.Free;
      except
        on E: Exception do begin
          E.Message := 'Internal Error: Error freeing FFreeObjectOnTerminate.' + CRLF + CRLF + E.Message;
          raise;
        end;
      end;
    end;
  except
    FException := AcquireExceptionObject;
  end;
end;

//-----------------------------------------------------------------

var
  GCancelForm: TTntCancelableForm;

procedure TCancelableThread_Create(ThreadSafeEvent: TNotifyEvent; Sender: TObject;
  const WaitMsg: WideString; FreeObjectOnTerminate: TObject);
var
  StartTime: Cardinal;
  ExceptObject: Exception;
  Thread: TCancelableThread;
  WasCancelFormCancelPressed: Boolean;
  CreatedCancelForm: Boolean;
begin
  BeginCursor(crHourGlass);
  try
    if GCancelForm <> nil then
      CreatedCancelForm := False
    else begin
      GCancelForm := TTntCancelableForm.Create(nil);
      CreatedCancelForm := True;
    end;
    try
      GCancelForm.StartMessage(WaitMsg);
      try
        // start thread
        Thread := TCancelableThread.Create(True);
        with Thread do begin
          FThreadSafeEvent := ThreadSafeEvent;
          FSender := Sender;
          FFreeObjectOnTerminate := FreeObjectOnTerminate;
          Resume;
        end;
        // watch thread
        try
          StartTime := GetTickCount;
          while (WaitForSingleObject(Thread.Handle, 0) = WAIT_TIMEOUT)
          and (not GCancelForm.CancelPressed) do
          begin
            if (not GCancelForm.Visible) and ((GetTickCount - StartTime) > 1000) then
              GCancelForm.StartModal;
            if GCancelForm.Visible then
              Application.ProcessMessages
            else
              Sleep(25);
          end;
        finally
          if GCancelForm.Visible and CreatedCancelForm then
            GCancelForm.StopModal;
        end;
      finally
        GCancelForm.StopMessage;
      end;
      WasCancelFormCancelPressed := GCancelForm.CancelPressed;
    finally
      if CreatedCancelForm then
        FreeAndNil(GCancelForm);
    end;
    // hold local reference to any exceptions raised
    if WasCancelFormCancelPressed then
      ExceptObject := EAbort.Create(SOperationAborted)
    else begin
      ExceptObject := Thread.FException;
      Thread.FException := nil; { don't free }
    end;
    // handle cleanup
    if WaitForSingleObject(Thread.Handle, 0) = WAIT_OBJECT_0 then
      Thread.Free
    else
      Thread.FreeOnTerminate := True; { it will free later }
    // raise any exceptions captured
    if ExceptObject <> nil then
      raise ExceptObject;
  finally
    EndCursor;
  end;
end;

end.
