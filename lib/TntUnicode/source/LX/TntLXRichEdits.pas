
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXRichEdits;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Windows, Messages, Controls, TntComCtrls, TntDBCtrls, TntLXUtils, TntLXDBCtrls;

type
  TRichEditLXVarBlock = record
    LastMouseDown: Cardinal;
    AutoUrlDetect: Boolean;
    OnLinkClick: TStringEvent;
    LastLinkTime: Cardinal;
    SaveHint: WideString;
    SaveCursor: HICON;
    SavedHint: Boolean;
    ShowLinkHint: Boolean;
  end;

  TTntCustomRichEditLX = class(TTntCustomRichEdit)
  private
    FVarBlock: TRichEditLXVarBlock;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure SetAutoUrlDetect(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetShowLinkHint(const Value: Boolean);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoLinkClick(const LinkText: WideString); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    //--
    property AutoUrlDetect: Boolean read FVarBlock.AutoUrlDetect write SetAutoUrlDetect default True;
    property ShowLinkHint: Boolean read FVarBlock.ShowLinkHint write SetShowLinkHint default True;
    property OnLinkClick: TStringEvent read FVarBlock.OnLinkClick write FVarBlock.OnLinkClick;
  end;

  TTntRichEditLX = class(TTntCustomRichEditLX)
  published
    property AutoUrlDetect;
    property ShowLinkHint;
    property OnLinkClick;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
    property Lines;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF COMPILER_9_UP}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    {$IFDEF COMPILER_10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  TTntDBRichEditLX = class(TTntDBRichEditBasicLX)
  private
    FVarBlock: TRichEditLXVarBlock;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure SetAutoUrlDetect(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetShowLinkHint(const Value: Boolean);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoLinkClick(const LinkText: WideString); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
  published
    property AutoUrlDetect: Boolean read FVarBlock.AutoUrlDetect write SetAutoUrlDetect default True;
    property ShowLinkHint: Boolean read FVarBlock.ShowLinkHint write SetShowLinkHint default True;
    property OnLinkClick: TStringEvent read FVarBlock.OnLinkClick write FVarBlock.OnLinkClick;
  end;

function RTFToPlainText(const StrValue: WideString): WideString;

implementation

uses
  SysUtils, Forms, RichEdit, TntWindows, TntSysUtils, TntControls, TntForms,
  TntLXSafeWinInet;

resourcestring
  SNotAvailableOffline = '(not available offline)';

function RTFToPlainText(const StrValue: WideString): WideString;
var
  Stream: TStringStream{TNT-ALLOW TStringStream};
  RichEditControl: TTntRichEdit;
begin
  if (not IsRTF(StrValue)) then
    Result := StrValue
  else begin
    Stream := TStringStream{TNT-ALLOW TStringStream}.Create(StrValue);
    try
      RichEditControl := TTntRichEdit.Create(nil);
      try
        RichEditControl.Width := Screen.Width;
        RichEditControl.ParentWindow := Application.Handle;
        RichEditControl.PlainText := False;
        RichEditControl.Lines.LoadFromStream(Stream);
        Result := RichEditControl.Lines.Text
      finally
        FreeAndNil(RichEditControl);
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

procedure TntRichEditLX_AfterInherited_Create(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock);
begin
  with VarBlock do begin
    AutoUrlDetect := True;
    ShowLinkHint := True;
  end;
end;

var
  FRichEdit40Module: THandle = 0;

function IsRichEdit40Available: Boolean;
const
  RICHED40_DLL = 'MSFTEDIT.DLL';
begin
  if FRichEdit40Module = 0 then
    FRichEdit40Module := Tnt_LoadLibraryW(RICHED40_DLL);
  Result := FRichEdit40Module <> 0;
end;

const
  MSFTEDIT_CLASSW = 	'RICHEDIT50W';

function TntRichEditLX_CreateWindowHandle(Control: TTntCustomRichEdit; const Params: TCreateParams): Boolean;
begin
  Result := Win32PlatformIsUnicode and IsRichEdit40Available;
  if Result then
    CreateUnicodeHandle(Control, Params, MSFTEDIT_CLASSW);
end;

procedure TntRichEditLX_AfterInherited_CreateWnd(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock);
var
  EventMask: Longint;
begin
  with Control, VarBlock do begin
    if AutoUrlDetect then begin
      SendMessage(Handle, EM_AUTOURLDETECT, 1, 0);
      EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) or ENM_Link;
      SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
    end;
  end;
end;

type TAccessWinControl = class(TWinControl);

procedure TntRichEditLX_SetAutoUrlDetect(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; const Value: Boolean);
begin
  with Control, VarBlock do begin
    if AutoUrlDetect <> Value then begin
      AutoUrlDetect := Value;
      TAccessWinControl(Control).RecreateWnd;
    end;
  end;
end;

procedure TntRichEditLX_DoLinkClick(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; const LinkText: WideString);
begin
  with Control, VarBlock do begin
    if Assigned(OnLinkClick) then
      OnLinkClick(LinkText)
    else
      SemiSafeOpenURL(LinkText);
  end;
end;

procedure TntRichEditLX_SetLinkHint(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; Value: WideString);
begin
  with Control, VarBlock do begin
    if ShowLinkHint then begin
      if (Value = '') then begin
        { restore original hint, cursor }
        if SavedHint then begin
          Hint := SaveHint;
          SavedHint := False;
          { use standard cursor }
          Windows.SetCursor(SaveCursor);
        end;
      end else begin
        { remember original hint }
        if (not SavedHint) then begin
          SaveHint := Hint;
          SaveCursor := Windows.GetCursor;
          SavedHint := True;
        end;
        // display hint
        if Pos(':', Value) = 0 then begin
          if Pos('@', Value) > 1 then
            Value := 'mailto:' + Value
          else if (Pos('www.', Value) = 1)
          or WideSameText('.com', Copy(Value, Length(Value) - 4, 4))
          or WideSameText('.net', Copy(Value, Length(Value) - 4, 4))
          or WideSameText('.org', Copy(Value, Length(Value) - 4, 4))
          or WideSameText('.gov', Copy(Value, Length(Value) - 4, 4))
          or WideSameText('.edu', Copy(Value, Length(Value) - 4, 4))
          or WideSameText('.mil', Copy(Value, Length(Value) - 4, 4)) then
            Value := 'http://' + Value
        end;
        Hint := Value;
        if IsGlobalWorkOffline
        and (not IsURLAvailableOffline(Value)) then begin
          Hint := Hint + ' ' + SNotAvailableOffline;
          Windows.SetCursor(Screen.Cursors[GetOfflineHandCursor]);
        end else
          Windows.SetCursor(Screen.Cursors[crHandPoint]);
        if TntApplication.Hint <> Hint then
          Application.CancelHint;
      end;
    end;
  end;
end;

procedure TntRichEditLX_SetShowLinkHint(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; const Value: Boolean);
begin
  with Control, VarBlock do begin
    ShowLinkHint := Value;
    if not ShowLinkHint then
      TntRichEditLX_SetLinkHint(Control, VarBlock, '');
  end;
end;

type
  TAccessTntCustomRichEdit = class(TTntCustomRichEdit);

procedure TntRichEditLX_AfterInherited_CNNotify(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; var Message: TWMNotify);
type
  PENLink = ^TENLink;

  procedure CheckLinkMessage(EmLink: PENLink);
  var
    LinkText: WideString;
    LinkStart, LinkLength: Integer;
  begin
    with TAccessTntCustomRichEdit(Control), VarBlock do begin
      LinkStart := EmulatedCharPos(EMLink.chrg.cpMin) + 1;
      LinkLength := EmulatedCharPos(EMLink.chrg.cpMax) - EmulatedCharPos(EMLink.chrg.cpMin);
      LinkText := Copy(Text, LinkStart, LinkLength);
      case EMLink.msg of
        WM_LBUTTONUP:
          TntRichEditLX_DoLinkClick(Control, VarBlock, LinkText);
        WM_MOUSEMOVE:
          begin
            TntRichEditLX_SetLinkHint(Control, VarBlock, LinkText);
            LastLinkTime := GetTickCount;
          end;
      end;
    end;
  end;

begin
  if Message.NMHdr^.code = EN_LINK then
    CheckLinkMessage(PENLink(Message.NMHdr));
end;

procedure TntRichEditLX_AfterInherited_MouseDown(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock; out SimDblClick: Boolean);
var
  TimeElapsed: Cardinal;
begin
  SimDblClick := False;
  with VarBlock do begin
    TimeElapsed := Cardinal(GetTickCount - LastMouseDown);
    LastMouseDown := GetTickCount;
    if Is_WINE and (TimeElapsed > 0) and (TimeElapsed < GetDoubleClickTime) then
    begin
      SimDblClick := True;
    end;
  end;
end;

procedure TntRichEditLX_AfterInherited_MouseMove(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock);
begin
  with VarBlock do begin
    if (GetTickCount - LastLinkTime) > 100 then
      TntRichEditLX_SetLinkHint(Control, VarBlock, '');
  end;
end;

procedure TntRichEditLX_AfterInherited_CMMouseLeave(Control: TTntCustomRichEdit; var VarBlock: TRichEditLXVarBlock);
begin
  with VarBlock do begin
    if (GetTickCount - LastLinkTime) > 100 then
      TntRichEditLX_SetLinkHint(Control, VarBlock, '');
  end;
end;

{ TTntCustomRichEditLX }

constructor TTntCustomRichEditLX.Create(AOwner: TComponent);
begin
  inherited;
  TntRichEditLX_AfterInherited_Create(Self, FVarBlock);
end;

procedure TTntCustomRichEditLX.CreateWindowHandle(const Params: TCreateParams);
begin
  if not TntRichEditLX_CreateWindowHandle(Self, Params) then
    inherited;
end;

procedure TTntCustomRichEditLX.CreateWnd;
begin
  inherited;
  TntRichEditLX_AfterInherited_CreateWnd(Self, FVarBlock);
end;

procedure TTntCustomRichEditLX.SetAutoUrlDetect(const Value: Boolean);
begin
  TntRichEditLX_SetAutoUrlDetect(Self, FVarBlock, Value);
end;

procedure TTntCustomRichEditLX.DoLinkClick(const LinkText: WideString);
begin
  TntRichEditLX_DoLinkClick(Self, FVarBlock, LinkText);
end;

procedure TTntCustomRichEditLX.SetShowLinkHint(const Value: Boolean);
begin
  TntRichEditLX_SetShowLinkHint(Self, FVarBlock, Value);
end;

procedure TTntCustomRichEditLX.CNNotify(var Message: TWMNotify);
begin
  inherited;
  TntRichEditLX_AfterInherited_CNNotify(Self, FVarBlock, Message);
end;

procedure TTntCustomRichEditLX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SimDblClick: Boolean;
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseDown(Self, FVarBlock, SimDblClick);
  if SimDblClick then
    DblClick;
end;

procedure TTntCustomRichEditLX.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseMove(Self, FVarBlock);
end;

procedure TTntCustomRichEditLX.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseMove(Self, FVarBlock);
end;

{ TTntDBRichEditLX }

constructor TTntDBRichEditLX.Create(AOwner: TComponent);
begin
  inherited;
  TntRichEditLX_AfterInherited_Create(Self, FVarBlock);
end;

procedure TTntDBRichEditLX.CreateWindowHandle(const Params: TCreateParams);
begin
  if not TntRichEditLX_CreateWindowHandle(Self, Params) then
    inherited;
end;

procedure TTntDBRichEditLX.CreateWnd;
begin
  inherited;
  TntRichEditLX_AfterInherited_CreateWnd(Self, FVarBlock);
end;

procedure TTntDBRichEditLX.SetAutoUrlDetect(const Value: Boolean);
begin
  TntRichEditLX_SetAutoUrlDetect(Self, FVarBlock, Value);
end;

procedure TTntDBRichEditLX.DoLinkClick(const LinkText: WideString);
begin
  TntRichEditLX_DoLinkClick(Self, FVarBlock, LinkText);
end;

procedure TTntDBRichEditLX.SetShowLinkHint(const Value: Boolean);
begin
  TntRichEditLX_SetShowLinkHint(Self, FVarBlock, Value);
end;

procedure TTntDBRichEditLX.CNNotify(var Message: TWMNotify);
begin
  inherited;
  TntRichEditLX_AfterInherited_CNNotify(Self, FVarBlock, Message);
end;

procedure TTntDBRichEditLX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SimDblClick: Boolean;
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseDown(Self, FVarBlock, SimDblClick);
  if SimDblClick then
    DblClick;
end;

procedure TTntDBRichEditLX.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseMove(Self, FVarBlock);
end;

procedure TTntDBRichEditLX.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  TntRichEditLX_AfterInherited_MouseMove(Self, FVarBlock);
end;

initialization

finalization
  if FRichEdit40Module <> 0 then
    FreeLibrary(FRichEdit40Module);

end.

