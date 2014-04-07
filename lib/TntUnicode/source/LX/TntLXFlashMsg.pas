
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXFlashMsg;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, TntForms, TntLXUtils;

type
  TTntFlashForm = class(TTntForm)
  private
    procedure RedrawFrame;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetMessage(const Msg: WideString);
  end;

function FlashMessage(Msg: WideString): TComponent;
procedure UpdateFlash(Flash: TObject; Msg: WideString);
function GetFlash(Flash: TObject): WideString;

var
  SupressFlash: Boolean = False;
  FOnGlobalFlashMsg: TStringEvent;

implementation

{$R *.DFM}

uses
  Math, TntGraphics, TntLXVclUtils;

function FlashMessage(Msg: WideString): TComponent;
begin
  result := TTntFlashForm.Create(Application);
  with result as TTntFlashForm do begin
    ClientHeight := 0;
    SetMessage(Msg);
    if not SupressFlash then begin
      Show;
      RedrawFrame;
    end;
  end;
end;

procedure UpdateFlash(Flash: TObject; Msg: WideString);
begin
  with (Flash as TTntFlashForm) do 
    SetMessage(Msg);
end;

function GetFlash(Flash: TObject): WideString;
begin
  result := (Flash as TTntFlashForm).Caption;
end;

var
  FlashForms: TList;

procedure UpdateGlobalFlash;
var
  Msg: WideString;
begin
  if GetCurrentThreadID = MainThreadID then begin
    Msg := '';
    if FlashForms.Count > 0 then
      Msg := TTntFlashForm(FlashForms.Last).Caption;
    if Assigned(FOnGlobalFlashMsg) then
      FOnGlobalFlashMsg(Msg);
  end;
end;

{ TTntFlashForm }

constructor TTntFlashForm.Create(AOwner: TComponent);
begin
  inherited;
  FlashForms.Add(Self);
  BeginCursor(crHourGlass);
end;

destructor TTntFlashForm.Destroy;
begin
  EndCursor;
  FlashForms.Remove(Self);
  UpdateGlobalFlash;
  inherited;
end;

function GetNonClientWidth(Handle: THandle; const Text: WideString): Integer;
var
  Size: TSize;
  dc: HDC;
begin
  dc := GetWindowDC(Handle);
  Size.cX := 0;
  Size.cY := 0;
  Windows.GetTextExtentPoint32W(dc, PWideChar(Text), Length(Text), Size);
  Result := Size.cX;
  ReleaseDC(Handle, dc);
end;

procedure TTntFlashForm.RedrawFrame;
begin
  if Visible then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE
      or SWP_NOACTIVATE or SWP_NOZORDER or SWP_DRAWFRAME);
end;

procedure TTntFlashForm.SetMessage(const Msg: WideString);
begin
  if GetCurrentThreadID = MainThreadID then begin
    Width := Max(Width, GetNonClientWidth(Handle, Msg));
    Caption := Msg;
    RedrawFrame;
    UpdateGlobalFlash;
  end;
end;

initialization
  FlashForms := TList.Create;

finalization
  FreeAndNil(FlashForms);

end.
