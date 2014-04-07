
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXHotLabel;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TntStdCtrls;

type
  TTntHotLabel = class(TTntLabel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOldColor: TColor;
    FHotColor: TColor;
    FHotCursor: TCursor;
    FOldCursor: TCursor;
    FWebLink: WideString;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property HotColor: TColor read FHotColor write FHotColor default clBlue;
    property HotCursor: TCursor read FHotCursor write FHotCursor default crHandPoint;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property WebLink: WideString read FWebLink write FWebLink;
  end;

implementation

uses
  TntLXSafeWinInet;

{ TTntHotLabel }
constructor TTntHotLabel.Create(AOwner: TComponent);
begin
  inherited;
  FOldColor := clBlack;
  FHotColor := clBlue;
  FOldCursor := crDefault;
  FHotCursor := crHandPoint;
end;

procedure TTntHotLabel.CMMouseEnter(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then begin
    FOldColor := Font.Color;
    Font.Color := HotColor;
    FOldCursor := Cursor;
    if IsGlobalWorkOffline and (not IsURLAvailableOffline(WebLink)) and (HotCursor = crHandPoint) then
      Cursor := GetOfflineHandCursor
    else
      Cursor := FHotCursor;
    if Assigned(OnMouseEnter) then
      OnMouseEnter(Self);
  end;
  inherited;
end;

procedure TTntHotLabel.CMMouseLeave(var Message: TMessage);
begin
  Font.Color := FOldColor;
  Cursor := FOldCursor;
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);

  inherited;
end;

procedure TTntHotLabel.Click;
begin
  inherited;
  if WebLink <> '' then
    SafeOpenURL(WebLink);
end;

end.

