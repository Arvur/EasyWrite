
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXDBCtrls;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Controls, TntDBCtrls, DBCtrls;

type
  TTntDBEditLX = class(TTntDBEdit)
  private
    FDataLink: TFieldDataLink;
    FInheritedEditingChange: TNotifyEvent;
    procedure EditingChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTntDBMemoLX = class(TTntDBMemo)
  private
    FDataLink: TFieldDataLink;
    FInheritedEditingChange: TNotifyEvent;
    procedure EditingChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadMemo; override;
  end;

  TTntDBRichEditBasicLX = class(TTntDBRichEdit)
  private
    FDataLink: TFieldDataLink;
    FInheritedEditingChange: TNotifyEvent;
    procedure EditingChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadMemo; override;
  end;

implementation

uses
  Messages, StdCtrls, DB;

{ TTntDBEditLX }

{$IFNDEF COMPILER_9_UP}
type TCustomEdit{TNT-ALLOW TCustomEdit} = class(StdCtrls.TCustomEdit{TNT-ALLOW TCustomEdit});
{$ENDIF}

constructor TTntDBEditLX.Create(AOwner: TComponent);
begin
  inherited;
  // wire-up event
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FInheritedEditingChange := FDataLink.OnEditingChange;
  FDataLink.OnEditingChange := EditingChange;
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

procedure TTntDBEditLX.EditingChange(Sender: TObject);
begin
  if Assigned(FInheritedEditingChange) then
    FInheritedEditingChange(Sender);
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

{ TTntDBMemoLX }

constructor TTntDBMemoLX.Create(AOwner: TComponent);
begin
  inherited;
  // wire-up event
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FInheritedEditingChange := FDataLink.OnEditingChange;
  FDataLink.OnEditingChange := EditingChange;
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

procedure TTntDBMemoLX.EditingChange(Sender: TObject);
begin
  if Assigned(FInheritedEditingChange) then
    FInheritedEditingChange(Sender);
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

procedure TTntDBMemoLX.LoadMemo;
begin
  inherited;
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

{ TTntDBRichEditBasicLX }

constructor TTntDBRichEditBasicLX.Create(AOwner: TComponent);
begin
  inherited;
  // wire-up event
  FDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0)) as TFieldDataLink;
  FInheritedEditingChange := FDataLink.OnEditingChange;
  FDataLink.OnEditingChange := EditingChange;
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

procedure TTntDBRichEditBasicLX.EditingChange(Sender: TObject);
begin
  if Assigned(FInheritedEditingChange) then
    FInheritedEditingChange(Sender);
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

procedure TTntDBRichEditBasicLX.LoadMemo;
begin
  inherited;
  // fix
  TCustomEdit{TNT-ALLOW TCustomEdit}(Self).ReadOnly := (not FDataLink.CanModify);
end;

end.
