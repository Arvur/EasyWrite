
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXMemoInputQueryFrm;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, TntLXForms;

type
  TTntMemoInputQueryFrm = class(TTntFormLX)
    TntMemo1: TTntMemo;
    OKBtn: TTntButton;
    CancelBtn: TTntButton;
    PromptLbl: TTntLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MemoInputQuery(const ACaption, APrompt: WideString; var Value: WideString; ReadOnly: Boolean = False): Boolean;

implementation

{$R *.dfm}

uses
  Consts;

function MemoInputQuery(const ACaption, APrompt: WideString; var Value: WideString; ReadOnly: Boolean = False): Boolean;
begin
  with TTntMemoInputQueryFrm.Create(nil) do
  try
    Caption := ACaption;
    PromptLbl.Caption := APrompt;
    CancelBtn.Caption := SMsgDlgCancel;
    OKBtn.Caption := SMsgDlgOK;
    TntMemo1.Lines.Text := Value;
    TntMemo1.ReadOnly := ReadOnly;
    Result := (ShowModal = mrOK);
    if Result then begin
      Value := TntMemo1.Lines.Text;
    end;
  finally
    Free;
  end;
end;

end.
