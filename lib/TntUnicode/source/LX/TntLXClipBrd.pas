
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXClipBrd;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Windows, TntClipbrd;

type
  TTntClipboardLX = class(TTntClipboard)
  private
    procedure SetAsHTML(const Value: WideString);
  public
    property AsHTML: WideString write SetAsHTML;
  end;

function TntClipboardLX: TTntClipboardLX;

implementation

uses
  SysUtils, TntSysUtils;

{ TTntClipboardLX }

var
  CF_HTML: Word;

procedure TTntClipboardLX.SetAsHTML(const Value: WideString);

  function FormatHTMLClipboardHeader(HTMLText: UTF8String): UTF8String;
  // found this function here:
  //   http://www.swissdelphicenter.ch/torry/showcode.php?id=1391
  //   Author: Author: Thomas Stutz
  const
    CrLf = #13#10;
  begin
    Result := 'Version:0.9' + CrLf;
    Result := Result + 'StartHTML:-1' + CrLf;
    Result := Result + 'EndHTML:-1' + CrLf;
    Result := Result + 'StartFragment:000081' + CrLf;
    Result := Result + 'EndFragment:같같같' + CrLf;
    Result := Result + HTMLText + CrLf;
    Result := Tnt_WideStringReplace(Result, '같같같', WideFormat('%.6d', [Length(Result)]), []);
  end;

var
  UTF8: AnsiString;
begin
  Open;
  try
    AsWideText := Value;
    UTF8 := FormatHTMLClipboardHeader(UTF8Encode(Value));
    SetBuffer(CF_HTML, PAnsiChar(UTF8)^, (Length(UTF8) + 1) * SizeOf(AnsiChar));
  finally
    Close;
  end;
end;

//------------------------------------------

var
  GTntClipboardLX: TTntClipboardLX;

function TntClipboardLX: TTntClipboardLX;
begin
  if GTntClipboardLX = nil then
    GTntClipboardLX := TTntClipboardLX.Create;
  Result := GTntClipboardLX;
end;

initialization
  CF_HTML := RegisterClipboardFormat('HTML Format');

finalization
  GTntClipboardLX.Free;

end.
