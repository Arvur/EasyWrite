
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXMapiUtils;

{$INCLUDE TntCompilers.inc}

interface

procedure SendMailMAPI(const Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: AnsiString; SendImmediately: Boolean = False);

implementation

uses
  Mapi, Windows, SysUtils, Forms, TntWindows, TntLXUtils, TntClasses;


function GetSendMailErrorMessage(const Code: Integer): WideString;
begin
  Assert(Code <> SUCCESS_SUCCESS);
  Result := '';
  case Code of
    MAPI_E_AMBIGUOUS_RECIPIENT:
      Result := 'A recipient matched more than one of the recipient descriptor structures and MAPI_DIALOG was not set. No message was sent.';
    MAPI_E_ATTACHMENT_NOT_FOUND:
      Result := 'The specified attachment was not found. No message was sent.';
    MAPI_E_ATTACHMENT_OPEN_FAILURE:
      Result := 'The specified attachment could not be opened. No message was sent.';
    MAPI_E_BAD_RECIPTYPE:
      Result := 'The type of a recipient was not MAPI_TO, MAPI_CC, or MAPI_BCC. No message was sent.';
    MAPI_E_FAILURE:
      Result := 'One or more unspecified errors occurred. No message was sent.';
    MAPI_E_INSUFFICIENT_MEMORY:
      Result := 'There was insufficient memory to proceed. No message was sent.';
    MAPI_E_INVALID_RECIPS:
      Result := 'One or more recipients were invalid or did not resolve to any address.';
    MAPI_E_LOGIN_FAILURE:
      Result := 'There was no default logon, and the user failed to log on successfully when the logon dialog box was displayed. No message was sent.';
    MAPI_E_TEXT_TOO_LARGE:
      Result := 'The text in the message was too large. No message was sent.';
    MAPI_E_TOO_MANY_FILES:
      Result := 'There were too many file attachments. No message was sent.';
    MAPI_E_TOO_MANY_RECIPIENTS:
      Result := 'There were too many recipients. No message was sent.';
    MAPI_E_UNKNOWN_RECIPIENT:
      Result := 'A recipient did not appear in the address list. No message was sent.';
    MAPI_E_USER_ABORT:
      Result := 'The user canceled one of the dialog boxes. No message was sent.';
    else
      Result := WideFormat('MAPISendMail resulted in an error (%d).', [Code]);
  end;
end;

procedure SendMailMAPI(const Subject, Body, FileName, SenderName, SenderEMail,
                  RecepientName, RecepientEMail: AnsiString; SendImmediately: Boolean = False);
{ adapted from http://www.delphifaq.com/fq/q5012.shtml }
var
  message: TMapiMessage;
  lpSender: TMapiRecipDesc;
  lpRecepient: array of TMapiRecipDesc;
  FileAttach: TMapiFileDesc;
  SM: TFNMapiSendMail;
  MAPIModule: HModule;
  RetVal: Integer;
  SendFlags: Cardinal;
  RecepList: TTntStringList;
  i: integer;
  Recep_DisplayName, Recep_EmailAddress: WideString;
  Recep_DisplayName_Array, Recep_EmailAddress_Array: array of AnsiString;
begin
  FillChar(message, SizeOf(message), 0);
  with message do
  begin
    if (Subject <> '') then
      lpszSubject := PAnsiChar(Subject);

    if (Body <> '') then
      lpszNoteText := PAnsiChar(Body);

    if (SenderEMail <> '') then begin
      lpSender.ulRecipClass := MAPI_ORIG;

      if (SenderName = '') then
        lpSender.lpszName := PAnsiChar(SenderEMail)
      else
        lpSender.lpszName := PAnsiChar(SenderName);

      lpSender.lpszAddress := PAnsiChar('SMTP:' + SenderEMail);
      lpSender.ulReserved := 0;
      lpSender.ulEIDSize := 0;
      lpSender.lpEntryID := nil;
      lpOriginator := @lpSender;
    end;
    if (RecepientEMail <> '') then
    begin
      RecepList := TTntStringList.Create;
      try
        BuildNormalizedEmailList(RecepList, RecepientEmail, RecepientName, True, True);
        if RecepList.Count > 0 then
        begin
          SetLength(lpRecepient, RecepList.Count);
          SetLength(Recep_DisplayName_Array, RecepList.Count);
          SetLength(Recep_EmailAddress_Array, RecepList.Count);
          for i := 0 to RecepList.Count - 1 do begin
            ParseEmailAddress(RecepList[i], RecepientName, Recep_EmailAddress, Recep_DisplayName);
            Recep_DisplayName_Array[i] := Recep_DisplayName;
            Recep_EmailAddress_Array[i] := 'SMTP:' + Recep_EmailAddress;
            lpRecepient[i].ulRecipClass := MAPI_TO;
            lpRecepient[i].lpszName := PAnsiChar(Recep_DisplayName_Array[i]);
            lpRecepient[i].lpszAddress := PAnsiChar(Recep_EmailAddress_Array[i]);
            lpRecepient[i].ulReserved := 0;
            lpRecepient[i].ulEIDSize := 0;
            lpRecepient[i].lpEntryID := nil;
          end;
          nRecipCount := Length(lpRecepient);
          lpRecips := @lpRecepient[0];
        end;
      finally
        FreeAndNil(RecepList);
      end;
    end else begin
      lpRecips := nil
    end;

    if (FileName='') then begin
      nFileCount := 0;
      lpFiles := nil;
    end else begin
      FillChar(FileAttach, SizeOf(FileAttach), 0);
      FileAttach.nPosition := Cardinal($FFFFFFFF);
      FileAttach.lpszPathName := PAnsiChar(FileName);
      nFileCount := 1;
      lpFiles := @FileAttach;
    end;
  end;

  MAPIModule := Tnt_LoadLibraryW(PWideChar(WideString(MAPIDLL)));
  if MAPIModule=0 then
    raise Exception.Create('Mapi32.dll is not available at this time.');
  try
    SM := GetProcAddress(MAPIModule, 'MAPISendMail');
    if @SM = nil then
      raise Exception.Create('MAPISendMail is not available at this time (in Mapi32.dll).');

    SendFlags := MAPI_LOGON_UI;
    if (not SendImmediately) then
      SendFlags := SendFlags or MAPI_DIALOG;
    {$IFDEF COMPILER_9_UP}
    RetVal := SM(0, Application.ActiveFormHandle, message, SendFlags, 0);
    {$ELSE}
    RetVal := SM(0, Application.Handle, message, SendFlags, 0);
    {$ENDIF}
    if RetVal = MAPI_USER_ABORT then
      Abort;
    if RetVal <> SUCCESS_SUCCESS then
      raise Exception.Create(GetSendMailErrorMessage(RetVal));
  finally
    FreeLibrary(MAPIModule);
  end;
end;

end.
