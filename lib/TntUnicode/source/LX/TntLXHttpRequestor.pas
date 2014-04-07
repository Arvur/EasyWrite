
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXHttpRequestor;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Windows, Forms, SysUtils,
  TntClasses,
  TntLXUtils, WinInet, TntLXSafeWinInet;

const
  HTTPS_PROTOCOL = 'https://'; { do not localize }
  HTTP_PROTOCOL  = 'http://';
  FILE_PROTOCOL  = 'file://';

type
  TProgressNotifyEvent = procedure(BytesDownloaded, TotalAvailable: Int64) of object;
  TExceptionEvent = procedure(E: Exception) of object;
  TRequestDoneEvent = procedure(Stream: TStream; ErrorsOccurred: Boolean) of object;

  TDownloadOption = (doStream, doSizeOnly, doNone);

  TTntHttpRequestor = class(TComponent)
  private
    ThreadedWinInetHttpRequestSender: TComponent;
    hSession: HINTERNET;
    hConnect: HINTERNET;
    FOnMessage: TStringEvent;
    FOnRequestDone: TRequestDoneEvent;
    FOnProgress: TProgressNotifyEvent;
    FSecure: Boolean;
    FQuiet: Boolean;
    FDownloadOption: TDownloadOption;
    procedure DoRequestDone(const ErrorsOccurred: Boolean);
  private
    TempFile_InternalStream: WideString;
    InternalStream: TTntFileStream;
    FServer, FServlet: WideString;
    procedure Disconnect;
  protected
    procedure DoMessage(Msg: WideString);
    procedure DoProgress(BytesDownloaded, TotalAvailable: Int64);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StopRequest;
    procedure StartRequest(Port: INTERNET_PORT;
      Server, Servlet, UserName, Password: WideString; PostData, Message: WideString);
  published
    property Secure: Boolean read FSecure write FSecure default False;
    property Quiet: Boolean read FQuiet write FQuiet default False;
    property DownloadOption: TDownloadOption read FDownloadOption write FDownloadOption default doStream;
    property OnMessage: TStringEvent read FOnMessage write FOnMessage;
    property OnRequestDone: TRequestDoneEvent read FOnRequestDone write FOnRequestDone;
    property OnProgress: TProgressNotifyEvent read FOnProgress write FOnProgress;
  end;

resourcestring
  SNoInternetDownload = 'Internet content was not downloaded.';
  SKBytesReceivedMsg = 'Received %d KB';
  SDefaultWaitMsg = 'Please wait...';
  SRequestCancelled = 'Request cancelled.';

implementation

uses
  Math, TntSysUtils, TntLXVerUtils, TntLXClasses;

resourcestring
  SConnectionNotAvailable = 'A connection to the Internet is not available to this application.';

type
  EWinInetError = class(ETntGeneralError);

function GetWinInetErrorMsg(ErrorCode: Integer): WideString;
begin
  Result := Trim(WideLibraryErrorMessage('wininet', GetModuleHandle('wininet'), ErrorCode));
end;

function WinInetIsUnicode: Boolean;
begin
  Result := (GetMajorVersion(WinInetVersion) >= 5);
end;

function GetHttpQueryInfoString(h_Request: HINTERNET; dwInfoLevel: DWORD): WideString;
var
  NumChars: Cardinal;
  Dummy: Cardinal;
  Result_Ansi: AnsiString;
begin
  Dummy := 0;
  NumChars := 0;
  if WinInetIsUnicode then begin
    HttpQueryInfoW(h_Request, dwInfoLevel, nil, NumChars, Dummy);
    SetLength(Result, NumChars);
    Dummy := 0;
    HttpQueryInfoW(h_Request, dwInfoLevel, PWideChar(Result), NumChars, Dummy);
  end else begin
    HttpQueryInfoA(h_Request, dwInfoLevel, nil, NumChars, Dummy);
    SetLength(Result_Ansi, NumChars);
    Dummy := 0;
    HttpQueryInfoA(h_Request, dwInfoLevel, PAnsiChar(Result_Ansi), NumChars, Dummy);
    Result := Result_Ansi;
  end;
end;

{ TTntWinInetHttpRequestThread }

type
  TTntWinInetHttpRequestThread = class(TThread)
  private
    FTotalBytesRead: Int64;
    FTotalBytesAvailable: Int64;
    FLastException: Exception;
    procedure UpdateProgress;
    procedure ShowThreadException;
    procedure OpenHttpRequest;
    function SendHttpRequest: Integer;
    procedure HandleSendError(const dwErrorCode: Integer;
      var Re_OpenDesired, Re_SendDesired: Boolean);
    procedure CalculateSize;
    procedure DownloadToStream;
  private
    hConnect: HINTERNET;
    Server: WideString;
    Servlet: WideString;
    Method: WideString;
    Flags: Cardinal;
    PostStr: WideString;
    Stream: TStream;
    DownloadOption: TDownloadOption;
    Quiet: Boolean;
    //--
    OnProgress: TProgressNotifyEvent;
    OnException: TExceptionEvent;
    FormHandle: THandle;
  protected
    procedure Execute; override;
  public
    H_Request: HINTERNET;
  end;

procedure TTntWinInetHttpRequestThread.UpdateProgress;
begin
  if Assigned(OnProgress) then
    OnProgress(FTotalBytesRead, FTotalBytesAvailable);
end;

procedure TTntWinInetHttpRequestThread.ShowThreadException;
begin
  if Assigned(OnException) then
    OnException(FLastException);
end;

var
  IgnoreCertificateDate: TTntStringList;
  IgnoreCertificateName: TTntStringList;
  IgnoreCertificateCA: TTntStringList;

procedure TTntWinInetHttpRequestThread.OpenHttpRequest;
var
  Accept: WideString;
  AcceptTypes: array[0..1] of PWideChar;
  Ansi_AcceptTypes: array[0..1] of PAnsiChar;
begin
  Accept := 'Accept: */*'; { do not localize }

  // Open Request
  if IgnoreCertificateDate.IndexOf(Server) <> -1 then
    Flags := Flags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;

  if IgnoreCertificateName.IndexOf(Server) <> -1 then
    Flags := Flags or INTERNET_FLAG_IGNORE_CERT_CN_INVALID;

  if WinInetIsUnicode then begin
    AcceptTypes[0] := PWideChar(Accept);
    AcceptTypes[1] := nil;
    h_Request := HttpOpenRequestW(hConnect, PWideChar(Method),
      PWideChar(Servlet), nil, nil, @AcceptTypes[0],
        INTERNET_FLAG_KEEP_CONNECTION or Flags, 0);
  end else begin
    Ansi_AcceptTypes[0] := PAnsiChar(AnsiString(Accept));
    Ansi_AcceptTypes[1] := nil;
    h_Request := HttpOpenRequestA(hConnect, PAnsiChar(AnsiString(Method)),
      PAnsiChar(AnsiString(Servlet)), nil, nil, @Ansi_AcceptTypes[0],
        INTERNET_FLAG_KEEP_CONNECTION or Flags, 0);
  end;

  if h_Request = nil then
    raise EWinInetError.Create(GetWinInetErrorMsg(GetLastError));
end;

function TTntWinInetHttpRequestThread.SendHttpRequest: Integer;
var
  Hdrs: WideString;
  dwFlags: DWORD;
  dwBuffLen: DWORD;
  Success: Boolean;
begin
  Hdrs := 'Content-Type: application/x-www-form-urlencoded'; { do not localize }

  if IgnoreCertificateCA.IndexOf(Server) <> -1 then begin
    dwBuffLen := sizeof(dwFlags);
    InternetQueryOption(H_Request, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, dwBuffLen);
    dwFlags := dwFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
    InternetSetOption(H_Request, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, sizeof(dwFlags));
  end;

  if WinInetIsUnicode then begin
    Success := HttpSendRequestW(h_Request, PWideChar(Hdrs), Length(Hdrs),
      PAnsiChar(AnsiString(PostStr)), Length(AnsiString(PostStr)) * SizeOf(AnsiChar))
  end else begin
    Success := HttpSendRequestA(h_Request, PAnsiChar(AnsiString(Hdrs)), Length(AnsiString(Hdrs)),
      PAnsiChar(AnsiString(PostStr)), Length(AnsiString(PostStr)) * SizeOf(AnsiChar));
  end;

  if (not Success) then
    result := GetLastError
  else
    result := 0;
end;

procedure TTntWinInetHttpRequestThread.HandleSendError(const dwErrorCode: Integer;
  var Re_OpenDesired, Re_SendDesired: Boolean);
var
  NilP: Pointer;
  DlgResult: Integer;
begin
  if Quiet then begin
    if (dwErrorCode <> 0) then
      raise EWinInetError.Create(GetWinInetErrorMsg(dwErrorCode));
    exit;
  end;

  if  (dwErrorCode <> 0)
  and (dwErrorCode <> ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR)
  and (dwErrorCode <> ERROR_INTERNET_INCORRECT_PASSWORD)
  and (dwErrorCode <> ERROR_INTERNET_INVALID_CA)
  and (dwErrorCode <> ERROR_INTERNET_POST_IS_NON_SECURE)
  and (dwErrorCode <> ERROR_INTERNET_SEC_CERT_CN_INVALID)
  and (dwErrorCode <> ERROR_INTERNET_SEC_CERT_DATE_INVALID) then begin
    // normal wininet error
    raise EWinInetError.Create(GetWinInetErrorMsg(dwErrorCode))
  end else begin
    // analyze authentication error
    NilP := nil;
    DlgResult := InternetErrorDlg(FormHandle, h_Request, dwErrorCode,
                             FLAGS_ERROR_UI_FILTER_FOR_ERRORS or
                             FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                             FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,
                             NilP);

    if (DlgResult = ERROR_CANCELLED) then begin
      // CANCEL
      raise EWinInetError.Create(SRequestCancelled);

    end else if (dwErrorCode = ERROR_INTERNET_SEC_CERT_DATE_INVALID) then begin
      // ignore certification date for this server
      IgnoreCertificateDate.Add(Server);
      Re_OpenDesired := True;

    end else if (dwErrorCode = ERROR_INTERNET_SEC_CERT_CN_INVALID) then begin
      // ignore certification name for this server
      IgnoreCertificateName.Add(Server);
      Re_OpenDesired := True;

    end else if (dwErrorCode = ERROR_INTERNET_INVALID_CA) then begin
      // retry...
      IgnoreCertificateCA.Add(Server);
      Re_SendDesired := True; {MSKB: ArticleID Q182888}

    end else if (DlgResult = ERROR_INTERNET_FORCE_RETRY) then begin
      // retry...
      Re_SendDesired := True;
    end;
  end;
end;

procedure TTntWinInetHttpRequestThread.CalculateSize;
begin
  if Terminated then exit;
  FTotalBytesAvailable := StrToInt64Def(GetHttpQueryInfoString(h_Request, HTTP_QUERY_CONTENT_LENGTH), 0);

  if Terminated then exit;
  FTotalBytesRead := 0;
  Synchronize(UpdateProgress);
end;

procedure TTntWinInetHttpRequestThread.DownloadToStream;
var
  BytesAvailable: Cardinal;
  Buf: array of byte;
  BytesRead: Cardinal;
begin
  if Terminated then
    exit;
  Assert(DownloadOption = doStream);
  Assert(Stream <> nil);
  repeat
    if Terminated then exit;
    InternetQueryDataAvailable(h_Request, BytesAvailable, 0, 0);
    BytesAvailable := Min(BytesAvailable, 1024);
    // Allocate a buffer for the file.
    SetLength(Buf, BytesAvailable);
    if BytesAvailable > 0 then begin
      // Read the file into the buffer.
      if not InternetReadFile(h_Request, @Buf[0], BytesAvailable, BytesRead) then
        raise EWinInetError.Create(GetWinInetErrorMsg(GetLastError));

      if Terminated then exit;

      SetLength(Buf, BytesRead);
      Stream.Write(Buf[0], BytesRead);
      Inc(FTotalBytesRead, BytesRead);

      if Terminated then exit;
      Synchronize(UpdateProgress);
    end;
  until (BytesAvailable = 0);
end;

procedure TTntWinInetHttpRequestThread.Execute;
var
  dwErrorCode: Integer;
  Re_OpenDesired: Boolean;
  Re_SendDesired: Boolean;
begin
  try
    // Open and Send Request  (Handle special send errors)
    repeat
      Re_OpenDesired := False;
      // Open Request
      OpenHttpRequest;
      repeat
        Re_SendDesired := False;
        // Send Request
        dwErrorCode := SendHttpRequest;
        if Terminated then exit;
        // Handle special send errors (some errors are hidden such as proxy authentication)
        HandleSendError(dwErrorCode, Re_OpenDesired, Re_SendDesired);
      until (not Re_SendDesired) or Terminated;
    until (not Re_OpenDesired) or Terminated;

    // calculate size
    if DownloadOption in [doSizeOnly, doStream] then
      CalculateSize;

    // download stream
    if DownloadOption in [doStream] then
      DownloadToStream;

  except
    on E: Exception do begin
      if not Terminated then begin
        FLastException := E;
        Synchronize(ShowThreadException);
      end;
    end;
  end;
end;

{ TTntThreadedWinInetHttpRequestSender }
type
  TTntThreadedWinInetHttpRequestSender = class(TComponent)
  private
    FQuiet: Boolean;
    FErrorsOccured: Boolean;
    RequestThread : TThread;

    FOnProgress: TProgressNotifyEvent;
    FOnRequestDone: TBooleanEvent;

    procedure DoThreadException(E: Exception);
    procedure DoPostThreadDone(Sender: TObject);
  public
    destructor Destroy; override;
    procedure StartRequest(_HConnect: HINTERNET; Server: WideString; Servlet: WideString;
      Method: WideString; Flags: Cardinal; PostStr: WideString; Stream: TStream;
        DownloadOption: TDownloadOption; Quiet: Boolean);
    procedure StopRequest;
  published
    property OnProgress: TProgressNotifyEvent read FOnProgress write FOnProgress;
    property OnRequestDone: TBooleanEvent read FOnRequestDone write FOnRequestDone;
  end;

destructor TTntThreadedWinInetHttpRequestSender.Destroy;
begin
  StopRequest;
  inherited;
end;

procedure TTntThreadedWinInetHttpRequestSender.StopRequest;
begin
  if RequestThread <> nil then begin
    RequestThread.OnTerminate := nil;
    (RequestThread as TTntWinInetHttpRequestThread).OnProgress := nil;
    (RequestThread as TTntWinInetHttpRequestThread).OnException := nil;
    RequestThread.Terminate;
    if (RequestThread as TTntWinInetHttpRequestThread).h_Request <> nil then
      InternetCloseHandle((RequestThread as TTntWinInetHttpRequestThread).h_Request);
    RequestThread := nil;
  end;
end;

procedure TTntThreadedWinInetHttpRequestSender.DoThreadException(E: Exception);
begin
  FErrorsOccured := True;
  if (not FQuiet) then
    SafeShowException(E);
end;

procedure TTntThreadedWinInetHttpRequestSender.DoPostThreadDone(Sender: TObject);
begin
  try
    StopRequest;
    if Assigned(OnRequestDone) then
      OnRequestDone(FErrorsOccured);
  except
    on E: Exception do begin
      SafeShowException(E);
      try
        if Assigned(OnRequestDone) then
          OnRequestDone(True);
      except
        on E: Exception do
          SafeShowException(E);
      end;
    end;
  end;
end;

function GetOwnerForm(C: TComponent): TForm{TNT-ALLOW TForm};
var
  F: TComponent;
begin
  F := C;
  repeat
    F := F.Owner;
  until (F = nil) or (F is TForm{TNT-ALLOW TForm});
  result := F as TForm{TNT-ALLOW TForm};
end;

function GetDialogOwnerHandle(C: TComponent): THandle;
begin
  if GetOwnerForm(C) <> nil then
    result := GetOwnerForm(C).Handle
  else begin
    {$IFDEF COMPILER_9_UP}
    result := Application.ActiveFormHandle;
    {$ELSE}
    result := Application.Handle;
    {$ENDIF}
  end;
end;

procedure TTntThreadedWinInetHttpRequestSender.StartRequest(
      _HConnect: HINTERNET;
      Server: WideString;
      Servlet: WideString;
      Method: WideString;
      Flags: Cardinal;
      PostStr: WideString;
      Stream: TStream;
      DownloadOption: TDownloadOption;
      Quiet: Boolean);
begin
  Force(RequestThread = nil, 'Internal Error: Can only process one request at a time.');
  RequestThread := TTntWinInetHttpRequestThread.Create(True);
  FQuiet := Quiet;

  // initialize thread
  (RequestThread as TTntWinInetHttpRequestThread).HConnect   := _HConnect;
  (RequestThread as TTntWinInetHttpRequestThread).Server     := Server;
  (RequestThread as TTntWinInetHttpRequestThread).Servlet    := Servlet;
  (RequestThread as TTntWinInetHttpRequestThread).Method     := Method;
  (RequestThread as TTntWinInetHttpRequestThread).Flags      := Flags;
  (RequestThread as TTntWinInetHttpRequestThread).PostStr    := PostStr;
  (RequestThread as TTntWinInetHttpRequestThread).Stream     := Stream;
  (RequestThread as TTntWinInetHttpRequestThread).DownloadOption := DownloadOption;
  (RequestThread as TTntWinInetHttpRequestThread).Quiet      := Quiet;

  if DownloadOption = doStream then
    Assert(Stream <> nil);

  // connect to self
  (RequestThread as TTntWinInetHttpRequestThread).OnProgress := OnProgress;
  (RequestThread as TTntWinInetHttpRequestThread).OnException := DoThreadException;
  (RequestThread as TTntWinInetHttpRequestThread).FormHandle := GetDialogOwnerHandle(Self);
  RequestThread.OnTerminate := DoPostThreadDone;

  // start thread
  RequestThread.FreeOnTerminate := True;
  FErrorsOccured := False;
  RequestThread.Resume;
end;

{ TTntHttpRequestor }

constructor TTntHttpRequestor.Create(AOwner: TComponent);
begin
  inherited;
  TempFile_InternalStream := GetGarbageCollectedTempFile;
  InternalStream := TTntFileStream.Create(TempFile_InternalStream, fmCreate);

  ThreadedWinInetHttpRequestSender := TTntThreadedWinInetHttpRequestSender.Create(Self);
  (ThreadedWinInetHttpRequestSender as TTntThreadedWinInetHttpRequestSender).OnRequestDone := DoRequestDone;
  (ThreadedWinInetHttpRequestSender as TTntThreadedWinInetHttpRequestSender).OnProgress := DoProgress;
end;

destructor TTntHttpRequestor.Destroy;
begin
  InternalStream.Free;
  WideDeleteFile(TempFile_InternalStream);
  Disconnect;
  inherited;
end;

procedure TTntHttpRequestor.Disconnect;
begin
  if (hConnect <> nil) then begin
    InternetCloseHandle(hConnect);
    hConnect := nil;
  end;
  if (hSession <> nil) then begin
    InternetCloseHandle(hSession);
    hSession := nil;
  end;
end;

procedure TTntHttpRequestor.StopRequest;
begin
  (ThreadedWinInetHttpRequestSender as TTntThreadedWinInetHttpRequestSender).StopRequest;
  Disconnect;
end;

procedure TTntHttpRequestor.DoRequestDone(const ErrorsOccurred: Boolean);
begin
  Disconnect;
  if Assigned(OnRequestDone) then begin
    if ErrorsOccurred then
      OnRequestDone(nil, True)
    else
      OnRequestDone(InternalStream, False);
  end;
end;

procedure TTntHttpRequestor.DoMessage(Msg: WideString);
begin
  if Assigned(OnMessage) then
    OnMessage(Msg);
end;

procedure TTntHttpRequestor.DoProgress(BytesDownloaded, TotalAvailable: Int64);
begin
  DoMessage(WideFormat(SKBytesReceivedMsg, [BytesDownloaded div 1024]));
  if Assigned(OnProgress) then
    OnProgress(BytesDownloaded, TotalAvailable);
end;

procedure TTntHttpRequestor.StartRequest(Port: INTERNET_PORT; Server, Servlet,
  UserName, Password, PostData, Message: WideString);
var
  Method: WideString;
  Flags: Cardinal;
  Stream: TStream;
begin
  if (hSession <> nil) or (hConnect <> nil) then
    raise ETntInternalError.Create('Internal Error: Previous request is already in progress.');

  if not InternetIsAllowed then
    raise ETntGeneralError.Create(SInternetConnectionDisabled);
  if IsGlobalWorkOffline
  and (Quiet or (not PromptToGoOnline(Server))) then
    Abort;
  if not InternetIsConnected(Quiet) then
    raise EExtraInfoException.Create(SConnectionNotAvailable,
      'If Internet Explorer is able to connect to the Internet, make sure that a firewall like ZoneAlarm is not blocking this application.');
  if WinInetIsUnicode then begin
    hSession := WinCheckP(InternetOpenW(PWideChar(WideString(Application.Title)),
      INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0));
    hConnect := WinCheckP(InternetConnectW(hSession, PWideChar(Server),
      Port, PWideChar(UserName), PWideChar(Password), INTERNET_SERVICE_HTTP, 0, 0));
  end else begin
    hSession := WinCheckP(InternetOpenA(PAnsiChar(Application.Title),
      INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0));
    hConnect := WinCheckP(InternetConnectA(hSession, PAnsiChar(AnsiString(Server)),
      Port, PAnsiChar(AnsiString(UserName)), PAnsiChar(AnsiString(Password)),
        INTERNET_SERVICE_HTTP, 0, 0));
  end;

  Self.FServer := Server;
  Self.FServlet := Servlet;

  // show message
  DoMessage(Message);

  if (DownloadOption <> doStream) then
    Stream := nil
  else begin
    InternalStream.Size := 0;
    Stream := InternalStream;
  end;

  if PostData <> '' then
    Method := 'POST' { do not localize }
  else
    Method := 'GET'; { do not localize }


  Flags := INTERNET_FLAG_PRAGMA_NOCACHE //Forces the request to be resolved by the origin server, even if a cached copy exists on the proxy.
        or INTERNET_FLAG_RELOAD; //Forces a download of the requested file, object, or directory listing from the origin server, not from the cache.

  if Secure then
    Flags := Flags or INTERNET_FLAG_SECURE;

  // start request
  (ThreadedWinInetHttpRequestSender as TTntThreadedWinInetHttpRequestSender).StartRequest(
    hConnect, FServer, FServlet, Method, Flags, PostData, Stream, DownloadOption, Quiet);
end;

initialization
  IgnoreCertificateDate := TTntStringList.Create;
  IgnoreCertificateDate.Sorted := True;
  IgnoreCertificateDate.Duplicates := dupIgnore;

  IgnoreCertificateName := TTntStringList.Create;
  IgnoreCertificateName.Sorted := True;
  IgnoreCertificateName.Duplicates := dupIgnore;

  IgnoreCertificateCA := TTntStringList.Create;
  IgnoreCertificateCA.Sorted := True;
  IgnoreCertificateCA.Duplicates := dupIgnore;

finalization
  IgnoreCertificateDate.Free;
  IgnoreCertificateName.Free;
  IgnoreCertificateCA.Free;

end.
