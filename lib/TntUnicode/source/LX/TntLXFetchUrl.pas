
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXFetchUrl;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls,
  TntClasses, TntForms, TntButtons, TntComCtrls, WinInet,
  TntStdCtrls, TntLXClasses, TntLXForms, TntLXSafeWinInet, TntLXHttpRequestor;

type
  TTntWebDownloadProgressForm = class(TTntFormLX)
    CancelBtn: TTntButton;
    Label1: TTntLabel;
    Animate1: TAnimate;
    ProgressBar1: TTntProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    FLastTotalAvailable: Cardinal;
    procedure FetchUrl1Message(const Value: WideString);
    procedure FetchUrl1Progress(BytesDownloaded, TotalAvailable: Int64);
  end;

  TTntFetchUrl = class(TTntHttpRequestor)
  public
    LastFetchUrl: WideString;
    LastFetchPostData: WideString;
    procedure FetchUrl(const Url, UserName, Password, PostStr: WideString);
  end;

const
  POST_DATA_LINE_PREFIX = 'Post Data = ';

function ErrorTextForDownload(const URL, PostData: WideString; Data: TTntStrings): WideString;
procedure RaiseErrorForDownload(const URL, PostData: WideString; sList: TTntStrings);
procedure GetUrlParts(const Url: WideString; var Protocol, ServerName, ObjectName: WideString;
  var Port: INTERNET_PORT);
procedure FetchUrl(_Strings: TTntStrings; _Url, _UserName, _Password: WideString;
  _PostData: WideString; _Caption: WideString; _OwnerForm: TCustomForm; _Stream: TStream = nil;
    _TimeOutSeconds: Cardinal = 0);
function FetchUrl_Size(_Url, _UserName, _Password: WideString;
  _PostData: WideString; _Caption: WideString; _OwnerForm: TCustomForm;
    _TimeOutSeconds: Cardinal = 0): Int64;
function FetchUrlInBackground(_Url, _UserName, _Password: WideString; _OnSuccess: TNotifyEvent; Owner: TComponent;
  _Post: WideString = ''): TObject;            {_OnSuccess: Sender is a TStream (or nil if error)}

resourcestring
  SDownloadError = 'Unable to download information from server. You may try again later.';

type
  EDownloadError = class(EExtraInfoException)
  public
    FRawDownload: WideString;
    constructor Create(const Msg: WideString; const ExtraInfo, RawDownload: WideString);
    procedure DisplayHtmlError;
  end;

implementation

uses
  SysUtils, TntSysUtils, TntLXUtils, Consts, TntLXVclUtils;

resourcestring
  SConnecting = 'Connecting...';
  SDetailsFile = 'For details see this file: %s';
  SRequestTimedOut = 'Request timed out.';
  SRequestingWebFileSize = 'Requesting file size from Web...';

{$R *.DFM}

function ErrorTextForDownload(const URL, PostData: WideString; Data: TTntStrings): WideString;
begin
  Result :=  'URL = ' + URL
    + CRLF + POST_DATA_LINE_PREFIX + PostData
    + CRLF + 'File Size = ' + IntToStr(Length(Data.Text))
    + CRLF + '---- Downloaded Data BEGIN ----'
    + CRLF + Data.Text
    + CRLF + '---- Downloaded Data END ----';
end;

procedure RaiseErrorForDownload(const URL, PostData: WideString; sList: TTntStrings);
begin
  raise EDownloadError.Create(SDownloadError, ErrorTextForDownload(URL, PostData, sList), sList.Text);
end;

procedure GetUrlParts(const Url: WideString; var Protocol, ServerName, ObjectName: WideString;
  var Port: INTERNET_PORT);
const
  END_OF_URL_PROTOCOL = '://';
var
  TempUrl: WideString;
  SlashPos: Integer;
begin
  TempUrl := Url;
  // find protocol
  SlashPos := Pos(END_OF_URL_PROTOCOL, TempUrl);
  if SlashPos = 0 then
    Protocol := ''
  else begin
    Protocol := Copy(TempUrl, 1, SlashPos + Length(END_OF_URL_PROTOCOL) - 1);
  end;
  // get rid of protocol
  Delete(TempUrl, 1, Length(Protocol));

  // Determine default port from protocol
  if Protocol = '' then
    Protocol := HTTP_PROTOCOL;
  if WideSameText(Protocol, HTTPS_PROTOCOL) then
    Port := INTERNET_DEFAULT_HTTPS_PORT
  else if WideSameText(Protocol, HTTP_PROTOCOL) then
    Port := INTERNET_DEFAULT_HTTP_PORT
  else if WideSameText(Protocol, FILE_PROTOCOL) then begin
    Port := INTERNET_INVALID_PORT_NUMBER;
    ServerName := WideExtractFileDrive(TempUrl);
    ObjectName := TempUrl;
    exit;
  end else
    raise ETntInternalError.CreateFmt('Internal Error: Port [%s] not supported.', [Protocol]);

  // find server and object
  SlashPos := Pos('/', TempUrl);
  if SlashPos = 0 then begin
    ServerName := TempUrl;
    ObjectName := '';
  end else begin
    ServerName := Copy(TempUrl, 1, SlashPos - 1);
    Delete(TempUrl, 1, SlashPos);
    ObjectName := TempUrl;
  end;

  // handle any port override
  SlashPos := Pos(':', ServerName);
  if SlashPos <> 0 then begin
    Port := StrToInt(Copy(ServerName, SlashPos + 1, Length(ServerName)));
    ServerName := Copy(ServerName, 1, SlashPos - 1);
  end;
end;

{ TTntFetchUrl }

procedure TTntFetchUrl.FetchUrl(const Url, UserName, Password, PostStr: WideString);
var
  Port: INTERNET_PORT;
  Protocol: WideString;
  ServerName: WideString;
  ObjectName: WideString;
  FileStream: TTntFileStream;
begin
  if Trim(Url) = '' then
    raise Exception.Create('Internal Error: [Url] was an empty string.');

  LastFetchUrl := Url;
  LastFetchPostData := PostStr;
  GetUrlParts(Url, Protocol, ServerName, ObjectName, Port);
  if WideSameText(Protocol, FILE_PROTOCOL) then begin
    // file protocol
    OnMessage(SDefaultWaitMsg);
    try
      FileStream := TTntFileStream.Create(ObjectName, fmOpenRead);
      try
        OnRequestDone(FileStream, False);
      finally
        FreeAndNil(FileStream);
      end;
    except
      OnRequestDone(nil, True);
      raise;
    end;
  end else begin
    // non-file (assuming http) protocol
    Secure := WideSameText(Protocol, HTTPS_PROTOCOL);
    StartRequest(Port, ServerName, ObjectName, UserName, Password, PostStr, SDefaultWaitMsg);
  end;
end;

{ TFetchUrlIntoString }
type
  TFetchUrlIntoString = class(TComponent)
  private
    FetchUrl1: TTntFetchUrl;
    procedure FetchUrl1RequestDone(Stream: TStream; ErrorsOccurred: Boolean);
  public
    Done: Boolean;
    Errors: Boolean;
    FetchIntoStrings: TTntStrings;
    FetchIntoStream: TStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelFetch(Sender: TObject);
  end;

constructor TFetchUrlIntoString.Create(AOwner: TComponent);
begin
  inherited;
  FetchUrl1 := TTntFetchUrl.Create(Self);
  FetchUrl1.OnRequestDone := FetchUrl1RequestDone;
end;

destructor TFetchUrlIntoString.Destroy;
begin
  FreeAndNil(FetchUrl1);
  inherited;
end;

procedure TFetchUrlIntoString.FetchUrl1RequestDone(Stream: TStream;
  ErrorsOccurred: Boolean);
begin
  Done := True;
  Errors := ErrorsOccurred;
  if not Errors then begin
    Stream.Position := 0;
    if FetchIntoStrings <> nil then begin
      Stream.Position := 0;
      FetchIntoStrings.LoadFromStream(Stream);
    end;
    if FetchIntoStream <> nil then begin
      Stream.Position := 0;
      FetchIntoStream.CopyFrom(Stream, Stream.Size);
    end;
  end;
end;

procedure TFetchUrlIntoString.CancelFetch(Sender: TObject);
begin
  Done := True;
  Errors := True;
  FetchUrl1.StopRequest;
end;

{ TTntWebUpdateProgressForm }

procedure TTntWebDownloadProgressForm.FormCreate(Sender: TObject);
begin
  CancelBtn.Caption := SCancelButton;
  if (not Is_WINE) then begin
    Animate1.CommonAVI := aviFindComputer;
    Animate1.Active := True;
  end;
end;

procedure TTntWebDownloadProgressForm.FetchUrl1Message(const Value: WideString);
begin
  Label1.Caption := Value;
end;

procedure TTntWebDownloadProgressForm.FetchUrl1Progress(BytesDownloaded, TotalAvailable: Int64);
begin
  FLastTotalAvailable := TotalAvailable;
  if TotalAvailable > 0 then begin
    ProgressBar1.Show;
    ProgressBar1.Max := 1000;
    ProgressBar1.Position := round((BytesDownloaded / TotalAvailable) * 1000);
  end;
end;

function FileUrl(const Url: WideString): WideString;
var
  Protocol, ServerName, ObjectName: WideString;
  Port: Word;
begin
  GetUrlParts(Url, Protocol, ServerName, ObjectName, Port);
  if WideSameText(Protocol, FILE_PROTOCOL) then
    Result := ObjectName
  else
    Result := '';
end;

// ------- DoWebUpdate ----------

procedure LoadStreamFromFile(Stream: TStream; FileName: WideString);
var
  FileStream: TTntFileStream;
begin
  FileStream := TTntFileStream.Create(FileName, fmOpenRead);
  try
    Stream.Position := 0;
    Stream.CopyFrom(FileStream, FileStream.Size)
  finally
    FileStream.Free;
  end;
end;

procedure FetchUrl(_Strings: TTntStrings; _Url, _UserName, _Password: WideString;
  _PostData: WideString; _Caption: WideString; _OwnerForm: TCustomForm; _Stream: TStream = nil;
    _TimeOutSeconds: Cardinal = 0);
var
  StartTime: TDateTime;

  function TimeOutExpired: Boolean;
  begin
    Result := (_TimeOutSeconds <> 0)
      and (Trunc((Now - StartTime) / SYS_SECOND) > _TimeOutSeconds);
  end;

var
  ProgressForm: TTntWebDownloadProgressForm;
begin
  if FileUrl(_Url) <> '' then begin
    if _Stream <> nil then
      LoadStreamFromFile(_Stream, FileUrl(_Url));
    if _Strings <> nil then
      _Strings.LoadFromFile(FileUrl(_Url))
  end else begin
    ProgressForm := TTntWebDownloadProgressForm.Create(_OwnerForm);
    with ProgressForm do
    try
      Label1.Caption := SConnecting;
      if _Caption <> '' then
        Caption := _Caption;

      StartModal;
      try
        with TFetchUrlIntoString.Create(ProgressForm) do
        try
          FetchIntoStrings := _Strings;
          FetchIntoStream := _Stream;

          CancelBtn.OnClick := CancelFetch;
          FetchUrl1.OnMessage := FetchUrl1Message;
          FetchUrl1.OnProgress := FetchUrl1Progress;

          // start request, wait until done
          FetchUrl1.FetchUrl(_Url, _UserName, _Password, _PostData);
          StartTime := Now;
          While (not Done) do begin
            if TimeOutExpired then
              raise ETntGeneralError.Create(SRequestTimedOut);
            Application.ProcessMessages;
            Sleep(50);
          end;
          if Errors then Abort;
        finally
          Free;
        end;
      finally
        StopModal;
      end;
    finally
      Free;
    end;
  end;
end;

function FetchUrl_Size(_Url, _UserName, _Password: WideString;
  _PostData: WideString; _Caption: WideString; _OwnerForm: TCustomForm;
    _TimeOutSeconds: Cardinal = 0): Int64;
var
  StartTime: TDateTime;

  function TimeOutExpired: Boolean;
  begin
    Result := (_TimeOutSeconds <> 0)
      and (Trunc((Now - StartTime) / SYS_SECOND) > _TimeOutSeconds);
  end;

var
  ProgressForm: TTntWebDownloadProgressForm;
begin
  if FileUrl(_Url) <> '' then
    Result := SizeOfFile(FileUrl(_Url))
  else begin
    Result := 0;
    ProgressForm := TTntWebDownloadProgressForm.Create(_OwnerForm);
    with ProgressForm do
    try
      Label1.Caption := SConnecting;
      if _Caption <> '' then
        Caption := _Caption
      else
        Caption := SRequestingWebFileSize;

      StartModal;
      try
        with TFetchUrlIntoString.Create(ProgressForm) do
        try
          FetchIntoStrings := nil;
          FetchIntoStream := nil;
          FetchUrl1.DownloadOption := doSizeOnly;

          CancelBtn.OnClick := CancelFetch;
          FetchUrl1.OnMessage := FetchUrl1Message;
          FetchUrl1.OnProgress := FetchUrl1Progress;

          // start request, wait until done
          FetchUrl1.FetchUrl(_Url, _UserName, _Password, _PostData);
          StartTime := Now;
          While (not Done) do begin
            if TimeOutExpired then
              raise ETntGeneralError.Create(SRequestTimedOut);
            Application.ProcessMessages;
            Sleep(50);
          end;
          if Errors then Abort;
          Result := FLastTotalAvailable;
        finally
          Free;
        end;
      finally
        StopModal;
      end;
    finally
      Free;
    end;
  end;
end;

function FetchUrlIntoStrings_Quiet(_Stream: TStream; _Url, _UserName, _Password, _Post: WideString): Boolean;
var
  FS: TTntFileStream;
begin
  Result := True;
  try
    with TFetchUrlIntoString.Create(nil) do
    try
      FetchIntoStrings := nil;
      FetchIntoStream := _Stream;

      //Update registry from web
      if FileUrl(_Url) <> '' then begin
        if FetchIntoStrings <> nil then
          FetchIntoStrings.LoadFromFile(FileUrl(_Url));
        if FetchIntoStream <> nil then begin
          FS := TTntFileStream.Create(FileUrl(_Url), fmOpenRead);
          try
            FS.Position := 0;
            FetchIntoStream.CopyFrom(FS, FS.Size);
          finally
            FreeAndNil(FS);
          end;
        end;
      end else begin
        if (not InternetIsConnected(True)) then
          Result := False
        else begin
          // start request, wait until done
          FetchUrl1.Quiet := True;
          FetchUrl1.FetchUrl(_Url, _UserName, _Password, _Post);
          While not Done do begin
            Sleep(250);
          end;
          if Errors then
            Abort;
        end;
      end;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ TWebStatThread }
type
  TWebStatThread = class(TThread)
  private
    FUrl: WideString;
    FUserName: WideString;
    FPassword: WideString;
    FPost: WideString;
    FStream: TStream;
    FOnSuccess: TNotifyEvent;
    procedure DoOnSuccess;
    procedure DoOnError;
  protected
    procedure Execute; override;
  public
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
  end;

procedure TWebStatThread.DoOnSuccess;
begin
  if Assigned(FOnSuccess) then
    FOnSuccess(FStream);
end;

procedure TWebStatThread.DoOnError;
begin
  if Assigned(FOnSuccess) then
    FOnSuccess(nil);
end;

procedure TWebStatThread.Execute;
var
  Success: Boolean;
begin
  try
    FStream := TTntMemoryStream.Create;
    try
      Success := FetchUrlIntoStrings_Quiet(FStream, FUrl, FUserName, FPassword, FPost);
      if (not Terminated) then begin
        if Success then
          Synchronize(DoOnSuccess)
        else
          Synchronize(DoOnError)
      end;
    finally
      FreeAndNil(FStream);
    end;
  except
    // just go away!
  end;
end;

{ TWebStatThreadOwner }
type
  TWebStatThreadOwner = class(TComponent)
  private
    Thread: TWebStatThread;
    procedure DoThreadTerminate(Sender: TObject);
  public
    destructor Destroy; override;
  end;

destructor TWebStatThreadOwner.Destroy;
begin
  inherited;
  if Thread <> nil then begin
    Thread.OnSuccess := nil;
    Thread.OnTerminate := nil;
    Thread.Terminate;
    Thread := nil;
  end;
end;

procedure TWebStatThreadOwner.DoThreadTerminate(Sender: TObject);
begin
  Thread := nil;
  Free;
end;

function FetchUrlInBackground(_Url, _UserName, _Password: WideString; _OnSuccess: TNotifyEvent; Owner: TComponent;
  _Post: WideString = ''): TObject;
begin
  Result := TWebStatThreadOwner.Create(Owner);
  with TWebStatThreadOwner(Result) do begin
    Thread := TWebStatThread.Create(True);
    with Thread do
    begin
      Priority := tpIdle;
      FUrl := _Url;
      FUserName := _UserName;
      FPassword := _Password;
      FPost := _Post;
      OnSuccess := _OnSuccess;
      OnTerminate := DoThreadTerminate;
      FreeOnTerminate := True;
      Resume;
    end;
  end;
end;

{ EDownloadError }

constructor EDownloadError.Create(const Msg, ExtraInfo, RawDownload: WideString);
begin
  inherited Create(Msg, ExtraInfo);
  FRawDownload := RawDownload;
end;

procedure EDownloadError.DisplayHtmlError;
var
  sList: TTntStringList;
  TempHtmlFile: WideString;
begin
  if ((WideTextPos('<html>', FRawDownload) > 0) or (WideTextPos('<html ', FRawDownload) > 0))
  and (WideTextPos('</html>', FRawDownload) > 0) then
  begin
    sList := TTntStringList.Create;
    try
      sList.Text := FRawDownload;
      TempHtmlFile := GetTempFileWithExt('.html');
      sList.SaveToFile(TempHtmlFile);
      ShellOpenFile(TempHtmlFile);
    finally
      sList.Free;
    end;
  end;
end;

end.
