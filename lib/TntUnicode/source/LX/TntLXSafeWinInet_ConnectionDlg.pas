
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXSafeWinInet_ConnectionDlg;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TntLxForms, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls;

type
  TTntLXSafeWinInetConnectionForm = class(TTntFormLX)
    TntBevel1: TTntBevel;
    TryAgainBtn: TTntButton;
    CancelBtn: TTntButton;
    AdvancedSettingsBtn: TTntButton;
    ConnectionNotAvailLbl: TTntLabel;
    TntImage1: TTntImage;
    FirewallDescLbl: TTntLabel;
    FirewallLbl: TTntLabel;
    MsIeLbl: TTntLabel;
    MsIeDescLbl: TTntLabel;
    TntImage2: TTntImage;
    TntImage3: TTntImage;
    procedure AdvancedSettingsBtnClick(Sender: TObject);
    procedure TntFormLXCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ReportInternetConnectionIssue(out Retry: Boolean);

implementation

{$R *.DFM}

uses
  TntLXSafeWinInet;

procedure ReportInternetConnectionIssue(out Retry: Boolean);
begin
  with TTntLXSafeWinInetConnectionForm.Create(Application) do
  try
    Retry := False;
    case ShowModal of
      mrCancel: Abort;
      mrRetry: Retry := True;
    end;
  finally
    Free;
  end;
end;

{ TTntLXSafeWinInetConnectionForm }

resourcestring
  SConnectionNotAvail = 'Connection to the Internet is Not Available';
  SConnectionNotAvail2 = 'A connection to the Internet is not available at t' +
  'his time.';
  SPossibleFirewallIssue = 'Possible Personal Firewall Issue';
  SFirewallDescription = 'If Microsoft Internet Explorer is able to connect ' +
  'to the Internet, make sure that a personal firewall application is not bl' +
  'ocking this application.  Some examples include: ZoneAlarm and Norton Int' +
  'ernet Security.';
  SMicrosoftIE = 'Microsoft Internet Explorer';
  SMSIEDescription = 'This application uses the same settings and components' +
  ' as Microsoft Internet Explorer.  If you are having problems connecting t' +
  'his application to the Internet, first make sure that Microsoft Internet ' +
  'Explorer is able to connect to the Internet.';
  STryAgain = 'Try Again';
  SCancel = 'Cancel';
  SAdvancedSettings = 'Advanced Settings...';

procedure TTntLXSafeWinInetConnectionForm.TntFormLXCreate(Sender: TObject);
begin
  Caption := SConnectionNotAvail;
  ConnectionNotAvailLbl.Caption := SConnectionNotAvail2;
  FirewallLbl.Caption := SPossibleFirewallIssue;
  FirewallDescLbl.Caption := SFirewallDescription;
  MsIeLbl.Caption := SMicrosoftIE;
  MsIeDescLbl.Caption := SMSIEDescription;
  TryAgainBtn.Caption := STryAgain;
  CancelBtn.Caption := SCancel;
  AdvancedSettingsBtn.Caption := SAdvancedSettings;
end;

procedure TTntLXSafeWinInetConnectionForm.AdvancedSettingsBtnClick(Sender: TObject);
begin
  if ConfigureAdvancedConnectionSettings then
    ModalResult := mrRetry;
end;

end.
