
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXSafeWinInet_AdvancedDlg;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TntLxForms, StdCtrls, TntStdCtrls, Spin, ExtCtrls, TntExtCtrls;

type
  TTntLXSafeWinInetAdvancedForm = class(TTntFormLX)
    AutoPrimeEnableBox: TTntCheckBox;
    AutoPrimeMaxAttemptsEdit: TSpinEdit;
    MaxAttemptsLbl: TTntLabel;
    UrlToUseLbl: TTntLabel;
    AutoPrimeUrlEdit: TTntEdit;
    AutoPrimeDelayEdit: TSpinEdit;
    DelayLbl: TTntLabel;
    AlwaysReportConnectionBox: TTntCheckBox;
    CancelBtn: TTntButton;
    OKBtn: TTntButton;
    TntBevel1: TTntBevel;
    InternetConnectionLbl: TTntLabel;
    procedure AutoPrimeEnableBoxClick(Sender: TObject);
    procedure TntFormLXCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function _ConfigureAdvancedConnectionSettings(
  var GAutoPrime_Enable: Boolean;
  var GAutoPrime_MaxTrialCount: Integer;
  var GAutoPrime_URL: WideString;
  var GAutoPrime_PostDelay: Integer;
  var GAlwaysReport_InternetAsConnected: Boolean): Boolean;

implementation

{$R *.dfm}

function _ConfigureAdvancedConnectionSettings(
  var GAutoPrime_Enable: Boolean;
  var GAutoPrime_MaxTrialCount: Integer;
  var GAutoPrime_URL: WideString;
  var GAutoPrime_PostDelay: Integer;
  var GAlwaysReport_InternetAsConnected: Boolean): Boolean;
begin
  with TTntLXSafeWinInetAdvancedForm.Create(Application) do
  try
    // read values
    AutoPrimeEnableBox.Checked := GAutoPrime_Enable;
    AutoPrimeMaxAttemptsEdit.Value := GAutoPrime_MaxTrialCount;
    AutoPrimeUrlEdit.Text := GAutoPrime_URL;
    AutoPrimeDelayEdit.Value := GAutoPrime_PostDelay;
    AlwaysReportConnectionBox.Checked := GAlwaysReport_InternetAsConnected;
    // show dialog
    AutoPrimeEnableBoxClick(nil);
    Result := (ShowModal = mrOK);
    if Result then begin
      // write values
      GAutoPrime_Enable := AutoPrimeEnableBox.Checked;
      GAutoPrime_MaxTrialCount := AutoPrimeMaxAttemptsEdit.Value;
      GAutoPrime_URL := AutoPrimeUrlEdit.Text;
      GAutoPrime_PostDelay := AutoPrimeDelayEdit.Value;
      GAlwaysReport_InternetAsConnected := AlwaysReportConnectionBox.Checked;
    end;
  finally
    Free;
  end;
end;

{ TTntLXSafeWinInetAdvancedForm }

resourcestring
  SAdvancedInternetSettings = 'Advanced Internet Connection Settings';
  SMaximumAttempts = 'Maximum attempts';
  SURLToUse = 'URL to use';
  SDelayMsecAfterEachAttempt = 'Delay (msec) after each attempt';
  SForcePreliminaryConnections = 'Force preliminary connection attempts.';
  SAlwaysReportConnectionAsAvailable = 'Always report an Internet connection as being available.';
  SCancel = 'Cancel';
  SOK = 'OK';
  SInternetConnection = 'Detecting Availability of Internet Connection';

procedure TTntLXSafeWinInetAdvancedForm.TntFormLXCreate(Sender: TObject);
begin
  Caption := SAdvancedInternetSettings;
  InternetConnectionLbl.Caption := SInternetConnection;
  MaxAttemptsLbl.Caption := SMaximumAttempts;
  UrlToUseLbl.Caption := SURLToUse;
  DelayLbl.Caption := SDelayMsecAfterEachAttempt;
  AutoPrimeEnableBox.Caption := SForcePreliminaryConnections;
  AlwaysReportConnectionBox.Caption := SAlwaysReportConnectionAsAvailable;
  CancelBtn.Caption := SCancel;
  OKBtn.Caption := SOK;
end;

procedure TTntLXSafeWinInetAdvancedForm.AutoPrimeEnableBoxClick(Sender: TObject);
begin
  AutoPrimeMaxAttemptsEdit.Enabled := AutoPrimeEnableBox.Checked;
  MaxAttemptsLbl.Enabled := AutoPrimeEnableBox.Checked;
  AutoPrimeUrlEdit.Enabled := AutoPrimeEnableBox.Checked;
  UrlToUseLbl.Enabled := AutoPrimeEnableBox.Checked;
  AutoPrimeDelayEdit.Enabled := AutoPrimeEnableBox.Checked;
  DelayLbl.Enabled := AutoPrimeEnableBox.Checked;
end;

end.
