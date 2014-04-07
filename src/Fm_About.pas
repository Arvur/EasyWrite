unit Fm_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Fm_Base_Modal, TBXDkPanels, ExtCtrls, TB2Dock, TB2ToolWindow,
  TBX, PJVersionInfo, pngimage, SpTBXDkPanels, ImgList, PngImageList,
  DKLang, SpTBXControls;

type
  TFmAbout = class(TFmBase_Modal)
    VerInfo: TPJVersionInfo;
    iLst_About: TPngImageList;
    lbl_Product: TSpTBXLabel;
    pnl_Copyright: TSpTBXPanel;
    lbl_Copyright: TSpTBXLabel;
    lbl_Link: TSpTBXLabel;
    lbl_Mail: TSpTBXLabel;
    lc_About: TDKLanguageController;
    lbl_DBClient: TSpTBXLabel;
    lbl_Build: TSpTBXLabel;
    lbl_Version: TSpTBXLabel;
    procedure FormCreate(Sender: TObject);
    procedure EOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
  public
    constructor Create(AOwner: TComponent; ShowContacts : Boolean = True); reintroduce;
  end;

var
  FmAbout: TFmAbout;

implementation

uses U_Cmn_Files;

const
 SubjectPrefix : string = '?Subject=';
 _sep : string = ' : ';

{$R *.dfm}

constructor TFmAbout.Create(AOwner: TComponent; ShowContacts : Boolean = True);
begin
 inherited Create(AOwner);

 if not ShowContacts then begin
  lbl_Mail.Visible := False;
  lbl_Link.Hint := ''; lbl_Link.LinkText := '';
  lbl_Link.ImageIndex := -1;
 end;
end;

procedure TFmAbout.FormCreate(Sender: TObject);
begin
 VerInfo.FileName := Application.ExeName;
 lbl_Logo.Caption := VerInfo.ProductName;
 lbl_Product.Caption := VerInfo.ProductVersion;
 lbl_Version.Caption := lbl_Version.Caption + _sep + VerInfo.FileVersion;
 lbl_Build.Caption := lbl_Build.Caption + _sep + DateTimeToStr(PETimeStamp(VerInfo.FileName));

 lbl_Mail.LinkText := lbl_Mail.LinkText + SubjectPrefix + VerInfo.ProductName;

 VerInfo.FileName := HomePath + 'fbclient.dll';
 if FileExists(VerInfo.FileName) then begin
  lbl_DBClient.Caption := lbl_DBClient.Caption + _sep + VerInfo.FileVersion;
  lbl_DBClient.Visible := True;
 end;
end;

procedure TFmAbout.EOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 Close;
end;

end.
