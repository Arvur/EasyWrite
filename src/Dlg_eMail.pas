unit Dlg_eMail;

interface

uses
  StrUtils{LeftStr}, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TBXDkPanels, SpTBXDkPanels, TB2Dock, TB2ToolWindow, TBX,
  StdCtrls, TntStdCtrls, SpTBXEditors, DKLang, SpTBXControls, PropFilerEh,
  PropStorageEh;

type
  TDlgEMail = class(TForm)
    pnl_Dialog: TTBXToolWindow;
    pnl_Buttons: TTBXAlignmentPanel;
    btn_Cancel: TSpTBXButton;
    btn_Ok: TSpTBXButton;
    pnl_Protect: TTBXAlignmentPanel;
    chk_Protect: TSpTBXCheckBox;
    chk_Aggresive: TSpTBXCheckBox;
    edt_EMail: TSpTBXEdit;
    DKLang: TDKLanguageController;
    PropStorage: TPropStorageEh;
    procedure ProcessEMail(Sender: TObject);
    procedure CheckBoxGroupping(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgEMail: TDlgEMail;
  ProcessedEMail : string;

implementation

uses Dm_Main;

{$R *.dfm}

procedure TDlgEMail.ProcessEMail(Sender: TObject);
begin
 ProcessedEMail := '';
 if chk_Protect.Checked and (edt_EMail.Text <> '') then begin
  if chk_Aggresive.Checked
   then ProcessedEMail := '[img]mailpng.cgi?word=' + edt_EMail.Text + '[/img]'
   else ProcessedEMail := StringReplace(edt_EMail.Text, '@', ' @ ', []);
 end;
end;

procedure TDlgEMail.CheckBoxGroupping(Sender: TObject);
begin
 chk_Aggresive.Enabled := chk_Protect.Checked;
end;

procedure TDlgEMail.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Key = VK_RETURN) then btn_Ok.Click;
end;

end.
