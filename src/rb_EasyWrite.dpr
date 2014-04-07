program rb_EasyWrite;

uses
  TellTail,
  Forms,
  Fm_Main in 'Fm_Main.pas' {FmMain},
  Dm_Main in 'Dm_Main.pas' {DmMain: TDataModule},
  Fm_TblSelect in 'Fm_TblSelect.pas' {FmTblSelect},
  Frm_Smiles in 'Frm_Smiles.pas' {FrmSmiles: TFrame},
  Frm_Tables in 'Frm_Tables.pas' {FrmTables: TFrame},
  Dlg_eMail in 'Dlg_eMail.pas' {DlgEMail},
  Dlg_Links in 'Dlg_Links.pas' {DlgLinks},
  U_Ib2Html in 'U_Ib2Html.pas',
  U_Cmn_Files in 'U_Cmn_Files.pas',
  Fm_Base_Modal in 'Fm_Base_Modal.pas' {FmBase_Modal},
  Fm_About in 'Fm_About.pas' {FmAbout},
  Frm_Html in 'Frm_Html.pas' {FrmHtml: TFrame},
  Dlg_Base in 'Dlg_Base.pas' {DlgBase};

{$R *.res}
{$R *.dkl_const.res}

var
 InstanceChecked : Boolean;
begin
  Application.Initialize;
  Application.Title := 'Ru.Board EasyWrite';
  with TTellTail.CreateWithNames('{84D95AA1-A5DE-4617-8506-67CC48651229}', '{0B805116-3A72-487F-A2B0-1855F57380B2}', Application) do begin
   Enabled := True;
   InstanceChecked := Execute;
  end;
  if InstanceChecked then begin
   Application.CreateForm(TDmMain, DmMain);
   Application.CreateForm(TFmMain, FmMain);
  end;
  Application.Run;
end.
