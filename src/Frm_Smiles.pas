unit Frm_Smiles;

interface

uses
  Htmlsubs{TImageObj}, StrUtils{RightStr},
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, TB2Dock, TB2ToolWindow, TBX, ComCtrls, rmTBXTabControl, Htmlview,
  ExtCtrls, TBXDkPanels, SpTBXDkPanels, StdCtrls, TntStdCtrls, SpTBXEditors,
  DB, TntLXDataSet, TntLXVarArrayDataSet, TntLXCsvUtils, Frm_Html, DKLang,
  SpTBXControls;

type
  TFrmSmiles = class(TFrame)
    pCtrl_Smiles: TrmTBXTabControl;
    pnl_Collection: TTBXAlignmentPanel;
    lbl_Collection: TSpTBXLabel;
    cmb_Collection: TSpTBXComboBox;
    tbl_Smiles: TTntCSVTable;
    FrmHtml: TFrmHtml;
    DKLang: TDKLanguageController;
    procedure FrameResize(Sender: TObject);
    procedure EOnCollection(Sender: TObject);
    procedure EOnImageClick(Sender, Obj: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EOnLanguageChanged(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses Dm_Main, U_Cmn_Files;

const
 str_SmilesHeader = '<link rel="stylesheet" href="Ru-Board.css" type="text/css"><body bgcolor="#ffffff" alink="#333333" vlink="#333333" link="#333333" topmargin="5" leftmargin="0"><table align=center width="100%" border="0" cellspacing="1" cellpadding="1" bgcolor=#999999>';
 str_SmilesBody   = '<tr bgcolor=#FFFFFF class=dats align=center><td><img src="smiles\%s\%s" title="%s" border=0></td></tr>';
 str_SmilesFooter = '</table>';

{$R *.dfm}

constructor TFrmSmiles.Create(AOwner: TComponent);
begin
 inherited;
 tbl_Smiles.DelimiterChar := ';';

 cmb_Collection.Items.AnsiStrings.Text := DmMain.str_Collections.Strings.Text;
 if (cmb_Collection.ItemIndex = -1) and (cmb_Collection.Items.Count > 0) then
  cmb_Collection.ItemIndex := 0;
 EOnCollection(cmb_Collection);
end;

procedure TFrmSmiles.FrameResize(Sender: TObject);
begin
 cmb_Collection.Left := lbl_Collection.Left + lbl_Collection.Width;
 cmb_Collection.Width := pnl_Collection.Width - cmb_Collection.Left - 5;
end;

procedure TFrmSmiles.EOnCollection(Sender: TObject);
var
 tmpHtml, tmpStr, tmpLang : string;
begin
 if (cmb_Collection.Text <> '') then begin
  if (LangManager.LanguageIndex = 0)
   then tmpLang := 'English'
   else tmpLang := ChangeFileExt(ExtractFileName(LangManager.LanguageResources[LangManager.LanguageIndex].wsName), '');

  if (Sender = cmb_Collection) then
   with tbl_Smiles do begin
    LoadFromFile(HomePath +'smiles\' + cmb_Collection.Text + '.csv');
    pCtrl_Smiles.Tabs.Clear;
    tbl_Smiles.First;
    while not tbl_Smiles.EOF do begin
     if (pCtrl_Smiles.Tabs.IndexOf(tbl_Smiles['Group.' + tmpLang]) = -1) then
      pCtrl_Smiles.Tabs.Add(tbl_Smiles['Group.' + tmpLang]);
     tbl_Smiles.Next;
    end;
    pCtrl_Smiles.TabIndex := 0;
   end;

  tmpHtml := str_SmilesHeader;
  tbl_Smiles.First;
  while not tbl_Smiles.EOF do begin
   if (tbl_Smiles['Group.' + tmpLang] = pCtrl_Smiles.Tabs[pCtrl_Smiles.TabIndex]) then begin
    tmpStr := tbl_Smiles['Link'];
    if (Pos('://', tmpStr) > 0)
     then tmpStr := RightStr(tmpStr, Length(tmpStr) - LastDelimiter('/', tmpStr))
     else tmpStr := tmpStr + '.gif';
    tmpHtml := tmpHtml + Format(str_SmilesBody, [cmb_Collection.Text, tmpStr, tbl_Smiles['Name.' + tmpLang]]);
   end;
   tbl_Smiles.Next;
  end;
  tmpHtml := tmpHtml + str_SmilesFooter;
  FrmHtml.html_Viewer.LoadFromString(tmpHtml);
 end; 
end;

procedure TFrmSmiles.EOnImageClick(Sender, Obj: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 tmpStr : string;
begin
 tmpStr := ExtractFileName((Obj as TImageObj).Source);
 if (cmb_Collection.Text = 'Ru-Board')
  then begin
   DmMain.str_Smiles.Strings.Values[ChangeFileExt(tmpStr, '')] := '[img]' + tmpStr + '[/img]';
   MsgEditor.SelText := ':' + ChangeFileExt(tmpStr, '') + ':';
  end
  else begin
   tbl_Smiles.First;
   while (not tbl_Smiles.EOF) and (Pos(tmpStr, tbl_Smiles['Link']) = 0) do
    tbl_Smiles.Next;
   if (Pos(tmpStr, tbl_Smiles['Link']) > 0) then
    MsgEditor.SelText := '[img]' + tbl_Smiles['Link'] + '[/img]';
  end;
end;

procedure TFrmSmiles.EOnLanguageChanged(Sender: TObject);
begin
 FrameResize(Self);
 EOnCollection(cmb_Collection);
end;

end.
