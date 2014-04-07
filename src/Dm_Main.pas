unit Dm_Main;

interface

uses
  Forms{Application}, IniFiles{TIniFile}, StdCtrls{TCustomEdit}, Math{ifthen},
  TntStdCtrls{TTntMemo}, TntWindows{Tnt_ShellExecuteW}, TntClasses{TTntStringList},
  Dialogs,TntDialogs{WideMessageDlg}, TntSysUtils{Tnt_WideStringReplace},
  Windows, StrUtils, SysUtils, DKLang, ExpStringHolder, ImgList, Controls,
  PngImageList, ExtActns, TntExtActns, StdActns, TntStdActns, TntActnList,
  Classes, ActnList, TBXSwitcher, PropStorageEh, ExpMultiStringHolder;

type
  TDmMain = class(TDataModule)
    Switcher: TTBXSwitcher;
    aLst_Main: TTntActionList;
    iLst_Main: TPngImageList;
    iLst_Trash: TPngImageList;
    str_w3Colors: TTExpStringHolder;
    str_Smiles: TTExpStringHolder;
    str_Collections: TTExpStringHolder;
    DKLang: TDKLanguageController;
    act_Lnk_Developers: TTntBrowseURL;
    act_Lnk_Forum: TTntBrowseURL;
    act_About: TTntAction;
    act_Spec_Spam: TTntAction;
    act_Spec_Links: TTntAction;
    act_Ins_Image: TTntAction;
    act_Ins_eMail: TTntAction;
    act_Ins_Link: TTntAction;
    act_Ins_Bullet: TTntAction;
    act_Ins_Break: TTntAction;
    act_Ins_Line: TTntAction;
    act_Fmt_Plain: TTntAction;
    act_Fmt_Quote: TTntAction;
    act_Fmt_Code: TTntAction;
    act_Fmt_Center: TTntAction;
    act_Fmt_Upper: TTntAction;
    act_Fmt_Lower: TTntAction;
    act_Fmt_Small: TTntAction;
    act_Fmt_Striked: TTntAction;
    act_Fmt_Underline: TTntAction;
    act_Fmt_Italic: TTntAction;
    act_Fmt_Bold: TTntAction;
    act_File_Save: TTntFileSaveAs;
    act_File_Open: TTntFileOpen;
    act_File_New: TTntAction;
    act_Edit_SelectAll: TTntEditSelectAll;
    act_Edit_Delete: TTntEditDelete;
    act_Edit_Paste: TTntEditPaste;
    act_Edit_Copy: TTntEditCopy;
    act_Edit_Cut: TTntEditCut;
    act_Exit: TTntFileExit;
    act_Edit_Undo: TTntEditUndo;
    PropStorMan: TIniPropStorageManEh;
    str_Tags: TExpMultiStringHolder;
    act_Fmt_Hide: TTntAction;
    act_Spec_Browser: TTntAction;
    procedure EOnClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure EOnExecute(Sender: TObject);
  private
  public
    procedure InsertImage(aURL : string);
    function GetPreview : WideString;
  end;

  procedure SaveDefaults;
  procedure LoadDefaults;
  procedure WrapSelection(aLeft, aRight : string; aText : string = ''; Persistence : Boolean = True);

var
  DmMain: TDmMain;
  MsgEditor: TTntMemo;

const
 str_InsSymbols : array [1..3] of string = ('hr', 'br', '*');
 str_ListStyles : array [0..2] of string = ('', '=1', '=a');

implementation

uses Fm_Main, Dlg_Links, Dlg_eMail, U_Ib2Html, U_Cmn_Files, Fm_About{$IFDEF EW_ALPHA}, U_Templates{$ENDIF};

{$R *.dfm}

procedure SaveDefaults;
begin
 if not FileExists(AuxName('defaults')) then begin
  DmMain.PropStorMan.IniFileName := AuxName('defaults');
   FmMain.SaveProperties;
  DmMain.PropStorMan.IniFileName := AuxName('ini');
 end;
end;

procedure LoadDefaults;
begin
 if FileExists(AuxName('defaults')) then begin
  DeleteFile(DmMain.PropStorMan.IniFileName);
  DmMain.PropStorMan.IniFileName := AuxName('defaults');
   FmMain.LoadProperties;
  DmMain.PropStorMan.IniFileName := AuxName('ini');
 end;
end;

procedure WrapSelection(aLeft, aRight : string; aText : string = ''; Persistence : Boolean = True);
var
 oldSelLen : Integer;
begin
 oldSelLen := ifthen((FmMain.mnu_Opt_PSelection.Checked and Persistence), MsgEditor.SelLength);
 if (aText = '') then aText := MsgEditor.SelText;
 MsgEditor.SelText := aLeft + aText + aRight;
 MsgEditor.SelStart := MsgEditor.SelStart - (Length(aRight) + oldSelLen);
 MsgEditor.SelLength := oldSelLen;
end;

{ TDmMain }

procedure TDmMain.DataModuleCreate(Sender: TObject);
var
 tmpIni : TIniFile;
 tmpList : TStringList;
 Counter : Integer;
 tmpStr : string;
begin
 PropStorMan.IniFileName := AuxName('ini');
// SetDefaultPropStorageManager(PropStorMan);

 tmpIni := TIniFile.Create(HomePath + 'tags\Ru-Board.ini');
 for Counter := 0 to (str_Tags.MultipleStrings.Count - 1) do begin
  tmpStr := str_Tags.MultipleStrings.Items[Counter].Name;
  tmpIni.ReadSectionValues(tmpStr, str_Tags.ItemStringsByName[tmpStr]);
 end;
 tmpIni.Free;

 tmpList := TStringList.Create;
  GetSubFolders(HomePath + 'smiles\', tmpList);
  str_Collections.Strings.Text := tmpList.Text;
 tmpList.Free;
end;

procedure TDmMain.InsertImage(aURL: string);
begin
 if (aURL <> '') then
  WrapSelection('[img]', '[/img]', aURL, False);
end;

procedure TDmMain.EOnClick(Sender: TObject);
begin
 if (Sender = act_File_Open) then
  MsgEditor.Lines.LoadFromFile(act_File_Open.Dialog.FileName);

 if (Sender = act_File_Save) then
  MsgEditor.Lines.SaveToFile(act_File_Save.Dialog.FileName);
end;

procedure TDmMain.EOnExecute(Sender: TObject);
var
 tmpAct : TTntAction;
 tmpTag, tmpStr : string;
begin
 if (Sender is TTntAction) then begin
  tmpAct := Sender as TTntAction;

  if (Sender = act_About) then
   with TFmAbout.Create(nil, True) do begin
    lbl_Logo.Font.Name := 'Monotype Corsiva';
    ShowModal;
   end;

  if (Sender = act_File_New) then begin
   MsgEditor.Lines.Clear;
   MsgEditor.OnChange(MsgEditor);
  end;

  if (tmpAct.Category = 'Format') then begin
   tmpTag := str_Tags.ItemStringsByName['ACTIONS'].Values[tmpAct.Name];
   if (tmpTag <> '') then begin
    if FmMain.mnu_Opt_ShortTags.Checked then begin
     tmpStr := str_Tags.ItemStringsByName['ABBREVIATIONS'].Values[tmpTag];
     if (tmpStr <> '') then tmpTag := tmpStr;
    end;
    WrapSelection('[' + tmpTag + ']', '[/' + tmpTag + ']');
   end;
  end;

  if (tmpAct.Category = 'Insert') then
   if (tmpAct.Tag in [1..3])
    then MsgEditor.SelText := '[' + str_InsSymbols[tmpAct.Tag] + ']'
    else begin
     if (tmpAct = act_Ins_Image) then InsertImage(''); // InputBox('Insert Image', 'Enter image link', '')
     if (tmpAct = act_Ins_Link) then begin
      tmpTag := '[/url]';
      tmpStr := Trim(InputBox(LangManager.ConstantValue['msg_Desc_Link'], '', ''));
      MsgEditor.SelText := '[url' + ifthen(tmpStr <> '', '=') + ']' + tmpStr + tmpTag;
      MsgEditor.SelStart := MsgEditor.SelStart - Length(tmpTag);
      if (tmpStr <> '') then MsgEditor.SelStart := MsgEditor.SelStart - (Length(tmpStr) + 1);
     end;
     if (tmpAct = act_Ins_eMail) then begin
      tmpTag := '[/email]';
      tmpStr := Trim(InputBox(LangManager.ConstantValue['msg_Desc_Mail'], '', ''));
      MsgEditor.SelText := '[email' + ifthen(tmpStr <> '', '=') + ']' + tmpStr + tmpTag;
      MsgEditor.SelStart := MsgEditor.SelStart - Length(tmpTag);
      if (tmpStr <> '') then MsgEditor.SelStart := MsgEditor.SelStart - (Length(tmpStr) + 1);
     end;
    end;

  if (tmpAct.Category = 'Specials') then begin
   if (tmpAct = act_Spec_Links) then
    with TDlgLinks.Create(nil) do begin
     if (ShowModal = mrOk) then MsgEditor.SelText := ProcessedLinks;
     Free;
    end;
   if (tmpAct = act_Spec_Spam) then
    with TDlgEMail.Create(nil) do begin
     if (ShowModal = mrOk) then MsgEditor.SelText := ProcessedEMail;
     Free;
    end;

   if (tmpAct = act_Spec_Browser) then begin
    tmpStr := HomePath + '\Preview.html';
    with TTntStringList.Create do begin
     Text := GetPreview;
     SaveToFile(WideString(tmpStr));
     Free;
    end;
    Tnt_ShellExecuteW(Application.Handle, 'open', PWideChar(WideString(tmpStr)), nil, nil, sw_ShowNormal);
   end;
  end;
 end;
end;

function TDmMain.GetPreview: WideString;
var
 tmpStr : WideString;
 Counter : integer;
begin
 tmpStr := MsgEditor.Text;
 for Counter := 0 to (str_Smiles.Strings.Count - 1) do
  tmpStr := Tnt_WideStringReplace(tmpStr, ':' + str_Smiles.Strings.Names[Counter] + ':', str_Smiles.Strings.ValueFromIndex[Counter], [rfReplaceAll, rfIgnoreCase]);
 with TTntStringList.Create do begin
  try
   LoadFromFile(HomePath + '\preview\Ru-Board.html');
   Result := StringReplace(Text, '$$$', Do_Ib2Html(tmpStr), []);
  finally
   Free;
  end 
 end;
end;

end.
