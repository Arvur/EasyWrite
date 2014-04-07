unit Dlg_Links;

interface

uses
  StrUtils{LeftStr}, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TBXDkPanels, SpTBXDkPanels, TB2Dock, TB2ToolWindow, TBX,
  StdCtrls, TntStdCtrls, DKLang, SpTBXControls, PropFilerEh, PropStorageEh;

type
  TDlgLinks = class(TForm)
    pnl_Dialog: TTBXToolWindow;
    pnl_Buttons: TTBXAlignmentPanel;
    btn_Cancel: TSpTBXButton;
    btn_Ok: TSpTBXButton;
    pnl_Protect: TTBXAlignmentPanel;
    pnl_Memo: TTBXAlignmentPanel;
    memo_Links: TTntMemo;
    chk_Protect: TSpTBXCheckBox;
    chk_Friendly: TSpTBXCheckBox;
    DKLang: TDKLanguageController;
    PropStorage: TPropStorageEh;
    procedure ProcessLinks(Sender: TObject);
    procedure CheckBoxGroupping(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgLinks: TDlgLinks;
  ProcessedLinks : string;

implementation

uses Dm_Main;

{$R *.dfm}

procedure TDlgLinks.ProcessLinks(Sender: TObject);
var
 Counter : Integer;
 tmpStr : string;
begin
 ProcessedLinks := '';
 if chk_Protect.Checked then
  for Counter := 0 to (memo_Links.Lines.Count - 1) do begin
   if (ProcessedLinks <> '') then ProcessedLinks := ProcessedLinks + #13#10;
   tmpStr := Trim(memo_Links.Lines[Counter]);
   if (LeftStr(tmpStr, 6) = 'ftp://') or (LeftStr(tmpStr, 7) = 'http://') then
    if chk_Friendly.Checked
     then tmpStr := '_' + tmpStr
     else begin
      if (LeftStr(tmpStr, 7) = 'http://') then tmpStr[3] := 'x';
      tmpStr[2] := 'x';
     end;
   memo_Links.Lines[Counter] := tmpStr;
   ProcessedLinks := ProcessedLinks + tmpStr;
  end;
end;

procedure TDlgLinks.CheckBoxGroupping(Sender: TObject);
begin
 chk_Friendly.Enabled := chk_Protect.Checked;
end;

end.
