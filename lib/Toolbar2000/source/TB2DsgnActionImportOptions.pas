unit TB2DsgnActionImportOptions;

{
  OPTIONS Dialog for TB2ActionImporter
  
  Toolbar2000
  Copyright (C) 1998-2004 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2DsgnConvertOptions.pas,v 1.5 2004/02/26 07:05:57 jr Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst;

type
  TTBActionImportOptionsForm = class(TForm)
    ConvertButton: TButton;
    HelpButton: TButton;
    Button1: TButton;
    tbItemCreationOptionsGroup: TGroupBox;
    Label3: TLabel;
    editComponentNamePrefix: TEdit;
    editActionNamePrefix: TEdit;
    GroupBox1: TGroupBox;
    ActionsCheckList: TCheckListBox;
    ButtonSelAll: TButton;
    ButtonUnselAll: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    CategoryCombo: TComboBox;
    ActionListCombo: TComboBox;
    cbPullDownMenus: TRadioButton;
    cbToolbarButtons: TRadioButton;
    Label4: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionListComboChange(Sender: TObject);
    procedure CategoryComboChange(Sender: TObject);
    procedure ButtonSelAllClick(Sender: TObject);
    procedure ButtonUnselAllClick(Sender: TObject);

  private
    { Private declarations }
    FActionLists:TStringList;
    FUpdating :Boolean;
  public
    { Public declarations }

    procedure AddActionList( actionList:TObject; actionListName:String );

    procedure RefreshCategoriesList; // when we change action lists, refresh to get new categories list.
    procedure RefreshActions; // and when we select a new category, refresh to select which actions to show in the listbox.

    function GetActionList:TObject;


  end;

implementation

{$R *.DFM}

uses ActnList;

procedure TTBActionImportOptionsForm.RefreshCategoriesList;
var
 C:TObject;
 A:TActionList;
 actn:TContainedAction; // An element of a TActionList.
 t:Integer;
 cap,cat:String;
 categories:TStringList;
 wasUpdating : Boolean;
begin
  wasUpdating := FUpdating;
  FUpdating := true;
  try

  CategoryCombo.Items.Clear;
  C := GetActionList;
  if not Assigned(C) then exit;

  if not (C is TActionList) then
      exit;
  A := C as TActionList;
  categories := TStringLIst.Create;
  try  
  for t := 0 to A.ActionCount-1 do begin
      actn := A.Actions[t];
      cat := actn.Category;
      if (cat='') then
         cat :='<None>';      
      if categories.IndexOf(cat)<0 then begin

         categories.Add(cat);
      end;
  end;
  if categories.Count=0 then
    categories.Add('<No Categories/Actions Defined Yet?>');
    
  categories.Sort;
  categories.Insert(0,'<All Categories>'); // Always first!
  CategoryCombo.Items.Assign(categories);
  CategoryCombo.ItemIndex := 0;

  RefreshActions;
  finally
    categories.Free;
  end;
  finally
      FUpdating := wasUpdating;
  end;
end;


procedure TTBActionImportOptionsForm.RefreshActions;
var
 C:TObject;
 A:TActionList;
 actn:TContainedAction;
 t:Integer;
 cap,cat:String;
 wasUpdating:Boolean;
begin
  wasUpdating := FUpdating;
  FUpdating := true;
  try

  ActionsCheckList.Items.Clear;

  C := GetActionList;
  if not Assigned(C) then exit;
  if not (C is TActionList) then
      exit;
  A := C as TActionList;
  for t := 0 to A.ActionCount-1 do begin
      actn := A.Actions[t];
      if  actn.InheritsFrom(TAction) then
          cap := TAction(actn).Caption
      else
          cap := '<'+String(actn.ClassName)+'>'; // TFileOpen, TFileOpenWith, 
          
      cap := StringReplace(cap,'&', '',[rfReplaceAll]);
      if CategoryCombo.ItemIndex>0 then begin
          cat := actn.Category;
          if (cat='') then
             cat :='<None>';
          if cat<>CategoryCombo.Text then continue; // FILTERED!
      end;
      ActionsCheckList.Items.AddObject(actn.Name+Chr(9)+cap,actn);
  end;
  finally
      FUpdating := wasUpdating;
  end;
end;

(*
procedure TTBActionImportOptionsForm.HelpButtonClick(Sender: TObject);
const
  SMsg1 = 'This will create a pulldown menu or toolbar from actions in a TActionList' +
    'component on this form, or another form/datamodule in your current project.'#13#10#13#10 +
    'The new items will take the names from the action names, with a given prefix. ' +
     #13#10#13#10 +
    'After the conversion process completes, you should verify that ' +
    'everything was copied correctly. Afterward, you may delete the ' +
    'old menu component.';

begin
  Application.MessageBox(SMsg1, 'Convert Help', MB_OK or MB_ICONINFORMATION);
end;
*)

procedure TTBActionImportOptionsForm.FormCreate(Sender: TObject);
begin
    FActionLists := TStringList.Create;
end;

procedure TTBActionImportOptionsForm.AddActionList( actionList:TObject; actionListName:String );
begin
  Assert(Assigned(Self));
  Assert(Assigned(FACtionLists));
  Assert(Assigned(ActionListCombo));
  FActionLists.AddObject( name, actionList );
  ActionListCombo.Items.Add( actionListName );
end;

function TTBActionImportOptionsForm.GetActionList:TObject;
begin
  if ActionListCombo.ItemIndex<0 then
      raise Exception.Create('No action list selected');
  result := FActionLists.Objects[ ActionListCombo.ItemIndex ];
end;


procedure TTBActionImportOptionsForm.FormDestroy(Sender: TObject);
begin
  FUpdating := true;
    FreeAndNil( FActionLists );
end;

procedure TTBActionImportOptionsForm.ActionListComboChange(
  Sender: TObject);
begin
  if not FUpdating then begin
    RefreshCategoriesList;
  end;
end;

procedure TTBActionImportOptionsForm.CategoryComboChange(Sender: TObject);
begin
  if not FUpdating then begin
    RefreshActions;
  end;
end;

procedure TTBActionImportOptionsForm.ButtonSelAllClick(Sender: TObject);
var
 t:Integer;
begin
  for t := 0 to ActionsCheckList.Items.Count-1 do begin
     ActionsCheckList.Checked[t] := true;
  end;
end;

procedure TTBActionImportOptionsForm.ButtonUnselAllClick(Sender: TObject);
var
 t:Integer;
begin
  for t := 0 to ActionsCheckList.Items.Count-1 do begin
     ActionsCheckList.Checked[t] := false;
  end;
end;

end.
