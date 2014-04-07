unit TB2DsgnActionImporter;

{
  NEW UNIT BASED ON TB2DsgnConverter's code, but designed to allow creation
  of toolbars from Action lists.

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

  $jrsoftware: tb2k/Source/TB2DsgnConverter.pas,v 1.14 2004/02/26 07:05:57 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, SysUtils, Classes, Controls, Forms, Menus, StdCtrls,
  TB2Item, TBX;

type
  TTBActionImporterForm = class(TForm)
    MessageList: TListBox;
    CloseButton: TButton;
    CopyButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure Log(s:String);

  end;

procedure DoActionImport(const ParentItem: TTBCustomItem; const Owner: TComponent);


implementation

{$R *.DFM}

uses
  Clipbrd, Contnrs, ActnList, TB2DsgnActionImportOptions;

function ExcludePrefix(prefix,aStrValue:String):String;
begin
  if Pos(prefix,aStrValue)=1 then begin
      result := Copy(aStrValue,Length(prefix)+1,Length(aStrValue));
  end else begin
      result := aStrValue;
  end;
end;

procedure _ActionImport(  {progress output to:}         ActionImporterForm: TTBActionImporterForm;
                          {convert from action items}   ActionList: TActionList;
                                                        SelectedActions:TObjectList;
                                                        ComponentNamePrefix:String; {blank=no prefix}
                                                        ActionNamePrefix:String;    {blank=use full action name as part of component name. ComponentNamePrefix and ActionNamePrefix can't both be blank. }
                                                        pullDownMenus:Boolean;      {true=pulldown menu,false=toolbar}
                            {xxx more options here}
                          {destination:}                TBItem: TTBCustomItem;
                                                        const Owner: TComponent
                          );
var
    I: Integer;
    action:TBasicAction;
    customAction:TCustomAction;
    submenu:TTBXSubMenuItem;
    item: TTBXCustomItem;
    N,categoryName,itemName,subMenuName: String;
    procedure Log(s:String);
    begin
      if Assigned(ActionImporterForm) then
          ActionImporterForm.Log(s);
    end;
    {Creates or Returns a Sub-Menu Item given the component name}
    function CreateOrFindSubMenuItem(componentName:String;defaultCaption:String):TTBXSubMenuItem;
    var
      K:Integer;
    begin
        Assert(Length(componentName)>0);
        for K := 0 to TBItem.Count-1 do begin
          if not TBItem.Items[K].InheritsFrom(TTBXSubMenuItem) then
              continue;
          result := TBItem.Items[K] as TTBXSubMenuItem;
          if result.Name=componentName then
              exit;
        end;
        result := TTBXSubmenuItem.Create(Owner);
        result.Name := componentName;
        result.Caption := defaultCaption; // Usually the same as the category text, ie File, Edit.
        TBItem.Add(result);
    end;
    function CreateOrFindItem( parent:TTBCustomItem; itemName:String):TTBCustomItem;
    var
      K:Integer;
    begin
      Assert(Length(itemName)>0);
      for K := 0 to parent.Count-1 do begin
         result := parent.Items[K] as TTBCustomItem;
         if result.Name=itemName then exit;
      end;
      result := TTBXItem.Create(Owner);
      result.Name := itemName;
      result.Caption := itemName;
      parent.Add(result);
    end;
    
  begin
    for I := 0 to SelectedActions.Count-1 do begin
      action := TBasicAction(SelectedActions[I]);

      itemName := ComponentNamePrefix+ExcludePrefix( actionNamePrefix, action.Name);

      if pullDownMenus then begin
          if (action is TCustomAction) then begin
             categoryName := TCustomAction(action).Category;
             if categoryName='' then begin
                 Log(Format('No component category set for action %s. "Unknown" used.', [ action.Name ]) );
                 categoryName := 'Unknown';
             end;
          end else begin
             Log(Format('Unable to determine component category for action %s. Unknown used.', [ action.Name ]) );
             categoryName := 'Unknown';
          end;
          subMenuName := ComponentNamePrefix+categoryName;
          submenu := CreateOrFindSubMenuItem( subMenuName, categoryName );
          item    := CreateOrFindItem(submenu, itemName ) as TTBXCustomItem;
      end else begin
          submenu := nil;
          item    := CreateOrFindItem(TBItem, itemName ) as TTBXCustomItem;
      end;
      Assert(Assigned(item));
      item.Action := action;
      if (action is TCustomAction) then begin // Update
          customAction := TCustomAction(action);
          item.Caption := customAction.Caption;
          item.Checked := customAction.Checked;
          item.Enabled := customAction.Enabled;
          item.HelpContext := customAction.HelpContext;
          item.ImageIndex :=  customAction.ImageIndex;
          item.ShortCut := customAction.ShortCut;
          item.Hint     := customAction.Hint;
          item.Tag      := customAction.Tag;
          item.Visible  := customAction.Visible;
          if Assigned(customAction.ActionList) then begin
            if Assigned(customAction.ActionList.Images) then begin
                item.Images := customAction.ActionList.Images;
            end;
          end;
          item.GroupIndex :=  customAction.GroupIndex;
          item.AutoCheck  :=  customAction.AutoCheck;
          
          //----------------------------------------------------------------
          // Warren Added HelpKeyword String property to TTBCustomItem,
          // for to support string topic/keyword links, just like VCL
          // TControls base class. If this breaks your standard version of
          // TB2K, just comment the next line out!
          //----------------------------------------------------------------
//          item.HelpKeyword := customAction.HelpKeyword;

      end

      //---------------------------------------------------------------
      // Note that TBItem.Add(item) happens above, implicitly inside
      // CreateOrFindItem!  First time through it would actually
      // create, second time through it just finds and updates the
      // existing menu items.
      //---------------------------------------------------------------      

      //if item.GroupIndex <> 0 then
      //  Log(Format(SPropNotTransferred, ['GroupIndex', Dst.Name]));
      //if item.RadioItem then
      // Log(Format(SPropNotTransferred, ['RadioItem', Dst.Name]));

    end;{for loop}

end;
  
procedure DoActionImport(const ParentItem: TTBCustomItem; const Owner: TComponent);
const
  SPropNotTransferred = 'Warning: %s property not transferred on ''%s''.';
var
  ActionImporterForm: TTBActionImporterForm;

  procedure Log(const S: String);
  begin
    if not Assigned(ActionImporterForm) then exit;
    ActionImporterForm.Log(S);
  end;


var
  OptionsForm: TTBActionImportOptionsForm;
  I: Integer;
  C: TComponent;
  ActionList: TActionList;
  SelectedActions:TObjectList;
  componentNamePrefix,actionNamePrefix:String;
  pullDownMenus:Boolean;
begin
  ActionList := nil;
  OptionsForm := TTBActionImportOptionsForm.Create(Application);
  OptionsForm.Caption := ParentItem.Name +' - '+OptionsForm.Caption;
  SelectedActions := nil;

  try // ensure SelectedActions is freed.

  try // ensure OptionsForm is freed.

    for I := 0 to Owner.ComponentCount-1 do begin
      C := Owner.Components[I];
      if (C is TActionList) then
        OptionsForm.AddActionList( C, Owner.Name+'.'+TActionList(C).Name );
    end;
    if OptionsForm.ActionListCombo.Items.Count = 0 then
      raise Exception.Create('Could not find any Action Lists on the form.');
    OptionsForm.ActionListCombo.ItemIndex := 0;
    OptionsForm.RefreshCategoriesList;

    if (OptionsForm.ShowModal <> mrOK) or (OptionsForm.ActionListCombo.ItemIndex < 0) then
      Exit;
      ActionList := TActionList(OptionsForm.GetActionList);
      SelectedActions := TObjectList.Create;
      SelectedActions.OwnsObjects := false;

      // Get selected actions from options form:
      for I := 0 to OptionsForm.ActionsCheckList.Count-1 do begin
          if not OptionsForm.ActionsCheckList.Checked[i] then
              continue;
          C := TComponent(OptionsForm.ActionsCheckList.Items.Objects[i]);
          SelectedActions.Add(C);
      end;
      { get options from dialog}
      componentNamePrefix := OptionsForm.editComponentNamePrefix.Text;
      actionNamePrefix :=    OptionsForm.editActionNamePrefix.Text;
      pullDownMenus   :=     OptionsForm.cbPulldownMenus.Checked;

  finally
    OptionsForm.Free;
  end;

  if SelectedActions.Count=0 then begin
      Application.MessageBox('No Action items selected. No action taken.','No Items Selected.',MB_OK);
      exit;
  end;
  
  ParentItem.SubMenuImages := ActionList.Images;
  ActionImporterForm := TTBActionImporterForm.Create(Application);
  ActionImporterForm.Show;
  ActionImporterForm.Update;
  Log(Format('Importing selected items from ''%s'', please wait...', [ActionList.Name]));
  ParentItem.ViewBeginUpdate;
  try // ensure ParentItem.ViewEndUpdate is called!
    _ActionImport(
           {form to show progress and error messages on }ActionImporterForm,
           {the action list}
              ActionList,
           {a subset of actions}
              SelectedActions,
          {strings for naming}
              componentNamePrefix, actionNamePrefix,
          {whether to create as a submenu menu (pulldown menus) or just toolbar items}
              pullDownMenus,
          {parent, item container:}
              ParentItem as TTBCustomItem,
          {TComponent.Owner}
              Owner );
    Log('Finished.');
  finally
    ParentItem.ViewEndUpdate;
  end;
  Log('Done!');
  ActionImporterForm.CloseButton.Enabled := True;
  ActionImporterForm.CopyButton.Enabled := True;
  finally
      FreeAndNil(SelectedActions);
  end;
end;


{ TTBActionImporterForm }

procedure TTBActionImporterForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTBActionImporterForm.Log(s:String);
begin
      MessageList.Items.Add(S);
      MessageList.TopIndex := MessageList.Items.Count-1;
      Update;
end;


procedure TTBActionImporterForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTBActionImporterForm.CopyButtonClick(Sender: TObject);
begin
  Clipboard.AsText := MessageList.Items.Text;
end;


procedure FreeActionImporterForms;
var
  I: Integer;
  Form: TCustomForm;
label Restart;
begin
  Restart:
  for I := 0 to Screen.CustomFormCount-1 do begin
    Form := Screen.CustomForms[I];
    if Form is TTBActionImporterForm then begin
      Form.Free;
      goto Restart;
    end;
  end;
end;

initialization
finalization
  FreeActionImporterForms;
end.
