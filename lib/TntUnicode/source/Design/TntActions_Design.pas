
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntActions_Design;

{$INCLUDE ..\Source\TntCompilers.inc}
{$DEFINE TNTACTIONS}

interface

procedure Register;

implementation

uses
  Classes, ActnList, TntActnList, StdActns, TntStdActns,
  ExtActns, TntExtActns, ListActns, TntListActns, BandActn, TntBandActn,
  DBActns, TntDBActns, TntDesignEditors_Design;

procedure Register;
begin
  RegisterClass(TTntAction);
  {$IFDEF TNTACTIONS}
    RegisterActions('', [ TTntAction ], nil);
  {$ENDIF}

  // StdActns
  RegisterClass(TTntEditAction);
  RegisterClass(TTntEditCut);
  RegisterClass(TTntEditCopy);
  RegisterClass(TTntEditPaste);
  RegisterClass(TTntEditSelectAll);
  RegisterClass(TTntEditUndo);
  RegisterClass(TTntEditDelete);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntEdit', [ TTntEditAction, TTntEditCut, TTntEditCopy, TTntEditPaste,
                                 TTntEditSelectAll, TTntEditUndo, TTntEditDelete ], nil);
  {$ENDIF}

  RegisterClass(TTntWindowAction);
  RegisterClass(TTntWindowClose);
  RegisterClass(TTntWindowCascade);
  RegisterClass(TTntWindowTileHorizontal);
  RegisterClass(TTntWindowTileVertical);
  RegisterClass(TTntWindowMinimizeAll);
  RegisterClass(TTntWindowArrange);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntWindow', [ TTntWindowAction, TTntWindowClose, TTntWindowCascade, TTntWindowTileHorizontal,
                                   TTntWindowTileVertical, TTntWindowMinimizeAll, TTntWindowArrange ], nil);
  {$ENDIF}

  RegisterClass(TTntHelpAction);
  RegisterClass(TTntHelpContents);
  RegisterClass(TTntHelpTopicSearch);
  RegisterClass(TTntHelpOnHelp);
  RegisterClass(TTntHelpContextAction);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntHelp', [ TTntHelpAction, TTntHelpContents, TTntHelpTopicSearch,
                                 TTntHelpOnHelp, TTntHelpContextAction ], nil);
  {$ENDIF}

  RegisterClass(TTntFileOpen);
  RegisterClass(TTntFileOpenWith);
  RegisterClass(TTntFileSaveAs);
  RegisterClass(TTntFilePrintSetup);
  RegisterClass(TTntFileExit);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntFile', [ TTntFileOpen, TTntFileOpenWith, TTntFileSaveAs,
                                 TTntFilePrintSetup, TTntFileExit ], nil);
  {$ENDIF}

  RegisterClass(TTntSearchFind);
  RegisterClass(TTntSearchReplace);
  RegisterClass(TTntSearchFindFirst);
  RegisterClass(TTntSearchFindNext);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntSearch', [ TTntSearchFind, TTntSearchReplace, TTntSearchFindFirst,
                                   TTntSearchFindNext ], nil);
  {$ENDIF}

  RegisterClass(TTntFontEdit);
  RegisterClass(TTntColorSelect);
  RegisterClass(TTntPrintDlg);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntDialog', [ TTntFontEdit, TTntColorSelect, TTntPrintDlg ], nil);
  {$ENDIF}

  // ExtActns
  RegisterClass(TTntFileRun);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntFile', [ TTntFileRun ], nil);
  {$ENDIF}

  RegisterClass(TTntRichEditAction);
  RegisterClass(TTntRichEditBold);
  RegisterClass(TTntRichEditItalic);
  RegisterClass(TTntRichEditUnderline);
  RegisterClass(TTntRichEditStrikeOut);
  RegisterClass(TTntRichEditBullets);
  RegisterClass(TTntRichEditAlignLeft);
  RegisterClass(TTntRichEditAlignRight);
  RegisterClass(TTntRichEditAlignCenter);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntFormat', [ TTntRichEditAction, TTntRichEditBold, TTntRichEditItalic,
                                   TTntRichEditUnderline, TTntRichEditStrikeOut, TTntRichEditBullets,
                                   TTntRichEditAlignLeft, TTntRichEditAlignRight, TTntRichEditAlignCenter ], nil);
  {$ENDIF}

  RegisterClass(TTntPreviousTab);
  RegisterClass(TTntNextTab);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntTab', [ TTntPreviousTab, TTntNextTab ], nil);
  {$ENDIF}

  RegisterClass(TTntOpenPicture);
  RegisterClass(TTntSavePicture);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntDialog', [ TTntOpenPicture, TTntSavePicture ], nil);
  {$ENDIF}

  RegisterClass(TTntURLAction);
  RegisterClass(TTntBrowseURL);
  RegisterClass(TTntDownLoadURL);
  RegisterClass(TTntSendMail);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntInternet', [ TTntURLAction, TTntBrowseURL, TTntDownLoadURL, TTntSendMail ], nil);
  {$ENDIF}

  RegisterClass(TTntListControlCopySelection);
  RegisterClass(TTntListControlDeleteSelection);
  RegisterClass(TTntListControlSelectAll);
  RegisterClass(TTntListControlClearSelection);
  RegisterClass(TTntListControlMoveSelection);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntList', [ TTntListControlCopySelection, TTntListControlDeleteSelection, TTntListControlSelectAll,
                                 TTntListControlClearSelection, TTntListControlMoveSelection ], nil);
  {$ENDIF}

  // ListActns
  RegisterClass(TTntStaticListAction);
  RegisterClass(TTntVirtualListAction);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntExtended', [ TTntStaticListAction, TTntVirtualListAction ], nil);
  {$ENDIF}

  {$IFDEF COMPILER_7_UP}
  RegisterClass(TTntFilePageSetup);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntExtended', [ TTntFilePageSetup ], nil);
  {$ENDIF}

  {$ENDIF}
  // DBActns
  RegisterClass(TTntDataSetAction);
  RegisterClass(TTntDataSetFirst);
  RegisterClass(TTntDataSetPrior);
  RegisterClass(TTntDataSetNext);
  RegisterClass(TTntDataSetLast);
  RegisterClass(TTntDataSetInsert);
  RegisterClass(TTntDataSetDelete);
  RegisterClass(TTntDataSetEdit);
  RegisterClass(TTntDataSetPost);
  RegisterClass(TTntDataSetCancel);
  RegisterClass(TTntDataSetRefresh);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntDataSet', [ TTntDataSetAction, TTntDataSetFirst, TTntDataSetPrior, TTntDataSetNext,
                                    TTntDataSetLast, TTntDataSetInsert, TTntDataSetDelete, TTntDataSetEdit,
                                    TTntDataSetPost, TTntDataSetCancel, TTntDataSetRefresh ], nil);
  {$ENDIF}

  // BandActn
  RegisterClass(TTntCustomizeActionBars);
  {$IFDEF TNTACTIONS}
    RegisterActions('TntExtended', [ TTntCustomizeActionBars ], nil);
  {$ENDIF}

end;

//------------------------

function GetTntActionClass(OldActionClass: TContainedActionClass): TContainedActionClass;
begin
  Result := TContainedActionClass(GetClass('TTnt' + Copy(OldActionClass.ClassName, 2, Length(OldActionClass.ClassName))));
end;

type
  TAccessContainedAction = class(TContainedAction);

function UpgradeAction(ActionList: TTntActionList; OldAction: TContainedAction): TContainedAction;
var
  Name: TComponentName;
  i: integer;
  NewActionClass: TContainedActionClass;
begin
  Result := nil;
  if (OldAction = nil) or (OldAction.Owner = nil) or (OldAction.Name = '') then
    Exit;

  NewActionClass := GetTntActionClass(TContainedActionClass(OldAction.ClassType));
  if NewActionClass <> nil then begin
    // create new action
    Result := NewActionClass.Create(OldAction.Owner) as TContainedAction;
    Include(TAccessContainedAction(Result).FComponentStyle, csTransient);
    // copy base class info
    Result.ActionComponent := OldAction.ActionComponent;
    Result.Category := OldAction.Category; { Assign Category before ActionList/Index to avoid flicker. }
    Result.ActionList := ActionList;
    Result.Index := OldAction.Index;
    // assign props
    Result.Assign(OldAction);
    // point all links to this new action
    for i := TAccessContainedAction(OldAction).FClients.Count - 1 downto 0 do
      TBasicActionLink(TAccessContainedAction(OldAction).FClients[i]).Action := Result;
    // free old object, preserve name...
    Name := OldAction.Name;
    OldAction.Free;
    Result.Name := Name; { link up to old name }
    Exclude(TAccessContainedAction(Result).FComponentStyle, csTransient);
  end;
end;

procedure TntActionList_UpgradeActionListItems(ActionList: TTntActionList);
var
  DesignerNotify: IDesignerNotify;
  Designer: ITntDesigner;
  TntSelections: TTntDesignerSelections;
  i: integer;
  OldAction, NewAction: TContainedAction;
begin
  DesignerNotify := FindRootDesigner(ActionList);
  if (DesignerNotify <> nil) then begin
    DesignerNotify.QueryInterface(ITntDesigner, Designer);
    if (Designer <> nil) then begin
      TntSelections := TTntDesignerSelections.Create;
      try
        Designer.GetSelections(TntSelections);
        for i := ActionList.ActionCount - 1 downto 0 do begin
          OldAction := ActionList.Actions[i];
          NewAction := UpgradeAction(ActionList, OldAction);
          if (NewAction <> nil) then
            TntSelections.ReplaceSelection(OldAction, NewAction);
        end;
        Designer.SetSelections(TntSelections);
      finally
        TntSelections.Free;
      end;
    end;
  end;
end;

initialization
  UpgradeActionListItemsProc := TntActionList_UpgradeActionListItems;

end.
