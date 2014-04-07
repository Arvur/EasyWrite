unit rmTBXPageControlEdit;

interface

{$I Compilerdefines.inc}

uses classes, SysUtils,
	{$ifdef BCB5_OR_HIGHER} DesignEditors, DesignIntf, {$else}dsgnintf,  {$endif}
	rmTBXPageControl, rmTBXTabControl;

type
  TrmTBXPageControlEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

Procedure Register;

implementation

resourcestring
 sTABSHEET_DEFAULT_NAME = 'rmTBXTabSheet';
 sNEW_PAGE              = 'Ne&w Page';
 sDEL_PAGE              = '&Delete Page';
 sNEXT_PAGE             = 'Ne&xt Page';
 sPREV_PAGE             = '&Previous Page';

procedure Register;
begin
   RegisterComponents('Toolbar2000', [TrmTBXPageControl, TrmTBXTabControl]);
   RegisterClass(TrmTBXTabSheet);
   RegisterComponentEditor(TrmTBXPageControl, TrmTBXPageControlEditor);
   RegisterComponentEditor(TrmTBXTabSheet, TrmTBXPageControlEditor);
end;

{ TPageControlesEditor }

procedure TrmTBXPageControlEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TrmTBXTabSheet;
  PControl : TrmTBXPageControl;
begin
  if Component is TrmTBXPageControl then
    PControl := TrmTBXPageControl(Component)
  else PControl := TrmTBXPageControl(TrmTBXTabSheet(Component).PageControl);

  case Index of
    0:  begin  //  New Page
          NewPage := TrmTBXTabSheet.Create(Designer.GetRoot);
          with NewPage do
          begin
            Parent      := PControl;
            PageControl := PControl;
            Caption     := sTABSHEET_DEFAULT_NAME + IntToStr(PControl.PageCount);
            Name        := Caption;
          end;
        end;
    1:  begin  //  Delete Page
          with PControl do
          begin
            NewPage := TrmTBXTabSheet(ActivePage);
            NewPage.PageControl := nil;
            NewPage.Free;
          end;
        end;
    2:  begin  //  Next Page
          PControl.FindNextPage(PControl.ActivePage,True,False);
        end;
    3:  begin  //  Previous Page
          PControl.FindNextPage(PControl.ActivePage,False,False);
        end;
  end;
  if Designer <> nil then Designer.Modified;
end;

function TrmTBXPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  result := sNEW_PAGE;
    1:  result := sDEL_PAGE;
    2:  result := sNEXT_PAGE;
    3:  result := sPREV_PAGE;
  end;
end;

function TrmTBXPageControlEditor.GetVerbCount: Integer;
begin
  result := 4;
end;

end.
