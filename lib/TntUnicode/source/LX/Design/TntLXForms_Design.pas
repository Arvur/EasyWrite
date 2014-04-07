
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXForms_Design;

{$INCLUDE ..\Source\TntCompilers.inc}

interface

procedure Register;

implementation

uses
  Classes, Forms, TntLXForms, TntForms_Design, DesignIntf, ToolsApi, DesignEditors,
  DMForm, WCtlForm;

type
  TTntFormLXModule = class(TCustomModule)
  public
    class function DesignClass: TComponentClass; override;
  end;

  TTntNewTntFormLXWizard = class(TTntNewFormWizard)
  protected
    function ThisFormClass: TComponentClass; override;
  public
    function GetName: AnsiString; override;
    function GetComment: AnsiString; override;
  end;

procedure Register;
begin
  //--
  RegisterCustomModule(TTntFormLX, TTntFormLXModule);
  //--
  RegisterPackageWizard(TTntNewTntFormLXWizard.Create);
end;

{ TTntFormLXModule }

class function TTntFormLXModule.DesignClass: TComponentClass;
begin
  result := TTntFormLX;
end;

{ TTntNewTntFormLXWizard }

function TTntNewTntFormLXWizard.ThisFormClass: TComponentClass;
begin
  result := TTntFormLX;
end;

function TTntNewTntFormLXWizard.GetName: AnsiString;
begin
  result := ThisFormName + ' (LX)';
end;

function TTntNewTntFormLXWizard.GetComment: AnsiString;
begin
  Result := 'Creates a new TntFormLX';
end;

end.
