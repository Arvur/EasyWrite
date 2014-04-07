
{*****************************************************************************}
{                                                                             }
{    Tnt LX Controls                                                          }
{      http://www.tntware.com/delphicontrols/lx/                              }
{        Version: 1.3.0                                                       }
{                                                                             }
{    Copyright (c) 2003-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntLXControls_Register;

{$INCLUDE ..\Source\TntCompilers.inc}

interface

procedure Register;

implementation

uses
  Classes, DesignIntf, TntLXLookupCtrls, TntLXVarArrayDataSet, TntLXStringGrids,
  TntLXCombos, TntLXDBGrids, TntLXRichEdits, TntLXAdoDataSet, TntLXListView,
  TntLXHotLabel, TntLXCsvUtils, TntLXHttpRequestor, TntLXFetchUrl, TntLXPasswordEdit,
  TntLXCheckLst, TntLXDBCtrls, TntLXDataSet, TntLXComCtrls, TntLXDialogs;

procedure Register;
begin
  RegisterComponents('TntLX', [TTntDBLookupListBoxLX, TTntDBLookupComboBoxLX]);
  RegisterComponents('TntLX', [TTntVarArrayDataSet]);
  RegisterComponents('TntLX', [TTntMonthCalendarLX]);
  RegisterComponents('TntLX', [TTntStringGridLX,
                                 TTntStringGridLXEdit,
                                 TTntStringGridLXComboBox,
                                 TTntStringGridLXDatePicker]);
  RegisterComponents('TntLX', [TTntComboBoxLX, TTntDBComboBoxLX, TTntComboBoxLX_Action]);
  Registercomponents('TntLX', [TTntDbGridLX]);
  RegisterComponents('TntLX', [TTntRichEditLX, TTntDBRichEditLX]);
  RegisterComponents('TntLX', [TTntADODataSetLX]);
  RegisterComponents('TntLX', [TTntADOTableLX]);
  RegisterComponents('TntLX', [TTntADOQueryLX]);
  RegisterComponents('TntLX', [TTntListViewLX]);

  RegisterComponents('TntLX', [TTntHotLabel]);
  RegisterComponents('TntLX', [TTntCSVTable]);
  RegisterComponents('TntLX', [TTntHttpRequestor]);
  RegisterComponents('TntLX', [TTntFetchUrl]);

  RegisterComponents('TntLX', [TTntPasswordEdit]);

  RegisterComponents('TntLX', [TTntCheckListBoxLX]);
  RegisterComponents('TntLX', [TTntDBEditLX]);
  RegisterComponents('TntLX', [TTntDBMemoLX]);
  RegisterComponents('TntLX', [TTntDBRichEditLX]);

  RegisterComponents('TntLX', [TTntDataSet]);

  RegisterComponents('TntLX', [TTntOpenDialogLX]);
  RegisterComponents('TntLX', [TTntSaveDialogLX]);

  {$IFDEF COMPILER_10_UP}
  TntLXDataSet.RegisterTntFields;
  {$ENDIF}
end;

end.
