unit Frm_Tables;

interface

uses
  Math{Max},
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, TntGrids, TBXDkPanels, SpTBXDkPanels, TBXExtItems,
  SpTBXEditors, TB2ExtItems, TB2Item, TBX, SpTBXItem, TB2Toolbar, TB2Dock,
  TB2ToolWindow, ComCtrls, rmTBXTabControl, DKLang, SpTBXControls;

type
  TFrmTables = class(TFrame)
    pnl_Tables: TTBXToolWindow;
    pnl_Grid: TTBXAlignmentPanel;
    grid_Table: TTntStringGrid;
    btn_InsertTable: TSpTBXButton;
    tbar_Table_Caption: TSpTBXToolbar;
    lbl_Tbl_Caption: TSpTBXLabelItem;
    edt_Table_Caption: TSpTBXEditItem;
    tbar_Table_Dims: TSpTBXToolbar;
    cmb_Tbl_Type: TSpTBXComboBoxItem;
    lbl_Tbl_Rows: TSpTBXLabelItem;
    edt_Tbl_Rows: TTBXSpinEditItem;
    lbl_Tbl_Cols: TSpTBXLabelItem;
    edt_Tbl_Cols: TTBXSpinEditItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    DKLang: TDKLanguageController;
    procedure EOnValueChange(Sender: TTBXCustomSpinEditItem; const AValue: Extended);
    procedure EOnClick(Sender: TObject);
  private
    FListMode: Boolean;
    procedure SetListMode(aValue : Boolean);
    function GetText4List : string;
    function GetText4Table : string;
  public
    property ListMode: Boolean read FListMode write SetListMode;
  end;

implementation

uses Fm_Main, Dm_Main;

{$R *.dfm}

procedure TFrmTables.SetListMode(aValue: Boolean);
begin
 grid_Table.DefaultColWidth := 55 * ifthen(aValue, 2, 1);
 grid_Table.ColWidths[0] := 5;
 grid_Table.RowHeights[0] := 5;

 FListMode := aValue;

 if aValue then edt_Tbl_Cols.Value := 1;
 tbar_Table_Caption.Visible := aValue;
 cmb_Tbl_Type.Visible := aValue;
 lbl_Tbl_Cols.Visible := not aValue;
 edt_Tbl_Cols.Visible := not aValue;
end;

procedure TFrmTables.EOnValueChange(Sender: TTBXCustomSpinEditItem; const AValue: Extended);
begin
 if (Sender = edt_Tbl_Rows) then grid_Table.RowCount := Trunc(Max(AValue, 1)) + 1;
 if (Sender = edt_Tbl_Cols) then grid_Table.ColCount := Trunc(Max(AValue, 1)) + 1;
 with (Sender as TTBXSpinEditItem) do
  if (Value <> Max(AValue, 1)) then Value := Max(AValue, 1);
end;

function TFrmTables.GetText4List: string;
var
 cRow : Integer;
begin
 Result := Trim(edt_Table_Caption.Text); if (Result <> '') then Result := Result + #13#10;
 Result := '[list' + str_ListStyles[cmb_Tbl_Type.Strings.IndexOf(cmb_Tbl_Type.Text)] + ']' + Result;
 for cRow := 1 to Trunc(edt_Tbl_Rows.Value) do
  Result := Result + '[*]' + Trim(grid_Table.Cells[1, cRow]) + #13#10;
 Result := Result + '[/list]';
end;

function TFrmTables.GetText4Table: string;
var
 cCol, cRow : Integer;
begin
 Result := '[table]';
 for cRow := 1 to Trunc(edt_Tbl_Rows.Value) do
  for cCol := 1 to Trunc(edt_Tbl_Cols.Value) do begin
   Result := Result + Trim(grid_Table.Cells[cCol, cRow]);
   if (cCol <> Trunc(edt_Tbl_Cols.Value))
    then Result := Result + '[tab]'
    else if (cRow <> Trunc(edt_Tbl_Rows.Value)) then Result := Result + #13#10'[tr]';
  end;
 Result := Result + '[/table]';
end;

procedure TFrmTables.EOnClick(Sender: TObject);
begin
 if ListMode
  then MsgEditor.SelText := GetText4List
  else MsgEditor.SelText := GetText4Table;
// FmMain.pnl_Table.Visible := False;
end;

end.
