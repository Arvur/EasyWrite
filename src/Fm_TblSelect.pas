unit Fm_TblSelect;

interface

uses
  TB2Item{TTBItemViewer}, SpTBXItem{Toolbar&Item},
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, XPMan;

type
  TFmTblSelect = class(TForm)
    procedure TableSelPaint(Sender: TObject);
    procedure TableSelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELeave;
    procedure FormDeactivate(Sender: TObject);
    procedure SendInfo(r, c: integer; canceled: boolean);
  private
    { Private declarations }
  public
    procedure ShowSelector(aToolBar : TSpTBXToolbar; aItem : TSpTBXItem);
  end;

var
  FmTblSelect: TFmTblSelect;
  mpos: TPoint;
  css, ttop, tleft, tolerance, ncol, nrow: integer;
  UpsideDown, incancel: boolean;
  tbltxt: string;
  Caller : TSpTBXItem;

implementation

uses Fm_Main;

{$R *.dfm}

const
 cellsizeValue : Integer = 15;
 spacingValue : Integer = 2;
 srow : Integer = 4;
 scol : Integer = 5;
 txth : Integer = 25;
 BackCl : TColor = clWhite;
 LineCl : TColor = clHighlight;
 BlankCellCl : TColor = clWhite;
 OverCellCl : TColor = clHighlight;

procedure TFmTblSelect.ShowSelector(aToolBar : TSpTBXToolbar; aItem : TSpTBXItem);
var
  aViewer: TTBItemViewer;
  anItemRect: TRect;
  FormOriginPoint: TPoint;
  FromTop: TPoint;
begin
  Caller := aItem;

  aViewer := aToolBar.View.Find(aItem);
  anItemRect := aViewer.BoundsRect;
  FormOriginPoint := aToolBar.ClientToScreen(Point(anItemRect.Left, anItemRect.Bottom));
  FromTop := aToolBar.ClientToScreen(Point(anItemRect.Right, anItemRect.Top));

    UpsideDown :=  False; // UpsideDown := (aToolBar.CurrentDock = DockDown);
    Width := scol * css + spacingValue + 2;
    Height := srow * css + txth + 2;
    Left := FormOriginPoint.x;
    if UpsideDown = false then
      Top := FormOriginPoint.Y
    else
      Top := FromTop.Y - Height;
    Show;

end;

procedure TFmTblSelect.CMMouseLeave(var Message: TMessage);
begin
  mpos := point(0, 0);
  ncol := 0;
  nrow := 0;
  incancel := true;
  Self.Repaint;
end;

procedure TFmTblSelect.TableSelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if UpsideDown = false then
  begin
    if x < Self.Width - 1 then
      ncol := (x - (1 + spacingValue)) div css + 1
    else
      ncol := (x - (2 + spacingValue)) div css + 1;
    if y < Self.Height - (txth + 1) then
      nrow := (y - (1 + spacingValue)) div css + 1;
  end
  else
  begin
    if x < Self.Width - 1 then
      ncol := (x - (1 + spacingValue)) div css + 1
    else
      ncol := (x - (2 + spacingValue)) div css + 1;
    if y > txth + 1 then
      nrow := (Self.Height - (y + 1)) div css + 1;
  end;
  // povecavanje velicine po potrebi
  if ssLeft in Shift then
    if UpsideDown = false then
    begin
      if (nrow >= srow) and (y >= ((Self.Height - (txth + 2)) div css) * css
        + 1 + tolerance) then
        Self.Height := Self.Height + css;
      if (ncol >= scol) and (Self.Width < ncol * css + 2) then
        Self.Width := Self.Width + css;
    end
    else
    begin
      if (nrow >= srow) and (y <= (txth + 1) + tolerance) then
      begin
        Self.Top := Self.Top - css;
        Self.Height := Self.Height + css;
      end;
      if (ncol >= scol) and (Self.Width < ncol * css + 2) then
        Self.Width := Self.Width + css;
    end;
  mpos := point(x, y);
  Self.Repaint;
end;

procedure TFmTblSelect.TableSelPaint(Sender: TObject);
var
  myrect: TRect;
  i, j: integer;
begin
  css := cellsizeValue + spacingValue;
  // background
  Self.Canvas.Pen.Color := LineCl;
  Self.Canvas.Brush.Color := BackCl;
  myrect := Rect(0, 0, Self.Width, Self.Height);
  Self.Canvas.Rectangle(myrect);
  // table text
  if UpsideDown = false then
  begin
    if (mpos.y < Self.Height - (txth + 1) + tolerance) and ((nrow > 0) and
      (ncol > 0)) and (mpos.y > 1) and (mpos.x > 1) then
      tbltxt := IntToStr(nrow) + ' by ' + IntToStr(ncol) + ' Table'
    else
      tbltxt := 'Cancel';
    ttop := Self.Height - ((txth - Self.Canvas.TextHeight(tbltxt)) div 2
      + Self.Canvas.TextHeight(tbltxt) + 1);
    tleft := (Self.Width - (2 + Self.Canvas.TextWidth(tbltxt))) div 2 + 1;
  end
  else
  begin
    if (mpos.y > (1 + txth) - tolerance) and ((nrow > 0) and (ncol > 0)) and
      (mpos.y < Self.Height - 1) and (mpos.x > 1) then
      tbltxt := IntToStr(nrow) + ' by ' + IntToStr(ncol) + ' Table'
    else
      tbltxt := 'Cancel';
    ttop := (txth - Self.Canvas.TextHeight(tbltxt)) div 2 + 1;
    tleft := (Self.Width - (2 + Self.Canvas.TextWidth(tbltxt))) div 2 + 1;
  end;
  incancel := (tbltxt = 'Cancel');
  //painting
  if UpsideDown = false then
    for i := 0 to ((Self.Width - (2 + spacingValue)) div css) - 1 do
      for j := 0 to ((Self.Height - (2 + txth)) div css) - 1 do
      begin
        Self.Canvas.Brush.Color := BackCl;
        Self.Canvas.TextOut(tleft, ttop, tbltxt);
        if (mpos.x >= ((1 + spacingValue) + (i * css))) and
           (mpos.y >= ((1 + spacingValue) + (j * css))) and (incancel = false) then
          Self.Canvas.Brush.Color := OverCellCl
        else
          Self.Canvas.Brush.Color := BlankCellCl;
        Self.Canvas.Rectangle((1 + spacingValue) + (i * css), (1 +
          spacingValue) + (j * css), (1 + css) + (i * css), (1 + css) + (j *
          css));
      end
  else
    for i := 0 to ((Self.Width - (2 + spacingValue)) div css) - 1 do
      for j := 0 to ((Self.Height - (2 + txth)) div css) - 1 do
      begin
        Self.Canvas.Brush.Color := BackCl;
        Self.Canvas.TextOut(tleft, ttop, tbltxt);
        if (mpos.x >= ((1 + spacingValue) + (i * css))) and
           (mpos.y <= (1 + (j + 1) * css + (txth - spacingValue))) and (incancel = false) then
          Self.Canvas.Brush.Color := OverCellCl
        else
          Self.Canvas.Brush.Color := BlankCellCl;
        Self.Canvas.Rectangle((1 + spacingValue) + (i * css), (1 + spacingValue + j * css + (txth - spacingValue)),
                                  (1 + css) + (i * css), (1 + (j + 1) * css + (txth - spacingValue)));
      end;
end;

procedure TFmTblSelect.FormResize(Sender: TObject);
begin
  Self.Repaint;
end;

procedure TFmTblSelect.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    if tbltxt <> 'Cancel' then
      SendInfo(nrow, ncol, false)
    else
      SendInfo(0, 0, true);
end;

procedure TFmTblSelect.FormCreate(Sender: TObject);
begin
  Self.ControlStyle := Self.ControlStyle + [csOpaque];
  DoubleBuffered := true;
  // user settings
  cellsizeValue := 15;
  spacingValue := 2;
  srow := 4;
  scol := 5;
  txth := 25;
  BackCl := clWhite;
  LineCl := clHighlight;
  BlankCellCl := clWhite;
  OverCellCl := clHighlight;
  UpsideDown := false;
  // set by default
  tolerance := 3;
  nrow := 0;
  ncol := 0;
  mpos := Point(0, 0);
  // calculation
  css := cellsizeValue + spacingValue;
  Self.Width := scol * css + spacingValue + 2;
  Self.Height := srow * css + txth + 2;
  tbltxt := 'Cancel';
  incancel := true;
end;

procedure TFmTblSelect.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RIGHT then
  begin
    if incancel then
      ncol := 0;
    mpos.x := ncol * css + css;
    ncol := ncol + 1;
    if ncol * css + 2 + spacingValue > Self.Width then
      Self.Width := Self.Width + css;
    Self.Repaint;
  end;
  if Key = VK_LEFT then
  begin
    if incancel then
      ncol := 0;
    if ncol > 0 then
    begin
      mpos.x := ncol * css - css;
      ncol := ncol - 1;
    end;
    Self.Repaint;
  end;
  if UpsideDown = false then
  begin
    if Key = VK_DOWN then
    begin
      if incancel then
        nrow := 0;
      mpos.y := nrow * css + css + 1;
      nrow := nrow + 1;
      if nrow * css + txth + 2 > Self.Height then
        Self.Height := Self.Height + css;
      Self.Repaint;
    end;
    if Key = VK_UP then
    begin
      if incancel then
        nrow := 0;
      if nrow > 0 then
      begin
        mpos.y := nrow * css - css + 1;
        nrow := nrow - 1;
      end;
      Self.Repaint;
    end;
  end
  else
  begin
    if Key = VK_UP then
    begin
      if incancel then
        nrow := 0;
      if nrow = (Self.Height - (txth + 2)) div css then
      begin
        Self.Top := Self.Top - css;
        Self.Height := Self.Height + css;
      end;
      mpos.y := Self.Height - ((nrow + 1) * css + 1);
      nrow := nrow + 1;
      Self.Repaint;
    end;
    if Key = VK_DOWN then
    begin
      if incancel then
        nrow := 0;
      if nrow > 0 then
      begin
        mpos.y := txth + 1 + ((((Self.Height - (txth + 2)) div css) * css) - nrow * css) + css;
        nrow := nrow - 1;
      end;
      Self.Repaint;
    end;
  end;
  if Key = VK_RETURN then
    SendInfo(nrow, ncol, false);
  if Key = VK_ESCAPE then
    SendInfo(0, 0, true);
end;

procedure TFmTblSelect.FormShow(Sender: TObject);
begin
  // set by default
  nrow := 0;
  ncol := 0;
  mpos := Point(0, 0);
  // calculation
  css := cellsizeValue + spacingValue;
  Self.Width := scol * css + spacingValue + 2;
  Self.Height := srow * css + txth + 2;
  tbltxt := 'Cancel';
  incancel := true;
end;

procedure TFmTblSelect.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFmTblSelect.SendInfo(r, c: integer; canceled: boolean);
begin
//  if canceled then
//    TableSelDemo.Label1.Caption := 'You canceled the selection.'
//  else
//    TableSelDemo.Label1.Caption := 'You selected a ' + IntToStr(nrow) + ' by ' + IntToStr(ncol) + ' table.';
  if not canceled then begin
   Caller.Click;
   FmMain.FrmTables.edt_Tbl_Rows.Value := nrow + 1;
   FmMain.FrmTables.edt_Tbl_Cols.Value := ncol + 1;
  end;
  Close;
end;

end.

