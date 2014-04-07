unit Dlg_Base;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Fm_Base_Modal, TBXDkPanels, SpTBXDkPanels, ImgList,
  PngImageList, pngimage, ExtCtrls, TB2Dock, TB2ToolWindow, TBX, SpTBXControls;

type
  TDlgBase = class(TFmBase_Modal)
    iLst_Dlg: TPngImageList;
    dlg_Open: TOpenDialog;
    pnl_Info: TTBXAlignmentPanel;
    lbl_Info: TSpTBXLabel;
    pnl_Buttons: TTBXAlignmentPanel;
    btn_Ok: TSpTBXButton;
    btn_Cancel: TSpTBXButton;
    btn_Params: TSpTBXButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FNoCancel : Boolean;
  public
    function ShowModalInput(CanCancel : Boolean = True) : Integer;
  end;

var
  DlgBase: TDlgBase;

implementation

{$R *.dfm}

procedure TDlgBase.FormCreate(Sender: TObject);
begin
 FNoCancel := False;
 inherited;
end;

function TDlgBase.ShowModalInput(CanCancel : Boolean = True) : Integer;
begin
 FNoCancel := not CanCancel; btn_Cancel.Enabled := CanCancel;
 Result := ShowModal;
end;

procedure TDlgBase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := (not FNoCancel) or (ModalResult = mrOk) or btn_Cancel.Enabled;
end;

end.
