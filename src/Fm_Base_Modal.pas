unit Fm_Base_Modal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TBXDkPanels, SpTBXDkPanels, ExtCtrls, TB2Dock, TB2ToolWindow,
  TBX, SpTBXControls;

type
  TFmBase_Modal = class(TForm)
    pnl_Background: TTBXToolWindow;
    pnl_Logo: TTBXAlignmentPanel;
    img_Logo: TImage;
    lbl_Logo: TSpTBXLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var
  FmBase_Modal: TFmBase_Modal;

implementation

{$R *.dfm}

procedure TFmBase_Modal.FormCreate(Sender: TObject);
begin
 Self.Caption := Application.Title;
end;

procedure TFmBase_Modal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Key = VK_RETURN) then ModalResult := mrOk;
 if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

procedure TFmBase_Modal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

end.
