unit Frm_Html;

interface

uses
  StrUtils{RightStr},
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Htmlview, ExtCtrls;

type
  TFrmHtml = class(TFrame)
    html_Viewer: THTMLViewer;
    tmr_Hint: TTimer;
    procedure EOnStyle(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
    procedure EOnHint(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EOnImage(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
    procedure EOnTimer(Sender: TObject);
    procedure EOnHotSpotCovered(Sender: TObject; const SRC: String);
  private
    FStyle, FImage : TMemoryStream;
    FOldHint : string;
    FTimerCount : Integer;
    FHintVisible : Boolean;
    FHintWindow : THintWindow;
    procedure CloseHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses Dm_Main, U_Cmn_Files;

{$R *.dfm}

constructor TFrmHtml.Create(AOwner: TComponent);
begin
 inherited;
 FStyle := TMemoryStream.Create;
 FImage := TMemoryStream.Create;

 html_Viewer.Base := HomePath;

 FHintWindow := THintWindow.Create(Self);
 FHintWindow.Color := $C0FFFF;
end;

destructor TFrmHtml.Destroy;
begin
 FStyle.Free; FImage.Free;
 FHintWindow.Free;
 inherited;
end;

procedure TFrmHtml.EOnStyle(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
begin
 FStyle.LoadFromFile(HomePath + 'styles\' + SRC);
 Stream := FStyle;
end;

procedure TFrmHtml.EOnImage(Sender: TObject; const SRC: String; var Stream: TMemoryStream);
var
 Counter : Integer;
 tmpStr : string;
begin
 Counter := 0; tmpStr := RightStr(SRC, Length(SRC) - LastDelimiter('/', SRC));
 while (Counter <= (DmMain.str_Collections.Strings.Count - 1)) and
       (not FileExists(HomePath + 'smiles\' + DmMain.str_Collections.Strings[Counter] + '\' + tmpStr)) do
  inc(Counter);
 if (Counter <= (DmMain.str_Collections.Strings.Count - 1))
  then begin
   FImage.LoadFromFile(HomePath + 'smiles\' + DmMain.str_Collections.Strings[Counter] + '\' + tmpStr);
   Stream := FImage;
  end
  else if FileExists(HomePath + 'cache\' + tmpStr) then begin
        FImage.LoadFromFile(HomePath + 'cache\' + tmpStr);
        Stream := FImage;
       end

end;

procedure TFrmHtml.EOnHint(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
 tmpStr : string;
begin
 if not tmr_Hint.Enabled // and Assigned(ActiveControl) and ActiveControl.Focused
  then begin
   tmpStr := html_Viewer.TitleAttr;
   if tmpStr = ''
    then FOldHint := ''
    else if (tmpStr <> FOldHint) then begin
          FTimerCount := 0;
          tmr_Hint.Enabled := True;
          FOldHint := tmpStr;
         end;
  end;
end;

procedure TFrmHtml.CloseHint;
begin
 tmr_Hint.Enabled := False;
 FHintWindow.ReleaseHandle;
 FHintVisible := False;
end;

procedure TFrmHtml.EOnTimer(Sender: TObject);
 const
  StartCount = 2; {timer counts before hint window opens}
  EndCount = 20;  {after this many timer counts, hint window closes}
 var
  Pt, PtV: TPoint;
  ARect: TRect;
  tmpStr: string;
begin
 Inc(FTimerCount);
 GetCursorPos(Pt); PtV := html_Viewer.ScreenToClient(Pt);
 tmpStr := html_Viewer.TitleAttr;

 if (tmpStr = '') or not PtInRect(html_Viewer.ClientRect, PtV)
  then begin
   FOldHint := '';
   CloseHint;
  end
  else
   if (tmpStr <> FOldHint)
    then begin
     FTimerCount := 0;
     FOldHint := tmpStr;
     FHintWindow.ReleaseHandle;
     FHintVisible := False;
    end
    else
     if (FTimerCount > EndCount)
      then CloseHint
      else if (FTimerCount >= StartCount) and not FHintVisible then begin
           ARect := FHintWindow.CalcHintRect(300, tmpStr, nil);
           with ARect do
            FHintWindow.ActivateHint(Rect(Pt.X, Pt.Y + 18, Pt.X + Right, Pt.Y + 18 + Bottom), tmpStr);
            FHintVisible := True;
           end;
end;

procedure TFrmHtml.EOnHotSpotCovered(Sender: TObject; const SRC: String);
begin
 html_Viewer.Hint := SRC;
end;

end.
