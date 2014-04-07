(*------------------------------------------------------------------------------
  Unit Name: TBXDbNavItem
  Author   : hans gulo (HG)
  Purpose  : TTBXItem descendant featuring DataSource's DataSet navigation
             functionality.

  History
    Created: 2004-03-03
------------------------------------------------------------------------------*)

unit TBXDbNavItem;

interface

uses
  Classes, TB2Item, TBX, DB;

type
  TTBXDbAction = (daFirst, daPrior, daNext, daLast, daInsert, daDelete, daEdit, daPost, daCancel, daRefresh);

  TTBXDbActionEvent = procedure (Sender: TObject; Action: TTBXDbAction) of object;

  TTBXNavDataLink = class;

  TTBXDBNavItem = class(TTBXCustomItem)
  private
    FConfirmDelete: Boolean;
    FActive: Boolean;
    FDbAction: TTBXDbAction;
    FDataLink: TTBXNavDataLink;
    FOnBeforeAction: TTBXDbActionEvent;
    FOnAfterAction: TTBXDbActionEvent;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDbOpKind(const Value: TTBXDbAction);
    procedure SetActive(const Value: Boolean);
    procedure InternalClick;
  protected
    function GetConfirmDeleteStored: Boolean;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Enabled property is not published and won't be stored on DFM. It is
      managed internally. See Active property. }
    property Enabled stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    { Specify the active state of data-awareness. This property replaces the
      purpose of Enabled property since the actual enabled state of this item
      should reflect the state of linked-datasource. Specifying this value will
      affect Enabled state too. }
    property Active: Boolean read FActive write SetActive default True;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete stored GetConfirmDeleteStored default True;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DbAction: TTBXDbAction read FDbAction write SetDbOpKind;
    property Action;
    //property AutoCheck; // -> irrelevant for this item purpose
    property Caption;
    //property Checked;   // -> irrelevant for this item purpose
    property DisplayMode;
    property FontSettings;
    property GroupIndex;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property Layout;
    property MaskOptions;
    property MinHeight;
    property MinWidth;
    property Options;
    { Triggered after dataset operation performed. }
    property OnAfterAction: TTBXDbActionEvent read FOnAfterAction write FOnAfterAction;
    { Triggered before dataset operation performed. }
    property OnBeforeAction: TTBXDbActionEvent read FOnBeforeAction write FOnBeforeAction;
    property ShortCut;
    property Stretch;
    property Visible;
    property OnAdjustFont;
    property OnDrawImage;
    property OnClick;
    property OnSelect;
  end;

  TTBXNavDataLink = class(TDataLink)
  private
    FTBXDbNavItem: TTBXDBNavItem;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ATBXItem: TTBXDBNavItem);
    destructor Destroy; override;
  end;

var
  { Application may define custom value for these variables }
  TBXDBNavMsgTextDeleteRecord: string;      // MessageBox text for deletion confirmation
  TBXDBNavMsgCaptionDeleteRecord: string;   // MessageBox caption text for deletion confirmation

implementation

uses Windows, Forms;

resourcestring
  SMsgTextDeleteRecord = 'Delete this record?';
  SMsgCaptionDeleteRecord = 'Confirmation';

{ TTBXDBNavItem }

procedure TTBXDBNavItem.ActiveChanged;
begin
  if not (FActive and FDataLink.Active) then
    Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TTBXDBNavItem.Click;
begin
  if FActive then
    InternalClick;
  inherited Click;
  if FActive then
    if not (csDesigning in ComponentState) and Assigned(FOnAfterAction) then
      FOnAfterAction(Self, FDbAction);
end;

constructor TTBXDBNavItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FDataLink := TTBXNavDataLink.Create(Self);
  FConfirmDelete := True;
end;

procedure TTBXDBNavItem.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := FActive and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := FActive and FDataLink.Active and not FDataLink.DataSet.EOF;
  case FDbAction of
    daFirst,
    daPrior:
      Enabled := UpEnable;
    daNext, daLast:
      Enabled := DnEnable;
    daDelete:
      Enabled := FActive and FDataLink.Active and
        FDataLink.DataSet.CanModify and
        not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  end;
end;

destructor TTBXDBNavItem.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TTBXDBNavItem.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := FActive and FDataLink.Active and FDataLink.DataSet.CanModify;
  case FDbAction of
    daInsert: Enabled := CanModify;
    daEdit: Enabled := CanModify and not FDataLink.Editing;
    daPost: Enabled := CanModify and FDataLink.Editing;
    daCancel: Enabled := CanModify and FDataLink.Editing;
    daRefresh: Enabled := CanModify;
  end;
end;

function TTBXDBNavItem.GetConfirmDeleteStored: Boolean;
begin
  Result := (FDbAction = daDelete) and not FConfirmDelete;
end;

function TTBXDBNavItem.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TTBXDBNavItem.InternalClick;

  function GetActiveFormHandle: HWND;
  begin
    if Screen.ActiveForm <> nil then
    begin
      Result := Screen.ActiveForm.Handle;
      if not Windows.IsWindowVisible(Result) then
        Result := Application.Handle;
    end else
      Result := Application.Handle;
    if not (IsWindow(Result) and IsWindowVisible(Result)) then
      Result := 0;
  end;

begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FOnBeforeAction) then
      FOnBeforeAction(Self, FDbAction);
    with DataSource.DataSet do
    begin
      case FDbAction of
        daPrior: Prior;
        daNext: Next;
        daFirst: First;
        daLast: Last;
        daInsert: Insert;
        daEdit: Edit;
        daCancel: Cancel;
        daPost: Post;
        daRefresh: Refresh;
        daDelete:
          if not FConfirmDelete or (MessageBox(GetActiveFormHandle,
            PChar(TBXDBNavMsgTextDeleteRecord), PChar(TBXDBNavMsgCaptionDeleteRecord),
            MB_YESNO or MB_ICONQUESTION) <> IDNO) then Delete;
      end;
    end;
  end;
end;

procedure TTBXDBNavItem.Loaded;
begin
  inherited Loaded;
  Enabled := True;
  ActiveChanged;
end;

procedure TTBXDBNavItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TTBXDBNavItem.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not (csLoading in ComponentState) then
      ActiveChanged;
  end;
end;

procedure TTBXDBNavItem.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TTBXDBNavItem.SetDbOpKind(const Value: TTBXDbAction);
begin
  FDbAction := Value;
end;

{ TTBXNavDataLink }

procedure TTBXNavDataLink.ActiveChanged;
begin
  if FTBXDbNavItem <> nil then FTBXDbNavItem.ActiveChanged;
end;

constructor TTBXNavDataLink.Create(ATBXItem: TTBXDBNavItem);
begin
  inherited Create;
  FTBXDbNavItem := ATBXItem;
  VisualControl := True;
end;

procedure TTBXNavDataLink.DataSetChanged;
begin
  if FTBXDbNavItem <> nil then FTBXDbNavItem.DataChanged;
end;

destructor TTBXNavDataLink.Destroy;
begin
  FTBXDbNavItem := nil;
  inherited Destroy;
end;

procedure TTBXNavDataLink.EditingChanged;
begin
  if FTBXDbNavItem <> nil then FTBXDbNavItem.EditingChanged;
end;

initialization
  TBXDBNavMsgTextDeleteRecord := SMsgTextDeleteRecord;
  TBXDBNavMsgCaptionDeleteRecord := SMsgCaptionDeleteRecord;
end.
