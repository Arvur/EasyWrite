unit TBXSwitcherItem;

interface

uses
  Classes, TB2Item, TBX, TBXThemes,
  // We don't actually use TBXSwitcher, but it makes sure the standard
  // TBX themes are linked into the EXE.
  TBXSwitcher;

type
  TTBXSwitcherItem = class(TTBXCustomItem)
  private
    FCaptionSuffix: String;
    FCaptionPrefix: String;
    FUpdatingItems: Boolean;
    procedure ItemClick(Sender: TObject);
    procedure SetCaptionPrefix(const Value: String);
    procedure SetCaptionSuffix(const Value: String);
  protected
    procedure Change (NeedResize: Boolean); override;
    procedure EnabledChanged; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure UpdateItems;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
  published
    property CaptionPrefix: String read FCaptionPrefix write SetCaptionPrefix;
    property CaptionSuffix: String read FCaptionSuffix write SetCaptionSuffix;
    property Enabled;
  end;

implementation

{ TTBXSwitcherItem }

procedure TTBXSwitcherItem.Change(NeedResize: Boolean);
begin
  inherited Change(NeedResize);

  if csDesigning in ComponentState then
  begin
    // InitiateAction isn't called at design time, so we catch theme
    // changes here and force the design-time items to update.
    if not FUpdatingItems then
    begin
      FUpdatingItems := True;
      try
        UpdateItems;
      finally
        FUpdatingItems := False;
      end;
    end;
  end;
end;

constructor TTBXSwitcherItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := '(Theme Switcher)';

  UpdateItems;
end;

procedure TTBXSwitcherItem.EnabledChanged;
var
  I: Integer;
begin
  inherited EnabledChanged;
  for I := 0 to Count-1 do
    Items[I].Enabled := Enabled;
end;

procedure TTBXSwitcherItem.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  // Do nothing -- this prevents our subitems from being saved in the .dfm
end;

procedure TTBXSwitcherItem.InitiateAction;
begin
  inherited InitiateAction;

  UpdateItems;
end;

procedure TTBXSwitcherItem.ItemClick(Sender: TObject);
var
  ItemCaption: String;
  ThemeName: String;
begin
  ItemCaption := (Sender as TTBXCustomItem).Caption;
  // Remove CaptionPrefix and CaptionSuffix from item caption
  ThemeName := Copy(ItemCaption, 1 + Length(FCaptionPrefix),
    Length(ItemCaption) - Length(FCaptionPrefix) - Length(FCaptionSuffix));

  TBXSetTheme(ThemeName);
end;

procedure TTBXSwitcherItem.SetCaptionPrefix(const Value: String);
begin
  FCaptionPrefix := Value;
  UpdateItems;
end;

procedure TTBXSwitcherItem.SetCaptionSuffix(const Value: String);
begin
  FCaptionSuffix := Value;
  UpdateItems;
end;

procedure TTBXSwitcherItem.UpdateItems;
var
  I: Integer;
  ItemCount: Integer;
  Item: TTBCustomItem;
  Themes: TStringList;
  ThemeName: String;
  DefaultThemeFound: Boolean;
begin
  Themes := TStringList.Create;
  try
    GetAvailableTBXThemes(Themes);

    // Sort the themes, but put 'Default' first
    I := Themes.IndexOf('Default');
    DefaultThemeFound := (I >= 0);
    if DefaultThemeFound then
      Themes.Delete(I);

    Themes.Sort;
    if DefaultThemeFound then
      Themes.Insert(0, 'Default');

    ItemCount := Themes.Count;
    if ItemCount < 0 then
      ItemCount := 0;

    // Adjust until we have the right number of subitems
    while Count < ItemCount do
    begin
      Item := TTBXCustomItem.Create(Self);
      Item.Enabled := Enabled;
      Item.OnClick := ItemClick;
      Add(Item);
    end;
    while Count > ItemCount do
      Items[Count - 1].Free;

    // Set the captions
    for I := 0 to ItemCount - 1 do
    begin
      Item := Items[I];
      ThemeName := Themes[I];
      Item.Caption := FCaptionPrefix + ThemeName + FCaptionSuffix;
      Item.Checked := (TBXCurrentTheme = ThemeName);
    end;
  finally
    Themes.Free;
  end;
end;

end.
