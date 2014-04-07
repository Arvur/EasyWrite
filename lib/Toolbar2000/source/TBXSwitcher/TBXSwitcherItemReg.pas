unit TBXSwitcherItemReg;

interface

{$I TB2Ver.inc}

uses
  Classes, Controls, SysUtils, TB2DsgnItemEditor, TBXSwitcherItem;

procedure Register;

implementation

procedure Register;
begin
  RegisterNoIcon([TTBXSwitcherItem]);
  RegisterClasses([TTBXSwitcherItem]);
  TBRegisterItemClass(TTBXSwitcherItem, 'New TBX Switcher Item', HInstance);
end;

end.
