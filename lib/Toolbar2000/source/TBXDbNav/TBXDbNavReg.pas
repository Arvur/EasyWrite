(*------------------------------------------------------------------------------
  Unit Name: TBXDbNavReg
  Author   : hans gulo (HG)
  Purpose  : Component registration unit for TTBXDbNavItem class.

  History
------------------------------------------------------------------------------*)

unit TBXDbNavReg;

interface

procedure Register;

implementation

uses Classes, TB2DsgnItemEditor, TBXDbNavItem;

procedure Register;
begin
  RegisterNoIcon([TTBXDBNavItem]);
  RegisterClass(TTBXDBNavItem);
  TBRegisterItemClass(TTBXDBNavItem, 'New TBX DB Na&vigator Item', HInstance);
end;

end.

