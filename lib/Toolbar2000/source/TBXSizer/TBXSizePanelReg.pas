unit TBXSizePanelReg;
// TBX Sizer panel
// Copyright 2005 Konstantin Rudenko. All Rights Reserved

interface

{$I TB2Ver.inc}

procedure Register;

implementation

uses
  Classes,
  {$IFDEF JR_D6} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF}
  TB2DsgnItemEditor,
  TBXSizer;

procedure Register;
begin
 RegisterNoIcon([TTBXSizerItem]);
 RegisterClasses([TTBXSizerItem]);
 TBRegisterItemClass(TTBXSizerItem, 'New TBX Sizer Item', HInstance);
end;

end.
