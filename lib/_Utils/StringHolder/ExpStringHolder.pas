{*******************************************************************************

         Explido String Holder Component
         Version 1.0

         Copyright (C) 2004 Explido Software GmbH & Co. KG
         All Rights Reserved.

 The contents of this file are subject to the Mozilla Public License
 Version 1.1 (the "License"); you may not use this file except in compliance
 with the License. You may obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1.1.html

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Original Code is: ExpStringHolder.PAS, released on 2004-05-07.

 The Initial Developer of the Original Code is Marc Geldon [marcgeldon@web.de]
 Portions created by Marc Geldon are Copyright (C) 2004 Marc Geldon.
 All Rights Reserved.

 Contributor(s):
 -

 Change(s):
 -

 Last Modified: 2004-05-07
*******************************************************************************}

unit ExpStringHolder;

interface

uses
  SysUtils,
  Classes,
  Dialogs;

type
  TTExpStringHolder = class(TComponent)
  private
    { Private-Deklarationen }
    FStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Strings: TStrings read FStrings write SetStrings;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Explido', [TTExpStringHolder]);
end;

constructor TTExpStringHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
end;

destructor TTExpStringHolder.Destroy;
begin
  FreeAndNil(FStrings);
  inherited Destroy;
end;

procedure TTExpStringHolder.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

end.
 
