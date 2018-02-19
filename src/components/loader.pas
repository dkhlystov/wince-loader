{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit loader;

{$warn 5023 off : no warning about unused units}
interface

uses
  ImageButton, TyphonPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ImageButton', @ImageButton.Register);
end;

initialization
  RegisterPackage('loader', @Register);
end.
