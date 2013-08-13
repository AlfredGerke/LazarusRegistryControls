{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RegistryControls;

interface

uses
  regcheckbox, regradiobutton, regradiogroup, regedit, reglistbox, 
  regcombobox, regcheckgroup, regchecklistbox, regvaluelisteditor, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regcheckbox', @regcheckbox.Register);
  RegisterUnit('regradiobutton', @regradiobutton.Register);
  RegisterUnit('regradiogroup', @regradiogroup.Register);
  RegisterUnit('regedit', @regedit.Register);
  RegisterUnit('reglistbox', @reglistbox.Register);
  RegisterUnit('regcombobox', @regcombobox.Register);
  RegisterUnit('regcheckgroup', @regcheckgroup.Register);
  RegisterUnit('regchecklistbox', @regchecklistbox.Register);
  RegisterUnit('regvaluelisteditor', @regvaluelisteditor.Register);
end;

initialization
  RegisterPackage('RegistryControls', @Register);
end.
