{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit registrycontrols;

interface

uses
  regcheckbox, regradiobutton, regradiogroup, regedit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regcheckbox', @regcheckbox.Register);
  RegisterUnit('regradiobutton', @regradiobutton.Register);
  RegisterUnit('regradiogroup', @regradiogroup.Register);
  RegisterUnit('regedit', @regedit.Register);
end;

initialization
  RegisterPackage('registrycontrols', @Register);
end.
