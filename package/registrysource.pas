{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RegistrySource;

{$warn 5023 off : no warning about unused units}
interface

uses
  regsourcen, regmsg, regconst, regtype, regpropedits, dlgTrueFalse, 
  dlgeditsettings, dlgaboutcomponent, regresstrings, regutils, regbaseform, 
  dlgguid, regbasics, regconvutils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regsourcen', @regsourcen.Register);
end;

initialization
  RegisterPackage('RegistrySource', @Register);
end.
