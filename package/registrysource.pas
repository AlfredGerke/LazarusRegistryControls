{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RegistrySource;

interface

uses
  regsourcen, regmsg, regconst, regtype, regpropedits, dlgTrueFalse, 
  dlgeditsettings, dlgaboutcomponent, regresstrings, regutils, regbaseform, 
  regconvutils, dlgguid, regbasics, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regsourcen', @regsourcen.Register);
end;

initialization
  RegisterPackage('RegistrySource', @Register);
end.
