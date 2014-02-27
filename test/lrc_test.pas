unit lrc_test;

{$mode Delphi}{$H+}

interface

uses
  testregistry,
  registrysource_test,
  regedit_test;

implementation

initialization

  RegisterTest('Package: RegistrySource', TRegistrySourceTest);
  RegisterTest('Package: RegistryControls', TRegEditTest);
end.

