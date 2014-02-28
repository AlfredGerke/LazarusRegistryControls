unit lrc_test;

{$mode Delphi}{$H+}

interface

uses
  testregistry,
  registrysource_test,
  regedit_test,
  regcheckbox_test;

implementation

initialization

  RegisterTest('Package: RegistrySource', TRegistrySourceTest);
  RegisterTest('Package: RegistryControls', TRegEditTest);
  RegisterTest('Package: RegistryControls', TRegCheckBoxTest);
end.

