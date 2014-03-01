unit lrc_test;

{$mode Delphi}{$H+}

interface

uses
  testregistry,
  registrysource_test,
  regedit_test,
  regcheckbox_test,
  regradiobutton_test;

implementation

initialization
  { TODO 1 -oAlfred Gerke -cGUI : Formular zum Dynamischen erzeugen von RegistryControls.
(s. testpreferredsize.pas in C:\Users\Alfred\Programme\lazarus\test\lcltests) }
  RegisterTest('Package: RegistrySource', TRegistrySourceTest);
  RegisterTest('Package: RegistryControls', TRegEditTest);
  RegisterTest('Package: RegistryControls', TRegCheckBoxTest);
  RegisterTest('Package: RegistryControls', TRegRadioButtonTest);
end.

