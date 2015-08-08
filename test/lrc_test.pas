unit lrc_test;

{$mode Delphi}{$H+}

interface

uses
  testregistry,
  registrysource_test,
  regedit_test,
  regcheckbox_test,
  regradiobutton_test,
  reglabel_test,
  issue00030_test,
  issue00035_test,
  lrcreginifile_test,
  reglistbox_test;

implementation

initialization
  { TODO 1 -oAlfred Gerke -cGUI : Formular zum Dynamischen erzeugen von RegistryControls.
(s. testpreferredsize.pas in C:\Users\Alfred\Programme\lazarus\test\lcltests) }

  // Alle Tests für TLRCRegInifile
  //!<--
  RegisterTest('Voraussetzungen', TLRCRegInifileTest);
  //-->

  // Alle Tests für das Package: RegistrySource
  //!<--
  RegisterTest('Package: RegistrySource', TRegistrySourceTest);
  //-->

  // Alle Tests für das Package: RegistryControls
  //!<--
  RegisterTest('Package: RegistryControls', TRegEditTest);
  RegisterTest('Package: RegistryControls', TRegCheckBoxTest);
  RegisterTest('Package: RegistryControls', TRegRadioButtonTest);
  RegisterTest('Package: RegistryControls', TRegLabelTest);
  RegisterTest('Package: RegistryControls', TRegListBoxTest);
  //-->

  // Alle Tests für die Issues
  // (s. GitHub: https://github.com/AlfredGerke/LazarusRegistryControls/issues)
  //!<--
  RegisterTest('LRC 09 M2.Issue #30 - MergeData umbenennen', TDoMergeDataTest);
  RegisterIssue00035Tests('LRC 09 M2.Issue #35 - UTF8-Bug');
  //-->
end.

