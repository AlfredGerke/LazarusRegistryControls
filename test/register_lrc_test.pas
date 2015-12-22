unit register_lrc_test;

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
  reglistbox_test,
  issue00051_test;

implementation

{$ifdef Debug}
uses
  dbugintf;
{$endif}

initialization

  // DebugClient einrichten
  {$ifdef Debug}
  InitDebugClient;
  {$endif}

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
  RegisterIssue00030Tests('LRC 09 M2.Issue #30 - MergeData umbenennen');
  RegisterIssue00035Tests('LRC 09 M2.Issue #35 - UTF8-Bug');
  RegisterIssue00051Tests('LRC 09 M2.Issue #51 - DeleteItem einführen');
  //-->
end.

