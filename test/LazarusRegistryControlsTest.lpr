program LazarusRegistryControlsTest;

{$mode Delphi}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  registrysource_test,
  regedit_test,
  registrysource_wrapper,
  regedit_wrapper,
  test_wrapper,
  regcheckbox_wrapper,
  regcheckbox_test,
  regradiobutton_wrapper,
  regradiobutton_test,
  issue00030_test,
  reglistbox_wrapper,
  regchecklistbox_wrapper,
  regcombobox_wrapper,
  regradiogroup_wrapper,
  regcheckgroup_wrapper,
  regvaluelisteditor_wrapper,
  issue00035_test,
  reglabel_test,
  reglabel_wrapper,
  lrc_testcase,
  lrcreginifile_test,
  reglistbox_test,
  register_lrc_test,
  test_const,
  test_utils,
  issue00051_test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

