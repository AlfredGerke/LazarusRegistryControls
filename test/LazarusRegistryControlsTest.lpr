program LazarusRegistryControlsTest;

{$mode Delphi}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  registrysource_test,
  lrc_test,
  regedit_test,
  registrysource_wrapper,
  regedit_wrapper,
  test_const,
  test_wrapper,
  regcheckbox_wrapper,
  regcheckbox_test,
  regradiobutton_wrapper,
  regradiobutton_test,
  test_utils,
  issue00030_test,
  reglistbox_wrapper,
  regchecklistbox_wrapper,
  regcombobox_wrapper,
  regradiogroup_wrapper,
  regcheckgroup_wrapper,
  regvaluelisteditor_wrapper,
  issue00035_test,
  reglabel_test,
  reglabel_wrapper;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

