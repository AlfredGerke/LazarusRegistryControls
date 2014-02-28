program LazarusRegistryControlsTest;

{$mode objfpc}{$H+}

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
  regcheckbox_wrapper, regcheckbox_test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

