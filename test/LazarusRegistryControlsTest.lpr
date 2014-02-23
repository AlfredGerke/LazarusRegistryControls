program LazarusRegistryControlsTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, registrysource_test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

