unit issue00035_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit;

type

  { TCheckRTLAnsiTest }

  TCheckRTLAnsiTest= class(TTestCase)
  private
    procedure SetRegistryEntries;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckSettings;
  end;

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35');

implementation

uses
  Registry,
  registrysource_test,
  regedit_test,
  regcheckbox_test,
  regradiobutton_test,
  testregistry;

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35');
begin
  RegisterTest(aSuitePath, TCheckRTLAnsiTest);
  RegisterTest(aSuitePath, TRegistrySourceUTF8Test);
  RegisterTest(aSuitePath, TRegEditUTF8Test);
  RegisterTest(aSuitePath, TRegCheckBoxUTF8Test);
  RegisterTest(aSuitePath, TRegRadioButtonUTF8Test);
end;

procedure TCheckRTLAnsiTest.SetRegistryEntries;
var
  {%H-}ini: TRegIniFile;
begin
  ini := TRegIniFile.Create('');
end;

procedure TCheckRTLAnsiTest.SetUp;
begin
  SetRegistryEntries;
end;

procedure TCheckRTLAnsiTest.TearDown;
begin
  //
end;

procedure TCheckRTLAnsiTest.CheckSettings;
begin
  //
end;

end.

