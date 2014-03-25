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

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35 - UTF8-Bug');

implementation

uses
  Registry,
  registrysource_test,
  regedit_test,
  regcheckbox_test,
  regradiobutton_test,
  testregistry;

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35 - UTF8-Bug');
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
  root_key: string;
begin
  root_key :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-'
    + 'AE9B-1548DC65F930}';
  ini := TRegIniFile.Create(root_key);
  try
    with ini do
    begin

    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;
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

