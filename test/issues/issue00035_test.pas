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
    function GetRookKey: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckSettings;
  end;

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35 - UTF8-Bug');

implementation

uses
  registrysource_test,
  regedit_test,
  regcheckbox_test,
  regradiobutton_test,
  reglabel_test,
  testregistry,
  regbasics,
  reglistbox_test;

procedure RegisterIssue00035Tests(aSuitePath: string = 'LRC 09 M2.Issue #35 - UTF8-Bug');
begin
  RegisterTest(aSuitePath, TCheckRTLAnsiTest);
  RegisterTest(aSuitePath, TRegistrySourceUTF8Test);
  RegisterTest(aSuitePath, TRegEditUTF8Test);
  RegisterTest(aSuitePath, TRegCheckBoxUTF8Test);
  RegisterTest(aSuitePath, TRegRadioButtonUTF8Test);
  RegisterTest(aSuitePath, TRegLabelUTF8Test);
  RegisterTest(aSuitePath, TRegListBoxUTF8Test);
end;

function TCheckRTLAnsiTest.GetRookKey: string;
begin
  Result := UTF8Decode(
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-'
    + 'AE9B-1548DC65F930}');
end;

procedure TCheckRTLAnsiTest.SetUp;
begin
  //
end;

procedure TCheckRTLAnsiTest.TearDown;
begin
  //
end;

procedure TCheckRTLAnsiTest.CheckSettings;
var
  {%H-}ini: TLRCRegIniFile;
  root_key: string;
  check_string_1: string;
  check_string_2: string;
  check_string_3: string;
  check_integer_1: integer;
  check_integer_2: integer;
  check_bool_1: boolean;
  check_bool_2: boolean;
begin
  root_key := GetRookKey;

  ini := TLRCRegIniFile.Create(root_key);
  try
    with ini do
    begin
      check_string_1 :=
        ReadString('ReadSection_mit_ßÜÖÄüöä', 'String_Ident_mit_ßÜÖÄüöä',
        'String_Value_mit_ßÜÖÄüöä');

      check_integer_1 :=
        ReadInteger('ReadSection_mit_ßÜÖÄüöä', 'Integer_Ident_mit_ßÜÖÄüöä',
        12345);

      check_bool_1 :=
        ReadBool('ReadSection_mit_ßÜÖÄüöä', 'Boolean_Ident_mit_ßÜÖÄüöä',
        True);

      check_string_2 :=
        ReadString('WriteSection_mit_ßÜÖÄüöä', 'String_Ident_mit_ßÜÖÄüöä',
        'String_Value_mit_ßÜÖÄüöä');

      check_integer_2 :=
        ReadInteger('WriteSection_mit_ßÜÖÄüöä', 'Integer_Ident_mit_ßÜÖÄüöä',
        12345);

      check_bool_2 :=
        ReadBool('WriteSection_mit_ßÜÖÄüöä', 'Boolean_Ident_mit_ßÜÖÄüöä',
        True);

      check_string_3 :=
        ReadString('RenameSection_mit_ßÜÖÄüöä', 'String_Ident_mit_ßÜÖÄüöä',
        'String_Value_mit_ßÜÖÄüöä');
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;

  AssertEquals('Testwert für String lesen', 'String_Value_mit_ßÜÖÄüöä',
    check_string_1);
  AssertEquals('Testwert für Integer lesen', 12345, check_integer_1);
  AssertEquals('Testwert für Boolean lesen', True, check_bool_1);
  AssertEquals('Testwert für String schreiben', 'String_Value_mit_ßÜÖÄüöä',
    check_string_2);
  AssertEquals('Testwert für Integer schreiben', 12345, check_integer_2);
  AssertEquals('Testwert für Boolean schreiben', True, check_bool_2);
  AssertEquals('Testwert für Key umbenennen', 'String_Value_mit_ßÜÖÄüöä',
    check_string_3);
end;

end.

