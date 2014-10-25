unit regedit_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regedit_wrapper,
  regtype,
  lrc_testcase,
  Registry,
  regbasics;

type

  { TRegEditGenericTest }

  TRegEditGenericTest<_T1,_T2>= class(TLRCTestCase<String>)
  private
    FRegSrcWrapper: _T1;
    FRegEditWrapper: _T2;

    procedure ReadRegistryCase3(aIni: TLRCRegIniFile;
                                aSection: string;
                                aIdent: string;
                                aDefault: string);
    procedure ReadRegistryCase2(aIni: TLRCRegIniFile;
                                aSection: string;
                                aIdent: string;
                                aDefault: string);
    procedure ReadRegistryCase1(aIni: TLRCRegIniFile;
                                aSection: string;
                                aIdent: string;
                                aDefault: string);
    procedure WriteRegistryCase1(aIni: TLRCRegIniFile;
                                 aSection: string;
                                 aIdent: string;
                                 aDefault: string);
    procedure WriteRegistryCase2(aIni: TLRCRegIniFile;
                                 aSection: string;
                                 aIdent: string;
                                 aDefault: string);
    procedure WriteRegistryCase3(aIni: TLRCRegIniFile;
                                 aSection: string;
                                 aIdent: string;
                                 aDefault: string);
    procedure WriteRegistryProc(aIni: TLRCRegIniFile);
    procedure ReadRegistryProc(aIni: TLRCRegIniFile);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure PublishedProperties;
    procedure ReadRegistry;
    procedure WriteRegistry;
  end;

  { TRegEditTest }

  TRegEditTest= class(TRegEditGenericTest<TRegistrySourceWrapper, TRegEditWrapper>)
    procedure SetSectionsAndIdents; override;
  end;

  { TRegEditUTF8Test }

  TRegEditUTF8Test= class(TRegEditGenericTest<TRegistrySourceWrapperUTF8, TRegEditWrapperUTF8>)
    procedure SetSectionsAndIdents; override;
  end;

implementation

uses
  test_const,
  test_utils,
  regconvutils;

{ TRegEditGenericTest }

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistryCase3(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 3. Fall CanRead = True (Ident nicht vorhanden)
    FRegEditWrapper.RegControl.RegistrySettings.CanRead := True;
    // Verhindert das das Setzen der Text-Eigenschaft sofort Änderungen in der
    // Registry vornimmt
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := False;

    FRegSrcWrapper.RegistrySource.DeleteKey(Section, Ident);

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertFalse('3. Fall CanRead = True (Ident nicht vorhanden): Test nicht '
      + 'durchführbar, Ident darf nicht vorhanden sein', ident_exists);

    regvalue := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue, DEFAULT_TEXT_ENTRY) = 0);

    AssertTrue(Format('3. Fall CanRead = True (Ident nicht vorhanden): Test nicht '
      + 'durchführbar, Value muss %s sein', [DEFAULT_TEXT_ENTRY]), checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.ReadFromReg(True, 'TRegEdit');

    // Wenn CanRead = True und kein Ident vorhanden ist, dann muss der Default-Wert
    // in der Text-Eigenschaft vorhanden sein
    AssertEquals('3. Fall CanRead = True (Ident nicht vorhanden): TRegEdit.Text:',
      aDefault, FRegEditWrapper.RegControl.Text);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistryCase2(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 2. Fall CanRead = False
    FRegEditWrapper.RegControl.RegistrySettings.CanRead := False;
    // Verhindert das das Setzen der Text-Eigenschaft sofort Änderungen in der
    // Registry vornimmt
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := False;

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertTrue('2. Fall CanRead = False: Test nicht durchführbar, Ident ist nicht '
      + 'vorhanden', ident_exists);

    regvalue := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue, DEFAULT_TEXT_ENTRY) <> 0);

    AssertTrue(Format('2. Fall CanRead = False: Test nicht durchführbar, Value '
      + 'darf nicht %s sein', [DEFAULT_TEXT_ENTRY]), checked);

    checked := (CompareText(regvalue, _TEXT_ENTRY) = 0);

    AssertTrue(Format('2. Fall CanRead = False: Test nicht durchführbar, Value '
      + 'muss %s sein', [_TEXT_ENTRY]), checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.ReadFromReg(True, 'TRegEdit');

    // Wenn CanRead = False dann darf der Wert aus der Registry nicht in der
    // Text-Eigenschaft vorhanden sein, die Text-Eigenschaft darf sich nicht
    // geändert haben
    AssertEquals('2. Fall CanRead = False: TRegEdit.Text:', _TEST_STRING,
      FRegEditWrapper.RegControl.Text);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistryCase1(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 1. Fall CanRead = True
    FRegEditWrapper.RegControl.RegistrySettings.CanRead := True;
    // Verhindert das das Setzen der Text-Eigenschaft sofort Änderungen in der
    // Registry vornimmt
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := False;

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertTrue('1. Fall CanRead = True: Test nicht durchführbar, Ident ist nicht '
      + 'vorhanden', ident_exists);

    regvalue := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue, DEFAULT_TEXT_ENTRY) <> 0);

    AssertTrue(Format('1. Fall CanRead = True: Test nicht durchführbar, Value '
      + 'darf nicht %s sein', [DEFAULT_TEXT_ENTRY]), checked);

    checked := (CompareText(regvalue, _TEXT_ENTRY) = 0);

    AssertTrue(Format('1. Fall CanRead = True: Test nicht durchführbar, Value '
      + 'muss %s sein', [_TEXT_ENTRY]), checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.ReadFromReg(True, 'TRegEdit');

    // Wenn CanRead = True dann muss der Wert aus der Registry in der
    // Text-Eigenschaft vorhanden sein
    AssertEquals('1. Fall CanRead = True: TRegEdit.Text:', regvalue,
      FRegEditWrapper.RegControl.Text);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistryCase3(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue_before: string;
  regvalue_post: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 3. Fall CanWrite = True (Ident nicht vorhanden)
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := True;
    FRegSrcWrapper.RegistrySource.DeleteKey(Section, Ident);

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertFalse('3. Fall CanWrite = True (Ident nicht vorhanden): Test nicht '
      + 'durchführbar, Ident darf nicht vorhanden sein', ident_exists);

    regvalue_before := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue_before, DEFAULT_TEXT_ENTRY) = 0);

    AssertTrue(Format('3. Fall CanWrite = True (Ident nicht vorhanden): Test '
      + 'nicht durchführbar, Value muss: %s sein', [DEFAULT_TEXT_ENTRY]),
      checked);

    checked := (CompareText(regvalue_before, _TEST_STRING) <> 0);

    AssertTrue(Format('3. Fall CanWrite = True (Ident nicht vorhanden): Test '
      + 'nicht durchführbar, Value darf nicht: %s sein', [_TEST_STRING]),
      checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.RegControl.TriggerChange;

    regvalue_post := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    AssertTrue('3. Fall CanWrite = True (Ident nicht vorhanden): Keine Änderungen '
      + 'in der Registry vorgenommen', (regvalue_before <> regvalue_post));

    // Wenn CanWrite = True muss nach dem Setzen der Text-Eigenschaft in der
    // Registry eine neuer Ident mit dem Wert angelegt worden sein
    AssertEquals('3. Fall CanWrite = True (Ident nicht vorhanden): TRegEdit.Text:',
      _TEST_STRING, regvalue_post);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistryCase2(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue_before: string;
  regvalue_post: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 2. Fall CanWrite = False
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := False;

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertTrue('2. Fall CanWrite = False: Test nicht durchführbar, Ident ist '
      + 'nicht vorhanden', ident_exists);

    regvalue_before := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue_before, _TEXT_ENTRY) = 0);

    AssertTrue(Format('2. Fall CanWrite = False: Test nicht durchführbar, Value '
      + 'muss: %s sein', [_TEXT_ENTRY]), checked);

    checked := (CompareText(regvalue_before, _TEST_STRING) <> 0);

    AssertTrue(Format('2. Fall CanWrite = False: Test nicht durchführbar, Value '
      + 'darf nicht: %s sein', [_TEST_STRING]), checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.RegControl.TriggerChange;

    regvalue_post := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    AssertTrue('2. Fall CanWrite = False: Änderungen in der Registry vorgenommen',
      (regvalue_before = regvalue_post));

    // Wenn CanWrite = False darf nach dem Setzen der Text-Eigenschaft der Wert
    // in der Registry trotzdem nicht verändert worden sein
    AssertEquals('2. Fall CanWrite = False: TRegEdit.Text:', _TEXT_ENTRY,
      regvalue_post);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistryCase1(aIni: TLRCRegIniFile;
  aSection: string;
  aIdent: string;
  aDefault: string);
var
  regvalue_before: string;
  regvalue_post: string;
  ident_exists: boolean;
  checked: boolean;
begin
  with aIni do
  begin
    // 1. Fall CanWrite = True
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := True;

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(Section, Ident);

    AssertTrue('1. Fall CanWrite = True: Test nicht durchführbar, Ident ist nicht '
      + 'vorhanden', ident_exists);

    regvalue_before := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    checked := (CompareText(regvalue_before, _TEXT_ENTRY) = 0);

    AssertTrue(Format('1. Fall CanWrite = True: Test nicht durchführbar, Value '
      + 'muss: %s sein', [_TEXT_ENTRY]), checked);

    checked := (CompareText(regvalue_before, _TEST_STRING) <> 0);

    AssertTrue(Format('1. Fall CanWrite = True: Test nicht durchführbar, Value '
      + 'darf nicht: %s sein', [_TEST_STRING]), checked);

    FRegEditWrapper.RegControl.Text := _TEST_STRING;
    FRegEditWrapper.RegControl.TriggerChange;

    regvalue_post := ReadString(aSection, aIdent, DEFAULT_TEXT_ENTRY);

    AssertTrue('1. Fall CanWrite = True: Keine Änderungen in der Registry '
      + 'vorgenommen', (regvalue_before <> regvalue_post));

    // Wenn CanWrite = True muss nach dem Setzen der Text-Eigenschaft der Wert
    // der Text-Eigenschaft auch in der Registry stehen
    AssertEquals('1. Fall CanWrite = True: TRegEdit.Text:', _TEST_STRING,
      regvalue_post);
  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistryProc(aIni: TLRCRegIniFile);
var
  test_section: string;
  test_ident: string;
  test_default: string;
begin
  test_section := GetSectionUTF8Decoded;
  test_ident := GetIdentUTF8Decoded;
  test_default := UTF8DecodeIfNeeded(Default, CheckRTLNeeded);

  WriteRegistryCase1(aIni, test_section, test_ident, test_default);
  FRegEditWrapper.SetRegistryEntries;
  WriteRegistryCase2(aIni, test_section, test_ident, test_default);
  FRegEditWrapper.SetRegistryEntries;
  WriteRegistryCase3(aIni, test_section, test_ident, test_default);
end;

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistryProc(aIni: TLRCRegIniFile);
var
  test_section: string;
  test_ident: string;
  test_default: string;
begin
  test_section := GetSectionUTF8Decoded;
  test_ident := GetIdentUTF8Decoded;
  test_default := UTF8DecodeIfNeeded(Default, CheckRTLNeeded);

  ReadRegistryCase1(aIni, test_section, test_ident, test_default);
  FRegEditWrapper.SetRegistryEntries;
  ReadRegistryCase2(aIni, test_section, test_ident, test_default);
  FRegEditWrapper.SetRegistryEntries;
  ReadRegistryCase3(aIni, test_section, test_ident, test_default);
end;

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegEditWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadRegistryProc);
end;

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegEditWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteRegistryProc);
end;

procedure TRegEditGenericTest<_T1,_T2>.RootKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  root_keys_struct.Clear;

  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegEditWrapper.RootKeys('TRegEdit',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegEditGenericTest<_T1,_T2>.PublishedProperties;
begin
  FRegEditWrapper.PublishedProperties('TRegEdit');
end;

procedure TRegEditGenericTest<_T1,_T2>.SetUp;
begin
  inherited SetUp;

  FRegSrcWrapper := _T1.Create;
  FRegEditWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegEditGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegEditWrapper);
  FreeAndNil(FRegSrcWrapper);
end;

{ TRegEditUTF8Test }

procedure TRegEditUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGEDIT;
  Ident := IDENT_TEXT_PROPERTY;
  Default := DEFAULT_TEXT_ENTRY;

  CheckRTLNeeded := True;
end;

{ TRegEditTest }

procedure TRegEditTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGEDIT;
  Ident := IDENT_TEXT_PROPERTY;
  Default := DEFAULT_TEXT_ENTRY;

  CheckRTLNeeded := True;
end;

end.

