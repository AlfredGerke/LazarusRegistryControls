unit regcheckbox_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regcheckbox_wrapper,
  regtype,
  test_const,
  lrc_testcase,
  Registry;

type

  { TRegCheckBoxGenericTest }

  TRegCheckBoxGenericTest<_T1,_T2>= class(TLRCTestCase<Boolean>)
  private
    FRegSrcWrapper: _T1;
    FRegCheckBoxWrapper: _T2;

    procedure WriteRegistryProc(aIni: TRegIniFile);
    procedure ReadRegistryProc(aIni: TRegIniFile);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RookKeys;
    procedure PublishedProperties;
    procedure ReadByCaptionSettings;
    procedure ReadRegistry;
    procedure WriteRegistry;
  end;

  { TRegCheckBoxTest }

  TRegCheckBoxTest= class(TRegCheckBoxGenericTest<TRegistrySourceWrapper,TRegCheckBoxWrapper>)
    procedure SetSectionsAndIdents; override;
  end;

  { TRegCheckBoxUTF8Test }

  TRegCheckBoxUTF8Test= class(TRegCheckBoxGenericTest<TRegistrySourceWrapperUTF8,TRegCheckBoxWrapperUTF8>)
    procedure SetSectionsAndIdents; override;
  end;

implementation

uses
  test_utils;


procedure TRegCheckBoxGenericTest<_T1,_T2>.WriteRegistryProc(aIni: TRegIniFile);
var
  test_ident: string;
  test_section: string;
  test_default: boolean;

  value_by_regini_before: boolean;
  value_by_regini_post: boolean;
begin
  test_default := Default;
  test_section:= GetSectionUTF8Decoded;
  test_ident := GetIdentUTF8Decoded;

  with aIni do
  begin
    // 1. Fall CanWrite = True
    FRegCheckBoxWrapper.RegControl.RegistrySettings.CanWrite := True;

    value_by_regini_before :=
      ReadBool(test_section, test_ident, test_default);

    // Prüfen ob der Test-Ident vorhanden und ungleich dem Default
    // Nur wenn der gelesene Wert aus der Registry nicht mit dem Default
    // übereinstimmt kann man sicher sein, das der Wert tatsächlich in der
    // Registry vorhanden ist
    AssertTrue('1. Fall CanWrite = True - TRegCheckBox.WriteReg: Test nicht '
      + 'durchführbar, TestIdent und Default unterscheiden sich nicht',
      (value_by_regini_before <> test_default));

    FRegCheckBoxWrapper.RegControl.Checked := not value_by_regini_before;
    FRegCheckBoxWrapper.RegControl.TriggerClick;

    value_by_regini_post :=
      ReadBool(test_section, test_ident, value_by_regini_before);

    AssertTrue('1. Fall CanWrite = True - TRegCheckBox.WriteReg: Test nicht '
      + 'eindeutig, Wert-Vorher und Wert-Nachher sind identisch',
      (value_by_regini_before <> value_by_regini_post));

    AssertEquals('1. Fall CanWrite = True - TRegCheckBox.Checked',
      value_by_regini_post, FRegCheckBoxWrapper.RegControl.Checked);

    // 2. Fall CanWrite = False
    FRegCheckBoxWrapper.RegControl.RegistrySettings.CanWrite := False;
    // Testwerte wieder einrichten
    FRegCheckBoxWrapper.SetRegistryEntries;

    value_by_regini_before :=
      ReadBool(test_section, test_ident, test_default);

    // Prüfen ob der Test-Ident vorhanden und ungleich dem Default
    // Nur wenn der gelesene Wert aus der Registry nicht mit dem Default übereinstimmt
    // kann man sicher sein, das der Wert tatsächlich in der Registry vorhanden ist
    AssertTrue('2. Fall CanWrite = False = True - TRegCheckBox.WriteReg: Test '
      + 'nicht durchführbar, TestIdent und Default unterscheiden sich nicht',
      (value_by_regini_before <> test_default));

    FRegCheckBoxWrapper.RegControl.Checked := not value_by_regini_before;
    FRegCheckBoxWrapper.RegControl.TriggerClick;

    value_by_regini_post :=
      ReadBool(test_section, test_ident, value_by_regini_before);

    AssertTrue('2. Fall CanWrite = False - TRegCheckBox.WriteReg: Test nicht '
      + 'eindeutig, Wert-Vorher und Wert-Nachher müssen identisch sein',
      (value_by_regini_before = value_by_regini_post));

    AssertEquals('2. Fall CanWrite = False - TRegCheckBox.Checked',
      not value_by_regini_before, FRegCheckBoxWrapper.RegControl.Checked);
  end;
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.ReadRegistryProc(aIni: TRegIniFile);
var
  test_ident: string;
  test_section: string;
  test_default: boolean;

  value_by_regini: boolean;
begin
  test_default := Default;
  test_section:= GetSectionUTF8Decoded;
  test_ident := GetIdentUTF8Decoded;

  with aIni do
  begin
    // 1. Fall CanRead = True
    FRegCheckBoxWrapper.RegControl.RegistrySettings.CanRead := True;

    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('1. Fall CanRead = True - TRegCheckBox.ReadReg: Test nicht '
      + 'durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegCheckBoxWrapper.RegControl.Checked := test_default;
    FRegCheckBoxWrapper.ReadFromReg(True, rdoGeneral, 'TRegCheckBox');

    AssertEquals('1. Fall CanRead = True - TRegCheckBox.Checked', value_by_regini,
      FRegCheckBoxWrapper.RegControl.Checked);

    // 2. Fall CanRead = False
    FRegCheckBoxWrapper.RegControl.RegistrySettings.CanRead := False;

    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('2. Fall CanRead = False - TRegCheckBox.ReadReg: Test nicht '
      + 'durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegCheckBoxWrapper.RegControl.Checked := test_default;
    FRegCheckBoxWrapper.ReadFromReg(True, rdoGeneral, 'TRegCheckBox');

    AssertEquals('2. Fall CanRead = False - TRegCheckBox.Checked', test_default,
      FRegCheckBoxWrapper.RegControl.Checked);
  end;
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.RookKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  root_keys_struct.Clear;

  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegCheckBoxWrapper.RootKeys('TRegCheckBox',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.PublishedProperties;
begin
  FRegCheckBoxWrapper.PublishedProperties('TRegCheckBox');
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.ReadByCaptionSettings;
var
  caption_by_default: string;
  caption_by_registry: string;
begin
  caption_by_registry := _TREGCHECKBOX_CAPTION_VALUE;
  caption_by_default := DEFAULT_CAPTION_VALUE;

  FRegCheckBoxWrapper.ReadCaption(caption_by_default, caption_by_registry,
    'Caption');
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.SetUp;
begin
  inherited SetUp;

  FRegSrcWrapper := _T1.Create;
  FRegCheckBoxWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegCheckBoxWrapper);
  FreeAndNil(FRegSrcWrapper);
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegCheckBoxWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadRegistryProc);
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.WriteRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegCheckBoxWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteRegistryProc);
end;

{ TRegCheckBoxTest }

procedure TRegCheckBoxTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGCHECKBOX;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CheckRTLNeeded := True;
end;

{ TRegCheckBoxUTF8Test }

procedure TRegCheckBoxUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGCHECKBOX;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CheckRTLNeeded := True;
end;

end.

