unit regradiobutton_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  FPCUnit,
  registrysource_wrapper,
  regradiobutton_wrapper,
  regtype,
  Registry,
  lrc_testcase;

type

  { TRegRadioButtonGenericTest }

  TRegRadioButtonGenericTest<_T1,_T2>= class(TLRCTestCase<Boolean>)
  private
    FRegSrcWrapper: _T1;
    FRegRadioButtonWrapper: _T2;

    procedure WriteRegistryProc(aIni: TRegIniFile);
    procedure ReadRegistryProc(aIni: TRegIniFile);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure PublishedProperties;
    procedure ReadByCaptionSettings;
    procedure ReadRegistry;
    procedure WriteRegistry;
  end;

  { TRegRadioButtonTest }

  TRegRadioButtonTest= class(TRegRadioButtonGenericTest<TRegistrySourceWrapper,TRegRaidoButtonWrapper>)
    procedure SetSectionsAndIdents; override;
  end;

  { TRegRadioButtonUTF8Test }

  TRegRadioButtonUTF8Test= class(TRegRadioButtonGenericTest<TRegistrySourceWrapperUTF8,TRegRaidoButtonWrapperUTF8>)
    procedure SetSectionsAndIdents; override;
  end;

implementation

uses
  test_const,
  test_utils,
  regconvutils;

procedure TRegRadioButtonGenericTest<_T1,_T2>.ReadRegistryProc(
  aIni: TRegIniFile);
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
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := True;

    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('1. Fall CanRead = True - TRegRadioButton.ReadReg: Test nicht durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := test_default;
    FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

    AssertEquals('1. Fall CanRead = True - TRegRadioButton.Checked', value_by_regini,
      FRegRadioButtonWrapper.RegControl.Checked);

    // 2. Fall CanRead = False
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := False;

    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('2. Fall CanRead = False - TRegRadioButton.ReadReg: Test nicht durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := test_default;
    FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

    AssertEquals('2. Fall CanRead = False - TRegRadioButton.Checked', test_default,
      FRegRadioButtonWrapper.RegControl.Checked);
  end;
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.WriteRegistryProc(
  aIni: TRegIniFile);
var
  test_ident: string;
  test_section: string;
  test_default: boolean;

  value_by_regini_before: boolean;
  value_by_regini_post: boolean;
begin
  { TODO 10 -oAlfred Gerke -cTest : Komplett überarbeiten; vorhandener Code ist NUR ein Beispiel }
  test_default := Default;
  test_section:= GetSectionUTF8Decoded;
  test_ident := GetIdentUTF8Decoded;

  with aIni do
  begin
    // 1. Fall CanWrite = True
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanWrite := True;

    value_by_regini_before :=
      ReadBool(test_section, test_ident, test_default);

    // Prüfen ob der Test-Ident vorhanden und ungleich dem Default
    // Nur wenn der gelesene Wert aus der Registry nicht mit dem Default übereinstimmt
    // kann man sicher sein, das der Wert tatsächlich in der Registry vorhanden ist
    AssertTrue('1. Fall CanWrite = True - TRegRadioButton.WriteReg: Test nicht durchführbar, TestIdent und Default unterscheiden sich nicht',
      (value_by_regini_before <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := not value_by_regini_before;
    FRegRadioButtonWrapper.RegControl.TriggerClick;

    value_by_regini_post :=
      ReadBool(test_section, test_ident, value_by_regini_before);

    AssertTrue('1. Fall CanWrite = True - TRegRadioButton.WriteReg: Test nicht eindeutig, Wert-Vorher und Wert-Nachher sind identisch',
      (value_by_regini_before <> value_by_regini_post));

    AssertEquals('1. Fall CanWrite = True - TRegRadioButton.Checked', value_by_regini_post,
      FRegRadioButtonWrapper.RegControl.Checked);

    // 2. Fall CanWrite = False
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanWrite := False;
    // Testwerte wieder einrichten
    FRegRadioButtonWrapper.SetRegistryEntries;

    value_by_regini_before :=
      ReadBool(test_section, test_ident, test_default);

    // Prüfen ob der Test-Ident vorhanden und ungleich dem Default
    // Nur wenn der gelesene Wert aus der Registry nicht mit dem Default übereinstimmt
    // kann man sicher sein, das der Wert tatsächlich in der Registry vorhanden ist
    AssertTrue('2. Fall CanWrite = False = True - TRegRadioButton.WriteReg: Test nicht durchführbar, TestIdent und Default unterscheiden sich nicht',
      (value_by_regini_before <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := not value_by_regini_before;
    FRegRadioButtonWrapper.RegControl.TriggerClick;

    value_by_regini_post :=
      ReadBool(test_section, test_ident, value_by_regini_before);

    AssertTrue('2. Fall CanWrite = False - TRegRadioButton.WriteReg: Test nicht eindeutig, Wert-Vorher und Wert-Nachher müssen identisch sein',
      (value_by_regini_before = value_by_regini_post));

    AssertEquals('2. Fall CanWrite = False - TRegRadioButton.Checked', not value_by_regini_before,
      FRegRadioButtonWrapper.RegControl.Checked);
  end;
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.RootKeys;
var
  {%H-}check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  {%H-}root_keys_struct.Clear;

  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegRadioButtonWrapper.RootKeys('TRegRadioButton',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.PublishedProperties;
begin
  FRegRadioButtonWrapper.PublishedProperties('TRegRadioButton');
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.ReadByCaptionSettings;
var
  {%H-}caption_by_default: string;
  {%H-}caption_by_registry: string;
begin
  caption_by_registry := _TREGCHECKBOX_CAPTION_VALUE;
  caption_by_default := DEFAULT_CAPTION_VALUE;

  FRegRadioButtonWrapper.ReadCaption(caption_by_default, caption_by_registry,
    'Caption');
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegRadioButtonWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadRegistryProc);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.WriteRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegRadioButtonWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteRegistryProc);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.SetUp;
begin
  inherited SetUp;

  FRegSrcWrapper := _T1.Create;
  FRegRadioButtonWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegRadioButtonWrapper);
  FreeAndNil(FRegSrcWrapper);
end;

{ TRegRadioButtonUTF8Test }

procedure TRegRadioButtonUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGRADIOBUTTON;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CheckRTLNeeded := True;
end;

{ TRegRadioButtonTest }

procedure TRegRadioButtonTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGRADIOBUTTON;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CheckRTLNeeded := True;
end;

end.

