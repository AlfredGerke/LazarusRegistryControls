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
  test_utils;

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
    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('TRegRadioButton.ReadReg: Test nicht durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := test_default;
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := True;
    FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

    AssertEquals('TRegRadioButton.Checked', value_by_regini,
      FRegRadioButtonWrapper.RegControl.Checked);

    // 2. Fall CanRead = False
    value_by_regini :=
      ReadBool(test_section, test_ident, test_default);

    AssertTrue('TRegRadioButton.ReadReg: Test nicht durchführbar, Vergleichswerte sind identisch',
      (value_by_regini <> test_default));

    FRegRadioButtonWrapper.RegControl.Checked := test_default;
    FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := False;
    FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

    AssertEquals('TRegRadioButton.Checked', test_default,
      FRegRadioButtonWrapper.RegControl.Checked);
  end;
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.WriteRegistryProc(
  aIni: TRegIniFile);
begin
    // 1. Fall: check Section, Ident, Default
  FRegRadioButtonWrapper.SectionIdentDefault;

  with aIni do
  begin

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
end;

{ TRegRadioButtonTest }

procedure TRegRadioButtonTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGRADIOBUTTON;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;
end;

end.

