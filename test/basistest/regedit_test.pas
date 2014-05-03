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
  Registry;

type

  { TRegEditGenericTest }

  TRegEditGenericTest<_T1,_T2>= class(TLRCTestCase<String>)
  private
    FRegSrcWrapper: _T1;
    FRegEditWrapper: _T2;

    procedure WriteRegistryProc(aIni: TRegIniFile);
    procedure ReadRegistryProc(aIni: TRegIniFile);
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
  test_utils;

{ TRegEditGenericTest }

procedure TRegEditGenericTest<_T1,_T2>.WriteRegistryProc(aIni: TRegIniFile);
var
  test_section: string;
  test_ident: string;
  test_default: string;

  regvalue_by_ini: string;
  ident_exists: boolean;
begin
  test_section := Section;
  test_ident := Ident;
  test_default := Default;

  with aIni do
  begin
    // 1. Fall CanWrite = True
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := True;

    ident_exists :=
      FRegSrcWrapper.RegistrySource.IdentExists(test_section, test_ident);

    AssertTrue('1. Fall CanWrite = True: Test nicht durchführbar, Ident ist nicht vorhanden',
      ident_exists);

    regvalue_by_ini := ReadString(test_section, test_ident, DEFAULT_TEXT_ENTRY);

    AssertTrue('1. Fall CanWrite = True: Test nicht durchführbar, Testidents sind identisch',
      (regvalue_by_ini <> _TEXT_ENTRY) and (regvalue_by_ini <> _TEST_STRING));

    FRegEditWrapper.RegControl.Text := _TEST_STRING;




    // 2. Fall CanWrite = False
    FRegEditWrapper.RegControl.RegistrySettings.CanWrite := False;
    FRegEditWrapper.SetRegistryEntries;

  end;
end;

procedure TRegEditGenericTest<_T1,_T2>.ReadRegistryProc(aIni: TRegIniFile);
begin



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

