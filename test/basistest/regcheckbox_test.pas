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
begin

end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.ReadRegistryProc(aIni: TRegIniFile);
begin

end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.RookKeys;
var
  {%H-}check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  {%H-}root_keys_struct.Clear;

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
  {%H-}caption_by_default: string;
  {%H-}caption_by_registry: string;
begin
  caption_by_registry := _TREGCHECKBOX_CAPTION_VALUE;
  caption_by_default := DEFAULT_CAPTION_VALUE;

  FRegCheckBoxWrapper.ReadCaption(caption_by_default, caption_by_registry,
    'Caption');
end;

procedure TRegCheckBoxGenericTest<_T1,_T2>.SetUp;
begin
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
end;

{ TRegCheckBoxUTF8Test }

procedure TRegCheckBoxUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGCHECKBOX;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;
end;

end.

