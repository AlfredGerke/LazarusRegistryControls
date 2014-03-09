unit regcheckbox_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regcheckbox_wrapper,
  regtype,
  test_const;

type

  { TRegCheckBoxGenericTest }

  TRegCheckBoxGenericTest<_T1,_T2>= class(TTestCase)
  private
    FRegSrcWrapper: _T1;
    FRegCheckBoxWrapper: _T2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RookKeys;
    procedure PublishedProperties;
    procedure ReadByCaptionSettings;
  end;

  { TRegCheckBoxTest }

  TRegCheckBoxTest= class(TRegCheckBoxGenericTest<TRegistrySourceWrapper,TRegCheckBoxWrapper>)
  end;

  { TRegCheckBoxUTF8Test }

  TRegCheckBoxUTF8Test= class(TRegCheckBoxGenericTest<TRegistrySourceWrapperUTF8,TRegCheckBoxWrapperUTF8>)
  end;

implementation

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

end.

