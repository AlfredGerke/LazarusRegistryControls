unit regradiobutton_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  FPCUnit,
  registrysource_wrapper,
  regradiobutton_wrapper,
  regtype,
  test_const;

type

  { TRegRadioButtonGenericTest }

  TRegRadioButtonGenericTest<_T1,_T2>= class(TTestCase)
  private
    FRegSrcWrapper: _T1;
    FRegRadioButtonWrapper: _T2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure PublishedProperties;
    procedure ReadByCaptionSettings;
    procedure ReadRegistry;
  end;

  { TRegRadioButtonTest }

  TRegRadioButtonTest= class(TRegRadioButtonGenericTest<TRegistrySourceWrapper,TRegRaidoButtonWrapper>)
  end;

  { TRegRadioButtonUTF8Test }

  TRegRadioButtonUTF8Test= class(TRegRadioButtonGenericTest<TRegistrySourceWrapperUTF8,TRegRaidoButtonWrapperUTF8>)
  end;

implementation

procedure TRegRadioButtonGenericTest<_T1,_T2>.RootKeys;
var
  check_rtl_ansi: boolean;
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
  caption_by_default: string;
  caption_by_registry: string;
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

  // 2. Fall CanRead = True: _CHECKED_ENTRY muss in Checked eingetragen werden
  FRegRadioButtonWrapper.RegControl.Checked := DEFAULT_CHECKED_ENTRY;
  FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := True;
  FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

  AssertEquals('TRegRadioButton.RegistrySection.Default', _CHECKED_ENTRY,
    FRegRadioButtonWrapper.RegControl.Checked);

  // 3. Fall CanRead = False: DEFAULT_CHECKED_ENTRY muss in Checked eingetragen
  // werden
  FRegRadioButtonWrapper.RegControl.Checked := DEFAULT_CHECKED_ENTRY;
  FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := False;
  FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

  AssertEquals('TRegRadioButton.RegistrySection.Default', DEFAULT_CHECKED_ENTRY,
    FRegRadioButtonWrapper.RegControl.Checked);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.SetUp;
begin
  FRegSrcWrapper := _T1.Create;
  FRegRadioButtonWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegRadioButtonGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegRadioButtonWrapper);
  FreeAndNil(FRegSrcWrapper);
end;

end.

