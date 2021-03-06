unit reglabel_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  reglabel_wrapper,
  regtype,
  test_const,
  lrc_testcase;

type

  { TRegLabelGenericTest }

  TRegLabelGenericTest<_T1,_T2>= class(TLRCTestCase<string>)
  private
    FRegSrcWrapper: _T1;
    FRegLabelWrapper: _T2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure PublishedProperties;
    procedure ReadRegistry;
  end;

  TRegLabelTest= class(TRegLabelGenericTest<TRegistrySourceWrapper,TRegLabelWrapper>)
    procedure SetSectionsAndIdents; override;
  end;

  TRegLabelUTF8Test= class(TRegLabelGenericTest<TRegistrySourceWrapperUTF8,TRegLabelWrapperUTF8>)
    procedure SetSectionsAndIdents; override;
  end;

implementation

procedure TRegLabelGenericTest<_T1,_T2>.RootKeys;
var
  {%H-}check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  {%H-}root_keys_struct.Clear;

  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegLabelWrapper.RootKeys('TRegLabel',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegLabelGenericTest<_T1,_T2>.PublishedProperties;
begin
  FRegLabelWrapper.PublishedProperties('TRegLabel');
end;

procedure TRegLabelGenericTest<_T1,_T2>.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegLabelWrapper.SectionIdentDefault;

  // 2. Fall CanRead = True: _TREGLABEL_CAPTION_VALUE muss in Caption
  // eingetragen werden
  FRegLabelWrapper.RegControl.RegistrySettings.CanRead := True;
  FRegLabelWrapper.ReadFromReg(True, 'TRegLabel');

  AssertEquals('TRegLabel.Caption', CaptionValueByReg, FRegLabelWrapper.RegControl.Caption);

  // 3. Fall CanRead = False: DEFAULT_CAPTION_VALUE muss in Caption eingetragen
  // werden
  FRegLabelWrapper.RegControl.Caption := DEFAULT_CAPTION_VALUE;
  FRegLabelWrapper.RegControl.RegistrySettings.CanRead := False;
  FRegLabelWrapper.ReadFromReg(True, 'TRegLabel');

  AssertEquals('TRegLabel.Caption', DEFAULT_CAPTION_VALUE,
    FRegLabelWrapper.RegControl.Caption);
end;

procedure TRegLabelGenericTest<_T1,_T2>.SetUp;
begin
  inherited;

  FRegSrcWrapper := _T1.Create;
  FRegLabelWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegLabelGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegLabelWrapper);
  FreeAndNil(FRegSrcWrapper);

  inherited;
end;

{ TRegLabelTest }

procedure TRegLabelTest.SetSectionsAndIdents;
begin
  CaptionValueByReg := _TREGLABEL_CAPTION_VALUE;
end;

{ TRegLabelUTF8Test }

procedure TRegLabelUTF8Test.SetSectionsAndIdents;
begin
  CaptionValueByReg := _TREGLABEL_CAPTION_VALUE_UTF8;
end;

end.

