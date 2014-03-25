unit regedit_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regedit_wrapper,
  regtype;

type

  { TRegEditGenericTest }

  TRegEditGenericTest<_T1,_T2>= class(TTestCase)
  private
    FRegSrcWrapper: _T1;
    FRegEditWrapper: _T2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure PublishedProperties;
  end;

  { TRegEditTest }
  TRegEditTest= class(TRegEditGenericTest<TRegistrySourceWrapper, TRegEditWrapper>)
  end;

  { TRegEditUTF8Test }
  TRegEditUTF8Test= class(TRegEditGenericTest<TRegistrySourceWrapperUTF8, TRegEditWrapperUTF8>)
  end;

implementation

{ TRegEditGenericTest }

procedure TRegEditGenericTest<_T1,_T2>.RootKeys;
var
  {%H-}check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  {%H-}root_keys_struct.Clear;

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
  FRegSrcWrapper := _T1.Create;
  FRegEditWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegEditGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegEditWrapper);
  FreeAndNil(FRegSrcWrapper);
end;

end.

