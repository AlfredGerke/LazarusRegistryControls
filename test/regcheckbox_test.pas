unit regcheckbox_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regcheckbox_wrapper,
  regtype;

type

  TRegCheckBoxTest= class(TTestCase)
  protected
    FRegSrcWrapper: TRegistrySourceWrapper;
    FRegCheckBoxWrapper: TRegCheckBoxWrapper;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RookKeys;
  end;

implementation

procedure TRegCheckBoxTest.RookKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegCheckBoxWrapper.RootKeys('TRegCheckBox',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegCheckBoxTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
  FRegCheckBoxWrapper :=
    TRegCheckBoxWrapper.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegCheckBoxTest.TearDown;
begin
  if Assigned(FRegCheckBoxWrapper) then
    FreeAndNil(FRegCheckBoxWrapper);

  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

