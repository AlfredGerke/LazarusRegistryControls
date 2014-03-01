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

  { TRegEditTest }

  TRegEditTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
    FRegEditWrapper: TRegEditWrapper;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
  end;

implementation

procedure TRegEditTest.RootKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegEditWrapper.RootKeys('TRegEdit',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegEditTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
  FRegEditWrapper := TRegEditWrapper.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegEditTest.TearDown;
begin
  if Assigned(FRegEditWrapper) then
    FreeAndNil(FRegEditWrapper);

  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

