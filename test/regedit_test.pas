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
    procedure PublishedProperties;
  end;

implementation

procedure TRegEditTest.RootKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  check_rtl_ansi := False;
  {%H-}root_keys_struct.Clear;

  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegEditWrapper.RootKeys('TRegEdit',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegEditTest.PublishedProperties;
begin
  FRegEditWrapper.PublishedProperties('TRegEdit');
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

