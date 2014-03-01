unit regradiobutton_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  FPCUnit,
  registrysource_wrapper,
  regradiobutton_wrapper,
  regtype;

type

  TRegRadioButtonTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
    FRegRadioButtonWrapper: TRegRaidoButtonWrapper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
  end;

implementation

procedure TRegRadioButtonTest.RootKeys;
var
  check_rtl_ansi: boolean;
  root_keys_struct: TRootKeysStruct;
begin
  FRegSrcWrapper.GetRootKeys(check_rtl_ansi, root_keys_struct);

  FRegRadioButtonWrapper.RootKeys('TRegRadioButton',
    FRegSrcWrapper.RegistrySource, root_keys_struct, check_rtl_ansi);
end;

procedure TRegRadioButtonTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
  FRegRadioButtonWrapper := TRegRaidoButtonWrapper.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegRadioButtonTest.TearDown;
begin
  if Assigned(FRegRadioButtonWrapper) then
    FreeAndNil(FRegRadioButtonWrapper);

  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

