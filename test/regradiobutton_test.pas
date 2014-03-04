unit regradiobutton_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  FPCUnit,
  registrysource_wrapper,
  regradiobutton_wrapper,
  regtype,
  test_const;

type

  { TRegRadioButtonTest }

  TRegRadioButtonTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
    FRegRadioButtonWrapper: TRegRaidoButtonWrapper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RootKeys;
    procedure ReadCaptionSettings;
  end;

implementation

procedure TRegRadioButtonTest.RootKeys;
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

procedure TRegRadioButtonTest.ReadCaptionSettings;
var
  caption_by_default: string;
  caption_by_registry: string;
begin
  caption_by_registry := _TREGCHECKBOX_CAPTION_VALUE;
  caption_by_default := DEFAULT_CAPTION_VALUE;

  FRegRadioButtonWrapper.ReadCaption(caption_by_default, caption_by_registry,
    'Caption');
end;

procedure TRegRadioButtonTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
  FRegRadioButtonWrapper :=
    TRegRaidoButtonWrapper.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegRadioButtonTest.TearDown;
begin
  if Assigned(FRegRadioButtonWrapper) then
    FreeAndNil(FRegRadioButtonWrapper);

  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

