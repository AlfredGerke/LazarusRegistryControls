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
    procedure PublishedProperties;
    procedure ReadByCaptionSettings;
    procedure ReadRegistry;
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

procedure TRegRadioButtonTest.PublishedProperties;
begin
  FRegRadioButtonWrapper.PublishedProperties('TRegRadioButton');
end;

procedure TRegRadioButtonTest.ReadByCaptionSettings;
var
  caption_by_default: string;
  caption_by_registry: string;
begin
  caption_by_registry := _TREGCHECKBOX_CAPTION_VALUE;
  caption_by_default := DEFAULT_CAPTION_VALUE;

  FRegRadioButtonWrapper.ReadCaption(caption_by_default, caption_by_registry,
    'Caption');
end;

procedure TRegRadioButtonTest.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  AssertEquals('TRegRadioButton.RegistrySection.Section', SEC_TREGRADIOBUTTON,
    FRegRadioButtonWrapper.RegControl.RegistrySettings.Section);
  AssertEquals('TRegRadioButton.RegistrySection.Ident', IDENT_CHECK_PROPERTY,
    FRegRadioButtonWrapper.RegControl.RegistrySettings.Ident);
  AssertEquals('TRegRadioButton.RegistrySection.Default', DEFAULT_CHECKED_ENTRY,
    FRegRadioButtonWrapper.RegControl.RegistrySettings.Default);

  // 1. Fall CanRead = False: DEFAULT_CHECKED_ENTRY muss in Checked eingetragen
  // werden
  FRegRadioButtonWrapper.RegControl.Checked := DEFAULT_CHECKED_ENTRY;
  FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := False;
  FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

  AssertEquals('TRegRadioButton.RegistrySection.Default', DEFAULT_CHECKED_ENTRY,
    FRegRadioButtonWrapper.RegControl.Checked);

  // 2. Fall CanRead = True: _CHECKED_ENTRY muss in Checked eingetragen werden
  FRegRadioButtonWrapper.RegControl.Checked := DEFAULT_CHECKED_ENTRY;
  FRegRadioButtonWrapper.RegControl.RegistrySettings.CanRead := True;
  FRegRadioButtonWrapper.ReadFromReg(True, rdoGeneral, 'TRegRadioButton');

  AssertEquals('TRegRadioButton.RegistrySection.Default', _CHECKED_ENTRY,
    FRegRadioButtonWrapper.RegControl.Checked);
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

