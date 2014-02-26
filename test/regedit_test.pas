unit regedit_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regedit_wrapper,
  regtype,
  regconst,
  regconvutils;

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
    procedure CheckPublishedProperties;
  end;

implementation

procedure TRegEditTest.CheckPublishedProperties;
var
  regsettings_rootkey: string;
  regsettings_rootkeyfordefaults: string;
  regsettings_guid: string;
  regsettings_project: string;
  regsettings_organisation: string;
  check_rtl_ansi: boolean;
begin
  // Properties von TRegistrysource
  AssertEquals('TRegistrySource.RootKey',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%',
    FRegSrcWrapper.RegistrySource.RootKey);
  AssertEquals('TRegistrySource.RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegSrcWrapper.RegistrySource.RootKeyForDefaults);
  AssertEquals('TRegistrySource.RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegSrcWrapper.RegistrySource.RootKeyForCommon);
  AssertEquals('TRegistrySource.Project', 'LazarusRegistryControls',
    FRegSrcWrapper.RegistrySource.Project);
  AssertEquals('TRegistrySource.Organisation', 'ExampleFactory',
    FRegSrcWrapper.RegistrySource.Organisation);
  AssertEquals('TRegistrySource.RootForDefaults', 'HKEY_LOCAL_MACHINE',
    FRegSrcWrapper.RegistrySource.RootForDefaults);
  AssertEquals('TRegistrySource.ReadDefaults', True,
    FRegSrcWrapper.RegistrySource.ReadDefaults);
  AssertEquals('TRegistrySource.WriteDefaults', False,
    FRegSrcWrapper.RegistrySource.WriteDefaults);
  AssertEquals('TRegistrySource.GUID', '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}',
    FRegSrcWrapper.RegistrySource.GUID);
  AssertEquals('TRegistrySource.DoSyncData', True,
    FRegSrcWrapper.RegistrySource.DoSyncData);
  AssertEquals('TRegistrySource.PrefereStrings', False,
    FRegSrcWrapper.RegistrySource.PrefereStrings);
  AssertEquals('TRegistrySource.CheckRTLAnsi', True,
    FRegSrcWrapper.RegistrySource.CheckRTLAnsi);

  check_rtl_ansi := FRegSrcWrapper.RegistrySource.CheckRTLAnsi;
  regsettings_rootkey :=
    IncludeTrailingPathDelimiter(FRegSrcWrapper.RegistrySource.RootKey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegSrcWrapper.RegistrySource.Organisation,
      regsettings_rootkey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegSrcWrapper.RegistrySource.Project,
      regsettings_rootkey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegSrcWrapper.RegistrySource.GUID,
      regsettings_rootkey);

  regsettings_rootkeyfordefaults :=
    IncludeTrailingPathDelimiter(FRegSrcWrapper.RegistrySource.RootKeyForDefaults);
  regsettings_rootkeyfordefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegSrcWrapper.RegistrySource.Organisation,
      regsettings_rootkeyfordefaults);
  regsettings_rootkeyfordefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegSrcWrapper.RegistrySource.Project,
      regsettings_rootkeyfordefaults);
  regsettings_rootkeyfordefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegSrcWrapper.RegistrySource.GUID,
      regsettings_rootkeyfordefaults);

  regsettings_guid := FRegSrcWrapper.RegistrySource.GUID;
  regsettings_project := FRegSrcWrapper.RegistrySource.Project;
  regsettings_organisation := FRegSrcWrapper.RegistrySource.Organisation;

  // Properties von TRegEdit
  // Jeder Getter für ein String-Property besitzt ein UTF8ToSysIfNeeded
  AssertEquals('TRegEdit.RegistrySettings.RootKey',
    UTF8ToSysIfNeeded(regsettings_rootkey, check_rtl_ansi),
    FRegEditWrapper.RegEdit.RegistrySettings.RootKey);
  AssertEquals('TRegEdit.RegistrySettings.RootKeyForDefaults',
    UTF8ToSysIfNeeded(regsettings_rootkeyfordefaults, check_rtl_ansi),
    FRegEditWrapper.RegEdit.RegistrySettings.RootKeyForDefaults);
  AssertEquals('TRegEdit.RegistrySettings.RootForDefaults',
    FRegSrcWrapper.RegistrySource.RootForDefaults,
    FRegEditWrapper.RegEdit.RegistrySettings.RootForDefaults);
  AssertEquals('TRegEdit.RegistrySettings.Project',
    UTF8ToSysIfNeeded(regsettings_project, check_rtl_ansi),
    FRegEditWrapper.RegEdit.RegistrySettings.Project);
  AssertEquals('TRegEdit.RegistrySettings.Organisation',
    UTF8ToSysIfNeeded(regsettings_organisation, check_rtl_ansi),
    FRegEditWrapper.RegEdit.RegistrySettings.Organisation);
  AssertEquals('TRegEdit.RegistrySettings.GUID',
    UTF8ToSysIfNeeded(regsettings_guid, check_rtl_ansi),
    FRegEditWrapper.RegEdit.RegistrySettings.GUID);
  AssertEquals('TRegEdit.RegistrySettings.ReadDefaults',
    FRegSrcWrapper.RegistrySource.ReadDefaults,
    FRegEditWrapper.RegEdit.RegistrySettings.ReadDefaults);
  AssertEquals('TRegEdit.RegistrySettings.WriteDefaults',
    FRegSrcWrapper.RegistrySource.WriteDefaults,
    FRegEditWrapper.RegEdit.RegistrySettings.WriteDefaults);
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

