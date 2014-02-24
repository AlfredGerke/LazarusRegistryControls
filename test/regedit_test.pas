unit regedit_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  regedit,
  regsourcen,
  regtype,
  regconst;

type

  { TRegEditTest }

  TRegEditTest= class(TTestCase)
  private
    FRegEdit: TRegEdit;
    FRegistrySource: TRegistrySource;

    procedure SetRegEdit;
    procedure SetRegistrySource;
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
begin
  // Properties von TRegistrysource
  AssertEquals('TRegistrySource.RootKey',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKey);
  AssertEquals('TRegistrySource.RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegistrySource.RootKeyForDefaults);
  AssertEquals('TRegistrySource.RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKeyForCommon);
  AssertEquals('TRegistrySource.Project', 'LazarusRegistryControls',
    FRegistrySource.Project);
  AssertEquals('TRegistrySource.Organisation', 'ExampleFactory',
    FRegistrySource.Organisation);
  AssertEquals('TRegistrySource.RootForDefaults', 'HKEY_LOCAL_MACHINE',
    FRegistrySource.RootForDefaults);
  AssertEquals('TRegistrySource.ReadDefaults', True,
    FRegistrySource.ReadDefaults);
  AssertEquals('TRegistrySource.WriteDefaults', False,
    FRegistrySource.WriteDefaults);
  AssertEquals('TRegistrySource.GUID', '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}',
    FRegistrySource.GUID);
  AssertEquals('TRegistrySource.DoSyncData', True, FRegistrySource.DoSyncData);
  AssertEquals('TRegistrySource.PrefereStrings', False,
    FRegistrySource.PrefereStrings);
  AssertEquals('TRegistrySource.CheckRTLAnsi', True,
    FRegistrySource.CheckRTLAnsi);

  regsettings_rootkey := IncludeTrailingPathDelimiter(FRegistrySource.RootKey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegEdit.RegistrySettings.Organisation,
      regsettings_rootkey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegEdit.RegistrySettings.Project,
      regsettings_rootkey);
  regsettings_rootkey :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegEdit.RegistrySettings.GUID,
      regsettings_rootkey);

  // Properties von TRegEdit
  AssertEquals('TRegEdit.RegistrySettings.RootKey',
    regsettings_rootkey, FRegEdit.RegistrySettings.RootKey);
  AssertEquals('TRegEdit.RegistrySettings.RootKeyForDefaults',
    FRegistrySource.RootKeyForDefaults,
    FRegEdit.RegistrySettings.RootKeyForDefaults);
  AssertEquals('TRegEdit.RegistrySettings.RootForDefaults',
    FRegistrySource.RootForDefaults, FRegEdit.RegistrySettings.RootForDefaults);
  AssertEquals('TRegEdit.RegistrySettings.Project',
    FRegistrySource.Project, FRegEdit.RegistrySettings.Project);
  AssertEquals('TRegEdit.RegistrySettings.Organisation',
    FRegistrySource.Organisation, FRegEdit.RegistrySettings.Organisation);
  AssertEquals('TRegEdit.RegistrySettings.GUID',
    FRegistrySource.GUID, FRegEdit.RegistrySettings.GUID);
  AssertEquals('TRegEdit.RegistrySettings.ReadDefaults',
    FRegistrySource.ReadDefaults, FRegEdit.RegistrySettings.ReadDefaults);
  AssertEquals('TRegEdit.RegistrySettings.WriteDefaults',
    FRegistrySource.WriteDefaults, FRegEdit.RegistrySettings.WriteDefaults);
end;

procedure TRegEditTest.SetRegEdit;
begin
  FRegEdit.RegistrySource := FRegistrySource;

  FRegEdit.RegistrySettings.CanRead := True;
  FRegEdit.RegistrySettings.CanWrite := True;
  FRegEdit.RegistrySettings.DoWriteAdHoc := True;
  FRegEdit.RegistrySettings.GroupIndex := 0;
  FRegEdit.RegistrySettings.DoSyncData := False;
  FRegEdit.RegistrySettings.Default := 'Default-Eintrag';
  FRegEdit.RegistrySettings.Section := 'Captions';
  FRegEdit.RegistrySettings.Ident := 'Tüäßv-ident';
  FRegEdit.RegistrySource := FRegistrySource;
end;

procedure TRegEditTest.SetRegistrySource;
begin
  FRegistrySource.RootKey :=
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%';
  FRegistrySource.RootKeyForDefaults :=
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%';
  FRegistrySource.RootKeyForCommon :=
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%';
  FRegistrySource.Project := 'LazarusRegistryControls';
  FRegistrySource.Organisation := 'ExampleFactory';
  FRegistrySource.RootForDefaults := 'HKEY_LOCAL_MACHINE';
  FRegistrySource.ReadDefaults := True;
  FRegistrySource.WriteDefaults := False;
  FRegistrySource.GUID := '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  FRegistrySource.DoSyncData := True;
  FRegistrySource.PrefereStrings := False;
  FRegistrySource.CheckRTLAnsi := True;
end;

procedure TRegEditTest.SetUp;
begin
  FRegistrySource := TRegistrySource.Create(nil);
  SetRegistrySource;
  FRegEdit := TRegEdit.Create(nil);
  SetRegEdit;
end;

procedure TRegEditTest.TearDown;
begin
  if Assigned(FRegEdit) then
    FreeAndNil(FREgEdit);

  if Assigned(FRegistrySource) then
    FreeAndNil(FRegistrySource);
end;

end.

