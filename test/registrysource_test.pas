unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  regsourcen;

type

  { TRegistrySourceTest }

  TRegistrySourceTest= class(TTestCase)
  private
    FRegistrySource: TRegistrySource;

    procedure SetRegistrySource;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckPublishedProperties;
  end;

implementation

procedure TRegistrySourceTest.CheckPublishedProperties;
begin
  AssertEquals('RootKey', FRegistrySource.RootKey,
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%');
  AssertEquals('RootKeyForDefaults', FRegistrySource.RootKeyForDefaults,
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%');
  AssertEquals('RootKeyForCommon', FRegistrySource.RootKeyForCommon,
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%');
  AssertEquals('Project', FRegistrySource.Project, 'LazarusRegistryControls');
  AssertEquals('Organisation', FRegistrySource.Organisation, 'ExampleFactory');
  AssertEquals('RootForDefaults', FRegistrySource.RootForDefaults,
    'HKEY_LOCAL_MACHINE');
  AssertEquals('ReadDefaults', FRegistrySource.ReadDefaults, True);
  AssertEquals('WriteDefaults', FRegistrySource.WriteDefaults, False);
  AssertEquals('GUID', FRegistrySource.GUID,
    '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}');
  AssertEquals('DoSyncData', FRegistrySource.DoSyncData, True);
  AssertEquals('PrefereStrings', FRegistrySource.PrefereStrings, False);
  AssertEquals('CheckRTLAnsi', FRegistrySource.CheckRTLAnsi, True);
end;

procedure TRegistrySourceTest.SetRegistrySource;
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

procedure TRegistrySourceTest.SetUp;
begin
  FRegistrySource := TRegistrySource.Create(nil);
  SetRegistrySource;
end;

procedure TRegistrySourceTest.TearDown;
begin
  if Assigned(FRegistrySource) then
    FreeAndNil(FRegistrySource);
end;

initialization

  RegisterTest(TRegistrySourceTest);
end.

