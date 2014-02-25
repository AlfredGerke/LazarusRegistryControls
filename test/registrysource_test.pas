unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
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
  AssertEquals('RootKey', 'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKey);
  AssertEquals('RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegistrySource.RootKeyForDefaults);
  AssertEquals('RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKeyForCommon);
  AssertEquals('Project', 'LazarusRegistryControls', FRegistrySource.Project);
  AssertEquals('Organisation', 'ExampleFactory', FRegistrySource.Organisation);
  AssertEquals('RootForDefaults', 'HKEY_LOCAL_MACHINE',
  FRegistrySource.RootForDefaults);
  AssertEquals('ReadDefaults', True, FRegistrySource.ReadDefaults);
  AssertEquals('WriteDefaults', False, FRegistrySource.WriteDefaults);
  AssertEquals('GUID', '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}',
    FRegistrySource.GUID);
  AssertEquals('DoSyncData', True, FRegistrySource.DoSyncData);
  AssertEquals('PrefereStrings', False, FRegistrySource.PrefereStrings);
  AssertEquals('CheckRTLAnsi', True, FRegistrySource.CheckRTLAnsi);
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

end.

