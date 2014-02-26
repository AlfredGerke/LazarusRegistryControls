unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper;

type

  { TRegistrySourceTest }

  TRegistrySourceTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
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
    FRegSrcWrapper.RegistrySource.RootKey);
  AssertEquals('RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegSrcWrapper.RegistrySource.RootKeyForDefaults);
  AssertEquals('RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegSrcWrapper.RegistrySource.RootKeyForCommon);
  AssertEquals('Project', 'LazarusRegistryControls',
    FRegSrcWrapper.RegistrySource.Project);
  AssertEquals('Organisation', 'ExampleFactory',
    FRegSrcWrapper.RegistrySource.Organisation);
  AssertEquals('RootForDefaults', 'HKEY_LOCAL_MACHINE',
    FRegSrcWrapper.RegistrySource.RootForDefaults);
  AssertEquals('ReadDefaults', True,
    FRegSrcWrapper.RegistrySource.ReadDefaults);
  AssertEquals('WriteDefaults', False,
    FRegSrcWrapper.RegistrySource.WriteDefaults);
  AssertEquals('GUID', '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}',
    FRegSrcWrapper.RegistrySource.GUID);
  AssertEquals('DoSyncData', True,
    FRegSrcWrapper.RegistrySource.DoSyncData);
  AssertEquals('PrefereStrings', False,
    FRegSrcWrapper.RegistrySource.PrefereStrings);
  AssertEquals('CheckRTLAnsi', True,
    FRegSrcWrapper.RegistrySource.CheckRTLAnsi);
end;

procedure TRegistrySourceTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
end;

procedure TRegistrySourceTest.TearDown;
begin
  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

