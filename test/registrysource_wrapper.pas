unit registrysource_wrapper;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  regsourcen,
  test_const;

type

  { TRegistrySourceWrapper }

  TRegistrySourceWrapper = class
  private
    FRegistrySource: TRegistrySource;
  protected
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property RegistrySource : TRegistrySource
      read FRegistrySource;
  end;

implementation

{ TRegistrySourceWrapper }

procedure TRegistrySourceWrapper.SetRegistryEntries;
begin
  FRegistrySource.WriteString(SEC_FPCUNIT_TEST, IDENT_LRC_VERSION, _VERSION);
end;

procedure TRegistrySourceWrapper.SetRegistrySettings;
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

constructor TRegistrySourceWrapper.Create;
begin
  FRegistrySource := TRegistrySource.Create(nil);
  SetRegistrySettings;
  SetRegistryEntries;
end;

destructor TRegistrySourceWrapper.Destroy;
begin
  if Assigned(FRegistrySource) then
    FreeAndNil(FRegistrySource);

  inherited Destroy;
end;

end.

