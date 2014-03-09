unit registrysource_wrapper;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  regsourcen,
  test_const,
  fpcunit,
  regtype,
  regconst;

type

  { TRegistrySourceWrapper }

  TRegistrySourceWrapper = class
  private
    FRegistrySource: TRegistrySource;
  protected
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings; virtual;
  public
    procedure GetRootKeys(var ACheckRTLAnsi: boolean;
                          var ARootKeysStruct: TRootKeysStruct);
    procedure PublishedProperties; virtual;

    // Folgende Methoden sollten in einer GUI getestet werden
    //!<--
    procedure PostClientData;
    procedure RefreshMergeDataProperty;
    procedure GetClientList;
    procedure ClearClientItems;
    procedure RenameClient;
    procedure ShowClientEditDialog;
    procedure GetClientByName;
    procedure GetClientNameByIndex;
    procedure FreeRegistrySource;
    procedure RefreshWriteAdHocProperty;
    procedure RefreshSyncProperty;
    procedure RefreshSettings;
    procedure RefreshClientData;
    procedure RegisterClient;
    procedure UnRegisterClient;
    //-->

    constructor Create; virtual;
    destructor Destroy; override;
  public
    property RegistrySource : TRegistrySource
      read FRegistrySource;
  end;

  { TRegistrySourceWrapperUTF8 }

  TRegistrySourceWrapperUTF8 = class(TRegistrySourceWrapper)
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings; override;
  public
    procedure PublishedProperties; override;
  end;

implementation

{ TRegistrySourceWrapperUTF8 }

procedure TRegistrySourceWrapperUTF8.SetRegistryEntries;
begin
  FRegistrySource.WriteString(SEC_FPCUNIT_URF8TEST, IDENT_LRC_VERSION_UTF8TEST,
    _VERSION);
end;

procedure TRegistrySourceWrapperUTF8.SetRegistrySettings;
begin
  FRegistrySource.RootKey :=
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%';
  FRegistrySource.RootKeyForDefaults :=
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%';
  FRegistrySource.RootKeyForCommon :=
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%';
  FRegistrySource.Project := 'Project_mit_ßÜÖÄüöä';
  FRegistrySource.Organisation := 'Organisation_mit_ßÜÖÄüöä';
  FRegistrySource.RootForDefaults := 'HKEY_LOCAL_MACHINE';
  FRegistrySource.ReadDefaults := True;
  FRegistrySource.WriteDefaults := False;
  FRegistrySource.GUID := '{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  FRegistrySource.DoSyncData := True;
  FRegistrySource.PrefereStrings := False;
  FRegistrySource.CheckRTLAnsi := True;
end;

procedure TRegistrySourceWrapperUTF8.PublishedProperties;
begin
  TAssert.AssertEquals('TRegistrySource.RootKey',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKey);

  TAssert.AssertEquals('TRegistrySource.RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegistrySource.RootKeyForDefaults);

  TAssert.AssertEquals('TRegistrySource.RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKeyForCommon);

  TAssert.AssertEquals('TRegistrySource.Project', 'Project_mit_ßÜÖÄüöä',
    FRegistrySource.Project);

  TAssert.AssertEquals('TRegistrySource.Organisation', 'Organisation_mit_ßÜÖÄüöä',
    FRegistrySource.Organisation);

  TAssert.AssertEquals('TRegistrySource.RootForDefaults', 'HKEY_LOCAL_MACHINE',
    FRegistrySource.RootForDefaults);

  TAssert.AssertEquals('TRegistrySource.ReadDefaults', True,
    FRegistrySource.ReadDefaults);

  TAssert.AssertEquals('TRegistrySource.WriteDefaults', False,
    FRegistrySource.WriteDefaults);

  TAssert.AssertEquals('TRegistrySource.GUID',
    '{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}',
    FRegistrySource.GUID);

  TAssert.AssertEquals('TRegistrySource.DoSyncData', True,
    FRegistrySource.DoSyncData);

  TAssert.AssertEquals('TRegistrySource.PrefereStrings', False,
    FRegistrySource.PrefereStrings);

  TAssert.AssertEquals('TRegistrySource.CheckRTLAnsi', True,
    FRegistrySource.CheckRTLAnsi);
end;

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

procedure TRegistrySourceWrapper.GetRootKeys(var ACheckRTLAnsi: boolean;
  var ARootKeysStruct: TRootKeysStruct);
begin
  ACheckRTLAnsi := FRegistrySource.CheckRTLAnsi;
  ARootKeysStruct.Clear;

  ARootKeysStruct.RootKey :=
    IncludeTrailingPathDelimiter(FRegistrySource.RootKey);

  ARootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegistrySource.Organisation,
      ARootKeysStruct.RootKey);

  ARootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegistrySource.Project,
      ARootKeysStruct.RootKey);

  ARootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegistrySource.GUID,
      ARootKeysStruct.RootKey);

  ARootKeysStruct.RootKeyForDefaults :=
    IncludeTrailingPathDelimiter(FRegistrySource.RootKeyForDefaults);

  ARootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegistrySource.Organisation,
      ARootKeysStruct.RootKeyForDefaults);

  ARootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegistrySource.Project,
      ARootKeysStruct.RootKeyForDefaults);

  ARootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegistrySource.GUID,
      ARootKeysStruct.RootKeyForDefaults);

  ARootKeysStruct.GUID := FRegistrySource.GUID;
  ARootKeysStruct.Project := FRegistrySource.Project;
  ARootKeysStruct.Organisation := FRegistrySource.Organisation;
end;

procedure TRegistrySourceWrapper.PublishedProperties;
begin
  TAssert.AssertEquals('TRegistrySource.RootKey',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKey);

  TAssert.AssertEquals('TRegistrySource.RootKeyForDefaults',
    'SOFTWARE\%%ORGANISATION%%\%%PROJECT%%\DEFAULTS\%%GUID%%',
    FRegistrySource.RootKeyForDefaults);

  TAssert.AssertEquals('TRegistrySource.RootKeyForCommon',
    'SOFTWARE\%%ORGANISATION%%\GEMEINSAME DATEN\%%PROJECT%%\%%GUID%%',
    FRegistrySource.RootKeyForCommon);

  TAssert.AssertEquals('TRegistrySource.Project', 'LazarusRegistryControls',
    FRegistrySource.Project);

  TAssert.AssertEquals('TRegistrySource.Organisation', 'ExampleFactory',
    FRegistrySource.Organisation);

  TAssert.AssertEquals('TRegistrySource.RootForDefaults', 'HKEY_LOCAL_MACHINE',
    FRegistrySource.RootForDefaults);

  TAssert.AssertEquals('TRegistrySource.ReadDefaults', True,
    FRegistrySource.ReadDefaults);

  TAssert.AssertEquals('TRegistrySource.WriteDefaults', False,
    FRegistrySource.WriteDefaults);

  TAssert.AssertEquals('TRegistrySource.GUID',
    '{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}',
    FRegistrySource.GUID);

  TAssert.AssertEquals('TRegistrySource.DoSyncData', True,
    FRegistrySource.DoSyncData);

  TAssert.AssertEquals('TRegistrySource.PrefereStrings', False,
    FRegistrySource.PrefereStrings);

  TAssert.AssertEquals('TRegistrySource.CheckRTLAnsi', True,
    FRegistrySource.CheckRTLAnsi);
end;

procedure TRegistrySourceWrapper.PostClientData;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RefreshMergeDataProperty;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.GetClientList;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.ClearClientItems;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RenameClient;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.ShowClientEditDialog;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.GetClientByName;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.GetClientNameByIndex;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.FreeRegistrySource;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RefreshWriteAdHocProperty;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RefreshSyncProperty;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RefreshSettings;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RefreshClientData;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.RegisterClient;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
end;

procedure TRegistrySourceWrapper.UnRegisterClient;
begin
  TAssert.Fail('GUI-Test nocht nicht implementiert!');
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

