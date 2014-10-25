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
    procedure SetRootKeysStruct(aRootKey: string;
                                aRootKeyForDefaults: string;
                                aProject: string;
                                aOrganisation: string;
                                aGUID: string);
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings; virtual;
  public
    procedure GetRootKeys(var aCheckRTLAnsi: boolean;
                          var aRootKeysStruct: TRootKeysStruct);
    procedure PublishedProperties; virtual;
    procedure RootKeysStruct; virtual;

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
    procedure RootKeysStruct; override;
  end;

implementation

uses
  Registry,
  test_utils,
  regbasics;

{ TRegistrySourceWrapperUTF8 }

procedure TRegistrySourceWrapperUTF8.SetRegistryEntries;
var
  {%H-}ini: TLRCRegIniFile;
  root_key: string;
begin
  root_key :=
    UTF8Decode(
      'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-'
    + 'AE9B-1548DC65F930}');

  ini := TLRCRegIniFile.Create(root_key);
  try
    with ini do
    begin
      WriteString(UTF8Decode('ReadSection_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Ident_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Value_mit_ßÜÖÄüöä'));

      WriteInteger(UTF8Decode('ReadSection_mit_ßÜÖÄüöä'),
        UTF8Decode('Integer_Ident_mit_ßÜÖÄüöä'),
        12345);

      WriteBool(UTF8Decode('ReadSection_mit_ßÜÖÄüöä'),
        UTF8Decode('Boolean_Ident_mit_ßÜÖÄüöä'),
        True);

      WriteString(UTF8Decode('WriteSection_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Ident_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Value_mit_ßÜÖÄüöä'));

      WriteInteger(UTF8Decode('WriteSection_mit_ßÜÖÄüöä'),
        UTF8Decode('Integer_Ident_mit_ßÜÖÄüöä'),
        12345);

      WriteBool(UTF8Decode('WriteSection_mit_ßÜÖÄüöä'),
        UTF8Decode('Boolean_Ident_mit_ßÜÖÄüöä'),
        True);

      WriteString(UTF8Decode('RenameSection_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Ident_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Value_mit_ßÜÖÄüöä'));

      WriteString(UTF8Decode(SEC_FPCUNIT_URF8TEST),
        UTF8Decode(IDENT_LRC_VERSION_UTF8TEST),
        UTF8Decode(_VERSION));

      WriteString(UTF8Decode('KeyExistsSection_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Ident_mit_ßÜÖÄüöä'),
        UTF8Decode('String_Value_mit_ßÜÖÄüöä'));
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;
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

procedure TRegistrySourceWrapperUTF8.RootKeysStruct;
begin
  SetRootKeysStruct('RootKey_mit_ÄÖÜßäöü', 'RootKeyForDefaults_mit_ÄÖÜßäöü',
    'Project_mit_ÄÖÜßäöü', 'Organisation_mit_ÄÖÜßäöü', 'GUID_mit_ÄÖÜßäöü');
end;

{ TRegistrySourceWrapper }

procedure TRegistrySourceWrapper.SetRegistryEntries;
var
  {%H-}ini: TLRCRegIniFile;
  root_key: string;
begin
  root_key :=
    'SOFTWARE\ExampleFactory\LazarusRegistryControls\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';

  ini := TLRCRegIniFile.Create(root_key);
  try
    with ini do
    begin
      WriteString('ReadSection', 'String_Ident', 'String_Value');

      WriteInteger('ReadSection', 'Integer_Ident', 12345);

      WriteBool('ReadSection', 'Boolean_Ident', True);

      WriteString('WriteSection', 'String_Ident', 'String_Value');

      WriteInteger('WriteSection', 'Integer_Ident', 12345);

      WriteBool('WriteSection', 'Boolean_Ident', True);

      WriteString('RenameSection', 'String_Ident', 'String_Value');

      WriteString('KeyExistsSection', 'String_Ident', 'String_Value');

      WriteString(SEC_FPCUNIT_TEST, IDENT_LRC_VERSION, _VERSION);
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;
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

procedure TRegistrySourceWrapper.GetRootKeys(var aCheckRTLAnsi: boolean;
  var aRootKeysStruct: TRootKeysStruct);
begin
  aCheckRTLAnsi := FRegistrySource.CheckRTLAnsi;
  aRootKeysStruct.Clear;

  aRootKeysStruct.RootKey :=
    IncludeTrailingPathDelimiter(FRegistrySource.RootKey);

  aRootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegistrySource.Organisation,
      ARootKeysStruct.RootKey);

  aRootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegistrySource.Project,
      ARootKeysStruct.RootKey);

  aRootKeysStruct.RootKey :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegistrySource.GUID,
      ARootKeysStruct.RootKey);

  aRootKeysStruct.RootKeyForDefaults :=
    IncludeTrailingPathDelimiter(FRegistrySource.RootKeyForDefaults);

  aRootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttOrganisation],
      FRegistrySource.Organisation,
      ARootKeysStruct.RootKeyForDefaults);

  aRootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttProject],
      FRegistrySource.Project,
      aRootKeysStruct.RootKeyForDefaults);

  aRootKeysStruct.RootKeyForDefaults :=
    _ChangeTokenForKey(TokenTypeStr[ttGUID], FRegistrySource.GUID,
      aRootKeysStruct.RootKeyForDefaults);

  aRootKeysStruct.GUID := FRegistrySource.GUID;
  aRootKeysStruct.Project := FRegistrySource.Project;
  aRootKeysStruct.Organisation := FRegistrySource.Organisation;
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

procedure TRegistrySourceWrapper.RootKeysStruct;
begin
  SetRootKeysStruct('RootKey', 'RootKeyForDefaults', 'Project', 'Organisation',
    'GUID');
end;

procedure TRegistrySourceWrapper.SetRootKeysStruct(aRootKey: string;
  aRootKeyForDefaults: string;
  aProject: string;
  aOrganisation: string;
  aGUID: string);
var
  root_key_struct: TRootKeysStruct;
begin
  root_key_struct.Found := True;
  root_key_struct.RootKey := aRootKey;
  root_key_struct.RootKeyForDefaults := aRootKeyForDefaults;
  root_key_struct.ReadDefaults := True;
  root_key_struct.WriteDefaults := True;
  root_key_struct.RootForDefaults := 'RootForDefaults';
  root_key_struct.Project := aProject;
  root_key_struct.Organisation := aOrganisation;
  root_key_struct.GUID := aGUID;

  TAssert.AssertEquals('TRootKeysStruct.Found', True,
    root_key_struct.Found);

  TAssert.AssertEquals('TRootKeysStruct.RootKey', aRootKey,
    root_key_struct.RootKey);

  TAssert.AssertEquals('TRootKeysStruct.RootKeyForDefaults',
    aRootKeyForDefaults, root_key_struct.RootKeyForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.ReadDefaults', True,
    root_key_struct.ReadDefaults);

  TAssert.AssertEquals('TRootKeysStruct.WriteDefaults', True,
    root_key_struct.WriteDefaults);

  TAssert.AssertEquals('TRootKeysStruct.RootForDefaults', 'RootForDefaults',
    root_key_struct.RootForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.Project', aProject,
    root_key_struct.Project);

  TAssert.AssertEquals('TRootKeysStruct.Organisation', aOrganisation,
    root_key_struct.Organisation);

  TAssert.AssertEquals('TRootKeysStruct.GUID', aGUID,
    root_key_struct.GUID);

  root_key_struct.Clear;

  TAssert.AssertEquals('TRootKeysStruct.Found', False,
    root_key_struct.Found);

  TAssert.AssertEquals('TRootKeysStruct.RootKey', EmptyStr,
    root_key_struct.RootKey);

  TAssert.AssertEquals('TRootKeysStruct.RootKeyForDefaults', EmptyStr,
    root_key_struct.RootKeyForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.ReadDefaults', False,
    root_key_struct.ReadDefaults);

  TAssert.AssertEquals('TRootKeysStruct.WriteDefaults', False,
    root_key_struct.WriteDefaults);

  TAssert.AssertEquals('TRootKeysStruct.RootForDefaults', EmptyStr,
    root_key_struct.RootForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.Project', EmptyStr,
    root_key_struct.Project);

  TAssert.AssertEquals('TRootKeysStruct.Organisation', EmptyStr,
    root_key_struct.Organisation);

  TAssert.AssertEquals('TRootKeysStruct.GUID', EmptyStr,
    root_key_struct.GUID);

  root_key_struct.SetRootKeys(aRootKey, aRootKeyForDefaults, True, True,
    'RootForDefaults', aProject, aOrganisation, aGUID);

  TAssert.AssertEquals('TRootKeysStruct.Found', True,
    root_key_struct.Found);

  TAssert.AssertEquals('TRootKeysStruct.RootKey', aRootKey,
    root_key_struct.RootKey);

  TAssert.AssertEquals('TRootKeysStruct.RootKeyForDefaults',
    aRootKeyForDefaults, root_key_struct.RootKeyForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.ReadDefaults', True,
    root_key_struct.ReadDefaults);

  TAssert.AssertEquals('TRootKeysStruct.WriteDefaults', True,
    root_key_struct.WriteDefaults);

  TAssert.AssertEquals('TRootKeysStruct.RootForDefaults', 'RootForDefaults',
    root_key_struct.RootForDefaults);

  TAssert.AssertEquals('TRootKeysStruct.Project', aProject,
    root_key_struct.Project);

  TAssert.AssertEquals('TRootKeysStruct.Organisation', aOrganisation,
    root_key_struct.Organisation);

  TAssert.AssertEquals('TRootKeysStruct.GUID', aGUID,
    root_key_struct.GUID);
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
  FRegistrySource.Name := Format('%s%d', [TREGISTRYSOURCE_NAME, GetNextCount]);
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

