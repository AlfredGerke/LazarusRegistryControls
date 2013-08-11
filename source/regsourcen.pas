unit regsourcen;

{S+}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  LResources,
  PropEdits;

const

  PROJECT_TOKEN = '%%PROJECT%%';
  ORGANISATION_TOKEN = '%%ORGANISATION%%';
  GUID_TOKEN = '%%GUID%%';

type

  { TCustomProperties }

  TCustomProperties = class(TPersistent)
  private
  protected
  public
  published
  end;

  { TCustomRegistrySettings }

  TCustomRegistrySettings<_T> = class(TCustomProperties)
  private
    //Basisschlüssel, z.B.: SOFTWARE\SOFTWARE AUS ERWITTE\%%PROJECT%%\
    FRootKey: string;
    //Basisschlüssel für Defaults, z.B.: SOFTWARE\SOFTWARE AUS ERWITTE\%%PROJECT%%\DEFAULTS\
    FRootKeyForDefaults: string;
    //Unterschlüssel unter dem Basisschlüssel, z.B.: Desktop
    FSection: string;
    //Ident im Unterschlüssel, z.B.: FileDir
    FIdent: string;
    //Default, wenn kein Wert in der Registry gefunden wurde
    FDefault: _T;
    //Wenn TRUE, dann Werte aus dem Basisschlüssel für Defaults lesen
    FReadDefaults: boolean;
    //Wenn TRUE, dann Werte in den Basisschlüssel für Defaults schreiben
    FWriteDefaults: boolean;
    //Root des Basisschlüssel für Defaults (in der Regel: HKEY_LOCAL_MACHINE)
    FRootForDefaults: string;
    //Werte dürfen aus der Registry gelesen werden
    FCanRead: boolean;
    //Werte dürfen in die Registry geschrieben werden
    FCanWrite: boolean;
    //Werte werde sofort (OnChange, OnClick, etc.) in die Registry geschrieben
    FDoWriteAdHoc: boolean;
    FProject: string;
    FOrganisation: string;
    FGUID: string;
    FOnChange: TNotifyEvent;

  protected
    procedure SetRootKey(aRootKey: string);
    procedure SetRootKeyForDefaults(aRootKeyForDefaults: string);
    procedure SetSection(aSection: string);
    procedure SetIdent(aIdent: string);
    procedure SetReadDefaults(aReadDefaults: boolean);
    procedure SetWriteDefaults(aWriteDefaults: boolean);
    procedure SetRootForDefaults(aRootForDefaults: string);
    procedure SetCanRead(aCanRead: boolean);
    procedure SetCanWrite(aCanWrite: boolean);
    procedure SetDoWriteAdHoc(aDoWriteAdHoc: boolean);
    procedure SetDefault(aDefault: _T);
    procedure SetGUID(aGUID: string);
    procedure SetProject(aProject: string);
    procedure SetOrganisation(aOrganisation: string);

    property Default: _T
      read FDefault
      write SetDefault;
  public
    property GUID: string
      read FGUID
      write SetGUID;
    property Project: string
      read FProject
      write SetProject;
    property Organisation: string
      read FOrganisation
      write SetOrganisation;
    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  published
    property RookKey: string
      read FRootKey
      write SetRootKey;
    property RootKeyForDefaults: string
      read FRootKeyForDefaults
      write SetRootKeyForDefaults;
    property Section: string
      read FSection
      write SetSection;
    property Ident: string
      read FIdent
      write SetIdent;
    property ReadDefaults: boolean
      read FReadDefaults
      write SetReadDefaults;
    property WriteDefaults: boolean
      read FWriteDefaults
      write SetWriteDefaults;
    property RootForDefaults: string
      read FRootForDefaults
      write SetRootForDefaults;
    property CanRead: boolean
      read FCanRead
      write SetCanRead;
    property CanWrite: boolean
      read FCanWrite
      write SetCanWrite;
    property DoWriteAdHoc: boolean
      read FDoWriteAdHoc
      write SetDoWriteAdHoc;
  end;

  { TRegistrySettingsBooleanDefault }

  TRegistrySettingsBooleanDefault = class(TCustomRegistrySettings<boolean>)
  published
    property Default;
  end;

  { TRegistrySettingsIntegerDefault }

  TRegistrySettingsIntegerDefault = class(TCustomRegistrySettings<integer>)
  published
    property Default;
  end;

  { TRegistrySettingsStringDefault }

  TRegistrySettingsStringDefault = class(TCustomRegistrySettings<string>)
  published
    property Default;
  end;

  { TRegistrySettingsList }

  TRegistrySettingsList = class(TRegistrySettingsIntegerDefault)
  private
    FItemsByRegistry: boolean;
    FListSection: string;
  protected
  public
  published
    property ItemsByRegistry: boolean
      read FItemsByRegistry
      write FItemsByRegistry;
    property ListSection: string
      read FListSection
      write FListSection;
  end;

  { TCustomRegistrySource }

  TCustomRegistrySource = class(TComponent)
  private
    FRootKey: string;
    FRootKeyForDefaults: string;
    FRootKeyForCommon: string;
    FProject: string;
    FOrganisation: string;
    FRootForDefaults: string;
    FReadDefaults: boolean;
    FWriteDefaults: boolean;
    FGUID: string;
  protected
    property RootKey: string
      read FRootKey
      write FRootKey;
    property RootKeyForDefaults: string
      read FRootKeyForDefaults
      write FRootKeyForDefaults;
    property RootKeyForCommon: string
      read FRootKeyForCommon
      write FRootKeyForCommon;
    property Project: string
      read FProject
      write FProject;
    property Organisation: string
      read FOrganisation
      write FOrganisation;
    property RootForDefaults: string
      read FRootForDefaults
      write FRootForDefaults;
    property ReadDefaults: boolean
      read FReadDefaults
      write FReadDefaults;
    property WriteDefaults: boolean
      read FWriteDefaults
      write FWriteDefaults;
    property GUID: string
      read FGUID
      write FGUID;
  public
    function GetRootKey: string;
    function GetRootKeyForDefaults: string;
    function GetRootKeyForCommon: string;
    function ReadString(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aIdent: string;
                        aDefault: string;
                        aUseDefaults: boolean): string; reintroduce; overload;
    function ReadString(aSection: string;
                        aIdent: string;
                        aDefault: string): string; reintroduce; overload;
    function ReadInteger(aRootKey: string;
                         aRootKeyForDefaults: string;
                         aRootForDefaults: string;
                         aSection: string;
                         aIdent: string;
                         aDefault: integer;
                         aUseDefaults: boolean): integer; reintroduce; overload;
    function ReadInteger(aSection: string;
                         aIdent: string;
                         aDefault: integer): integer; reintroduce; overload;
    function ReadBool(aRootKey: string;
                      aRootKeyForDefaults: string;
                      aRootForDefaults: string;
                      aSection: string;
                      aIdent: string;
                      aDefault: boolean;
                      aUseDefaults: boolean): boolean; reintroduce; overload;
    function ReadBool(aSection: string;
                      aIdent: string;
                      aDefault: boolean): boolean; reintroduce; overload;
    procedure ReadSection(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aRootForDefaults: string;
                          aSection: string;
                          aStrings: TStrings;
                          aUseDefaults: boolean); reintroduce; overload;
    procedure ReadSection(aSection: string;
                          aStrings: TStrings); reintroduce; overload;
    procedure WriteString(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aRootForDefaults: string;
                          aSection: string;
                          aIdent: string;
                          aDefault: string;
                          aUseDefaults: boolean); reintroduce; overload;
    procedure WriteString(aSection: string;
                          aIdent: string;
                          aDefault: string); reintroduce; overload;
    procedure WriteInteger(aRootKey: string;
                           aRootKeyForDefaults: string;
                           aRootForDefaults: string;
                           aSection: string;
                           aIdent: string;
                           aDefault: integer;
                           aUseDefaults: boolean); reintroduce; overload;
    procedure WriteInteger(aSection: string;
                           aIdent: string;
                           aDefault: integer); reintroduce; overload;
    procedure WriteBool(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aIdent: string;
                        aDefault: boolean;
                        aUseDefaults: boolean); reintroduce; overload;
    procedure WriteBool(aSection: string;
                        aIdent: string;
                        aDefault: boolean); reintroduce; overload;
    constructor Create(AOwner: TComponent); override;
  published
  end;

  { TRegistrySource }

  TRegistrySource = class(TCustomRegistrySource)
  private
  protected
  public
  published
    property RootKey;
    property RootKeyForDefaults;
    property RootKeyForCommon;
    property Project;
    property Organisation;
    property RootForDefaults;
    property ReadDefaults;
    property WriteDefaults;
    property GUID;
  end;

procedure Register;

implementation

uses
  regutils;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegistrySource]);
end;

procedure TCustomRegistrySettings<_T>.SetRootKey(aRootKey: string);
begin
  FRootKey := IncludeTrailingPathDelimiter(aRootKey);

  if pos(PROJECT_TOKEN, FRootKey) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKey := StringReplace(FRootKey, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, FRootKey) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKey := StringReplace(FRootKey, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(GUID_TOKEN, FRootKey) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKey := StringReplace(FRootKey, GUID_TOKEN, FGUID, [rfReplaceAll]);

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootKeyForDefaults(
  aRootKeyForDefaults: string);
begin
  FRootKeyForDefaults := IncludeTrailingPathDelimiter(aRootKeyForDefaults);

  if pos(PROJECT_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(GUID_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, GUID_TOKEN, FGUID, [rfReplaceAll]);

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetSection(aSection: string);
begin
  FSection := aSection;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetIdent(aIdent: string);
begin
  FIdent := aIdent;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetReadDefaults(aReadDefaults: boolean);
begin
  FReadDefaults := aReadDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetWriteDefaults(aWriteDefaults: boolean);
begin
  FWriteDefaults := aWriteDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootForDefaults(
  aRootForDefaults: string);
begin
  FRootForDefaults := aRootForDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanRead(aCanRead: boolean);
begin
  FCanRead := aCanRead;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanWrite(aCanWrite: boolean);
begin
  FCanWrite := aCanWrite;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetDoWriteAdHoc(aDoWriteAdHoc: boolean);
begin
  FDoWriteAdHoc := aDoWriteAdHoc;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetDefault(aDefault: _T);
begin
  FDefault := aDefault;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetGUID(aGUID: string);
begin
  FGUID := aGUID;

  if pos(GUID_TOKEN, FRootKey) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKey := StringReplace(FRootKey, GUID_TOKEN, FGUID, [rfReplaceAll]);

  if pos(GUID_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, GUID_TOKEN, FGUID, [rfReplaceAll]);
end;

procedure TCustomRegistrySettings<_T>.SetProject(aProject: string);
begin
  FProject := aProject;

  if pos(PROJECT_TOKEN, FRootKey) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKey := StringReplace(FRootKey, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(PROJECT_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, PROJECT_TOKEN, FProject, [rfReplaceAll]);
end;

procedure TCustomRegistrySettings<_T>.SetOrganisation(aOrganisation: string);
begin
  FOrganisation := aOrganisation;

  if pos(ORGANISATION_TOKEN, FRootKey) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKey := StringReplace(FRootKey, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);
end;

function TCustomRegistrySource.GetRootKey: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKey);

  if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForDefaults: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForDefaults);

  if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForCommon: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForCommon);

  if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);

  if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);

  if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);

  Result := root_key;
end;

constructor TCustomRegistrySource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FReadDefaults := False;
  FWriteDefaults := False;
  FGUID := EmptyStr;
end;

function TCustomRegistrySource.ReadString(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: string;
  aUseDefaults: boolean): string;
var
  streg: TSTRegIniFile;
begin
  Result := '';
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        Result := streg.ReadStringCheck4Defaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadString(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := '';
        raise;
      end;
    end;
  finally
    streg.Free;
  end;
end;

function TCustomRegistrySource.ReadString(aSection: string;
  aIdent: string;
  aDefault: string): string;
begin
  try
    Result := ReadString(RootKey,
                RootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

function TCustomRegistrySource.ReadInteger(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: integer;
  aUseDefaults: boolean): integer;
var
  streg: TSTRegIniFile;
begin
  Result := aDefault;
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        Result := streg.ReadIntegerCheck4Defaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadInteger(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := aDefault;
        raise;
      end;
    end;
  finally
    streg.Free;
  end;
end;

function TCustomRegistrySource.ReadInteger(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
begin
  try
    Result := ReadInteger(RootKey,
                RootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
    begin
      Result := aDefault;
      raise;
    end;
  end;
end;

function TCustomRegistrySource.ReadBool(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: boolean;
  aUseDefaults: boolean): boolean;
var
  streg: TSTRegIniFile;
begin
  Result := aDefault;
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        Result := streg.ReadBoolCheck4Defaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadBool(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := aDefault;
        raise;
      end;
    end;
  finally
    streg.Free;
  end;
end;

function TCustomRegistrySource.ReadBool(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
begin
  try
    Result := ReadBool(RootKey,
                RootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
    begin
      Result := aDefault;
      raise;
    end;
  end;
end;

procedure TCustomRegistrySource.ReadSection(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aStrings: TStrings;
  aUseDefaults: boolean);
var
  streg: TSTRegIniFile;
begin
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        streg.ReadSectionCheck4Defaults(aSection, aStrings)
      else
        streg.ReadSection(aSection, aStrings);
    except
      on E: Exception do
        raise;
    end;
  finally
    streg.Free;
  end;
end;

procedure TCustomRegistrySource.ReadSection(aSection: string;
  aStrings: TStrings);
begin
  try
    try
      ReadSection(RootKey,
        RootKeyForDefaults,
        RootForDefaults,
        aSection,
        aStrings,
        ReadDefaults);
    except
      on E: Exception do
        raise;
    end;
  finally
  end;
end;

procedure TCustomRegistrySource.WriteString(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: string;
  aUseDefaults: boolean);
var
  streg: TSTRegIniFile;
begin
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        streg.WriteStringCheck4Defaults(aSection, aIdent, aDefault)
      else
        streg.WriteString(aSection, aIdent, aDefault);
    except
      on E: Exception do
        raise;
    end;
  finally
    streg.Free;
  end;
end;

procedure TCustomRegistrySource.WriteString(aSection: string;
  aIdent: string;
  aDefault: string);
begin
  try
    WriteString(RootKey,
      RootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.WriteInteger(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: integer;
  aUseDefaults: boolean);
var
  streg: TSTRegIniFile;
begin
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        streg.WriteIntegerCheck4Defaults(aSection, aIdent, aDefault)
      else
        streg.WriteInteger(aSection, aIdent, aDefault);
    except
      on E: Exception do
        raise;
    end;
  finally
    streg.Free;
  end;
end;

procedure TCustomRegistrySource.WriteInteger(aSection: string;
  aIdent: string;
  aDefault: integer);
begin
  try
    WriteInteger(RootKey,
      RootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults);
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

procedure TCustomRegistrySource.WriteBool(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: boolean;
  aUseDefaults: boolean);
var
  streg: TSTRegIniFile;
begin
  try
    try
      if aUseDefaults then
        streg := TSTRegIniFile.Create(aRootKey, aRootForDefaults, aRootKeyForDefaults)
      else
        streg := TSTRegIniFile.Create(aRootKey);

      if aUseDefaults then
        streg.WriteBoolCheck4Defaults(aSection, aIdent, aDefault)
      else
        streg.WriteBool(aSection, aIdent, aDefault);
    except
      on E: Exception do
        raise;
    end;
  finally
    streg.Free;
  end;
end;

procedure TCustomRegistrySource.WriteBool(aSection: string;
  aIdent: string;
  aDefault: boolean);
begin
  try
    WriteBool(RootKey,
      RootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

initialization
  {$I ..\package\registrysource.lrs}

end.
