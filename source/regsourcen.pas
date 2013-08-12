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

type
  TTokenType = (ttUnknown, ttProject, ttOrganisation, ttGUID);

const
  TokenTypeStr : array[TTokenType] of string = ('unknown', '%%PROJECT%%', '%%ORGANISATION%%', '%%GUID%%');

type

  { TCustomProperties }

  TCustomProperties = class(TPersistent)
  private
  protected
  public
    constructor Create(aOwner: TObject); virtual; abstract;
  published
  end;

  { TCustomRegistrySettings }
  TRegistrySettingString = string[255];

  TRegistrySettingKind = (rskUnknown, rskRootKey, rskRootKeyForDefault,
    rskSection, rskIdent, rskDefault, rskReadDefaults, rskWriteDefaults,
    rskRootForDefaults, rskCanRead, rskCanWrite, rskDoWriteAdHoc);

  TRegistrySettingValue = record
    case Kind: TRegistrySettingKind of
      rskRootKey: (RootKey: TRegistrySettingString);
      rskRootKeyForDefault: (RootKeyForDefault: TRegistrySettingString);
      rskSection: (Section: TRegistrySettingString);
      rskIdent: (Ident: TRegistrySettingString);
      rskDefault: (Default: TRegistrySettingString);
      rskReadDefaults: (ReadDefaults: boolean);
      rskWriteDefaults: (WriteDefaults: boolean);
      rskRootForDefaults: (RootForDefaults: TRegistrySettingString);
      rskCanRead: (CanRead: boolean);
      rskCanWrite: (CanWrite: boolean);
      rskDoWriteAdHoc: (DoWriteAdHoc: boolean);
  end;

  TOnRegistrySettingsChange = procedure(aSettingInfo: TRegistrySettingValue;
                                        var aIsOk: boolean) of object;

  TCustomRegistrySettings<_T> = class(TCustomProperties)
  private
    FOwner: TObject;
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
    FOnBeforeRegistrySettingChange: TOnRegistrySettingsChange;

    function ChangeTokenForKey(aToken: TTokenType;
                               aKey: string): string;
    function TriggerOnBeforeRegistrySettingChange(aKind: TRegistrySettingKind;
                                                  aValue: variant): boolean;
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
    function GetNamePath: string; override;
    constructor Create(aOwner: TObject); override;

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
    property OnBeforeRegistrySettingChange: TOnRegistrySettingsChange
      read FOnBeforeRegistrySettingChange
      write FOnBeforeRegistrySettingChange;
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

function _ChangeTokenForKey(aToken: string;
                            aTokenValue: string;
                            aKey: string): string;
procedure Register;

implementation

uses
  regutils;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegistrySource]);
end;


function _ChangeTokenForKey(aToken: string;
  aTokenValue: string;
  aKey: string): string;
begin
  if pos(aToken, aKey) > 0 then
    if ((Trim(aTokenValue) <> EmptyStr) and
      (aTokenValue <> aToken))then
      result := StringReplace(aKey, aToken, aTokenValue, [rfReplaceAll]);
end;

function TCustomRegistrySettings<_T>.ChangeTokenForKey(aToken: TTokenType;
  aKey: string): string;
var
  token: string;
  value_for_token: string;
begin
  Result := aKey;

  case aToken of
    ttProject: value_for_token := FProject;
    ttOrganisation: value_for_token := FOrganisation;
    ttGUID: value_for_token := FGUID;
  else
    Exit;
  end;

  token := TokenTypeStr[aToken];

  Result := _ChangeTokenForKey(token, value_for_token, aKey);
end;

function TCustomRegistrySettings<_T>.TriggerOnBeforeRegistrySettingChange(
  aKind: TRegistrySettingKind;
  aValue: variant): boolean;
var
  setting_value: TRegistrySettingValue;
  is_ok: boolean;
begin
  if Assigned(FOnBeforeRegistrySettingChange) then
  begin
    setting_value.kind := aKind;
    case aKind of
      rskUnknown:
      begin
        Result := False;
        Exit;
      end;
      rskRootKey: setting_value.RootKey := aValue;
      rskRootKeyForDefault: setting_value.RootKeyForDefault := aValue;
      rskSection: setting_value.Section := aValue;
      rskIdent: setting_value.Ident := aValue;
      rskDefault: setting_value.Default := aValue;
      rskReadDefaults: setting_value.ReadDefaults := aValue;
      rskWriteDefaults: setting_value.WriteDefaults := aValue;
      rskRootForDefaults: setting_value.RootForDefaults := aValue;
      rskCanRead: setting_value.CanRead := aValue;
      rskCanWrite: setting_value.CanWrite := aValue;
      rskDoWriteAdHoc: setting_value.DoWriteAdHoc := aValue;
    end;

    is_ok:= True;
    FOnBeforeRegistrySettingChange(setting_value, is_ok);
    if not is_ok then
      Exit;
  end;
end;

procedure TCustomRegistrySettings<_T>.SetRootKey(aRootKey: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootKey, aRootKey) then
    Exit;

  FRootKey := IncludeTrailingPathDelimiter(aRootKey);

  FRootKey := ChangeTokenForKey(ttProject, FRootKey);
  {if pos(PROJECT_TOKEN, FRootKey) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKey := StringReplace(FRootKey, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  FRootKey := ChangeTokenForKey(ttOrganisation, FRootKey);
  {if pos(ORGANISATION_TOKEN, FRootKey) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKey := StringReplace(FRootKey, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  FRootKey := ChangeTokenForKey(ttGUID, FRootKey);
  {if pos(GUID_TOKEN, FRootKey) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKey := StringReplace(FRootKey, GUID_TOKEN, FGUID, [rfReplaceAll]);}

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootKeyForDefaults(
  aRootKeyForDefaults: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootKeyForDefault, aRootKeyForDefaults) then
    Exit;

  FRootKeyForDefaults := IncludeTrailingPathDelimiter(aRootKeyForDefaults);

  FRootKeyForDefaults := ChangeTokenForKey(ttProject, FRootKeyForDefaults);
  {if pos(PROJECT_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  FRootKeyForDefaults := ChangeTokenForKey(ttOrganisation, FRootKeyForDefaults);
  {if pos(ORGANISATION_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  FRootKeyForDefaults := ChangeTokenForKey(ttGUID, FRootKeyForDefaults);
  {if pos(GUID_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, GUID_TOKEN, FGUID, [rfReplaceAll]);}

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetSection(aSection: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskSection, aSection) then
    Exit;

  FSection := aSection;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetIdent(aIdent: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskIdent, aIdent) then
    Exit;

  FIdent := aIdent;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetReadDefaults(aReadDefaults: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskReadDefaults, aReadDefaults) then
    Exit;

  FReadDefaults := aReadDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetWriteDefaults(aWriteDefaults: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskWriteDefaults, aWriteDefaults) then
    Exit;

  FWriteDefaults := aWriteDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootForDefaults(
  aRootForDefaults: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootForDefaults, aRootForDefaults) then
    Exit;

  FRootForDefaults := aRootForDefaults;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanRead(aCanRead: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskCanRead, aCanRead) then
    Exit;

  FCanRead := aCanRead;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanWrite(aCanWrite: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskCanWrite, aCanWrite) then
    Exit;

  FCanWrite := aCanWrite;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetDoWriteAdHoc(aDoWriteAdHoc: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDoWriteAdHoc, aDoWriteAdHoc) then
    Exit;

  FDoWriteAdHoc := aDoWriteAdHoc;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetDefault(aDefault: _T);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDefault, aDefault) then
    Exit;

  FDefault := aDefault;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetGUID(aGUID: string);
begin
  FGUID := aGUID;

  FRootKey := ChangeTokenForKey(ttGUID, FRootKey);
  {if pos(GUID_TOKEN, FRootKey) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKey := StringReplace(FRootKey, GUID_TOKEN, FGUID, [rfReplaceAll]);}

  FRootKeyForDefaults := ChangeTokenForKey(ttGUID, FRootKeyForDefaults);
  {if pos(GUID_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, GUID_TOKEN, FGUID, [rfReplaceAll]);}
end;

procedure TCustomRegistrySettings<_T>.SetProject(aProject: string);
begin
  FProject := aProject;

  FRootKey := ChangeTokenForKey(ttProject, FRootKey);
  {if pos(PROJECT_TOKEN, FRootKey) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKey := StringReplace(FRootKey, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  FRootKeyForDefaults := ChangeTokenForKey(ttProject, FRootKeyForDefaults);
  {if pos(PROJECT_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, PROJECT_TOKEN, FProject, [rfReplaceAll]);}
end;

procedure TCustomRegistrySettings<_T>.SetOrganisation(aOrganisation: string);
begin
  FOrganisation := aOrganisation;

  FRootKey := ChangeTokenForKey(ttOrganisation, FRootKey);
  {if pos(ORGANISATION_TOKEN, FRootKey) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKey := StringReplace(FRootKey, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  FRootKeyForDefaults := ChangeTokenForKey(ttOrganisation, FRootKeyForDefaults);
  {if pos(ORGANISATION_TOKEN, FRootKeyForDefaults) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      FRootKeyForDefaults := StringReplace(FRootKeyForDefaults, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}
end;

function TCustomRegistrySettings<_T>.GetNamePath: string;
begin
  Result:= FOwner.ClassName;
end;

constructor TCustomRegistrySettings<_T>.Create(aOwner: TObject);
begin
  FOwner := aOwner;
end;

function TCustomRegistrySource.GetRootKey: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKey);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  {if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  {if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);
  {if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);}

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForDefaults: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForDefaults);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  {if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  {if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);
  {if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);}

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForCommon: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForCommon);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  {if pos(PROJECT_TOKEN, root_key) > 0 then
    if ((Trim(FProject) <> EmptyStr) and
      (FProject <> PROJECT_TOKEN))then
      root_key := StringReplace(root_key, PROJECT_TOKEN, FProject, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  {if pos(ORGANISATION_TOKEN, root_key) > 0 then
    if ((Trim(FOrganisation) <> EmptyStr) and
      (FOrganisation <> ORGANISATION_TOKEN))then
      root_key := StringReplace(root_key, ORGANISATION_TOKEN, FOrganisation, [rfReplaceAll]);}

  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);
  {if pos(GUID_TOKEN, root_key) > 0 then
    if ((Trim(FGUID) <> EmptyStr) and
      (FGUID <> GUID_TOKEN))then
      root_key := StringReplace(root_key, GUID_TOKEN, FGUID, [rfReplaceAll]);}

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
