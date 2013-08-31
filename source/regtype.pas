unit regtype;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  regconst;

type

  { TKeyValueItems }

  TKeyValueItems = record
    Found: boolean;
    Key: string;
    Value: string;
    Col: integer;
    Row: integer;

    function IsEqual(aKey: string;
                     aValue: string;
                     aCol: integer;
                     aRow: integer): boolean;
    procedure SetItems(aKey: string;
                       aValue: string;
                       aCol: integer;
                       aRow: integer);
    procedure Clear;
  end;

  { TKeyValues }

  TKeyValues = record
    OldKeyItems: TKeyValueItems;
    NewKeyItems: TKeyValueItems;

    function IsEqual: boolean;
    function KeyValueDataChanged: boolean;
    function KeyDataChanged: boolean;
    function ValueDataChanged: boolean;
    procedure Clear;
  end;

  { TInfoKind }

  TInfoKind = (ikItemIndex, ikInfo);

  { TListSourceKind }

  TListSourceKind = (lskUnknown, byKey, byValue, Both);

  { TCustomProperties }

  TCustomProperties = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    procedure _Initialize; virtual; abstract;
    procedure _Finalize; virtual; abstract;

    function GetOwnerComponentState: TComponentState;

    function GetOwner: TPersistent; override;
    procedure SetOwner(aOwner: TPersistent); dynamic;

    function GetOwnerIsLoading: boolean;
    property OwnerIsLoading: boolean
      read GetOwnerIsLoading;
  public
    property Owner: TPersistent
      read GetOwner;

    constructor Create(aOwner: TPersistent); virtual; abstract;
  published
  end;

  { TRegistrySettingString }

  TRegistrySettingString = string[255];

  { TRegistrySettingKind }

  TRegistrySettingKind = (rskUnknown, rskRootKey, rskRootKeyForDefault,
    rskSection, rskIdent, rskDefault, rskReadDefaults, rskWriteDefaults,
    rskRootForDefaults, rskCanRead, rskCanWrite, rskDoWriteAdHoc, rskDoSyncData);

  { TRegistrySettingValue }

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
      rskDoSyncData: (DoSyncData: boolean);
  end;

  { TOnRegistrySettingsChange }

  TOnRegistrySettingsChange = procedure(aSettingInfo: TRegistrySettingValue;
                                        var aIsOk: boolean) of object;

  { TRootKeysStruct }

  TRootKeysStruct = record
    Found: boolean;
    RootKey: string;
    RootKeyForDefaults: string;
    ReadDefaults: boolean;
    WriteDefaults: boolean;
    RootForDefaults: string;
    Project: string;
    Organisation: string;
    GUID: string;

    procedure SetRootKeys(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aReadDefaults: boolean;
                          aWriteDefaults: boolean;
                          aRootForDefaults: string;
                          aProject: string;
                          aOrganisation: string;
                          aGUID: string);
    procedure Clear;
  end;

  { TCustomRegistrySettings }

  TCustomRegistrySettings<_T> = class(TCustomProperties)
  private
    FRootKey: string;
    FRootKeyForDefaults: string;
    FSection: string;
    FIdent: string;
    FDefault: _T;
    FReadDefaults: boolean;
    FWriteDefaults: boolean;
    FRootForDefaults: string;
    FCanRead: boolean;
    FCanWrite: boolean;
    FDoWriteAdHoc: boolean;
    FGroupIndex: cardinal;
    FDoSyncData: boolean;
    FProject: string;
    FOrganisation: string;
    FGUID: string;
    FOnChange: TNotifyEvent;
    FOnBeforeRegistrySettingChange: TOnRegistrySettingsChange;
    FTriggerEvents: boolean;

    function ChangeTokenForKey(aToken: TTokenType;
                               aKey: string): string;
    function TriggerOnBeforeRegistrySettingChange(aKind: TRegistrySettingKind;
                                                  aValue: variant): boolean;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;

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
    procedure SetGroupIndex(aGroupIndex: cardinal);
    procedure SetDoSyncData(aDoSyncData: boolean);

    property Default: _T
      read FDefault
      write SetDefault;
    property Section: string
      read FSection
      write SetSection;
    property Ident: string
      read FIdent
      write SetIdent;
  public
    procedure GetRootKeys(var aRootKeys: TRootKeysStruct);
    procedure SetRootKeys(aRootKeys: TRootKeysStruct);
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create(aOwner: TPersistent); override;
    destructor Destroy; override;

    property Owner;

    property RootKey: string
      read FRootKey
      write SetRootKey;
    property RootKeyForDefaults: string
      read FRootKeyForDefaults
      write SetRootKeyForDefaults;
    property ReadDefaults: boolean
      read FReadDefaults
      write SetReadDefaults;
    property WriteDefaults: boolean
      read FWriteDefaults
      write SetWriteDefaults;
    property RootForDefaults: string
      read FRootForDefaults
      write SetRootForDefaults;

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
    property CanRead: boolean
      read FCanRead
      write SetCanRead;
    property CanWrite: boolean
      read FCanWrite
      write SetCanWrite;
    property DoWriteAdHoc: boolean
      read FDoWriteAdHoc
      write SetDoWriteAdHoc;
    property GroupIndex: cardinal
      read FGroupIndex
      write SetGroupIndex;
    property DoSyncData: boolean
      read FDoSyncData
      write SetDoSyncData;
    property OnBeforeRegistrySettingChange: TOnRegistrySettingsChange
      read FOnBeforeRegistrySettingChange
      write FOnBeforeRegistrySettingChange;
  end;

function _ChangeTokenForKey(aToken: string;
                            aTokenValue: string;
                            aKey: string): string;

implementation

uses
  SysUtils;

function _ChangeTokenForKey(aToken: string;
  aTokenValue: string;
  aKey: string): string;
begin
  if pos(aToken, aKey) > 0 then
    if ((Trim(aTokenValue) <> EmptyStr) and
      (aTokenValue <> aToken))then
      result := StringReplace(aKey, aToken, aTokenValue, [rfReplaceAll]);
end;

{ TKeyValues }

function TKeyValues.IsEqual: boolean;
begin
  Result := ((OldKeyItems.Key=NewKeyItems.Key) and
             (OldKeyItems.Value=NewKeyItems.Value) and
             (OldKeyItems.Col=NewKeyItems.Col) and
             (OldKeyItems.Row=NewKeyItems.Row));
end;

function TKeyValues.KeyValueDataChanged: boolean;
begin
  Result := ((OldKeyItems.Key<>NewKeyItems.Key) or
             (OldKeyItems.Value<>NewKeyItems.Value))
            and
            ((OldKeyItems.Col=NewKeyItems.Col) and
             (OldKeyItems.Row=NewKeyItems.Row));
end;

function TKeyValues.KeyDataChanged: boolean;
begin
  Result := False;
  if KeyValueDataChanged then
    Result := (OldKeyItems.Key<>NewKeyItems.Key);
end;

function TKeyValues.ValueDataChanged: boolean;
begin
  Result := False;
  if KeyValueDataChanged then
    Result := (OldKeyItems.Value<>NewKeyItems.Value);
end;

procedure TKeyValues.Clear;
begin
  OldKeyItems.Clear;
  NewKeyItems.Clear;
end;

{ TKeyValueItems }

function TKeyValueItems.IsEqual(aKey: string;
  aValue: string;
  aCol: integer;
  aRow: integer): boolean;
begin
  Result := ((Key=aKey) and (Value=aValue) and (Col=aCol) and (Row=aRow)) and Found;
end;

procedure TKeyValueItems.SetItems(aKey: string;
  aValue: string;
  aCol: integer;
  aRow: integer);
begin
  Found := True;
  Key := aKey;
  Value := aValue;
  Col := aCol;
  Row := aRow;
end;

procedure TKeyValueItems.Clear;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TRootKeysStruct }

procedure TRootKeysStruct.SetRootKeys(aRootKey: string;
  aRootKeyForDefaults: string;
  aReadDefaults: boolean;
  aWriteDefaults: boolean;
  aRootForDefaults: string;
  aProject: string;
  aOrganisation: string;
  aGUID: string);
begin
  Found := True;
  RootKey := aRootKey;
  RootKeyForDefaults := aRootKeyForDefaults;
  ReadDefaults := aReadDefaults;
  WriteDefaults := aWriteDefaults;
  RootForDefaults := aRootForDefaults;
  Project := aProject;
  Organisation := aOrganisation;
  GUID := aGUID;
end;

procedure TRootKeysStruct.Clear;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TCustomProperties }

function TCustomProperties.GetOwnerComponentState: TComponentState;
begin
  if Owner is TComponent then
    Result := TComponent(Owner).ComponentState
  else
    Result := [];
end;

function TCustomProperties.GetOwner: TPersistent;
begin
  Result:= FOwner;
end;

procedure TCustomProperties.SetOwner(aOwner: TPersistent);
begin
  FOwner := aOwner;
end;

function TCustomProperties.GetOwnerIsLoading: boolean;
begin
  Result := (csloading in GetOwnerComponentState);
end;

{ TCustomRegistrySettings }

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
    Result := False;

    if OwnerIsLoading then
      Exit;

    setting_value.kind := aKind;
    case aKind of
      rskUnknown: Exit;
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
      rskDoSyncData: setting_value.DoSyncData := aValue;
    end;

    is_ok:= True;
    FOnBeforeRegistrySettingChange(setting_value, is_ok);
    if not is_ok then
      Exit;

    Result := True;
  end
  else
    Result := True;
end;

procedure TCustomRegistrySettings<_T>._Initialize;
begin
  // In Ableitungen verwenden
end;

procedure TCustomRegistrySettings<_T>._Finalize;
begin
  // In Ableitungen verwenden
end;

procedure TCustomRegistrySettings<_T>.SetRootKey(aRootKey: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootKey, aRootKey) then
    Exit;

  FRootKey := IncludeTrailingPathDelimiter(aRootKey);

  FRootKey := ChangeTokenForKey(ttProject, FRootKey);
  FRootKey := ChangeTokenForKey(ttOrganisation, FRootKey);
  FRootKey := ChangeTokenForKey(ttGUID, FRootKey);

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootKeyForDefaults(
  aRootKeyForDefaults: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootKeyForDefault, aRootKeyForDefaults) then
    Exit;

  FRootKeyForDefaults := IncludeTrailingPathDelimiter(aRootKeyForDefaults);

  FRootKeyForDefaults := ChangeTokenForKey(ttProject, FRootKeyForDefaults);
  FRootKeyForDefaults := ChangeTokenForKey(ttOrganisation, FRootKeyForDefaults);
  FRootKeyForDefaults := ChangeTokenForKey(ttGUID, FRootKeyForDefaults);

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetSection(aSection: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskSection, aSection) then
    Exit;

  FSection := aSection;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetIdent(aIdent: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskIdent, aIdent) then
    Exit;

  FIdent := aIdent;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetReadDefaults(aReadDefaults: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskReadDefaults, aReadDefaults) then
    Exit;

  FReadDefaults := aReadDefaults;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetWriteDefaults(aWriteDefaults: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskWriteDefaults, aWriteDefaults) then
    Exit;

  FWriteDefaults := aWriteDefaults;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetRootForDefaults(
  aRootForDefaults: string);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskRootForDefaults, aRootForDefaults) then
    Exit;

  FRootForDefaults := aRootForDefaults;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanRead(aCanRead: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskCanRead, aCanRead) then
    Exit;

  FCanRead := aCanRead;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetCanWrite(aCanWrite: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskCanWrite, aCanWrite) then
    Exit;

  FCanWrite := aCanWrite;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetDoWriteAdHoc(aDoWriteAdHoc: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDoWriteAdHoc, aDoWriteAdHoc) then
    Exit;

  FDoWriteAdHoc := aDoWriteAdHoc;
end;

procedure TCustomRegistrySettings<_T>.SetDefault(aDefault: _T);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDefault, aDefault) then
    Exit;

  FDefault := aDefault;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomRegistrySettings<_T>.SetGUID(aGUID: string);
begin
  FGUID := aGUID;

  FRootKey := ChangeTokenForKey(ttGUID, FRootKey);
  FRootKeyForDefaults := ChangeTokenForKey(ttGUID, FRootKeyForDefaults);
end;

procedure TCustomRegistrySettings<_T>.SetProject(aProject: string);
begin
  FProject := aProject;

  FRootKey := ChangeTokenForKey(ttProject, FRootKey);
  FRootKeyForDefaults := ChangeTokenForKey(ttProject, FRootKeyForDefaults);
end;

procedure TCustomRegistrySettings<_T>.SetOrganisation(aOrganisation: string);
begin
  FOrganisation := aOrganisation;

  FRootKey := ChangeTokenForKey(ttOrganisation, FRootKey);
  FRootKeyForDefaults := ChangeTokenForKey(ttOrganisation, FRootKeyForDefaults);
end;

procedure TCustomRegistrySettings<_T>.SetGroupIndex(aGroupIndex: cardinal);
begin
  FGroupIndex := aGroupIndex;
end;

procedure TCustomRegistrySettings<_T>.SetDoSyncData(aDoSyncData: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDoSyncData, aDoSyncData) then
    Exit;

  FDoSyncData := aDoSyncData;
end;

procedure TCustomRegistrySettings<_T>.GetRootKeys(var aRootKeys: TRootKeysStruct);
begin
  aRootKeys.RootKey := FRootKey;
  aRootKeys.RootKeyForDefaults := FRootKeyForDefaults;
  aRootKeys.ReadDefaults := FReadDefaults;
  aRootKeys.WriteDefaults := FWriteDefaults;
  aRootKeys.RootForDefaults := FRootForDefaults;
  aRootKeys.Project := FProject;
  aRootKeys.Organisation := FOrganisation;
  aRootKeys.GUID := FGUID;
end;

procedure TCustomRegistrySettings<_T>.SetRootKeys(aRootKeys: TRootKeysStruct);
begin
  FRootKey := aRootKeys.RootKey;
  FRootKeyForDefaults := aRootKeys.RootKeyForDefaults;
  FReadDefaults := aRootKeys.ReadDefaults;
  FWriteDefaults := aRootKeys.WriteDefaults;
  FRootForDefaults := aRootKeys.RootForDefaults;
  FProject := aRootKeys.Project;
  FOrganisation := aRootKeys.Organisation;
  FGUID := aRootKeys.GUID;
end;

procedure TCustomRegistrySettings<_T>.BeginUpdate;
begin
  FTriggerEvents := False;
end;

procedure TCustomRegistrySettings<_T>.EndUpdate;
begin
  FTriggerEvents := True;
end;

constructor TCustomRegistrySettings<_T>.Create(aOwner: TPersistent);
begin
  SetOwner(aOwner);
  FTriggerEvents := True;
  FDoSyncData := False;
  FGroupIndex := 0;

  _Initialize;
end;

destructor TCustomRegistrySettings<_T>.Destroy;
begin
  _Finalize;

  inherited;
end;

end.

