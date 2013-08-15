unit regtype;

{S+}

interface

uses
  Classes,
  regconst;

type

  { TCustomProperties }

  TCustomProperties = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
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
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create(aOwner: TPersistent); override;

    property Owner;
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
    if OwnerIsLoading then
    begin
      Result := False;
      Exit;
    end;

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

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
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
  FDoSyncData := aDoSyncData;
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
end;

end.

