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
    {$ifndef fpdoc}
    function IsEqual(aKey: string;
                     aValue: string;
                     aCol: integer;
                     aRow: integer): boolean;
    procedure SetItems(aKey: string;
                       aValue: string;
                       aCol: integer;
                       aRow: integer);
    procedure Clear;
    {$endif}
  end;

  { TKeyValues }

  TKeyValues = record
    OldKeyItems: TKeyValueItems;
    NewKeyItems: TKeyValueItems;
    {$ifndef fpdoc}
    function IsEqual: boolean;
    function KeyValueDataChanged: boolean;
    function KeyDataChanged: boolean;
    function ValueDataChanged: boolean;
    procedure Clear;
    {$endif}
  end;

  { TInfoKind }

  TInfoKind = (ikItemIndex, ikInfo);

  { TListSourceKind }

  TListSourceKind = (lskUnknown, lskByKey, lskByValue, lskByKeyValue);

  { TCustomProperties }

  TCustomProperties = class(TPersistent)
  private
    FOwner: TPersistent;
    FCheckRTLAnsi: boolean;
  protected
    procedure _Initialize; virtual; abstract;
    procedure _Finalize; virtual; abstract;

    function GetOwnerComponentState: TComponentState;

    function GetOwner: TPersistent; override;
    procedure SetOwner(aOwner: TPersistent); dynamic;

    function GetOwnerIsLoading: boolean;
    property OwnerIsLoading: boolean
      read GetOwnerIsLoading;

    property CheckRTLAnsi: boolean
      read FCheckRTLAnsi
      write FCheckRTLAnsi;
  public
    property Owner: TPersistent
      read GetOwner;

    constructor Create(aOwner: TPersistent;
                       aCheckRTLAnsi: boolean = True); virtual; abstract;
  published
  end;

  { TRegistrySettingString }

  TRegistrySettingString = string[255];

  { TReadSource }

  TRegistryDataOrigin = (rdoUnknown, rdoGeneral, rdoCaption, rdoAll);

  { TRegistrySettingKind }

  TRegistrySettingKind = (rskUnknown, rskRootKey, rskRootKeyForDefault,
    rskSection, rskIdent, rskDefault, rskReadDefaults, rskWriteDefaults,
    rskRootForDefaults, rskCanRead, rskCanWrite, rskDoWriteAdHoc, rskDoSyncData,
    rskDoMergeData, rskSectionForCaption, rskIdentForCaption);

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
      rskDoMergeData: (DoMergeData: boolean);
      rskSectionForCaption: (SectionForCaption: TRegistrySettingString);
      rskIdentForCaption: (IdentForCaption: TRegistrySettingString);
  end;

  { TOnRegistrySettingsChange }

  TOnRegistrySettingsChange = procedure(aOldSettingInfo: TRegistrySettingValue;
                                        aNewSettingInfo: TRegistrySettingValue;
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
    {$ifndef fpdoc}
    procedure SetRootKeys(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aReadDefaults: boolean;
                          aWriteDefaults: boolean;
                          aRootForDefaults: string;
                          aProject: string;
                          aOrganisation: string;
                          aGUID: string);
    procedure Clear;
    {$endif}
  end;

  { TCustomCaptionSettings }

  TCustomCaptionSettings = class(TCustomProperties)
  private
    FSection: string;
    FIdent: string;
    FCaptionByRegistry: boolean;
    FOnBeforeCaptionSettingChange: TOnRegistrySettingsChange;
    FTriggerEvents: boolean;
    FOnChange: TNotifyEvent;

    function TriggerOnBeforeCaptionSettingChange(aKind: TRegistrySettingKind;
                                                 aValue: variant): boolean;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;

    function GetSection: string;
    procedure SetSection(aSection: string);
    function GetIdent: string;
    procedure SetIdent(aIdent: string);
    procedure SetCaptionByRegistry(aCaptionByRegistry: boolean);

    property Section: string
      read GetSection
      write SetSection;
    property Ident: string
      read GetIdent
      write SetIdent;
    property CaptionByRegistry: boolean
      read FCaptionByRegistry
      write SetCaptionByRegistry;
    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  public
    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create(aOwner: TPersistent;
                       aCheckRTLAnsi: boolean = True); override;
    destructor Destroy; override;

    property Owner;
  published
    property OnBeforeCaptionSettingChange: TOnRegistrySettingsChange
      read FOnBeforeCaptionSettingChange
      write FOnBeforeCaptionSettingChange;
  end;

  { TCustomRegistrySettings }
  
  {$ifndef fpdoc}
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
    FDoMergeData: boolean;

    function ChangeTokenForKey(aToken: TTokenType;
                               aKey: string): string;
    function TriggerOnBeforeRegistrySettingChange(aKind: TRegistrySettingKind;
                                                  aValue: variant): boolean;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;

    function GetRootKey: string;
    procedure SetRootKey(aRootKey: string);

    function GetRootKeyForDefaults: string;
    procedure SetRootKeyForDefaults(aRootKeyForDefaults: string);

    procedure SetReadDefaults(aReadDefaults: boolean);

    procedure SetWriteDefaults(aWriteDefaults: boolean);

    function GetRootForDefaults: string;
    procedure SetRootForDefaults(aRootForDefaults: string);

    procedure SetCanRead(aCanRead: boolean);
    procedure SetCanWrite(aCanWrite: boolean);

    procedure SetDoWriteAdHoc(aDoWriteAdHoc: boolean);

    function GetGUID: string;
    procedure SetGUID(aGUID: string);

    function GetProject: string;
    procedure SetProject(aProject: string);

    procedure SetGroupIndex(aGroupIndex: cardinal);
    procedure SetDoSyncData(aDoSyncData: boolean);
    procedure SetDoMergeData(aDoMergeData: boolean);

    function GetOrganisation: string;
    procedure SetOrganisation(aOrganisation: string);

    function GetDefault: _T;
    procedure SetDefault(aDefault: _T);
    property Default: _T
      read GetDefault
      write SetDefault;

    function GetSection: string;
    procedure SetSection(aSection: string);
    property Section: string
      read GetSection
      write SetSection;

    function GetIdent: string;
    procedure SetIdent(aIdent: string);
    property Ident: string
      read GetIdent
      write SetIdent;

    property DoMergeData: boolean
      read FDoMergeData
      write SetDoMergeData;
  public
    procedure GetRootKeys(var aRootKeys: TRootKeysStruct);
    procedure SetRootKeys(aRootKeys: TRootKeysStruct);
    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create(aOwner: TPersistent;
                       aCheckRTLAnsi: boolean = True); override;
    destructor Destroy; override;

    property Owner;

    property RootKey: string
      read GetRootKey
      write SetRootKey;

    property RootKeyForDefaults: string
      read GetRootKeyForDefaults
      write SetRootKeyForDefaults;

    property ReadDefaults: boolean
      read FReadDefaults
      write SetReadDefaults;

    property WriteDefaults: boolean
      read FWriteDefaults
      write SetWriteDefaults;

    property RootForDefaults: string
      read GetRootForDefaults
      write SetRootForDefaults;

    property GUID: string
      read GetGUID
      write SetGUID;

    property Project: string
      read GetProject
      write SetProject;

    property Organisation: string
      read GetOrganisation
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
  {$endif}

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
  Result := aKey;

  if pos(aToken, aKey) > 0 then
    if ((Trim(aTokenValue) <> EmptyStr) and
      (aTokenValue <> aToken))
    then
      Result := StringReplace(aKey, aToken, aTokenValue, [rfReplaceAll]);
end;

{ TCustomCaptionSettings }

function TCustomCaptionSettings.TriggerOnBeforeCaptionSettingChange(
  aKind: TRegistrySettingKind;
  aValue: variant): boolean;
var
  old_setting_value: TRegistrySettingValue;
  new_setting_value: TRegistrySettingValue;
  is_ok: boolean;
begin
  if (Assigned(FOnBeforeCaptionSettingChange) and
      FTriggerEvents and not OwnerIsLoading)
  then
  begin
    Result := False;

    old_setting_value.kind := aKind;
    case aKind of
      rskUnknown: Exit;
      rskSectionForCaption:
        old_setting_value.SectionForCaption := self.Section;
      rskIdentForCaption:
        old_setting_value.IdentForCaption := self.Ident;
    end;

    new_setting_value.kind := aKind;
    case aKind of
      rskUnknown: Exit;
      rskSectionForCaption:
        new_setting_value.SectionForCaption := aValue;
      rskIdentForCaption:
        new_setting_value.IdentForCaption := aValue;
    end;

    is_ok:= True;
    FOnBeforeCaptionSettingChange(old_setting_value,
                                  new_setting_value,
                                  is_ok);
    if not is_ok then
      Exit;

    Result := True;
  end
  else
    Result := True;
end;

procedure TCustomCaptionSettings._Initialize;
begin
  // wird als letztes im Create aufgerufen
end;

procedure TCustomCaptionSettings._Finalize;
begin
  // wird als erstes im Destroy aufgerufen
end;

function TCustomCaptionSettings.GetSection: string;
begin
  result := FSection;
end;

procedure TCustomCaptionSettings.SetSection(
  aSection: string);
begin
  if not TriggerOnBeforeCaptionSettingChange(rskSectionForCaption,
    aSection)
  then
    Exit;

  FSection := aSection;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

function TCustomCaptionSettings.GetIdent: string;
begin
  result := FIdent;
end;

procedure TCustomCaptionSettings.SetIdent(aIdent: string);
begin
  if not TriggerOnBeforeCaptionSettingChange(rskIdentForCaption, aIdent) then
    Exit;

  FIdent := aIdent;

  if Assigned(FOnChange) and FTriggerEvents then
    FOnChange(self);
end;

procedure TCustomCaptionSettings.SetCaptionByRegistry(
  aCaptionByRegistry: boolean);
begin
  if (FCaptionByRegistry <> aCaptionByRegistry) then
    FCaptionByRegistry := aCaptionByRegistry;

  if (FCaptionByRegistry and
    Assigned(FOnChange) and
    FTriggerEvents)
  then
    FOnChange(self);
end;

procedure TCustomCaptionSettings.BeginUpdate;
begin
  FTriggerEvents := False;
end;

procedure TCustomCaptionSettings.EndUpdate;
begin
  FTriggerEvents := True;
end;

constructor TCustomCaptionSettings.Create(aOwner: TPersistent;
  aCheckRTLAnsi: boolean = True);
begin
  SetOwner(aOwner);
  FTriggerEvents := True;
  CheckRTLAnsi := aCheckRTLAnsi;

  _Initialize;
end;

destructor TCustomCaptionSettings.Destroy;
begin
  _Finalize;

  CheckRTLAnsi := True;

  inherited Destroy;
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

function TCustomRegistrySettings<_T>.GetOrganisation: string;
begin
  Result := FOrganisation;
end;

function TCustomRegistrySettings<_T>.GetProject: string;
begin
  Result := FProject;
end;

function TCustomRegistrySettings<_T>.TriggerOnBeforeRegistrySettingChange(
  aKind: TRegistrySettingKind;
  aValue: variant): boolean;
var
  old_setting_value: TRegistrySettingValue;
  new_setting_value: TRegistrySettingValue;
  is_ok: boolean;
begin
  if (Assigned(FOnBeforeRegistrySettingChange) and
      FTriggerEvents and not OwnerIsLoading)
  then
  begin
    Result := False;

    old_setting_value.kind := aKind;
    case aKind of
      rskUnknown: Exit;
      rskRootKey:
        old_setting_value.RootKey := self.RootKey;
      rskRootKeyForDefault:
        old_setting_value.RootKeyForDefault := self.RootKeyForDefaults;
      rskSection:
        old_setting_value.Section := self.Section;
      rskIdent:
        old_setting_value.Ident := self.Ident;
      // auf Default sollte nicht zugegriffen werden
      //rskDefault:
      //  old_setting_value.Default := self.Default;
      rskDefault:
        old_setting_value.Default := EmptyStr;
      rskReadDefaults:
        old_setting_value.ReadDefaults := self.ReadDefaults;
      rskWriteDefaults:
        old_setting_value.WriteDefaults := self.WriteDefaults;
      rskRootForDefaults:
        old_setting_value.RootForDefaults := self.RootForDefaults;
      rskCanRead:
        old_setting_value.CanRead := self.CanRead;
      rskCanWrite:
        old_setting_value.CanWrite := self.CanWrite;
      rskDoWriteAdHoc:
        old_setting_value.DoWriteAdHoc := self.DoWriteAdHoc;
      rskDoSyncData:
        old_setting_value.DoSyncData := self.DoSyncData;
      rskDoMergeData:
        old_setting_value.DoMergeData := self.DoMergeData;
    end;

    new_setting_value.kind := aKind;
    case aKind of
      rskUnknown: Exit;
      rskRootKey:
        new_setting_value.RootKey := aValue;
      rskRootKeyForDefault:
        new_setting_value.RootKeyForDefault := aValue;
      rskSection:
        new_setting_value.Section := aValue;
      rskIdent:
        new_setting_value.Ident := aValue;
      // auf Default sollte nicht zugegriffen werden
      //rskDefault:
      //  new_setting_value.Default := aValue;
      rskReadDefaults:
        new_setting_value.ReadDefaults := aValue;
      rskWriteDefaults:
        new_setting_value.WriteDefaults := aValue;
      rskRootForDefaults:
        new_setting_value.RootForDefaults := aValue;
      rskCanRead:
        new_setting_value.CanRead := aValue;
      rskCanWrite:
        new_setting_value.CanWrite := aValue;
      rskDoWriteAdHoc:
        new_setting_value.DoWriteAdHoc := aValue;
      rskDoSyncData:
        new_setting_value.DoSyncData := aValue;
      rskDoMergeData:
        new_setting_value.DoMergeData := aValue;
    end;

    is_ok:= True;
    FOnBeforeRegistrySettingChange(old_setting_value,
                                   new_setting_value,
                                   is_ok);
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

  FRootKey := FRootKey;

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

  FRootKeyForDefaults := FRootKeyForDefaults;

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

procedure TCustomRegistrySettings<_T>.SetDoMergeData(aDoMergeData: boolean);
begin
  if not TriggerOnBeforeRegistrySettingChange(rskDoMergeData, aDoMergeData) then
    Exit;

  FDoMergeData := aDoMergeData;
end;

function TCustomRegistrySettings<_T>.GetSection: string;
begin
  Result := FSection;
end;

function TCustomRegistrySettings<_T>.GetDefault: _T;
begin
  Result := FDefault;
end;

function TCustomRegistrySettings<_T>.GetIdent: string;
begin
  Result := FIdent;
end;

function TCustomRegistrySettings<_T>.GetGUID: string;
begin
  Result := FGUID;
end;

function TCustomRegistrySettings<_T>.GetRootKey: string;
begin
  Result := FRootKey;
end;

function TCustomRegistrySettings<_T>.GetRootKeyForDefaults: string;
begin
  Result := FRootKeyForDefaults;
end;

function TCustomRegistrySettings<_T>.GetRootForDefaults: string;
begin
  Result := FRootForDefaults;
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
  RootKey := aRootKeys.RootKey;
  RootKeyForDefaults := aRootKeys.RootKeyForDefaults;
  ReadDefaults := aRootKeys.ReadDefaults;
  WriteDefaults := aRootKeys.WriteDefaults;
  RootForDefaults := aRootKeys.RootForDefaults;
  Project := aRootKeys.Project;
  Organisation := aRootKeys.Organisation;
  GUID := aRootKeys.GUID;
end;

procedure TCustomRegistrySettings<_T>.BeginUpdate;
begin
  FTriggerEvents := False;
end;

procedure TCustomRegistrySettings<_T>.EndUpdate;
begin
  FTriggerEvents := True;
end;

constructor TCustomRegistrySettings<_T>.Create(aOwner: TPersistent;
  aCheckRTLAnsi: boolean = True);
begin
  SetOwner(aOwner);
  FTriggerEvents := True;
  FDoSyncData := False;
  FGroupIndex := 0;
  CheckRTLAnsi := ACheckRTLAnsi;

  _Initialize;
end;

destructor TCustomRegistrySettings<_T>.Destroy;
begin
  _Finalize;

  CheckRTLAnsi := True;
  inherited;
end;

end.

