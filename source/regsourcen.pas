unit regsourcen;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  LResources,
  regconst,
  regtype;

type

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
    FSourceKind: TListSourceKind;
  protected
  public
  published
    property ItemsByRegistry: boolean
      read FItemsByRegistry
      write FItemsByRegistry;
    property ListSection: string
      read FListSection
      write FListSection;
    property SourceKind: TListSourceKind
      read FSourceKind
      write FSourceKind;
  end;

  { TRegistrySettingsCheckedList }

  TRegistrySettingsCheckedList = class(TRegistrySettingsIntegerDefault)
  private
    FItemsByRegistry: boolean;
    FListSection: string;
    FSourceKind: TListSourceKind;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;
  public
    property SourceKind: TListSourceKind
      read FSourceKind;
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
    FClientList: TStrings;
    FDoSyncData: boolean;
    FPrefereStrings: boolean;
    FEditClientRootKeys: boolean;

    procedure OnSyncData(aGroupIndex: Cardinal);
    procedure DeliverMessage(aMessageConst: cardinal;
                             aClientName: string = '';
                             aGroupIndex: cardinal = 0;
                             aWParam: integer = 0);
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
    property DoSyncData: boolean
      read FDoSyncData
      write FDoSyncData;
    property PrefereStrings: boolean
      read FPrefereStrings
      write FPrefereStrings;
    function GetClientCount: integer;
    property ClientCount: integer
      read GetClientCount;
    property EditClientRootKeys: boolean
      read FEditClientRootKeys
      write FEditClientRootKeys;
  public
    procedure RenameClient(aOldName: TComponentName;
                           aNewName: TComponentName);
    procedure ShowClientEditDialog(aClientName: string);
    function GetClientByName(aClientName: string): TComponent;
    function GetClientNameByIndex(aIndex: integer) : string;
    procedure FreeRegistrySource(aClientName: string = '';
                                 aGroupIndex: cardinal = 0);
    procedure RefreshWriteAdHocProperty(aDoWriteAdHoc : boolean = True;
                                        aClientName: string = '';
                                        aGroupIndex: cardinal = 0);
    procedure RefreshSyncProperty(aDoSync : boolean = True;
                                  aClientName: string = '';
                                  aGroupIndex: cardinal = 0);
    procedure RefreshSettings(aClientName: string = '');
    procedure RefreshControlData(aClientName: string = '';
                                 aGroupIndex: cardinal = 0);
    procedure RegisterControl(aControl: TComponent);
    procedure UnRegisterControl(aControl: TComponent);
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
                          aUseDefaults: boolean;
                          aListSource: TListSourceKind = byKey); reintroduce; overload;
    procedure ReadSection(aSection: string;
                          aStrings: TStrings); reintroduce; overload;
    procedure WriteString(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aRootForDefaults: string;
                          aSection: string;
                          aIdent: string;
                          aDefault: string;
                          aUseDefaults: boolean;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteString(aSection: string;
                          aIdent: string;
                          aDefault: string;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteInteger(aRootKey: string;
                           aRootKeyForDefaults: string;
                           aRootForDefaults: string;
                           aSection: string;
                           aIdent: string;
                           aDefault: integer;
                           aUseDefaults: boolean;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteInteger(aSection: string;
                           aIdent: string;
                           aDefault: integer;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteBool(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aIdent: string;
                        aDefault: boolean;
                        aUseDefaults: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteBool(aSection: string;
                        aIdent: string;
                        aDefault: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TRegistrySource }

  TRegistrySource = class(TCustomRegistrySource)
  private
  protected
  public
    property ClientCount;
    property EditClientRootKeys;
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
    property DoSyncData;
    property PrefereStrings;
  end;

procedure Register;

implementation

uses
  regutils,
  regmsg,
  LMessages,
  PropEdits,
  ComponentEditors,
  regpropedits;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegistrySource]);
  RegisterComponentEditor(TRegistrySource, TRegistrySourceComponentEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsStringDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsIntegerDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsBooleanDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsList, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
end;

{ TRegistrySettingsCheckedList }

procedure TRegistrySettingsCheckedList._Initialize;
begin
  FSourceKind := Both;

  inherited;
end;

procedure TRegistrySettingsCheckedList._Finalize;
begin
  FSourceKind := lskUnknown;

  inherited;
end;

{ TCustomRegistrySource }

procedure TCustomRegistrySource.OnSyncData(aGroupIndex: cardinal = 0);
begin
  RefreshControlData('', aGroupIndex);
end;

procedure TCustomRegistrySource.DeliverMessage(aMessageConst: cardinal;
  aClientName: string = '';
  aGroupIndex: cardinal = 0;
  aWParam: integer = 0);
var
  anz: integer;
  msg: TLMessage;
begin
  // löst Hint: Local variable "msg" does not seem to be initialized
  msg.Result:=0;
  FillChar(msg, SizeOf(msg), #0);
  msg.Msg := aMessageConst;
  msg.lParam:=AGroupIndex;
  msg.wParam:=aWParam;
  for anz := FClientList.count-1 downto 0 do
  begin
    if Assigned(FClientList.Objects[anz]) then
      if FClientList.Objects[anz] is TComponent then
      begin
        if (AClientName = EmptyStr) then
          TComponent(FClientList.Objects[anz]).Dispatch(msg)
        else
          if (LowerCase(AClientName) = LowerCase(TComponent(FClientList.Objects[anz]).Name)) then
            TComponent(FClientList.Objects[anz]).Dispatch(msg);
      end;
  end;
end;

function TCustomRegistrySource.GetClientCount: integer;
begin
  if Assigned(FClientList) then
    Result := FClientList.Count
  else
    Result := 0;
end;

procedure TCustomRegistrySource.RenameClient(aOldName: TComponentName;
  aNewName: TComponentName);
var
  index: integer;
begin
  if Assigned(FClientList) then
    if (FClientList.Count > 0) then
    begin
      index := FClientList.IndexOf(aOldName);
      if (index <> -1) then
        FClientList.Strings[index] := aNewName;
    end;
end;

procedure TCustomRegistrySource.ShowClientEditDialog(aClientName: string);
var
  w_param: integer;
begin
  if FEditClientRootKeys then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SHOW_EDITDIALOG, aClientName, 0, w_param);
end;

function TCustomRegistrySource.GetClientByName(aClientName: string): TComponent;
var
  index: integer;
begin
  Result := nil;
  if (ClientCount > 0) then
  begin
    index := FClientList.IndexOf(aClientName);
    if (index <> -1) then
      if Assigned(FClientList.Objects[index]) then
        if (FClientList.Objects[index] is TComponent) then
          Result := TComponent(FClientList.Objects[index]);
  end;
end;

function TCustomRegistrySource.GetClientNameByIndex(aIndex: integer): string;
begin
  if ((ClientCount > 0) and ((ClientCount-1) >= aIndex)) then
    Result := FClientList.Strings[aIndex];
end;

procedure TCustomRegistrySource.FreeRegistrySource(aClientName: string = '';
  aGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE, aClientName, aGroupIndex);
end;

procedure TCustomRegistrySource.RefreshWriteAdHocProperty(aDoWriteAdHoc: boolean = True;
  aClientName: string = '';
  aGroupIndex: cardinal = 0);
var
  w_param: integer;
begin
  if aDoWriteAdHoc then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SET_WRITEADHOC, aClientName, aGroupIndex, w_param);
end;

procedure TCustomRegistrySource.RefreshSyncProperty(aDoSync: boolean = True;
  aClientName: string = '';
  aGroupIndex: cardinal = 0);
var
  w_param: integer;
begin
  if aDoSync then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SET_SYNC, aClientName, aGroupIndex, w_param);
end;

procedure TCustomRegistrySource.RefreshSettings(aClientName: string = '');
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_SETTINGS, aClientName);
end;

procedure TCustomRegistrySource.RefreshControlData(aClientName: string = '';
  aGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_DATA, aClientName, aGroupIndex);
end;

procedure TCustomRegistrySource.RegisterControl(aControl: TComponent);
var
  index: integer;
  name: string;
begin
  if Assigned(FClientList) then
  begin
    name := AControl.Name;
    index := FClientList.IndexOf(name);
    if (index = -1) then
      FClientList.AddObject(name, AControl);
  end;
end;

procedure TCustomRegistrySource.UnRegisterControl(aControl: TComponent);
var
  index: integer;
  name: string;
begin
  if Assigned(FClientList) then
  begin
    name := AControl.Name;
    index := FClientList.IndexOf(name);
    if (index <> -1) then
      FClientList.Delete(index);
  end;
end;

function TCustomRegistrySource.GetRootKey: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKey);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForDefaults: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForDefaults);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := root_key;
end;

function TCustomRegistrySource.GetRootKeyForCommon: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForCommon);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := root_key;
end;

constructor TCustomRegistrySource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRootKey := EmptyStr;
  FRootKeyForDefaults := EmptyStr;
  FRootKeyForCommon := EmptyStr;
  FProject := EmptyStr;
  FOrganisation := EmptyStr;
  FRootForDefaults := EmptyStr;
  FReadDefaults := False;
  FWriteDefaults := False;
  FGUID := EmptyStr;
  FDoSyncData := False;
  FPrefereStrings := False;
  FEditClientRootKeys := False;

  FClientList := TStringList.Create;
end;

destructor TCustomRegistrySource.Destroy;
begin
  FreeRegistrySource;

  if Assigned(FClientList) then
    FreeAndNil(FClientList);

  inherited Destroy;
end;

function TCustomRegistrySource.ReadString(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: string;
  aUseDefaults: boolean): string;
var
  streg: TDataByCurrentUser;
begin
  Result := '';

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        Result := streg.ReadStringCheckForDefaults(aSection, aIdent, aDefault)
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
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadString(aSection: string;
  aIdent: string;
  aDefault: string): string;
begin
  try
    Result := ReadString(GetRootKey,
                GetRootKeyForDefaults,
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
  streg: TDataByCurrentUser;
begin
  Result := aDefault;

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(
               aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        Result := streg.ReadIntegerCheckForDefaults(aSection, aIdent, aDefault)
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
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadInteger(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
begin
  try
    Result := ReadInteger(GetRootKey,
                GetRootKeyForDefaults,
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
  streg: TDataByCurrentUser;
begin
  Result := aDefault;

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        Result := streg.ReadBoolCheckForDefaults(aSection, aIdent, aDefault)
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
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadBool(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
begin
  try
    Result := ReadBool(GetRootKey,
                GetRootKeyForDefaults,
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
  aUseDefaults: boolean;
  aListSource: TListSourceKind = byKey);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
      begin
        case aListSource of
          byKey: streg.ReadSectionCheckForDefaults(aSection, aStrings);
          byValue: streg.ReadSectionValuesOnlyForDefaults(aSection, aStrings);
          Both: streg.ReadSectionValuesCheckForDefaults(aSection, aStrings);
        else
          aStrings.clear;
        end;
      end
      else
      begin
        case aListSource of
          byKey: streg.ReadSection(aSection, aStrings);
          byValue: streg.ReadSectionValuesOnly(aSection, aStrings);
          Both: streg.ReadSectionValuesEx(aSection, aStrings);
        else
          aStrings.clear;
        end;
      end;
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.ReadSection(aSection: string;
  aStrings: TStrings);
begin
  try
    try
      ReadSection(GetRootKey,
        GetRootKeyForDefaults,
        RootForDefaults,
        aSection,
        aStrings,
        ReadDefaults,
        byKey);
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
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        streg.WriteStringCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteString(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteString(aSection: string;
  aIdent: string;
  aDefault: string;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteString(GetRootKey,
      GetRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
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
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        streg.WriteIntegerCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteInteger(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteInteger(aSection: string;
  aIdent: string;
  aDefault: integer;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteInteger(GetRootKey,
      GetRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
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
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings)
  else
    streg := TDataByCurrentUser.Create(aRootKey);

  try
    try
      if aUseDefaults then
        streg.WriteBoolCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteBool(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteBool(aSection: string;
  aIdent: string;
  aDefault: boolean;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteBool(GetRootKey,
      GetRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

initialization
  {$I ..\package\registrysource.lrs}

end.
