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
  PropEdits,
  regconst,
  regtype,
  regpropedits;

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
    FClientList: TStrings;
    FDoSyncData: boolean;

    procedure OnSyncData(aGroupIndex: Cardinal);
    procedure DeliverMessage(aMessageConst: cardinal;
                             aClientName: string = '';
                             AGroupIndex: cardinal = 0);
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
  public
    procedure RefreshSettings(AClientName: string = '');
    procedure RefreshControlData(AClientName: string = '';
                                 AGroupIndex: cardinal = 0);
    procedure RegisterControl(AControl: TComponent);
    procedure UnRegisterControl(AControl: TComponent);
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
  end;

procedure Register;

implementation

uses
  regutils,
  regmsg,
  LMessages;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegistrySource]);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsStringDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsIntegerDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsBooleanDefault, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange), TRegistrySettingsList, 'OnBeforeRegistrySettingChange', TRegistrySettingsPropertyEditor);
end;

{ TCustomRegistrySource }

procedure TCustomRegistrySource.OnSyncData(aGroupIndex: cardinal = 0);
begin
  RefreshControlData('', aGroupIndex);
end;

procedure TCustomRegistrySource.DeliverMessage(aMessageConst: cardinal;
  aClientName: string = '';
  AGroupIndex: cardinal = 0);
var
  anz: integer;
  msg: TLMessage;
begin
  FillChar(msg, SizeOf(msg), #0);
  msg.Msg := aMessageConst;
  msg.lParam:=AGroupIndex;
  for anz := 0 to FClientList.count-1 do
  begin
    if Assigned(FClientList.Objects[anz]) then
      if FClientList.Objects[anz] is TWinControl then
      begin
        if (AClientName = EmptyStr) then
          TComponent(FClientList.Objects[anz]).Dispatch(msg)
        else
          if (LowerCase(AClientName) = LowerCase(TComponent(FClientList.Objects[anz]).Name)) then
            TComponent(FClientList.Objects[anz]).Dispatch(msg);
      end;
  end;
end;

procedure TCustomRegistrySource.RefreshSettings(AClientName: string = '');
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_SETTINGS, AClientName);
end;

procedure TCustomRegistrySource.RefreshControlData(AClientName: string = '';
  AGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_DATA, AClientName, AGroupIndex);
end;

procedure TCustomRegistrySource.RegisterControl(AControl: TComponent);
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

procedure TCustomRegistrySource.UnRegisterControl(AControl: TComponent);
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

  FReadDefaults := False;
  FWriteDefaults := False;
  FGUID := EmptyStr;
  FDoSyncData:= False;

  FClientList := TStringList.Create;
end;

destructor TCustomRegistrySource.Destroy;
begin
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
      ReadSection(GetRootKey,
        GetRootKeyForDefaults,
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
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
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

      if FDoSyncData then
        OnSyncData(aGroupIndex);
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

      if FDoSyncData then
        OnSyncData(aGroupIndex);
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

      if FDoSyncData then
        OnSyncData(aGroupIndex);
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
