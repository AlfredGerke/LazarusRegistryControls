unit regvaluelisteditor;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Controls,
  Graphics,
  ValEdit,
  regconst,
  LMessages,
  regmsg,
  regsourcen,
  regtype,
  grids;

type

  { TCustomRegValueListEditor }

  TCustomRegValueListEditor = class(TValueListEditor)
  private
    FCurrKeyValueItems: TKeyValueItems;
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsValueList;

    procedure UpdateKeyValueInfo(aCol: integer;
                                 aRow: integer);
    procedure SetItemsByList(aList: TStrings);
    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
    function GetItemsByRegistry: boolean;
  protected
    procedure ShowEditDialog(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SHOW_EDITDIALOG;
    procedure FreeRegistrySource(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE;
    procedure RefreshWriteAdHoc(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SET_WRITEADHOC;
    procedure RefreshSync(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SET_SYNC;
    procedure RefreshSettings(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_REFRESH_SETTINGS;
    procedure RefreshData(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_REFRESH_DATA;

    procedure SetName(const NewName: TComponentName); override;
    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;

    property RegistrySettings: TRegistrySettingsValueList
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  public
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TRegValueListEditor }

  TRegValueListEditor = class(TCustomRegValueListEditor)
  private
  protected
  public
  published
    property RegistrySettings;
    property RegistrySource;
  end;

procedure Register;

implementation

uses
  Forms,
  Dialogs,
  regpropedits,
  ComponentEditors,
  dlgeditsettings;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegValueListEditor]);
  RegisterComponentEditor(TRegValueListEditor, TRegistryControlComponentEditor);
end;

{ TCustomRegValueListEditor }

procedure TCustomRegValueListEditor.UpdateKeyValueInfo(aCol: integer;
  aRow: integer);
var
  idxRow: integer;
  keyValue: string;
  valValue: string;
begin
  idxRow := ARow-FixedRows;
  keyValue := Strings.Names[idxRow];
  valValue := Strings.ValueFromIndex[idxRow];
  if not FCurrKeyValueItems.IsEqual(keyValue, valValue, ACol, ARow) then
    FCurrKeyValueItems.SetItems(keyValue, valValue, ACol, ARow);

  if RegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegValueListEditor.SetItemsByList(aList: TStrings);
begin
  Strings.Clear;
  Strings.BeginUpdate;
  try
    Strings.AddStrings(aList);
    Loaded;
    FCurrKeyValueItems.Clear;
  finally
    Strings.EndUpdate;
  end;
end;

function TCustomRegValueListEditor.RefreshRegistrySettings: boolean;
begin
  Result := False;

  if Assigned(FRegistrySource) then
  begin
    FRegistrySettings.BeginUpdate;
    try
      FRegistrySettings.RootKey := FRegistrySource.RootKey;
      FRegistrySettings.RootKeyForDefaults := FRegistrySource.RootKeyForDefaults;
      FRegistrySettings.RootForDefaults := FRegistrySource.RootForDefaults;
      FRegistrySettings.Project := FRegistrySource.Project;
      FRegistrySettings.Organisation := FRegistrySource.Organisation;
      FRegistrySettings.GUID := FRegistrySource.GUID;
      FRegistrySettings.ReadDefaults := FRegistrySource.ReadDefaults;
      FRegistrySettings.WriteDefaults := FRegistrySource.WriteDefaults;

      Result := ReadFromReg;
    finally
      FRegistrySettings.EndUpdate
    end;
  end;
end;

procedure TCustomRegValueListEditor.ReadWriteInfo(aRead: boolean);
var
  sync_state_by_default: boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
    if assigned(RegistrySource) then
    begin
      if ((FRegistrySettings.RootKey <> '') and
        (FRegistrySettings.RootKeyForDefaults <> '') and
        (FRegistrySettings.RootForDefaults <> '') and
        (FRegistrySettings.ListSection <> '')) then
      begin
        case aRead of
          Read:
          begin
            if FRegistrySettings.CanRead then
              GetItemsByRegistry
          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and Modified) then
            begin
              sync_state_by_default := FRegistrySettings.DoSyncData;
              FRegistrySettings.DoSyncData := False;
              try
                // wie wird man in die Registry schreiben?
              finally
                FRegistrySettings.DoSyncData := sync_state_by_default;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomRegValueListEditor.GetItemsByRegistry: boolean;
var
  list: TStrings;
begin
  Result := False;
  list := TStringList.Create;
  try
    try
      if not (csDesigning in ComponentState) then
      begin
        if assigned(RegistrySource) then
        begin
          if ((FRegistrySettings.RootKey <> '') and
            (FRegistrySettings.RootKeyForDefaults <> '') and
            (FRegistrySettings.RootForDefaults <> '') and
            (FRegistrySettings.ListSection <> '')) then
          begin
            RegistrySource.ReadSection(FRegistrySettings.RootKey,
              FRegistrySettings.RootKeyForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.ListSection,
              list,
              FRegistrySettings.ReadDefaults,
              FRegistrySettings.SourceKind);
            SetItemsByList(list);
          end;
        end;
      end;

      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TCustomRegValueListEditor.ShowEditDialog(var aMessage: TLMessage);
var
  edit_settings: TEditSettings;
  do_edit: boolean;
  root_keys: TRootKeysStruct;
begin
  root_keys.Found := False;
  root_keys.Clear;

  if (aMessage.wParam = 1) then
    do_edit := True
  else
    do_edit := False;

  edit_settings := TEditSettings.Create(nil);
  try
    with edit_settings do
    begin
      RegistrySettings.GetRootKeys(root_keys);
      SetData(root_keys);

      case ShowModalEx(do_edit) of
        mrOk:
        begin
          root_keys.Clear;
          GetData(root_keys);
          RegistrySettings.SetRootKeys(root_keys);
          aMessage.Result := 1;
        end;
        mrCancel: ;
      end;
    end;
  finally
    if Assigned(edit_settings) then
      edit_settings.Release;
  end;
end;

procedure TCustomRegValueListEditor.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegValueListEditor.RefreshWriteAdHoc(var aMessage: TLMessage);
var
  group_index: cardinal;
  do_writeadhoc_flag: integer;
  do_writeadhoc: boolean;
begin
  group_index := aMessage.lParam;
  do_writeadhoc_flag := aMessage.wParam;
  do_writeadhoc := (do_writeadhoc_flag = 1);

  if (group_index > 0) then
  begin
    if (group_index = FRegistrySettings.GroupIndex) then
      FRegistrySettings.DoWriteAdHoc := do_writeadhoc;
  end
  else
    FRegistrySettings.DoWriteAdHoc := do_writeadhoc;
end;

procedure TCustomRegValueListEditor.RefreshSync(var aMessage: TLMessage);
var
  group_index: cardinal;
  do_sync_flag: integer;
  do_sync: boolean;
begin
  group_index := aMessage.lParam;
  do_sync_flag := aMessage.wParam;
  do_sync := (do_sync_flag = 1);

  if (group_index > 0) then
  begin
    if (group_index = FRegistrySettings.GroupIndex) then
      FRegistrySettings.DoSyncData := do_sync;
  end
  else
    FRegistrySettings.DoSyncData := do_sync;
end;

procedure TCustomRegValueListEditor.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := longint(RefreshRegistrySettings);
end;

procedure TCustomRegValueListEditor.RefreshData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin
    group_index := aMessage.lParam;
    if (group_index > 0) then
    begin
      if group_index = FRegistrySettings.GroupIndex then
        aMessage.Result := longint(ReadFromReg);
    end
    else
      aMessage.Result := longint(ReadFromReg);
  end;
end;

procedure TCustomRegValueListEditor.SetName(const NewName: TComponentName);
var
  old_name: TComponentName;
  new_name: TComponentName;
begin
  old_name := Name;

  inherited SetName(NewName);

  new_name := Name;

  if Assigned(FRegistrySource) then
    FRegistrySource.RenameClient(old_name, new_name);
end;

procedure TCustomRegValueListEditor.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegValueListEditor.SetRegistrySource(
  aRegistrySource: TRegistrySource);
begin
  if (FRegistrySource <> aRegistrySource) then
  begin
    if Assigned(FRegistrySource) then
      FRegistrySource.UnRegisterControl(self);

    FRegistrySource := aRegistrySource;
    if Assigned(FRegistrySource) then
    begin
      FRegistrySource.RegisterControl(self);
      RefreshRegistrySettings;
    end;
  end;
end;

procedure TCustomRegValueListEditor.SetEditText(ACol, ARow: Longint;
  const Value: string);
begin
  inherited SetEditText(ACol, ARow, Value);

  UpdateKeyValueInfo(ACol, ARow);
end;

procedure TCustomRegValueListEditor.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TCustomRegValueListEditor.ReadFromReg: boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Read);
    Application.ProcessMessages;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCustomRegValueListEditor.WriteToReg: boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Write);
    Application.ProcessMessages;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

constructor TCustomRegValueListEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRegistrySettings := TRegistrySettingsValueList.Create(Self);
  FRegistrySettings.OnChange := OnChangeSettings;
  FCurrKeyValueItems.Clear;
end;

destructor TCustomRegValueListEditor.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
