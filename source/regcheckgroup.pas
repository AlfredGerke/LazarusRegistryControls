unit regcheckgroup;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Controls,
  Graphics,
  ExtCtrls,
  regconst,
  LMessages,
  regmsg,
  regsourcen;

type

  { TCustomRegCheckGroup }

  TCustomRegCheckGroup = class(TCheckGroup)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsCheckedList;
    FIsModified: boolean;

    procedure SetCheckedItemsByValue(aList: TStrings);
    procedure SetCheckedItemsByList(aList: TStrings);
    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
    function GetItemsByRegistry: boolean;
  protected
    procedure ShowEditDialog(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SHOW_EDITDIALOG;
    procedure FreeRegistrySource(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE;
    procedure RefreshWriteAdHoc(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SET_WRITEADHOC;
    procedure RefreshSync(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SET_SYNC;
    procedure RefreshSettings(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_SETTINGS;
    procedure RefreshData(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_DATA;

    procedure SetName(const NewName: TComponentName); override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;

    property RegistrySettings: TRegistrySettingsCheckedList
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
    procedure SetItemIndex(aItemIndex: integer); virtual;
  public
    procedure Click; override;
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TCheckGroup }

  TRegCheckGroup = class(TCustomRegCheckGroup)
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
  dlgeditsettings,
  regtype;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegCheckGroup]);
  RegisterComponentEditor(TRegCheckGroup, TRegistryControlComponentEditor);
end;

{ TCustomRegCheckGroup }

procedure TCustomRegCheckGroup.SetCheckedItemsByValue(aList: TStrings);
var
  anz: integer;
  res_str: string;
  res_bool: boolean;
  item_value: string;
begin
  Items.Clear;
  for anz := 0 to aList.Count - 1 do
  begin
    res_str := aList.ValueFromIndex[anz];
    item_value := aList.Names[anz];
    Items.Add(item_value);

    if TryStrToBool(res_str, res_bool) then
      Checked[anz] := res_bool
    else
      Checked[anz] := False;
  end;
end;

procedure TCustomRegCheckGroup.SetCheckedItemsByList(aList: TStrings);
begin
  SetCheckedItemsByValue(aList);
end;

function TCustomRegCheckGroup.RefreshRegistrySettings: boolean;
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

procedure TCustomRegCheckGroup.ReadWriteInfo(aRead: boolean);
var
  sync_state_by_default: boolean;
  ident_by_itemindex: string;
  checked_by_itemindex: boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
    if assigned(RegistrySource) then
    begin
      if ((FRegistrySettings.RootKey <> '') and
        (FRegistrySettings.RootKeyForDefaults <> '') and
        (FRegistrySettings.RootForDefaults <> '') and
        (FRegistrySettings.Section <> '') and
        (FRegistrySettings.Ident <> '')) then
      begin
        case aRead of
          Read:
          begin
            if (FRegistrySettings.CanRead and FRegistrySettings.ItemsByRegistry) then
              GetItemsByRegistry
            else
            begin
              if (FRegistrySettings.CanRead and not
                FRegistrySettings.ItemsByRegistry) then
                ItemIndex := RegistrySource.ReadInteger(FRegistrySettings.RootKey,
                  FRegistrySettings.RootKeyForDefaults,
                  FRegistrySettings.RootForDefaults,
                  FRegistrySettings.Section,
                  FRegistrySettings.Ident,
                  FRegistrySettings.Default,
                  FRegistrySettings.ReadDefaults);
            end;
          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and FIsModified) then
            begin
              sync_state_by_default := FRegistrySettings.DoSyncData;
              FRegistrySettings.DoSyncData := False;
              try
                RegistrySource.WriteInteger(FRegistrySettings.RootKey,
                  FRegistrySettings.RootKeyForDefaults,
                  FRegistrySettings.RootForDefaults,
                  FRegistrySettings.Section,
                  FRegistrySettings.Ident,
                  ItemIndex,
                  FRegistrySettings.WriteDefaults,
                  FRegistrySettings.GroupIndex);

                  if (FRegistrySettings.ListSection <> '') then
                  begin
                    ident_by_itemindex := Items.Strings[ItemIndex];
                    checked_by_itemindex := Checked[ItemIndex];
                    RegistrySource.WriteBool(FRegistrySettings.RootKey,
                      FRegistrySettings.RootKeyForDefaults,
                      FRegistrySettings.RootForDefaults,
                      FRegistrySettings.ListSection,
                      ident_by_itemindex,
                      checked_by_itemindex,
                      FRegistrySettings.WriteDefaults,
                      FRegistrySettings.GroupIndex);
                  end;
                FIsModified := False;
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

function TCustomRegCheckGroup.GetItemsByRegistry: boolean;
var
  list: TStrings;
  index: integer;
begin
  Result := False;
  try
    Items.BeginUpdate;
    try
      list := TStringList.Create;

      if assigned(Items) then
      begin
        list.Clear;
        Items.Clear;
      end;

      if not (csDesigning in ComponentState) then
      begin
        if assigned(RegistrySource) then
        begin
          if ((FRegistrySettings.RootKey <> '') and
            (FRegistrySettings.RootKeyForDefaults <> '') and
            (FRegistrySettings.RootForDefaults <> '') and
            (FRegistrySettings.Section <> '') and
            (FRegistrySettings.Ident <> '')) then
          begin
            RegistrySource.ReadSection(FRegistrySettings.RootKey,
              FRegistrySettings.RootKeyForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.ListSection,
              list,
              FRegistrySettings.ReadDefaults,
              FRegistrySettings.SourceKind);
            SetCheckedItemsByList(list);
            index := RegistrySource.ReadInteger(FRegistrySettings.RootKey,
              FRegistrySettings.RootKeyForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.Section,
              FRegistrySettings.Ident,
              FRegistrySettings.Default,
              FRegistrySettings.ReadDefaults);
            if (index <= Items.Count - 1) then
              ItemIndex := index
            else
              ItemIndex := Items.Count - 1;
          end;
        end;
      end;

      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    Items.EndUpdate;
    list.Free;
  end;
end;

procedure TCustomRegCheckGroup.ShowEditDialog(var aMessage: TLMessage);
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

procedure TCustomRegCheckGroup.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegCheckGroup.RefreshWriteAdHoc(var aMessage: TLMessage);
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

procedure TCustomRegCheckGroup.RefreshSync(var aMessage: TLMessage);
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

procedure TCustomRegCheckGroup.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := longint(RefreshRegistrySettings);
end;

procedure TCustomRegCheckGroup.RefreshData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin
    ;
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

procedure TCustomRegCheckGroup.SetName(const NewName: TComponentName);
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

procedure TCustomRegCheckGroup.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegCheckGroup.SetRegistrySource(
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

procedure TCustomRegCheckGroup.SetItemIndex(aItemIndex: integer);
begin
  FItemIndex := aItemIndex;
end;

procedure TCustomRegCheckGroup.Click;
begin
  inherited;

  ItemIndex := 1;
  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegCheckGroup.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TCustomRegCheckGroup.ReadFromReg: boolean;
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

function TCustomRegCheckGroup.WriteToReg: boolean;
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

constructor TCustomRegCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsCheckedList.Create(Self);
  FRegistrySettings.OnChange := OnChangeSettings;
end;

destructor TCustomRegCheckGroup.Destroy;
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
