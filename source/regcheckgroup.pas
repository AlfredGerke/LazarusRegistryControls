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
  regsourcen,
  regtype;

type

  { TCustomRegCheckGroup }

  TCustomRegCheckGroup = class(TCheckGroup)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsCheckedList;
    FCaptionSettings: TCaptionSettings;
    FIsModified: boolean;
    FLastChecked: integer;
    FOnOriginalItemCheck: TCheckGroupClicked;
    FOnCustomItemCheck: TCheckGroupClicked;
    FCatchEvents: boolean;

    function GetKeyBySourceKind(aItemIndex: integer): string;
    procedure SaveItemsStatesToReg(aOnlyByItemIndex: boolean = True);
    procedure OnHookedItemCheck(Sender: TObject; Index: integer);
    procedure SetCheckedItemsByList(aList: TStrings;
                                    aCheckOnly: boolean);
    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
    procedure ReadWriteCaption(aRead: boolean);
    function GetItemsByRegistry(aCheckOnly: boolean): boolean;
  protected
    procedure RefreshMerge(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_MERGE_LIST;
    procedure ClearClientItems(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_CLEAR_LIST;
    procedure ShowEditDialog(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SHOW_EDITDIALOG;
    procedure FreeRegistrySource(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE;
    procedure RefreshWriteAdHoc(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SET_WRITEADHOC;
    procedure RefreshSync(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SET_SYNC;
    procedure RefreshSettings(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_SETTINGS;
    procedure RefreshData(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_DATA;
    procedure PostData(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_POST_DATA;

    procedure SetName(const NewName: TComponentName); override;
    procedure Loaded; override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure OnChangeCaptionSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    function GetEditDialog(aEdit: boolean;
                           aAtDesignTime: boolean = True): boolean; virtual;

    property RegistrySettings: TRegistrySettingsCheckedList
      read FRegistrySettings
      write FRegistrySettings;
    property CaptionSettings: TCaptionSettings
      read FCaptionSettings
      write FCaptionSettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
    procedure SetLastChecked(aLastChecked: integer);
    property LastChecked: integer
      read FLastChecked
      write SetLastChecked;
    property OnCustomItemCheck: TCheckGroupClicked
      read FOnCustomItemCheck
      write FOnCustomItemCheck;
  public
    function DeleteItem(aItemIndex: integer = -1;
                        aAskFor: boolean = True;
                        aMsg: string = 'Delete Item?'): boolean;
    function ClearItems(aAskFor: boolean = True;
                        aMsg: string = 'Clear Items?'): boolean; virtual;
    procedure Click; override;
    procedure AfterConstruction; override;
    function ReadFromReg(aReadSource: TRegistryDataOrigin = rdoGeneral): boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TCheckGroup }

  { TRegCheckGroup }

  TRegCheckGroup = class(TCustomRegCheckGroup)
  private
  protected
  public
  published
    property LastChecked;
    property CaptionSettings;
    property RegistrySettings;
    property RegistrySource;
    property OnCustomItemCheck;
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
  RegisterComponents('Registry Controls', [TRegCheckGroup]);
  RegisterComponentEditor(TRegCheckGroup, TRegistryControlComponentEditor);
end;

{ TCustomRegCheckGroup }

function TCustomRegCheckGroup.GetKeyBySourceKind(aItemIndex: integer): string;
begin
  case RegistrySettings.SourceKind of
    lskByKeyValue:
      Result := Items.Strings[aItemIndex];
  else
    MessageDlg('Invalid SourceKind!',
      mtWarning, [mbOK], 0);
    Result := EmptyStr;
  end;
end;

procedure TCustomRegCheckGroup.SaveItemsStatesToReg(aOnlyByItemIndex: boolean = True);
var
  ident_by_itemindex: String;
  checked_by_itemindex: Boolean;
  anz: Integer;
begin
  if aOnlyByItemIndex then
  begin
    ident_by_itemindex := Items.Strings[LastChecked];
    checked_by_itemindex := Checked[LastChecked];
    RegistrySource.WriteBool(FRegistrySettings.RootKey,
      FRegistrySettings.RootKeyForDefaults,
      FRegistrySettings.RootForDefaults,
      FRegistrySettings.ListSection,
      ident_by_itemindex,
      checked_by_itemindex,
      FRegistrySettings.WriteDefaults,
      FRegistrySettings.GroupIndex);
  end
  else
  begin
    Items.BeginUpdate;
    try
      for anz := 0 to Items.Count-1 do
      begin
        ident_by_itemindex := Items.Strings[anz];
        checked_by_itemindex := Checked[anz];
        RegistrySource.WriteBool(FRegistrySettings.RootKey,
          FRegistrySettings.RootKeyForDefaults,
          FRegistrySettings.RootForDefaults,
          FRegistrySettings.ListSection,
          ident_by_itemindex,
          checked_by_itemindex,
          FRegistrySettings.WriteDefaults,
          FRegistrySettings.GroupIndex);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TCustomRegCheckGroup.OnHookedItemCheck(Sender: TObject;
  Index: integer);
begin
  FLastChecked := Index;

  Click;

  if Assigned(FOnOriginalItemCheck) then
    if (@FOnOriginalItemCheck <> @OnItemClick) then
      FOnOriginalItemCheck(Sender, Index);

  if Assigned(FOnCustomItemCheck) then
    FOnCustomItemCheck(Sender, Index);
end;

procedure TCustomRegCheckGroup.SetCheckedItemsByList(aList: TStrings;
  aCheckOnly: boolean);
var
  anz: integer;
  res_str: string;
  res_bool: boolean;
  item_value: string;
  index: integer;
begin
  if not aCheckOnly then
  begin
    Items.Clear;

    for anz := 0 to aList.Count - 1 do
    begin
        item_value := aList.Names[anz];
        Items.Add(item_value);
    end;

    Loaded;
  end
  else
    if (Items.Count = 0) then
      Exit;

  for anz := 0 to aList.Count - 1 do
  begin
    item_value := aList.Names[anz];
    res_str := aList.Values[item_value];

    if not aCheckOnly then
      index := anz
    else
    begin
      index := Items.IndexOf(item_value);
      if (index = -1) then
        Continue;
    end;

    if TryStrToBool(res_str, res_bool) then
      Checked[index] := res_bool
    else
      Checked[index] := False;
  end;
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

      Result := ReadFromReg(rdoAll);
    finally
      FRegistrySettings.EndUpdate
    end;
  end;
end;

procedure TCustomRegCheckGroup.ReadWriteInfo(aRead: boolean);
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
        (FRegistrySettings.Section <> '') and
        (FRegistrySettings.Ident <> '')) then
      begin
        case aRead of
          Read:
          begin
            if (FRegistrySettings.CanRead and FRegistrySettings.ItemsByRegistry) then
              GetItemsByRegistry(False)
            else
              if (FRegistrySettings.CanRead and not FRegistrySettings.ItemsByRegistry) then
                GetItemsByRegistry(True)
          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and FIsModified) then
            begin
              sync_state_by_default := FRegistrySettings.DoSyncData;
              FRegistrySettings.BeginUpdate;
              FRegistrySettings.DoSyncData := False;
              try
                RegistrySource.WriteInteger(FRegistrySettings.RootKey,
                  FRegistrySettings.RootKeyForDefaults,
                  FRegistrySettings.RootForDefaults,
                  FRegistrySettings.Section,
                  FRegistrySettings.Ident,
                  LastChecked,
                  FRegistrySettings.WriteDefaults,
                  FRegistrySettings.GroupIndex);

                if ((FRegistrySettings.ListSection <> '') and
                    (Items.Count > 0) and
                    (LastChecked > -1)) then
                begin
                  if FRegistrySettings.DoWriteAdHoc then
                    SaveItemsStatesToReg(True)
                  else
                    SaveItemsStatesToReg(False);
                end;
                FIsModified := False;
              finally
                FRegistrySettings.DoSyncData := sync_state_by_default;
                FRegistrySettings.EndUpdate;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomRegCheckGroup.ReadWriteCaption(aRead: boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (assigned(RegistrySource) and FCaptionSettings.CaptionByRegistry) then
    begin
      if ((FRegistrySettings.RootKey <> '') and
        (FRegistrySettings.RootKeyForDefaults <> '') and
        (FRegistrySettings.RootForDefaults <> '') and
        (FCaptionSettings.Section <> '') and
        (FCaptionSettings.Ident <> '')) then
      begin
        case aRead of
          Read:
          begin
            if FRegistrySettings.CanRead then
              Caption := RegistrySource.ReadString(FRegistrySettings.RootKey,
                           FRegistrySettings.RootKeyForDefaults,
                           FRegistrySettings.RootForDefaults,
                           FCaptionSettings.Section,
                           FCaptionSettings.Ident,
                           Caption,
                           FRegistrySettings.ReadDefaults);

          end;
          Write:;
        end;
      end;
    end;
  end;
end;

function TCustomRegCheckGroup.GetItemsByRegistry(aCheckOnly: boolean): boolean;
var
  list: TStrings;
  index: integer;
begin
  Result := False;
  list := TStringList.Create;
  Items.BeginUpdate;
  try
    try
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
              FRegistrySettings.DoMergeData,
              FRegistrySettings.ReadDefaults,
              FRegistrySettings.SourceKind);
            SetCheckedItemsByList(list, aCheckOnly);
            index := RegistrySource.ReadInteger(FRegistrySettings.RootKey,
              FRegistrySettings.RootKeyForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.Section,
              FRegistrySettings.Ident,
              FRegistrySettings.Default,
              FRegistrySettings.ReadDefaults);
            if (index <= Items.Count - 1) then
              LastChecked := index
            else
              LastChecked := Items.Count - 1;
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
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TCustomRegCheckGroup.RefreshMerge(var aMessage: TLMessage);
var
  group_index: cardinal;
  do_mergedata_flag: integer;
  do_mergedata: boolean;
begin
  group_index := aMessage.lParam;
  do_mergedata_flag := aMessage.wParam;
  do_mergedata := (do_mergedata_flag = 1);

  if (group_index > 0) then
  begin
    if (group_index = FRegistrySettings.GroupIndex) then
      FRegistrySettings.DoMergeData := do_mergedata;
  end
  else
    FRegistrySettings.DoMergeData := do_mergedata;
end;

procedure TCustomRegCheckGroup.ClearClientItems(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  group_index := aMessage.lParam;

  if (group_index > 0) then
  begin
    if (group_index = FRegistrySettings.GroupIndex) then
      ClearItems(False);
  end
  else
    ClearItems(False);
end;

procedure TCustomRegCheckGroup.ShowEditDialog(var aMessage: TLMessage);
var
  do_edit: boolean;
  design_time: boolean;
begin
  if (aMessage.wParam = 1) then
    do_edit:= True
  else
    do_edit := False;

  if (aMessage.lParam = 1) then
    design_time := True
  else
    design_time := False;

  aMessage.Result := LongInt(GetEditDialog(do_edit, design_time));
end;

procedure TCustomRegCheckGroup.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);
  FRegistrySource := nil;
  RegistrySettings.BeginUpdate;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  RegistrySettings.EndUpdate;
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

procedure TCustomRegCheckGroup.PostData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin
    group_index := aMessage.lParam;
    if (group_index > 0) then
    begin
      if group_index = FRegistrySettings.GroupIndex then
        aMessage.Result := LongInt(WriteToReg)
    end
    else
      aMessage.Result := LongInt(WriteToReg);
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

procedure TCustomRegCheckGroup.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
    if FCatchEvents then
    begin
      FCatchEvents := False;

      if Assigned(OnItemClick) then
        FOnOriginalItemCheck := OnItemClick;

      OnItemClick := OnHookedItemCheck;
    end;
end;

procedure TCustomRegCheckGroup.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegCheckGroup.OnChangeCaptionSettings(Sender: TObject);
begin
  ReadFromReg(rdoCaption);
end;

procedure TCustomRegCheckGroup.SetRegistrySource(
  aRegistrySource: TRegistrySource);
begin
  if (FRegistrySource <> aRegistrySource) then
  begin
    if Assigned(FRegistrySource) then
      FRegistrySource.UnRegisterClient(self);

    FRegistrySource := aRegistrySource;
    if Assigned(FRegistrySource) then
    begin
      FRegistrySource.RegisterClient(self);
      RefreshRegistrySettings;
    end;
  end;
end;

function TCustomRegCheckGroup.GetEditDialog(aEdit: boolean;
  aAtDesignTime: boolean = True): boolean;
var
  edit_settings: TEditSettings;
  root_keys: TRootKeysStruct;
begin
  Result := False;
  root_keys.Found := False;
  root_keys.Clear;

  edit_settings := TEditSettings.Create(nil);
  edit_settings.AtDesignTime := aAtDesignTime;
  try
    with edit_settings do
    begin
      RegistrySettings.GetRootKeys(root_keys);
      SetData(root_keys);

      case ShowModalEx(aEdit) of
        mrOk:
        begin
          root_keys.Clear;
          GetData(root_keys);
          RegistrySettings.SetRootKeys(root_keys);
          Result := True;
        end;
        mrCancel:;
      end;
    end;
  finally
    if Assigned(edit_settings) then
      edit_settings.Release;
  end;
end;

procedure TCustomRegCheckGroup.SetLastChecked(aLastChecked: integer);
begin
  FLastChecked := aLastChecked;
end;

function TCustomRegCheckGroup.DeleteItem(aItemIndex: integer = -1;
  aAskFor: boolean = True;
  aMsg: string = 'Delete Item?'): boolean;
var
  key: string;
  start: boolean;
begin
  if (aItemIndex = -1) then
    aItemIndex := Self.LastChecked
  else
  if ((aItemIndex < 0) or (aItemIndex > Items.Count-1)) then
    raise Exception.CreateFmt('Invalid Index: %d', [aItemIndex]);

  start := not aAskFor;

  if FRegistrySettings.ItemsByRegistry then
  begin
    if aAskFor then
      start := (MessageDlg(aMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    if start then
    begin
      key := GetKeyBySourceKind(aItemIndex);
      if (key <> EmptyStr) then
        if ((FRegistrySettings.RootKey <> '') and
            (FRegistrySettings.RootKeyForDefaults <> '') and
            (FRegistrySettings.RootForDefaults <> '') and
            (FRegistrySettings.ListSection <> ''))
        then
          FRegistrySource.DeleteKey(FRegistrySettings.RootKey,
            FRegistrySettings.RootKeyForDefaults,
            FRegistrySettings.RootForDefaults,
            FRegistrySettings.ListSection,
            key,
            FRegistrySettings.WriteDefaults,
            FRegistrySettings.GroupIndex);
    end;
  end;

  Result := (start and FRegistrySettings.ItemsByRegistry);
end;

function TCustomRegCheckGroup.ClearItems(aAskFor: boolean = True;
  aMsg: string = 'Clear Items?'): boolean;
var
  start: boolean;
begin
  start := not aAskFor;
  if FRegistrySettings.ItemsByRegistry then
  begin
    if aAskFor then
      start := (MessageDlg(aMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    if start then
      FRegistrySource.EraseSection(FRegistrySettings.RootKey,
        FRegistrySettings.RootKeyForDefaults,
        FRegistrySettings.RootForDefaults,
        FRegistrySettings.ListSection,
        FRegistrySettings.WriteDefaults,
        FRegistrySettings.GroupIndex);
  end;

  Result := (start and FRegistrySettings.ItemsByRegistry);
end;

procedure TCustomRegCheckGroup.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegCheckGroup.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TCustomRegCheckGroup.ReadFromReg(aReadSource: TRegistryDataOrigin = rdoGeneral): boolean;
begin
  Result := False;

  if (not Assigned(FRegistrySource) or (aReadSource = rdoUnknown)) then
    Exit;

  try
    case aReadSource of
      rdoGeneral:
        ReadWriteInfo(Read);
      rdoCaption:
        ReadWriteCaption(Read);
      rdoAll:
      begin
        ReadWriteInfo(Read);
        ReadWriteCaption(Read);
      end;
    end;

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

  FCatchEvents := True;
  FIsModified := False;
  FRegistrySettings := TRegistrySettingsCheckedList.Create(Self);
  FRegistrySettings.OnChange := OnChangeSettings;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := OnChangeCaptionSettings;
end;

destructor TCustomRegCheckGroup.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

    if Assigned(FCaptionSettings) then
    FreeAndNil(FCaptionSettings);

  inherited Destroy;
end;

initialization
  {$I ..\package\regcheckgroup.lrs}

end.
