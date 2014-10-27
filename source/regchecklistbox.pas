unit regchecklistbox;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Controls,
  Graphics,
  CheckLst,
  regconst,
  LMessages,
  regmsg,
  regsourcen,
  regtype;

type

  { TCustomRegCheckListBox }
  TCustomRegCheckListBox = class(TCheckListBox)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsCheckedList;
    FIsModified: boolean;

    procedure SaveItemsStatesToReg(aOnlyByItemIndex: boolean = True);
    procedure SetCheckedItemsByList(aList: TStrings;
                                    aCheckOnly: boolean);
    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean;
                            aType: TInfoKind = ikItemIndex);
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
    procedure ItemClick(const AIndex: integer); override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    function GetEditDialog(aEdit: boolean;
                           aAtDesignTime: boolean = True): boolean; virtual;

    property RegistrySettings: TRegistrySettingsCheckedList
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  public
    procedure ClearItems(aAskFor: boolean = True;
                         aMsg: string = 'Clear Items?'); virtual;
    procedure Click; override;
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg(aType: TInfoKind = ikItemIndex): boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TCheckListBox }

  TRegCheckListBox = class(TCustomRegCheckListBox)
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
  RegisterComponents('Registry Controls', [TRegCheckListBox]);
  RegisterComponentEditor(TRegCheckListBox, TRegistryControlComponentEditor);
end;

{ TCustomRegCheckListBox }

procedure TCustomRegCheckListBox.SaveItemsStatesToReg(
  aOnlyByItemIndex: boolean = True);
var
  ident_by_itemindex: String;
  checked_by_itemindex: Boolean;
  anz: Integer;
begin
  if aOnlyByItemIndex then
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

procedure TCustomRegCheckListBox.SetCheckedItemsByList(aList: TStrings;
  aCheckOnly: boolean);
var
  anz: integer;
  res_str: string;
  res_bool: boolean;
  item_value: string;
  index: integer;
begin
  if not aCheckOnly then
    Items.Clear
  else
    if (Items.Count = 0) then
      Exit;

  for anz := 0 to aList.Count - 1 do
  begin
    item_value := aList.Names[anz];
    res_str := aList.Values[item_value];

    if not aCheckOnly then
    begin
      Items.Add(item_value);
      index := anz;
    end
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

function TCustomRegCheckListBox.RefreshRegistrySettings: boolean;
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

procedure TCustomRegCheckListBox.ReadWriteInfo(aRead: boolean;
  aType: TInfoKind = ikItemIndex);
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
                GetItemsByRegistry(True);
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

                case aType of
                  ikInfo:
                  begin
                    if ((FRegistrySettings.ListSection <> '') and (Items.Count > 0)) then
                    begin
                      if FRegistrySettings.DoWriteAdHoc then
                        SaveItemsStatesToReg(True)
                      else
                        SaveItemsStatesToReg(False);
                    end;
                  end;
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

function TCustomRegCheckListBox.GetItemsByRegistry(aCheckOnly: boolean): boolean;
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
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TCustomRegCheckListBox.RefreshMerge(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.ClearClientItems(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.ShowEditDialog(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegCheckListBox.RefreshWriteAdHoc(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.RefreshSync(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := longint(RefreshRegistrySettings);
end;

procedure TCustomRegCheckListBox.RefreshData(var aMessage: TLMessage);
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

procedure TCustomRegCheckListBox.PostData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin
    group_index := aMessage.lParam;
    if (group_index > 0) then
    begin
      if group_index = FRegistrySettings.GroupIndex then
        aMessage.Result := LongInt(WriteToReg(ikInfo))
    end
    else
      aMessage.Result := LongInt(WriteToReg(ikInfo));
  end;
end;

procedure TCustomRegCheckListBox.SetName(const NewName: TComponentName);
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

procedure TCustomRegCheckListBox.ItemClick(const AIndex: integer);
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg(ikInfo);
end;

procedure TCustomRegCheckListBox.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegCheckListBox.SetRegistrySource(aRegistrySource: TRegistrySource);
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

function TCustomRegCheckListBox.GetEditDialog(aEdit: boolean;
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

procedure TCustomRegCheckListBox.ClearItems(aAskFor: boolean = True;
  aMsg: string = 'Clear Items?');
var
  start: boolean;
begin
  start := not aAskFor;
  if FRegistrySettings.ItemsByRegistry then
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

procedure TCustomRegCheckListBox.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg(ikItemIndex);
end;

procedure TCustomRegCheckListBox.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TCustomRegCheckListBox.ReadFromReg: boolean;
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

function TCustomRegCheckListBox.WriteToReg(aType: TInfoKind = ikItemIndex): boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Write, aType);
    Application.ProcessMessages;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

constructor TCustomRegCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsCheckedList.Create(Self);
  FRegistrySettings.OnChange := OnChangeSettings;
end;

destructor TCustomRegCheckListBox.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

initialization
  {$I ..\package\regchecklistbox.lrs}

end.
