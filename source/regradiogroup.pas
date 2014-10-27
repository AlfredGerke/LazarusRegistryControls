unit regradiogroup;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  regsourcen,
  LResources,
  regconst,
  LMessages,
  regmsg,
  regtype;

type

  { TCustomRegRadioGroup }

  TCustomRegRadioGroup = class(TRadioGroup)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsList;
    FCaptionSettings: TCaptionSettings;
    FIsModified: boolean;

    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
    function GetItemsByRegistry: boolean;
    procedure ReadWriteCaption(aRead: boolean);
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

    // wird zu oft getriggert
    procedure CheckItemIndexChanged; override;
    // wird nicht getirggert
    procedure Click; override;
    procedure SetName(const NewName: TComponentName); override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure OnChangeCaptionSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    function GetEditDialog(aEdit: boolean;
                           aAtDesignTime: boolean = True): boolean; virtual;

    property RegistrySettings: TRegistrySettingsList
      read FRegistrySettings
      write FRegistrySettings;
    property CaptionSettings: TCaptionSettings
      read FCaptionSettings
      write FCaptionSettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  public
    procedure ClearItems(aAskFor: boolean = True;
                         aMsg: string = 'Clear Items?'); virtual;
    procedure AfterConstruction; override;
    function ReadFromReg(aReadSource: TRegistryDataOrigin = rdoGeneral): boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TRegRadioGroup }

  TRegRadioGroup = class(TCustomRegRadioGroup)
  private
  protected
  public
  published
    property CaptionSettings;
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
  RegisterComponents('Registry Controls', [TRegRadioGroup]);
  RegisterComponentEditor(TRegRadioGroup, TRegistryControlComponentEditor);
end;

{ TCustomRegRadioGroup }

procedure TCustomRegRadioGroup.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegRadioGroup.OnChangeCaptionSettings(Sender: TObject);
begin
  ReadFromReg(rdoCaption);
end;

function TCustomRegRadioGroup.RefreshRegistrySettings: boolean;
begin
  Result := False;

  if Assigned(FRegistrySource) then
  begin
    FRegistrySettings.BeginUpdate;
    try
      FRegistrySettings.RootKey := FRegistrySource.RootKey;
      FRegistrySettings.RootKeyForDefaults := FRegistrySource.RootKeyForDefaults;
      FRegistrySettings.RootForDefaults := FRegistrySource.RootForDefaults;
      FRegistrySettings.Project:= FRegistrySource.Project;
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

procedure TCustomRegRadioGroup.ReadWriteInfo(aRead: boolean);
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
            if (FRegistrySettings.CanRead and FRegistrySettings.ItemsByRegistry and AutoFill) then
              GetItemsByRegistry
            else
            begin
              if (FRegistrySettings.CanRead and not FRegistrySettings.ItemsByRegistry) then
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

function TCustomRegRadioGroup.GetItemsByRegistry: boolean;
var
  list: TStrings;
  index: integer;
begin
  Result := False;
  list := TStringList.Create;
  try
    Items.BeginUpdate;
    try
      if assigned(Items) then
        Items.Clear;

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
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.ListSection,
              list,
              FRegistrySettings.DoMergeData,
              FRegistrySettings.ReadDefaults,
              FRegistrySettings.SourceKind);
            Items.AddStrings(list);
            index := RegistrySource.ReadInteger(FRegistrySettings.RootKey,
                       FRegistrySettings.RootKeyForDefaults,
                       FRegistrySettings.RootForDefaults,
                       FRegistrySettings.Section,
                       FRegistrySettings.Ident,
                       FRegistrySettings.Default,
                       FRegistrySettings.ReadDefaults);
            Items.EndUpdate;
            if (index <= Items.Count-1) then
              ItemIndex := index
            else
              ItemIndex := Items.Count-1;
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

procedure TCustomRegRadioGroup.ReadWriteCaption(aRead: boolean);
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

procedure TCustomRegRadioGroup.RefreshMerge(var aMessage: TLMessage);
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

procedure TCustomRegRadioGroup.ClearClientItems(var aMessage: TLMessage);
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

procedure TCustomRegRadioGroup.ShowEditDialog(var aMessage: TLMessage);
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

procedure TCustomRegRadioGroup.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegRadioGroup.RefreshWriteAdHoc(var aMessage: TLMessage);
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
      FRegistrySettings.DoWriteAdHoc := do_writeadhoc
  end
  else
    FRegistrySettings.DoWriteAdHoc := do_writeadhoc;
end;

procedure TCustomRegRadioGroup.RefreshSync(var aMessage: TLMessage);
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
      FRegistrySettings.DoSyncData := do_sync
  end
  else
    FRegistrySettings.DoSyncData := do_sync;
end;

procedure TCustomRegRadioGroup.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := LongInt(RefreshRegistrySettings);
end;

procedure TCustomRegRadioGroup.RefreshData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin
    group_index := aMessage.lParam;
    if (group_index > 0) then
    begin
      if group_index = FRegistrySettings.GroupIndex then
        aMessage.Result := LongInt(ReadFromReg)
    end
    else
      aMessage.Result := LongInt(ReadFromReg)
  end;
end;

procedure TCustomRegRadioGroup.PostData(var aMessage: TLMessage);
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

procedure TCustomRegRadioGroup.SetRegistrySource(aRegistrySource: TRegistrySource);
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

function TCustomRegRadioGroup.GetEditDialog(aEdit: boolean;
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

procedure TCustomRegRadioGroup.CheckItemIndexChanged;
begin
  inherited CheckItemIndexChanged;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegRadioGroup.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegRadioGroup.SetName(const NewName: TComponentName);
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

procedure TCustomRegRadioGroup.ClearItems(aAskFor: boolean = True;
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

constructor TCustomRegRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsList.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;

  FCaptionSettings := TCaptionSettings.Create(Self);
  FCaptionSettings.OnChange := OnChangeCaptionSettings;
end;

destructor TCustomRegRadioGroup.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  if Assigned(FCaptionSettings) then
    FreeAndNil(FCaptionSettings);

  inherited Destroy;
end;

procedure TCustomRegRadioGroup.AfterConstruction;
begin
  inherited;
end;

function TCustomRegRadioGroup.ReadFromReg(aReadSource: TRegistryDataOrigin = rdoGeneral): boolean;
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

function TCustomRegRadioGroup.WriteToReg: boolean;
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

initialization
  {$I ..\package\regradiogroup.lrs}

end.
