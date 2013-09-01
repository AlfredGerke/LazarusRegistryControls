unit reglistbox;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Controls,
  Graphics,
  StdCtrls,
  regconst,
  LMessages,
  regmsg,
  regsourcen;

type

  { TCustomRegListBox }

  TCustomRegListBox = class(TListBox)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsList;
    FIsModified: boolean;

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
    procedure RefreshSync(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_SET_SYNC;
    procedure RefreshSettings(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_SETTINGS;
    procedure RefreshData(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_REFRESH_DATA;

    procedure SetName(const NewName: TComponentName); override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    function GetEditDialog(aEdit: boolean): boolean; virtual;

    property RegistrySettings: TRegistrySettingsList
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
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TRegListBox }

  TRegListBox = class(TCustomRegListBox)
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
  RegisterComponents('Registry Controls', [TRegListBox]);
  RegisterComponentEditor(TRegListBox, TRegistryControlComponentEditor);
end;

{ TCustomRegListBox }

function TCustomRegListBox.RefreshRegistrySettings: boolean;
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

      Result := ReadFromReg;
    finally
      FRegistrySettings.EndUpdate
    end;
  end;
end;

procedure TCustomRegListBox.ReadWriteInfo(aRead: boolean);
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

function TCustomRegListBox.GetItemsByRegistry: boolean;
var
  list: TStrings;
  index: integer;
begin
  Result := False;
  list := TStringList.Create;
  Items.BeginUpdate;
  try
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
              FRegistrySettings.RootKeyForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.ListSection,
              list,
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
    Items.EndUpdate;
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TCustomRegListBox.ShowEditDialog(var aMessage: TLMessage);
var
  do_edit: boolean;
begin
  if (aMessage.wParam = 1) then
    do_edit:= True
  else
    do_edit := False;

  aMessage.Result := LongInt(GetEditDialog(do_edit));
end;

procedure TCustomRegListBox.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegListBox.RefreshWriteAdHoc(var aMessage: TLMessage);
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

procedure TCustomRegListBox.RefreshSync(var aMessage: TLMessage);
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

procedure TCustomRegListBox.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := LongInt(RefreshRegistrySettings);
end;

procedure TCustomRegListBox.RefreshData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if FRegistrySettings.DoSyncData then
  begin;
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

procedure TCustomRegListBox.SetName(const NewName: TComponentName);
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

procedure TCustomRegListBox.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegListBox.SetRegistrySource(aRegistrySource: TRegistrySource);
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

function TCustomRegListBox.GetEditDialog(aEdit: boolean): boolean;
var
  edit_settings: TEditSettings;
  root_keys: TRootKeysStruct;
begin
  Result := False;
  root_keys.Found := False;
  root_keys.Clear;

  edit_settings := TEditSettings.Create(nil);
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

procedure TCustomRegListBox.ClearItems(aAskFor: boolean = True;
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

procedure TCustomRegListBox.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegListBox.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TCustomRegListBox.ReadFromReg: boolean;
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

function TCustomRegListBox.WriteToReg: boolean;
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

constructor TCustomRegListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsList.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegListBox.Destroy;
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
