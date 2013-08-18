unit regcheckbox;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  regsourcen,
  regmsg,
  LResources,
  regconst,
  LMessages;

type

  { TCustomRegCheckBox }

  TCustomRegCheckBox = class(TCheckBox)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsBooleanDefault;
    FIsModified: boolean;

    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
  protected
    procedure ShowEditDialog(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SHOW_EDITDIALOG;
    procedure FreeRegistrySource(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE;
    procedure RefreshWriteAdHoc(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SET_WRITEADHOC;
    procedure RefreshSync(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_SET_SYNC;
    procedure RefreshSettings(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_REFRESH_SETTINGS;
    procedure RefreshData(var aMessage: TLMessage); message LM_REGISTRY_CONTROL_REFRESH_DATA;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    procedure Click; override;
    procedure SetName(const NewName: TComponentName); override;

    property RegistrySettings: TRegistrySettingsBooleanDefault
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

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TRegCheckBox }

  TRegCheckBox = class(TCustomRegCheckBox)
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
  regpropedits,
  ComponentEditors,
  dlgeditsettings,
  regtype;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegCheckBox]);
  RegisterComponentEditor(TRegCheckBox, TRegistryControlComponentEditor);
end;

{ TCustomRegCheckBox }

procedure TCustomRegCheckBox.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

function TCustomRegCheckBox.RefreshRegistrySettings: boolean;
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

procedure TCustomRegCheckBox.ReadWriteInfo(aRead: boolean);
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
            if FRegistrySettings.CanRead then
              Checked := RegistrySource.ReadBool(FRegistrySettings.RootKey,
                           FRegistrySettings.RootKeyForDefaults,
                           FRegistrySettings.RootForDefaults,
                           FRegistrySettings.Section,
                           FRegistrySettings.Ident,
                           FRegistrySettings.Default,
                           FRegistrySettings.ReadDefaults);

          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and FIsModified) then
            begin
              sync_state_by_default := FRegistrySettings.DoSyncData;
              FRegistrySettings.DoSyncData:= False;
              try
                RegistrySource.WriteBool(FRegistrySettings.Rootkey,
                  FRegistrySettings.RootKeyForDefaults,
                  FRegistrySettings.RootForDefaults,
                  FRegistrySettings.Section,
                  FRegistrySettings.Ident,
                  Checked,
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

procedure TCustomRegCheckBox.ShowEditDialog(var aMessage: TLMessage);
var
  edit_settings: TEditSettings;
  do_edit: boolean;
  root_keys: TRootKeysStruct;
begin
  root_keys.Found := False;
  root_keys.Clear;

  if (aMessage.wParam = 1) then
    do_edit:= True
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
        mrCancel:;
      end;
    end;
  finally
    if Assigned(edit_settings) then
      edit_settings.Release;
  end;
end;

procedure TCustomRegCheckBox.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegCheckBox.RefreshWriteAdHoc(var aMessage: TLMessage);
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

procedure TCustomRegCheckBox.RefreshSync(var aMessage: TLMessage);
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

procedure TCustomRegCheckBox.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := LongInt(RefreshRegistrySettings);
end;

procedure TCustomRegCheckBox.RefreshData(var aMessage: TLMessage);
var
  group_index: cardinal;
begin
  if  FRegistrySettings.DoSyncData then
  begin;
    group_index := aMessage.lParam;
    if (group_index > 0) then
    begin
      if group_index = FRegistrySettings.GroupIndex then
        aMessage.Result := LongInt(ReadFromReg)
    end
    else
      aMessage.Result := LongInt(ReadFromReg);
  end;
end;

procedure TCustomRegCheckBox.SetRegistrySource(aRegistrySource: TRegistrySource);
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

procedure TCustomRegCheckBox.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegCheckBox.SetName(const NewName: TComponentName);
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

constructor TCustomRegCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsBooleanDefault.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegCheckBox.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterControl(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TCustomRegCheckBox.AfterConstruction;
begin
  inherited;
end;

function TCustomRegCheckBox.ReadFromReg: boolean;
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

function TCustomRegCheckBox.WriteToReg: boolean;
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
  {$I ..\package\registrycontrols.lrs}

end.
