unit regedit;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  regsourcen,
  regconst,
  regmsg,
  LMessages,
  LResources;

type

  { TCustomRegEdit }

  TCustomRegEdit = class(TEdit)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsStringDefault;

    function RefreshRegistrySettings: boolean;
    procedure ReadWriteInfo(aRead: boolean);
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
    procedure PostData(var aMessage: TLMessage);
      message LM_REGISTRY_CONTROL_POST_DATA;

    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;

    procedure OnChangeSettings(Sender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource);
    function GetEditDialog(aEdit: boolean;
                           aAtDesignTime: boolean = True): boolean; virtual;

    property RegistrySettings: TRegistrySettingsStringDefault
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

  { TRegEdit }

  TRegEdit = class(TCustomRegEdit)
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
  dialogs,
  regpropedits,
  ComponentEditors,
  dlgeditsettings,
  regtype;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegEdit]);
  RegisterComponentEditor(TRegEdit, TRegistryControlComponentEditor);
end;

{ TCustomRegEdit }

procedure TCustomRegEdit.OnChangeSettings(Sender: TObject);
begin
  ReadFromReg;
end;

function TCustomRegEdit.RefreshRegistrySettings: boolean;
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

procedure TCustomRegEdit.ReadWriteInfo(aRead: boolean);
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
            begin
              Text := RegistrySource.ReadString(FRegistrySettings.RootKey,
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
            if (FRegistrySettings.CanWrite and Modified) then
            begin
              sync_state_by_default:= FRegistrySettings.DoSyncData;
              FRegistrySettings.DoSyncData := False;
              try
                RegistrySource.WriteString(FRegistrySettings.RootKey,
                  FRegistrySettings.RootKeyForDefaults,
                  FRegistrySettings.RootForDefaults,
                  FRegistrySettings.Section,
                  FRegistrySettings.Ident,
                  Text,
                  FRegistrySettings.WriteDefaults,
                  FRegistrySettings.GroupIndex);

                Modified := False;
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

procedure TCustomRegEdit.ShowEditDialog(var aMessage: TLMessage);
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

procedure TCustomRegEdit.FreeRegistrySource(var aMessage: TLMessage);
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);
  FRegistrySource := nil;
  RegistrySettings.CanWrite := False;
  RegistrySettings.CanRead := False;
  RegistrySettings.DoWriteAdHoc := False;
  aMessage.Result := 1;
end;

procedure TCustomRegEdit.RefreshWriteAdHoc(var aMessage: TLMessage);
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

procedure TCustomRegEdit.RefreshSync(var aMessage: TLMessage);
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

procedure TCustomRegEdit.RefreshSettings(var aMessage: TLMessage);
begin
  aMessage.Result := LongInt(RefreshRegistrySettings);
end;

procedure TCustomRegEdit.RefreshData(var aMessage: TLMessage);
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

procedure TCustomRegEdit.PostData(var aMessage: TLMessage);
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

procedure TCustomRegEdit.SetRegistrySource(aRegistrySource: TRegistrySource);
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

function TCustomRegEdit.GetEditDialog(aEdit: boolean;
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

procedure TCustomRegEdit.Change;
begin
  inherited;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

procedure TCustomRegEdit.SetName(const NewName: TComponentName);
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

constructor TCustomRegEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRegistrySettings := TRegistrySettingsStringDefault.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegEdit.Destroy;
begin
  if Assigned(FRegistrySource) then
    FRegistrySource.UnRegisterClient(self);

  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TCustomRegEdit.AfterConstruction;
begin
  inherited;

end;

function TCustomRegEdit.ReadFromReg: boolean;
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

function TCustomRegEdit.WriteToReg: boolean;
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
  {$I ..\package\regedit.lrs}

end.
