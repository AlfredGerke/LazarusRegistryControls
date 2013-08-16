unit regpropedits;

{S+}

interface

uses
  PropEdits,
  ComponentEditors,
  Menus,
  Classes;

type



  { TRegistryControlComponentEditor }

  TRegistryControlComponentEditor = class(TComponentEditor)
  protected
    procedure ExecuteShowInfo;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TRegistrySourceComponentEditor }

  TRegistrySourceComponentEditor = class(TComponentEditor)
  protected
    procedure OnClientMenuItemClick(Sender: TObject);
    procedure AddMenuItemsByClientList(aParentMenuItem: TMenuItem;
                                       aRegistrySource: TComponent);
    procedure AddMenuItemsForComponent(aIndex: integer;
                                       aParentMenuItem: TMenuItem;
                                       aRegistrySource: TComponent);
    procedure ExecuteRefreshSettings;
    procedure ExecuteRefreshSyncProperty;
    procedure ExecuteRefreshWriteAdHocProperty;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;
  end;

  { TRegistrySettingsPropertyEditor }

  TRegistrySettingsPropertyEditor = class(TMethodPropertyEditor)
  public
    function GetFormMethodName: shortstring; override;
  end;

implementation

uses
  SysUtils,
  Forms,
  regtype,
  regsourcen,
  Dialogs;

{ TRegistryControlComponentEditor }

procedure TRegistryControlComponentEditor.ExecuteShowInfo;
begin

end;

procedure TRegistryControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ExecuteShowInfo;
  end;
end;

function TRegistryControlComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Show Rootkeys';
  end;
end;

function TRegistryControlComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TRegistrySourceComponentEditor }

procedure TRegistrySourceComponentEditor.OnClientMenuItemClick(Sender: TObject);
var
  registry_source: TRegistrySource;
  control: TComponent;
begin
  registry_source := Component as TRegistrySource;

  if (Sender is TMenuItem) then;
  begin
    control := registry_source.GetClientByName(TMenuItem(sender).Caption);
    if Assigned(Control) then
      Designer.SelectOnlyThisComponent(control);
  end;
end;

procedure TRegistrySourceComponentEditor.AddMenuItemsByClientList(
  aParentMenuItem: TMenuItem;
  aRegistrySource: TComponent);
var
  anz: integer;
  client_menu_item: TMenuItem;
begin
  if (aRegistrySource is TRegistrySource) then
    for anz := 0 to TRegistrySource(aRegistrySource).ClientCount-1 do
    begin
      client_menu_item := TMenuItem.Create(aParentMenuItem);
      client_menu_item.Name := 'RegSrcSub' + IntToStr(anz);
      client_menu_item.Caption := TRegistrySource(aRegistrySource).GetClientNameByIndex(anz);
      client_menu_item.OnClick := OnClientMenuItemClick;
      aParentMenuItem.Add(client_menu_item);
    end;
end;

procedure TRegistrySourceComponentEditor.AddMenuItemsForComponent(aIndex: integer;
  aParentMenuItem: TMenuItem;
  aRegistrySource: TComponent);
begin
  case aIndex of
    0: ;
    1: ;
    2: ;
    3: AddMenuItemsByClientList(aParentMenuItem, aRegistrySource);
  end;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshSettings;
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  registry_source.RefreshSettings;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshSyncProperty;
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  registry_source.RefreshSyncProperty;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshWriteAdHocProperty;
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  registry_source.RefreshWriteAdHocProperty;
end;

procedure TRegistrySourceComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ExecuteRefreshSettings;
    1: ExecuteRefreshWriteAdHocProperty;
    2: ExecuteRefreshSyncProperty;
  end;
end;

function TRegistrySourceComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Refresh ClientSettings';
    1: Result := 'Refresh DoWriteAdHoc';
    2: Result := 'Refresh DoSyncData';
    3: Result := 'Registered Clients';
  end;
end;

function TRegistrySourceComponentEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure TRegistrySourceComponentEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  case Index of
    0: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    1: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    2: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    3: if (assigned(registry_source) and (registry_source.ClientCount > 0)) then
         AddMenuItemsForComponent(Index, aItem, registry_source)
       else
         AItem.Enabled := False;
  end;
end;

{ TRegistrySettingsPropertyEditor }

function TRegistrySettingsPropertyEditor.GetFormMethodName: shortstring;
var
  anz: Integer;
  root: TPersistent;
begin
  Result := EmptyStr;
  if (PropertyHook.LookupRoot = nil) then
    exit;
  if (GetComponent(0) = PropertyHook.LookupRoot) then
  begin
    root := PropertyHook.LookupRoot;
    if (root is TCustomForm) then
      Result := 'Form'
    else
    if (root is TDataModule) then
      Result := 'DataModule'
    else
    if (root is TFrame) then
      Result := 'Frame'
    else
    begin
      Result := ClassNameToComponentName(PropertyHook.GetRootClassName);
    end;
  end
  else
  begin
    if (GetComponent(0) is TCustomProperties) then
      Result := PropertyHook.GetObjectName(TCustomProperties(GetComponent(0)).Owner)
    else
      Result := PropertyHook.GetObjectName(GetComponent(0));

    for anz := Length(Result) downto 1 do
    if not ((Result[anz] in ['a'..'z', 'A'..'Z', '_']) or
      (anz > 1) and (Result[anz] in ['0'..'9']))
    then
      System.Delete(Result, anz, 1);
  end;
  if (Result = EmptyStr) then
    exit;
  Result := Result + GetTrimmedEventName;
end;

end.

