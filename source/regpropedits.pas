unit regpropedits;

{$mode Delphi}{$H+}

interface

uses
  PropEdits,
  ComponentEditors,
  Menus,
  Classes;

type

  { TRegistryControlComponentEditor }

  TRegistryControlComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure ExecuteShowInfo;
    procedure ExecuteEditRootKeys;
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
    procedure ExecuteShowInfo;
    procedure ExecuteRefreshSettings;
    procedure ExecuteRefreshSyncProperty;
    procedure ExecuteRefreshWriteAdHocProperty;
    procedure ExecuteRefreshMergeDataProperty;
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
  dlgTrueFalse,
  Controls,
  Dialogs,
  dlgaboutcomponent,
  LMessages,
  regmsg,
  ObjInspStrConsts,
  regresstrings,
  FileUtil;

{ TRegistryControlComponentEditor }

procedure TRegistryControlComponentEditor.ExecuteShowInfo;
begin
  StartAbout;
end;

procedure TRegistryControlComponentEditor.ExecuteEditRootKeys;
var
  msg: TLMessage;
begin
  // löst Hint: Local variable "msg" does not seem to be initialized
  msg.Result:=0;
  FillChar(msg, SizeOf(msg), #0);
  msg.Msg := LM_REGISTRY_CONTROL_SHOW_EDITDIALOG;
  msg.lParam:=0;
  msg.wParam:=0;

  Component.Dispatch(msg);
end;

procedure TRegistryControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1:;
    2: ExecuteShowInfo;
    3: ExecuteEditRootKeys;
  end;
end;

function TRegistryControlComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := oisCreateDefaultEvent;
    1: Result := '-';
    2: Result := SysToUTF8(rsVerbAbout);
    3: Result := SysToUTF8(rsVerbShowRootKeys);
  end;
end;

function TRegistryControlComponentEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TRegistrySourceComponentEditor }

procedure TRegistrySourceComponentEditor.OnClientMenuItemClick(Sender: TObject);
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;

  if (Sender is TMenuItem) then;
    registry_source.ShowClientEditDialog(TMenuItem(sender).Caption);

  if registry_source.EditClientRootKeys then
    Modified;
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
    3: ;
    4: ;
    5: ;
    6: AddMenuItemsByClientList(aParentMenuItem, aRegistrySource);
  end;
end;

procedure TRegistrySourceComponentEditor.ExecuteShowInfo;
begin
  StartAbout;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshSettings;
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  registry_source.RefreshSettings;
  Modified;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshSyncProperty;
var
  registry_source: TRegistrySource;
  dlg_true_false: TSetSyncData;
begin
  dlg_true_false := TSetSyncData.Create(nil);
  try
    with dlg_true_false do
    begin
      case ShowModal of
        mrOk:
        begin
          registry_source := Component as TRegistrySource;
          case SelectedIndex of
            0:
            begin
              registry_source.RefreshSyncProperty(False);
              Modified;
            end;
            1:
            begin
              registry_source.RefreshSyncProperty(True);
              Modified;
            end
          else
            MessageDlg(rsExecuteInvalidSection, mtInformation, [mbOk], 0);
          end;
        end;
      else
        MessageDlg(rsExecuteRequestCanceled, mtInformation, [mbOk], 0);
      end;
    end;
  finally
    if Assigned(dlg_true_false) then
      dlg_true_false.Release;
  end;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshWriteAdHocProperty;
var
  registry_source: TRegistrySource;
  dlg_true_false: TSetWriteAdHoc;
begin
  dlg_true_false := TSetWriteAdHoc.Create(nil);
  try
    with dlg_true_false do
    begin
      case ShowModal of
        mrOk:
        begin
          registry_source := Component as TRegistrySource;
          case SelectedIndex of
            0:
            begin
              registry_source.RefreshWriteAdHocProperty(False);
              Modified;
            end;
            1:
            begin
              registry_source.RefreshWriteAdHocProperty(True);
              Modified;
            end
          else
            MessageDlg(rsExecuteInvalidSection, mtInformation, [mbOk], 0);
          end;
        end;
      else
        MessageDlg(rsExecuteRequestCanceled, mtInformation, [mbOk], 0);
      end;
    end;
  finally
    if Assigned(dlg_true_false) then
      dlg_true_false.Release;
  end;
end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshMergeDataProperty;
var
  registry_source: TRegistrySource;
  dlg_true_false: TSetMergeData;
begin
  dlg_true_false := TSetMergeData.Create(nil);
  try
    with dlg_true_false do
    begin
      case ShowModal of
        mrOk:
        begin
          registry_source := Component as TRegistrySource;
          case SelectedIndex of
            0:
            begin
              registry_source.RefreshMergeDataProperty(False);
              Modified;
            end;
            1:
            begin
              registry_source.RefreshMergeDataProperty(True);
              Modified;
            end
          else
            MessageDlg(rsExecuteInvalidSection, mtInformation, [mbOk], 0);
          end;
        end;
      else
        MessageDlg(rsExecuteRequestCanceled, mtInformation, [mbOk], 0);
      end;
    end;
  finally
    if Assigned(dlg_true_false) then
      dlg_true_false.Release;
  end;
end;

procedure TRegistrySourceComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ExecuteShowInfo;
    2: ExecuteRefreshSettings;
    3: ExecuteRefreshWriteAdHocProperty;
    4: ExecuteRefreshSyncProperty;
    5: ExecuteRefreshMergeDataProperty;
  end;
end;

function TRegistrySourceComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := SysToUTF8(rsVerbAbout);
    1: Result := '-';
    2: Result := SysToUTF8(rsVerbRefreshClientSettings);
    3: Result := SysToUTF8(rsVerbRefreshDoWriteAdHoc);
    4: Result := SysToUTF8(rsVerbRefreshDoSyncData);
    5: Result := SysToUTF8(rsVerbMergeData);
    6: Result := SysToUTF8(rsVerbRegisteredClients);
  end;
end;

function TRegistrySourceComponentEditor.GetVerbCount: Integer;
begin
  Result := 7;
end;

procedure TRegistrySourceComponentEditor.PrepareItem(Index: Integer;
  const AItem: TMenuItem);
var
  registry_source: TRegistrySource;
begin
  registry_source := Component as TRegistrySource;
  case Index of
    0:;
    1:;
    2: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    3: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    4: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    5: AItem.Enabled := (assigned(registry_source) and (registry_source.ClientCount > 0));
    6: if (assigned(registry_source) and (registry_source.ClientCount > 0)) then
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

