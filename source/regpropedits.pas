unit regpropedits;

{S+}

interface

uses
  PropEdits,
  ComponentEditors;

type

  { TRegistrySourceComponentEditor }

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
    procedure ExecuteRefreshSettings;
    procedure ExecuteRefreshSyncProperty;
    procedure ExecuteRefreshWriteAdHocProperty;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TRegistrySettingsPropertyEditor }

  TRegistrySettingsPropertyEditor = class(TMethodPropertyEditor)
  public
    function GetFormMethodName: shortstring; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  Forms,
  regtype;

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

procedure TRegistrySourceComponentEditor.ExecuteRefreshSettings;
begin

end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshSyncProperty;
begin

end;

procedure TRegistrySourceComponentEditor.ExecuteRefreshWriteAdHocProperty;
begin

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
    0: Result := 'Refresh RegistrySettings';
    1: Result := 'Refresh DoWriteAdHoc';
    2: Result := 'Refresh DoSyncData';
  end;
end;

function TRegistrySourceComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
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

