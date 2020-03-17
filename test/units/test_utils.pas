unit test_utils;

{$mode delphi}

interface

uses
  SysUtils,
  Registry,
  classes,
  regbasics;

type
  TManageRegIniFile = procedure(aIni: TLRCRegIniFile) of object;
  TManageRegistry = procedure(aReg: TRegistry) of object;

procedure AssertFindSectionTrue(aReg: TRegistry;
                                aSection: string);

procedure GetRegistry(aRoot: HKEY;
                      aRootKey: string;
                      aSection: string;
                      aProc: TManageRegistry;
                      aCanCreate: boolean = True);

procedure GetRegIniFile(aRootKey: string;
                        aProc: TManageRegIniFile);

function _IfEmptyThen(aString: string;
                      aDefault: string): string;

function CheckPropertyAvailable(aObj: TObject;
                                aPropertyName: string;
                                aMsg: string = ''): boolean;

procedure CheckListItems(aControlListItems: TStrings;
                         aRegistrySectionItems: TStrings);

function GetNextCount: integer;

function GetHKEYAsStr(AKey: HKEY): string;

implementation

uses
  fpcunit,
  typinfo;

var
  count: integer;

procedure AssertFindSectionTrue(aReg: TRegistry;
  aSection: string);
var
  list: TStrings;
  index: integer;
begin
  list := TStringList.Create;
  try
    with aReg do
      GetKeyNames(list);

    index := list.IndexOf(aSection);

    TAssert.AssertTrue(Format('Test nicht durchführbar, %s vorhanden', [aSection]), index=-1);
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure CheckListItems(aControlListItems: TStrings;
  aRegistrySectionItems: TStrings);
var
  index: integer;
  anz: integer;
  item: string;
begin
  if not Assigned(aControlListItems) then
    TAssert.Fail('Test nicht durchführbar, Control-Liste nicht zugewiesen');

  if not Assigned(aRegistrySectionItems) then
    TAssert.Fail('Test nicht durchführbar, Registry-Section-Liste nicht zugewiesen');

  TAssert.AssertEquals('Test nicht durchführbar, Anzahl Registry-Einträge ungleich Anzahl Items im Control',
    aRegistrySectionItems.Count, aControlListItems.Count);

  for anz := 0 to aRegistrySectionitems.Count-1 do
  begin
    item := aRegistrySectionItems.Strings[anz];
    index := aControlListItems.IndexOf(item);
    TAssert.AssertTrue(Format('Test nicht durchführbar, Item: %s nicht in der Control-Liste vorhanden',
      [item]), index > -1);
  end;
end;

function GetNextCount: integer;
begin
   inc(count);
   Result := count;
end;

function GetHKEYAsStr(AKey: HKEY): string;
begin
  case AKey of
    HKEY_CLASSES_ROOT: Result := 'HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER: Result := 'HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE: Result := 'HKEY_LOCAL_MACHINE';
    HKEY_USERS: Result := 'HKEY_USERS';
    HKEY_PERFORMANCE_DATA: Result := 'HKEY_PERFORMANCE_DATA';
    HKEY_CURRENT_CONFIG: Result := 'HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA: Result := 'HKEY_DYN_DATA';
  else
    Result := 'UNKOWN';
  end;
end;

procedure GetRegistry(aRoot: HKEY;
  aRootKey: string;
  aSection: string;
  aProc: TManageRegistry;
  aCanCreate: boolean = True);
var
  reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    with reg do
    begin
      RootKey := aRoot;

      if (Trim(aSection) = EmptyStr) then
        key := IncludeTrailingPathDelimiter(aRootKey)
      else
        key := IncludeTrailingPathDelimiter(aRootKey) +
          IncludeTrailingPathDelimiter(aSection);
      if OpenKey(key, aCanCreate) then
      begin
        if Assigned(aProc) then
          aProc(reg);

        CloseKey;
      end;
    end;

  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure GetRegIniFile(aRootKey: string;
  aProc: TManageRegIniFile);
var
  ini: TLRCRegIniFile;
begin
  ini := TLRCRegIniFile.Create(UTF8Decode(aRootKey));
  try
    with ini do
    begin
      if Assigned(aProc) then
        aProc(ini);
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;
end;

function _IfEmptyThen(aString: string;
  aDefault: string): string;
begin
  if (Trim(aString) = EmptyStr) then
    Result := aDefault
  else
    Result := aString;
end;

function CheckPropertyAvailable(aObj: TObject;
  aPropertyName: string;
  aMsg: string = ''): boolean;
var
  error: string;
  found: boolean;
  assert_message: string;
begin
  try
    try
      FindPropInfo(aObj, aPropertyName);
      found := True;
    except
      on E: Exception do
      begin
        found := False;
        error := E.Message;
      end;
    end;
  finally
    if (Trim(aMsg) = EmptyStr) then
      assert_message := Format('%s', [error])
    else
      assert_message := Format('%s - %s', [aMsg, error]);

    TAssert.AssertTrue(assert_message, found);

    Result := found;
  end;
end;

initialization
  count := 0;

end.

