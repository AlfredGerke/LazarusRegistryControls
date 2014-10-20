unit test_utils;

{$mode delphi}

interface

uses
  SysUtils,
  Registry,
  classes;

type
  TManageRegIniFile = procedure(aIni: TRegIniFile) of object;
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

function GetNextCount: integer;

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

    TAssert.AssertTrue(
      Format('Test nicht durchführbar, %s vorhanden', [aSection]), index=-1);
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

function GetNextCount: integer;
begin
   inc(count);
   Result := count;
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
  ini: TRegIniFile;
begin
  ini := TRegIniFile.Create(UTF8Decode(aRootKey));
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

