unit test_utils;

{$mode delphi}

interface

uses
  SysUtils,
  Registry;

type
  TManageRegIniFile = procedure(aIni: TRegIniFile) of object;

procedure GetRegIniFile(aRootKey: string;
                        aProc: TManageRegIniFile);

function _IfEmptyThen(aString: string;
                      aDefault: string): string;

function CheckPropertyAvailable(aObj: TObject;
                                aPropertyName: string;
                                aMsg: string = ''): boolean;

implementation

uses
  fpcunit,
  typinfo;

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

end.

