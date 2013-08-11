unit regutils;

interface

uses
  Classes,
  INIFILES,
  REGISTRY;

type

  TSTDefaults4CurrentUser = class
  private
    FRoot: string;
    FDefaultKey: string;
  protected
    function GetDefaultKey: string;
    function GetHKeyRoot: HKEY;
  public
    function ReadString(aSection: string; aIdent: string;
      aDefault: string): string; virtual;
    function ReadInteger(aSection: string; aIdent: string;
      aDefault: integer): integer; virtual;
    function ReadBool(aSection: string; aIdent: string;
      aDefault: boolean): boolean; virtual;
    procedure ReadSection(aSection: string; aStrings: TStrings); virtual;

    procedure WriteString(aSection: string; aIdent: string;
      aString: string); virtual;
    procedure WriteInteger(aSection: string; aIdent: string;
      aInteger: integer); virtual;
    procedure WriteBool(aSection: string; aIdent: string;
      aBool: boolean); virtual;

    constructor Create(aRoot: string; aDefaultKey: string); virtual;
    destructor Destroy; override;

  published
    property Root: string read FRoot write FRoot;
    property DefaultKey: string read GetDefaultKey write FDefaultKey;
  end;

  TSTRegIniFile = class(TRegIniFile)
  private
    FUseDefaults: TSTDefaults4CurrentUser;
  protected
  public
    function ReadStringCheck4Defaults(aSection: string;
      aIdent: string; aDefault: string): string; virtual;
    function ReadIntegerCheck4Defaults(aSection: string;
      aIdent: string; aDefault: integer): integer; virtual;
    function ReadBoolCheck4Defaults(aSection: string; aIdent: string;
      aDefault: boolean): boolean; virtual;
    procedure ReadSectionCheck4Defaults(aSection: string;
      aStrings: TStrings); virtual;

    procedure WriteStringCheck4Defaults(aSection: string;
      aIdent: string; aString: string); virtual;
    procedure WriteIntegerCheck4Defaults(aSection: string;
      aIdent: string; aInteger: integer); virtual;
    procedure WriteBoolCheck4Defaults(aSection: string;
      aIdent: string; aBool: boolean); virtual;

    constructor Create(aFileName: string; aDefaultsRoot: string;
      aDefaultKey: string); reintroduce; overload;
    destructor Destroy; override;
  published
    property UseDefaults: TSTDefaults4CurrentUser
      read FUseDefaults write FUseDefaults;
  end;

implementation

uses
  SysUtils;

function TSTDefaults4CurrentUser.GetDefaultKey: string;
begin
  Result := IncludeTrailingPathDelimiter(FDefaultKey);
end;

function TSTDefaults4CurrentUser.GetHKeyRoot: HKEY;
var
  resultValue: HKEY;
begin
  try
    if (UpperCase(Trim(Root)) = 'HKEY_CLASSES_ROOT') then
    begin
      resultValue := HKEY_CLASSES_ROOT;
    end;

    if (UpperCase(Trim(Root)) = 'HKEY_CURRENT_USER') then
    begin
      resultValue := HKEY_CURRENT_USER;
    end;

    if (UpperCase(Trim(Root)) = 'HKEY_LOCAL_MACHINE') then
    begin
      resultValue := HKEY_LOCAL_MACHINE;
    end;

    if (UpperCase(Trim(Root)) = 'HKEY_USERS') then
    begin
      resultValue := HKEY_USERS;
    end;

    if (UpperCase(Trim(Root)) = 'HKEY_CURRENT_CONFIG') then
    begin
      resultValue := HKEY_CURRENT_CONFIG;
    end;

    if (UpperCase(Trim(Root)) = 'HKEY_DYN_DATA') then
    begin
      resultValue := HKEY_DYN_DATA;
    end;
  finally
    Result := resultValue;
  end;
end;

{********************************* public    **********************************}
constructor TSTDefaults4CurrentUser.Create(aRoot: string; aDefaultKey: string);
begin
  inherited Create;

  FRoot := aRoot;
  FDefaultKey := aDefaultKey;
end;

destructor TSTDefaults4CurrentUser.Destroy;
begin
  FRoot := '';
  FDefaultKey := '';

  inherited Destroy;
end;

function TSTDefaults4CurrentUser.ReadString(aSection: string;
  aIdent: string; aDefault: string): string;
var
  resultValue: string;

  STReg: TRegistry;

  aKey: string;
begin

  resultValue := '';

  try
    try

      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKeyReadOnly(aKey);

        resultValue := ReadString(aIdent);

        if (LowerCase(Trim(resultValue)) = '') then
        begin
          resultValue := aDefault;
        end;

        CloseKey;
      end;

    except
      on E: Exception do
      begin
        resultValue := aDefault;
      end;
    end;
  finally
    STReg.Free;
    Result := resultValue;
  end;
end;

function TSTDefaults4CurrentUser.ReadInteger(aSection: string;
  aIdent: string; aDefault: integer): integer;
var
  resultValue: integer;

  STReg: TRegistry;

  aKey: string;
begin

  resultValue := -1;

  try
    try

      STReg := TRegistry.Create;

      with STReg do
      begin

        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKeyReadOnly(aKey);

        resultValue := StrToInt(ReadString(aIdent));

        CloseKey;

      end;

    except
      on E: Exception do
      begin
        resultValue := aDefault;
      end;
    end;
  finally
    STReg.Free;
    Result := resultValue;
  end;
end;

function TSTDefaults4CurrentUser.ReadBool(aSection: string; aIdent: string;
  aDefault: boolean): boolean;
var
  resultValue: boolean;

  STReg: TRegistry;

  aKey: string;
begin

  resultValue := False;

  try
    try

      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKeyReadOnly(aKey);

        resultValue := StrToInt(ReadString(aIdent)) <> 0;

        CloseKey;
      end;

    except
      on E: Exception do
      begin
        resultValue := aDefault;
      end;
    end;
  finally
    STReg.Free;
    Result := resultValue;
  end;
end;

procedure TSTDefaults4CurrentUser.ReadSection(aSection: string; aStrings: TStrings);
var
  STReg: TRegistry;
  aKey: string;
begin
  try
    try

      aStrings.Clear;

      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKeyReadOnly(aKey);

        GetValueNames(aStrings);

        CloseKey;
      end;

    except
      on E: Exception do
      begin
        aStrings.Clear;
      end;
    end;
  finally
    STReg.Free;
  end;
end;

procedure TSTDefaults4CurrentUser.WriteString(aSection: string;
  aIdent: string; aString: string);
var
  STReg: TRegistry;

  aKey: string;
begin
  try
    try
      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKey(aKey, True);

        WriteString(aIdent, aString);

        CloseKey;
      end;

    except
      on E: Exception do
      begin
      end;
    end;
  finally
    STReg.Free;
  end;
end;

procedure TSTDefaults4CurrentUser.WriteInteger(aSection: string;
  aIdent: string; aInteger: integer);
var
  STReg: TRegistry;

  aKey: string;
begin
  try
    try
      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKey(aKey, True);

        WriteString(aIdent, IntToStr(aInteger));

        CloseKey;
      end;

    except
      on E: Exception do
      begin
      end;
    end;
  finally
    STReg.Free;
  end;
end;

procedure TSTDefaults4CurrentUser.WriteBool(aSection: string;
  aIdent: string; aBool: boolean);
var
  STReg: TRegistry;

  aKey: string;
begin
  try
    try
      STReg := TRegistry.Create;

      with STReg do
      begin
        RootKey := GetHKEYRoot;
        aKey := concat(DefaultKey, aSection);

        OpenKey(aKey, True);

        if aBool then
          WriteString(aIdent, '1')
        else
          WriteString(aIdent, '0');

        CloseKey;
      end;

    except
      on E: Exception do
      begin
      end;
    end;
  finally
    STReg.Free;
  end;
end;

{********************************* published **********************************}
{********************************* ONNOTIFY  **********************************}
{******************************************************************************}
{******************************************************************************}
{******************************************************************************}

{********************************* TSTRegIniFile ******************************}
{******************************************************************************}
{******************************************************************************}
{********************************* private   **********************************}
{********************************* protected **********************************}
{********************************* public    **********************************}
constructor TSTRegIniFile.Create(aFileName: string; aDefaultsRoot: string;
  aDefaultKey: string);
begin
  inherited Create(aFileName);

  FUseDefaults := TSTDefaults4CurrentUser.Create(aDefaultsRoot, aDefaultKey);
end;

destructor TSTRegIniFile.Destroy;
begin
  FUseDefaults.Free;

  inherited Destroy;
end;

function TSTRegIniFile.ReadStringCheck4Defaults(aSection: string;
  aIdent: string; aDefault: string): string;
var
  resultValue: string;
begin
  try
    resultValue := ReadString(aSection, aIdent,
      UseDefaults.ReadString(aSection, aIdent, aDefault));
  finally
    Result := resultValue;
  end;
end;

function TSTRegIniFile.ReadIntegerCheck4Defaults(aSection: string;
  aIdent: string; aDefault: integer): integer;
var
  resultValue: integer;
begin
  try
    resultValue := ReadInteger(aSection, aIdent,
      UseDefaults.ReadInteger(aSection, aIdent, aDefault));
  finally
    Result := resultValue;
  end;
end;

function TSTRegIniFile.ReadBoolCheck4Defaults(aSection: string;
  aIdent: string; aDefault: boolean): boolean;
var
  resultValue: boolean;
begin
  try
    resultValue := ReadBool(aSection, aIdent,
      UseDefaults.ReadBool(aSection, aIdent, aDefault));
  finally
    Result := resultValue;
  end;
end;

procedure TSTRegIniFile.ReadSectionCheck4Defaults(aSection: string;
  aStrings: TStrings);
begin
  try
    try
      aStrings.Clear;

      ReadSection(aSection, aStrings);

      if aStrings.Count = 0 then
        UseDefaults.ReadSection(aSection, aStrings);

    except
      on E: Exception do
      begin
        aStrings.Clear;
      end;
    end;
  finally
  end;
end;

procedure TSTRegIniFile.WriteStringCheck4Defaults(aSection: string;
  aIdent: string; aString: string);
begin
  WriteString(aSection, aIdent, aString);
  UseDefaults.WriteString(aSection, aIdent, aString);
end;

procedure TSTRegIniFile.WriteIntegerCheck4Defaults(aSection: string;
  aIdent: string; aInteger: integer);
begin
  WriteInteger(aSection, aIdent, aInteger);
  UseDefaults.WriteInteger(aSection, aIdent, aInteger);
end;

procedure TSTRegIniFile.WriteBoolCheck4Defaults(aSection: string;
  aIdent: string; aBool: boolean);
begin
  WriteBool(aSection, aIdent, aBool);
  UseDefaults.WriteBool(aSection, aIdent, aBool);
end;

{********************************* published **********************************}
{********************************* ONNOTIFY  **********************************}
{******************************************************************************}
{******************************************************************************}
{******************************************************************************}

end.
