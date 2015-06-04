unit regbasics;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Registry;

type

  { TLRCRegUtils }

  TLRCRegUtils = class
  public type
    THandleRegistry = function(aReg: TRegistry;
                               aOpenKey: string): boolean of object;

    { THandleValue }

    THandleValue = record
      Value: variant;

      procedure SetValueByInteger(aValue: integer);
      procedure SetValueByString(aValue: string);
      procedure SetValueByBoolean(aValue: boolean);
      function GetValueAsInteger: integer;
      function GetValueAsString: string;
      function GetValueAsBoolean: boolean;

      procedure Clear;
    end;
  private
    FSection: string;
    FIdent: string;
    FValue: THandleValue;
    FStrings: TStrings;
    FOpenReadOnly: boolean;
    FCanCreate: boolean;
  public
    class function GetInstance: TLRCRegUtils;

    procedure SetStrings(aStrings: TStrings);
    function GetStrings: TStrings;

    function GetRegistry(aRoot: HKEY;
                         aRootKey: string;
                         aProc: THandleRegistry;
                         aOpenKeyReadOnly: boolean = True;
                         aCanCreate: boolean = False;
                         aDoCloseKey: boolean = True): boolean;
    procedure Refresh;

    // Wird von GetRegistry gesetzt
    property CanCreate: boolean
      read FCanCreate;

    property OpenReadOnly: boolean
      read FOpenReadOnly;

    property Section: string
      read FSection
      write FSection;

    property Ident: string
      read FIdent
      write FIdent;

    property Value: THandleValue
      read FValue
      write FValue;
  end;

  { TLRCRegIniFile }

  TLRCRegIniFile = class
  strict private
    FReg: TLRCRegUtils;
    FRoot: HKEY;
    FFuncResult: TLRCRegUtils.THandleValue;
    FPreferStringValues: boolean;

    function ReadIntegerProc(aReg: TRegistry;
                             aOpenKey: string): boolean;
    function ReadStringProc(aReg: TRegistry;
                            aOpenKey: string): boolean;
    function ReadSectionProc(aReg: TRegistry;
                             aOpenKey: string): boolean;
    function ReadSectionsProc(aReg: TRegistry;
                              aOpenKey: string): boolean;
    function ReadSectionValuesProc(aReg: TRegistry;
                                   aOpenKey: string): boolean;
    function WriteBoolProc(aReg: TRegistry;
                           aOpenKey: string): boolean;
    function WriteIntegerProc(aReg: TRegistry;
                              aOpenKey: string): boolean;
    function WriteStringProc(aReg: TRegistry;
                             aOpenKey: string): boolean;
    function ReadBoolProc(aReg: TRegistry;
                          aOpenKey: string): boolean;
    function EraseSectionProc(aReg: TRegistry;
                              aOpenKey: string): boolean;
    function DeleteKeyProc(aReg: TRegistry;
                           aOpenKey: string): boolean;
    function KeyEixstsProc(aReg: TRegistry;
                           aOpenKey: string): boolean;
    function ValueExistsProc(aReg: TRegistry;
                             aOpenKey: string): boolean;
  private
    FFilename: string;

  protected
    property Root: HKEY
      read FRoot;
  public
    constructor Create(const aFileName: string;
                       aRoot: HKEY = HKEY_CURRENT_USER); virtual; overload;
    constructor Create(const aFileName: string;
                       aRoot: string = 'HKEY_CURRENT_USER'); virtual;  overload;

    destructor Destroy; override;

    function StrToHKeyRoot(aRootStr: string): HKEY;

    // ist in TRegIniFile nicht vorhanden
    function HandleRegistry(const aSection: string;
                            aHandleRegistryProc: TLRCRegUtils.THandleRegistry;
                            aCompleteByFilename: boolean = True): boolean;

    // kommt in TRegIniFile direkt aus TRegistry
    // Parameter und Händling angepasst (s. Testprogramm)
    function ValueExists(const aKey: string;
                         const aName: string;
                         aCompleteByFilename: boolean = True): boolean;

    // kommt in TRegIniFile direkt aus TRegistry
    // Parameter und Händling angepasst (s. Testprogramm)
    function KeyExists(const aKey: string;
                       aCompleteByFilename: boolean = True): boolean;

    function DeleteKey(const aSection: string;
                       const aIdent: string): boolean;

    function EraseSection(const aSection: string): boolean;

    function ReadBool(const aSection: string;
                      const aIdent: string;
                      aDefault: Boolean): Boolean;

    function ReadInteger(const aSection: string;
                         const aIdent: string;
                         aDefault: Longint): Longint;

    function ReadString(const aSection: string;
                        const aIdent: string;
                        const aDefault: string): string;

    procedure ReadSection(const aSection: string;
                          aStrings: TStrings);

    procedure ReadSections(aStrings: TStrings);

    procedure ReadSectionValues(const aSection: string;
                                aStrings: TStrings);

    procedure WriteBool(const aSection: string;
                        const aIdent: string;
                        aValue: Boolean);

    procedure WriteInteger(const aSection: string;
                           const aIdent: string;
                           aValue: Longint);

    procedure WriteString(const aSection: string;
                          const aIdent: string;
                          const aValue: string);

    function GetFilename: string;
    property Filename: string
      read GetFilename
      write FFilename;

    property PreferStringValues: boolean
      read FPreferStringValues
      write FPreferStringValues;
  published
  end;


implementation

var
  reg_util_instance: TLRCRegUtils;

{ TLRCRegUtils.THandleValue }

procedure TLRCRegUtils.THandleValue.SetValueByInteger(aValue: integer);
begin
  Value := aValue;
end;

procedure TLRCRegUtils.THandleValue.SetValueByString(aValue: string);
begin
  Value := aValue;
end;

procedure TLRCRegUtils.THandleValue.SetValueByBoolean(aValue: boolean);
begin
  Value := aValue;
end;

function TLRCRegUtils.THandleValue.GetValueAsInteger: integer;
begin
  Result := Value;
end;

function TLRCRegUtils.THandleValue.GetValueAsString: string;
begin
  Result := Value;
end;

function TLRCRegUtils.THandleValue.GetValueAsBoolean: boolean;
begin
  Result := Value;
end;

procedure TLRCRegUtils.THandleValue.Clear;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TLRCRegUtils }

class function TLRCRegUtils.GetInstance: TLRCRegUtils;
begin
  if not Assigned(reg_util_instance) then
    reg_util_instance := TLRCRegUtils.Create;

  Result := reg_util_instance;
end;

procedure TLRCRegUtils.SetStrings(aStrings: TStrings);
begin
  FStrings := aStrings;
end;

function TLRCRegUtils.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TLRCRegUtils.GetRegistry(aRoot: HKEY;
  aRootKey: string;
  aProc: THandleRegistry;
  aOpenKeyReadOnly: boolean = True;
  aCanCreate: boolean = False;
  aDoCloseKey: boolean = True): boolean;
var
  reg: TRegistry;
  key_exists: boolean;
  do_proc: boolean;
  proc_success: boolean;
begin
  proc_success := False;
  FOpenReadOnly := aOpenKeyReadOnly;
  FCanCreate := aCanCreate;

  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := aRoot;

        key_exists := KeyExists(aRootKey);

        if (FOpenReadOnly and key_exists) then
          do_proc := OpenKeyReadOnly(aRootKey)
        else
          do_proc := OpenKey(aRootKey, FCanCreate);
      end;

      if (do_proc and Assigned(aProc)) then
        proc_success := aProc(reg, aRootKey);

      Result := proc_success;
    except
      on E: Exception do
      begin
        Result := False;
      end;
    end;
  finally
    if Assigned(reg) then
    begin
      // wenn do_proc=True war OpenKey oder OpenKeyReadOnly erfogreich
      if (aDoCloseKey and do_proc) then
        reg.CloseKey;

      FreeAndNil(reg);
    end;
  end
end;

procedure TLRCRegUtils.Refresh;
begin
  FSection := EmptyStr;
  FIdent := EmptyStr;
  FOpenReadOnly := True;
  FCanCreate := False;
end;

{ TLRCRegIniFile }

function TLRCRegIniFile.ReadIntegerProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  ident: string;
  res: integer;
begin
  ident := FReg.Ident;
  if aReg.ValueExists(ident) then
  begin
    if FPreferStringValues then
      res := StrToInt(aReg.ReadString(ident))
    else
      res := aReg.ReadInteger(ident);

    FFuncResult.SetValueByInteger(res);
  end
  else
    FFuncResult.SetValueByInteger(FReg.Value.GetValueAsInteger);

  Result := True;
end;

function TLRCRegIniFile.ReadStringProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  ident: string;
  res: string;
begin
  ident := FReg.Ident;
  if aReg.ValueExists(ident) then
  begin
    res := aReg.ReadString(ident);
    FFuncResult.SetValueByString(res);
  end
  else
    FFuncResult.SetValueByString(FReg.Value.GetValueAsString);

  Result := True;
end;

function TLRCRegIniFile.ReadSectionProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  values: TStrings;
begin
  values := FReg.GetStrings;
  if Assigned(values) then
  begin
    aReg.GetValueNames(values);
    FReg.SetStrings(values);

    Result := Assigned(values);
  end
  else
    Result := False;
end;

function TLRCRegIniFile.ReadSectionsProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  values: TStrings;
begin
  values := FReg.GetStrings;
  if Assigned(values) then
  begin
    aReg.GetKeyNames(values);
    FReg.SetStrings(values);

    Result := Assigned(values);
  end
  else
    Result := False;
end;

function TLRCRegIniFile.ReadSectionValuesProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  names: TStrings;
  anz: integer;
  ident: string;
  value_data_type: TRegDataType;
  value: string;
begin
  names := FReg.GetStrings;
  if Assigned(names) then
  begin
    aReg.GetValueNames(names);

    for anz := 0 to names.Count-1 do
    begin
      ident := names[anz];
      value_data_type := aReg.GetDataType(ident);

      case value_data_type of
        rdString,
        rdExpandString:
          value := aReg.ReadString(ident);
        rdBinary:
          Continue;
        rdInteger:
          value := IntToStr(aReg.ReadInteger(ident));
      else
        Continue;
      end;

      names[anz] := Format('%s=%s', [ident, value]);
    end;

    FReg.SetStrings(names);

    Result := Assigned(names);
  end
  else
    Result := False;
end;

function TLRCRegIniFile.WriteBoolProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  if FPreferStringValues then
    aReg.WriteString(FReg.Ident, FReg.Value.GetValueAsString)
  else
    aReg.WriteBool(FReg.Ident, FReg.Value.GetValueAsBoolean);

  Result := True;
end;

function TLRCRegIniFile.WriteIntegerProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  if FPreferStringValues then
    aReg.WriteString(FReg.Ident, FReg.Value.GetValueAsString)
  else
    aReg.WriteInteger(FReg.Ident, FReg.Value.GetValueAsInteger);

  Result := True;
end;

function TLRCRegIniFile.WriteStringProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.WriteString(FReg.Ident, FReg.Value.GetValueAsString);

  Result := True;
end;

function TLRCRegIniFile.ReadBoolProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  ident: string;
  res: boolean;
begin
  ident := FReg.Ident;
  if aReg.ValueExists(ident) then
  begin
    if FPreferStringValues then
      res := (StrToInt(aReg.ReadString(ident)) <> 0)
    else
      res := aReg.ReadBool(ident);

    FFuncResult.SetValueByBoolean(res);
  end
  else
    FFuncResult.SetValueByBoolean(FReg.Value.GetValueAsBoolean);

  Result := True;
end;

function TLRCRegIniFile.EraseSectionProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.CloseKey;
  Result := aReg.DeleteKey(aOpenKey);
end;

function TLRCRegIniFile.DeleteKeyProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  Result := aReg.DeleteValue(FReg.Ident);
end;

function TLRCRegIniFile.KeyEixstsProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  with FReg do
  begin
    if (Section = EmptyStr) then
      Result := True
    else
      Result := aReg.KeyExists(Section);
  end;
end;

function TLRCRegIniFile.ValueExistsProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  Result := aReg.ValueExists(FReg.Ident);
end;

constructor TLRCRegIniFile.Create(const aFileName: string;
  aRoot: HKEY = HKEY_CURRENT_USER);
begin
  FFilename := IncludeTrailingPathDelimiter(aFileName);
  FRoot := aRoot;
  FReg := TLRCRegUtils.Create;
end;

constructor TLRCRegIniFile.Create(const aFileName: string;
  aRoot: string = 'HKEY_CURRENT_USER');
var
  _root: HKEY;
begin
  _root := StrToHKeyRoot(aRoot);

  create(aFilename, _root);
end;


destructor TLRCRegIniFile.Destroy;
begin
  FFilename := EmptyStr;

  if Assigned(FReg) then
    FreeAndNil(FReg);

  inherited Destroy;
end;


function TLRCRegIniFile.StrToHKeyRoot(aRootStr: string): HKEY;
begin
  if (UpperCase(Trim(aRootStr)) = 'HKEY_CLASSES_ROOT') then
    Result := HKEY_CLASSES_ROOT
  else
  if (UpperCase(Trim(aRootStr)) = 'HKEY_CURRENT_USER') then
    Result := HKEY_CURRENT_USER
  else
  if (UpperCase(Trim(aRootStr)) = 'HKEY_LOCAL_MACHINE') then
    Result := HKEY_LOCAL_MACHINE
  else
  if (UpperCase(Trim(aRootStr)) = 'HKEY_USERS') then
    Result := HKEY_USERS
  else
  if (UpperCase(Trim(aRootStr)) = 'HKEY_CURRENT_CONFIG') then
    Result := HKEY_CURRENT_CONFIG
  else
  if (UpperCase(Trim(aRootStr)) = 'HKEY_DYN_DATA') then
    Result := HKEY_DYN_DATA
end;

function TLRCRegIniFile.HandleRegistry(const aSection: string;
  aHandleRegistryProc: TLRCRegUtils.THandleRegistry;
  aCompleteByFilename: boolean): boolean;
var
  root_key: string;
begin
  with FReg do
  begin
    Refresh;
    try
      if aCompleteByFilename then
        root_key := Filename + Section
      else
        root_key := aSection;
      Result :=
        GetRegistry(FRoot, root_key, aHandleRegistryProc, False, True);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.ValueExists(const aKey: string;
  const aName: string;
  aCompleteByFilename: boolean = True): boolean;
var
  root_key: string;
begin
  with FReg do
  begin
    Refresh;
    try
      Ident := aName;

      if aCompleteByFilename then
        root_key := Filename + aKey
      else
        root_key := aKey;

      Result :=
        GetRegistry(FRoot, root_key, ValueExistsProc, True);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.KeyExists(const aKey: string;
  aCompleteByFilename: boolean = True): boolean;
var
  root_key: string;
begin
  with FReg do
  begin
    Refresh;
    try
      if aCompleteByFilename then
      begin
        Section := aKey;
        root_key := Filename;
      end
      else
      begin
        root_key := aKey;
      end;

      Result :=
        GetRegistry(FRoot, root_key, KeyEixstsProc, True);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.DeleteKey(const aSection: string;
  const aIdent: string): boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;

      Result :=
        GetRegistry(FRoot, Filename + aSection, DeleteKeyProc, False);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.EraseSection(const aSection: string): boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;

      Result := GetRegistry(FRoot, Filename + aSection, EraseSectionProc, False);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.ReadBool(const aSection: string;
  const aIdent: string;
  aDefault: Boolean): Boolean;
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByBoolean(aDefault);
      FFuncResult.Clear;

      success :=
        GetRegistry(FRoot, Filename + aSection, ReadBoolProc, True);

      if success then
        Result := FFuncResult.GetValueAsBoolean
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.ReadInteger(const aSection: string;
  const aIdent: string;
  aDefault: Longint): Longint;
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByInteger(aDefault);
      FFuncResult.Clear;

      success :=
        GetRegistry(FRoot, Filename + aSection, ReadIntegerProc, True);

      if success then
        Result := FFuncResult.GetValueAsInteger
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.ReadString(const aSection: string;
  const aIdent: string;
  const aDefault: string): string;
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByString(aDefault);
      FFuncResult.Clear;

      success :=
        GetRegistry(FRoot, Filename + aSection, ReadStringProc, True);

      if success then
        Result := FFuncResult.GetValueAsString
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.ReadSection(const aSection: string;
  aStrings: TStrings);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      SetStrings(aStrings);

      success :=
        GetRegistry(FRoot, Filename + aSection, ReadSectionProc, True);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.ReadSections(aStrings: TStrings);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      SetStrings(aStrings);

      success := GetRegistry(FRoot, Filename, ReadSectionsProc, True);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.ReadSectionValues(const aSection: string;
  aStrings: TStrings);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      SetStrings(aStrings);

      success :=
        GetRegistry(FRoot, Filename + aSection, ReadSectionValuesProc, True);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.WriteBool(const aSection: string;
  const aIdent: string;
  aValue: Boolean);
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByBoolean(aValue);

      GetRegistry(FRoot, Filename + aSection, WriteBoolProc, False, True);
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.WriteInteger(const aSection: string;
  const aIdent: string;
  aValue: Longint);
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByInteger(aValue);

      GetRegistry(FRoot, Filename + aSection, WriteIntegerProc, False, True);
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRegIniFile.WriteString(const aSection: string;
  const aIdent: string;
  const aValue: string);
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByString(aValue);

      GetRegistry(FRoot, Filename + aSection, WriteStringProc, False, True);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRegIniFile.GetFilename: string;
begin
  if (Trim(FFilename) <> EmptyStr) then
    Result := IncludeLeadingPathDelimiter(FFilename)
  else
    Result := EmptyStr;
end;

end.
