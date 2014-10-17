unit regutils;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  Registry,
  regtype;

type

  { TRegUtils }

  TRegUtils = class
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
    class function GetInstance: TRegUtils;

    procedure SetStrings(aStrings: TStrings);
    function GetStrings: TStrings;

    function GetRegistry(aRoot: HKEY;
                         aRootKey: string;
                         aProc: THandleRegistry;
                         aOpenKeyReadOnly: boolean = True;
                         aCanCreate: boolean = False): boolean;
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

  { TLRCRRegIniFile }

  TLRCRRegIniFile = class
  strict private
    FReg: TRegUtils;
    FRoot: HKEY;
    FFuncResult: TRegUtils.THandleValue;

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
  private
    FFilename: string;
  protected
  public
    constructor Create(const aFileName: string;
                       aRoot: HKEY = HKEY_CURRENT_USER); virtual;
    destructor Destroy; override;

    function DeleteKey(const aSection: string;
                       const aIdent: string): boolean;

    procedure EraseSection(const aSection: string);

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
  published
  end;

  { TDefaultsForCurrentUser }

  TDefaultsForCurrentUser = class
  private
    FRoot: string;
    FDefaultKey: string;
    FPrefereStrings: boolean;
    FCheckRTLAnsi: boolean;

    function SectionExistsProc(aReg: TRegistry;
                               aOpenKey: string): boolean;
    function IdentExistsProc(aReg: TRegistry;
                             aOpenKey: string): boolean;
    procedure ReadSectionValuesByKind(aSection: string;
                                      aStrings: TStrings;
                                      aKind: TListSourceKind = Both;
                                      aMerge: boolean = False);
  protected
    function GetDefaultKey: string;
    function GetHKeyRoot: HKEY;

    property CheckRTLAnsi: boolean
      read FCheckRTLAnsi
      write FCheckRTLAnsi;
  public
    function SectionExists(aSection: string): boolean;
    function IdentExists(aSection: string;
                         aIdent: string): boolean;
    function DeleteKey(aSection: string): boolean;
    function DeleteValue(aSection: string;
                         aKey: string): boolean;
    procedure RenameKey(aSection: string;
                        aOldKey: string;
                        aNewKey: string);
    function ReadString(aSection: string;
                        aIdent: string;
                        aDefault: string): string; virtual;
    function ReadInteger(aSection: string;
                         aIdent: string;
                         aDefault: integer): integer; virtual;
    function ReadBool(aSection: string;
                      aIdent: string;
                      aDefault: boolean): boolean; virtual;
    procedure ReadSectionValuesOnly(aSection: string;
                                    aStrings: TStrings;
                                    aMerge: boolean);
    procedure ReadSection(aSection: string;
                          aStrings: TStrings;
                          aMerge: boolean); virtual;
    procedure ReadSectionValues(aSection: string;
                                aStrings: TStrings;
                                aMerge: boolean); virtual;
    procedure WriteString(aSection: string;
                          aIdent: string;
                          aString: string); virtual;
    procedure WriteInteger(aSection: string;
                           aIdent: string;
                           aInteger: integer); virtual;
    procedure WriteBool(aSection: string;
                        aIdent: string;
                        aBool: boolean); virtual;

    constructor Create(aRoot: string;
                       aDefaultKey: string;
                       aPrefereString: boolean = False;
                       aCheckRTLAnsi: boolean = True); virtual;
    destructor Destroy; override;
  published
    property Root: string
      read FRoot
      write FRoot;

    property DefaultKey: string
      read GetDefaultKey
      write FDefaultKey;
  end;

  { TDataByCurrentUser }

  TDataByCurrentUser = class(TRegIniFile)
  private
    FUseDefaults: TDefaultsForCurrentUser;
    FCheckRTLAnsi: boolean;

    procedure ReadSectionValuesByKind(aSection: string;
                                      aStrings: TStrings;
                                      aKind: TListSourceKind = Both);
  protected
    property CheckRTLAnsi: boolean
      read FCheckRTLAnsi
      write FCheckRTLAnsi;
  public
    // überschrieben aus TRegIniFile/TRegistry
    //!<--
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section: String;
                        const Ident: String);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSection(const Section: string;
                          Strings: TStrings);
    procedure WriteString(const Section: String;
                          const Ident: String;
                          const Value: String);
    function ReadString(const Section: string;
                        const Ident: string;
                        const Default: string): string;
    function ReadInteger(const Section: string;
                         const Ident: string;
                         Default: Longint): Longint;
    function ReadBool(const Section: string;
                      const Ident: string;
                      Default: Boolean): Boolean;
    procedure WriteBool(const Section: string;
                        const Ident: string;
                        Value: Boolean);
    procedure WriteInteger(const Section: string;
                           const Ident: string;
                           Value: Longint);
    //!-->

    function SectionExistsForDefaults(aSection: string): boolean;
    function SectionExists(aSection: string): boolean;
    function IdentExistsForDefaults(aSection: string;
                                    aIdent: string): boolean;
    function IdentExists(aSection: string;
                         aIdent: string): boolean;
    procedure EraseSectionForDefaults(aSection: string);
    procedure DeleteKeyForDefaults(aSection: string;
                                   aKey: string);
    procedure RenameKey(aSection: string;
                        aOldKey: string;
                        aNewKey: string);
    procedure RenameKeyForDefaults(aSection: string;
                                   aOldKey: string;
                                   aNewKey: string);
    procedure ReadSectionValuesEx(aSection: string;
                                  aStrings: TStrings); virtual;
    procedure ReadSectionValuesOnly(aSection: string;
                                    aStrings: TStrings);
    procedure ReadSectionValuesOnlyForDefaults(aSection: string;
                                               aStrings: TStrings;
                                               aMerge: boolean);
    function ReadStringCheckForDefaults(aSection: string;
                                        aIdent: string;
                                        aDefault: string): string; virtual;
    function ReadIntegerCheckForDefaults(aSection: string;
                                         aIdent: string;
                                         aDefault: integer): integer; virtual;
    function ReadBoolCheckForDefaults(aSection: string;
                                      aIdent: string;
                                      aDefault: boolean): boolean; virtual;
    procedure ReadSectionCheckForDefaults(aSection: string;
                                          aStrings: TStrings;
                                          aMerge: boolean); virtual;
    procedure ReadSectionValuesCheckForDefaults(aSection: string;
                                                aStrings: TStrings;
                                                aMerge: boolean); virtual;
    procedure WriteStringCheckForDefaults(aSection: string;
                                          aIdent: string;
                                          aString: string); virtual;
    procedure WriteIntegerCheckForDefaults(aSection: string;
                                           aIdent: string;
                                           aInteger: integer); virtual;
    procedure WriteBoolCheckForDefaults(aSection: string;
                                        aIdent: string;
                                        aBool: boolean); virtual;

    constructor Create(aFileName: string;
                       aDefaultsRoot: string;
                       aDefaultKey: string;
                       aPrefereStrings: boolean = False;
                       aCheckRTLAnsi: boolean = True); reintroduce; overload;
    constructor Create(aFileName: string;
                       aPrefereStrings: boolean = False;
                       aCheckRTLAnsi: boolean = True); reintroduce; overload;

    destructor Destroy; override;
  published
    property UseDefaults: TDefaultsForCurrentUser
      read FUseDefaults
      write FUseDefaults;
  end;

function _GenGUIDAsStr(var aGUIDAsStr: string): integer;

implementation

uses
  SysUtils,
  FileUtil,
  regconvutils;

function _GenGUIDAsStr(var aGUIDAsStr: string): integer;
var
  guid: TGUID;
  error: integer;
begin
  aGUIDAsStr := EmptyStr;
  error:= CreateGUID(guid);
  if (error = 0) then
    aGUIDAsStr := GUIDToString(guid);
  Result := error;
end;

{ TRegUtils }

var
  reg_util_instance: TRegUtils;

{ TRegUtils.THandleValue }

procedure TRegUtils.THandleValue.SetValueByInteger(aValue: integer);
begin
  Value := aValue;
end;

procedure TRegUtils.THandleValue.SetValueByString(aValue: string);
begin
  Value := aValue;
end;

procedure TRegUtils.THandleValue.SetValueByBoolean(aValue: boolean);
begin
  Value := aValue;
end;

function TRegUtils.THandleValue.GetValueAsInteger: integer;
begin
  Result := Value;
end;

function TRegUtils.THandleValue.GetValueAsString: string;
begin
  Result := Value;
end;

function TRegUtils.THandleValue.GetValueAsBoolean: boolean;
begin
  Result := Value;
end;

procedure TRegUtils.THandleValue.Clear;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TLRCRRegIniFile }

function TLRCRRegIniFile.ReadIntegerProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  ident: string;
  res: integer;
begin
  ident := FReg.Ident;
  if aReg.ValueExists(ident) then
  begin
    res := aReg.ReadInteger(ident);
    FFuncResult.SetValueByInteger(res);
  end
  else
    FFuncResult.SetValueByInteger(FReg.Value.GetValueAsInteger);

  Result := True;
end;

function TLRCRRegIniFile.ReadStringProc(aReg: TRegistry;
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

function TLRCRRegIniFile.ReadSectionProc(aReg: TRegistry;
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

function TLRCRRegIniFile.ReadSectionsProc(aReg: TRegistry;
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

function TLRCRRegIniFile.ReadSectionValuesProc(aReg: TRegistry;
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

function TLRCRRegIniFile.WriteBoolProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.WriteBool(FReg.Ident, FReg.Value.GetValueAsBoolean);

  Result := True;
end;

function TLRCRRegIniFile.WriteIntegerProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.WriteInteger(FReg.Ident, FReg.Value.GetValueAsInteger);

  Result := True;
end;

function TLRCRRegIniFile.WriteStringProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.WriteString(FReg.Ident, FReg.Value.GetValueAsString);

  Result := True;
end;

function TLRCRRegIniFile.ReadBoolProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  ident: string;
  res: boolean;
begin
  ident := FReg.Ident;
  if aReg.ValueExists(ident) then
  begin
    res := aReg.ReadBool(ident);
    FFuncResult.SetValueByBoolean(res);
  end
  else
    FFuncResult.SetValueByBoolean(FReg.Value.GetValueAsBoolean);

  Result := True;
end;

function TLRCRRegIniFile.EraseSectionProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  aReg.CloseKey;
  Result := aReg.DeleteKey(aOpenKey);
end;

function TLRCRRegIniFile.DeleteKeyProc(aReg: TRegistry;
  aOpenKey: string): boolean;
begin
  Result := aReg.DeleteValue(FReg.Ident);
end;

constructor TLRCRRegIniFile.Create(const aFileName: string;
  aRoot: HKEY = HKEY_CURRENT_USER);
begin
  FFilename := IncludeTrailingPathDelimiter(aFileName);
  FRoot := aRoot;
  FReg := TRegUtils.Create;
end;

destructor TLRCRRegIniFile.Destroy;
begin
  FFilename := EmptyStr;

  if Assigned(FReg) then
    FreeAndNil(FReg);

  inherited Destroy;
end;

function TLRCRRegIniFile.DeleteKey(const aSection: string;
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

procedure TLRCRRegIniFile.EraseSection(const aSection: string);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;

      success :=
        GetRegistry(FRoot, Filename + aSection, EraseSectionProc, False);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRRegIniFile.ReadBool(const aSection: string;
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
        GetRegistry(FRoot, Filename + aSection, ReadBoolProc, False);

      if success then
        Result := FFuncResult.GetValueAsBoolean
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

function TLRCRRegIniFile.ReadInteger(const aSection: string;
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
        GetRegistry(FRoot, Filename + aSection, ReadIntegerProc, False);

      if success then
        Result := FFuncResult.GetValueAsInteger
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

function TLRCRRegIniFile.ReadString(const aSection: string;
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
        GetRegistry(FRoot, Filename + aSection, ReadStringProc, False);

      if success then
        Result := FFuncResult.GetValueAsString
      else
        Result := aDefault;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.ReadSection(const aSection: string;
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
        GetRegistry(FRoot, Filename + aSection, ReadSectionProc, False);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.ReadSections(aStrings: TStrings);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      SetStrings(aStrings);

      success := GetRegistry(FRoot, Filename, ReadSectionsProc, False);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.ReadSectionValues(const aSection: string;
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
        GetRegistry(FRoot, Filename + aSection, ReadSectionValuesProc, False);

      if success then
        aStrings := GetStrings;
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.WriteBool(const aSection: string;
  const aIdent: string;
  aValue: Boolean);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByBoolean(aValue);

      success :=
        GetRegistry(FRoot, Filename + aSection, WriteBoolProc, False);
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.WriteInteger(const aSection: string;
  const aIdent: string;
  aValue: Longint);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByInteger(aValue);

      success :=
        GetRegistry(FRoot, Filename + aSection, WriteIntegerProc, False);
    finally
      Refresh;
    end;
  end;
end;

procedure TLRCRRegIniFile.WriteString(const aSection: string;
  const aIdent: string;
  const aValue: string);
var
  success: boolean;
begin
  with FReg do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Value.SetValueByString(aValue);

      success :=
        GetRegistry(FRoot, Filename + aSection, WriteStringProc, False);
    finally
      Refresh;
    end;
  end;
end;

function TLRCRRegIniFile.GetFilename: string;
begin
  if (Trim(FFilename) <> EmptyStr) then
    Result := IncludeLeadingPathDelimiter(FFilename)
  else
    Result := EmptyStr;
end;

class function TRegUtils.GetInstance: TRegUtils;
begin
  if not Assigned(reg_util_instance) then
    reg_util_instance := TRegUtils.Create;

  Result := reg_util_instance;
end;

procedure TRegUtils.SetStrings(aStrings: TStrings);
begin
  FStrings := aStrings;
end;

function TRegUtils.GetStrings: TStrings;
begin
  Result := FStrings;
end;

{ TRegUtils }

function TRegUtils.GetRegistry(aRoot: HKEY;
  aRootKey: string;
  aProc: THandleRegistry;
  aOpenKeyReadOnly: boolean = True;
  aCanCreate: boolean = False): boolean;
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
      FreeAndNil(reg);
  end
end;

procedure TRegUtils.Refresh;
begin
  FSection := EmptyStr;
  FIdent := EmptyStr;
  FOpenReadOnly := True;
  FCanCreate := False;
end;

{ TDefaultsForCurrentUser }

function TDefaultsForCurrentUser.SectionExistsProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  key_to_check: string;
  section_to_check: string;
begin
  Result := False;

  with TRegUtils.GetInstance do
  begin
    section_to_check := Section;
  end;

  with aReg do
  begin
    key_to_check := concat(CurrentPath, section_to_check);

    Result := KeyExists(key_to_check);
  end;
end;

function TDefaultsForCurrentUser.IdentExistsProc(aReg: TRegistry;
  aOpenKey: string): boolean;
var
  key_to_check: string;
  section_to_check: string;
  ident_to_check: string;
begin
  Result := False;

  with TRegUtils.GetInstance do
  begin
    section_to_check := Section;
    ident_to_check := Ident;
  end;

  with aReg do
  begin
    key_to_check := concat(CurrentPath, section_to_check);

    if KeyExists(key_to_check) then
      if OpenKeyReadOnly(key_to_check) then
        Result := ValueExists(ident_to_check);
  end;
end;

procedure TDefaultsForCurrentUser.ReadSectionValuesByKind(aSection: string;
  aStrings: TStrings;
  aKind: TListSourceKind = Both;
  aMerge: boolean = False);

  procedure AddString(aStrings: TStrings;
                      aValue: string;
                      aMerge: boolean);
  var
    index: Integer;
    value_name: string;
    pos_index: SizeInt;
  begin
     if aMerge then
     begin
       pos_index := Pos('=', aValue);
       if (pos_index = 0) then
         index := aStrings.IndexOf(aValue)
       else
       begin
         value_name := Copy(aValue, 1, pos_index-1);
         index := aStrings.IndexOfName(value_name);
       end;
       if (index = -1) then
         aStrings.Add(aValue);
     end
     else
       aStrings.Add(aValue);
  end;
var
  list: TStrings;
  reg: TRegistry;
  key: string;
  anz: integer;
  value: string;
  value_name: string;
  value_name_utf8_decoded: string;
  value_data_type: TRegDataType;
begin
  reg := TRegistry.Create;
  list := TStringList.Create;
  try
    try
      if not aMerge then
        aStrings.Clear;
      with reg do
      begin
        RootKey := GetHKEYRoot;

        aSection := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
        begin
          GetValueNames(list);

          // Aus dieser Liste werden die Idents entnommen,
          // eventuell besser auf eine Umwandlung verzichten
          SysToUTF8StringsIfNeeded(list, CheckRTLAnsi);

          for anz := 0 to list.Count-1 do
  	  begin
            value_name := list.Strings[anz];
            value_name_utf8_decoded := UTF8ToSysIfNeeded(value_name, CheckRTLAnsi);
            value_name_utf8_decoded := UTF8DecodeIfNeeded(value_name_utf8_decoded, CheckRTLAnsi);

            value_data_type := GetDataType(value_name);

            case value_data_type of
              rdString,
              rdExpandString:
              begin
                value := reg.ReadString(value_name_utf8_decoded);

                if CheckRTLAnsi then
                  if NeedRTLAnsi then
                    value := SysToUTF8(value);

              end;
              rdBinary:
                Continue;
              rdInteger:
                value := IntToStr(ReadInteger(value_name));
            else
              Continue;
            end;

            case aKind of
              byValue: AddString(aStrings, value, aMerge);
              byKey: AddString(aStrings, value_name, aMerge);
              Both: AddString(aStrings, value_name + '=' + value, aMerge);
            else
              Break;
            end;
  	  end;
        end;

        CloseKey;
      end;
    except
      on E: Exception do
        aStrings.Clear;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

function TDefaultsForCurrentUser.GetDefaultKey: string;
begin
  Result := IncludeTrailingPathDelimiter(FDefaultKey);
end;

function TDefaultsForCurrentUser.GetHKeyRoot: HKEY;
begin
  if (UpperCase(Trim(Root)) = 'HKEY_CLASSES_ROOT') then
  begin
    Result := HKEY_CLASSES_ROOT;
    Exit;
  end;

  if (UpperCase(Trim(Root)) = 'HKEY_CURRENT_USER') then
  begin
    Result := HKEY_CURRENT_USER;
    Exit;
  end;

  if (UpperCase(Trim(Root)) = 'HKEY_LOCAL_MACHINE') then
  begin
    Result := HKEY_LOCAL_MACHINE;
    Exit;
  end;

  if (UpperCase(Trim(Root)) = 'HKEY_USERS') then
  begin
    Result := HKEY_USERS;
    Exit;
  end;

  if (UpperCase(Trim(Root)) = 'HKEY_CURRENT_CONFIG') then
  begin
    Result := HKEY_CURRENT_CONFIG;
    Exit;
  end;

  if (UpperCase(Trim(Root)) = 'HKEY_DYN_DATA') then
  begin
    Result := HKEY_DYN_DATA;
    Exit;
  end;
end;

function TDefaultsForCurrentUser.SectionExists(aSection: string): boolean;
begin
  with TRegUtils.GetInstance do
  begin
    Refresh;
    try
      Section := aSection;
      Result := GetRegistry(GetHKeyRoot, DefaultKey, SectionExistsProc);
    finally
      Refresh;
    end;
  end;
end;

function TDefaultsForCurrentUser.IdentExists(aSection: string;
  aIdent: string): boolean;
begin
  with TRegUtils.GetInstance do
  begin
    Refresh;
    try
      Section := aSection;
      Ident := aIdent;
      Result := GetRegistry(GetHKeyRoot, DefaultKey, IdentExistsProc);
    finally
      Refresh;
    end;
  end;
end;

function TDefaultsForCurrentUser.DeleteKey(aSection: string): boolean;
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded: string;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        key := concat(DefaultKey, section_utf8_decoded);

        Result := KeyExists(key);

        CloseKey;
      end;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

function TDefaultsForCurrentUser.DeleteValue(aSection: string;
  aKey: string): boolean;
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded: string;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded :=  UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        key := concat(DefaultKey, section_utf8_decoded);

        if OpenKey(key, True) then
          Result := DeleteValue(aKey);

        CloseKey;
      end;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDefaultsForCurrentUser.RenameKey(aSection: string;
  aOldKey: string;
  aNewKey: string);
var
  value_str: string;
  value_int: integer;
  value_data_type: TRegDataType;
  reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create;
  try
    with reg do
    begin
      RootKey := GetHKEYRoot;

      aSection := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
      key := concat(DefaultKey, aSection);

      aOldKey := UTF8DecodeIfNeeded(aOldKey, CheckRTLAnsi);
      aNewKey := UTF8DecodeIfNeeded(aNewKey, CheckRTLAnsi);

      if OpenKey(key, True) then
      begin
        value_data_type := reg.GetDataType(aOldKey);

        case value_data_type of
          rdString,
          rdExpandString:
          begin
            value_str := ReadString(aOldKey);

            // wird nicht benötigt
            //value_str := SysToUTF8IfNeeded(value_str, CheckRTLAnsi);
          end;
          rdInteger:
            value_int := ReadInteger(aOldKey);
        end;
      end;

      DeleteValue(aOldKey);

      case value_data_type of
        rdString,
        rdExpandString:
        begin
          // wird nicht benötigt
          //value_str := UTF8ToSysIfNeeded(value_str);

          WriteString(aNewKey, value_str);
        end;
        rdInteger:
          WriteInteger(aNewKey, value_int);
      end;

       CloseKey;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

constructor TDefaultsForCurrentUser.Create(aRoot: string;
  aDefaultKey: string;
  aPrefereString: boolean = False;
  aCheckRTLAnsi: boolean = True);
begin
  inherited Create;

  FRoot := aRoot;
  FDefaultKey := aDefaultKey;
  FPrefereStrings := aPrefereString;
  FCheckRTLAnsi := aCheckRTLAnsi;
end;

destructor TDefaultsForCurrentUser.Destroy;
begin
  FRoot := '';
  FDefaultKey := '';

  inherited Destroy;
end;

function TDefaultsForCurrentUser.ReadString(aSection: string;
  aIdent: string;
  aDefault: string): string;
var
  reg: TRegistry;
  key: string;
begin
  Result := aDefault;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        aSection := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        aIdent := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
          if ValueExists(aIdent) then
          begin
            Result := ReadString(aIdent);

            Result := SysToUTF8IfNeeded(Result, CheckRTLAnsi)
          end;

        CloseKey;
      end;
    except
      on E: Exception do
        Result := aDefault;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

function TDefaultsForCurrentUser.ReadInteger(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded: string;
  ident_utf8_decoded: string;
begin
  Result := aDefault;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        ident_utf8_decoded := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);
        key := concat(DefaultKey, section_utf8_decoded);

        if OpenKeyReadOnly(key) then
        begin
          if ValueExists(ident_utf8_decoded) then
          begin
            if FPrefereStrings then
              Result := StrToInt(ReadString(ident_utf8_decoded))
            else
              Result := ReadInteger(ident_utf8_decoded);
          end;
        end;

        CloseKey;
      end;
    except
      on E: Exception do
        Result := aDefault;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

function TDefaultsForCurrentUser.ReadBool(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded : string;
  ident_utf8_decoded : string;
begin
  Result := aDefault;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        ident_utf8_decoded := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

        key := concat(DefaultKey, section_utf8_decoded);

        if OpenKeyReadOnly(key) then
        begin
          if ValueExists(ident_utf8_decoded) then
          begin
            if FPrefereStrings then
              Result := StrToInt(ReadString(ident_utf8_decoded)) <> 0
            else
              Result := ReadBool(ident_utf8_decoded);
          end;
        end;

        CloseKey;
      end;
    except
      on E: Exception do
        Result := aDefault;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDefaultsForCurrentUser.ReadSectionValuesOnly(aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, byValue, aMerge);
end;

procedure TDefaultsForCurrentUser.ReadSection(aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, byKey, aMerge);
end;

procedure TDefaultsForCurrentUser.ReadSectionValues(aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, Both, aMerge);
end;

procedure TDefaultsForCurrentUser.WriteString(aSection: string;
  aIdent: string;
  aString: string);
var
  reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        aSection := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);

        key := concat(DefaultKey, aSection);

        if OpenKey(key, True) then
        begin
          aIdent := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);
          aString := UTF8DecodeIfNeeded(aString, CheckRTLAnsi);

          WriteString(aIdent, aString);
        end;

        CloseKey;
      end;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDefaultsForCurrentUser.WriteInteger(aSection: string;
  aIdent: string;
  aInteger: integer);
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded : string;
  ident_utf8_decoded : string;
begin
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        ident_utf8_decoded := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

        key := concat(DefaultKey, section_utf8_decoded);

        if OpenKey(key, True) then
        begin
          if FPrefereStrings then
            WriteString(ident_utf8_decoded, IntToStr(aInteger))
          else
            WriteInteger(ident_utf8_decoded, aInteger);
        end;

        CloseKey;
      end;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDefaultsForCurrentUser.WriteBool(aSection: string;
  aIdent: string;
  aBool: boolean);
var
  reg: TRegistry;
  key: string;
  section_utf8_decoded : string;
  ident_utf8_decoded : string;
begin
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        ident_utf8_decoded := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

        key := concat(DefaultKey, section_utf8_decoded);

        if OpenKey(key, True) then
        begin
          if FPrefereStrings then
          begin
            if aBool then
              WriteString(ident_utf8_decoded, '1')
            else
              WriteString(ident_utf8_decoded, '0');
            end
          else
            WriteBool(ident_utf8_decoded, aBool);
        end;

        CloseKey;
      end;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

{ TDataByCurrentUser }

constructor TDataByCurrentUser.Create(aFileName: string;
  aDefaultsRoot: string;
  aDefaultKey: string;
  aPrefereStrings: boolean = False;
  aCheckRTLAnsi: boolean = True);
begin
  Create(aFileName, aPrefereStrings, aCheckRTLAnsi);

  FUseDefaults :=
    TDefaultsForCurrentUser.Create(aDefaultsRoot, aDefaultKey, aPrefereStrings);
end;

constructor TDataByCurrentUser.Create(aFileName: string;
  aPrefereStrings: boolean = False;
  aCheckRTLAnsi: boolean = True);
var
  file_name: string;
begin
  file_name := UTF8DecodeIfNeeded(aFileName, aCheckRTLAnsi);

  inherited Create(file_name);

  CheckRTLAnsi := aCheckRTLAnsi;
  PreferStringValues := aPrefereStrings;
end;

destructor TDataByCurrentUser.Destroy;
begin
  FUseDefaults.Free;

  inherited Destroy;
end;

procedure TDataByCurrentUser.ReadSectionValuesByKind(aSection: string;
  aStrings: TStrings;
  aKind: TListSourceKind);
var
  list: TStringList;
  anz: Integer;
  section_utf8_decoded: string;
  value: String;
  value_name: string;
  value_name_utf8_decoded: string;
  value_data_type: TRegDataType;
  reg: TRegistry;
  key: string;
begin
  aStrings.Clear;
  list := TStringlist.Create;
  try
    case aKind of
      byKey:
        ReadSection(aSection, aStrings);
      byValue,
      Both:
      begin
        ReadSection(aSection, list);

        reg := TRegistry.Create;
        reg.RootKey := HKEY_CURRENT_USER;

        section_utf8_decoded := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);

        key := Concat(FileName + section_utf8_decoded);

        if reg.OpenKeyReadOnly(key) then
        begin
          for anz := 0 to list.Count-1 do
          begin
            value_name := list[anz];
            value_name_utf8_decoded := UTF8ToSysIfNeeded(value_name, CheckRTLAnsi);
            value_name_utf8_decoded := UTF8DecodeIfNeeded(value_name_utf8_decoded, CheckRTLAnsi);

            value_data_type := reg.GetDataType(value_name_utf8_decoded);

            case value_data_type of
              rdString,
              rdExpandString:
                value := ReadString(aSection, UTF8ToSysIfNeeded(value_name, CheckRTLAnsi), EmptyStr);
              rdBinary:
                Continue;
              rdInteger:
                value := IntToStr(ReadInteger(aSection, UTF8ToSysIfNeeded(value_name, CheckRTLAnsi), $FFFFFF));
            else
              Continue;
            end;

            if (aKind = Both) then
              value := value_name + '=' + value;

            aStrings.Add(value);
          end;
        end;

        reg.CloseKey;
      end;
    else
      Exit;
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TDataByCurrentUser.EraseSection(const Section: string);
var
  section_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);

  inherited EraseSection(section_str);
end;

procedure TDataByCurrentUser.DeleteKey(const Section: String;
  const Ident: String);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);

  inherited DeleteKey(section_str, ident_str);
end;

procedure TDataByCurrentUser.ReadSections(Strings: TStrings);
begin
  inherited ReadSections(Strings);

  SysToUTF8StringsIfNeeded(Strings, CheckRTLAnsi);
end;

procedure TDataByCurrentUser.ReadSection(const Section: string;
  Strings: TStrings);
var
  section_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);

  inherited ReadSection(section_str, Strings);

  SysToUTF8StringsIfNeeded(Strings, CheckRTLAnsi);
end;

procedure TDataByCurrentUser.WriteString(const Section: string;
  const Ident: string;
  const Value: String);
var
  section_str: string;
  ident_str: string;
  value_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);
  value_str := UTF8DecodeIfNeeded(Value, CheckRTLAnsi);

  inherited WriteString(section_str, ident_str, value_str);
end;

function TDataByCurrentUser.ReadString(const Section: string;
  const Ident: string;
  const Default: string): string;
var
  section_str: string;
  ident_str: string;
  default_str: string;
  value: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);
  default_str := UTF8DecodeIfNeeded(Default, CheckRTLAnsi);

  value := inherited ReadString(section_str, ident_str, default_str);

  Result := SysToUTF8IfNeeded(value, CheckRTLAnsi);
end;

function TDataByCurrentUser.ReadInteger(const Section: string;
  const Ident: string;
  Default: Longint): Longint;
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);

  Result := inherited ReadInteger(section_str, ident_str, Default);
end;

function TDataByCurrentUser.ReadBool(const Section: string;
  const Ident: string;
  Default: Boolean): Boolean;
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);

  Result := inherited ReadBool(section_str, ident_str, Default);
end;

procedure TDataByCurrentUser.WriteBool(const Section: string;
  const Ident: string;
  Value: Boolean);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);

  inherited WriteBool(section_str, ident_str, Value);
end;

procedure TDataByCurrentUser.WriteInteger(const Section: string;
  const Ident: string;
  Value: Longint);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(Section, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(Ident, CheckRTLAnsi);

  inherited WriteInteger(section_str, ident_str, Value);
end;

function TDataByCurrentUser.SectionExistsForDefaults(aSection: string): boolean;
begin
  Result := UseDefaults.SectionExists(aSection);
end;

function TDataByCurrentUser.SectionExists(aSection: string): boolean;
var
  key_to_check: string;
begin
  key_to_check :=
    concat(IncludeLeadingPathDelimiter(FileName),
      UTF8DecodeIfNeeded(aSection, CheckRTLAnsi));

  Result := KeyExists(key_to_check);
end;

function TDataByCurrentUser.IdentExistsForDefaults(aSection: string;
  aIdent: string): boolean;
begin
  Result := UseDefaults.IdentExists(aSection, aIdent);
end;

function TDataByCurrentUser.IdentExists(aSection: string;
  aIdent: string): boolean;
var
  key_to_open: string;
  ident_to_check: string;
begin
  if SectionExists(aSection) then
  begin
    key_to_open :=
      concat(IncludeLeadingPathDelimiter(FileName),
        UTF8DecodeIfNeeded(aSection, CheckRTLAnsi));
    ident_to_check := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

    if OpenKeyReadOnly(key_to_open) then
    begin
      Result := ValueExists(ident_to_check);
      CloseKey;
    end;
  end;
end;

procedure TDataByCurrentUser.EraseSectionForDefaults(aSection: string);
begin
  EraseSection(aSection);
  UseDefaults.DeleteKey(aSection);
end;

procedure TDataByCurrentUser.DeleteKeyForDefaults(aSection: string;
  aKey: string);
begin
  DeleteKey(aSection, aKey);
  UseDefaults.DeleteValue(aSection, aKey);
end;

procedure TDataByCurrentUser.RenameKey(aSection: string;
  aOldKey: string;
  aNewKey: string);
var
  value_str: string;
  value_int: integer;
  value_data_type: TRegDataType;
  reg: TRegistry;
  key: string;
  sec: string;
  old_key: string;
begin
  sec := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  old_key := UTF8DecodeIfNeeded(aOldKey, CheckRTLAnsi);
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    key := Concat(FileName + sec);

    if reg.OpenKeyReadOnly(key) then
    begin
      value_data_type := reg.GetDataType(old_key);

      case value_data_type of
        rdString,
        rdExpandString:
        begin
          value_str := ReadString(aSection, aOldKey, EmptyStr);

          if CheckRTLAnsi then
            if NeedRTLAnsi then
              value_str := SysToUTF8(value_str);
        end;
        rdInteger:
          value_int := ReadInteger(aSection, aOldKey, $FFFFFF);
      end;
    end;
    reg.CloseKey;

    DeleteKey(aSection, aOldKey);

    case value_data_type of
      rdString,
      rdExpandString:
      begin
        if CheckRTLAnsi then
          if NeedRTLAnsi then
            value_str := UTF8ToSys(value_str);

        WriteString(aSection, aNewKey, value_str);
      end;
      rdInteger:
        WriteInteger(aSection, aNewKey, value_int);
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDataByCurrentUser.RenameKeyForDefaults(aSection: string;
  aOldKey: string;
  aNewKey: string);
begin
  RenameKey(aSection, aOldKey, aNewKey);
  UseDefaults.RenameKey(aSection, aOldKey, aNewKey);
end;

procedure TDataByCurrentUser.ReadSectionValuesEx(aSection: string;
  aStrings: TStrings);
begin
  ReadSectionValuesByKind(aSection, aStrings, Both);
end;

procedure TDataByCurrentUser.ReadSectionValuesOnly(aSection: string;
  aStrings: TStrings);
begin
  ReadSectionValuesByKind(aSection, aStrings, byValue);
end;

procedure TDataByCurrentUser.ReadSectionValuesOnlyForDefaults(aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  try
    aStrings.Clear;

    ReadSectionValuesOnly(aSection, aStrings);

    if (aStrings.Count = 0) then
      UseDefaults.ReadSectionValuesOnly(aSection, aStrings, False)
    else
      if aMerge then
        UseDefaults.ReadSectionValuesOnly(aSection, aStrings, True);
  except
    on E: Exception do
      aStrings.Clear;
  end;
end;

function TDataByCurrentUser.ReadStringCheckForDefaults(aSection: string;
  aIdent: string;
  aDefault: string): string;
begin
  Result := ReadString(aSection,
              aIdent,
              UseDefaults.ReadString(aSection, aIdent, aDefault));
end;

function TDataByCurrentUser.ReadIntegerCheckForDefaults(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
begin
  Result := ReadInteger(aSection,
              aIdent,
              UseDefaults.ReadInteger(aSection, aIdent, aDefault));
end;

function TDataByCurrentUser.ReadBoolCheckForDefaults(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
begin
  Result := ReadBool(aSection,
              aIdent,
              UseDefaults.ReadBool(aSection, aIdent, aDefault));
end;

procedure TDataByCurrentUser.ReadSectionCheckForDefaults(aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  try
    aStrings.Clear;

    ReadSection(aSection, aStrings);

    if (aStrings.Count = 0) then
      UseDefaults.ReadSection(aSection, aStrings, False)
    else
      if aMerge then
        UseDefaults.ReadSection(aSection, aStrings, True);
  except
    on E: Exception do
      aStrings.Clear;
  end;
end;

procedure TDataByCurrentUser.ReadSectionValuesCheckForDefaults(
  aSection: string;
  aStrings: TStrings;
  aMerge: boolean);
begin
  try
    aStrings.Clear;

    ReadSectionValuesEx(aSection, aStrings);

    if aStrings.Count = 0 then
      UseDefaults.ReadSectionValues(aSection, aStrings, False)
    else
      if aMerge then
        UseDefaults.ReadSectionValues(aSection, aStrings, True);
  except
    on E: Exception do
      aStrings.Clear;
  end;
end;

procedure TDataByCurrentUser.WriteStringCheckForDefaults(aSection: string;
  aIdent: string;
  aString: string);
begin
  WriteString(aSection, aIdent, aString);
  UseDefaults.WriteString(aSection, aIdent, aString);
end;

procedure TDataByCurrentUser.WriteIntegerCheckForDefaults(aSection: string;
  aIdent: string;
  aInteger: integer);
begin
  WriteInteger(aSection, aIdent, aInteger);
  UseDefaults.WriteInteger(aSection, aIdent, aInteger);
end;

procedure TDataByCurrentUser.WriteBoolCheckForDefaults(aSection: string;
  aIdent: string;
  aBool: boolean);
begin
  WriteBool(aSection, aIdent, aBool);
  UseDefaults.WriteBool(aSection, aIdent, aBool);
end;

end.
