unit regutils;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  Registry,
  regtype,
  regbasics;

type

  { TDefaultsForCurrentUser }

  TDefaultsForCurrentUser = class(TLRCRegInifile)
  private
    FCheckRTLAnsi: boolean;

    procedure ReadSectionValuesByKind(aSection: string;
      aStrings: TStrings;
      aKind: TListSourceKind = Both;
      aMerge: boolean = False);
  protected
    property CheckRTLAnsi: boolean
      read FCheckRTLAnsi
      write FCheckRTLAnsi;
  public
    function SectionExists(const aSection: string): boolean;
    function IdentExists(const aSection: string;
                         const aIdent: string): boolean;
    function DeleteKey(const aSection: string): boolean; overload;
    function DeleteValue(const aSection: string;
                         const aKey: string): boolean;
    procedure RenameKey(const aSection: string;
                        const aOldKey: string;
                        const aNewKey: string);
    function ReadString(const aSection: string;
                        const aIdent: string;
                        const aDefault: string): string;
    function ReadInteger(const aSection: string;
                         const aIdent: string;
                         const aDefault: integer): integer;
    function ReadBool(const aSection: string;
                      const aIdent: string;
                      aDefault: boolean): boolean;
    procedure ReadSectionValuesOnly(const aSection: string;
                                    const aStrings: TStrings;
                                    aMerge: boolean);
    procedure ReadSection(const aSection: string;
                          const aStrings: TStrings;
                          aMerge: boolean);
    procedure ReadSectionValues(const aSection: string;
                                const aStrings: TStrings;
                                aMerge: boolean); virtual;
    procedure WriteString(const aSection: string;
                          const aIdent: string;
                          const aString: string);
    procedure WriteInteger(const aSection: string;
                           const aIdent: string;
                           aInteger: integer);
    procedure WriteBool(const aSection: string;
                        const aIdent: string;
                        aBool: boolean);

    constructor Create(aRoot: string;
                       aDefaultKey: string;
                       aPrefereString: boolean = False;
                       aCheckRTLAnsi: boolean = True); virtual;
    destructor Destroy; override;
  end;

  { TDataByCurrentUser }

  TDataByCurrentUser = class(TLRCRegInifile)
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
    // überschrieben aus TLRCRegInifile/TRegistry
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

{ TDefaultsForCurrentUser }

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
        RootKey := Root;

        aSection := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
        key := concat(Filename, aSection);

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

function TDefaultsForCurrentUser.SectionExists(const aSection: string): boolean;
var
  section_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);

  Result := KeyExists(section_str);
end;

function TDefaultsForCurrentUser.IdentExists(const aSection: string;
  const aIdent: string): boolean;
var
  key_to_open: string;
  ident_to_check: string;
begin
  if SectionExists(aSection) then
  begin
    key_to_open := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
    ident_to_check := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

    Result := ValueExists(key_to_open, ident_to_check, True);
  end;
end;

function TDefaultsForCurrentUser.DeleteKey(const aSection: string): boolean;
var
  section_str: string;
begin
  Result := False;

  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);

  Result := EraseSection(section_str);
end;

function TDefaultsForCurrentUser.DeleteValue(const aSection: string;
  const aKey: string): boolean;
var
  section_str: string;
  ident_str: string;
begin
  Result := False;

  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aKey, CheckRTLAnsi);

  Result := DeleteKey(section_str, ident_str);
end;

procedure TDefaultsForCurrentUser.RenameKey(const aSection: string;
  const aOldKey: string; const aNewKey: string);
var
  section_str: string;
  old_key_str: string;
  new_key_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  old_key_str :=  UTF8DecodeIfNeeded(aOldKey, CheckRTLAnsi);
  new_key_str :=  UTF8DecodeIfNeeded(aNewKey, CheckRTLAnsi);

  RenameIdent(section_str, old_key_str, new_key_str);
end;

constructor TDefaultsForCurrentUser.Create(aRoot: string;
  aDefaultKey: string;
  aPrefereString: boolean = False;
  aCheckRTLAnsi: boolean = True);
begin
  inherited Create(aDefaultKey, aRoot);

  PreferStringValues := aPrefereString;
  FCheckRTLAnsi := aCheckRTLAnsi;
end;

destructor TDefaultsForCurrentUser.Destroy;
begin
  inherited Destroy;
end;

function TDefaultsForCurrentUser.ReadString(const aSection: string;
  const aIdent: string;
  const aDefault: string): string;
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  Result := inherited ReadString(section_str, ident_str, aDefault);
end;

function TDefaultsForCurrentUser.ReadInteger(const aSection: string;
  const aIdent: string;
  const aDefault: integer): integer;
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  Result := inherited ReadInteger(section_str, ident_str, aDefault);
end;

function TDefaultsForCurrentUser.ReadBool(const aSection: string;
  const aIdent: string;
  aDefault: boolean): boolean;
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  Result := inherited ReadBool(section_str, ident_str, aDefault);
end;

procedure TDefaultsForCurrentUser.ReadSectionValuesOnly(const aSection: string;
  const aStrings: TStrings; aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, byValue, aMerge);
end;

procedure TDefaultsForCurrentUser.ReadSection(const aSection: string;
  const aStrings: TStrings; aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, byKey, aMerge);
end;

procedure TDefaultsForCurrentUser.ReadSectionValues(const aSection: string;
  const aStrings: TStrings; aMerge: boolean);
begin
  ReadSectionValuesByKind(aSection, aStrings, Both, aMerge);
end;

procedure TDefaultsForCurrentUser.WriteString(const aSection: string;
  const aIdent: string;
  const aString: string);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  inherited WriteString(section_str, ident_str, aString);
end;

procedure TDefaultsForCurrentUser.WriteInteger(const aSection: string;
  const aIdent: string;
  aInteger: integer);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  inherited WriteInteger(section_str, ident_str, aInteger);
end;

procedure TDefaultsForCurrentUser.WriteBool(const aSection: string;
  const aIdent: string; aBool: boolean);
var
  section_str: string;
  ident_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  ident_str := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

  inherited WriteBool(section_str, ident_str, aBool);
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

  inherited Create(file_name, HKEY_CURRENT_USER);

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
    key_to_open := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
    ident_to_check := UTF8DecodeIfNeeded(aIdent, CheckRTLAnsi);

    Result := ValueExists(key_to_open, ident_to_check, True);
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
  section_str: string;
  old_key_str: string;
  new_key_str: string;
begin
  section_str := UTF8DecodeIfNeeded(aSection, CheckRTLAnsi);
  old_key_str :=  UTF8DecodeIfNeeded(aOldKey, CheckRTLAnsi);
  new_key_str :=  UTF8DecodeIfNeeded(aNewKey, CheckRTLAnsi);

  RenameIdent(section_str, old_key_str, new_key_str);
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
  Result :=
    ReadString(aSection, aIdent, UseDefaults.ReadString(aSection, aIdent,
      aDefault));
end;

function TDataByCurrentUser.ReadIntegerCheckForDefaults(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
begin
  Result :=
    ReadInteger(aSection, aIdent, UseDefaults.ReadInteger(aSection, aIdent,
      aDefault));
end;

function TDataByCurrentUser.ReadBoolCheckForDefaults(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
begin
  Result :=
    ReadBool(aSection, aIdent, UseDefaults.ReadBool(aSection, aIdent,
      aDefault));
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
