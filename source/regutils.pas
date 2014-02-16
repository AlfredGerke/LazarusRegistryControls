unit regutils;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  Registry,
  regtype;

type

  { TDefaultsForCurrentUser }

  TDefaultsForCurrentUser = class
  private
    FRoot: string;
    FDefaultKey: string;
    FPrefereStrings: boolean;
    FCheckRTLAnsi: boolean;

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
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure WriteString(const Section, Ident, Value: String);
    function ReadString(const Section, Ident, Default: string): string;

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

procedure SysToUTF8Strings(aStrings: TStrings);
procedure UTF8ToSysStrings(aStrings: TStrings);

implementation

uses
  SysUtils,
  FileUtil;

procedure ConvertStrings(aStrings: TStrings;
  aTarget: TStrConvertTarget);
var
  anz: integer;
  list: TStrings;
  item: string;
begin
  list := TStringList.Create;
  try
    list.AddStrings(aStrings);
    aStrings.Clear;
    for anz := 0 to list.count-1 do
    begin
      case aTarget of
        sctToUTF8: item := SysToUTF8(list.strings[anz]);
        sctToAnsi: item := UTF8ToSys(list.strings[anz]);
      else
        item := list.strings[anz];
      end;

      aStrings.add(item);
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure SysToUTF8Strings(aStrings: TStrings);
begin
  ConvertStrings(aStrings, sctToUTF8);
end;

procedure UTF8ToSysStrings(aStrings: TStrings);
begin
  ConvertStrings(aStrings, sctToAnsi);
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
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
        begin
          GetValueNames(list);

 	 for anz := 0 to list.Count-1 do
 	 begin
           value_name := list.Strings[anz];
           value_data_type := GetDataType(value_name);

           case value_data_type of
             rdString:
               value := reg.ReadString(value_name);
             rdExpandString:
               value := reg.ReadString(value_name);
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

function TDefaultsForCurrentUser.DeleteKey(aSection: string): boolean;
var
  reg: TRegistry;
  key: string;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if KeyExists(key) then
          Result := DeleteKey(key);

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
begin
  Result := False;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

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
      key := concat(DefaultKey, aSection);

      if OpenKey(key, True) then
      begin
        value_data_type := reg.GetDataType(aOldKey);

        case value_data_type of
          rdString:
            value_str := ReadString(aOldKey);
          rdExpandString:
            value_str := ReadString(aOldKey);
          rdInteger:
            value_int := ReadInteger(aOldKey);
        end;
      end;

      DeleteValue(aOldKey);

      case value_data_type of
        rdString:
          WriteString(aNewKey, value_str);
        rdExpandString:
          WriteString(aNewKey, value_str);
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
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
          if ValueExists(aIdent) then
            Result := ReadString(aIdent);

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
begin
  Result := aDefault;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
        begin
          if ValueExists(aIdent) then
          begin
            if FPrefereStrings then
              Result := StrToInt(ReadString(aIdent))
            else
              Result := ReadInteger(aIdent);
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
begin
  Result := aDefault;
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
        begin
          if ValueExists(aIdent) then
          begin
            if FPrefereStrings then
              Result := StrToInt(ReadString(aIdent)) <> 0
            else
              Result := ReadBool(aIdent);
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
        key := concat(DefaultKey, aSection);

        if OpenKey(key, True) then
          WriteString(aIdent, aString);

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
begin
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if OpenKey(key, True) then
        begin
          if FPrefereStrings then
            WriteString(aIdent, IntToStr(aInteger))
          else
            WriteInteger(aIdent, aInteger);
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
begin
  reg := TRegistry.Create;
  try
    try
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if OpenKey(key, True) then
        begin
          if FPrefereStrings then
          begin
            if aBool then
              WriteString(aIdent, '1')
            else
              WriteString(aIdent, '0');
            end
          else
            WriteBool(aIdent, aBool);
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
  inherited Create(aFileName);

  CheckRTLAnsi := aCheckRTLAnsi;
  PreferStringValues := aPrefereStrings;
  FUseDefaults :=
    TDefaultsForCurrentUser.Create(aDefaultsRoot, aDefaultKey, aPrefereStrings);
end;

constructor TDataByCurrentUser.Create(aFileName: string;
  aPrefereStrings: boolean = False;
  aCheckRTLAnsi: boolean = True);
begin
  inherited Create(aFileName);

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
  value: String;
  value_name: string;
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
        key := Concat(FileName + aSection);

        if reg.OpenKeyReadOnly(key) then
        begin
          for anz := 0 to list.Count-1 do
          begin
            value_name := list[anz];
            value_data_type := reg.GetDataType(value_name);

            case value_data_type of
              rdString:
                value := ReadString(aSection, value_name, EmptyStr);
              rdExpandString:
                value := ReadString(aSection, value_name, EmptyStr);
              rdBinary:
                Continue;
              rdInteger:
                value := IntToStr(ReadInteger(aSection, value_name, $FFFFFF));
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

procedure TDataByCurrentUser.ReadSection(const Section: string;
  Strings: TStrings);
begin
  inherited ReadSection(Section, Strings);
  if CheckRTLAnsi then
    if NeedRTLAnsi then
      SysToUTF8Strings(Strings);
end;

procedure TDataByCurrentUser.WriteString(const Section, Ident, Value: String);
begin
  inherited WriteString(Section, Ident, Value);
end;

function TDataByCurrentUser.ReadString(const Section, Ident, Default: string
  ): string;
var
  value: string;
begin
  value := inherited ReadString(Section, Ident, Default);
  if CheckRTLAnsi then
    if NeedRTLAnsi then
      value:= SysToUTF8(value);
  Result := value;
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
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    key := Concat(FileName + aSection);

    if reg.OpenKeyReadOnly(key) then
    begin
      value_data_type := reg.GetDataType(aOldKey);

      case value_data_type of
        rdString:
          value_str := ReadString(aSection, aOldKey, EmptyStr);
        rdExpandString:
          value_str := ReadString(aSection, aOldKey, EmptyStr);
        rdInteger:
          value_int := ReadInteger(aSection, aOldKey, $FFFFFF);
      end;
    end;
    reg.CloseKey;

    DeleteKey(aSection, aOldKey);

    case value_data_type of
      rdString:
        WriteString(aSection, aNewKey, value_str);
      rdExpandString:
        WriteString(aSection, aNewKey, value_str);
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
