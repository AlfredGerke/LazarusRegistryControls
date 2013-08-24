unit regutils;

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

    procedure ReadSectionValuesByKind(aSection: string;
                                      aStrings: TStrings;
                                      aKind: TListSourceKind = Both);
  protected
    function GetDefaultKey: string;
    function GetHKeyRoot: HKEY;
  public
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
                                    aStrings: TStrings);
    procedure ReadSection(aSection: string;
                          aStrings: TStrings); virtual;
    procedure ReadSectionValues(aSection: string;
                               aStrings: TStrings); virtual;
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
                       aPrefereString: boolean = False); virtual;
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

    procedure ReadSectionValuesByKind(aSection: string;
                                      aStrings: TStrings;
                                      aKind: TListSourceKind = Both);
  protected
  public
    procedure ReadSectionValuesEx(aSection: string;
                                  aStrings: TStrings); virtual;
    procedure ReadSectionValuesOnly(aSection: string;
                                    aStrings: TStrings);
    procedure ReadSectionValuesOnlyForDefaults(aSection: string;
                                               aStrings: TStrings);
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
                                          aStrings: TStrings); virtual;
    procedure ReadSectionValuesCheckForDefaults(aSection: string;
                                                aStrings: TStrings); virtual;
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
                       aPrefereStrings: boolean = False); reintroduce; overload;
    destructor Destroy; override;
  published
    property UseDefaults: TDefaultsForCurrentUser
      read FUseDefaults
      write FUseDefaults;
  end;

implementation

uses
  SysUtils;

{ TDefaultsForCurrentUser }

procedure TDefaultsForCurrentUser.ReadSectionValuesByKind(aSection: string;
  aStrings: TStrings;
  aKind: TListSourceKind = Both);
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
             byValue: aStrings.Add(value);
             byKey: aStrings.Add(value_name);
             Both: aStrings.Add(value_name + '=' + value);
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

constructor TDefaultsForCurrentUser.Create(aRoot: string;
  aDefaultKey: string;
  aPrefereString: boolean = False);
begin
  inherited Create;

  FRoot := aRoot;
  FDefaultKey := aDefaultKey;
  FPrefereStrings := aPrefereString;
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
  aStrings: TStrings);
begin
  ReadSectionValuesByKind(aSection, aStrings, byValue);
end;

procedure TDefaultsForCurrentUser.ReadSection(aSection: string;
  aStrings: TStrings);
var
  reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create;
  try
    try
      aStrings.Clear;
      with reg do
      begin
        RootKey := GetHKEYRoot;
        key := concat(DefaultKey, aSection);

        if OpenKeyReadOnly(key) then
          GetValueNames(aStrings);

        CloseKey;
      end;
    except
      on E: Exception do
        aStrings.Clear;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TDefaultsForCurrentUser.ReadSectionValues(aSection: string;
  aStrings: TStrings);
begin
  ReadSectionValuesByKind(aSection, aStrings, Both);
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
  aPrefereStrings: boolean = False);
begin
  inherited Create(aFileName);

  FUseDefaults :=
    TDefaultsForCurrentUser.Create(aDefaultsRoot, aDefaultKey, aPrefereStrings);
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
  aStrings: TStrings);
begin
  try
    aStrings.Clear;

    ReadSectionValuesOnly(aSection, aStrings);

    if (aStrings.Count = 0) then
      UseDefaults.ReadSectionValuesOnly(aSection, aStrings);
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
  aStrings: TStrings);
begin
  try
    aStrings.Clear;

    ReadSection(aSection, aStrings);

    if aStrings.Count = 0 then
      UseDefaults.ReadSection(aSection, aStrings);
  except
    on E: Exception do
      aStrings.Clear;
  end;
end;

procedure TDataByCurrentUser.ReadSectionValuesCheckForDefaults(
  aSection: string;
  aStrings: TStrings);
begin
  try
    aStrings.Clear;

    ReadSectionValuesEx(aSection, aStrings);

    if aStrings.Count = 0 then
      UseDefaults.ReadSectionValues(aSection, aStrings);
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
