unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regtype;

type

  { TRegistrySourceTest }

  TRegistrySourceTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PublishedProperties;
    procedure RootKeysStruct;
    procedure DeleteRootKey;
    procedure GetRootKey;
    procedure GetRootKeyForDefaults;
    procedure GetRootKeyForCommon;
    procedure ReadString;
    procedure ReadInteger;
    procedure ReadBool;
    procedure ReadSection;
    procedure WriteString;
    procedure WriteInteger;
    procedure WriteBool;
    procedure RenameKey;
    procedure DeleteKey;
    procedure EraseSection;
  end;

implementation

procedure TRegistrySourceTest.PublishedProperties;
begin
  FRegSrcWrapper.PublishedProperties;
end;

procedure TRegistrySourceTest.RootKeysStruct;
var
  root_key_struct: TRootKeysStruct;
begin
  root_key_struct.Found := True;
  root_key_struct.RootKey := 'RootKey';
  root_key_struct.RootKeyForDefaults := 'RootKeyForDefaults';
  root_key_struct.ReadDefaults := True;
  root_key_struct.WriteDefaults := True;
  root_key_struct.RootForDefaults := 'RootForDefaults';
  root_key_struct.Project := 'Project';
  root_key_struct.Organisation := 'Organisation';
  root_key_struct.GUID := 'GUID';

  AssertEquals('TRootKeysStruct.Found', True, root_key_struct.Found);
  AssertEquals('TRootKeysStruct.RootKey', 'RootKey', root_key_struct.RootKey);
  AssertEquals('TRootKeysStruct.RootKeyForDefaults', 'RootKeyForDefaults',
    root_key_struct.RootKeyForDefaults);
  AssertEquals('TRootKeysStruct.ReadDefaults', True,
    root_key_struct.ReadDefaults);
  AssertEquals('TRootKeysStruct.WriteDefaults', True,
    root_key_struct.WriteDefaults);
  AssertEquals('TRootKeysStruct.RootForDefaults', 'RootForDefaults',
    root_key_struct.RootForDefaults);
  AssertEquals('TRootKeysStruct.Project', 'Project', root_key_struct.Project);
  AssertEquals('TRootKeysStruct.Organisation', 'Organisation',
    root_key_struct.Organisation);
  AssertEquals('TRootKeysStruct.GUID', 'GUID', root_key_struct.GUID);

  root_key_struct.Clear;

  AssertEquals('TRootKeysStruct.Found', False, root_key_struct.Found);
  AssertEquals('TRootKeysStruct.RootKey', EmptyStr, root_key_struct.RootKey);
  AssertEquals('TRootKeysStruct.RootKeyForDefaults', EmptyStr,
    root_key_struct.RootKeyForDefaults);
  AssertEquals('TRootKeysStruct.ReadDefaults', False,
    root_key_struct.ReadDefaults);
  AssertEquals('TRootKeysStruct.WriteDefaults', False,
    root_key_struct.WriteDefaults);
  AssertEquals('TRootKeysStruct.RootForDefaults', EmptyStr,
    root_key_struct.RootForDefaults);
  AssertEquals('TRootKeysStruct.Project', EmptyStr, root_key_struct.Project);
  AssertEquals('TRootKeysStruct.Organisation', EmptyStr,
    root_key_struct.Organisation);
  AssertEquals('TRootKeysStruct.GUID', EmptyStr, root_key_struct.GUID);

  root_key_struct.SetRootKeys('RootKey', 'RootKeyForDefaults', True, True,
    'RootForDefaults', 'Project', 'Organisation', 'GUID');

  AssertEquals('TRootKeysStruct.Found', True, root_key_struct.Found);
  AssertEquals('TRootKeysStruct.RootKey', 'RootKey', root_key_struct.RootKey);
  AssertEquals('TRootKeysStruct.RootKeyForDefaults', 'RootKeyForDefaults',
    root_key_struct.RootKeyForDefaults);
  AssertEquals('TRootKeysStruct.ReadDefaults', True,
    root_key_struct.ReadDefaults);
  AssertEquals('TRootKeysStruct.WriteDefaults', True,
    root_key_struct.WriteDefaults);
  AssertEquals('TRootKeysStruct.RootForDefaults', 'RootForDefaults',
    root_key_struct.RootForDefaults);
  AssertEquals('TRootKeysStruct.Project', 'Project', root_key_struct.Project);
  AssertEquals('TRootKeysStruct.Organisation', 'Organisation',
    root_key_struct.Organisation);
  AssertEquals('TRootKeysStruct.GUID', 'GUID', root_key_struct.GUID);
end;

procedure TRegistrySourceTest.DeleteRootKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.GetRootKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.GetRootKeyForDefaults;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.GetRootKeyForCommon;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.ReadString;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.ReadInteger;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.ReadBool;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.ReadSection;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.WriteString;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.WriteInteger;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.WriteBool;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.RenameKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.DeleteKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.EraseSection;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
end;

procedure TRegistrySourceTest.TearDown;
begin
  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

