unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  regtype;

type

  { TRegistrySourceGenericTest }

  TRegistrySourceGenericTest<_T1>= class(TTestCase)
  private
    FRegSrcWrapper: _T1;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    property RegSrcWrapper: _T1
      read FRegSrcWrapper;
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

  { TRegistrySourceGenericTest }

  TRegistrySourceTest = class(TRegistrySourceGenericTest<TRegistrySourceWrapper>)
  end;

  { TRegistrySourceUTF8Test }
  TRegistrySourceUTF8Test = class(TRegistrySourceGenericTest<TRegistrySourceWrapperUTF8>)
  end;

implementation

procedure TRegistrySourceGenericTest<_T1>.PublishedProperties;
begin
  FRegSrcWrapper.PublishedProperties;
end;

procedure TRegistrySourceGenericTest<_T1>.RootKeysStruct;
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

procedure TRegistrySourceGenericTest<_T1>.DeleteRootKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.GetRootKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.GetRootKeyForDefaults;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.GetRootKeyForCommon;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.ReadString;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.ReadInteger;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.ReadBool;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.ReadSection;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.WriteString;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.WriteInteger;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.WriteBool;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.RenameKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.EraseSection;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.SetUp;
begin
  FRegSrcWrapper := _T1.Create;
end;

procedure TRegistrySourceGenericTest<_T1>.TearDown;
begin
  FreeAndNil(FRegSrcWrapper);
end;

end.

