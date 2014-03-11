unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper;

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
begin
  FRegSrcWrapper.RootKeysStruct;
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

