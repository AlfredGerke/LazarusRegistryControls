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

    FReadSectionName: string;
    FWriteSectionName: string;
    FRenameSectionName: string;
    FStringIdent: string;
    FIntegerIdent: string;
    FBooleanIdent: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetSectionsAndIdents; virtual;

    property RegSrcWrapper: _T1
      read FRegSrcWrapper;

    property ReadSectionName: string
      read FReadSectionName
      write FReadSectionName;

    property WriteSectionName: string
      read FWriteSectionName
      write FWriteSectionName;

    property RenameSectionName: string
      read FRenameSectionName
      write FRenameSectionName;

    property StringIdent: string
      read FStringIdent
      write FStringIdent;

    property IntegerIdent: string
      read FIntegerIdent
      write FIntegerIdent;

    property BooleanIdent: string
      read FBooleanIdent
      write FBooleanIdent;
  published
    procedure PublishedProperties;
    procedure RootKeysStruct;
    procedure GetComposedRootKey;
    procedure GetComposedRootKeyForDefaults;
    procedure GetComposedRootKeyForCommon;
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
    procedure DeleteRootKey;
  end;

  { TRegistrySourceGenericTest }

  { TRegistrySourceTest }

  TRegistrySourceTest = class(TRegistrySourceGenericTest<TRegistrySourceWrapper>)
  protected
    procedure SetSectionsAndIdents; override;
  end;

  { TRegistrySourceUTF8Test }
  TRegistrySourceUTF8Test = class(TRegistrySourceGenericTest<TRegistrySourceWrapperUTF8>)
  protected
    procedure SetSectionsAndIdents; override;
  end;

implementation

uses
  Registry,
  Classes;

procedure TRegistrySourceGenericTest<_T1>.PublishedProperties;
begin
  FRegSrcWrapper.PublishedProperties;
end;

procedure TRegistrySourceGenericTest<_T1>.RootKeysStruct;
begin
  FRegSrcWrapper.RootKeysStruct;
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteRootKey;
var
  ini: TRegIniFile;
  sections: TStrings;
  count1: integer;
  count2: integer;
  root_key: string;
begin
  sections := TStringList.Create;
  root_key := RegSrcWrapper.RegistrySource.GetComposedRootKey;
  ini := TRegIniFile.Create(root_key);
  try
    with ini do
    begin
      ini.ReadSections(sections);

      count1 := sections.count;

      RegSrcWrapper.RegistrySource.DeleteRootKey;

      sections.Clear;
      ini.ReadSections(sections);

      count2 := sections.count;
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);

    if Assigned(sections) then
      FreeAndNil(sections);
  end;

    AssertEquals('Vor dem Löschen des RootKey', 3, count1);
    AssertEquals('Nach dem Löschen des RootKey', 0, count2);
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKey;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKeyForDefaults;
begin
  Fail('Test noch nicht implementiert!');
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKeyForCommon;
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

procedure TRegistrySourceGenericTest<_T1>.SetSectionsAndIdents;
begin
  // Wird in der Ableitung gesetzt
end;

{ TRegistrySourceTest }

procedure TRegistrySourceTest.SetSectionsAndIdents;
begin
  ReadSectionName := 'ReadSection';
  WriteSectionName := 'WriteSection';
  RenameSectionName := 'RenameSection';
  StringIdent := 'String_Ident';
  IntegerIdent := 'Integer_Ident';
  BooleanIdent := 'Boolean_Ident';
end;

{ TRegistrySourceUTF8Test }

procedure TRegistrySourceUTF8Test.SetSectionsAndIdents;
begin
  ReadSectionName := 'ReadSection_mit_ßÜÖÄüöä';
  WriteSectionName := 'WriteSection_mit_ßÜÖÄüöä';
  RenameSectionName := 'RenameSection_mit_ßÜÖÄüöä';
  StringIdent := 'String_Ident_mit_ßÜÖÄüöä';
  IntegerIdent := 'Integer_Ident_mit_ßÜÖÄüöä';
  BooleanIdent := 'Boolean_Ident_mit_ßÜÖÄüöä';
end;

end.

