unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  Registry;

type

  { TRegistrySourceGenericTest }

  TRegistrySourceGenericTest<_T1>= class(TTestCase)
  strict private
  type
    TManageRegIniFile = procedure(aIni: TRegIniFile) of object;
  private
    FRegSrcWrapper: _T1;

    FReadSectionName: string;
    FWriteSectionName: string;
    FRenameSectionName: string;
    FStringIdent: string;
    FIntegerIdent: string;
    FBooleanIdent: string;
    FComposedRookKeyForCheck: string;
    FComposedRootKeyForDefaultsForCheck: string;
    FComposedRootKeyForCommonForCheck: string;

    procedure DeleteRootKeyProc(aIni: TRegIniFile);
    procedure ReadStringProc(aIni: TRegIniFile);
    procedure ReadIntegerProc(aIni: TRegIniFile);
    procedure ReadBoolProc(aIni: TRegIniFile);
    procedure ReadSectionProc(aIni: TRegIniFile);
    procedure WriteStringProc(aIni: TRegIniFile);
    procedure WriteIntegerProc(aIni: TRegIniFile);
    procedure WriteBoolProc(aIni: TRegIniFile);
    procedure RenameKeyProc(aIni: TRegIniFile);
    procedure DeleteKeyProc(aIni: TRegIniFile);
    procedure EraseSectionProc(aIni: TRegIniFile);

    procedure GetRegIniFile(aProc: TManageRegIniFile);
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

    property ComposedRookKeyForCheck: string
      read FComposedRookKeyForCheck
      write FComposedRookKeyForCheck;

    property ComposedRootKeyForDefaultsForCheck: string
      read FComposedRootKeyForDefaultsForCheck
      write FComposedRootKeyForDefaultsForCheck;

    property ComposedRootKeyForCommonForCheck: string
      read FComposedRootKeyForCommonForCheck
      write FComposedRootKeyForCommonForCheck;
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
  Classes;

{ TRegistrySourceGenericTest }

procedure TRegistrySourceGenericTest<_T1>.GetRegIniFile(aProc: TManageRegIniFile);
var
  ini: TRegIniFile;
  root_key: string;
begin
  root_key := RegSrcWrapper.RegistrySource.GetComposedRootKey;

  ini := TRegIniFile.Create(UTF8Decode(root_key));
  try
    with ini do
    begin
      if Assigned(aProc) then
        aProc(ini);
    end;
  finally
    if Assigned(ini) then
      FreeAndNil(ini);
  end;
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteRootKeyProc(aIni: TRegIniFile);
var
  sections: TStrings;
  count1: integer;
  count2: integer;
begin
  //UTF8Decode, UTF8Encode sind notwendig, wenn Umlaute über TRegIniFile gelesen
  //oder geschrieben werden
  //TRegistrySource soll dies automatisch können

  sections := TStringList.Create;
  try
    with aIni do
    begin
      ReadSections(sections);
      count1 := sections.count;
      AssertTrue('Es wurden keine Schlüssel im RookKey gefunden', (count1 > 0));

      RegSrcWrapper.RegistrySource.DeleteRootKey;
      sections.Clear;

      ReadSections(sections);
      count2 := sections.count;
      AssertTrue('Nach Löschen des RootKey dürfen keine Sections gefunden werden', (count2 = 0));
    end;
  finally
    if Assigned(sections) then
      FreeAndNil(sections);
  end;
end;

procedure TRegistrySourceGenericTest<_T1>.ReadStringProc(aIni: TRegIniFile);
var
  value_by_regini: string;
  value_by_regsrc: string;
begin
  //UTF8Decode, UTF8Encode sind notwendig, wenn Umlaute über TRegIniFile gelesen
  //oder geschrieben werden
  //TRegistrySource soll dies automatisch können

  value_by_regini :=
    aIni.ReadString(UTF8Decode(ReadSectionName), UTF8Decode(StringIdent),
      'String1');

  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadString(ReadSectionName, StringIdent,
      'String2');

  AssertEquals('ReadString: RegistrySource liefert falschen Wert',
    UTF8Encode(value_by_regini), value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadIntegerProc(aIni: TRegIniFile);
var
  value_by_regini: integer;
  value_by_regsrc: integer;
begin
  //UTF8Decode, UTF8Encode sind notwendig, wenn Umlaute über TRegIniFile gelesen
  //oder geschrieben werden
  //TRegistrySource soll dies automatisch können

  value_by_regini :=
    aIni.ReadInteger(UTF8Decode(ReadSectionName), UTF8Decode(IntegerIdent),
      0);

  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadInteger(ReadSectionName, IntegerIdent,
      1);

  AssertEquals('ReadInteger: RegistrySource liefert falschen Wert',
    value_by_regini, value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadBoolProc(aIni: TRegIniFile);
var
  value_by_regini: boolean;
  value_by_regsrc: boolean;
begin
  //UTF8Decode, UTF8Encode sind notwendig, wenn Umlaute über TRegIniFile gelesen
  //oder geschrieben werden
  //TRegistrySource soll dies automatisch können

  value_by_regini :=
    aIni.ReadBool(UTF8Decode(ReadSectionName), UTF8Decode(BooleanIdent),
      False);

  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadBool(ReadSectionName, BooleanIdent,
      True);

  AssertEquals('ReadBool: RegistrySource liefert falschen Wert',
    value_by_regini, value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadSectionProc(aIni: TRegIniFile);
var
  value_by_regini: TStrings;
  value_by_regsrc: TStrings;
  anz: integer;
begin
  //UTF8Decode, UTF8Encode sind notwendig, wenn Umlaute über TRegIniFile gelesen
  //oder geschrieben werden
  //TRegistrySource soll dies automatisch können

  value_by_regini := TStringlist.Create;
  value_by_regsrc := TStringlist.Create;
  try
    aIni.ReadSection(UTF8Decode(ReadSectionName), value_by_regini);

    RegSrcWrapper.RegistrySource.ReadSection(ReadSectionName, value_by_regsrc);

    AssertTrue('ReadSection: RegistrySource liefert falsche Anzahl von Sektionen',
      (value_by_regini.Count = value_by_regsrc.Count));

    for anz := 0 to value_by_regini.Count-1 do
    begin
      AssertEquals('ReadSection: Registry liefert falsche Einträge in der Liste',
        UTF8Encode(value_by_regini.strings[anz]), value_by_regsrc.strings[anz]);
    end;
  finally
    if Assigned(value_by_regini) then
        FreeAndNil(value_by_regini);

    if Assigned(value_by_regsrc) then
        FreeAndNil(value_by_regsrc);
  end;
end;

procedure TRegistrySourceGenericTest<_T1>.WriteStringProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

procedure TRegistrySourceGenericTest<_T1>.WriteIntegerProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

procedure TRegistrySourceGenericTest<_T1>.WriteBoolProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

procedure TRegistrySourceGenericTest<_T1>.RenameKeyProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteKeyProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

procedure TRegistrySourceGenericTest<_T1>.EraseSectionProc(aIni: TRegIniFile);
begin
  Fail('Test wurde noch nicht implementiert');
end;

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
  GetRegIniFile(DeleteRootKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKey;
var
  key_by_regsrc: string;
  key_for_check: string;
begin
  key_by_regsrc := RegSrcWrapper.RegistrySource.GetComposedRootKey;
  key_for_check := IncludeTrailingPathDelimiter(ComposedRookKeyForCheck);

  AssertEquals('GetComposedRootKey', key_for_check, key_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKeyForDefaults;
var
  key_by_regsrc: string;
  key_for_check: string;
begin
  key_by_regsrc := RegSrcWrapper.RegistrySource.GetComposedRootKeyForDefaults;
  key_for_check :=
    IncludeTrailingPathDelimiter(ComposedRootKeyForDefaultsForCheck);

  AssertEquals('GetComposedRootKeyForDefaults', key_for_check, key_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKeyForCommon;
var
  key_by_regsrc: string;
  key_for_check: string;
begin
  key_by_regsrc := RegSrcWrapper.RegistrySource.GetComposedRootKeyForCommon;
  key_for_check :=
    IncludeTrailingPathDelimiter(ComposedRootKeyForCommonForCheck);

  AssertEquals('GetComposedRootKeyForCommon', key_for_check, key_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadString;
begin
  GetRegIniFile(ReadStringProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadInteger;
begin
  GetRegIniFile(ReadIntegerProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadBool;
begin
  GetRegIniFile(ReadBoolProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadSection;
begin
  GetRegIniFile(ReadSectionProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteString;
begin
  GetRegIniFile(WriteStringProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteInteger;
begin
  GetRegIniFile(WriteIntegerProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteBool;
begin
  GetRegIniFile(WriteBoolProc);
end;

procedure TRegistrySourceGenericTest<_T1>.RenameKey;
begin
  GetRegIniFile(RenameKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteKey;
begin
  GetRegIniFile(DeleteKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.EraseSection;
begin
  GetRegIniFile(EraseSectionProc);
end;

procedure TRegistrySourceGenericTest<_T1>.SetUp;
begin
  FRegSrcWrapper := _T1.Create;
  SetSectionsAndIdents;
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
  ComposedRookKeyForCheck :=
    'SOFTWARE\ExampleFactory\LazarusRegistryControls\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  ComposedRootKeyForDefaultsForCheck :=
    'SOFTWARE\ExampleFactory\LazarusRegistryControls\DEFAULTS\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  ComposedRootKeyForCommonForCheck :=
    'SOFTWARE\ExampleFactory\GEMEINSAME DATEN\LazarusRegistryControls\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
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
  ComposedRookKeyForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  ComposedRootKeyForDefaultsForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\DEFAULTS\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  ComposedRootKeyForCommonForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\GEMEINSAME DATEN\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
end;

end.

