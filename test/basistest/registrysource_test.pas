unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  Registry,
  regbasics;

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
    FComposedRootKeyForCheck: string;
    FComposedRootKeyForDefaultsForCheck: string;
    FComposedRootKeyForCommonForCheck: string;
    FTestString: string;
    FTestInteger: integer;
    FTestBoolean: boolean;
    FNewStringIdent: string;
    FGroupIdx: integer;
    FCheckRTLNeeded: boolean;
    FKeyExistsSectionName: string;

    procedure SectionExistsProc(aIni: TLRCRegIniFile);
    procedure IdentExistsProc(aIni: TLRCRegIniFile);
    procedure DeleteRootKeyProc(aIni: TLRCRegIniFile);
    procedure ReadStringProc(aIni: TLRCRegIniFile);
    procedure ReadIntegerProc(aIni: TLRCRegIniFile);
    procedure ReadBoolProc(aIni: TLRCRegIniFile);
    procedure ReadSectionProc(aIni: TLRCRegIniFile);
    procedure WriteStringProc(aIni: TLRCRegIniFile);
    procedure WriteIntegerProc(aIni: TLRCRegIniFile);
    procedure WriteBoolProc(aIni: TLRCRegIniFile);
    procedure RenameKeyProc(aIni: TLRCRegIniFile);
    procedure DeleteKeyProc(aIni: TLRCRegIniFile);
    procedure EraseSectionProc(aIni: TLRCRegIniFile);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetSectionsAndIdents; virtual;

    property RegSrcWrapper: _T1
      read FRegSrcWrapper;

    property KeyExistsSectionName: string
      read FKeyExistsSectionName
      write FKeyExistsSectionName;

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

    property ComposedRootKeyForCheck: string
      read FComposedRootKeyForCheck
      write FComposedRootKeyForCheck;

    property ComposedRootKeyForDefaultsForCheck: string
      read FComposedRootKeyForDefaultsForCheck
      write FComposedRootKeyForDefaultsForCheck;

    property ComposedRootKeyForCommonForCheck: string
      read FComposedRootKeyForCommonForCheck
      write FComposedRootKeyForCommonForCheck;

    property TestString: string
      read FTestString
      write FTestString;

    property TestInteger: integer
    read FTestInteger
    write FTestInteger;

    property TestBoolean: boolean
      read FTestBoolean
      write FTestBoolean;

    property NewStringIdent: string
      read FNewStringIdent
      write FNewStringIdent;

    property GroupIdx: integer
      read FGroupIdx
      write FGroupIdx;

    property CheckRTLNeeded: boolean
      read FCheckRTLNeeded
      write FCheckRTLNeeded;
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
    procedure SectionExists;
    procedure IdentExists;
  end;

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
  Classes,
  test_utils,
  regconvutils;

{ TRegistrySourceGenericTest }

procedure TRegistrySourceGenericTest<_T1>.SectionExistsProc(aIni: TLRCRegIniFile);
var
  key_for_check: string;
  key_is_present: boolean;

  section_found: boolean;
begin
  key_for_check :=
    UTF8DecodeIfNeeded(ComposedRootKeyForCheck  + '\' + KeyExistsSectionName,
      CheckRTLNeeded);
  try
    key_is_present := aIni.KeyExists(key_for_check, False);

    // Prüfen ob der Test-Ident vorhanden ist
    // Zu Beginn des Test muss der Test-Ident vorhanden sein
    AssertTrue('KeyExists: Test nicht durchführbar, TestKey ist nicht vorhanden',
      key_is_present);
  finally
    //
  end;

  section_found :=
    FRegSrcWrapper.RegistrySource.SectionExists(KeyExistsSectionName);

  AssertEquals('SectionExists: RegistrySource liefert falschen Wert',
    key_is_present, section_found);
end;

procedure TRegistrySourceGenericTest<_T1>.IdentExistsProc(aIni: TLRCRegIniFile);
var
  ident_is_present: boolean;
  ident_found: boolean;
  key_for_check: string;
begin
  key_for_check :=
    UTF8DecodeIfNeeded(ComposedRootKeyForCheck  + '\' + KeyExistsSectionName,
      CheckRTLNeeded);

  ident_is_present :=
    aIni.ValueExists(key_for_check, UTF8DecodeIfNeeded(StringIdent,
      CheckRTLNeeded), False);

  // Prüfen ob der Test-Ident vorhanden ist
  // Zu Beginn des Test muss der Test-Ident vorhanden sein
  AssertTrue(
    'IdentExists: Test nicht durchführbar, TestIdent ist nicht vorhanden',
      ident_is_present);

  ident_found :=
    FRegSrcWrapper.RegistrySource.IdentExists(KeyExistsSectionName, StringIdent);

  AssertEquals('IdentExists: RegistrySource liefert falschen Wert',
    ident_is_present, ident_found);
end;

procedure TRegistrySourceGenericTest<_T1>.SectionExists;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    SectionExistsProc);
end;

procedure TRegistrySourceGenericTest<_T1>.IdentExists;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    IdentExistsProc);
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteRootKeyProc(aIni: TLRCRegIniFile);
var
  sections: TStrings;
  count1: integer;
  count2: integer;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  sections := TStringList.Create;
  try
    with aIni do
    begin
      // 1. Anzahl Sectionen unterhalb von RootKey mit TLRCRegIniFile (Registry.pas)
      // ermitteln: Anzahl <> 0
      ReadSections(sections);
      count1 := sections.count;
      AssertTrue('DeleteRootKey: Es wurden keine Schlüssel im RookKey gefunden',
        (count1 > 0));

      // 2. RootKey über TRegistrySource löschen
      RegSrcWrapper.RegistrySource.DeleteRootKey;
      sections.Clear;

      // 3. Anzahl Sectionen unterhalb von RootKey erneut mit TLRCRegIniFile
      // (Registry.pas) ermitteln: Anzahl = 0
      ReadSections(sections);
      count2 := sections.count;
      AssertTrue('DeleteRootKey: Nach Löschen des RootKey dürfen keine Sections '
        + 'gefunden werden', (count2 = 0));
    end;
  finally
    if Assigned(sections) then
      FreeAndNil(sections);
  end;
end;

procedure TRegistrySourceGenericTest<_T1>.ReadStringProc(aIni: TLRCRegIniFile);
var
  value_by_regini: string;
  value_by_regsrc: string;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // 1. String mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(ReadSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded),
      'String1');

  // 2. String mit TRegistrySource ermitteln
  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadString(ReadSectionName, StringIdent,
      'String2');

  // 1. und 2. müssen den selben Wert ermitteln
  AssertEquals('ReadString: RegistrySource liefert falschen Wert',
    UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded), value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadIntegerProc(aIni: TLRCRegIniFile);
var
  value_by_regini: integer;
  value_by_regsrc: integer;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // 1. Integer mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadInteger(UTF8DecodeIfNeeded(ReadSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(IntegerIdent, CheckRTLNeeded), 0);

  // 2. Integer mit TRegistrySource ermitteln
  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadInteger(ReadSectionName, IntegerIdent,
      1);

  // 1. und 2. müssen den selben Wert ermitteln
  AssertEquals('ReadInteger: RegistrySource liefert falschen Wert',
    value_by_regini, value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadBoolProc(aIni: TLRCRegIniFile);
var
  value_by_regini: boolean;
  value_by_regsrc: boolean;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // 1. Integer mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadBool(UTF8DecodeIfNeeded(ReadSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(BooleanIdent, CheckRTLNeeded), False);

  // 2. Integer mit TRegistrySource ermitteln
  value_by_regsrc :=
    RegSrcWrapper.RegistrySource.ReadBool(ReadSectionName, BooleanIdent,
      True);

  // 1. und 2. müssen den selben Wert ermitteln
  AssertEquals('ReadBool: RegistrySource liefert falschen Wert',
    value_by_regini, value_by_regsrc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadSectionProc(aIni: TLRCRegIniFile);
var
  value_by_regini: TStrings;
  value_by_regsrc: TStrings;
  anz: integer;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  value_by_regini := TStringlist.Create;
  value_by_regsrc := TStringlist.Create;
  try
    // 1. Datenwerte einer Section mit TLRCRegIniFile (Registry.pas) ermitteln
    aIni.ReadSection(UTF8DecodeIfNeeded(ReadSectionName, CheckRTLNeeded),
      value_by_regini);

    // 2. Datenwerte einer Section mit TRegistrySource ermitteln
    RegSrcWrapper.RegistrySource.ReadSection(ReadSectionName, value_by_regsrc);

    // Anzahl aus 1. und 2. muss gleich sein
    AssertTrue('ReadSection: RegistrySource liefert falsche Anzahl Datenwerte in Sektionen',
      (value_by_regini.Count = value_by_regsrc.Count));

    AssertFalse('ReadSection: Test nicht durchführbar, Anzahl in Section = 0',
      (value_by_regini.Count = 0));

    // Jeder Wert aus 1. muss identisch sein mit jedem Wert aus 2.
    for anz := 0 to value_by_regini.Count-1 do
    begin
      AssertEquals('ReadSection: Registry liefert falsche Einträge in der Liste',
        UTF8EncodeIfNeeded(value_by_regini.strings[anz], CheckRTLNeeded),
        value_by_regsrc.strings[anz]);
    end;
  finally
    if Assigned(value_by_regini) then
        FreeAndNil(value_by_regini);

    if Assigned(value_by_regsrc) then
        FreeAndNil(value_by_regsrc);
  end;
end;

procedure TRegistrySourceGenericTest<_T1>.WriteStringProc(aIni: TLRCRegIniFile);
const
  TEST_STRING = 'Beispiel1';
var
  value_by_regini: string;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // Prüfen ob die Test-Strings unterschiedlich sind
  AssertFalse('WriteString: Test nicht durchführbar, Test-Strings sind identisch',
    (CompareStr(TestString, TEST_STRING) = 0));

  // 1. Test-String mit TLRCRegIniFile (Registry.pas) setzen
  aIni.WriteString(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded), TEST_STRING);

  // 2. Test-String mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded), EmptyStr);

  // Prüfen ob der Test-String eingerichtet wurde
  AssertTrue('WriteString: Test nicht durchführbar, Test-String falsch initialisiert',
    (CompareStr(UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded),
      TEST_STRING) = 0));

  // 3. Test-String aus 1. mit neuem Teststring mit TRegistrySource überschreiben
  RegSrcWrapper.RegistrySource.WriteString(WriteSectionName, StringIdent,
    TestString);

  // 4. Neuer Test-String mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded), EmptyStr);

  // Prüfen ob der Test-String eingerichtet wurde
  AssertEquals('WriteString: Test-Strings falsch', TestString,
    UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded));
end;

procedure TRegistrySourceGenericTest<_T1>.WriteIntegerProc(aIni: TLRCRegIniFile);
const
  TEST_INTEGER = -1;
var
  value_by_regini: integer;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // Prüfen ob die Test-Integer unterschiedlich sind
  AssertFalse('WriteInteger: Test nicht durchführbar, Test-Integer sind identisch',
    (TEST_INTEGER = TestInteger));

  // 1. Test-Integer mit TLRCRegIniFile (Registry.pas) setzen
  aIni.WriteInteger(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(IntegerIdent, CheckRTLNeeded), TEST_INTEGER);

  // 2. Test-Integer mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadInteger(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(IntegerIdent, CheckRTLNeeded), 0);

  // Prüfen ob der Test-Integer eingerichtet wurde
  AssertTrue('WriteInteger: Test nicht durchführbar, Test-Integer falsch '
    + 'initialisiert', (value_by_regini = TEST_INTEGER));

  // 3. Test-Integer aus 1. mit neuem Test-Integer mit TRegistrySource
  // überschreiben
  RegSrcWrapper.RegistrySource.WriteInteger(WriteSectionName, IntegerIdent,
    TestInteger);

  // 4. Neuer Test-Integer mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadInteger(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(IntegerIdent, CheckRTLNeeded), 0);

  // Prüfen ob der Test-Integer eingerichtet wurde
  AssertEquals('WriteInteger: Test-Integer falsch', TestInteger, value_by_regini);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteBoolProc(aIni: TLRCRegIniFile);
const
  TEST_BOOLEAN = False;
var
  value_by_regini: boolean;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // Prüfen ob die Test-Boolean unterschiedlich sind
  AssertFalse('WriteBool: Test nicht durchführbar, Test-Boolean sind identisch',
    (TEST_BOOLEAN = TestBoolean));

  // 1. Test-Boolean mit TLRCRegIniFile (Registry.pas) setzen
  aIni.WriteBool(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(BooleanIdent, CheckRTLNeeded), TEST_BOOLEAN);

  // 2. Test-Boolean mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadBool(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(BooleanIdent, CheckRTLNeeded), False);

  // Prüfen ob der Test-Boolean eingerichtet wurde
  AssertTrue('WriteBool: Test nicht durchführbar, Test-Boolean falsch '
    + 'initialisiert', (value_by_regini = TEST_BOOLEAN));

  // 3. Test-Boolean aus 1. mit neuem Test-Boolean mit TRegistrySource
  // überschreiben
  RegSrcWrapper.RegistrySource.WriteBool(WriteSectionName, BooleanIdent,
    TestBoolean);

  // 4. Neuer Test-Boolean mit TLRCRegIniFile (Registry.pas) ermitteln
  value_by_regini :=
    aIni.ReadBool(UTF8DecodeIfNeeded(WriteSectionName, CheckRTLNeeded),
      UTF8DecodeIfNeeded(BooleanIdent, CheckRTLNeeded), False);

  // Prüfen ob der Test-Boolean eingerichtet wurde
  AssertEquals('WriteBool: Test-Boolean falsch', TestBoolean, value_by_regini);
end;

procedure TRegistrySourceGenericTest<_T1>.RenameKeyProc(aIni: TLRCRegIniFile);
var
  value_by_regini: string;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  // Prüfen ob der Test-String kein Leerstring ist
  AssertTrue('RenameKey: Test nicht durchführbar, TestString ist ein Leerstring',
    (Trim(TestString) <> EmptyStr));

  // Vorhandenen Test-String mit TLRCRegIniFile (Registry.pas) auslesen,
  // Ergebnis darf kein Leerstring sein
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded), EmptyStr);

  // Prüfen ob die Initialisierung mit dem TestString nicht übereinstimmt
  AssertFalse('RenameKey: Test nicht durchführbar, Test-Strings sind identisch',
    (CompareStr(TestString, UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded))
      = 0));

  // 1. Den Test-String mit TLRCRegIniFile (Registry.pas) in den vorhandnen Ident
  // einfügen
  aIni.WriteString(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded),
    UTF8DecodeIfNeeded(TestString, CheckRTLNeeded));

  // 2. Den Test-String mit TLRCRegIniFile (Registry.pas) aus dem vorhandenen Ident
  // wieder auslesen
  // Ergebnis darf kein Leerstring sein
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(StringIdent, CheckRTLNeeded), EmptyStr);

  AssertTrue('RenameKey: Test nicht durchführbar, Test-Strings sind nicht identisch',
    (CompareStr(TestString, UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded))
      = 0));

  // 3. Aktuellen Ident mit TRegistrySource umbenennen
  RegSrcWrapper.RegistrySource.RenameKey(RenameSectionName, StringIdent, NewStringIdent);

  // 4. Den Test-String mit TLRCRegIniFile (Registry.pas) aus dem umbenannten Ident
  // auslesen
  // Ergebnis muss der Wert aus 1.
  value_by_regini :=
    aIni.ReadString(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
    UTF8DecodeIfNeeded(NewStringIdent, CheckRTLNeeded), EmptyStr);

  AssertEquals('RenameKey: Test-Strings sind nicht identisch', TestString,
    UTF8EncodeIfNeeded(value_by_regini, CheckRTLNeeded));
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteKeyProc(aIni: TLRCRegIniFile);
var
  key_for_check: string;
  ident_is_present: boolean;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  ident_is_present := False;
  key_for_check :=
    UTF8DecodeIfNeeded(ComposedRootKeyForCheck  + '\' + RenameSectionName,
      CheckRTLNeeded);

  ident_is_present :=
    aIni.ValueExists(key_for_check, UTF8DecodeIfNeeded(StringIdent,
      CheckRTLNeeded), False);

  // Prüfen ob der Test-Ident vorhanden ist
  // Zu Beginn des Test muss der Test-Ident vorhanden sein
  AssertTrue('DeleteKey: Test nicht durchführbar, TestIdent ist nicht vorhanden',
    ident_is_present);

  // 1. Aktuellen Ident mit TRegistrySource löschen
  RegSrcWrapper.RegistrySource.DeleteKey(RenameSectionName, StringIdent,
    GroupIdx);

  ident_is_present := aIni.ValueExists(key_for_check, StringIdent, False);

  // Prüfen ob der Test-Ident vorhanden ist
  // Nach dem Test darg der Test-Ident nicht mehr vorhanden sein
  AssertFalse('DeleteKey: Test-Ident wurde nicht gelöscht',
    ident_is_present);
end;

procedure TRegistrySourceGenericTest<_T1>.EraseSectionProc(aIni: TLRCRegIniFile);
var
  sections: TStrings;
  count: integer;
begin
  // UTF8DecodeIfNeeded, UTF8EncodeIfNeeded sind notwendig, wenn Umlaute über
  // TLRCRegIniFile gelesen oder geschrieben werden
  // TRegistrySource soll dies automatisch können

  sections := TStringList.Create;
  try
    aIni.ReadSection(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
      sections);
    count := sections.Count;

    AssertTrue('EraseSection: Test nicht durchführbar, Section ist schon leer',
      (count > 0));

    RegSrcWrapper.RegistrySource.EraseSection(RenameSectionName, GroupIdx);

    sections.Clear;
    aIni.ReadSection(UTF8DecodeIfNeeded(RenameSectionName, CheckRTLNeeded),
      sections);
    count := sections.Count;

    AssertTrue('EraseSection: Section wurde nicht geleert', (count = 0));
  finally
    if Assigned(sections) then
        FreeAndNil(sections);
  end;
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
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    DeleteRootKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.GetComposedRootKey;
var
  key_by_regsrc: string;
  key_for_check: string;
begin
  key_by_regsrc := RegSrcWrapper.RegistrySource.GetComposedRootKey;
  key_for_check := IncludeTrailingPathDelimiter(ComposedRootKeyForCheck);

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
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey, ReadStringProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadInteger;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadIntegerProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadBool;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadBoolProc);
end;

procedure TRegistrySourceGenericTest<_T1>.ReadSection;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadSectionProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteString;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteStringProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteInteger;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteIntegerProc);
end;

procedure TRegistrySourceGenericTest<_T1>.WriteBool;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    WriteBoolProc);
end;

procedure TRegistrySourceGenericTest<_T1>.RenameKey;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    RenameKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.DeleteKey;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    DeleteKeyProc);
end;

procedure TRegistrySourceGenericTest<_T1>.EraseSection;
begin
  GetRegIniFile(RegSrcWrapper.RegistrySource.GetComposedRootKey,
    EraseSectionProc);
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
  KeyExistsSectionName := 'KeyExistsSection';
  ReadSectionName := 'ReadSection';
  WriteSectionName := 'WriteSection';
  RenameSectionName := 'RenameSection';
  StringIdent := 'String_Ident';
  IntegerIdent := 'Integer_Ident';
  BooleanIdent := 'Boolean_Ident';
  ComposedRootKeyForCheck :=
    'SOFTWARE\ExampleFactory\LazarusRegistryControls\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  ComposedRootKeyForDefaultsForCheck :=
    'SOFTWARE\ExampleFactory\LazarusRegistryControls\DEFAULTS\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  ComposedRootKeyForCommonForCheck :=
    'SOFTWARE\ExampleFactory\GEMEINSAME DATEN\LazarusRegistryControls\{A4B6F463-1EF0-4DB0-B5DC-1580D2B944D4}';
  TestString := 'BeispielForTest';
  TestInteger := 123456;
  TestBoolean := True;
  NewStringIdent := 'New_String_Ident';
  GroupIdx := 0;
  CheckRTLNeeded := False;
end;

{ TRegistrySourceUTF8Test }

procedure TRegistrySourceUTF8Test.SetSectionsAndIdents;
begin
  KeyExistsSectionName := 'KeyExistsSection_mit_ßÜÖÄüöä';
  ReadSectionName := 'ReadSection_mit_ßÜÖÄüöä';
  WriteSectionName := 'WriteSection_mit_ßÜÖÄüöä';
  RenameSectionName := 'RenameSection_mit_ßÜÖÄüöä';
  StringIdent := 'String_Ident_mit_ßÜÖÄüöä';
  IntegerIdent := 'Integer_Ident_mit_ßÜÖÄüöä';
  BooleanIdent := 'Boolean_Ident_mit_ßÜÖÄüöä';
  ComposedRootKeyForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  ComposedRootKeyForDefaultsForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\Project_mit_ßÜÖÄüöä\DEFAULTS\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  ComposedRootKeyForCommonForCheck :=
    'SOFTWARE\Organisation_mit_ßÜÖÄüöä\GEMEINSAME DATEN\Project_mit_ßÜÖÄüöä\{2CD0EB3F-A81E-4F0D-AE9B-1548DC65F930}';
  TestString := 'BeispielForTest_mit_ßÜÖÄüöä';
  TestInteger := 123456;
  TestBoolean := True;
  NewStringIdent := 'New_String_Ident_mit_ßÜÖÄüöä';
  GroupIdx := 0;
  CheckRTLNeeded := True;
end;

end.

