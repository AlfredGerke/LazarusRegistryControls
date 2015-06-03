unit lrcreginifile_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  regutils,
  Registry,
  regbasics;

type

  { TLRCRegInifileTest }

  TTestLRCRegIniFile = class(TLRCRegIniFile)
  public
    property Root;
  end;

  TLRCRegInifileTest= class(TTestCase)
  private
    FLRCRRegIniFile: TTestLRCRegIniFile;
    FTestString1: string;
    FTestString2: string;
    FTestBool1: boolean;
    FTestBool2: boolean;
    FTestInteger1: integer;
    FTestInteger2: integer;
    FTestList1: TStrings;
    FTestList2: TStrings;

    procedure CheckListForReadSectionValuesTest(aList: TStrings);
    procedure CheckListForReadSectionsTest(aList: TStrings);
    procedure CheckListForReadSectionTest(aList: TStrings);

    procedure WriteStringBeforeLRCProc(aReg: TRegistry);
    procedure WriteStringAfterLRCProc1(aReg: TRegistry);
    procedure WriteStringAfterLRCProc2(aReg: TRegistry);
    procedure WriteStringAfterLRCProc3(aReg: TRegistry);
    procedure WriteStringAfterLRCProc4(aReg: TRegistry);
    procedure WriteIntegerBeforeLRCProc(aReg: TRegistry);
    procedure WriteIntegerAfterLRCProc1(aReg: TRegistry);
    procedure WriteIntegerAfterLRCProc2(aReg: TRegistry);
    procedure WriteIntegerAfterLRCProc3(aReg: TRegistry);
    procedure WriteIntegerAfterLRCProc4(aReg: TRegistry);
    procedure WriteBoolBeforeLRCPRoc(aReg: TRegistry);
    procedure WriteBoolAfterLRCProc1(aReg: TRegistry);
    procedure WriteBoolAfterLRCProc2(aReg: TRegistry);
    procedure WriteBoolAfterLRCProc3(aReg: TRegistry);
    procedure WriteBoolAfterLRCProc4(aReg: TRegistry);
    procedure ReadSectionValuesBeforeLRCProc(aReg: TRegistry);
    procedure ReadSectionsBeforeLRCProc(aReg: TRegistry);
    procedure ReadSectionBeforeLRCProc(aReg: Tregistry);
    procedure ReadStringBeforeLRCProc(aReg: TRegistry);
    procedure ReadIntegerBeforeLRCProc(aReg: TRegistry);
    procedure ReadBoolBeforeLRCProc(aReg: TRegistry);
    procedure DeleteKeyBeforeLRCProc(aReg: TRegistry);
    procedure DeleteKeyAfterLRCProc(aReg: TRegistry);
    procedure EraseSectionBeforeLRCProc(aReg: TRegistry);
    procedure EraseSectionAfterLRCProc(aReg: TRegistry);
    procedure SetSectionsAndIdents(aCreate: boolean = True);

    procedure Init;
    procedure Done;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Filename;
    procedure Root;
    procedure DeleteKey;
    procedure EraseSection;
    procedure ReadBool;
    procedure ReadInteger;
    procedure ReadString;
    procedure ReadSection;
    procedure ReadSections;
    procedure ReadSectionValues;
    procedure WriteBool;
    procedure WriteInteger;
    procedure WriteString;
    procedure KeyExists;
    procedure ValueExists;
  end;

implementation

uses
  test_const,
  test_utils;

procedure TLRCRegInifileTest.CheckListForReadSectionValuesTest(aList: TStrings);
var
  count: integer;
  index: integer;
  value: string;
  value_bool: boolean;
  value_int: integer;
begin
  count := aList.Count;

  AssertTrue(Format('Falsche Anzahl Einträge: Soll=3 - Ist=%d',
    [count]), count=3);

  index := aList.IndexOfName('StringIdent');
  AssertFalse('StringIdent nicht gefunden', (index=-1));

  value := aList.ValueFromIndex[index];
  AssertTrue(
    Format('Falscher Wert für StringIdent: Soll=Test - Ist=%s', [value]),
      (value='Test'));

  index := aList.IndexOfName('IntegerIdent');
  AssertFalse('IntegerIdent nicht gefunden', (index=-1));

  value := aList.ValueFromIndex[index];
  AssertTrue(
    Format('Wert für IntegerIdent kein Integer: %s', [value]),
      TryStrToInt(value, value_int));
  AssertTrue(
    Format('Falscher Wert für IntegerIdent: Soll=1234 - Ist=%s', [value]),
      (value='1234'));

  index := aList.IndexOfName('BooleanIdent');
  AssertFalse('BooleanIdent nicht gefuden', (index=-1));

  value := aList.ValueFromIndex[index];
  AssertTrue(
    Format('Wert für BooleanIdent kein Boolean: %s', [value]),
      TryStrToBool(value, value_bool));
  AssertTrue(
    Format('Falscher Wert für BooleanIdent: Soll=True - Ist=%s',
      [BoolToStr(value_bool, 'true', 'False')]), (value_bool=True));
end;

procedure TLRCRegInifileTest.CheckListForReadSectionsTest(aList: TStrings);
var
  count: integer;
  index: integer;
begin
  count := aList.Count;

  AssertTrue(Format('Falsche Anzahl Einträge: Soll=4 - Ist=%d',
    [count]), count=4);

  index := aList.IndexOf('StringSection');
  AssertFalse('StringSection nicht gefunden', (index=-1));

  index := aList.IndexOf('IntegerSection');
  AssertFalse('IntegerSection nicht gefunden', (index=-1));

  index := aList.IndexOf('BooleanSection');
  AssertFalse('BooleanSection nicht gefuden', (index=-1));

  index := aList.IndexOf('DeleteKeySection');
  AssertFalse('DeleteKeySection nicht gefuden', (index=-1));
end;

procedure TLRCRegInifileTest.CheckListForReadSectionTest(aList: TStrings);
var
  count: integer;
  index: integer;
begin
  count := aList.Count;

  AssertTrue(Format('Falsche Anzahl Einträge: Soll=3 - Ist=%d',
    [count]), count=3);

  index := aList.IndexOf('StringIdent');
  AssertFalse('StringIdent nicht gefunden', (index=-1));

  index := aList.IndexOf('IntegerIdent');
  AssertFalse('IntegerIdent nicht gefunden', (index=-1));

  index := aList.IndexOf('BooleanIdent');
  AssertFalse('BooleanIdent nicht gefuden', (index=-1));
end;

procedure TLRCRegInifileTest.WriteStringBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
  begin
    AssertTrue('Test nicht durchführbar, WriteString nicht vorhanden',
      ValueExists('WriteString'));

    FTestString1 := ReadString('WriteString');

    AssertTrue(
      Format('Falscher Wert für WriteString: Soll=TestStringForWriteTest - Ist=%s',
        [FTestString1]), (FTestString1='TestStringForWriteTest'));
  end;
end;

procedure TLRCRegInifileTest.WriteStringAfterLRCProc1(aReg: TRegistry);
begin
  with aReg do
  begin
    FTestString2 := ReadString('WriteString');

    AssertEquals('Falscher Wert für WriteString', 'Wert wird geändert', FTestString2);

    AssertTrue('Falscher Wert für WriteString', (FTestString1<>FTestString2));

    AssertFalse('Test nicht durchführbar, WriteStringNew vorhanden',
      ValueExists('WriteStringNew'));
  end;
end;

procedure TLRCRegInifileTest.WriteStringAfterLRCProc2(aReg: TRegistry);
begin
  with aReg do
    FTestString2 := ReadString('WriteStringNew');

  AssertTrue('Falscher Wert für WriteStringNew: Soll=Neuer Ident', (FTestString2='Neuer Ident'));

  AssertFalse('Falscher Wert für WriteStringNew', (FTestString1=FTestString2));
end;

procedure TLRCRegInifileTest.WriteStringAfterLRCProc3(aReg: TRegistry);
begin
  AssertFindSectionTrue(aReg, 'StringSectionNew');
end;

procedure TLRCRegInifileTest.WriteStringAfterLRCProc4(aReg: TRegistry);
begin
  with aReg do
  begin
    FTestString2 := ReadString('WriteString');

    AssertEquals('Falscher Wert für WriteString', 'Neue Section und neuer Ident', FTestString2);

    AssertTrue('Falscher Wert für WriteString', (FTestString1<>FTestString2));
  end;
end;

procedure TLRCRegInifileTest.WriteIntegerBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
  begin
    AssertTrue('Test nicht durchführbar, WriteInteger nicht vorhanden',
      ValueExists('WriteInteger'));

    FTestInteger1 := ReadInteger('WriteInteger');

    AssertTrue(
      Format('Falscher Wert für WriteInteger: Soll=1808 - Ist=%d',
        [FTestInteger1]), (FTestInteger1=1808));
  end;
end;

procedure TLRCRegInifileTest.WriteIntegerAfterLRCProc1(aReg: TRegistry);
begin
  with aReg do
    begin
      FTestInteger2 := ReadInteger('WriteInteger');

      AssertEquals('Falscher Wert für WriteInteger', 4321, FTestInteger2);

      AssertTrue('Falscher Wert für WriteInteger', (FTestInteger1<>FTestInteger2));

      AssertFalse('Test nicht durchführbar, WriteIntegerNew vorhanden',
        ValueExists('WriteIntegerNew'));
    end;
end;

procedure TLRCRegInifileTest.WriteIntegerAfterLRCProc2(aReg: TRegistry);
begin
  with aReg do
    FTestInteger2 := ReadInteger('WriteIntegerNew');

  AssertTrue('Falscher Wert für WriteIntegerNew: Soll=1111', (FTestInteger2=1111));

  AssertFalse('Falscher Wert für WriteIntegerNew', (FTestInteger1=FTestInteger2));
end;

procedure TLRCRegInifileTest.WriteIntegerAfterLRCProc3(aReg: TRegistry);
begin
  AssertFindSectionTrue(aReg, 'IntegerSectionNew');
end;

procedure TLRCRegInifileTest.WriteIntegerAfterLRCProc4(aReg: TRegistry);
begin
  with aReg do
  begin
    FTestInteger2 := ReadInteger('WriteInteger');

    AssertEquals('Falscher Wert für WriteInteger', 99, FTestInteger2);

    AssertTrue('Falscher Wert für WriteInteger', (FTestInteger1<>FTestInteger2));
  end;
end;

procedure TLRCRegInifileTest.WriteBoolBeforeLRCPRoc(aReg: TRegistry);
begin
  with aReg do
  begin
    AssertTrue('Test nicht durchführbar, WriteBoolean nicht vorhanden',
      ValueExists('WriteBoolean'));

    FTestBool1 := ReadBool('WriteBoolean');

    AssertTrue('Falscher Wert für WriteBoolean', (FTestBool1=True));
  end;
end;

procedure TLRCRegInifileTest.WriteBoolAfterLRCProc1(aReg: TRegistry);
begin
  with aReg do
  begin
    FTestBool2 := ReadBool('WriteBoolean');

    AssertEquals('Falscher Wert für WriteBoolean', False, FTestBool2);

    AssertTrue('Falscher Wert für WriteBoolean', (FTestBool1<>FTestBool2));

    AssertFalse('Test nicht durchführbar, WriteBooleanNew vorhanden',
      ValueExists('WriteBooleanNew'));
  end;
end;

procedure TLRCRegInifileTest.WriteBoolAfterLRCProc2(aReg: TRegistry);
begin
  with aReg do
    FTestBool2 := ReadBool('WriteBooleanNew');

  AssertTrue('Falscher Wert für WriteBooleanNew: Soll=True', (FTestBool2=True));

  AssertFalse('Falscher Wert für WriteBooleanNew', (FTestBool1=FTestBool2));
end;

procedure TLRCRegInifileTest.WriteBoolAfterLRCProc3(aReg: TRegistry);
begin
  AssertFindSectionTrue(aReg, 'BooleanSectionNew');
end;

procedure TLRCRegInifileTest.WriteBoolAfterLRCProc4(aReg: TRegistry);
begin
  with aReg do
  begin
    FTestBool2 := ReadBool('WriteBoolean');

    AssertEquals('Falscher Wert für WriteBoolean', True, FTestBool2);

    AssertTrue('Falscher Wert für WriteBoolean', (FTestBool1<>FTestBool2));
  end;
end;

procedure TLRCRegInifileTest.ReadSectionValuesBeforeLRCProc(aReg: TRegistry);
var
  list: TStrings;
  value_str: string;
  value_bool: boolean;
  value_int: integer;
  count: integer;
begin
  AssertTrue('Test nicht durchführbar, Liste nicht vorhanden',
    Assigned(FTestList1));

  FTestList1.Clear;

  list := TStringList.Create;
  try
    with aReg do
    begin
      GetValueNames(list);

      count := list.count;

      AssertTrue(
        Format('Test nicht durchführbar, falsche Anzahl Einträge: Soll=3 - Ist=%d',
          [count]), (count=3));

      value_str := aReg.ReadString('StringIdent');
      AssertTrue(
        Format('Test nicht durchführbar, falscher Wert für StringIdent: %s',
          [value_str]), (value_str='Test'));

      value_int := aReg.ReadInteger('IntegerIdent');
      AssertTrue(
        Format('Test nicht durchführbar, falscher Wert für IntegerIdent: %d',
          [value_int]), (value_int=1234));

      value_bool := aReg.ReadBool('BooleanIdent');
      AssertTrue(
        Format('Test nicht durchführbar, falscher Wert für BooleanIdent: %s',
          [BoolToStr(value_bool, 'True', 'False')]), (value_bool=True));
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.ReadSectionsBeforeLRCProc(aReg: TRegistry);
begin
  AssertTrue('Test nicht durchführbar, Liste nicht vorhanden',
    Assigned(FTestList1));

  FTestList1.Clear;

  with aReg do
    GetKeyNames(FTestList1);

  CheckListForReadSectionsTest(FTestList1);
end;

procedure TLRCRegInifileTest.ReadSectionBeforeLRCProc(aReg: Tregistry);
begin
  AssertTrue('Test nicht durchführbar, Liste nicht vorhanden',
    Assigned(FTestList1));

  FTestList1.Clear;

  with aReg do
    GetValueNames(FTestList1);

  CheckListForReadSectionTest(FTestList1);
end;

procedure TLRCRegInifileTest.ReadStringBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestString1 := ReadString('ReadString');

  AssertTrue(
    Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %s',
      [FTestString1]), FTestString1='TestStringForReadTest');
end;

procedure TLRCRegInifileTest.ReadIntegerBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestInteger1 := ReadInteger('ReadInteger');

  AssertTrue(
    Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %d',
      [FTestInteger1]), FTestInteger1=1965);
end;

procedure TLRCRegInifileTest.ReadBoolBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestBool1 := ReadBool('ReadBoolean');

  AssertTrue(
    Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %s',
      [BoolToStr(FTestBool1, 'True', 'False')]), FTestBool1=True);
end;

procedure TLRCRegInifileTest.DeleteKeyBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
     FTestString1 := ReadString('StringIdent');

  AssertTrue(
    Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %s',
      [FTestString1]), (FTestString1='Test'));
end;

procedure TLRCRegInifileTest.DeleteKeyAfterLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestString2 := ReadString('StringIdent');
end;

procedure TLRCRegInifileTest.EraseSectionBeforeLRCProc(aReg: TRegistry);
var
  list: TStrings;
  count: integer;
begin
  list := TStringList.Create;
  try
    with aReg do
      GetKeyNames(list);

    count := list.Count;

    AssertTrue(
      Format('Test nicht durchführbar, falsche Anzahl Sectionen: Soll=4 - Ist=%d',
        [count]), count = 4);
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.EraseSectionAfterLRCProc(aReg: TRegistry);
var
  list: TStrings;
  count: integer;
begin
  list := TStringList.Create;
  try
    with aReg do
      GetKeyNames(list);

    count := list.Count;

    AssertTrue(
      Format('Falsche Anzahl Sectionen: Soll=3 - Ist=%d', [count]),
        count = 3);
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.SetSectionsAndIdents(aCreate: boolean = True);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    with reg do
    begin
      RootKey := HKEY_CURRENT_USER;

      if aCreate then
      begin
        if OpenKey(LRCREGINIFILE_TESTROOT + '\StringSection\', True)
        then
        begin
          WriteString('WriteString', 'TestStringForWriteTest');
          WriteString('ReadString', 'TestStringForReadTest');
        end;
        CloseKey;

        if OpenKey(LRCREGINIFILE_TESTROOT + '\IntegerSection\', True)
        then
        begin
          WriteInteger('WriteInteger', 1808);
          WriteInteger('ReadInteger', 1965);
        end;
        CloseKey;

        if OpenKey(LRCREGINIFILE_TESTROOT + '\BooleanSection\', True)
        then
        begin
          WriteBool('WriteBoolean', True);
          WriteBool('ReadBoolean', True);
        end;
        CloseKey;

        if OpenKey(LRCREGINIFILE_TESTROOT + '\DeleteKeySection\', True)
        then
        begin
          WriteString('StringIdent', 'Test');
          WriteInteger('IntegerIdent', 1234);
          WriteBool('BooleanIdent', True);
        end;
        CloseKey;
      end
      else
      begin
        DeleteKey(LRCREGINIFILE_TESTROOT + '\StringSection\');
        if KeyExists(LRCREGINIFILE_TESTROOT + '\StringSectionNew\') then
          DeleteKey(LRCREGINIFILE_TESTROOT + '\StringSectionNew\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\IntegerSection\');
        if KeyExists(LRCREGINIFILE_TESTROOT + '\IntegerSectionNew\') then
          DeleteKey(LRCREGINIFILE_TESTROOT + '\IntegerSectionNew\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\BooleanSection\');
        if KeyExists(LRCREGINIFILE_TESTROOT + '\BooleanSectionNew\') then
          DeleteKey(LRCREGINIFILE_TESTROOT + '\BooleanSectionNew\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\DeleteKeySection\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\');

        CloseKey;
      end;
    end;
  finally
    if Assigned(reg) then
      FreeAndNil(reg);
  end;
end;

procedure TLRCRegInifileTest.Init;
begin
  FTestString1 := EmptyStr;
  FTestString2 := EmptyStr;
  FTestBool1 := False;
  FTestBool2 := False;
  FTestInteger1 := -1;
  FTestInteger2 := -1;
end;

procedure TLRCRegInifileTest.Done;
begin
  FTestString1 := EmptyStr;
  FTestString2 := EmptyStr;
  FTestBool1 := False;
  FTestBool2 := False;
  FTestInteger1 := -1;
  FTestInteger2 := -1;
end;

procedure TLRCRegInifileTest.SetUp;
begin
  Init;
  SetSectionsAndIdents;
  FLRCRRegIniFile := TTestLRCRegIniFile.Create(LRCREGINIFILE_TESTROOT);
end;

procedure TLRCRegInifileTest.TearDown;
begin
  SetSectionsAndIdents(False);
  Done;
end;

procedure TLRCRegInifileTest.DeleteKey;
var
  success: boolean;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'DeleteKeySection',
    DeleteKeyBeforeLRCProc);

  // Soll einen Datenwert löschen
  success := FLRCRRegIniFile.DeleteKey('DeleteKeySection', 'StringIdent');

  AssertTrue('Schlüssel konnte nicht aus Section gelöscht werden!', success);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'DeleteKeySection',
    DeleteKeyAfterLRCProc);

  AssertFalse('Schlüssel wurde nicht erfolgreich gelöscht!',
    (FTestString1 = FTestString2));
end;

procedure TLRCRegInifileTest.EraseSection;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
    EraseSectionBeforeLRCProc);

  // Entfernt eine Section und deren Datenwerte
  FLRCRRegIniFile.EraseSection('DeleteKeySection');

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
    EraseSectionAfterLRCProc);
end;

procedure TLRCRegInifileTest.ReadBool;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'BooleanSection',
    ReadBoolBeforeLRCProc);

  // Liest einen boolschen Datenwert
  FTestBool2 :=
    FLRCRRegIniFile.ReadBool('BooleanSection', 'ReadBoolean', False);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestBool1=FTestBool2);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestBool2=True);
end;

procedure TLRCRegInifileTest.ReadInteger;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'IntegerSection',
    ReadIntegerBeforeLRCProc);

  // Liest einen Integer Datenwert
  FTestInteger2 :=
    FLRCRRegIniFile.ReadInteger('IntegerSection', 'ReadInteger', -1);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestInteger1=FTestInteger2);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestInteger2=1965);
end;

procedure TLRCRegInifileTest.ReadString;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'StringSection',
    ReadStringBeforeLRCProc);

  // Liest einen String Datenwert
  FTestString2 :=
    FLRCRRegIniFile.ReadString('StringSection', 'ReadString', EmptyStr);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestString1=FTestString2);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestString2='TestStringForReadTest');
end;

procedure TLRCRegInifileTest.ReadSection;
begin
  FTestList1 := TStringList.Create;
  FTestList2 := TStringList.Create;
  try
    GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'DeleteKeySection',
      ReadSectionBeforeLRCProc);

    // Liest alle Namen aller Datenwerte einer Section (NAME)
    FLRCRRegIniFile.ReadSection('DeleteKeySection', FTestList2);

    CheckListForReadSectionTest(FTestList2);
  finally
    if Assigned(FTestList1) then
      FreeAndNil(FTestList1);

    if Assigned(FTestList2) then
      FreeAndNil(FTestList2);
  end;
end;

procedure TLRCRegInifileTest.ReadSections;
begin
  FTestList1 := TStringList.Create;
  FTestList2 := TStringList.Create;
  try
    GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
      ReadSectionsBeforeLRCProc);

    // Liest alle Unterschlüssel eines Hauptschlüssels
    FLRCRRegIniFile.ReadSections(FTestList2);

    CheckListForReadSectionsTest(FTestList2);
  finally
    if Assigned(FTestList1) then
      FreeAndNil(FTestList1);

    if Assigned(FTestList2) then
      FreeAndNil(FTestList2);
  end;
end;

procedure TLRCRegInifileTest.ReadSectionValues;
begin
  FTestList1 := TStringList.Create;
  FTestList2 := TStringList.Create;
  try
    GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'DeleteKeySection',
      ReadSectionValuesBeforeLRCProc);

    // Liest alle Datenwerte einer Section (NAME=VALUE)
    FLRCRRegIniFile.ReadSectionValues('DeleteKeySection', FTestList2);

    CheckListForReadSectionValuesTest(FTestList2);
  finally
    if Assigned(FTestList1) then
      FreeAndNil(FTestList1);

    if Assigned(FTestList2) then
      FreeAndNil(FTestList2);
  end;
end;

procedure TLRCRegInifileTest.WriteBool;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'BooleanSection',
    WriteBoolBeforeLRCProc);

  // Schreibt einen boolschen Datenwert
  FLRCRRegIniFile.WriteBool('BooleanSection', 'WriteBoolean', False);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'BooleanSection',
    WriteBoolAfterLRCProc1);

  FLRCRRegIniFile.WriteBool('BooleanSection', 'WriteBooleanNew', True);

  FTestBool1 := False;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'BooleanSection',
    WriteBoolAfterLRCProc2);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
    WriteBoolAfterLRCProc3);

  FLRCRRegIniFile.WriteBool('BooleanSectionNew', 'WriteBoolean', True);

  FTestBool1 := False;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'BooleanSectionNew',
    WriteBoolAfterLRCProc4);
end;

procedure TLRCRegInifileTest.WriteInteger;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'IntegerSection',
    WriteIntegerBeforeLRCProc);

  // Schreibt einen Interger Datenwert
  FLRCRRegIniFile.WriteInteger('IntegerSection', 'WriteInteger', 4321);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'IntegerSection',
    WriteIntegerAfterLRCProc1);

  FLRCRRegIniFile.WriteInteger('IntegerSection', 'WriteIntegerNew', 1111);

  FTestInteger1 := 1;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'IntegerSection',
    WriteIntegerAfterLRCProc2);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
    WriteIntegerAfterLRCProc3);

  FLRCRRegIniFile.WriteInteger('IntegerSectionNew', 'WriteInteger', 99);

  FTestInteger1 := 1;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'IntegerSectionNew',
    WriteIntegerAfterLRCProc4);
end;

procedure TLRCRegInifileTest.WriteString;
begin
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'StringSection',
    WriteStringBeforeLRCProc);

  // Schreibt einen String Datenwert
  FLRCRRegIniFile.WriteString('StringSection', 'WriteString', 'Wert wird geändert');

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'StringSection',
    WriteStringAfterLRCProc1);

  FLRCRRegIniFile.WriteString('StringSection', 'WriteStringNew', 'Neuer Ident');

  FTestString1 := EmptyStr;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'StringSection',
    WriteStringAfterLRCProc2);

  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, '',
    WriteStringAfterLRCProc3);

  FLRCRRegIniFile.WriteString('StringSectionNew', 'WriteString',
    'Neue Section und neuer Ident');

  FTestString1 := EmptyStr;
  GetRegistry(HKEY_CURRENT_USER, LRCREGINIFILE_TESTROOT, 'StringSectionNew',
    WriteStringAfterLRCProc4);
end;

procedure TLRCRegInifileTest.KeyExists;
var
  key_exists: boolean;
begin
   key_exists := FLRCRRegIniFile.KeyExists('StringSection');

   AssertTrue(
     'Section: StringSection wurde nicht gefunden, Test fehlgeschlagen',
       key_exists);
end;

procedure TLRCRegInifileTest.ValueExists;
var
  value_exists: boolean;
begin
  value_exists := FLRCRRegIniFile.ValueExists('StringSection', 'WriteString');

  AssertTrue(
    'Value: WriteSring der Section: StringSection wurde nicht gefunden, Test fehlgeschlagen',
      value_exists);
end;

procedure TLRCRegInifileTest.Filename;
var
  filename: string;
begin
  filename := FLRCRRegIniFile.Filename;

  AssertEquals('Falscher Eintrag für Rootsection',
      '\' + LRCREGINIFILE_TESTROOT + '\', filename);
end;

procedure TLRCRegInifileTest.Root;
begin
  AssertEquals('Falscher Eintrag für den Rootkey',
    HKEY_CURRENT_USER, FLRCRRegIniFile.Root);
end;

end.
