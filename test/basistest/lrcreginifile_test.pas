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
  Registry;

type

  { TLRCRegInifileTest }

  TLRCRegInifileTest= class(TTestCase)
  private
    FLRCRRegIniFile: TLRCRRegIniFile;
    FTestString1: string;
    FTestString2: string;
    FTestBool1: boolean;
    FTestBool2: boolean;
    FTestInteger1: integer;
    FTestInteger2: integer;

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
    procedure TestFilename;
  end;

implementation

uses
  test_const,
  test_utils;

procedure TLRCRegInifileTest.ReadStringBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestString1 := ReadString('ReadString');

  AssertTrue(Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %s',
    [FTestString1]), FTestString1='TestStringForReadTest');
end;

procedure TLRCRegInifileTest.ReadIntegerBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestInteger1 := ReadInteger('ReadInteger');

  AssertTrue(Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %d',
    [FTestInteger1]), FTestInteger1=1965);
end;

procedure TLRCRegInifileTest.ReadBoolBeforeLRCProc(aReg: TRegistry);
begin
  with aReg do
    FTestBool1 := ReadBool('ReadBoolean');

  AssertTrue(Format('Test nicht durchführbar, Datenwert besitzt falschen Wert: %s',
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

    AssertTrue(Format('Test nicht durchführbar, falsche Anzahl Sectionen: Soll=4 - Ist=%d', [count]),
      count = 4);
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

    AssertTrue(Format('Falsche Anzahl Sectionen: Soll=3 - Ist=%d', [count]),
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
        DeleteKey(LRCREGINIFILE_TESTROOT + '\IntegerSection\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\BooleanSection\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\DeleteKeySection\');
        DeleteKey(LRCREGINIFILE_TESTROOT + '\LRCRegInifile\');
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
end;

procedure TLRCRegInifileTest.Done;
begin
  FTestString1 := EmptyStr;
  FTestString2 := EmptyStr;
end;

procedure TLRCRegInifileTest.SetUp;
begin
  Init;
  SetSectionsAndIdents;
  FLRCRRegIniFile := TLRCRRegIniFile.Create(LRCREGINIFILE_TESTROOT);
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
  FTestBool2 := FLRCRRegIniFile.ReadBool('BooleanSection', 'ReadBoolean', False);

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
  FTestInteger2 := FLRCRRegIniFile.ReadInteger('IntegerSection', 'ReadInteger', -1);

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
  FTestString2 := FLRCRRegIniFile.ReadString('StringSection', 'ReadString', EmptyStr);

  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestString1=FTestString2);
  AssertTrue('Datenwert wurde nicht richtig ausgelesen',
    FTestString2='TestStringForReadTest');
end;

procedure TLRCRegInifileTest.ReadSection;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try

    // Liest alle Namen aller Datenwerte einer Section (NAME)
    FLRCRRegIniFile.ReadSection('DeleteKeySection', list);

  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.ReadSections;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try
    // Liest alle Unterschlüssel eines Hauptschlüssels
    FLRCRRegIniFile.ReadSections(list);

  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.ReadSectionValues;
var
  list: TStrings;
begin
  list := TStringList.Create;
  try

    // Liest alle Datenwerte einer Section (NAME=VALUE)
    FLRCRRegIniFile.ReadSectionValues('DeleteKeySection', list);

  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TLRCRegInifileTest.WriteBool;
begin
  // Schreibt einen boolschen Datenwert
end;

procedure TLRCRegInifileTest.WriteInteger;
begin
  // Schreibt einen Interger Datenwert
end;

procedure TLRCRegInifileTest.WriteString;
begin
  // Schreibt einen String Datenwert
end;

procedure TLRCRegInifileTest.TestFilename;
begin

end;

end.
