unit reglistbox_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  lrc_testcase,
  reglistbox_wrapper,
  registrysource_wrapper,
  regbasics;

type

  {TRegListBoxGenericTest}

  TRegListBoxGenericTest<_T1,_T2>= class(TLRCTestCase<Integer>)
  private
    FRegSrcWrapper: _T1;
    FRegListBoxWrapper: _T2;

    procedure ReadRegistryProc(aIni: TLRCRegIniFile);
  protected
    procedure DebugItems;

    procedure SetUp; override;
    procedure TearDown; override;

    procedure DoReadRegistry;
  published
  end;

  { TRegListBoxTest }

  TRegListBoxTest = class(TRegListBoxGenericTest<TRegistrySourceWrapper, TRegListBoxWrapper>)
  protected
    procedure SetSectionsAndIdents; override;
  published
    procedure ReadRegistry;
  end;

  { TRegListBoxUTF8Test }

  TRegListBoxUTF8Test = class(TRegListBoxGenericTest<TRegistrySourceWrapperUTF8, TRegListBoxWrapperUTF8>)
  protected
    procedure SetSectionsAndIdents; override;
  published
    procedure ReadRegistry;
  end;

  { TRegListBoxDeleteItemTest }

  TRegListBoxDeleteItemTest = class(TRegListBoxGenericTest<TRegistrySourceWrapper, TRegListBoxWrapperDeleteItem>)
  private
    procedure DeleteItemProc(aIni: TLRCRegIniFile);
  protected
    procedure SetSectionsAndIdents; override;
  published
    procedure DeleteItem;
  end;

implementation

uses
  test_utils,
  dbugintf;

{ TRegListBoxGenericTest }

procedure TRegListBoxGenericTest<_T1,_T2>.DebugItems;
var
  count: integer;
  item: string;
begin
  SendDebugEx('//-->', dlInformation);

  for count := 0 to FRegListBoxWrapper.ItemsCount-1 do
  begin
    item := FRegListBoxWrapper.GetItemByIndex(count);
    SendDebugFmtEx('Index: %d - Item: %s ', [count, item], dlInformation);
  end;
  SendDebugEx('//!<--', dlInformation);
  SendDebugEx('Items auflisten', dlInformation);
end;

procedure TRegListBoxGenericTest<_T1,_T2>.SetUp;
begin
  inherited SetUp;

  FRegSrcWrapper := _T1.Create;
  FRegListBoxWrapper := _T2.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TRegListBoxGenericTest<_T1,_T2>.TearDown;
begin
  FreeAndNil(FRegSrcWrapper);
  FreeAndNil(FRegListBoxWrapper);
end;

procedure TRegListBoxGenericTest<_T1,_T2>.DoReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegListBoxWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey, ReadRegistryProc);
end;

procedure TRegListBoxGenericTest<_T1,_T2>.ReadRegistryProc(aIni: TLRCRegIniFile);
begin
  Fail('Testprocedure noch nicht implementiert!');
end;

{ TRegListBoxTest }

procedure TRegListBoxTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  CheckRTLNeeded := True;
end;

procedure TRegListBoxTest.ReadRegistry;
begin
  DoReadRegistry;
end;

{ TRegListBoxUTF8Test }

procedure TRegListBoxUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  CheckRTLNeeded := True;
end;

procedure TRegListBoxUTF8Test.ReadRegistry;
begin
  DoReadRegistry;
end;

{ TRegListBoxDeleteItemTest }

procedure TRegListBoxDeleteItemTest.DeleteItemProc(aIni: TLRCRegIniFile);
var
  list: TStrings;
  count: integer;
  index: integer;
  success: boolean;
  found: boolean;
begin
  list := TStringList.Create;
  try
    with aIni do
    begin
      DebugItems;

      // 1. Anzahl prüfen
      ReadSection(FRegListBoxWrapper.SpecialListProperties.ListSection, list);
      count := list.count;

      AssertEquals('Test nicht durchführbar: Anzahl Registry-Einträge ungleich Anzahl Items im Control',
        count, FRegListBoxWrapper.ItemsCount);

      // 2. Key3 prüfen
      found := ValueExists(FRegListBoxWrapper.SpecialListProperties.ListSection, 'Key3');

      AssertTrue('Test nicht durchführbar: Item Key3 konnte nicht gefunden werden', found);

      // 3. Index für Key3 ermitteln
      index := FRegListBoxWrapper.GetIndexOfItem('Key3');

      AssertTrue('Test nicht durchführbar: Index für Item Key3 nicht gefunden', (index > -1));

      // 4: Key3 löschen
      success := FReglistBoxWrapper.DeleteItem(index);

      AssertTrue('DeleteItem: Item Key3 konnte nicht gelöscht werden', success);

      // 5. Anzahl Prüfen wenn DoSyncData = True
      if FRegListBoxWrapper.DoSyncData then
      begin
        ReadSection(FRegListBoxWrapper.SpecialListProperties.ListSection, list);
        count := list.count;

        AssertEquals('DeleteItem: Anzahl Registry-Einträge ungleich Anzahl Items im Control',
          count, FRegListBoxWrapper.ItemsCount);
      end;

      // 6. Key3 erneut prüfen
      found := ValueExists(FRegListBoxWrapper.SpecialListProperties.ListSection, 'Key3');

      AssertFalse('DeleteItem: Item Key3 konnte nicht gelöscht werden', found);

      DebugItems;
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
  end;
end;

procedure TRegListBoxDeleteItemTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  CheckRTLNeeded := True;
end;

procedure TRegListBoxDeleteItemTest.DeleteItem;
begin
  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey, DeleteItemProc);
end;

end.

