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
  test_utils;

{ TRegListBoxGenericTest }

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
begin
  list := TStringList.Create;
  try
    with aIni do
    begin
      // 1. Anzahl Prüfen
      ReadSection(FRegListBoxWrapper.SpecialListProperties.ListSection, list);
      count := list.count;

      AssertEquals('Test nicht durchführbar: Anzahl Registryeinträge ungleich Anzahl Items im Control',
        count, FRegListBoxWrapper.ItemsCount);

      // 2.
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

