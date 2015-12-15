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
  published
    procedure ReadRegistry;
  end;

  { TRegListBoxTest }

  TRegListBoxTest = class(TRegListBoxGenericTest<TRegistrySourceWrapper, TRegListBoxWrapper>)
  protected
    procedure SetSectionsAndIdents; override;
  end;

  { TRegListBoxUTF8Test }

  TRegListBoxUTF8Test = class(TRegListBoxGenericTest<TRegistrySourceWrapper, TRegListBoxWrapper>)
  protected
    procedure SetSectionsAndIdents; override;
  end;

  { TRegListBoxDeleteItemTest }

  TRegListBoxDeleteItemTest = class(TRegListBoxGenericTest<TRegistrySourceWrapper, TRegListBoxWrapper>)
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

procedure TRegListBoxGenericTest<_T1,_T2>.ReadRegistry;
begin
  // 1. Fall: check Section, Ident, Default
  FRegListBoxWrapper.SectionIdentDefault;

  GetRegIniFile(FRegSrcWrapper.RegistrySource.GetComposedRootKey,
    ReadRegistryProc);
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

{ TRegListBoxUTF8Test }

procedure TRegListBoxUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  CheckRTLNeeded := True;
end;

{ TRegListBoxDeleteItemTest }

procedure TRegListBoxDeleteItemTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  CheckRTLNeeded := True;
end;

procedure TRegListBoxDeleteItemTest.DeleteItem;
begin
  Fail('Testprocedure noch nicht implementiert!');
end;

end.

