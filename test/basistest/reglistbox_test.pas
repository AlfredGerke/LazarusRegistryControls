unit reglistbox_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  lrc_testcase,
  reglistbox_wrapper,
  registrysource_wrapper;

type

  {TRegListBoxGenericTest}

  TRegListBoxGenericTest<_T1,_T2>= class(TLRCTestCase<Integer>)
  private
    FRegSrcWrapper: _T1;
    FRegListBoxWrapper: _T2;
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
  test_const;

{ TRegListBoxGenericTest }

procedure TRegListBoxGenericTest<_T1,_T2>.SetUp;
begin
  //
end;

procedure TRegListBoxGenericTest<_T1,_T2>.TearDown;
begin
  //
end;

procedure TRegListBoxGenericTest<_T1,_T2>.ReadRegistry;
begin
  Fail('Methode nicht ausprogrammiert!');
end;

{ TRegListBoxTest }

procedure TRegListBoxTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGLISTBOX;
  //Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_ITEMINDEX_VALUE;

  CheckRTLNeeded := True;
end;

{ TRegListBoxUTF8Test }

procedure TRegListBoxUTF8Test.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGLISTBOX;
  //Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_ITEMINDEX_VALUE;

  CheckRTLNeeded := True;
end;

{ TRegListBoxDeleteItemTest }

procedure TRegListBoxDeleteItemTest.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;

  Section := SEC_TREGLISTBOX;
  //Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_ITEMINDEX_VALUE;

  CheckRTLNeeded := True;
end;

procedure TRegListBoxDeleteItemTest.DeleteItem;
begin
  Fail('Methode nicht ausprogrammiert!');
end;

end.

