unit reglabel_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  registrysource_wrapper,
  reglabel_wrapper;

type

  TRegLabelGenericTest<_T1,_T2>= class(TTestCase)
  private
    FRegSrcWrapper: _T1;
    FRegLabelWrapper: _T2;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

  TRegLabelTest= class(TRegLabelGenericTest<TRegistrySourceWrapper,TRegLabelWrapper>)
  end;

  TRegLabelUTF8Test= class(TRegLabelGenericTest<TRegistrySourceWrapperUTF8,TRegLabelWrapperUTF8>)
  end;

implementation


procedure TRegLabelGenericTest<_T1,_T2>.SetUp;
begin

end;

procedure TRegLabelGenericTest<_T1,_T2>.TearDown;
begin

end;

end.

