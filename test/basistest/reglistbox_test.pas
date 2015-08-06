unit reglistbox_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  lrc_testcase;

type

  TRegListBoxGenericTest<_T1,_T2>= class(TLRCTestCase<String>)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

implementation

procedure TRegListBoxGenericTest<_T1,_T2>.SetUp;
begin
  //
end;

procedure TRegListBoxGenericTest<_T1,_T2>.TearDown;
begin
  //
end;

end.

