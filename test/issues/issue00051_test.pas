unit issue00051_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  TDeleteItemTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

procedure RegisterIssue00051Tests(aSuitePath: string = 'LRC 09 M2.Issue #51 - DeleteItem einführen');

implementation

procedure RegisterIssue00051Tests(aSuitePath: string = 'LRC 09 M2.Issue #51 - DeleteItem einführen');
begin

end;


procedure TDeleteItemTest.SetUp;
begin

end;

procedure TDeleteItemTest.TearDown;
begin

end;

initialization

  RegisterTest(TDeleteItemTest);
end.

