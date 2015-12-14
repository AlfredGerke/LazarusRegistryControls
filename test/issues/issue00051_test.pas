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

  { TDeleteItemTest }

  TDeleteItemTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckSettings;
  end;

procedure RegisterIssue00051Tests(aSuitePath: string = 'LRC 09 M2.Issue #51 - DeleteItem einführen');

implementation

uses
  reglistbox_test;

procedure RegisterIssue00051Tests(aSuitePath: string = 'LRC 09 M2.Issue #51 - DeleteItem einführen');
begin
  RegisterTest(aSuitePath, TDeleteItemTest);
  RegisterTest(aSuitePath, TRegListBoxDeleteItemTest);
end;

procedure TDeleteItemTest.SetUp;
begin
  //
end;

procedure TDeleteItemTest.TearDown;
begin
  //
end;

procedure TDeleteItemTest.CheckSettings;
begin

end;

end.

