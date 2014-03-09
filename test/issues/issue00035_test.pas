unit issue00035_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit;

type

  { TCheckRTLAnsiTest }

  TCheckRTLAnsiTest= class(TTestCase)
  private
    procedure SetRegistryEntries;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckSettings;
  end;

implementation

uses
  Registry;

procedure TCheckRTLAnsiTest.SetRegistryEntries;
var
  {%H-}ini: TRegIniFile;
begin
  ini := TRegIniFile.Create('');
end;

procedure TCheckRTLAnsiTest.SetUp;
begin
  // 1. Registryknoten aufbauen um jede String-Eigenschaft mit Umlauten zu
  // testen. Dies muss vor dem Aufruf dem Erstellen der Controls geschehen
  SetRegistryEntries;
end;

procedure TCheckRTLAnsiTest.TearDown;
begin
end;

procedure TCheckRTLAnsiTest.CheckSettings;
begin
  //
end;

end.

