unit registrysource_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper;

type

  { TRegistrySourceTest }

  TRegistrySourceTest= class(TTestCase)
  private
    FRegSrcWrapper: TRegistrySourceWrapper;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckPublishedProperties;
  end;

implementation

procedure TRegistrySourceTest.CheckPublishedProperties;
begin
  FRegSrcWrapper.CheckPublishedProperties;
end;

procedure TRegistrySourceTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;
end;

procedure TRegistrySourceTest.TearDown;
begin
  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

