unit regcheckgroup_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regcheckgroup,
  regsourcen;

type

  { TRegCheckGroupWrapper }

  TRegCheckGroupWrapper = class(TWrapperCSLST<TRegCheckGroup>)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

{ TRegCheckGroupWrapper }

procedure TRegCheckGroupWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegCheckGroupWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

