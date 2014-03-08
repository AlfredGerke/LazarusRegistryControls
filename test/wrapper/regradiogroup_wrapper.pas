unit regradiogroup_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regradiogroup,
  regsourcen;

type

  { TRegRadioGroupWrapper }

  TRegRadioGroupWrapper = class(TWrapperCSLST<TRegRadioGroup>)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

{ TRegRadioGroupWrapper }

procedure TRegRadioGroupWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegRadioGroupWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

