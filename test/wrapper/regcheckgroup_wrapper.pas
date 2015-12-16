unit regcheckgroup_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regcheckgroup,
  regsourcen;

type

  { TRegCheckGroupForTest }

  TRegCheckGroupForTest = class(TRegCheckGroup)
  end;

  { TRegCheckGroupWrapper }

  TRegCheckGroupWrapper = class(TWrapperCSLST<TRegCheckGroupForTest>)
  private
  protected
    procedure SetRegControl; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

uses
  test_const;

{ TRegCheckGroupWrapper }

procedure TRegCheckGroupWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGCHECKGROUP_NAME);
end;

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

