unit regradiogroup_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regradiogroup,
  regsourcen;

type

  { TRegRadioGroupForTest }

  TRegRadioGroupForTest = class(TRegRadioGroup)
  end;

  { TRegRadioGroupWrapper }

  TRegRadioGroupWrapper = class(TWrapperCSLST<TRegRadioGroupForTest>)
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

{ TRegRadioGroupWrapper }

procedure TRegRadioGroupWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGRADIOBUTTON_NAME);
end;

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

