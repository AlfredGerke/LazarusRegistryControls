unit regchecklistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regchecklistbox,
  regsourcen;

type

  { TRegCheckListBoxForTest }

  TRegCheckListBoxForTest = class(TRegCheckListBox)
  end;

  { TRegCheckListBoxWrapper }

  TRegCheckListBoxWrapper = class(TWrapperLST<TRegCheckListBoxForTest>)
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

{ TRegCheckListBoxWrapper }

procedure TRegCheckListBoxWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGCHECKLISTBOX_NAME);
end;

procedure TRegCheckListBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegCheckListBoxWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

