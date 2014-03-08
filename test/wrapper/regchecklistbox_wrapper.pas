unit regchecklistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regchecklistbox,
  regsourcen;

type

  { TRegCheckListBoxWrapper }

  TRegCheckListBoxWrapper = class(TWrapperLST<TRegCheckListBox>)
  private
  protected
  procedure SetRegistryEntries; override;
  procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

{ TRegCheckListBoxWrapper }

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

