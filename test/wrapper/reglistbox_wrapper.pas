unit reglistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  reglistbox,
  regsourcen;

type

  { TRegListBoxWrapper }

  TRegListBoxWrapper = class(TWrapperLST<TRegListBox>)
  private
  protected
  procedure SetRegistryEntries; override;
  procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

{ TRegListBoxWrapper }

procedure TRegListBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegListBoxWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

