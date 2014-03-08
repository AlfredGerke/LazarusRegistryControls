unit regcombobox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regcombobox,
  regsourcen;

type

  { TRegComboBoxWrapper }

  TRegComboBoxWrapper = class(TWrapperLST<TRegComboBox>)
  private
  protected
  procedure SetRegistryEntries; override;
  procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                aSetRegSrc: boolean = True); override;
  public
  public
  end;

implementation

{ TRegComboBoxWrapper }

procedure TRegComboBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegComboBoxWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

