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

{ TRegComboBoxWrapper }

procedure TRegComboBoxWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGCOMBOBOX_NAME);
end;

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

