unit regcheckbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regcheckbox,
  regsourcen,
  test_wrapper;

type

  { TRegCheckBoxWrapper }

  TRegCheckBoxWrapper = class(TWrapper<TRegCheckBox>)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource); override;
  public
  public
  end;

implementation

uses
  test_const;

{ TRegCheckBoxWrapper }

procedure TRegCheckBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(SEC_TREGCHECKBOX, IDENT_CHECK_PROPERTY,
    _CHECKED_ENTRY);
end;

procedure TRegCheckBoxWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  inherited SetRegistrySettings(aRegistrySource);

  RegControl.RegistrySettings.Default := DEFAULT_CHECKED_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGCHECKBOX;
  RegControl.RegistrySettings.Ident := IDENT_CHECK_PROPERTY;
end;

end.
