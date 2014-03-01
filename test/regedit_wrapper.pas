unit regedit_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regedit,
  regsourcen,
  test_wrapper;

type

  { TRegEditWrapper }

  TRegEditWrapper = class(TWrapper<TRegEdit>)
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

{ TRegEditWrapper }

procedure TRegEditWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteString(SEC_TREGEDIT, IDENT_TEXT_PROPERTY,
    _TEXT_ENTRY);
end;

procedure TRegEditWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  inherited SetRegistrySettings(aRegistrySource);

  RegControl.RegistrySettings.Default := DEFAULT_TEXT_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGEDIT;
  RegControl.RegistrySettings.Ident := IDENT_TEXT_PROPERTY;
end;

end.

