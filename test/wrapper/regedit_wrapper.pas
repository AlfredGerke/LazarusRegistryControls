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
    procedure SetRegControl; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;

  { TRegEditWrapperUTF8 }

  TRegEditWrapperUTF8 = class(TRegEditWrapper)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

uses
  test_const;

{ TRegEditWrapperUTF8 }

procedure TRegEditWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegEditWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

{ TRegEditWrapper }

procedure TRegEditWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGEDIT_NAME);
end;

procedure TRegEditWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteString(SEC_TREGEDIT, IDENT_TEXT_PROPERTY,
    _TEXT_ENTRY);
end;

procedure TRegEditWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_TEXT_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGEDIT;
  RegControl.RegistrySettings.Ident := IDENT_TEXT_PROPERTY;
end;

end.

