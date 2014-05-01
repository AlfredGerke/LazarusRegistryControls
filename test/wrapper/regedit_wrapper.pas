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
    FDefault: string;
  protected
    procedure _Initialize; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegControl; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;

    property Default: string
      read FDefault
      write FDefault;
  public
    procedure SectionIdentDefault;
  end;

  { TRegEditWrapperUTF8 }

  TRegEditWrapperUTF8 = class(TRegEditWrapper)
  private
  protected
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;


implementation

uses
  test_const,
  fpcunit;

{ TRegEditWrapperUTF8 }

procedure TRegEditWrapperUTF8.SetSectionsAndIdents;
begin

end;

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

procedure TRegEditWrapper._Initialize;
begin
  inherited _Initialize;
end;

procedure TRegEditWrapper.SetSectionsAndIdents;
begin

end;

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

procedure TRegEditWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegEdit.RegistrySection.Section',
    Section, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegEdit.RegistrySection.Ident',
    Ident, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegEdit.RegistrySection.Default',
    Default, FRegControl.RegistrySettings.Default);
end;

end.

