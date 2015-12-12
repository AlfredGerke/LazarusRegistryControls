unit regedit_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regedit,
  regsourcen,
  test_wrapper;

type

  { TRegEditForTest }

  TRegEditForTest = class(TRegEdit)
  public
    procedure TriggerChange;
  end;

  { TRegEditWrapper }

  TRegEditWrapper = class(TWrapper<TRegEditForTest>)
  private
    FDefault: string;
  protected
    procedure _Initialize; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegControl; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;

    property Default: string
      read FDefault
      write FDefault;
  public
    procedure SetRegistryEntries; override;
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
  end;


implementation

uses
  test_const,
  fpcunit,
  sysutils;

{ TRegEditForTest }

procedure TRegEditForTest.TriggerChange;
begin
  Change;
end;

{ TRegEditWrapperUTF8 }

procedure TRegEditWrapperUTF8.SetSectionsAndIdents;
begin
  Section := SEC_TREGEDIT;
  Ident := IDENT_TEXT_PROPERTY;
  Default := DEFAULT_TEXT_ENTRY;
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

  SetSectionsAndIdents;
end;

procedure TRegEditWrapper.SetSectionsAndIdents;
begin
  Section := SEC_TREGEDIT;
  Ident := IDENT_TEXT_PROPERTY;
  Default := DEFAULT_TEXT_ENTRY;
end;

procedure TRegEditWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGEDIT_NAME);
  RegControl.Text := EmptyStr;
end;

procedure TRegEditWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteString(Section, Ident, _TEXT_ENTRY);
end;

procedure TRegEditWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := Default;
  RegControl.RegistrySettings.Section := Section;
  RegControl.RegistrySettings.Ident := Ident;
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

