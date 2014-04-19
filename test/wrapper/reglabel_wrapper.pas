unit reglabel_wrapper;

{$mode delphi}

interface

uses
  regsourcen,
  reglabel,
  test_wrapper;

type

  { TRegLabelWrapper }

  TRegLabelWrapper = class(TWrapper<TRegLabel>)
  private
  protected
    procedure SetRegControl; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
    procedure SectionIdentDefault;
  end;

  { TRegLabelWrapperUTF8 }

  TRegLabelWrapperUTF8 = class(TRegLabelWrapper)
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
  test_const,
  fpcunit;

{ TRegLabelWrapperUTF8 }

procedure TRegLabelWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegLabelWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

{ TRegLabelWrapper }

procedure TRegLabelWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGLABEL_NAME);
  RegControl.Caption := CAPTION_FOR_TREGLABEL;
end;

procedure TRegLabelWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteString(SEC_TREGLABEL, IDENT_CAPTION,
    _TREGLABEL_CAPTION_VALUE);
end;

procedure TRegLabelWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_CAPTION_VALUE;
  RegControl.RegistrySettings.Section := SEC_TREGLABEL;
  RegControl.RegistrySettings.Ident := IDENT_CAPTION
end;

procedure TRegLabelWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegLabel.RegistrySection.Section',
    SEC_TREGLABEL, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegLabel.RegistrySection.Ident',
    IDENT_CAPTION, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegLabel.RegistrySection.Default',
    DEFAULT_CAPTION_VALUE, FRegControl.RegistrySettings.Default);
end;

end.

