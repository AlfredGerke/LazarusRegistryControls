unit regradiobutton_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regradiobutton,
  regsourcen,
  test_wrapper;

type

  { TRegRaidoButtonWrapper }

  TRegRaidoButtonWrapper = class(TWrapperCS<TRegRadioButton>)
  private
  protected
    procedure SetRegControl; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
    procedure SectionIdentDefault;
  end;

implementation

uses
  test_const,
  fpcunit;

{ TRegRaidoButtonWrapper }

procedure TRegRaidoButtonWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := 'TRegRadioButton1';
  RegControl.Checked := False;
end;

procedure TRegRaidoButtonWrapper.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;

  RegControl.RegistrySource.DeleteKey(SEC_TREGRADIOBUTTON, IDENT_CAPTION);
end;

procedure TRegRaidoButtonWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(SEC_TREGRADIOBUTTON, IDENT_CHECK_PROPERTY,
    _CHECKED_ENTRY);

  RegControl.RegistrySource.WriteString(SEC_TREGRADIOBUTTON, IDENT_CAPTION,
    _TREGRADIOBUTTON_CAPTION_VALUE);
end;

procedure TRegRaidoButtonWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_CHECKED_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGRADIOBUTTON;
  RegControl.RegistrySettings.Ident := IDENT_CHECK_PROPERTY;
end;

procedure TRegRaidoButtonWrapper.SetCaptionSettings;
begin
  inherited SetCaptionSettings;

  RegControl.CaptionSettings.Section := SEC_TREGRADIOBUTTON;
  RegControl.CaptionSettings.Ident := IDENT_CAPTION;
  RegControl.CaptionSettings.CaptionByRegistry := True;

  RegControl.Caption := DEFAULT_CAPTION_VALUE;
end;

procedure TRegRaidoButtonWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Section',
    SEC_TREGRADIOBUTTON, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Ident',
    IDENT_CHECK_PROPERTY, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Default',
    DEFAULT_CHECKED_ENTRY, FRegControl.RegistrySettings.Default);
end;

end.
