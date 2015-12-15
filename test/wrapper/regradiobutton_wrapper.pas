unit regradiobutton_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regradiobutton,
  regsourcen,
  test_wrapper;

type

  { TRegRadioButtonForTest }

  TRegRadioButtonForTest = class(TRegRadioButton)
  public
    procedure TriggerClick;
  end;

  { TRegRaidoButtonWrapper }

  TRegRaidoButtonWrapper = class(TWrapperCS<TRegRadioButtonForTest>)
  private
    FDefault: boolean;

  protected
    procedure _Initialize; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegControl; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;

    property Default: boolean
      read FDefault
      write FDefault;
  public
    procedure SetRegistryEntries; override;
    procedure SectionIdentDefault;
  end;

  { TRegRaidoButtonWrapperUTF8 }

  TRegRaidoButtonWrapperUTF8 = class(TRegRaidoButtonWrapper)
  private
  protected
    procedure SetSectionsAndIdents; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
    procedure SetRegistryEntries; override;
  end;


implementation

uses
  test_const,
  fpcunit;

{ TRegRadioButtonForTest }

procedure TRegRadioButtonForTest.TriggerClick;
begin
  Click;
end;

{ TRegRaidoButtonWrapperUTF8 }

procedure TRegRaidoButtonWrapperUTF8.SetSectionsAndIdents;
begin
  Section := SEC_TREGRADIOBUTTON;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CaptionSection := SEC_TREGRADIOBUTTON;
  CaptionIdent := IDENT_CAPTION;
end;

procedure TRegRaidoButtonWrapperUTF8.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;
end;

procedure TRegRaidoButtonWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegRaidoButtonWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

procedure TRegRaidoButtonWrapperUTF8.SetCaptionSettings;
begin
  inherited SetCaptionSettings;
end;

{ TRegRaidoButtonWrapper }

procedure TRegRaidoButtonWrapper._Initialize;
begin
  inherited _Initialize;

  SetSectionsAndIdents;
end;

procedure TRegRaidoButtonWrapper.SetSectionsAndIdents;
begin
  Section := SEC_TREGRADIOBUTTON;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CaptionSection := SEC_TREGRADIOBUTTON;
  CaptionIdent := IDENT_CAPTION;
end;

procedure TRegRaidoButtonWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGRADIOBUTTON_NAME);
  RegControl.Checked := False;
end;

procedure TRegRaidoButtonWrapper.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;

  RegControl.RegistrySource.DeleteKey(CaptionSection, CaptionIdent);
end;

procedure TRegRaidoButtonWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(Section, Ident,
    _CHECKED_ENTRY);

  RegControl.RegistrySource.WriteString(CaptionSection, CaptionIdent,
    _TREGRADIOBUTTON_CAPTION_VALUE);
end;

procedure TRegRaidoButtonWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Section := Section;
  RegControl.RegistrySettings.Ident := Ident;
  RegControl.RegistrySettings.Default := Default;
end;

procedure TRegRaidoButtonWrapper.SetCaptionSettings;
begin
  inherited SetCaptionSettings;

  RegControl.CaptionSettings.Section := CaptionSection;
  RegControl.CaptionSettings.Ident := CaptionIdent;
  RegControl.CaptionSettings.CaptionByRegistry := True;

  RegControl.Caption := DEFAULT_CAPTION_VALUE;
end;

procedure TRegRaidoButtonWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Section',
    Section, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Ident',
    Ident, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegRadioButton.RegistrySection.Default',
    Default, FRegControl.RegistrySettings.Default);
end;

end.
