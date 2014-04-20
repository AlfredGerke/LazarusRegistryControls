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
    FSection: string;
    FCaptionSection: string;
    FIdent: string;
    FCaptionIdent: string;
    FDefault: boolean;
  protected
    procedure _Initialize; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegControl; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;

    property Section: string
      read FSection;

    property CaptionSection: string
      read FCaptionSection;

    property Ident: string
      read FIdent;

    property CaptionIdent: string
      read FCaptionIdent;

    property Default: boolean
      read FDefault;
  public
    procedure SectionIdentDefault;
  end;

  { TRegRaidoButtonWrapperUTF8 }

  TRegRaidoButtonWrapperUTF8 = class(TRegRaidoButtonWrapper)
  private
  protected
    procedure SetSectionsAndIdents; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
  end;


implementation

uses
  test_const,
  fpcunit;

{ TRegRaidoButtonWrapperUTF8 }

procedure TRegRaidoButtonWrapperUTF8.SetSectionsAndIdents;
begin
  FSection := SEC_TREGRADIOBUTTON;
  FIdent := IDENT_CHECK_PROPERTY;
  FDefault := DEFAULT_CHECKED_ENTRY;

  FCaptionSection := SEC_TREGRADIOBUTTON;
  FCaptionIdent := IDENT_CAPTION;
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
  FSection := SEC_TREGRADIOBUTTON;
  FIdent := IDENT_CHECK_PROPERTY;
  FDefault := DEFAULT_CHECKED_ENTRY;

  FCaptionSection := SEC_TREGRADIOBUTTON;
  FCaptionIdent := IDENT_CAPTION;
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
