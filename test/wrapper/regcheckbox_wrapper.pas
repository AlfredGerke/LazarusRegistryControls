unit regcheckbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regcheckbox,
  regsourcen,
  test_wrapper;

type

  { TRegCheckBoxForTest }

  TRegCheckBoxForTest = class(TRegCheckBox)
  public
    procedure TriggerClick;
  end;

  { TRegCheckBoxWrapper }

  TRegCheckBoxWrapper = class(TWrapperCS<TRegCheckBoxForTest>)
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

  { TRegCheckBoxWrapperUTF8 }

  TRegCheckBoxWrapperUTF8 = class(TRegCheckBoxWrapper)
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

{ TRegCheckBoxForTest }

procedure TRegCheckBoxForTest.TriggerClick;
begin
  Click;
end;

{ TRegCheckBoxWrapperUTF8 }

procedure TRegCheckBoxWrapperUTF8.SetSectionsAndIdents;
begin
  Section := SEC_TREGCHECKBOX;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CaptionSection := SEC_TREGCHECKBOX;
  CaptionIdent := IDENT_CAPTION;
end;

procedure TRegCheckBoxWrapperUTF8.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;
end;

procedure TRegCheckBoxWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegCheckBoxWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

procedure TRegCheckBoxWrapperUTF8.SetCaptionSettings;
begin
  inherited SetCaptionSettings;
end;

{ TRegCheckBoxWrapper }

procedure TRegCheckBoxWrapper._Initialize;
begin
  inherited _Initialize;

  SetSectionsAndIdents;
end;

procedure TRegCheckBoxWrapper.SetSectionsAndIdents;
begin
  Section := SEC_TREGCHECKBOX;
  Ident := IDENT_CHECK_PROPERTY;
  Default := DEFAULT_CHECKED_ENTRY;

  CaptionSection := SEC_TREGCHECKBOX;
  CaptionIdent := IDENT_CAPTION;
end;

procedure TRegCheckBoxWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGCHECKBOX_NAME);
  RegControl.Checked := False;
end;

procedure TRegCheckBoxWrapper.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;

  RegControl.RegistrySource.DeleteKey(CaptionSection, CaptionIdent);
end;

procedure TRegCheckBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(Section, Ident,
    _CHECKED_ENTRY);

  RegControl.RegistrySource.WriteString(CaptionSection, CaptionIdent,
    _TREGCHECKBOX_CAPTION_VALUE);
end;

procedure TRegCheckBoxWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := Default;
  RegControl.RegistrySettings.Section := Section;
  RegControl.RegistrySettings.Ident := Ident;
end;

procedure TRegCheckBoxWrapper.SetCaptionSettings;
begin
  inherited SetCaptionSettings;

  RegControl.CaptionSettings.Section := CaptionSection;
  RegControl.CaptionSettings.Ident := CaptionIdent;
  RegControl.CaptionSettings.CaptionByRegistry := True;

  RegControl.Caption := DEFAULT_CAPTION_VALUE;
end;

procedure TRegCheckBoxWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegCheckBox.RegistrySection.Section',
    Section, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegCheckBox.RegistrySection.Ident',
    Ident, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegCheckBox.RegistrySection.Default',
    Default, FRegControl.RegistrySettings.Default);
end;

end.
