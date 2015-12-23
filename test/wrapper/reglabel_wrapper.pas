unit reglabel_wrapper;

{$mode delphi}

interface

uses
  regsourcen,
  reglabel,
  test_wrapper;

type

  { TRegLabelForTest }

  TRegLabelForTest = class(TRegLabel)
  end;

  { TRegLabelWrapper }

  TRegLabelWrapper = class(TWrapper<TRegLabelForTest>)
  private
    FDefault: string;
    FCaptionValueByReg: string;
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

    property CaptionValueByReg: string
      read FCaptionValueByReg
      write FCaptionValueByReg;
  public
    procedure SectionIdentDefault;
  end;

  { TRegLabelWrapperUTF8 }

  TRegLabelWrapperUTF8 = class(TRegLabelWrapper)
  private
  protected
    procedure SetSectionsAndIdents; override;
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

procedure TRegLabelWrapperUTF8.SetSectionsAndIdents;
begin
  Section := SEC_TREGLABEL_UTF8;
  Ident := IDENT_CAPTION_UTF8;
  Default := DEFAULT_CAPTION_VALUE;

  CaptionValueByReg := _TREGCHECKBOX_CAPTION_VALUE_UTF8;
end;

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

procedure TRegLabelWrapper._Initialize;
begin
  inherited _Initialize;

  SetSectionsAndIdents;
end;

procedure TRegLabelWrapper.SetSectionsAndIdents;
begin
  Section := SEC_TREGLABEL;
  Ident := IDENT_CAPTION;
  Default := DEFAULT_CAPTION_VALUE;

  CaptionValueByReg := _TREGCHECKBOX_CAPTION_VALUE;
end;

procedure TRegLabelWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGLABEL_NAME);
  RegControl.Caption := CaptionValueByReg;
end;

procedure TRegLabelWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteString(Section, Ident,
    CaptionValueByReg);
end;

procedure TRegLabelWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_CAPTION_VALUE;
  RegControl.RegistrySettings.Section := Section;
  RegControl.RegistrySettings.Ident := Ident;
end;

procedure TRegLabelWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegLabel.RegistrySection.Section',
    Section, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegLabel.RegistrySection.Ident',
    Ident, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegLabel.RegistrySection.Default',
    Default, FRegControl.RegistrySettings.Default);
end;

end.

