unit regcheckbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regcheckbox,
  regsourcen,
  test_wrapper;

type

  { TRegCheckBoxWrapper }

  TRegCheckBoxWrapper = class(TWrapperCS<TRegCheckBox>)
  private
  protected
    procedure SetRegControl; override;
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
  public
  end;

  { TRegCheckBoxWrapperUTF8 }

  TRegCheckBoxWrapperUTF8 = class(TRegCheckBoxWrapper)
  private
  protected
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
  public
  end;

implementation

uses
  test_const;

{ TRegCheckBoxWrapperUTF8 }

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

procedure TRegCheckBoxWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGCHECKBOX_NAME);
end;

procedure TRegCheckBoxWrapper.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;

  RegControl.RegistrySource.DeleteKey(SEC_TREGCHECKBOX, IDENT_CAPTION);
end;

procedure TRegCheckBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(SEC_TREGCHECKBOX, IDENT_CHECK_PROPERTY,
    _CHECKED_ENTRY);

  RegControl.RegistrySource.WriteString(SEC_TREGCHECKBOX, IDENT_CAPTION,
    _TREGCHECKBOX_CAPTION_VALUE);
end;

procedure TRegCheckBoxWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_CHECKED_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGCHECKBOX;
  RegControl.RegistrySettings.Ident := IDENT_CHECK_PROPERTY;
end;

procedure TRegCheckBoxWrapper.SetCaptionSettings;
begin
  inherited SetCaptionSettings;

  RegControl.CaptionSettings.Section := SEC_TREGCHECKBOX;
  RegControl.CaptionSettings.Ident := IDENT_CAPTION;
  RegControl.CaptionSettings.CaptionByRegistry := True;

  RegControl.Caption := DEFAULT_CAPTION_VALUE;
end;

end.
