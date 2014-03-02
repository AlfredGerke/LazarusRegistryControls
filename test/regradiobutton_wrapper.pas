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
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource); override;
    procedure SetCaptionSettings; override;
  public
  public
  end;

implementation

uses
  test_const;

{ TRegRaidoButtonWrapper }

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

procedure TRegRaidoButtonWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  inherited SetRegistrySettings(aRegistrySource);

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

end.
